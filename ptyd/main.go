// ptyd — Windows の ConPTY を Emacs に橋渡しする常駐プロセス。
//
// Windows の Emacs には PTY が無いので、対話 TUI をそのまま動かせない。
// ptyd が疑似コンソール (ConPTY) を持って子プロセスを動かし、その VT
// バイト列を stdio 経由で Emacs に流す。
//
//	Emacs ──stdin (JSON Lines)──> ptyd ──ConPTY──> 子プロセス
//	      <──stdout (生の VT)──        <─────────
//	      <──stderr (診断の行)──
//
// stdin だけ JSON にしてあるのは、キー入力のほかに画面サイズの変更を
// 送る必要があるため。stdout を生のままにしてあるのは、そちらが本流で
// 量が多く、base64 と JSON のエスケープを挟む意味が無いから。
//
// 検討の経緯は docs/claude/emacs-claude-pty-proxy-study.md を参照。
package main

import (
	"bufio"
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"time"
	"unsafe"

	"golang.org/x/sys/windows"
)

// ---------------------------------------------------------------- ConPTY

type pty struct {
	hpc  windows.Handle
	in   *os.File       // ここに書くと子の標準入力になる
	outH windows.Handle // ここから読むと子の画面が VT で出てくる
	pi   windows.ProcessInformation
	mu   sync.Mutex
}

func startPty(cmdline, workdir string, cols, rows int16) (*pty, error) {
	var inR, inW, outR, outW windows.Handle
	if err := windows.CreatePipe(&inR, &inW, nil, 0); err != nil {
		return nil, fmt.Errorf("CreatePipe(in): %w", err)
	}
	if err := windows.CreatePipe(&outR, &outW, nil, 0); err != nil {
		return nil, fmt.Errorf("CreatePipe(out): %w", err)
	}
	var hpc windows.Handle
	if err := windows.CreatePseudoConsole(
		windows.Coord{X: cols, Y: rows}, inR, outW, 0, &hpc); err != nil {
		return nil, fmt.Errorf("CreatePseudoConsole: %w", err)
	}
	// ConPTY がこの 2 本を複製して持つので、こちら側は閉じてよい。
	windows.CloseHandle(inR)
	windows.CloseHandle(outW)

	al, err := windows.NewProcThreadAttributeList(1)
	if err != nil {
		return nil, fmt.Errorf("NewProcThreadAttributeList: %w", err)
	}
	defer al.Delete()
	if err := al.Update(windows.PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
		unsafe.Pointer(hpc), unsafe.Sizeof(hpc)); err != nil {
		return nil, fmt.Errorf("UpdateProcThreadAttribute: %w", err)
	}

	si := new(windows.StartupInfoEx)
	si.ProcThreadAttributeList = al.List()
	si.Cb = uint32(unsafe.Sizeof(*si))
	// これが無いと静かに壊れる。STARTF_USESTDHANDLES を立てて 3 つとも
	// NULL のままにしておくと、ConPTY が疑似コンソールのハンドルを
	// 割り当ててくれる。立てないと、bInheritHandles が false のときに
	// 「親の標準ハンドルの値」がそのまま子に渡り、子では無効なハンドルに
	// なる。子は疑似コンソールに attach していてタイトルまで出るのに、
	// 書き込みだけが全部失敗する、という分かりにくい壊れ方をする。
	si.StartupInfo.Flags |= windows.STARTF_USESTDHANDLES

	var wd *uint16
	if workdir != "" {
		wd, err = windows.UTF16PtrFromString(workdir)
		if err != nil {
			return nil, fmt.Errorf("workdir: %w", err)
		}
	}
	var pi windows.ProcessInformation
	if err := windows.CreateProcess(nil, windows.StringToUTF16Ptr(cmdline),
		nil, nil, false,
		windows.EXTENDED_STARTUPINFO_PRESENT|windows.CREATE_UNICODE_ENVIRONMENT,
		nil, wd, &si.StartupInfo, &pi); err != nil {
		return nil, fmt.Errorf("CreateProcess: %w", err)
	}
	return &pty{hpc: hpc, in: os.NewFile(uintptr(inW), "ptyd-in"),
		outH: outR, pi: pi}, nil
}

func (p *pty) resize(cols, rows int16) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	return windows.ResizePseudoConsole(p.hpc, windows.Coord{X: cols, Y: rows})
}

// ------------------------------------------------- term.el が読めない CSI

// stripUnsupported は `ESC[' の直後が < > = のシーケンスを落とす。
//
// Emacs の term.el はプライベートな CSI の目印として `?' しか見ておらず、
// `ESC[>4;2m' (modifyOtherKeys) を `>' ごと数値化して SGR 0;2、つまり
// 「全属性リセット + faint」として実行してしまう。`?' 付きは term.el が
// 正しく扱うので落としてはいけない。
//
// 途中で切れたシーケンスは持ち越す。ConPTY からの読み取りは任意の位置で
// 切れるので、1 回の Write に収まっている保証が無い。
type csiStripper struct {
	pending []byte
}

func (s *csiStripper) filter(buf []byte) []byte {
	data := append(s.pending, buf...)
	s.pending = nil
	out := make([]byte, 0, len(data))
	i := 0
	for i < len(data) {
		if data[i] != 0x1b {
			out = append(out, data[i])
			i++
			continue
		}
		// ESC 単体で終わっている
		if i+1 >= len(data) {
			s.pending = append(s.pending, data[i:]...)
			break
		}
		if data[i+1] != '[' {
			out = append(out, data[i], data[i+1])
			i += 2
			continue
		}
		// ESC [ …
		j := i + 2
		if j >= len(data) {
			s.pending = append(s.pending, data[i:]...)
			break
		}
		drop := data[j] == '<' || data[j] == '>' || data[j] == '='
		// 終端文字 (0x40-0x7e) まで進む
		k := j
		for k < len(data) && (data[k] < 0x40 || data[k] > 0x7e) {
			k++
		}
		if k >= len(data) {
			// まだ終端が来ていない
			s.pending = append(s.pending, data[i:]...)
			break
		}
		if !drop {
			out = append(out, data[i:k+1]...)
		}
		i = k + 1
	}
	return out
}

// ------------------------------------------------------------ stdin の形

type inMsg struct {
	Op   string `json:"op"`             // "i" 入力 / "r" リサイズ / "q" 終了
	D    string `json:"d,omitempty"`    // "i" のとき base64 のキー入力
	Cols int16  `json:"cols,omitempty"` // "r" のとき
	Rows int16  `json:"rows,omitempty"`
}

func main() {
	cols := flag.Int("cols", 100, "初期の桁数")
	rows := flag.Int("rows", 30, "初期の行数")
	workdir := flag.String("dir", "", "子プロセスの作業ディレクトリ")
	strip := flag.Bool("strip-unsupported-csi", false,
		"ESC[< ESC[> ESC[= のシーケンスを落とす (term.el 向け)")
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Fprintln(os.Stderr, "usage: ptyd [options] -- command [args...]")
		os.Exit(2)
	}
	cmdline := strings.Join(flag.Args(), " ")

	p, err := startPty(cmdline, *workdir, int16(*cols), int16(*rows))
	if err != nil {
		fmt.Fprintf(os.Stderr, "ptyd: %v\n", err)
		os.Exit(1)
	}
	fmt.Fprintf(os.Stderr, "ptyd: started pid=%d cols=%d rows=%d\n",
		p.pi.ProcessId, *cols, *rows)

	// 子 -> Emacs
	done := make(chan struct{})
	go func() {
		defer close(done)
		var st csiStripper
		buf := make([]byte, 16384)
		w := bufio.NewWriterSize(os.Stdout, 16384)
		for {
			var n uint32
			if err := windows.ReadFile(p.outH, buf, &n, nil); err != nil || n == 0 {
				w.Flush()
				return
			}
			b := buf[:n]
			if *strip {
				b = st.filter(b)
			}
			if len(b) > 0 {
				w.Write(b)
			}
			// 端末は「届いたぶんだけ描く」ものなので毎回流す。
			w.Flush()
		}
	}()

	// Emacs -> 子
	go func() {
		sc := bufio.NewScanner(os.Stdin)
		sc.Buffer(make([]byte, 0, 64*1024), 8*1024*1024)
		for sc.Scan() {
			line := strings.TrimSpace(sc.Text())
			if line == "" {
				continue
			}
			var m inMsg
			if err := json.Unmarshal([]byte(line), &m); err != nil {
				fmt.Fprintf(os.Stderr, "ptyd: bad line: %v\n", err)
				continue
			}
			switch m.Op {
			case "i":
				b, err := base64.StdEncoding.DecodeString(m.D)
				if err != nil {
					fmt.Fprintf(os.Stderr, "ptyd: bad base64: %v\n", err)
					continue
				}
				p.in.Write(b)
			case "r":
				if m.Cols > 0 && m.Rows > 0 {
					if err := p.resize(m.Cols, m.Rows); err != nil {
						fmt.Fprintf(os.Stderr, "ptyd: resize: %v\n", err)
					}
				}
			case "q":
				windows.TerminateProcess(p.pi.Process, 1)
			}
		}
	}()

	windows.WaitForSingleObject(p.pi.Process, windows.INFINITE)
	windows.ClosePseudoConsole(p.hpc)
	// ClosePseudoConsole のあとに conhost が最後の出力を吐くので待つ。
	select {
	case <-done:
	case <-time.After(2 * time.Second):
	}
	var code uint32
	windows.GetExitCodeProcess(p.pi.Process, &code)
	fmt.Fprintf(os.Stderr, "ptyd: child exited code=%d\n", code)
	io.Discard.Write(nil)
	os.Exit(int(code))
}
