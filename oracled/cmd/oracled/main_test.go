package main

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"strings"
	"testing"
	"time"

	"oracled/internal/oradb"
	"oracled/internal/rpc"
)

// capture は通知を受け取るための Conn。Handle を直に呼ぶ。
func capture() (*rpc.Conn, *bytes.Buffer) {
	var buf bytes.Buffer
	return rpc.NewConn(strings.NewReader(""), &buf), &buf
}

// notifications は buf に書かれた通知を method ごとに拾う。
func notifications(t *testing.T, buf *bytes.Buffer) []rpc.Message {
	t.Helper()
	var out []rpc.Message
	r := strings.NewReader(buf.String())
	// rpc 側の read は非公開なので、ここでは素でフレームを剥がす
	for {
		var length int
		for {
			line, err := readLine(r)
			if err != nil {
				return out
			}
			if line == "" {
				break
			}
			if k, v, ok := strings.Cut(line, ":"); ok && strings.EqualFold(strings.TrimSpace(k), "Content-Length") {
				length = atoi(strings.TrimSpace(v))
			}
		}
		body := make([]byte, length)
		if _, err := io.ReadFull(r, body); err != nil {
			return out
		}
		var m rpc.Message
		if err := json.Unmarshal(body, &m); err == nil {
			out = append(out, m)
		}
	}
}

func readLine(r io.Reader) (string, error) {
	var sb strings.Builder
	b := make([]byte, 1)
	for {
		if _, err := r.Read(b); err != nil {
			return sb.String(), err
		}
		if b[0] == '\n' {
			return strings.TrimRight(sb.String(), "\r"), nil
		}
		sb.WriteByte(b[0])
	}
}

func atoi(s string) int {
	n := 0
	for _, c := range s {
		if c < '0' || c > '9' {
			return n
		}
		n = n*10 + int(c-'0')
	}
	return n
}

func raw(v any) json.RawMessage {
	b, _ := json.Marshal(v)
	return b
}

// ---- DB の要らないテスト ----

func TestInitializeAndSplit(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()

	res, rerr := s.Handle(c, raw(1), "initialize", raw(map[string]any{}))
	if rerr != nil {
		t.Fatalf("initialize: %v", rerr)
	}
	m := res.(map[string]any)
	if m["protocol"] != PROTOCOL {
		t.Errorf("protocol = %v、期待 %d", m["protocol"], PROTOCOL)
	}

	res, rerr = s.Handle(c, raw(2), "sql/split",
		raw(map[string]any{"text": "select 1 from dual;\n@foo.sql\nbegin null; end;\n/"}))
	if rerr != nil {
		t.Fatalf("sql/split: %v", rerr)
	}
	b, _ := json.Marshal(res)
	var got struct {
		Statements []struct {
			Kind     string `json:"kind"`
			Text     string `json:"text"`
			Complete bool   `json:"complete"`
		} `json:"statements"`
	}
	json.Unmarshal(b, &got)
	if len(got.Statements) != 3 {
		t.Fatalf("文の数が %d: %s", len(got.Statements), b)
	}
	want := []string{"sql", "client", "plsql"}
	for i, k := range want {
		if got.Statements[i].Kind != k {
			t.Errorf("[%d] kind = %q、期待 %q", i, got.Statements[i].Kind, k)
		}
		if !got.Statements[i].Complete {
			t.Errorf("[%d] 未完になっている", i)
		}
	}

	if _, rerr = s.Handle(c, raw(3), "no/such", raw(map[string]any{})); rerr == nil ||
		rerr.Code != rpc.CodeMethodNotFound {
		t.Errorf("未知のメソッドは CodeMethodNotFound で返るべき: %v", rerr)
	}
	if _, rerr = s.Handle(c, raw(4), "session/info",
		raw(map[string]any{"session": "s999"})); rerr == nil || rerr.Code != rpc.CodeNoSession {
		t.Errorf("未知の session は CodeNoSession で返るべき: %v", rerr)
	}
}

// ---- 実 DB に繋ぐテスト（ORACLED_PASSWORD が無ければ飛ばす） ----

func openParams(t *testing.T) map[string]any {
	t.Helper()
	pass := os.Getenv("ORACLED_PASSWORD")
	if pass == "" {
		t.Skip("ORACLED_PASSWORD が無いので飛ばす")
	}
	p := map[string]any{
		"host":       env("ORACLED_HOST", "192.168.66.101"),
		"port":       1521,
		"service":    env("ORACLED_SERVICE", "XEPDB1"),
		"user":       env("ORACLED_USER", "TEST2TAX"),
		"password":   pass,
		"autocommit": false,
	}
	return p
}

func env(k, def string) string {
	if v := os.Getenv(k); v != "" {
		return v
	}
	return def
}

func openSession(t *testing.T, s *server, c *rpc.Conn) string {
	t.Helper()
	res, rerr := s.Handle(c, raw(1), "session/open", raw(openParams(t)))
	if rerr != nil {
		t.Fatalf("session/open: %v", rerr)
	}
	info := res.(oradb.Info)
	t.Logf("接続: %s@%s %s charset=%s sid=%s,%s cancel_sql=%v (%.1f ms)",
		info.User, info.DB, info.Version, info.Charset, info.SID, info.Serial,
		info.CanCancelSQL, info.ConnectMS)
	if info.Session == "" {
		t.Fatal("session が空")
	}
	return info.Session
}

func TestLiveQuery(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, buf := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	// 日本語・NULL・NUMBER・DATE をまとめて 1 文で見る
	res, rerr := s.Handle(c, raw(2), "sql/exec", raw(map[string]any{
		"session": sid,
		"text": `select 1 as n, 1/3 as frac, 'あいう' as jp,
		                cast(null as varchar2(10)) as nul, sysdate as d,
		                systimestamp as tz from dual`,
	}))
	if rerr != nil {
		t.Fatalf("sql/exec: %v", rerr)
	}
	r := res.(*oradb.Result)
	if r.Kind != "query" || r.Rows != 1 {
		t.Errorf("Kind=%q Rows=%d", r.Kind, r.Rows)
	}
	if len(r.Columns) != 6 {
		t.Fatalf("列が %d", len(r.Columns))
	}
	if !r.Columns[0].Numeric {
		t.Errorf("1 列目が Numeric になっていない: %+v", r.Columns[0])
	}
	if !r.Columns[4].Temporal {
		t.Errorf("sysdate が Temporal になっていない: %+v", r.Columns[4])
	}

	// rows/chunk 通知を見る
	var rows [][]any
	for _, m := range notifications(t, buf) {
		if m.Method != "rows/chunk" {
			continue
		}
		var p struct {
			Rows [][]any `json:"rows"`
			Done bool    `json:"done"`
		}
		json.Unmarshal(m.Params, &p)
		rows = append(rows, p.Rows...)
	}
	if len(rows) != 1 {
		t.Fatalf("rows/chunk で来た行が %d: %s", len(rows), buf.String())
	}
	row := rows[0]
	if row[0] != "1" {
		t.Errorf("NUMBER が %#v（文字列 \"1\" で来るはず）", row[0])
	}
	if fs, ok := row[1].(string); !ok || len(fs) < 20 {
		t.Errorf("1/3 が %#v（精度が落ちている）", row[1])
	}
	if row[2] != "あいう" {
		t.Errorf("日本語が %#v", row[2])
	}
	if row[3] != nil {
		t.Errorf("NULL が %#v（nil で来るはず）", row[3])
	}
	// 【重要】DATE はオフセットを付けない（付けると Emacs 側で 9 時間ずれる）
	if d, ok := row[4].(string); !ok || strings.ContainsAny(d, "Z+") {
		t.Errorf("DATE が %#v（オフセット無しの ISO-8601 で来るはず）", row[4])
	}
	if tz, ok := row[5].(string); !ok || !strings.ContainsAny(tz, "Z+-") {
		t.Errorf("TIMESTAMP WITH TZ が %#v（オフセット付きで来るはず）", row[5])
	}
}

// 行の分割送信と rows/more。
func TestLiveChunks(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, buf := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	res, rerr := s.Handle(c, raw(2), "sql/exec", raw(map[string]any{
		"session": sid, "chunk": 20, "maxrows": 50,
		"text": "select object_name from all_objects where rownum <= 500",
	}))
	if rerr != nil {
		t.Fatalf("sql/exec: %v", rerr)
	}
	r := res.(*oradb.Result)
	if r.Rows != 50 || !r.More {
		t.Errorf("Rows=%d More=%v（50 行で止まり、続きがあるはず）", r.Rows, r.More)
	}
	chunks := 0
	total := 0
	for _, m := range notifications(t, buf) {
		if m.Method != "rows/chunk" {
			continue
		}
		var p struct {
			Rows [][]any `json:"rows"`
			Seq  int     `json:"seq"`
		}
		json.Unmarshal(m.Params, &p)
		chunks++
		total += len(p.Rows)
	}
	if chunks < 2 || total != 50 {
		t.Errorf("chunk が %d 回で %d 行（20 行ずつ 3 回に分かれるはず）", chunks, total)
	}

	// 続きを取る
	res, rerr = s.Handle(c, raw(3), "rows/more",
		raw(map[string]any{"session": sid, "maxrows": 10, "chunk": 10}))
	if rerr != nil {
		t.Fatalf("rows/more: %v", rerr)
	}
	if r2 := res.(*oradb.Result); r2.Rows != 10 || !r2.More {
		t.Errorf("rows/more: Rows=%d More=%v", r2.Rows, r2.More)
	}
	if _, rerr = s.Handle(c, raw(4), "rows/close",
		raw(map[string]any{"session": sid})); rerr != nil {
		t.Errorf("rows/close: %v", rerr)
	}
}

// 【重要】走っている文を $/cancel で止められること。
// Phase 0 で probe では確かめたが、daemon の経路でも通ることを見る。
func TestLiveCancel(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	const heavy = `select count(*) from
	   (select 1 x from all_objects where rownum <= 20000) a,
	   (select 1 y from all_objects where rownum <= 20000) b`

	done := make(chan *rpc.Error, 1)
	start := time.Now()
	go func() {
		_, rerr := s.Handle(c, raw(2), "sql/exec",
			raw(map[string]any{"session": sid, "text": heavy}))
		done <- rerr
	}()
	time.Sleep(1500 * time.Millisecond)
	cancelAt := time.Now()
	if _, rerr := s.Handle(c, nil, "$/cancel",
		raw(map[string]any{"session": sid})); rerr != nil {
		t.Fatalf("$/cancel: %v", rerr)
	}
	select {
	case rerr := <-done:
		el := time.Since(cancelAt)
		if rerr == nil {
			t.Fatalf("止まらずに成功した（%s）", time.Since(start))
		}
		t.Logf("割り込み要求から %.1f ms で戻った: %s", float64(el.Microseconds())/1000, rerr.Message)
		if el > 3*time.Second {
			t.Errorf("止まるまで %s かかった", el)
		}
		if !strings.Contains(rerr.Message, "ORA-01013") {
			t.Errorf("ORA-01013 ではない: %s", rerr.Message)
		}
	case <-time.After(30 * time.Second):
		t.Fatal("割り込んでも戻ってこない")
	}

	// 割り込んだ後も同じ session が使えること
	if _, rerr := s.Handle(c, raw(3), "sql/exec",
		raw(map[string]any{"session": sid, "text": "select 1 from dual"})); rerr != nil {
		t.Errorf("割り込み後に使えない: %v", rerr)
	}
}

// autocommit OFF が *sql.Tx で成立していること、tx 制御が通ること。
func TestLiveTransaction(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	exec := func(text string) (*oradb.Result, *rpc.Error) {
		res, rerr := s.Handle(c, raw(2), "sql/exec",
			raw(map[string]any{"session": sid, "text": text}))
		if rerr != nil {
			return nil, rerr
		}
		return res.(*oradb.Result), nil
	}

	_, _ = exec("drop table ORACLED_T1 purge")
	if _, rerr := exec("create table ORACLED_T1 (id number)"); rerr != nil {
		t.Fatalf("create: %v", rerr)
	}
	defer exec("drop table ORACLED_T1 purge")

	r, rerr := exec("insert into ORACLED_T1 values (1)")
	if rerr != nil {
		t.Fatalf("insert: %v", rerr)
	}
	if r.Affected != 1 {
		t.Errorf("Affected = %d", r.Affected)
	}
	if !r.InTx {
		t.Errorf("autocommit OFF なのに tx の中にいない")
	}

	// rollback で消えること
	if _, rerr := s.Handle(c, raw(3), "tx/rollback",
		raw(map[string]any{"session": sid})); rerr != nil {
		t.Fatalf("tx/rollback: %v", rerr)
	}
	res, _ := s.Handle(c, raw(4), "sql/exec",
		raw(map[string]any{"session": sid, "text": "select count(*) from ORACLED_T1"}))
	if res.(*oradb.Result).Rows != 1 {
		t.Fatal("count が取れない")
	}

	// 【重要】tx 中の DDL は保留中の DML を巻き込んでコミットする。
	// implicit_commit が立つことを見る（Emacs がこれを出す）。
	if _, rerr := exec("insert into ORACLED_T1 values (2)"); rerr != nil {
		t.Fatalf("insert: %v", rerr)
	}
	r, rerr = exec("create table ORACLED_T2 (id number)")
	if rerr != nil {
		t.Fatalf("create: %v", rerr)
	}
	defer exec("drop table ORACLED_T2 purge")
	if !r.ImplicitCommit {
		t.Error("DDL で implicit_commit が立っていない")
	}
	if r.InTx {
		t.Error("DDL の後も tx の中にいることになっている")
	}

	// autocommit はトランザクション中は切り替えられない
	if _, rerr := exec("insert into ORACLED_T1 values (3)"); rerr != nil {
		t.Fatalf("insert: %v", rerr)
	}
	if _, rerr := s.Handle(c, raw(5), "tx/autocommit",
		raw(map[string]any{"session": sid, "on": true})); rerr == nil {
		t.Error("tx 中の autocommit 切り替えが通ってしまった")
	}
	s.Handle(c, raw(6), "tx/rollback", raw(map[string]any{"session": sid}))
}

// エラーが構造で返ること（code / message / position）。
func TestLiveError(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	_, rerr := s.Handle(c, raw(2), "sql/exec",
		raw(map[string]any{"session": sid, "text": "select foo from dual"}))
	if rerr == nil {
		t.Fatal("エラーにならなかった")
	}
	oe, ok := rerr.Data.(oradb.OraError)
	if !ok {
		t.Fatalf("Data が OraError でない: %#v", rerr.Data)
	}
	if oe.Code != 904 {
		t.Errorf("code = %d（ORA-00904 のはず）", oe.Code)
	}
	// 【重要】位置が取れると Emacs 側でソースの該当位置へ飛べる
	if oe.Position != 7 {
		t.Errorf("position = %d（select foo の f は 7 文字目）", oe.Position)
	}
	if strings.Contains(oe.Message, "error occur at position") {
		t.Errorf("message に位置の文字列が残っている: %q", oe.Message)
	}
}
