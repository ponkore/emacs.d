// oracled — Emacs から Oracle を使うための常駐プロセス。
//
//	Emacs ──stdio (JSON-RPC 2.0 / Content-Length)──> oracled ──TCP──> Oracle
//
// 設計は docs/oracle-client-design.md、Phase 0 の実測は
// docs/phase0-findings.md。約束は 3 つ。
//
//   - **daemon は表示を作らない。** 型付きの値を返し、整形は Emacs 側
//   - **daemon は SQL しか知らない。** SQL*Plus のコマンドは実行しないが、
//     「それが SQL でないこと」は見分ける（sqlsplit）
//   - **閉じる全経路で明示 ROLLBACK。** stdin の EOF も含む
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"time"

	_ "github.com/sijms/go-ora/v2"

	"oracled/internal/oradb"
	"oracled/internal/rpc"
	"oracled/internal/sqlsplit"
)

// PROTOCOL は Emacs 側の my:oracle-protocol と一致していないといけない。
// 不一致なら Emacs は機能を無効化して作り直しを促す（gitd と同じ）。
const PROTOCOL = 1

const version = "0.1.0-phase1"

type server struct {
	mgr   *oradb.Manager
	start time.Time
}

func main() {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	conn := rpc.NewConn(os.Stdin, os.Stdout)
	go conn.Serve(s)
	<-conn.Done
	// stdin が EOF（Emacs が死んだ / 終了した）。
	// 【重要】ここでも必ず ROLLBACK してから閉じる。
	s.shutdown()
}

func (s *server) shutdown() {
	for _, sess := range s.mgr.All() {
		if err := sess.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "oracled: %s を閉じるときに: %v\n", sess.ID, err)
		}
		s.mgr.Remove(sess.ID)
	}
}

func (s *server) Handle(c *rpc.Conn, id json.RawMessage, method string,
	params json.RawMessage) (any, *rpc.Error) {

	switch method {
	case "initialize":
		return map[string]any{
			"protocol": PROTOCOL, "version": version, "pid": os.Getpid(),
			"driver": "sijms/go-ora/v2", "go": runtime.Version(),
		}, nil

	case "shutdown":
		go func() {
			time.Sleep(50 * time.Millisecond) // 応答を送り終えてから
			s.shutdown()
			os.Exit(0)
		}()
		return map[string]any{"ok": true}, nil

	case "sql/split":
		// DB を 1 回も叩かない。対話バッファの RET の判定にも使う。
		var p struct {
			Text string `json:"text"`
		}
		if err := unmarshal(params, &p); err != nil {
			return nil, err
		}
		sts := sqlsplit.Split(p.Text)
		if sts == nil {
			sts = []sqlsplit.Statement{}
		}
		return map[string]any{"statements": sts}, nil

	case "session/open":
		var p oradb.OpenParams
		if err := unmarshal(params, &p); err != nil {
			return nil, err
		}
		ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
		defer cancel()
		sess, err := s.mgr.Open(ctx, p)
		if err != nil {
			return nil, dbError(err)
		}
		return sess.Info(), nil

	case "session/close":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		err := sess.Close()
		s.mgr.Remove(sess.ID)
		if err != nil {
			return nil, dbError(err)
		}
		return map[string]any{"ok": true}, nil

	case "session/info":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		return sess.Info(), nil

	case "session/list":
		out := []oradb.Info{}
		for _, sess := range s.mgr.All() {
			out = append(out, sess.Info())
		}
		return map[string]any{"sessions": out}, nil

	case "tx/commit", "tx/rollback":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		var err error
		if method == "tx/commit" {
			err = sess.Commit(context.Background())
		} else {
			err = sess.Rollback(context.Background())
		}
		if err != nil {
			return nil, dbError(err)
		}
		return sess.Info(), nil

	case "session/serveroutput":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		var p struct {
			On bool `json:"on"`
		}
		if err := unmarshal(params, &p); err != nil {
			return nil, err
		}
		if err := sess.SetServerOutput(context.Background(), p.On); err != nil {
			return nil, dbError(err)
		}
		return sess.Info(), nil

	case "tx/autocommit":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		var p struct {
			On bool `json:"on"`
		}
		if err := unmarshal(params, &p); err != nil {
			return nil, err
		}
		if err := sess.SetAutoCommit(p.On); err != nil {
			return nil, rpc.Errorf(rpc.CodeInvalidParams, "%s", err)
		}
		return sess.Info(), nil

	case "sql/exec":
		return s.exec(c, params)

	case "rows/more":
		return s.more(c, params)

	case "rows/close":
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		sess.CloseRows()
		return map[string]any{"ok": true}, nil

	case "$/cancel":
		// 通知。走っている文の最中に届く必要があるので、読み取りと
		// 処理を分けてある（rpc.Serve が要求ごとに goroutine を起こす）。
		sess, rerr := s.session(params)
		if rerr != nil {
			return nil, rerr
		}
		return map[string]any{"cancelled": sess.Cancel()}, nil
	}
	return nil, rpc.Errorf(rpc.CodeMethodNotFound, "未知のメソッド: %s", method)
}

// exec は 1 文を実行する。応答は先に {query} を返し、
// 行は rows/chunk 通知で、結末は stmt/result か stmt/error で流す。
func (s *server) exec(c *rpc.Conn, params json.RawMessage) (any, *rpc.Error) {
	var p struct {
		Session string `json:"session"`
		Text    string `json:"text"`
		Binds   []any  `json:"binds"`
		MaxRows int    `json:"maxrows"`
		Chunk   int    `json:"chunk"`
	}
	if err := unmarshal(params, &p); err != nil {
		return nil, err
	}
	sess, rerr := s.session(params)
	if rerr != nil {
		return nil, rerr
	}
	if p.Text == "" {
		return nil, rpc.Errorf(rpc.CodeInvalidParams, "text が空")
	}

	emit := func(query, seq int, rows [][]any, done bool) {
		if rows == nil {
			rows = [][]any{}
		}
		c.Notify("rows/chunk", map[string]any{
			"session": sess.ID, "query": query, "seq": seq,
			"rows": rows, "done": done,
		})
	}

	res, err := sess.Exec(context.Background(), p.Text, p.Binds, p.MaxRows, p.Chunk, emit)
	if err != nil {
		if err.Error() == "busy" {
			return nil, rpc.Errorf(rpc.CodeBusy, "その session は別の文を走らせている")
		}
		// 【重要】結末は応答だけで返す。通知でも返すと Emacs 側が
		// 2 回処理して、対話バッファにプロンプトが 2 つ出る（実機で踏んだ）。
		// エラーの前に出た DBMS_OUTPUT も一緒に渡す（SQL*Plus と同じ）。
		oe := oradb.Classify(err)
		oe.Output = sess.LastOutput()
		return nil, &rpc.Error{Code: rpc.CodeDBError, Message: oe.Message, Data: oe}
	}
	return res, nil
}

func (s *server) more(c *rpc.Conn, params json.RawMessage) (any, *rpc.Error) {
	var p struct {
		MaxRows int `json:"maxrows"`
		Chunk   int `json:"chunk"`
	}
	if err := unmarshal(params, &p); err != nil {
		return nil, err
	}
	sess, rerr := s.session(params)
	if rerr != nil {
		return nil, rerr
	}
	emit := func(query, seq int, rows [][]any, done bool) {
		if rows == nil {
			rows = [][]any{}
		}
		c.Notify("rows/chunk", map[string]any{
			"session": sess.ID, "query": query, "seq": seq,
			"rows": rows, "done": done,
		})
	}
	res, err := sess.More(context.Background(), p.MaxRows, p.Chunk, emit)
	if err != nil {
		oe := oradb.Classify(err)
		return nil, &rpc.Error{Code: rpc.CodeDBError, Message: oe.Message, Data: oe}
	}
	return res, nil
}

func (s *server) session(params json.RawMessage) (*oradb.Session, *rpc.Error) {
	var p struct {
		Session string `json:"session"`
	}
	if err := unmarshal(params, &p); err != nil {
		return nil, err
	}
	if p.Session == "" {
		return nil, rpc.Errorf(rpc.CodeInvalidParams, "session が無い")
	}
	sess, ok := s.mgr.Get(p.Session)
	if !ok {
		return nil, rpc.Errorf(rpc.CodeNoSession, "未知の session: %s", p.Session)
	}
	return sess, nil
}

func unmarshal(params json.RawMessage, v any) *rpc.Error {
	if len(params) == 0 {
		return rpc.Errorf(rpc.CodeInvalidParams, "params が無い")
	}
	if err := json.Unmarshal(params, v); err != nil {
		return rpc.Errorf(rpc.CodeInvalidParams, "params が読めない: %s", err)
	}
	return nil
}

func dbError(err error) *rpc.Error {
	oe := oradb.Classify(err)
	return &rpc.Error{Code: rpc.CodeDBError, Message: oe.Message, Data: oe}
}
