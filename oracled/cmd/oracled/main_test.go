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
	// 【重要】autocommit OFF でも、SELECT を投げただけで TX と出してはいけない。
	// daemon は内部で *sql.Tx を張るが、Oracle 側のトランザクションは
	// DML で初めて始まる（表示が「書いていないのに TX」になると信用を失う）。
	if r.InTx {
		t.Error("SELECT だけで in_tx が立っている")
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

// DBMS_OUTPUT が 1 往復で取れること（SERVEROUTPUT ON）。
// 【重要】GET_LINE を 1 行ずつ呼ぶと 1 行 = 1 往復（この回線で 110 ms）に
// なるので、GET_LINES を PL/SQL ブロックの中で連結して 1 つの OUT で受ける。
func TestLiveServerOutput(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	p := openParams(t)
	p["serveroutput"] = true
	res, rerr := s.Handle(c, raw(1), "session/open", raw(p))
	if rerr != nil {
		t.Fatalf("session/open: %v", rerr)
	}
	sid := res.(oradb.Info).Session
	if !res.(oradb.Info).ServerOutput {
		t.Error("serveroutput が立っていない")
	}
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	exec := func(text string) (*oradb.Result, *rpc.Error) {
		r, rerr := s.Handle(c, raw(2), "sql/exec",
			raw(map[string]any{"session": sid, "text": text}))
		if rerr != nil {
			return nil, rerr
		}
		return r.(*oradb.Result), nil
	}

	// 1 往復で何行取れるか（往復の回数を減らすのが目的）
	start := time.Now()
	r, rerr := exec(`begin
	  for i in 1 .. 50 loop
	    dbms_output.put_line('行 ' || i || ' あいう');
	  end loop;
	end;`)
	if rerr != nil {
		t.Fatalf("put_line: %v", rerr)
	}
	t.Logf("50 行の DBMS_OUTPUT を含む文: %.1f ms（往復込み）", float64(time.Since(start).Milliseconds()))
	if len(r.Output) != 50 {
		t.Fatalf("output が %d 行（50 行のはず）", len(r.Output))
	}
	if r.Output[0] != "行 1 あいう" {
		t.Errorf("1 行目が %q", r.Output[0])
	}
	if r.Output[49] != "行 50 あいう" {
		t.Errorf("50 行目が %q", r.Output[49])
	}

	// 取ったら消えること（次の文に持ち越さない）
	r, rerr = exec("select 1 from dual")
	if rerr != nil {
		t.Fatalf("select: %v", rerr)
	}
	if len(r.Output) != 0 {
		t.Errorf("持ち越している: %v", r.Output)
	}

	// 500 行を超えても（GET_LINES の 1 回分を超えても）全部取れること
	r, rerr = exec(`begin
	  for i in 1 .. 1200 loop dbms_output.put_line('L' || i); end loop;
	end;`)
	if rerr != nil {
		t.Fatalf("put_line 1200: %v", rerr)
	}
	if len(r.Output) != 1200 {
		t.Errorf("output が %d 行（1200 行のはず）", len(r.Output))
	} else if r.Output[1199] != "L1200" {
		t.Errorf("最後が %q", r.Output[1199])
	}

	// エラーで終わった文でも、その前に出た分は拾えること
	_, rerr = exec(`begin
	  dbms_output.put_line('途中まで出た');
	  raise_application_error(-20001, 'ここで失敗');
	end;`)
	if rerr == nil {
		t.Fatal("エラーにならなかった")
	}
	if out := s.mgr.All(); len(out) == 0 {
		t.Fatal("session が消えた")
	}
	// stmt/error の output はハンドラ側で LastOutput() を読んでいる。
	// ここでは Classify の結果だけ見る
	oe, ok := rerr.Data.(oradb.OraError)
	if !ok || oe.Code != 20001 {
		t.Fatalf("ORA-20001 ではない: %#v", rerr.Data)
	}
	// 【重要】エラーで終わった文でも、その前に出た DBMS_OUTPUT は見せる
	if len(oe.Output) != 1 || oe.Output[0] != "途中まで出た" {
		t.Errorf("エラー前の output が取れていない: %#v", oe.Output)
	}

	// 切り替え
	if _, rerr := s.Handle(c, raw(3), "session/serveroutput",
		raw(map[string]any{"session": sid, "on": false})); rerr != nil {
		t.Fatalf("serveroutput off: %v", rerr)
	}
	r, rerr = exec("begin dbms_output.put_line('出ないはず'); end;")
	if rerr != nil {
		t.Fatalf("put_line: %v", rerr)
	}
	if len(r.Output) != 0 {
		t.Errorf("OFF なのに取れている: %v", r.Output)
	}
}

// meta/describe は【重要】1 往復で全部を引く。
// 列・コメント・制約・索引・統計を別々に引くと、この回線では
// DESCRIBE 1 回で 0.7〜1 秒になる（docs/phase0-findings.md F）。
func TestLiveDescribe(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	exec := func(text string) *rpc.Error {
		_, rerr := s.Handle(c, raw(2), "sql/exec",
			raw(map[string]any{"session": sid, "text": text}))
		return rerr
	}
	_ = exec("drop table ORACLED_D1 purge")
	if rerr := exec(`create table ORACLED_D1 (
	    id    number(10)    not null,
	    code  varchar2(20)  not null,
	    name  varchar2(100),
	    qty   number(12,2),
	    constraint PK_ORACLED_D1 primary key (id),
	    constraint UQ_ORACLED_D1 unique (code))`); rerr != nil {
		t.Fatalf("create: %v", rerr)
	}
	defer exec("drop table ORACLED_D1 purge")
	if rerr := exec("create index IX_ORACLED_D1 on ORACLED_D1 (name, qty)"); rerr != nil {
		t.Fatalf("create index: %v", rerr)
	}
	if rerr := exec("comment on table ORACLED_D1 is '検証用の表'"); rerr != nil {
		t.Fatalf("comment: %v", rerr)
	}
	if rerr := exec("comment on column ORACLED_D1.name is '名前（日本語）'"); rerr != nil {
		t.Fatalf("comment column: %v", rerr)
	}

	// 既定（列とコメントだけ）
	start := time.Now()
	res, rerr := s.Handle(c, raw(3), "meta/describe",
		raw(map[string]any{"session": sid, "name": "oracled_d1"}))
	if rerr != nil {
		t.Fatalf("meta/describe: %v", rerr)
	}
	light := res.(*oradb.DescribeResult)
	t.Logf("describe（既定）: %.1f ms", float64(time.Since(start).Milliseconds()))
	if len(light.Constraints) != 0 || len(light.Indexes) != 0 {
		t.Error("既定で制約・索引まで引いている（遅いので C-u のときだけ）")
	}

	// C-u 相当（制約・索引まで）
	start = time.Now()
	res, rerr = s.Handle(c, raw(4), "meta/describe",
		raw(map[string]any{"session": sid, "name": "oracled_d1", "full": true}))
	if rerr != nil {
		t.Fatalf("meta/describe full: %v", rerr)
	}
	d := res.(*oradb.DescribeResult)
	t.Logf("describe（full）: %.1f ms", float64(time.Since(start).Milliseconds()))

	if d.Name != "ORACLED_D1" || d.ObjectType != "TABLE" {
		t.Errorf("name=%q type=%q", d.Name, d.ObjectType)
	}
	if d.Comment != "検証用の表" {
		t.Errorf("表のコメントが %q", d.Comment)
	}
	if len(d.Columns) != 4 {
		t.Fatalf("列が %d（4 のはず）: %+v", len(d.Columns), d.Columns)
	}
	if d.Columns[0].Name != "ID" || d.Columns[0].Type != "NUMBER(10)" ||
		d.Columns[0].Nullable || d.Columns[0].PK != 1 {
		t.Errorf("1 列目が %+v", d.Columns[0])
	}
	if d.Columns[1].Type != "VARCHAR2(20)" {
		t.Errorf("code の型が %q", d.Columns[1].Type)
	}
	if d.Columns[2].Comment != "名前（日本語）" {
		t.Errorf("列コメントが %q", d.Columns[2].Comment)
	}
	if d.Columns[3].Type != "NUMBER(12,2)" {
		t.Errorf("qty の型が %q", d.Columns[3].Type)
	}
	// 制約（P と U）
	var hasPK, hasUQ bool
	for _, k := range d.Constraints {
		switch k.Type {
		case "P":
			hasPK = true
			if len(k.Columns) != 1 || k.Columns[0] != "ID" {
				t.Errorf("主キーの列が %v", k.Columns)
			}
		case "U":
			hasUQ = true
		}
	}
	if !hasPK || !hasUQ {
		t.Errorf("制約が取れていない: %+v", d.Constraints)
	}
	// 索引（主キー・一意制約の索引 + 自分で作ったもの）
	var found bool
	for _, i := range d.Indexes {
		if i.Name == "IX_ORACLED_D1" {
			found = true
			if len(i.Columns) != 2 || i.Columns[0] != "NAME" || i.Columns[1] != "QTY" {
				t.Errorf("索引の列が %v（順序も見る）", i.Columns)
			}
			if i.Unique {
				t.Error("一意索引ではないのに unique になっている")
			}
		}
	}
	if !found {
		t.Errorf("索引が取れていない: %+v", d.Indexes)
	}

	// 無いものは分かるように落ちる
	if _, rerr := s.Handle(c, raw(5), "meta/describe",
		raw(map[string]any{"session": sid, "name": "ORACLED_NO_SUCH"})); rerr == nil {
		t.Error("無いオブジェクトでエラーにならなかった")
	}
}

// meta/objects と meta/token（キャッシュの鍵）。
func TestLiveObjects(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	start := time.Now()
	res, rerr := s.Handle(c, raw(2), "meta/objects",
		raw(map[string]any{"session": sid}))
	if rerr != nil {
		t.Fatalf("meta/objects: %v", rerr)
	}
	o := res.(*oradb.ObjectsResult)
	t.Logf("objects: %d 件 %.1f ms token=%s", len(o.Objects),
		float64(time.Since(start).Milliseconds()), o.Token)
	if len(o.Objects) == 0 {
		t.Fatal("1 件も返らない")
	}
	if o.Token == "" {
		t.Error("token が空（キャッシュの鍵に使えない)")
	}
	for _, x := range o.Objects {
		if x.Name == "" || x.Type == "" {
			t.Errorf("空の要素: %+v", x)
			break
		}
	}

	// token だけを引く経路は、一覧と同じ値を返す
	res, rerr = s.Handle(c, raw(3), "meta/token", raw(map[string]any{"session": sid}))
	if rerr != nil {
		t.Fatalf("meta/token: %v", rerr)
	}
	if got := res.(map[string]any)["token"]; got != o.Token {
		t.Errorf("token が食い違う: %v / %v", got, o.Token)
	}

	// DDL を打つと token が変わる（キャッシュが古いと分かる）
	before := o.Token
	_, _ = s.Handle(c, raw(4), "sql/exec",
		raw(map[string]any{"session": sid, "text": "create table ORACLED_TOK (id number)"}))
	defer s.Handle(c, raw(98), "sql/exec",
		raw(map[string]any{"session": sid, "text": "drop table ORACLED_TOK purge"}))
	res, rerr = s.Handle(c, raw(5), "meta/token", raw(map[string]any{"session": sid}))
	if rerr != nil {
		t.Fatalf("meta/token: %v", rerr)
	}
	if got := res.(map[string]any)["token"]; got == before {
		t.Errorf("DDL のあとも token が同じ: %v", got)
	}
}

// バインド変数（本物のバインド。`&var` の文字列置換ではない）。
func TestLiveBinds(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, buf := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	// sql/binds が文から名前を拾う
	res, rerr := s.Handle(c, raw(2), "sql/binds",
		raw(map[string]any{"text": "select * from t where a = :a and b = :b"}))
	if rerr != nil {
		t.Fatalf("sql/binds: %v", rerr)
	}
	if got := res.(map[string]any)["binds"].([]string); len(got) != 2 ||
		got[0] != "a" || got[1] != "b" {
		t.Errorf("binds = %v", got)
	}

	// 実際に値を渡して実行する（日本語・数値・NULL）
	_, rerr = s.Handle(c, raw(3), "sql/exec", raw(map[string]any{
		"session": sid,
		"text": `select :jp as jp, :n as n, nvl(:nul, 'それは NULL') as nul,
		                length(:jp) as len from dual`,
		"binds": []map[string]any{
			{"name": "jp", "value": "あいう漢字"},
			{"name": "n", "value": 42},
			{"name": "nul", "value": nil},
		},
	}))
	if rerr != nil {
		t.Fatalf("sql/exec with binds: %v", rerr)
	}
	var rows [][]any
	for _, m := range notifications(t, buf) {
		if m.Method != "rows/chunk" {
			continue
		}
		var p struct {
			Rows [][]any `json:"rows"`
		}
		json.Unmarshal(m.Params, &p)
		rows = append(rows, p.Rows...)
	}
	if len(rows) != 1 {
		t.Fatalf("行が %d", len(rows))
	}
	if rows[0][0] != "あいう漢字" {
		t.Errorf("日本語のバインドが %#v", rows[0][0])
	}
	if rows[0][1] != "42" {
		t.Errorf("数値のバインドが %#v（NUMBER は文字列で返る）", rows[0][1])
	}
	if rows[0][2] != "それは NULL" {
		t.Errorf("NULL のバインドが %#v", rows[0][2])
	}
	if rows[0][3] != "5" {
		t.Errorf("length(:jp) が %#v（文字数で 5 のはず）", rows[0][3])
	}

	// 【重要】文字列として連結されていないこと（インジェクションにならない）
	_, rerr = s.Handle(c, raw(4), "sql/exec", raw(map[string]any{
		"session": sid,
		"text":    "select :v as v from dual",
		"binds":   []map[string]any{{"name": "v", "value": "' or 1=1 --"}},
	}))
	if rerr != nil {
		t.Errorf("引用符を含む値で落ちた: %v", rerr)
	}
}

// LOB は上限を掛けて返す（巨大な CLOB が stdio を埋めないように）。
func TestLiveLobLimit(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, buf := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	_, rerr := s.Handle(c, raw(2), "sql/exec", raw(map[string]any{
		"session": sid,
		// 【重要】VARCHAR2 には 4000 バイトの上限があるので、日本語で
		// 3000 文字は作れない（rpad が途中で止まる）。CLOB で作る
		"text":      "select to_clob(rpad('x', 3000, 'x')) || 'あいう' as big from dual",
		"lob_limit": 100,
	}))
	if rerr != nil {
		t.Fatalf("sql/exec: %v", rerr)
	}
	var rows [][]any
	for _, m := range notifications(t, buf) {
		if m.Method != "rows/chunk" {
			continue
		}
		var p struct {
			Rows [][]any `json:"rows"`
		}
		json.Unmarshal(m.Params, &p)
		rows = append(rows, p.Rows...)
	}
	if len(rows) != 1 {
		t.Fatalf("行が %d", len(rows))
	}
	cell, ok := rows[0][0].(map[string]any)
	if !ok {
		t.Fatalf("切り詰められていない: %T", rows[0][0])
	}
	if cell["$trunc"] != true {
		t.Errorf("$trunc が立っていない: %#v", cell)
	}
	if n := len([]rune(cell["text"].(string))); n != 100 {
		t.Errorf("切り詰めた長さが %d（100 のはず）", n)
	}
	if cell["len"].(float64) != 3003 {
		t.Errorf("元の長さが %v（3003 のはず）", cell["len"])
	}
}

// EXPLAIN PLAN（文は実行しない）。
func TestLiveExplain(t *testing.T) {
	s := &server{mgr: oradb.NewManager(), start: time.Now()}
	c, _ := capture()
	sid := openSession(t, s, c)
	defer s.Handle(c, raw(99), "session/close", raw(map[string]any{"session": sid}))

	start := time.Now()
	res, rerr := s.Handle(c, raw(2), "plan/explain", raw(map[string]any{
		"session": sid,
		"text":    "select * from all_objects where object_name = :name",
	}))
	if rerr != nil {
		t.Fatalf("plan/explain: %v", rerr)
	}
	lines := res.(map[string]any)["lines"].([]string)
	t.Logf("explain: %d 行 %.0f ms", len(lines), float64(time.Since(start).Milliseconds()))
	if len(lines) < 5 {
		t.Fatalf("行が少なすぎる: %v", lines)
	}
	joined := strings.Join(lines, "\n")
	if !strings.Contains(joined, "Id") || !strings.Contains(joined, "Operation") {
		t.Errorf("計画の見出しが無い:\n%s", joined)
	}
	// バインド変数を含む文でも、値を渡さずに計画が取れる
	if _, rerr := s.Handle(c, raw(3), "plan/explain", raw(map[string]any{
		"session": sid, "text": "update ORACLED_NOPE set x = :v where id = :id"})); rerr == nil {
		t.Error("無い表の計画が取れてしまった")
	}
}
