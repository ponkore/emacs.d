// Package oradb は Oracle への接続を session として束ねる。
//
// 設計上の約束（設計 §3 / §6、実測は docs/phase0-findings.md）。
//
//   - **プールを使わない。** session は `*sql.Conn` を 1 本握り続ける。
//     文ごとに別セッションへ飛ぶと tx / ALTER SESSION / DBMS_OUTPUT が壊れる
//   - **autocommit OFF は `*sql.Tx` でしか作れない。** go-ora の
//     connection.autoCommit は既定 true で、Begin で false になる
//   - **閉じる前に必ず ROLLBACK。** 切断時の挙動は条件で変わる（実測では
//     ロールバックされたが、issue #767 はコミットを報告している）
//   - **DDL は保留中の DML を巻き込んでコミットする。** 起きたことを
//     呼び出し側に返して Emacs に出させる
package oradb

import (
	"context"
	"database/sql"
	"encoding/base64"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"time"

	go_ora "github.com/sijms/go-ora/v2"

	"oracled/internal/sqlsplit"
)

// OpenParams は session/open のパラメータ。
// 【重要】Password はログに出さない。DSN も組んだ後は持ち回さない。
type OpenParams struct {
	Host         string `json:"host"`
	Port         int    `json:"port"`
	Service      string `json:"service"`
	SID          string `json:"sid"`
	User         string `json:"user"`
	Password     string `json:"password"`
	AutoCommit   bool   `json:"autocommit"`
	ServerOutput bool   `json:"serveroutput"`
	OOB          bool   `json:"oob"`
}

// Info は接続して分かること。Emacs の header-line に出す材料。
type Info struct {
	Session    string `json:"session"`
	User       string `json:"user"`
	DB         string `json:"db"`
	Container  string `json:"container"`
	Version    string `json:"version"`
	Charset    string `json:"charset"`
	SID        string `json:"sid"`
	Serial     string `json:"serial"`
	AutoCommit bool   `json:"autocommit"`
	InTx       bool   `json:"in_tx"`
	// CanCancelSQL は ALTER SYSTEM CANCEL SQL が使えるか（割り込みの段 2）。
	// 【重要】判定は session_privs。user_sys_privs は直接付与だけなので
	// ロール経由の権限を見落とす（実測）。
	CanCancelSQL bool    `json:"can_cancel_sql"`
	ServerOutput bool    `json:"serveroutput"`
	ConnectMS    float64 `json:"connect_ms"`
}

type Column struct {
	Name      string `json:"name"`
	Type      string `json:"type"`
	Precision int64  `json:"precision,omitempty"`
	Scale     int64  `json:"scale,omitempty"`
	Nullable  bool   `json:"nullable"`
	// Numeric / Temporal は Emacs 側が桁揃えと書式を決めるための印。
	// 【重要】NUMBER は値が文字列で来るので、これが無いと右詰めにできない。
	Numeric  bool `json:"numeric"`
	Temporal bool `json:"temporal"`
}

// Result は 1 文の実行結果（行そのものは通知で先に流す）。
type Result struct {
	Query int    `json:"query"`
	Kind  string `json:"kind"` // "query" | "exec"
	// StmtKind は文の分類（"query" / "dml" / "plsql" / "ddl" / "tx"）。
	// Emacs 側が表示の文言を決めるのに使う。
	StmtKind       string   `json:"stmt_kind"`
	Columns        []Column `json:"columns,omitempty"`
	Rows           int      `json:"rows"`
	Affected       int64    `json:"affected"`
	ElapsedMS      float64  `json:"elapsed_ms"`
	More           bool     `json:"more"` // カーソルが開いたまま残っている
	ImplicitCommit bool     `json:"implicit_commit"`
	InTx           bool     `json:"in_tx"`
	// Output は DBMS_OUTPUT の行（SERVEROUTPUT ON のときだけ）。
	Output []string `json:"output,omitempty"`
}

// Session は 1 つの接続。文は 1 本ずつしか走らせない。
type Session struct {
	ID   string
	info Info

	db   *sql.DB
	conn *sql.Conn

	// 【重要】tx と開いたままのカーソルは**セッション寿命の context**で作る。
	// 文の context で作ると、割り込み（または Exec から戻るときの cancel）で
	// database/sql が tx を道連れにし、次の rollback が
	// 「transaction has already been committed or rolled back」や
	// ORA-01013 になる。統合テストで 3 件ともこれが原因だった。
	baseCtx    context.Context
	baseCancel context.CancelFunc

	mu           sync.Mutex // info / tx / rows / cancel を触るとき
	tx           *sql.Tx
	autocommit   bool
	serverOutput bool
	lastOutput   []string // 直前の文の DBMS_OUTPUT（エラー時も読めるように）
	// dirty は「このトランザクションで実際に書いたか」。
	// 【重要】tx オブジェクトの有無で InTx を出すと、autocommit OFF では
	// SELECT を 1 回投げただけで TX と表示されてしまう（Oracle 側の
	// トランザクションは DML で初めて始まる）。往復を増やさずに近づける
	// ため、問い合わせ以外を実行したかを数えるだけにしてある。
	// SELECT ... FOR UPDATE は取りこぼす（実害は表示だけ）。
	dirty bool

	stmtMu   sync.Mutex // 文を 1 本ずつにする
	cancel   context.CancelFunc
	querySeq int

	open *openRows
}

// openRows は maxrows で止めたカーソル。rows/more で続きを取る。
type openRows struct {
	query  int
	rows   *sql.Rows
	cols   []Column
	sent   int
	seq    int
	cancel context.CancelFunc // このカーソルを捨てるときに呼ぶ
}

type Manager struct {
	mu   sync.Mutex
	seq  int
	list map[string]*Session
}

func NewManager() *Manager { return &Manager{list: map[string]*Session{}} }

func (m *Manager) Get(id string) (*Session, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	s, ok := m.list[id]
	return s, ok
}

func (m *Manager) All() []*Session {
	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]*Session, 0, len(m.list))
	for _, s := range m.list {
		out = append(out, s)
	}
	return out
}

// Open は接続して session を作る。
func (m *Manager) Open(ctx context.Context, p OpenParams) (*Session, error) {
	if p.Port == 0 {
		p.Port = 1521
	}
	opts := map[string]string{}
	if p.OOB {
		opts["ENABLE_OOB"] = "true"
	}
	svc := p.Service
	if p.SID != "" {
		opts["SID"] = p.SID
		svc = "" // SID 指定のときサービス名を残すと ORA-12514
	}
	url := go_ora.BuildUrl(p.Host, p.Port, svc, p.User, p.Password, opts)

	start := time.Now()
	db, err := sql.Open("oracle", url)
	if err != nil {
		return nil, err
	}
	db.SetMaxOpenConns(1) // 固定した 1 本だけ使う
	conn, err := db.Conn(ctx)
	if err != nil {
		db.Close()
		return nil, err
	}
	connectMS := float64(time.Since(start).Microseconds()) / 1000

	base, baseCancel := context.WithCancel(context.Background())
	m.mu.Lock()
	m.seq++
	id := fmt.Sprintf("s%d", m.seq)
	s := &Session{ID: id, db: db, conn: conn, autocommit: p.AutoCommit,
		baseCtx: base, baseCancel: baseCancel}
	m.list[id] = s
	m.mu.Unlock()

	s.info = Info{Session: id, AutoCommit: p.AutoCommit, ConnectMS: connectMS}
	s.loadInfo(ctx)
	if p.ServerOutput {
		if err := s.SetServerOutput(ctx, true); err != nil {
			// 繋がってはいるので、失敗は呼び出し側に返さず info で分かるようにする
			s.mu.Lock()
			s.info.ServerOutput = false
			s.mu.Unlock()
		}
	}
	return s, nil
}

// SetServerOutput は DBMS_OUTPUT の取得を入り切りする（SQL*Plus の
// `SET SERVEROUTPUT ON` 相当）。
func (s *Session) SetServerOutput(ctx context.Context, on bool) error {
	stmt := "begin dbms_output.disable; end;"
	if on {
		stmt = "begin dbms_output.enable(null); end;"
	}
	if _, err := s.conn.ExecContext(ctx, stmt); err != nil {
		return err
	}
	s.mu.Lock()
	s.serverOutput = on
	s.info.ServerOutput = on
	s.mu.Unlock()
	return nil
}

// 【重要】DBMS_OUTPUT は 1 行ずつ GET_LINE を呼ぶと 1 行 = 1 往復になる
// （この回線では 110 ms）。GET_LINES でまとめて取り、ブロックの中で
// 連結して 1 つの OUT で受ける（コレクションのバインドも避けられる）。
// → docs/phase0-findings.md F
const fetchOutputBlock = `declare
  l dbms_output.chararr;
  n integer := 500;
  buf varchar2(32767);
  stopped number := 0;
begin
  dbms_output.get_lines(l, n);
  for i in 1 .. n loop
    if length(buf) + nvl(length(l(i)), 0) + 1 > 32000 then
      stopped := 1;
      exit;
    end if;
    buf := buf || l(i) || chr(10);
  end loop;
  if n = 500 and stopped = 0 then stopped := 2; end if;
  :1 := buf;
  :2 := stopped;
end;`

// fetchOutput は溜まっている DBMS_OUTPUT を取る。
// stopped が非 0 なら続きがあるので、上限を決めて繰り返す。
func (s *Session) fetchOutput(ctx context.Context) []string {
	var out []string
	for round := 0; round < 10; round++ {
		buf := strings.Repeat(" ", 32767) // OUT の領域を先に確保しておく
		stopped := 0
		if _, err := s.conn.ExecContext(ctx, fetchOutputBlock,
			sql.Out{Dest: &buf}, sql.Out{Dest: &stopped}); err != nil {
			return out
		}
		text := strings.TrimRight(buf, " \x00")
		if text != "" {
			lines := strings.Split(strings.TrimRight(text, "\n"), "\n")
			out = append(out, lines...)
		}
		if stopped == 0 {
			break
		}
	}
	return out
}

// takeOutput は SERVEROUTPUT が ON のときだけ取る。
func (s *Session) takeOutput(ctx context.Context) []string {
	s.mu.Lock()
	on := s.serverOutput
	s.mu.Unlock()
	if !on {
		return nil
	}
	out := s.fetchOutput(ctx)
	s.mu.Lock()
	s.lastOutput = out
	s.mu.Unlock()
	return out
}

// LastOutput は直前の文の DBMS_OUTPUT を返す（エラー経路で使う）。
func (s *Session) LastOutput() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := s.lastOutput
	s.lastOutput = nil
	return out
}

// loadInfo は 1 文で分かるものをまとめて引く。
// 【重要】往復が 1 回 100 ms 前後かかる回線があるので、1 文に畳む（実測 F）。
func (s *Session) loadInfo(ctx context.Context) {
	const q = `select
	  user,
	  sys_context('userenv','db_name'),
	  sys_context('userenv','con_name'),
	  (select version from product_component_version where rownum = 1),
	  (select value from nls_database_parameters where parameter = 'NLS_CHARACTERSET'),
	  sys_context('userenv','sid'),
	  (select count(*) from session_privs where privilege = 'ALTER SYSTEM')
	from dual`
	var user, db, con, ver, cs, sid sql.NullString
	var canCancel sql.NullString
	if err := s.conn.QueryRowContext(ctx, q).
		Scan(&user, &db, &con, &ver, &cs, &sid, &canCancel); err != nil {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.info.User = user.String
	s.info.DB = db.String
	s.info.Container = con.String
	s.info.Version = ver.String
	s.info.Charset = cs.String
	s.info.SID = sid.String
	s.info.CanCancelSQL = canCancel.String != "0" && canCancel.String != ""

	// serial# は v$session が引けるときだけ。段 2 で要る。
	var serial sql.NullString
	_ = s.conn.QueryRowContext(ctx,
		"select serial# from v$session where sid = sys_context('userenv','sid')").
		Scan(&serial)
	s.info.Serial = serial.String
}

func (s *Session) Info() Info {
	s.mu.Lock()
	defer s.mu.Unlock()
	i := s.info
	i.InTx = s.tx != nil && s.dirty
	i.AutoCommit = s.autocommit
	return i
}

// SetAutoCommit は autocommit を切り替える。
// 【重要】トランザクション中は拒否する（暗黙コミットを起こさないため）。
func (s *Session) SetAutoCommit(on bool) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.tx != nil {
		return fmt.Errorf("トランザクション中は切り替えられない（先に commit か rollback）")
	}
	s.autocommit = on
	s.info.AutoCommit = on
	return nil
}

// ensureTx は autocommit OFF のとき tx を用意する。呼び出し側は mu を持たない。
// 【重要】tx は baseCtx（セッション寿命）で張る。文の context で張ると
// 割り込みで tx ごと道連れになる。
func (s *Session) ensureTx() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.autocommit || s.tx != nil {
		return nil
	}
	tx, err := s.conn.BeginTx(s.baseCtx, nil)
	if err != nil {
		return err
	}
	s.tx = tx
	return nil
}

// execer は tx があればそちら、無ければ conn を返す。
type execer interface {
	ExecContext(context.Context, string, ...any) (sql.Result, error)
	QueryContext(context.Context, string, ...any) (*sql.Rows, error)
}

func (s *Session) execer() execer {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.tx != nil {
		return s.tx
	}
	return s.conn
}

// Commit / Rollback は tx を閉じ、autocommit OFF なら次の tx を張り直す。
func (s *Session) Commit(ctx context.Context) error { return s.endTx(ctx, true) }

func (s *Session) Rollback(ctx context.Context) error { return s.endTx(ctx, false) }

func (s *Session) endTx(ctx context.Context, commit bool) error {
	s.mu.Lock()
	tx := s.tx
	s.tx = nil
	s.mu.Unlock()
	if tx == nil {
		return nil // tx が無いのは正常（autocommit ON か、まだ何も書いていない）
	}
	s.mu.Lock()
	s.dirty = false
	s.mu.Unlock()
	if commit {
		return tx.Commit()
	}
	return tx.Rollback()
}

// Cancel は走っている文を止める（割り込みの段 1）。
// 実測で CPU を回している文が 165〜181 ms で止まり、セッションは生き残る。
func (s *Session) Cancel() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.cancel == nil {
		return false
	}
	s.cancel()
	return true
}

// Close は【重要】明示 ROLLBACK してから閉じる。
// 切断時にコミットされるかは条件で変わる（実測ではロールバックされたが、
// go-ora issue #767 はコミットを報告している）。依存しない。
func (s *Session) Close() error {
	s.closeRows()
	err := s.Rollback(context.Background())
	s.conn.Close()
	if cerr := s.db.Close(); err == nil {
		err = cerr
	}
	s.baseCancel() // tx を閉じた後に（先に呼ぶと rollback が道連れになる）
	return err
}

func (m *Manager) Remove(id string) { m.mu.Lock(); delete(m.list, id); m.mu.Unlock() }

// CloseRows は開いたままのカーソルを閉じる（rows/close）。
func (s *Session) CloseRows() { s.closeRows() }

func (s *Session) closeRows() {
	s.mu.Lock()
	open := s.open
	s.open = nil
	s.mu.Unlock()
	if open != nil {
		open.rows.Close()
		if open.cancel != nil {
			open.cancel() // 残していた文の context を解放する
		}
	}
}

// Emit は行の束を送る関数。daemon が rows/chunk 通知に変える。
type Emit func(query, seq int, rows [][]any, done bool)

// Exec は 1 文を実行する。行は emit で先に流し、最後に Result を返す。
func (s *Session) Exec(ctx context.Context, text string, binds []any,
	maxRows, chunk int, emit Emit) (*Result, error) {

	if !s.stmtMu.TryLock() {
		return nil, fmt.Errorf("busy")
	}
	defer s.stmtMu.Unlock()

	// 前の文のカーソルは閉じる（同じ接続を占有しているため）。
	s.closeRows()

	// 文の context は baseCtx の子。cancel しても tx には波及しない。
	cctx, cancel := context.WithCancel(s.baseCtx)
	keep := false // カーソルを開いたまま残すなら cancel を手放す
	s.mu.Lock()
	s.querySeq++
	query := s.querySeq
	s.cancel = cancel
	s.mu.Unlock()
	defer func() {
		if !keep {
			cancel()
		}
		s.mu.Lock()
		s.cancel = nil
		s.mu.Unlock()
	}()

	if maxRows <= 0 {
		maxRows = 1000
	}
	if chunk <= 0 {
		chunk = 200
	}

	// COMMIT / ROLLBACK は daemon の tx 状態と食い違わないよう専用経路へ。
	if cmd, ok := sqlsplit.IsTxControl(text); ok && (cmd == "COMMIT" || cmd == "ROLLBACK") {
		start := time.Now()
		var err error
		if cmd == "COMMIT" {
			err = s.Commit(cctx)
		} else {
			err = s.Rollback(cctx)
		}
		if err != nil {
			return nil, err
		}
		return &Result{Query: query, Kind: "exec", StmtKind: "tx",
			ElapsedMS: msOf(time.Since(start)), InTx: s.Info().InTx}, nil
	}

	if err := s.ensureTx(); err != nil {
		return nil, err
	}
	ddl := sqlsplit.IsDDL(text)
	start := time.Now()

	if sqlsplit.IsQuery(text) {
		rows, err := s.execer().QueryContext(cctx, text, binds...)
		if err != nil {
			s.takeOutput(context.Background()) // エラーの前に出た分も拾う
			return nil, err
		}
		cols, err := columnsOf(rows)
		if err != nil {
			rows.Close()
			return nil, err
		}
		open := &openRows{query: query, rows: rows, cols: cols, cancel: cancel}
		n, more, err := s.drain(open, maxRows, chunk, emit)
		elapsed := msOf(time.Since(start))
		if err != nil {
			rows.Close()
			return nil, err
		}
		if more {
			// カーソルを残すので、この文の context も一緒に残す。
			// ここで cancel すると rows/more が "context canceled" になる。
			keep = true
			s.mu.Lock()
			s.open = open
			s.mu.Unlock()
		} else {
			rows.Close()
		}
		return &Result{Query: query, Kind: "query", StmtKind: "query",
			Columns: cols, Rows: n, ElapsedMS: elapsed, More: more,
			InTx: s.Info().InTx, Output: s.takeOutput(context.Background())}, nil
	}

	res, err := s.execer().ExecContext(cctx, text, binds...)
	elapsed := msOf(time.Since(start))
	if err != nil {
		s.takeOutput(context.Background())
		return nil, err
	}
	output := s.takeOutput(context.Background())
	affected, _ := res.RowsAffected()
	if !ddl {
		s.mu.Lock()
		s.dirty = true // 問い合わせ以外を実行した
		s.mu.Unlock()
	}

	// 【重要】DDL は保留中の DML を巻き込んでサーバ側でコミットされている。
	// こちらの tx オブジェクトは用済みなので閉じて張り直し、起きたことを返す。
	implicit := false
	if ddl {
		s.mu.Lock()
		had := s.tx != nil
		s.mu.Unlock()
		if had {
			_ = s.endTx(cctx, true)
			implicit = true
		}
	}
	return &Result{Query: query, Kind: "exec", StmtKind: sqlsplit.Classify(text),
		Affected: affected, ElapsedMS: elapsed, ImplicitCommit: implicit,
		InTx: s.Info().InTx, Output: output}, nil
}

// More は開いたままのカーソルから続きを取る。
func (s *Session) More(ctx context.Context, maxRows, chunk int, emit Emit) (*Result, error) {
	if !s.stmtMu.TryLock() {
		return nil, fmt.Errorf("busy")
	}
	defer s.stmtMu.Unlock()
	s.mu.Lock()
	open := s.open
	s.mu.Unlock()
	if open == nil {
		return nil, fmt.Errorf("開いているカーソルが無い")
	}
	// 取得中も $/cancel で止められるようにする。
	s.mu.Lock()
	s.cancel = open.cancel
	s.mu.Unlock()
	defer func() { s.mu.Lock(); s.cancel = nil; s.mu.Unlock() }()
	if maxRows <= 0 {
		maxRows = 1000
	}
	if chunk <= 0 {
		chunk = 200
	}
	start := time.Now()
	n, more, err := s.drain(open, maxRows, chunk, emit)
	if err != nil {
		s.closeRows()
		return nil, err
	}
	if !more {
		s.closeRows()
	}
	return &Result{Query: open.query, Kind: "query", Columns: open.cols, Rows: n,
		ElapsedMS: msOf(time.Since(start)), More: more, InTx: s.Info().InTx}, nil
}

// drain は最大 maxRows 行を chunk ごとに emit する。
// 戻り値の more は「まだ行が残っている」。
func (s *Session) drain(open *openRows, maxRows, chunk int, emit Emit) (int, bool, error) {
	rows := open.rows
	cts, _ := rows.ColumnTypes()
	buf := make([][]any, 0, chunk)
	n := 0
	for n < maxRows {
		if !rows.Next() {
			if err := rows.Err(); err != nil {
				return n, false, err
			}
			if len(buf) > 0 {
				open.seq++
				emit(open.query, open.seq, buf, true)
			} else {
				open.seq++
				emit(open.query, open.seq, nil, true)
			}
			return n, false, nil
		}
		vals := make([]any, len(open.cols))
		ptrs := make([]any, len(open.cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return n, false, err
		}
		for i := range vals {
			var ct *sql.ColumnType
			if i < len(cts) {
				ct = cts[i]
			}
			vals[i] = convert(vals[i], ct)
		}
		buf = append(buf, vals)
		n++
		open.sent++
		if len(buf) >= chunk {
			open.seq++
			emit(open.query, open.seq, buf, false)
			buf = make([][]any, 0, chunk)
		}
	}
	if len(buf) > 0 {
		open.seq++
		emit(open.query, open.seq, buf, false)
	}
	return n, true, nil
}

func columnsOf(rows *sql.Rows) ([]Column, error) {
	cts, err := rows.ColumnTypes()
	if err != nil {
		return nil, err
	}
	out := make([]Column, len(cts))
	for i, ct := range cts {
		c := Column{Name: ct.Name(), Type: ct.DatabaseTypeName()}
		if p, sc, ok := ct.DecimalSize(); ok {
			c.Precision, c.Scale = p, sc
		}
		if nullable, ok := ct.Nullable(); ok {
			c.Nullable = nullable
		}
		c.Numeric = isNumericType(c.Type)
		c.Temporal = isTemporalType(c.Type)
		out[i] = c
	}
	return out, nil
}

func isNumericType(t string) bool {
	return strings.HasPrefix(t, "NUMBER") || t == "IBDouble" || t == "IBFloat" ||
		t == "BINARY_DOUBLE" || t == "BINARY_FLOAT"
}

func isTemporalType(t string) bool {
	return t == "DATE" || strings.HasPrefix(t, "TIMESTAMP") ||
		strings.HasPrefix(t, "TimeStamp")
}

// hasZone は「オフセットが意味を持つ型」か。
// 【重要】go-ora は DATE / TIMESTAMP を「壁時計の時刻 + UTC の札」で返す。
// そのままオフセット付きで渡すと Emacs 側で 9 時間ずれる（実測 G）。
func hasZone(t string) bool {
	return strings.Contains(t, "TZ") || strings.Contains(t, "LTZ")
}

const wallClock = "2006-01-02T15:04:05.999999999"

func convert(v any, ct *sql.ColumnType) any {
	switch x := v.(type) {
	case nil:
		return nil
	case []byte:
		return map[string]any{"$b64": base64.StdEncoding.EncodeToString(x), "len": len(x)}
	case time.Time:
		if ct != nil && hasZone(ct.DatabaseTypeName()) {
			return x.Format(time.RFC3339Nano)
		}
		return x.Format(wallClock)
	case string, bool, float64, float32, int64, int32, int:
		return x
	default:
		return fmt.Sprint(x)
	}
}

func msOf(d time.Duration) float64 {
	return float64(d.Microseconds()) / 1000
}

// OraError は Oracle が返したエラーを構造にしたもの。
// 【重要】go-ora の errPos は非公開なので、位置は Error() の文字列から取る
// （`error occur at position: N`。0 始まりの文字オフセット。実測 C'）。
type OraError struct {
	Code     int    `json:"code"`
	Message  string `json:"message"`
	Position int    `json:"position"` // 取れなければ -1
	// Output はエラーの前に出た DBMS_OUTPUT（SQL*Plus と同じく見せる）。
	Output []string `json:"output,omitempty"`
}

const posMarker = " error occur at position: "

// Classify はドライバのエラーを構造化する。
func Classify(err error) OraError {
	e := OraError{Message: err.Error(), Position: -1, Code: 0}
	if i := strings.Index(e.Message, posMarker); i >= 0 {
		if n, cerr := strconv.Atoi(strings.TrimSpace(e.Message[i+len(posMarker):])); cerr == nil {
			e.Position = n
		}
		e.Message = strings.TrimRight(strings.TrimSpace(e.Message[:i]), "\n/")
	}
	// ORA-NNNNN を拾う
	if i := strings.Index(e.Message, "ORA-"); i >= 0 && len(e.Message) >= i+9 {
		if n, cerr := strconv.Atoi(e.Message[i+4 : i+9]); cerr == nil {
			e.Code = n
		}
	}
	e.Message = strings.TrimSpace(e.Message)
	return e
}
