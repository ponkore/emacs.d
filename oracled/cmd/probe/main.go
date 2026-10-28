// probe — Phase 0 の計測用コマンド。
//
// daemon を書く前に「この方式が本当に成立するか」を測る。見たいのは 5 つ。
//
//  1. 繋がるか・何 ms か                     info
//  2. 型がどう返るか（NUMBER / DATE / LOB）   types
//  3. 日本語が往復するか                      jp
//  4. 走っている文を割り込めるか               cancel
//  5. PL/SQL と DBMS_OUTPUT が取れるか        plsql
//
// → 設計: docs/oracle-client-design.md §17「段階」の Phase 0
//
// 【重要】パスワードは引数で受け取らない（tasklist から見える）。
// 環境変数 ORACLED_PASSWORD だけから読む。daemon では stdio の RPC で渡す。
package main

import (
	"context"
	"database/sql"
	"errors"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"

	go_ora "github.com/sijms/go-ora/v2"
)

var (
	host    = flag.String("host", "192.168.66.101", "接続先ホスト")
	port    = flag.Int("port", 1521, "ポート")
	service = flag.String("service", "XEPDB1", "サービス名")
	sid     = flag.String("sid", "", "SID（古い DB はサービス名ではなくこちら）")
	user    = flag.String("user", "TEST2TAX", "ユーザー")
	after   = flag.Duration("after", 1500*time.Millisecond, "cancel: 何 ms 後に割り込むか")
	timeout = flag.Duration("timeout", 60*time.Second, "1 コマンドの上限")
	sqlText = flag.String("sql", "", "query: 投げる SQL")
	// 【重要】go-ora は ENABLE_OOB が無いと urgent data を名乗らない
	// （session_ctx.go: Options |= 1024 が EnableOOB のときだけ）。
	// そして Windows の sendOOB は中身が全部コメントアウトされた空実装で、
	// 必ず (false, nil) を返す（network/net_windows.go）。つまり Windows では
	// このフラグを立てても in-band marker の経路に落ちるはず。その確認用。
	oob    = flag.Bool("oob", false, "DSN に ENABLE_OOB=true を足す")
	heavyN = flag.Int("heavy", 30000000, "cancel: 重い文の反復回数")
)

// verdict は「割り込み要求から戻るまでの時間」を基準時間と比べて、
// 本当に止まったのかを言う。この判定が無いと、自然終了を
// 「割り込みが効いた」と読み違える。
func verdict(elapsed, base, after time.Duration) string {
	switch {
	case elapsed < 0:
		return "判定不能"
	case elapsed < base/4:
		return "**止まった**（基準の 1/4 未満で戻った）"
	case elapsed > base-after-base/4:
		return "**止まっていない**（自然終了を待っただけ）"
	default:
		return "あいまい（-heavy を増やして再測）"
	}
}

func main() {
	flag.Usage = usage
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		usage()
		os.Exit(2)
	}
	pass := os.Getenv("ORACLED_PASSWORD")
	if pass == "" {
		fatal(errors.New("環境変数 ORACLED_PASSWORD が空"))
	}

	ctx, cancel := context.WithTimeout(context.Background(), *timeout)
	defer cancel()

	opts := map[string]string{}
	if *oob {
		opts["ENABLE_OOB"] = "true"
	}
	// SID 指定のときはサービス名を空にする（ORA-12514 になる）。
	svc := *service
	if *sid != "" {
		opts["SID"] = *sid
		svc = ""
	}
	url := go_ora.BuildUrl(*host, *port, svc, *user, pass, opts)
	db, err := sql.Open("oracle", url)
	if err != nil {
		fatal(err)
	}
	// 【重要】プールに任せると文ごとに別セッションに飛びうる。
	// session は 1 本の *sql.Conn を握り続ける（設計 §3）。
	db.SetMaxOpenConns(1)
	defer db.Close()

	t0 := time.Now()
	conn, err := db.Conn(ctx)
	if err != nil {
		fatal(err)
	}
	connectMS := ms(time.Since(t0))
	defer conn.Close()

	fmt.Printf("# %s@%s:%d/%s%s  (ENABLE_OOB=%v)\n", *user, *host, *port, svc,
		map[bool]string{true: " SID=" + *sid, false: ""}[*sid != ""], *oob)
	fmt.Printf("接続: %s\n\n", connectMS)

	var cmdErr error
	switch args[0] {
	case "info":
		cmdErr = cmdInfo(ctx, conn)
	case "types":
		cmdErr = cmdTypes(ctx, conn)
	case "jp":
		cmdErr = cmdJP(ctx, conn)
	case "cancel":
		cmdErr = cmdCancel(ctx, conn, db)
	case "plsql":
		cmdErr = cmdPLSQL(ctx, conn)
	case "writes":
		cmdErr = cmdWrites(ctx, conn, url)
	case "bench":
		cmdErr = cmdBench(ctx, conn)
	case "query":
		cmdErr = cmdQuery(ctx, conn, *sqlText)
	case "exec":
		cmdErr = cmdExec(ctx, conn, *sqlText)
	default:
		usage()
		os.Exit(2)
	}
	if cmdErr != nil {
		fatal(cmdErr)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, `probe — Phase 0 の計測

  probe [フラグ] info|types|jp|cancel|plsql|writes|bench|query|exec

パスワードは環境変数 ORACLED_PASSWORD から読む（引数では渡せない）。`)
	flag.PrintDefaults()
}

// ---------------------------------------------------------------- info

func cmdInfo(ctx context.Context, conn *sql.Conn) error {
	// 版・キャラクタセット・セッション識別子。
	// v$ 系は権限が無いことがあるので、落ちても続ける。
	probes := []struct {
		label string
		query string
	}{
		{"版", "select version_full from product_component_version where rownum = 1"},
		{"版 (fallback)", "select version from product_component_version where rownum = 1"},
		{"DB charset", "select value from nls_database_parameters where parameter = 'NLS_CHARACTERSET'"},
		{"NCHAR charset", "select value from nls_database_parameters where parameter = 'NLS_NCHAR_CHARACTERSET'"},
		{"セッション charset", "select userenv('language') from dual"},
		{"user / db / con", "select user ||' / '|| sys_context('userenv','db_name') ||' / '|| sys_context('userenv','con_name') from dual"},
		{"sid", "select sys_context('userenv','sid') from dual"},
		// 段 2（ALTER SYSTEM CANCEL SQL）が使えるかの下見。
		// sid, serial# が取れなければ段 2 は最初から無い。
		{"sid, serial#", "select sid ||','|| serial# from v$session where sid = sys_context('userenv','sid')"},
		{"ALTER SYSTEM 権限", "select count(*) from user_sys_privs where privilege = 'ALTER SYSTEM'"},
	}
	for _, p := range probes {
		var v sql.NullString
		if err := conn.QueryRowContext(ctx, p.query).Scan(&v); err != nil {
			fmt.Printf("%-18s ! %s\n", p.label, oneLine(err.Error()))
			continue
		}
		fmt.Printf("%-18s %s\n", p.label, v.String)
	}

	// 【重要】末尾の `;` は通らない。lexer が落とす必要がある（設計 §7）。
	fmt.Println()
	var one int
	err := conn.QueryRowContext(ctx, "select 1 from dual;").Scan(&one)
	fmt.Printf("末尾の `;`        %s\n", result(err))
	err = conn.QueryRowContext(ctx, "select 1 from dual").Scan(&one)
	fmt.Printf("終端子なし        %s\n", result(err))
	return nil
}

// ---------------------------------------------------------------- types

func cmdTypes(ctx context.Context, conn *sql.Conn) error {
	cases := []string{
		"select 1 as n from dual",
		"select 42 / 1 as n from dual",
		"select 1/3 as n from dual",
		"select cast(42 as number(10,0)) as n from dual",
		"select cast(1.25 as number(10,2)) as n from dual",
		"select cast(1234567890123456789 as number(19,0)) as n from dual",
		"select 123456789012345678901234567890 as n from dual",
		"select cast(1.5 as binary_double) as n from dual",
		"select cast(1.5 as binary_float) as n from dual",
		"select sysdate as d from dual",
		"select systimestamp as ts from dual",
		"select localtimestamp as ts from dual",
		"select to_timestamp('2026-10-28 12:34:56.789','YYYY-MM-DD HH24:MI:SS.FF3') as ts from dual",
		"select interval '1' day as iv from dual",
		"select interval '2-3' year to month as iv from dual",
		"select hextoraw('DEADBEEF') as r from dual",
		"select to_clob('あいうえお') as c from dual",
		"select to_blob(hextoraw('DEADBEEF')) as b from dual",
		"select rowid as rid from dual",
		"select cast(null as varchar2(10)) as v from dual",
		"select '' as v from dual",
		"select 'あいう' as v from dual",
		"select cast('あいう' as nvarchar2(10)) as v from dual",
		"select cast('abc' as char(10)) as v from dual",
	}
	for _, q := range cases {
		dumpOne(ctx, conn, q)
	}
	return nil
}

// dumpOne は 1 列 1 行の問い合わせについて、
// Oracle 側の型名と Go 側の型・値を並べて出す。
func dumpOne(ctx context.Context, conn *sql.Conn, query string) {
	short := strings.TrimPrefix(query, "select ")
	short = strings.TrimSuffix(short, " from dual")
	rows, err := conn.QueryContext(ctx, query)
	if err != nil {
		fmt.Printf("%-62s ! %s\n", short, oneLine(err.Error()))
		return
	}
	defer rows.Close()
	cts, _ := rows.ColumnTypes()
	ora := ""
	if len(cts) > 0 {
		ora = cts[0].DatabaseTypeName()
		if p, s, ok := cts[0].DecimalSize(); ok {
			ora += fmt.Sprintf("(%d,%d)", p, s)
		}
		if n, ok := cts[0].Nullable(); ok && !n {
			ora += " NOT NULL"
		}
		if rt := cts[0].ScanType(); rt != nil {
			ora += " scan=" + rt.String()
		}
	}
	if !rows.Next() {
		fmt.Printf("%-62s %-28s （行が無い）\n", short, ora)
		return
	}
	var v any
	if err := rows.Scan(&v); err != nil {
		fmt.Printf("%-62s %-28s ! scan: %s\n", short, ora, oneLine(err.Error()))
		return
	}
	fmt.Printf("%-62s %-28s %-22T %s\n", short, ora, v, render(v))
}

// render は Go 側の値を、NULL と空文字が見分けられる形で文字列にする。
func render(v any) string {
	switch x := v.(type) {
	case nil:
		return "<nil>"
	case []byte:
		return fmt.Sprintf("[]byte(%d) %x", len(x), x)
	case string:
		return fmt.Sprintf("%q (len=%d)", x, len(x))
	case time.Time:
		return x.Format("2006-01-02T15:04:05.999999999Z07:00")
	default:
		return fmt.Sprintf("%v", x)
	}
}

// ---------------------------------------------------------------- jp

func cmdJP(ctx context.Context, conn *sql.Conn) error {
	// DUMP(..., 1016) で中身のバイト列を見る。リテラルとバインドで
	// 同じになるかが要点（SQLcl 版では stdin だけ cp932 で食い違っていた）。
	samples := []struct {
		label string
		value string
	}{
		{"ひらがな", "あ"},
		{"漢字", "日本語"},
		{"cp932 の外", "𠮟"}, // サロゲートペア
		{"絵文字", "😀"},
		{"全角記号", "〜—−"}, // 機種依存文字（my-japanese.el が cp932 で変換している組）
	}
	for _, s := range samples {
		var litDump, bindDump, echoed sql.NullString
		q := `select dump(:1, 1016) as bind_dump, :1 as echoed from dual`
		if err := conn.QueryRowContext(ctx, q, s.value).Scan(&bindDump, &echoed); err != nil {
			fmt.Printf("%-12s ! bind: %s\n", s.label, oneLine(err.Error()))
			continue
		}
		// リテラルとして埋め込んだ場合（SQL 文そのものの符号化を見る）
		lq := fmt.Sprintf("select dump('%s', 1016) from dual", s.value)
		if err := conn.QueryRowContext(ctx, lq).Scan(&litDump); err != nil {
			litDump.String = "! " + oneLine(err.Error())
		}
		// DUMP の戻りは "Typ=N Len=M CharacterSet=X: b1,b2,…"。
		// bind は VARCHAR2（Typ=1）、リテラルは CHAR（Typ=96）になるので、
		// 比べるのは `: ` の後ろのバイト列だけ。
		same := "バイト列 一致"
		if dumpBytes(litDump.String) != dumpBytes(bindDump.String) {
			same = "バイト列 **不一致**"
		}
		fmt.Printf("%-12s %-10s\n  bind    %s\n  literal %s\n  echo    %q (%s)\n",
			s.label, s.value, bindDump.String, litDump.String, echoed.String,
			map[bool]string{true: "往復 OK", false: "**化けた**"}[echoed.String == s.value])
		fmt.Printf("  判定    %s\n", same)
	}

	// NVARCHAR2 経路（NCHAR のキャラクタセットは別）
	var nd sql.NullString
	if err := conn.QueryRowContext(ctx,
		`select dump(cast(:1 as nvarchar2(10)), 1016) from dual`, "あ").Scan(&nd); err != nil {
		fmt.Printf("nvarchar2    ! %s\n", oneLine(err.Error()))
	} else {
		fmt.Printf("nvarchar2    %s\n", nd.String)
	}
	return nil
}

// dumpBytes は DUMP() の戻りからバイト列の部分だけを取り出す。
func dumpBytes(s string) string {
	if i := strings.LastIndex(s, ": "); i >= 0 {
		return s[i+2:]
	}
	return s
}

// ---------------------------------------------------------------- plsql

func cmdPLSQL(ctx context.Context, conn *sql.Conn) error {
	// DBMS_OUTPUT が取れるかは Phase 2 の前提（設計 §13）。
	// OUT パラメータに database/sql の sql.Out がそのまま使えるかも見る。
	if _, err := conn.ExecContext(ctx, `begin dbms_output.enable(null); end;`); err != nil {
		return fmt.Errorf("enable: %w", err)
	}
	if _, err := conn.ExecContext(ctx,
		`begin dbms_output.put_line('あいうえお'); dbms_output.put_line('line 2'); end;`); err != nil {
		return fmt.Errorf("put_line: %w", err)
	}

	fmt.Println("[1] sql.Out で get_line")
	for i := 0; i < 4; i++ {
		line := strings.Repeat(" ", 32767) // 領域を先に確保しておく（size のヒント代わり）
		status := 0
		_, err := conn.ExecContext(ctx, `begin dbms_output.get_line(:1, :2); end;`,
			sql.Out{Dest: &line}, sql.Out{Dest: &status})
		if err != nil {
			fmt.Printf("  ! %s\n", oneLine(err.Error()))
			break
		}
		fmt.Printf("  status=%d line=%q\n", status, strings.TrimRight(line, " \x00"))
		if status != 0 {
			break
		}
	}

	fmt.Println("[2] go_ora.Out で get_line（driver 固有の型が必要かの確認）")
	if _, err := conn.ExecContext(ctx,
		`begin dbms_output.put_line('ふたたび'); end;`); err == nil {
		var line string
		var status int
		_, err := conn.ExecContext(ctx, `begin dbms_output.get_line(:1, :2); end;`,
			go_ora.Out{Dest: &line, Size: 32767}, go_ora.Out{Dest: &status})
		if err != nil {
			fmt.Printf("  ! %s\n", oneLine(err.Error()))
		} else {
			fmt.Printf("  status=%d line=%q\n", status, line)
		}
	}

	fmt.Println("[3] エラーの形（PL/SQL のスタックが取れるか）")
	_, err := conn.ExecContext(ctx,
		`begin raise_application_error(-20001, 'テスト例外'); end;`)
	fmt.Printf("  %s\n", describe(err))

	fmt.Println("[4] 構文エラーの位置（設計 §8 の offset が取れるか）")
	_, err = conn.ExecContext(ctx, `select foo from dual`)
	fmt.Printf("  %s\n", describe(err))
	_, err = conn.ExecContext(ctx, `select * from このテーブルは無い`)
	fmt.Printf("  %s\n", describe(err))
	return nil
}

// ---------------------------------------------------------------- writes

// cmdWrites は書き込み経路の前提を 2 つ確かめる。
//
//	(a) トランザクションの外の DML が即コミットされるか
//	    go-ora の connection.autoCommit は既定 true で、Begin で false、
//	    Commit/Rollback で true に戻る（ソース確認済み）。つまり
//	    「autocommit OFF」は *sql.Tx の中で実行する形でしか作れない。
//	(b) 【重要】未コミットのまま正常 logoff するとコミットされるか
//	    （設計 §6。ここが真なら、閉じる全経路に明示 ROLLBACK が必須）
//
// 検証用の表 ORACLED_PROBE を作って最後に落とす。
func cmdWrites(ctx context.Context, conn *sql.Conn, url string) error {
	const table = "ORACLED_PROBE"

	// 別接続（観測者）。コミットされたものだけが見える。
	obsDB, err := sql.Open("oracle", url)
	if err != nil {
		return err
	}
	defer obsDB.Close()
	obsDB.SetMaxOpenConns(1)
	obs, err := obsDB.Conn(ctx)
	if err != nil {
		return err
	}
	defer obs.Close()

	count := func(label string) {
		var n int
		// 親 ctx には触らない（前の文の watchdog が break を投げてくると
		// ここが ORA-01013 になる。実際にそれで 2 回測り直した）
		err := obs.QueryRowContext(context.Background(),
			"select count(*) from "+table).Scan(&n)
		if err != nil {
			fmt.Printf("  %-44s ! %s\n", label, oneLine(err.Error()))
			return
		}
		fmt.Printf("  %-44s %d 件\n", label, n)
	}

	_, _ = conn.ExecContext(ctx, "drop table "+table+" purge")
	if _, err := conn.ExecContext(ctx,
		"create table "+table+" (id number(10), note varchar2(100))"); err != nil {
		return fmt.Errorf("create: %w", err)
	}
	defer func() {
		if _, err := conn.ExecContext(context.Background(),
			"drop table "+table+" purge"); err != nil {
			fmt.Printf("\n! 後始末に失敗（手で drop table %s purge）: %s\n",
				table, oneLine(err.Error()))
		} else {
			fmt.Printf("\n後始末: drop table %s purge\n", table)
		}
	}()

	fmt.Println("[1] Tx の外で INSERT（go-ora の autoCommit = true のはず）")
	if _, err := conn.ExecContext(ctx,
		"insert into "+table+" (id, note) values (1, 'outside tx')"); err != nil {
		return err
	}
	count("別接続から見える件数（INSERT 直後）")
	if _, err := conn.ExecContext(ctx, "rollback"); err != nil {
		fmt.Printf("  rollback: %s\n", result(err))
	}
	count("別接続から見える件数（rollback 後）")

	fmt.Println("[2] BeginTx の中で INSERT")
	tx, err := conn.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	if _, err := tx.ExecContext(ctx,
		"insert into "+table+" (id, note) values (2, 'in tx')"); err != nil {
		return err
	}
	count("別接続から見える件数（コミット前）")
	if err := tx.Rollback(); err != nil {
		fmt.Printf("  Rollback: %s\n", result(err))
	}
	count("別接続から見える件数（Rollback 後）")

	fmt.Println("[3] BeginTx → Commit")
	tx, err = conn.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	if _, err := tx.ExecContext(ctx,
		"insert into "+table+" (id, note) values (3, 'committed')"); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return err
	}
	count("別接続から見える件数（Commit 後）")

	// 【重要】ここが設計 §6 の裏取り。
	fmt.Println("[4] 未コミットのまま正常に切断（logoff コミットの確認）")
	lgDB, err := sql.Open("oracle", url)
	if err != nil {
		return err
	}
	lgDB.SetMaxOpenConns(1)
	lgConn, err := lgDB.Conn(ctx)
	if err != nil {
		return err
	}
	lgTx, err := lgConn.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	if _, err := lgTx.ExecContext(ctx,
		"insert into "+table+" (id, note) values (4, 'never committed')"); err != nil {
		return err
	}
	count("別接続から見える件数（切断前）")
	// Tx を抱えたまま、素直に閉じる（Rollback も Commit も呼ばない）。
	// *sql.Conn の Close は open Tx と相互作用するので DB.Close だけにする
	_ = lgTx
	if err := lgDB.Close(); err != nil {
		fmt.Printf("  DB.Close: %s\n", result(err))
	}
	time.Sleep(500 * time.Millisecond) // サーバ側の後始末を待つ
	count("別接続から見える件数（切断後）")
	fmt.Println("  → ここで 1 件増えていたら logoff コミット。")
	fmt.Println("    閉じる全経路に明示 ROLLBACK が必須（設計 §6）")

	fmt.Printf("[5] DDL は tx の中でも暗黙コミットされるか（親 ctx: %v）\n", ctx.Err())
	ctx = context.Background()
	tx, err = conn.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	if _, err := tx.ExecContext(ctx,
		"insert into "+table+" (id, note) values (5, 'before ddl')"); err != nil {
		return err
	}
	if _, err := tx.ExecContext(ctx, "create table "+table+"_X (id number)"); err != nil {
		fmt.Printf("  DDL: %s\n", result(err))
	}
	count("別接続から見える件数（DDL 後・Commit 前）")
	_ = tx.Rollback()
	count("別接続から見える件数（Rollback 後）")
	if _, err := conn.ExecContext(ctx, "drop table "+table+"_X purge"); err != nil {
		fmt.Printf("  %s_X の後始末: %s\n", table, result(err))
	}
	return nil
}

// ---------------------------------------------------------------- bench

func cmdBench(ctx context.Context, conn *sql.Conn) error {
	const n = 100
	start := time.Now()
	for i := 0; i < n; i++ {
		var one int
		if err := conn.QueryRowContext(ctx, "select 1 from dual").Scan(&one); err != nil {
			return err
		}
	}
	per := time.Since(start) / n
	fmt.Printf("1 行の往復 ×%d: 平均 %s\n", n, ms(per))

	const q = `select object_name, object_type, owner, created, last_ddl_time,
                      status, object_id, namespace, temporary, generated
               from all_objects where rownum <= 1000`
	start = time.Now()
	rows, err := conn.QueryContext(ctx, q)
	if err != nil {
		return err
	}
	firstRow := time.Duration(0)
	count := 0
	for rows.Next() {
		if count == 0 {
			firstRow = time.Since(start)
		}
		var (
			name, typ, owner, status, ns, temp, gen sql.NullString
			created, lastDDL                        sql.NullTime
			id                                      sql.NullInt64
		)
		if err := rows.Scan(&name, &typ, &owner, &created, &lastDDL,
			&status, &id, &ns, &temp, &gen); err != nil {
			return err
		}
		count++
	}
	if err := rows.Err(); err != nil {
		return err
	}
	fmt.Printf("1000 行 × 10 列: %s（1 行目まで %s、%d 行）\n",
		ms(time.Since(start)), ms(firstRow), count)
	return nil
}

// ---------------------------------------------------------------- query

func cmdQuery(ctx context.Context, conn *sql.Conn, query string) error {
	if strings.TrimSpace(query) == "" {
		return errors.New("-sql が空")
	}
	rows, err := conn.QueryContext(ctx, query)
	if err != nil {
		return err
	}
	defer rows.Close()
	cols, _ := rows.Columns()
	cts, _ := rows.ColumnTypes()
	for i, c := range cols {
		fmt.Printf("%d: %-30s %s\n", i, c, cts[i].DatabaseTypeName())
	}
	fmt.Println()
	n := 0
	for rows.Next() {
		vals := make([]any, len(cols))
		ptrs := make([]any, len(cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return err
		}
		parts := make([]string, len(vals))
		for i, v := range vals {
			parts[i] = fmt.Sprintf("%s=%T %s", cols[i], v, render(v))
		}
		fmt.Println(strings.Join(parts, " | "))
		n++
		if n >= 50 {
			fmt.Println("…（50 行で打ち切り）")
			break
		}
	}
	fmt.Printf("\n%d 行\n", n)
	return rows.Err()
}

// cmdExec は -sql をそのまま ExecContext に投げる（DDL や後始末用）。
func cmdExec(ctx context.Context, conn *sql.Conn, text string) error {
	if strings.TrimSpace(text) == "" {
		return errors.New("-sql が空")
	}
	t := time.Now()
	res, err := conn.ExecContext(ctx, text)
	if err != nil {
		return err
	}
	n, _ := res.RowsAffected()
	fmt.Printf("OK (%s) rows=%d\n", ms(time.Since(t)), n)
	return nil
}

// ---------------------------------------------------------------- 共通

func ms(d time.Duration) string {
	return fmt.Sprintf("%.1f ms", float64(d.Microseconds())/1000)
}

func result(err error) string {
	if err == nil {
		return "OK"
	}
	return "! " + oneLine(err.Error())
}

// describe はエラーの Go 側の型と中身を出す。
// go-ora の *network.OracleError は ErrCode を公開しているが、
// errPos（構文エラーの位置）は非公開で Error() の文字列にしか出ない。
func describe(err error) string {
	if err == nil {
		return "nil（止まらなかった / エラー無し）"
	}
	s := fmt.Sprintf("%T: %s", err, oneLine(err.Error()))
	if errors.Is(err, context.Canceled) {
		s += " [context.Canceled]"
	}
	if errors.Is(err, context.DeadlineExceeded) {
		s += " [context.DeadlineExceeded]"
	}
	return s
}

func oneLine(s string) string {
	s = strings.ReplaceAll(s, "\r", "")
	s = strings.ReplaceAll(s, "\n", " / ")
	return strings.TrimSpace(s)
}

func fatal(err error) {
	fmt.Fprintf(os.Stderr, "probe: %s\n", err)
	os.Exit(1)
}
