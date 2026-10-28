package main

// 割り込みの計測。Phase 0 でいちばん重い項目なので別ファイルにしてある。
// → docs/phase0-findings.md E

import (
	"context"
	"database/sql"
	"fmt"
	"time"
)

// load は 1 種類の負荷。break の効き方が違いうるので 3 つ用意する。
type load struct {
	name string
	kind string // "query" | "exec"
	text string
}

func cmdCancel(ctx context.Context, conn *sql.Conn, db *sql.DB) error {
	n := *heavyN
	// 【重要】負荷の作り方を 2 回間違えた。記録として残す。
	//   - connect by で行を作る → ORA-30009（CONNECT BY のメモリ不足）で即死
	//   - PL/SQL の空ループ     → オプティマイザに消され、30M 回でも 107 ms
	// どちらも「割り込みが効いた」ように見えてしまう。
	loads := []load{
		{"join", "query", fmt.Sprintf(
			"select count(*) from (select 1 x from all_objects where rownum <= %d) a,"+
				" (select 1 y from all_objects where rownum <= %d) b", n, n)},
		{"plsql", "exec", fmt.Sprintf(
			"declare x number := 0; begin for i in 1..%d loop x := x + sqrt(i); end loop; end;",
			n*5000)},
		// サーバが待っているだけの状態（ソケットも読んでいない）。
		{"sleep", "exec", "begin dbms_session.sleep(30); end;"},
	}

	run := func(c context.Context, l load) error {
		if l.kind == "query" {
			var v int64
			return conn.QueryRowContext(c, l.text).Scan(&v)
		}
		_, err := conn.ExecContext(c, l.text)
		return err
	}

	for _, l := range loads {
		fmt.Printf("=== 負荷: %s\n", l.name)

		// 【重要】基準を先に測らないと [1] の結果が読めない。
		// 「割り込みが効いた」と「文が自然に終わった」は、時間を比べないと
		// 区別できない（効いていないのに効いたように見える）。
		bctx, bcancel := context.WithTimeout(context.Background(), 3*time.Minute)
		t := time.Now()
		err := run(bctx, l)
		base := time.Since(t)
		bcancel()
		fmt.Printf("  [0] 基準（割り込まない）: %s %s\n", ms(base), result(err))
		if err != nil {
			fmt.Printf("  → この負荷は測定に使えない。飛ばす\n\n")
			continue
		}
		if base < 3**after {
			fmt.Printf("  ! 基準が短すぎる（割り込みは %s 後）。-heavy を増やす\n\n", *after)
			continue
		}

		// [1] WithCancel — 走っている途中で明示的に割り込む
		cctx, ccancel := context.WithCancel(context.Background())
		done := make(chan error, 1)
		t = time.Now()
		go func() { done <- run(cctx, l) }()
		time.Sleep(*after)
		cancelAt := time.Now()
		ccancel()
		err = <-done
		el := time.Since(cancelAt)
		fmt.Printf("  [1] WithCancel: 要求から戻るまで **%s**（投げてから %s で要求）\n",
			ms(el), ms(cancelAt.Sub(t)))
		fmt.Printf("      %s\n", describe(err))
		fmt.Printf("      判定: %s\n", verdict(el, base, *after))

		// [2] 割り込んだ後、同じセッションがまだ使えるか。
		// break 後は ErrConnReset になり、database/sql が接続を捨てることが
		// ある。捨てられると tx も消える。
		var sid sql.NullString
		t = time.Now()
		serr := conn.QueryRowContext(context.Background(),
			"select sys_context('userenv','sid') from dual").Scan(&sid)
		fmt.Printf("  [2] 割り込み後: sid=%s %s (%s)\n",
			sid.String, result(serr), ms(time.Since(t)))

		// [3] WithTimeout（issue #702 と同じ形）
		tctx, tcancel := context.WithTimeout(context.Background(), *after)
		t = time.Now()
		err = run(tctx, l)
		el = time.Since(t)
		tcancel()
		fmt.Printf("  [3] WithTimeout: %s で戻った\n      %s\n", ms(el), describe(err))
		fmt.Printf("      判定: %s\n\n", verdict(el-*after, base, *after))
	}

	fmt.Println("=== 最終確認")
	var one int
	err := conn.QueryRowContext(context.Background(), "select 1 from dual").Scan(&one)
	fmt.Printf("  select 1 from dual -> %s\n", result(err))
	fmt.Printf("  db.Stats: %+v\n", db.Stats())
	return nil
}
