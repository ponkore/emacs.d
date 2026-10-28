package sqlsplit

import "testing"

// want は 1 文に期待するもの。空文字・ゼロは「見ない」。
type want struct {
	text     string
	kind     Kind
	command  string
	begin    int
	end      int
	line     int
	complete bool
}

func TestSplit(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want []want
	}{
		{
			name: "単純な SELECT。末尾の ; は Text から落とす",
			src:  "select 1 from dual;",
			want: []want{{text: "select 1 from dual", kind: KindSQL,
				begin: 0, end: 19, line: 1, complete: true}},
		},
		{
			name: "終端子が無ければ未完",
			src:  "select 1 from dual",
			want: []want{{text: "select 1 from dual", kind: KindSQL, complete: false}},
		},
		{
			name: "2 文",
			src:  "select 1 from dual;\nselect 2 from dual;",
			want: []want{
				{text: "select 1 from dual", kind: KindSQL, line: 1, complete: true},
				{text: "select 2 from dual", kind: KindSQL, line: 2, complete: true},
			},
		},
		{
			name: "文字列の中の ; では切らない",
			src:  "select 'a;b' from dual;",
			want: []want{{text: "select 'a;b' from dual", complete: true}},
		},
		{
			name: "'' エスケープ",
			src:  "select 'it''s; ok' from dual;",
			want: []want{{text: "select 'it''s; ok' from dual", complete: true}},
		},
		{
			name: "代替引用 q'[...]' の中の ; と '",
			src:  "select q'[a;b'c]' from dual;",
			want: []want{{text: "select q'[a;b'c]' from dual", complete: true}},
		},
		{
			name: "代替引用の区切りが任意の文字",
			src:  "select q'#a;b#' from dual;",
			want: []want{{text: "select q'#a;b#' from dual", complete: true}},
		},
		{
			name: "nq'<...>'",
			src:  "select nq'<a;b>' from dual;",
			want: []want{{text: "select nq'<a;b>' from dual", complete: true}},
		},
		{
			name: "引用識別子の中の ;",
			src:  `select "a;b" from t;`,
			want: []want{{text: `select "a;b" from t`, complete: true}},
		},
		{
			name: "行コメントの中の ;",
			src:  "select 1 -- a;b\nfrom dual;",
			want: []want{{text: "select 1 -- a;b\nfrom dual", complete: true}},
		},
		{
			name: "ブロックコメントの中の ;",
			src:  "select /* a;b */ 1 from dual;",
			want: []want{{text: "select /* a;b */ 1 from dual", complete: true}},
		},
		{
			name: "ヒントは普通に残る",
			src:  "select /*+ full(t) */ * from t;",
			want: []want{{text: "select /*+ full(t) */ * from t", complete: true}},
		},
		{
			name: "文の手前のコメントはどの文にも属さない",
			src:  "-- これは説明\nselect 1 from dual;",
			want: []want{{text: "select 1 from dual", line: 2, complete: true}},
		},
		{
			name: "無名ブロック。中の ; では切らず、単独行の / で終わる",
			src:  "begin\n  null;\n  null;\nend;\n/\n",
			want: []want{{text: "begin\n  null;\n  null;\nend;", kind: KindPLSQL, complete: true}},
		},
		{
			name: "DECLARE で始まるブロック",
			src:  "declare x number; begin x := 1; end;\n/\n",
			want: []want{{text: "declare x number; begin x := 1; end;", kind: KindPLSQL, complete: true}},
		},
		{
			name: "/ が無い PL/SQL は未完（SQLcl と同じく黙って待つ状態）",
			src:  "begin null; end;",
			want: []want{{text: "begin null; end;", kind: KindPLSQL, complete: false}},
		},
		{
			name: "CREATE OR REPLACE PROCEDURE は PL/SQL",
			src:  "create or replace procedure p is begin null; end;\n/\n",
			want: []want{{text: "create or replace procedure p is begin null; end;",
				kind: KindPLSQL, complete: true}},
		},
		{
			name: "CREATE PACKAGE BODY も PL/SQL",
			src:  "create package body pkg as procedure p is begin null; end; end;\n/\n",
			want: []want{{kind: KindPLSQL, complete: true}},
		},
		{
			name: "CREATE TYPE は PL/SQL（/ で終わる）",
			src:  "create type t as object (id number);\n/\n",
			want: []want{{kind: KindPLSQL, complete: true}},
		},
		{
			name: "CREATE TABLE は SQL（; で終わる）",
			src:  "create table t (id number);",
			want: []want{{text: "create table t (id number)", kind: KindSQL, complete: true}},
		},
		{
			name: "CREATE OR REPLACE VIEW は SQL",
			src:  "create or replace view v as select 1 x from dual;",
			want: []want{{kind: KindSQL, complete: true}},
		},
		{
			name: "SQL でも単独行の / で終わる（SQL*Plus 流儀のスクリプト）",
			src:  "select 1 from dual\n/\n",
			want: []want{{text: "select 1 from dual", kind: KindSQL, complete: true}},
		},
		{
			name: "単独の / は何もしない（直前の文を再実行しない）",
			src:  "/\n",
			want: nil,
		},
		{
			name: "裸の ; も何もしない",
			src:  ";\n",
			want: nil,
		},
		{
			name: "@ は行末で終わる client",
			src:  "@scripts/foo.sql\nselect 1 from dual;",
			want: []want{
				{text: "@scripts/foo.sql", kind: KindClient, command: "@", complete: true},
				{text: "select 1 from dual", kind: KindSQL, line: 2, complete: true},
			},
		},
		{
			name: "@@ も client",
			src:  "@@sub.sql",
			want: []want{{text: "@@sub.sql", kind: KindClient, command: "@@", complete: true}},
		},
		{
			name: "SET は client。末尾の ; は落とす",
			src:  "SET SERVEROUTPUT ON;\n",
			want: []want{{text: "SET SERVEROUTPUT ON", kind: KindClient, command: "SET", complete: true}},
		},
		{
			name: "【重要】SET TRANSACTION は本物の SQL",
			src:  "set transaction read only;",
			want: []want{{text: "set transaction read only", kind: KindSQL, complete: true}},
		},
		{
			name: "SET ROLE も SQL",
			src:  "set role all;",
			want: []want{{kind: KindSQL, complete: true}},
		},
		{
			name: "SET CONSTRAINTS も SQL",
			src:  "set constraints all immediate;",
			want: []want{{kind: KindSQL, complete: true}},
		},
		{
			name: "DESC は client",
			src:  "desc mt_brand",
			want: []want{{text: "desc mt_brand", kind: KindClient, command: "DESC", complete: true}},
		},
		{
			name: "EXEC は client（呼び出し側が BEGIN...END; に包む）",
			src:  "exec p(1);",
			want: []want{{text: "exec p(1)", kind: KindClient, command: "EXEC", complete: true}},
		},
		{
			name: "COMMIT / ROLLBACK は SQL",
			src:  "commit;\nrollback;",
			want: []want{
				{text: "commit", kind: KindSQL, complete: true},
				{text: "rollback", kind: KindSQL, complete: true},
			},
		},
		{
			name: "閉じていない文字列は未完",
			src:  "select 'abc from dual;",
			want: []want{{kind: KindSQL, complete: false}},
		},
		{
			name: "CRLF でも切れる",
			src:  "select 1 from dual;\r\nselect 2 from dual;\r\n",
			want: []want{
				{text: "select 1 from dual", complete: true},
				{text: "select 2 from dual", complete: true},
			},
		},
		{
			name: "空白だけなら 0 文",
			src:  "\n\n   \t\n",
			want: nil,
		},
		{
			name: "コメントだけなら 0 文",
			src:  "-- なにもしない\n/* これも */\n",
			want: nil,
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got := Split(c.src)
			if len(got) != len(c.want) {
				t.Fatalf("文の数が %d（期待 %d）\n%#v", len(got), len(c.want), got)
			}
			for i, w := range c.want {
				g := got[i]
				if w.text != "" && g.Text != w.text {
					t.Errorf("[%d] Text = %q、期待 %q", i, g.Text, w.text)
				}
				if w.kind != "" && g.Kind != w.kind {
					t.Errorf("[%d] Kind = %q、期待 %q", i, g.Kind, w.kind)
				}
				if w.command != "" && g.Command != w.command {
					t.Errorf("[%d] Command = %q、期待 %q", i, g.Command, w.command)
				}
				if g.Complete != w.complete {
					t.Errorf("[%d] Complete = %v、期待 %v（Text=%q）", i, g.Complete, w.complete, g.Text)
				}
				if w.begin != 0 || w.end != 0 {
					if g.Begin != w.begin || g.End != w.end {
						t.Errorf("[%d] 位置 = [%d,%d)、期待 [%d,%d)", i, g.Begin, g.End, w.begin, w.end)
					}
				}
				if w.line != 0 && g.Line != w.line {
					t.Errorf("[%d] Line = %d、期待 %d", i, g.Line, w.line)
				}
			}
		})
	}
}

// 【重要】位置は文字（rune）単位。バイトで数えると日本語でずれ、
// Emacs 側がエラー位置や文の範囲を取り違える。
func TestSplitPositionsAreRunes(t *testing.T) {
	src := "select 'あいう' from dual;\nselect 2 from dual;"
	got := Split(src)
	if len(got) != 2 {
		t.Fatalf("文の数が %d", len(got))
	}
	// 1 文目: 0 から「;」の次まで。rune で数えると 23 文字
	// （"select " 7 + "'あいう'" 5 + " from dual" 10 + ";" 1 = 23）
	// バイトで数えると 29 になるので、ここがずれていたら rune で
	// 数えていない証拠になる。
	if got[0].Begin != 0 || got[0].End != 23 {
		t.Errorf("1 文目の位置 = [%d,%d)、期待 [0,23)", got[0].Begin, got[0].End)
	}
	if got[1].Begin != 24 { // 23 は改行
		t.Errorf("2 文目の Begin = %d、期待 24", got[1].Begin)
	}
	// Raw が位置から切り出せること（Emacs 側で range を使うため）
	r := []rune(src)
	if string(r[got[0].Begin:got[0].End]) != got[0].Raw {
		t.Errorf("Raw が位置と合わない: %q", got[0].Raw)
	}
	if string(r[got[1].Begin:got[1].End]) != got[1].Raw {
		t.Errorf("Raw が位置と合わない: %q", got[1].Raw)
	}
}

// スクリプト 1 本をまとめて通す。@file の中身に近い形。
func TestSplitScript(t *testing.T) {
	src := `-- 初期データ
PROMPT マスタを入れます
insert into mt_brand (id, name) values (1, 'あ;い');

begin
  for i in 1 .. 3 loop
    insert into mt_brand (id, name) values (i + 1, 'x' || i);
  end loop;
end;
/

@@sub.sql
commit;
`
	got := Split(src)
	wantKinds := []Kind{KindClient, KindSQL, KindPLSQL, KindClient, KindSQL}
	if len(got) != len(wantKinds) {
		t.Fatalf("文の数が %d（期待 %d）", len(got), len(wantKinds))
	}
	for i, k := range wantKinds {
		if got[i].Kind != k {
			t.Errorf("[%d] Kind = %q、期待 %q（Text=%q）", i, got[i].Kind, k, got[i].Text)
		}
		if !got[i].Complete {
			t.Errorf("[%d] 未完になっている（Text=%q）", i, got[i].Text)
		}
	}
	if got[0].Command != "PROMPT" {
		t.Errorf("PROMPT を拾えていない: %#v", got[0])
	}
	if got[3].Command != "@@" {
		t.Errorf("@@ を拾えていない: %#v", got[3])
	}
}

func TestBinds(t *testing.T) {
	cases := []struct {
		src  string
		want []string
	}{
		{"select * from t where id = :id", []string{"id"}},
		{"select * from t where a = :a and b = :b and a2 = :a", []string{"a", "b"}},
		{"select ':notabind' from dual", nil},
		{"select 1 from dual -- :nope", nil},
		{"select 1 from dual /* :nope */", nil},
		{"select q'[:nope]' from dual", nil},
		{`select "a:b" from t where x = :x`, []string{"x"}},
		{"select * from t where id = :1", nil}, // 位置指定は名前ではない
		{"begin x := 1; end;", nil},            // 代入
		{"select cast(x as number) from t where y = :y", []string{"y"}},
		{"update t set a = :val where id = :id", []string{"val", "id"}},
	}
	for _, c := range cases {
		got := Binds(c.src)
		if len(got) != len(c.want) {
			t.Errorf("%q: %v（期待 %v）", c.src, got, c.want)
			continue
		}
		for i := range got {
			if got[i] != c.want[i] {
				t.Errorf("%q: %v（期待 %v）", c.src, got, c.want)
				break
			}
		}
	}
}
