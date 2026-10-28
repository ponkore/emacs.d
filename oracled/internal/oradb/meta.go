package oradb

// メタ情報（DESCRIBE / オブジェクト一覧 / 補完の候補）。
//
// 【重要】1 回の往復が 100 ms 前後かかる回線がある（実測。
// docs/phase0-findings.md F）。列・コメント・制約・索引・統計を別々に
// 引くと DESCRIBE 1 回で 0.7〜1 秒になるので、**1 文に畳む**。
//
// 畳み方は「UNION ALL で種別の印（kind）を付け、汎用の桁（c1..c6）に
// 詰める」。読みにくさと引き換えに往復が 1 回で済む。各 kind の桁の
// 意味は下の SQL のコメントに書いてある。

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
)

type ColumnInfo struct {
	Name     string `json:"name"`
	Type     string `json:"type"`
	Nullable bool   `json:"nullable"`
	Comment  string `json:"comment,omitempty"`
	Position int    `json:"position"`
	PK       int    `json:"pk,omitempty"` // 主キーの何番目か（0 なら主キーでない）
}

type ConstraintInfo struct {
	Name    string   `json:"name"`
	Type    string   `json:"type"` // P / U / R / C
	Columns []string `json:"columns"`
	Refers  string   `json:"refers,omitempty"`
}

type IndexInfo struct {
	Name    string   `json:"name"`
	Unique  bool     `json:"unique"`
	Columns []string `json:"columns"`
}

type DescribeResult struct {
	Owner        string           `json:"owner"`
	Name         string           `json:"name"`
	ObjectType   string           `json:"object_type"`
	Comment      string           `json:"comment,omitempty"`
	NumRows      string           `json:"num_rows,omitempty"`
	LastAnalyzed string           `json:"last_analyzed,omitempty"`
	Columns      []ColumnInfo     `json:"columns"`
	Constraints  []ConstraintInfo `json:"constraints"`
	Indexes      []IndexInfo      `json:"indexes"`
	ElapsedMS    float64          `json:"elapsed_ms"`
}

// describeSQL は 1 文で全部を引く。
//
//	kind=O … c1:owner  c2:object_name c3:object_type c4:comment
//	          c5:num_rows c6:last_analyzed
//	kind=C … c1:column_name c2:type c3:nullable c4:comment c5:column_id c6:pk
//	kind=K … c1:constraint_name c2:type c3:columns(,) c4:refers
//	kind=I … c1:index_name c2:uniqueness c3:columns(,)
//
// 【重要】`data_default` と `search_condition` は LONG なので引かない。
// LONG はドライバによって扱いが違ううえ、DESCRIBE の用途では要らない
// （既定値まで見たいときは SQLcl の `INFO+` を使う。設計 §1 の非目標）。
// describeSQL は列とコメントだけ（既定）。
const describeSQL = `
with obj as (
  select owner, object_name, object_type from (
    select owner, object_name, object_type
      from all_objects
     where object_name = upper(:name)
       and (:owner is null or owner = upper(:owner))
       and object_type in ('TABLE','VIEW','MATERIALIZED VIEW')
     order by case when owner = user then 0 else 1 end, owner
  ) where rownum = 1
), pk as (
  select kc.column_name, kc.position
    from obj o
    join all_constraints pc
      on pc.owner = o.owner and pc.table_name = o.object_name
     and pc.constraint_type = 'P'
    join all_cons_columns kc
      on kc.owner = pc.owner and kc.constraint_name = pc.constraint_name
)
select 'O' kind, o.owner c1, o.object_name c2, o.object_type c3,
       substr(tc.comments, 1, 400) c4, to_char(t.num_rows) c5,
       to_char(t.last_analyzed, 'YYYY-MM-DD"T"HH24:MI:SS') c6,
       0 ord, 0 ord2
  from obj o
  left join all_tab_comments tc
    on tc.owner = o.owner and tc.table_name = o.object_name
  left join all_tables t
    on t.owner = o.owner and t.table_name = o.object_name
union all
select 'C', c.column_name,
       c.data_type ||
         case
           when c.data_type in ('VARCHAR2','NVARCHAR2','CHAR','NCHAR','RAW')
             then '(' || c.char_length || ')'
           when c.data_type = 'NUMBER' and c.data_precision is not null
             then '(' || c.data_precision ||
                  case when nvl(c.data_scale, 0) > 0
                       then ',' || c.data_scale end || ')'
         end,
       c.nullable, substr(cc.comments, 1, 200),
       to_char(c.column_id), to_char(pk.position),
       1, c.column_id
  from obj o
  join all_tab_columns c on c.owner = o.owner and c.table_name = o.object_name
  left join all_col_comments cc
    on cc.owner = c.owner and cc.table_name = c.table_name
   and cc.column_name = c.column_name
  left join pk on pk.column_name = c.column_name
 order by 8, 9, 2`

// describeHeavySQL は制約と索引（`C-u` のときだけ）。
//
// 【重要】この 2 つは**この DB では辞書ビュー自体が遅い**（実測:
// all_constraints の count だけで 0.8 秒、all_indexes で 0.9 秒。
// user_* に替えても変わらない）。往復を減らしても縮まないので、
// SQLcl の DESC と INFO+ の切り分けに合わせて既定から外してある。
const describeHeavySQL = `
with obj as (
  select owner, object_name from (
    select owner, object_name
      from all_objects
     where object_name = upper(:name)
       and (:owner is null or owner = upper(:owner))
       and object_type in ('TABLE','VIEW','MATERIALIZED VIEW')
     order by case when owner = user then 0 else 1 end, owner
  ) where rownum = 1
)
select 'K', k.constraint_name, k.constraint_type,
       listagg(kc.column_name, ',') within group (order by kc.position),
       max(case when k.constraint_type = 'R'
                then r.owner || '.' || r.table_name end), null, null, 2, 0
  from obj o
  join all_constraints k
    on k.owner = o.owner and k.table_name = o.object_name
   and k.constraint_type in ('P','U','R')
  left join all_cons_columns kc
    on kc.owner = k.owner and kc.constraint_name = k.constraint_name
   and kc.table_name = o.object_name
  left join all_constraints r
    on r.owner = k.r_owner and r.constraint_name = k.r_constraint_name
 group by k.constraint_name, k.constraint_type
union all
select 'I', i.index_name, i.uniqueness,
       listagg(ic.column_name, ',') within group (order by ic.column_position),
       null, null, null, 3, 0
  from obj o
  join all_indexes i
    on i.table_owner = o.owner and i.table_name = o.object_name
  join all_ind_columns ic
    on ic.index_owner = i.owner and ic.index_name = i.index_name
   and ic.table_name = o.object_name
 group by i.index_name, i.uniqueness order by 8, 9, 2`

// Describe はオブジェクトの定義を引く。NAME は "SCHEMA.TABLE" でも
// "TABLE" でもよい。FULL が真なら制約と索引も引く（往復が 1 回増え、
// この DB では 2〜3 秒かかる）。
func (s *Session) Describe(ctx context.Context, name string, full bool) (*DescribeResult, error) {
	owner := ""
	if i := strings.Index(name, "."); i > 0 {
		owner, name = name[:i], name[i+1:]
	}
	name = strings.Trim(strings.TrimSpace(name), `"`)
	if name == "" {
		return nil, fmt.Errorf("オブジェクト名が空")
	}
	var ownerArg any
	if owner != "" {
		ownerArg = strings.Trim(owner, `"`)
	}
	out := &DescribeResult{Columns: []ColumnInfo{},
		Constraints: []ConstraintInfo{}, Indexes: []IndexInfo{}}
	if err := s.scanDescribe(ctx, describeSQL, name, ownerArg, out); err != nil {
		return nil, err
	}
	if out.Name == "" {
		return nil, fmt.Errorf("%s が見つからない（表・ビュー・マテビューのみ）", name)
	}
	if full {
		if err := s.scanDescribe(ctx, describeHeavySQL, name, ownerArg, out); err != nil {
			return nil, err
		}
	}
	return out, nil
}

// scanDescribe は汎用の桁（kind, c1..c6）を読んで OUT に詰める。
func (s *Session) scanDescribe(ctx context.Context, query, name string,
	ownerArg any, out *DescribeResult) error {
	rows, err := s.conn.QueryContext(ctx, query,
		sql.Named("name", name), sql.Named("owner", ownerArg))
	if err != nil {
		return err
	}
	defer rows.Close()
	for rows.Next() {
		var kind string
		var c1, c2, c3, c4, c5, c6, ord, ord2 sql.NullString
		if err := rows.Scan(&kind, &c1, &c2, &c3, &c4, &c5, &c6, &ord, &ord2); err != nil {
			return err
		}
		switch kind {
		case "O":
			out.Owner, out.Name, out.ObjectType = c1.String, c2.String, c3.String
			out.Comment, out.NumRows, out.LastAnalyzed = c4.String, c5.String, c6.String
		case "C":
			out.Columns = append(out.Columns, ColumnInfo{
				Name: c1.String, Type: c2.String,
				Nullable: c3.String != "N", Comment: c4.String,
				Position: atoiSafe(c5.String), PK: atoiSafe(c6.String),
			})
		case "K":
			out.Constraints = append(out.Constraints, ConstraintInfo{
				Name: c1.String, Type: c2.String,
				Columns: splitList(c3.String), Refers: c4.String,
			})
		case "I":
			out.Indexes = append(out.Indexes, IndexInfo{
				Name: c1.String, Unique: c2.String == "UNIQUE",
				Columns: splitList(c3.String),
			})
		}
	}
	return rows.Err()
}

type ObjectInfo struct {
	Owner string `json:"owner"`
	Name  string `json:"name"`
	Type  string `json:"type"`
}

type ObjectsResult struct {
	Objects   []ObjectInfo `json:"objects"`
	Token     string       `json:"token"`
	ElapsedMS float64      `json:"elapsed_ms"`
}

// 【重要】キャッシュの無効化は「通知」ではなく**トークン**で表す
// （gitd と同じ。通知を 1 つ落とすと永久に古いままになる壊れ方をしない）。
// last_ddl_time は秒精度なので同じ秒の DDL は取りこぼす。手で取り直す道
// （`C-u`）を必ず残すこと。
const objectsSQL = `
select owner, object_name, object_type,
       (select to_char(max(last_ddl_time), 'YYYYMMDDHH24MISS') || '/' || count(*)
          from all_objects x
         where (:tok_scope = 1 or x.owner = user)) token
  from all_objects
 where (:scope = 1 or owner = user)
   and object_type not like '% BODY'
   and object_type in ('TABLE','VIEW','MATERIALIZED VIEW','SYNONYM',
                       'PROCEDURE','FUNCTION','PACKAGE','SEQUENCE','TYPE','TRIGGER')
 order by object_name, owner`

// Objects はオブジェクトの一覧とキャッシュ用のトークンを返す。
//
// 【重要】bind 変数の名前に `all` は使えない（ORA-01745: invalid
// host/bind variable name）。予約語と同じ綴りは弾かれる。
func (s *Session) Objects(ctx context.Context, all bool) (*ObjectsResult, error) {
	flag := 0
	if all {
		flag = 1
	}
	rows, err := s.conn.QueryContext(ctx, objectsSQL,
		sql.Named("tok_scope", flag), sql.Named("scope", flag))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	out := &ObjectsResult{Objects: []ObjectInfo{}}
	for rows.Next() {
		var owner, name, typ, token sql.NullString
		if err := rows.Scan(&owner, &name, &typ, &token); err != nil {
			return nil, err
		}
		out.Token = token.String
		out.Objects = append(out.Objects, ObjectInfo{
			Owner: owner.String, Name: name.String, Type: typ.String})
	}
	return out, rows.Err()
}

// Token はキャッシュが使えるかの判定に使う値だけを返す（1 行）。
func (s *Session) Token(ctx context.Context, all bool) (string, error) {
	flag := 0
	if all {
		flag = 1
	}
	var token sql.NullString
	err := s.conn.QueryRowContext(ctx,
		`select to_char(max(last_ddl_time), 'YYYYMMDDHH24MISS') || '/' || count(*)
           from all_objects where (:scope = 1 or owner = user)`,
		sql.Named("scope", flag)).Scan(&token)
	return token.String, err
}

func splitList(s string) []string {
	if s == "" {
		return nil
	}
	return strings.Split(s, ",")
}

func atoiSafe(s string) int {
	n := 0
	for _, c := range s {
		if c < '0' || c > '9' {
			return n
		}
		n = n*10 + int(c-'0')
	}
	return n
}
