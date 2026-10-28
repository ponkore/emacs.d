<!-- -*- gfm -*- -->

# Phase 0 — 成立するかの検証

| | |
|---|---|
| 対象 | go-ora **v2.9.0**（`go.mod` で固定） |
| 実測日 | 2026-10-28 |
| 環境 | Windows 11 / Go 1.27.3 / **Oracle 21c XE 21.3.0.0.0**（`192.168.66.101:1521/XEPDB1`、AL32UTF8） |
| 回線 | **RTT 約 54 ms**（同じ経路の `192.168.66.6` への ping。`.101` は ICMP が塞がれている） |
| 道具 | `cmd/probe`。生ログは `docs/_raw/`（git 管理外） |
| **結論** | **Phase 1 に進んでよい。**割り込み・日本語・型・書き込みのすべてが成立した |

12c（`192.168.66.6`、JA16SJISTILDE）は**インスタンスが登録されていない**ため測れていない
（下記 M）。目標は 21c なので保留。

---

# 1. 判断

| 進む条件（設計 §17） | 結果 |
|---|---|
| 日本語が往復する | **○**（バイト列が完全一致。`𠮟` も絵文字も `〜—−` も） |
| 走っている文を、tx を失わずに止められる | **○**（CPU を回す文で **165〜181 ms**、セッション生存） |

**そして段 0（OOB の fork）は要らない。** 静的調査の予測（E）は外れで、
**Windows でも in-band break が即座に効く**。→ 下記 E'

---

# 2. 実測

## 速さ

| | SQLcl 版（既存の実測） | **`oracled`（実測）** |
|---|---|---|
| 起動〜使える状態 | **7〜8 秒** | **567〜837 ms**（Go の起動 + TNS 接続 + 認証） |
| 1 行の往復 ×100 の平均 | — | **125.8 ms** |
| 1000 行 × 10 列 | — | **373.9 ms**（1 行目まで 89.5 ms） |
| `DESCRIBE` 相当 | — | 未実測（Phase 2.5） |
| 常駐メモリ | JVM（数百 MB） | 未実測（probe は 23 MB の exe） |

### 【重要】F. 支配しているのはネットワークの往復。CPU ではない

**125.8 ms は RTT 54 ms のほぼ 2 往復ぶん。** ドライバも Go も遅くない。
接続に 600 ms かかるのも認証のラウンドトリップが 10 回程度あるため。

設計に効く帰結が 3 つある。

| | |
|---|---|
| `meta/describe` の 6〜7 本のクエリ | **直列だと 0.7〜1 秒。1〜2 文にまとめる**（§10 を要修正） |
| `DBMS_OUTPUT` の `GET_LINE` | **1 行 = 1 往復 = 110 ms。`GET_LINES` で一括**（§13 を要修正） |
| `sql/split` を daemon に置いた判断 | **正しかった。**DB を 1 回も叩かないので 1 ms 未満で閉じる |

逆に「SQLcl より速い」と言えるのは**起動（約 10 倍）と往復の回数**だけで、
**1 文の体感は回線で決まる**。SQLcl も同じ回線を使うので不利ではない。

## 割り込み

`probe -heavy 20000 cancel`。**基準（割り込まない所要）を先に測ってから
1.5 秒後に割り込む。**これをやらないと「自然に終わった」のを
「止まった」と読み違える（実際に 1 回読み違えた）。

| 負荷 | 基準 | **割り込み要求から戻るまで** | エラー | セッション |
|---|---|---|---|---|
| CPU（表の直積 2 万×2 万） | 4821.6 ms | **165.8 ms** | ORA-01013 | **生存**（同じ sid） |
| CPU（PL/SQL の 1 億回ループ） | 15319.1 ms | **181.5 ms** | ORA-01013 | **生存** |
| 待ち（`DBMS_SESSION.SLEEP(30)`） | 30125.4 ms | **1671.4 ms** | ORA-01013 | **生存** |

`context.WithTimeout` でも同じ（`join` で 1675.4 ms = 1500 + 175）。
**issue #702 の「効かない」は再現しなかった。**

### E'. 【重要】静的調査 E の予測は外れた

E ではこう書いた — 「Windows の `sendOOB` は空実装なので in-band marker にしか
行かず、サーバは応答を返すまでソケットを読まないので break は効かないはず」。

**前半は正しいが（ソースのとおり）、後半が誤り。** 実測では
**CPU を回している最中の文が 165 ms で止まり、ORA-01013 が返り、セッションも
残った**。現代の Oracle は in-band の break を実行中にちゃんと拾う。

| 段 | 当初の予測 | **実測** |
|---|---|---|
| 0（OOB の fork） | 必要になるはず | **要らない** |
| 1（`context` cancel） | Windows では疑わしい | **これで足りる。165〜181 ms** |
| 2（`ALTER SYSTEM CANCEL SQL`） | 権限次第 | **権限あり**（下記 L）。保険として残す |
| 3（接続破棄） | 最後の手段 | 残す。ただし出番は無さそう |

**`sleep` だけ 1.67 秒かかる**のは、`DBMS_SESSION.SLEEP` が一定間隔でしか
break を見ないため。実務では「重いクエリを止める」が目的なので問題にならない。

→ **段 0 は実装しない**という提案（判断は設計 §9 に反映済み）。
fork の動機は `errPos` の公開（C）だけ残るが、文字列から取れている（下記 C'）。

## 日本語（`probe jp`）

DB charset は **AL32UTF8**、NCHAR は AL16UTF16。

| 文字 | bind（`:1`） | リテラル | 往復 |
|---|---|---|---|
| `あ` | `e3,81,82` | `e3,81,82` | OK |
| `日本語` | `e6,97,a5,e6,9c,ac,e8,aa,9e` | 同じ | OK |
| **`𠮟`**（cp932 の外・サロゲート） | `f0,a0,ae,9f` | 同じ | OK |
| **`😀`** | `f0,9f,98,80` | 同じ | OK |
| **`〜—−`**（機種依存の 3 字） | `e3,80,9c,e2,80,94,e2,88,92` | 同じ | OK |
| `あ` を NVARCHAR2 に cast | `CharacterSet=AL16UTF16: 30,42` | — | OK |

**バイト列は bind とリテラルで完全一致。** `DUMP` の `Typ=` が
1（VARCHAR2）と 96（CHAR）で違うのは型の違いで、中身ではない
（最初これを「不一致」と表示していたのは probe 側の比較ミス。修正済み）。

**SQLcl 版で払っていた `JDK_JAVA_OPTIONS` / stdin だけ cp932 / `NOTE:` 行は
全部要らなくなる。** Go も Oracle も UTF-8 で閉じている。

## 型（`probe types`）

| Oracle | `DatabaseTypeName` | **Go（`any` で受けた実体）** | 値の例 |
|---|---|---|---|
| `1`（リテラル） | `NUMBER(38,255)` | **string** | `"1"` |
| `1/3` | `NUMBER(38,255)` | **string** | `"0.3333…"`（**42 桁**） |
| `NUMBER(10,2)` | `NUMBER(10,2)` | **string** | `"1.25"` |
| `123456789012345678901234567890` | `NUMBER(38,255)` | **string** | 30 桁そのまま |
| `BINARY_DOUBLE` / `BINARY_FLOAT` | `IBDouble` / `IBFloat` | float64 / float32 | `1.5` |
| `SYSDATE` | `DATE` | time.Time | **`2026-10-28T16:57:49Z`** |
| `SYSTIMESTAMP` | `TimeStampTZ_DTY` | time.Time | `…+09:00` |
| `LOCALTIMESTAMP` | `TimeStampDTY` | time.Time | `…Z` |
| `INTERVAL '1' DAY` | `IntervalDS_DTY` | string | `"+01 00:00:00.000000"` |
| `RAW` | `RAW` | []byte | `deadbeef` |
| **`CLOB`** | **`LongVarChar`** | string | `"あいうえお"` |
| **`BLOB`** | **`LongRaw`** | []byte | `deadbeef` |
| `ROWID` | `ROWID` | string | `AAAACPAAB…` |
| `CAST(NULL AS VARCHAR2)` | `NCHAR` | **nil** | — |
| `CHAR(10)` | `CHAR` | string | `"abc       "`（右詰めの空白はそのまま） |

**B は確認できた**（NUMBER は常に string、精度は落ちない）。
`ColumnTypeScanType` が `NUMBER(38,255)` にも `float64` を返すのも確認。
**ヒントを信じてはいけない。**

### 【重要】G. `DATE` は「壁時計の時刻に UTC の札が付いて」返る

`SYSDATE` が `2026-10-28T16:57:49Z` で返った。実際の時刻は JST の 16:57 で、
**値は壁時計として正しいが、タイムゾーンが `Z`**（`conn.dbServerTimeZone` が
UTC のため）。**そのまま地方時へ変換すると 9 時間ずれる。**

| | daemon が返すもの |
|---|---|
| `DATE` / `TIMESTAMP` | **タイムゾーンを付けない** ISO-8601（`2026-10-28T16:57:49`） |
| `TIMESTAMP WITH TZ` | オフセット付き（`…+09:00`。これは正しく来る） |

設計 §5 の「ISO-8601 固定」を**「DATE / TIMESTAMP はオフセットを付けない」に
具体化する**。

### H. CLOB / BLOB は既定で全部インラインで来る

`DatabaseTypeName` が `LongVarChar` / `LongRaw` になり、**LOB ロケータではなく
中身が丸ごと**返る（go-ora の既定 `LOB FETCH=INLINE`）。

- **巨大な CLOB を含む表を素で `SELECT *` すると全部転送される。**
  設計 §5 の「既定は先頭 4 KB」は、**こちらで `DBMS_LOB.SUBSTR` に書き換える**か
  `LOB FETCH=POST`（stream）に切り替えるかの実装が必要
- `CLOB` と `LONG` が同じ型名で来るので、**列の実型は `meta/describe` 側で持つ**

### K. 1 列だけの `select '' from dual` は行が返らない

`rows.Next()` が false になる（ドライバの癖）。**複数列なら正常**で、
`select 1, '', 2 from dual` は `B=<nil>` で 3 列 1 行が返る。
Oracle では `''` は NULL なので実害はほぼ無いが、**「0 件」と「1 件の NULL」を
取り違える経路があることは覚えておく**。

## エラー（`probe plsql`）

| 文 | 結果 |
|---|---|
| `select 1 from dual;`（末尾 `;`） | `ORA-00933` + **position 18**（`;` の位置、0 始まり） |
| `select foo from dual` | `ORA-00904: "FOO": invalid identifier` + **position 7** |
| `select * from <無い表>` | `ORA-00942` + **position 14** |
| `raise_application_error(-20001, …)` | `ORA-20001: テスト例外` / `ORA-06512: at line 1`（**スタックも入る**） |

### C'. 位置は取れる（文字列から）

`errPos` は非公開のままだが、`Error()` の末尾に
`error occur at position: N` が付き、**0 始まりの文字オフセット**だった。
設計 §8 の「ソースバッファの該当位置へ飛ぶ」は**文字列から剥がせば実現できる**。
fork は要らない。

**末尾の `;` が `ORA-00933` になる**ことも確認できた（§7 の lexer が必須）。

## `DBMS_OUTPUT`（`probe plsql`）

| | 結果 |
|---|---|
| `sql.Out`（`database/sql` 標準）で `GET_LINE` | **取れた**（`status=0` で 2 行、`status=1` で終端） |
| `go_ora.Out`（ドライバ固有）でも | 取れた |
| 日本語 | `"あいうえお"` がそのまま |

**ただし 1 行につき 1 往復（110 ms）。** 実装では
`DBMS_OUTPUT.GET_LINES` を PL/SQL ブロックで包んで**1 往復に畳む**
（コレクションのバインドを避けるため、ブロックの中で連結して
1 つの OUT 変数で受ける）。

```sql
declare
  l dbms_output.chararr; n integer := 1000; buf clob;
begin
  dbms_output.get_lines(l, n);
  for i in 1 .. n loop buf := buf || l(i) || chr(10); end loop;
  :out := buf;
end;
```

## 書き込みとトランザクション（`probe writes`）

検証用の表 `ORACLED_PROBE` を作って測り、最後に `drop … purge` した
（表は残っていないことを確認済み）。

| 手順 | 別接続から見える件数 |
|---|---|
| [1] **Tx の外**で INSERT | **1 件**（= 即コミット） |
| [1] そのあと `rollback` | 1 件（**戻らない**） |
| [2] `BeginTx` の中で INSERT | 1 件（見えない = 正しい） |
| [2] `Rollback` | 1 件（消えた = 正しい） |
| [3] `BeginTx` → `Commit` | 2 件 |
| [4] **未コミットのまま `Close`** | **2 件（増えなかった）** |
| [5] Tx の中で DDL | **3 件**（保留中の INSERT が**コミットされた**） |
| [5] そのあと `Rollback` | 3 件（戻らない） |

### A（確認）autocommit OFF は `*sql.Tx` でしか作れない

[1] のとおり **Tx の外の DML は即コミット**。静的調査どおり。
設計の「autocommit 既定 OFF」は**常に `BeginTx` の中**で実行する形になる。

### 【重要】J. logoff コミットは**起きなかった**

未コミットの tx を抱えたまま `DB.Close()` しても件数は増えなかった。
`Connection.Close` は `Logoff()` → `WriteFinalPacket()` → `Disconnect()` と
正規の手順を踏むのに、**サーバ側はロールバックした**。

**それでも設計 §6（閉じる全経路で明示 ROLLBACK）は変えない。**

- issue #767 は**コミットされた**と報告している（キャンセル直後の Rollback が
  race で中断された経路）。**条件によって変わるものに依存しない**
- autocommit OFF が `*sql.Tx` 実装になるので、**`tx.Rollback()` を呼ぶのは
  ただの 1 行**。保険の値段が安い

### 【重要】I. Tx 中の DDL は保留中の DML を巻き込んでコミットする

[5] のとおり。Oracle の仕様（DDL は暗黙コミット）だが、**autocommit OFF の
つもりで操作しているユーザーには見えない**。

**`stmt/result` に「この文が暗黙コミットした」印を足して、Emacs 側で必ず
表示する。** 黙っていると「ロールバックできるつもりだったのに確定していた」に
なる。

## 権限と環境

### L. `ALTER SYSTEM` はロール経由で持っている

| 問い合わせ | 結果 |
|---|---|
| `user_sys_privs` に `ALTER SYSTEM` | **0 件** |
| **`session_privs`** に `ALTER SYSTEM` | **1 件** |

**直接付与だけを見ると見落とす。** 段 2（`ALTER SYSTEM CANCEL SQL`）の
可否判定は **`session_privs` で行う**。`sid, serial#` も `v$session` から
取れた（`632,49239`）。

### M. 12c（`192.168.66.6`）は今は繋がらない

`ORCL` / `ORCL.localdomain` / `ORCLCDB` / `ORCLPDB` / `PDBORCL` / `XE` /
`orclpdb1` をサービス名で、`ORCL` / `ORCLCDB` / `XE` / `orcl` を SID で試して
**全部 ORA-12514 / ORA-12505**。リスナーは応答するので、
**インスタンスが登録されていない**（停止中と思われる）。SQLcl でも同じになる。

JA16SJISTILDE での文字コード検証（D の本番）は**保留**。
go-ora は 832 / 838 / 830 の変換表を同梱しているので、動くときに測る。
なお probe は SID 指定に対応した（`-sid ORCL`）。

---

# 3. Phase 1 へ持っていく修正点

| | 設計のどこ | |
|---|---|---|
| F | §10 / §13 / §18 | **往復を数える設計にする。**メタ情報は 1〜2 文に畳む、`GET_LINES` で一括 |
| G | §5 | `DATE` / `TIMESTAMP` は**オフセットを付けない** ISO-8601 で返す |
| H | §5 | LOB は `DBMS_LOB.SUBSTR` で切るか stream に切り替える。**既定で全部来る** |
| I | §6 / §13 | **暗黙コミットを `stmt/result` で知らせる** |
| J | §6 | 明示 ROLLBACK は**続行**（理由を「実測で安全」から「条件で変わるから」に差し替え） |
| L | §9 | 段 2 の可否は `session_privs` で見る |
| E' | §9 | **段 0 は実装しない。**段 1 で足りる |
| C' | §8 | エラー位置は `Error()` の文字列から 0 始まりで取れる |
| B | §5 | NUMBER は**全部** string。`ScanType` は信じない |
| K | §5 | 1 列だけ NULL の結果で行が消える癖がある |

## probe 自身の反省（計測の作法）

- **基準を測らずに割り込みを測ってはいけない。** `connect by` は ORA-30009 で
  即死し、PL/SQL の空ループは**オプティマイザに消され**（30M 回が 107 ms）、
  どちらも「割り込みが効いた」ように見えた。`sqrt(i)` を入れて消えなくした
- **親 `context` を観測用の問い合わせに使い回さない。** 前の文の watchdog が
  break を投げてきて、無関係な `SELECT COUNT(*)` が ORA-01013 になった
  （2 回測り直した。issue #767 が言っている race の入口と同じ）
- `-timeout` の既定 60 秒では `writes` が途中で切れた
