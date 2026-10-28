# oracled

Emacs から Oracle を使うための軽量クライアント。**Go の常駐プロセス
（`oracled.exe`）+ Elisp（`user-lisp/my-oracle.el`）**の 2 枚で、
SQLcl を介さずに直接 DB と話す。

```
Emacs ──stdio (JSON-RPC 2.0 / Content-Length)──> oracled ──TCP──> Oracle
```

**現状: Phase 3 まで完了（2026-10-28）。** daemon（`cmd/oracled`）と Elisp
（`../user-lisp/my-oracle.el`）が動いている。`.sql` バッファからの送信、
**`SQL> ` プロンプトの対話バッファ**、**`@file`**、**`DBMS_OUTPUT`**、
**`DESCRIBE` / オブジェクト一覧 / 補完**、**バインド変数 / 縦表示 /
CSV・JSON 書き出し / `EXPLAIN PLAN`** まで。

| | |
|---|---|
| 設計 | [docs/oracle-client-design.md](docs/oracle-client-design.md) |
| **Phase 0 の実測** | [docs/phase0-findings.md](docs/phase0-findings.md) |

Phase 0 で確かめたこと（21c XE / RTT 54 ms の回線）。

| | |
|---|---|
| 起動〜使える状態 | **567〜837 ms**（SQLcl は 7〜8 秒） |
| **割り込み** | CPU を回している文が **165〜181 ms で停止**。`ORA-01013` が返り**セッションは生存** |
| 日本語 | bind とリテラルで**バイト列が完全一致**（`𠮟` / 絵文字 / `〜—−` も） |
| 型 | NUMBER は**常に文字列**で精度が落ちない。NULL と空文字も区別できる |
| 1 文の往復 | **125.8 ms**。中身はネットワーク（RTT の 2 往復）で、**時間は往復の回数で決まる** |

## なぜ必要か

`sql.el` から SQLcl を使う形（`M-x my:sql-sqlcl`、
[../docs/languages/sqlcl.md](../docs/languages/sqlcl.md)）は動いているが、
SQLcl が **CLI** であることに由来するコストを払い続けている。

| | |
|---|---|
| 起動 | **7〜8 秒**（JVM + 接続） |
| やりとり | **テキスト**。プロンプト同期・列幅・ANSI escape・右埋めを読み合う |
| 文字コード | stdin だけ native（cp932）。`JDK_JAVA_OPTIONS` で揃えている |
| 割り込み | **手段が無い**（走っている SELECT を止められない） |
| SQL\*Plus 互換の残骸 | 裸の `/` が直前の文を再実行する、`SHOW` が 1 語しか取らない |

**値を構造のまま受け取れば、この 5 つはどれも消える。**
桁揃えも Emacs 側でやったほうが正しい（SQLcl の列幅は DB 側のバイト幅なので、
日本語を含む列は端末で必ずずれる）。

## 設計の要点

- **ドライバは [sijms/go-ora](https://github.com/sijms/go-ora)（純 Go）。**
  Oracle Instant Client が要らないので、`gitd` / `ptyd` と同じ
  「`M-x my:oracle-build` で作って終わり」になる
- **daemon は表示を作らない。** 型付きの値を JSON で返し、整形は Emacs 側
- **daemon は SQL しか知らない。** `SET` / `COLUMN` / `SPOOL` は実装しない。
  ただし**「それが SQL でないこと」を見分ける責任は持つ**（`@foo.sql` は
  `;` ではなく行末で終わる。文の切り分けに必要）
- **`sql.DB` のプールを使わない。** session は `*sql.Conn` を 1 本握り続ける
  （トランザクション・`ALTER SESSION`・`DBMS_OUTPUT` が壊れるため）
- **割り込みは 3 段構え。** `context` cancel（go-ora の break）→
  `ALTER SYSTEM CANCEL SQL` → 接続破棄
- **SQLcl 版は消さない。** `ddl` / `datapump` / フルスペックのスクリプトは
  あちらに任せる

### 【重要】Oracle は正常 logoff でコミットする

未コミットのトランザクションを持ったまま普通に切断すると、Oracle は
**ロールバックではなくコミットする**。セッションを閉じる全経路
（`session/close` / `shutdown` / stdin EOF / Emacs 終了）で**必ず明示
`ROLLBACK` を先に出す**。`os.Exit` に直行する道を作らない。

## ディレクトリ

| | |
|---|---|
| `cmd/oracled/` | **daemon 本体。**JSON-RPC の捌き（`main.go`）と統合テスト |
| `internal/sqlsplit/` | **文の切り分け（lexer）。**終端子・コメント・`q'[]'`・`@file` の規則はここだけが持つ。テーブル駆動テスト 37 件 |
| `internal/oradb/` | session（固定した `*sql.Conn`、tx、行の分割送信、割り込み、型変換） |
| `internal/rpc/` | stdio 上の JSON-RPC 2.0（`Content-Length`） |
| `cmd/probe/` | Phase 0 の計測用コマンド。設計の前提を測り直すときに使う |
| `docs/` | 設計と実測の記録 |
| `oracled.exe` | ビルド成果物。**git 管理外**（`ptyd.exe` と同じ扱い） |

## ビルドと実行

Go のツールチェインが要る（`ptyd` と同じ）。依存は go-ora のみ。

```sh
cd ~/.emacs.d/oracled
go build -o oracled.exe ./cmd/oracled
go build -o probe.exe ./cmd/probe     # Phase 0 の計測用
go test ./...                         # DB 無しでも通る（lexer と RPC）
ORACLED_PASSWORD=... go test ./cmd/oracled/   # 実 DB に繋ぐ統合テスト
```

Emacs 側は `M-x my:oracle-build` でも作れる。

### 【重要】パスワードはコマンドライン引数に載せない

`tasklist` から見えてしまうので、**`probe` も daemon も引数では受け取らない。**
probe は環境変数から読む。daemon は stdio の RPC パラメータで受け取る。

```sh
export ORACLED_PASSWORD=...     # これだけは環境変数
./probe.exe info -host 192.168.66.101 -service XEPDB1 -user TEST2TAX
```

| サブコマンド | |
|---|---|
| `info` | 接続して時間を測る。版・キャラクタセット・sid/serial を出す |
| `types` | 型マッピングの実測（`NUMBER` / `DATE` / LOB / NULL と空文字） |
| `jp` | 日本語の往復（`DUMP(..., 1016)` で突き合わせる） |
| `cancel` | 重い文を投げて割り込む。**止まるか / 何 ms か / 後でセッションが使えるか** |
| `plsql` | PL/SQL ブロックと `DBMS_OUTPUT` の取り出し |
| `bench` | 1 行の往復と 1000 行の取得 |
| `query` | 任意の SQL を投げて Go 側の型を出す |

## 段階

| | 内容 |
|---|---|
| ~~**Phase 0**~~ | 完了。繋ぐ / 割り込む / 日本語 / 型 / 書き込みと tx を実測 → [docs/phase0-findings.md](docs/phase0-findings.md) |
| ~~**Phase 1**~~ | 完了。JSON-RPC・session・`sql/exec` + 行の分割送信・`sql/split` + テスト・エラーの構造化。Emacs 側は送信・結果の表・割り込み・tx |
| ~~**Phase 2**~~ | 完了。対話バッファ（`SQL> `）・`@file`・`DBMS_OUTPUT`・tx 表示 |
| ~~**Phase 2.5**~~ | 完了。`DESCRIBE`・オブジェクト一覧・補完（往復しない） |
| ~~**Phase 3**~~ | 完了。バインド変数・縦表示・CSV/JSON 書き出し・`EXPLAIN PLAN`・LOB の上限 |
| Phase 2.5 | `meta/describe` / `meta/objects` / 補完 |

## Emacs 側（`my-oracle.el`）

`.sql` バッファで使う。SQLcl 版（`my:sql-sqlcl`）と衝突しないよう、
送信は `C-c C-q`、残りは `C-c q` のプレフィクスに固めてある。

| キー | |
|---|---|
| `C-c q o` | 接続（`my:oracle-connections` から選ぶ。パスワードは `auth-source`） |
| `C-c C-q` | **点のある文を送る**（文の範囲は daemon の `sql/split` が決める） |
| `C-c M-q` | リージョンを送る |
| `C-c C-k` | **割り込み**（実測 165〜260 ms。セッションは残る） |
| `C-c q c` / `C-c q b` | commit / rollback |
| `C-c q a` | autocommit の切り替え（tx 中は daemon が拒否する） |
| `C-c q d` | **describe**（点の上の名前が既定。`C-u` で制約・索引も） |
| `C-c q l` | オブジェクトを選んで describe（`C-u` で一覧を取り直す） |
| `C-c q g` | メタ情報を取り直す（`C-u` で全スキーマ） |
| `C-c q x` | **縦表示**と表を切り替える（列が多い 1 行を読むとき） |
| `C-c q w` | 結果を CSV / TSV / JSON で書き出す |
| `C-c q e` | **実行計画**（`EXPLAIN PLAN`。文は実行しない） |
| `C-c q m` | 止めたカーソルから続きを取る |
| `C-c q r` | **対話バッファ**（`SQL> ` プロンプト）を開く |
| `C-c q f` | スクリプトを選んで実行（`@file` と同じ） |
| `C-c q s` | `DBMS_OUTPUT` の入り切り |
| `C-c q i` / `C-c q q` | 状態を出す / 切る |

### 対話バッファ（`*oracle(接続名)*`）

`comint` 派生。入力履歴（`M-p` / `M-n`）とプロンプトの read-only が効く。

| キー | |
|---|---|
| `RET` | **文が閉じていれば送る。閉じていなければ改行**（判定は daemon の `sql/split`） |
| `C-j` | 常に改行 |
| `C-c C-c` | 閉じていなくても強制的に送る |
| `C-c C-k` | 割り込み |

PL/SQL ブロックは SQL\*Plus と同じく**単独行の `/` で終わる**まで待つ。
トランザクション中はプロンプトが `SQL*> ` になる（**実際に書いたときだけ**。
`SELECT` だけでは付かない）。

**入力は SQL としてハイライトされる**（打っている途中・貼り付けたもの・
すでに送った文のすべて。結果の表やメッセージは対象外）。`sql-mode` の
Oracle 用 font-lock を `comint-fontify-input-mode` で入力部分だけに当てて
いるので、**閉じていない引用符や複数行の `/* */` が隣の入力へ漏れない**。
送った文の色を残すために `comint-highlight-input` は nil にしてある
（確定した入力を太字にする代わりに、SQL の色をそのまま残す）。
切るなら `my:oracle-repl-highlight` を nil（バッファごとに
`M-x comint-fontify-input-mode` でも入り切りできる）。

### `@file`

対応するのは **DML / DDL / PL/SQL ブロック / `@` / `@@` / `EXEC` / `PROMPT` /
`EXIT` / `COMMIT` / `ROLLBACK`** まで。`SET`（`SERVEROUTPUT` を除く）/
`COLUMN` / `SPOOL` / `DEFINE` / `&1` / `WHENEVER` は**未対応**。

**【重要】未対応のコマンドは Oracle に送らず、「12 行目: SPOOL は未対応
（無視しました）」と報告し、最後に「無視 n」を要約に入れる。** 送ると
ORA-00900 の山で本当のエラーが埋もれ、黙って飛ばすとスクリプトが半分だけ
効いたことに気づけない。

エラーが出たら既定で止まり（`my:oracle-script-on-error`）、
**トランザクションはそのまま残す**（自動 commit も自動 rollback もしない）。

文に**バインド変数**（`:name`）があると、送る前に値を訊く（名前の抽出は
daemon の lexer がやるので、文字列やコメントの中の `:` を取り違えない）。
`&var` の置換と違い**本物のバインド**として渡るので、引用符を含む値でも壊れない。

LOB と長い文字列は `my:oracle-lob-limit`（既定 4096 文字）で切り詰めて
`…(全 N 文字)` と出す（`C-u C-c C-q` で送ると上限が上がる）。
**go-ora は既定で LOB の中身を丸ごと転送する**ので、これが無いと
巨大な CLOB で stdio が埋まる。

結果は `*oracle(接続名):rows*` に表で出る（桁は `string-width` で揃えるので
**日本語を含む列でもずれない**）。履歴は `*oracle(接続名):log*`。
エラーは `ORA-NNNNN` と本文を出し、**位置が取れればソースのその場所へ飛ぶ**。
