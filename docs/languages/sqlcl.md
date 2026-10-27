<!-- -*- gfm -*- -->

# SQLcl を sql.el から使う

`user-lisp/my-lang-misc.el` の `[3] SQL — SQLcl (Oracle)` 節。
実測は 2026-10-07（成立するかの下調べ）と 2026-10-27（実装と作り込み）。

| | |
|---|---|
| 入口 | `M-x my:sql-sqlcl` |
| 接続先 | SQLcl の connmgr に保存した名前（`my:sql-sqlcl-connection` が既定値） |
| 実測環境 | SQLcl 26.3.0（scoop） / Oracle Database 21c XE / Windows 11 / Emacs 31.1 |

## なぜ別製品にしたか

`sql.el` の `oracle` 製品は **SQL*Plus** を前提にしている。SQLcl でも REPL 自体は
そのまま成立するが（`^SQL> ` を出す / 入力をエコーしない / 逐次フラッシュする）、
`sql-oracle-save-settings` が **複数オプションの `SHOW`** を投げるところだけ通らない。

```
SQL> SHOW ARRAYSIZE AUTOCOMMIT LINESIZE PAGESIZE
SP2-0158: SHOWオプション"ARRAYSIZE"が不明です
SP2-0158: SHOWオプション"AUTOCOMMIT"が不明です …
```

これに巻き込まれるのは `sql-list-all` / `sql-list-table` / 補完
（`:completion-object`）の 3 つ。`oracle` を書き換えて直すと SQL*Plus が使えなく
なるので、**`sqlcl` という別製品を `sql-add-product` で足した**。

| oracle から流用 | 差し替え |
|---|---|
| `:font-lock`（`sql-mode-oracle-font-lock-keywords`） | `:sqli-comint-func` |
| `:statement`（`sql-oracle-statement-starters`） | `:list-all` / `:list-table` |
| `:terminator` / `:syntax-alist` | `:completion-object` |
| `:input-filter`（`sql-placeholders-filter`） | `:sqli-login`（接続名だけ） |

`:completion-object` は問い合わせ自体は `sql-oracle-completion-object` を呼ぶ
（`sql-oracle-completion-types` もそのまま効く）。SHOW を通さない状態を自分で
作ってから渡しているだけ。

## 起動とログイン

```
sql -L -name WebCoreDB_main
```

`-name` は connmgr に保存した接続を使う。パスワードは SQLcl 側（`credentials.sso`）
にあるので Emacs 側では訊かない。保存済みでない文字列を入れた場合は接続文字列
（`USER/PASS@HOST:PORT/SERVICE`）としてそのまま渡し、空なら `/nolog` で起こす。

- **`-S` を付けてはいけない。** バナーと一緒にプロンプトまで消え、`sql-redirect` が
  完了を判定できなくなる
- **接続名の一覧は `connmgr list` を叩かずファイルから読む**
  （`%APPDATA%\DBTools\connections\<ハッシュ>\dbtools.properties` の `name=`）。
  JVM の起動に数秒かかるので、補完のたびに起こすわけにはいかない
- 起動は **7〜8 秒**（JVM + 接続）。`sql-product-interactive` はプロンプトが
  出るまで待つ（`sql-login-delay` は既定 7.5 秒だが、出力が届いている間は待ち続ける）

## 【重要】接続が済む前に stdin へ書くと、二度と応えない

起動から最初の `SQL> ` までの 8 秒の間に 1 行でも送ると、**SQLcl は以後
プロンプトも結果も一切返さなくなる**。実測:

| 送信 | 結果 |
|---|---|
| 何も送らない | `SQL> ` が出る。以後正常 |
| 起動直後に `SET LINESIZE 200` | **プロンプトが出ない。その後の SELECT も無反応** |

エラーは出ず、バナーと「接続先: …」までは普通に表示されるので、**見た目は
「繋がっているのに無言」**。セッションを作り直すしかない。

そのため 2 つ仕掛けてある。

- 起動時の設定（`my:sql-sqlcl-startup-commands`）は、`comint-output-filter-functions`
  で**最初のプロンプトを見てから**送る（`my:sql-sqlcl--on-output`）
- ユーザーの入力はそれまで**保留**して、プロンプトが出てから順に流す
  （`sql-input-sender` への advice `my:sql-sqlcl--hold-input`）。comint の入力も
  `sql-send-region` もここを通るので 1 箇所で押さえられる

**バッファローカルな設定を `:sqli-comint-func` の中でやってはいけない。**
`sql-interactive-mode` は `comint-mode` 経由で `kill-all-local-variables` を通るので
消える。入れるのは `sql-interactive-mode-hook`（`my:sql-sqlcl--interactive-setup`）。

## 【重要】継続プロンプト除去フィルタを外す

SQLcl は**継続プロンプト（`  2  `）を出さない**。複数行の文は完成するまで
何も返さず、完成すると結果と `SQL> ` を 1 回返す。

`sql-interactive-mode` は `sql-interactive-remove-continuation-prompt` を
`comint-preoutput-filter-functions` に無条件で入れる。これは「送った文の改行数ぶん
`comint-prompt-regexp` に一致するものを出力から削る」フィルタなので、継続プロンプトが
来ない SQLcl では**本物の `SQL> ` を代わりに食う**。実測（3 行の SELECT を送信）:

```
;; フィルタあり              ;; フィルタなし
   C3 D                      SQL>
_____ ___________               C2 D
    3 26-10-27              _____ ___________
                                2 26-10-27
```

プロンプトが画面から消えるだけでなく、プロンプトの数が合わなくなるので、
以後の `sql-redirect` が**1 つ前のプロンプトで完了したことにしてしまう**
（一覧が空になる / 前の文の echo が結果として入る）。

## 【重要】裸の `/` は直前の文をもう一度実行する

`sql-send-string` は本文を送った後に `sql-send-magic-terminator` を呼ぶ。
**`sql-send-terminator` が nil（既定）でも終端子は送られる**（関数の中で nil が
`";"` に読み替えられる）。SQLcl での実測:

| 送るもの | SQLcl の反応 |
|---|---|
| 裸の `;` | バッファの **listing だけ**（`1* select …`）。空なら `SP2-0223` |
| 裸の `/` | **直前の文を実行する**（`systimestamp` が変わることで確認） |

`sql-send-terminator` を `t` にすると製品の終端子（oracle 流儀の `/`）が送られる
ので、**`show linesize` を送っただけで直前の DELETE が走りうる**。
`my:sql-sqlcl--magic-terminator`（advice）で、SQLcl のときは

- 既に `;` か `/` で終わっていれば何も足さない（PL/SQL ブロックの `/` も含む）
- そうでなければ `;` だけを足す（`/` は絶対に送らない）

という形に固定してある。

残る見た目の問題として、`;` でも `/` でも終わっていない文字列（`SET` や `SHOW` の
行など）を送ると**直前の文の listing が返る**。無害なので放置している。

## 【重要】`INFO+` の直後はプロンプトの手前に escape が入る

SQLcl は見出しを必ず ANSI の太字で出す。**`SET COLOR OFF` は受け付けるが効かない**
（DESCRIBE で ESC 6 個、`INFO+` で 24 個を実測）。

問題は数ではなく位置。`INFO+` の出力は**最後の行が `\e[mSQL> ` で終わる**ので、
`"^SQL> "` では一致しない。`sql-redirect` の完了待ちループには**タイムアウトが無い**
ため、**Emacs がそのまま固まる**。

| コマンド | 末尾 | `^SQL> ` |
|---|---|---|
| SELECT / DESCRIBE / SET | `…\nSQL> ` | 一致 |
| **`INFO+`** | `…\n\e[mSQL> ` | **一致しない** |

そのため `:prompt-regexp` を `"^\\(?:\e\\[[0-9;]*m\\)*SQL> "` にしてある。

**会話バッファ側ではこれに気づけない。** `ansi-color-process-output` が
escape をバッファから消すので、見た目は普通のプロンプトになる。逆に
`sql-redirect` の出力先は comint ではないので escape が生で残る。
そのため一覧表示では `ansi-color-apply-on-region` を自分で当てている
（`my:sql-sqlcl--ansi-color`）。

## 文字コード — 出力は UTF-8、入力は native

`~/bin/sql.cmd`（JLine / JVM の警告を抑えるために自分で書いたラッパ）は
`-Dfile.encoding=UTF-8` を渡している。これで**出力**は UTF-8 になるが、
**`System.in` は `stdin.encoding`（既定は native = cp932）で読まれる**ので、
送った日本語リテラルだけが化ける。実測（`select dump('あ', 1016)` の往復）:

| ラッパ | Emacs 側の符号化 | 結果 |
|---|---|---|
| そのまま | cp932 | **正しい**（`Typ=96 Len=3 … e3,81,82`） |
| そのまま | utf-8 | **化ける**（`Len=6 … e7,b8,ba,ef,bf,bd`） |
| `-Dstdin.encoding=UTF-8` を足す | utf-8 | **正しい** |
| `JDK_JAVA_OPTIONS` で同じものを渡す | utf-8 | **正しい** |

cp932 で送る手もあるが、cp932 の外（絵文字や `𠮟`）が落ちる。ラッパを 2 つ
持ちたくもないので、**`JDK_JAVA_OPTIONS` を `process-environment` に足して
UTF-8 で揃える**形にした（`my:sql-sqlcl-java-options`）。

java のランチャは拾ったことを stderr に 1 行書く。消す手段は無いので、
会話バッファの先頭にこれが出るのは正常。

```
NOTE: Picked up JDK_JAVA_OPTIONS: -Dstdin.encoding=UTF-8 -Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8
```

## SQLcl の `SET` / `SHOW` は SQL*Plus の部分集合

知らないオプションは `SP2-0158` を返すだけで、**同じ行の他のオプションは効く**。
足すときは 1 つずつ確かめること。実測:

| 通る | 通らない |
|---|---|
| `LINESIZE`（32767 も可） / `PAGESIZE`（0 も可） / `SERVEROUTPUT` / `FEEDBACK` / `TIMING` / `TRIMOUT` / `TAB` / `COLOR`（受け付けるが効かない） | **`RECSEP`** |

`SHOW` は**オプション 1 つなら通る**（`show linesize` → `linesize 32767`）。
複数並べると最初の 1 語だけ解釈して残りは `SP2-0158`。

設定の退避に SHOW を使えないので、一覧・補完は「必要な `SET` を投げる →
終わったら `my:sql-sqlcl-startup-commands` を流し直す」形にしてある
（`my:sql-sqlcl--restore-settings`）。**現在値を読まないので SHOW が要らない。**

## 一覧表示と補完

| | 実装 |
|---|---|
| `C-c C-l a`（`sql-list-all`） | `user_objects`（`C-u` で `all_objects`）を SELECT。`COLUMN … FORMAT` は使わず SQLcl の自動列幅に任せる |
| `C-c C-l t`（`sql-list-table`） | `DESCRIBE`。**`C-u` なら `INFO+`**（制約・索引・統計・コメントまで出る） |
| 補完（`sql-contains-names`） | oracle の問い合わせを `SET PAGESIZE 0` の状態で呼ぶ |

補完で 1 つ手当てが要る。**SQLcl は列幅に合わせて右側を空白で埋める**
（`SET TRIMOUT ON` でも埋まる）。oracle 側の正規表現は `^\001\(.+\)$` で行末までを
名前として取るので、`string-trim-right` しないと候補が `"MT_BRAND     "` になる。

## 起動直後の `SQL> SQL> `

`my:sql-sqlcl-startup-commands` は**コマンド 1 つにつきプロンプトを 1 つ**増やす
（SET 自体は何も出力しない）。既定は 1 行にまとめてあるので、起動直後の
プロンプトが 1 つ余計に見える。これは正常。

## 無関係だったもの

- **comint が足す環境変数**（`TERM=dumb` / `TERMCAP=` / `INSIDE_EMACS`）は
  SQLcl の挙動に影響しない。`TERM=xterm-256color` でも同じ（実測）
- `comint-process-echoes` は不要。SQLcl は入力をエコーしない
- 専用パッケージ（`uhellstr/sqlcl-shell`）は **PTY 前提**（JLine が自前で
  エコーする）の Linux 向けで、Windows のパイプとは条件が逆

## 使い方の注意

- **`sql-product` の既定は `postgres` のまま**にしてある。`.sql` バッファから
  `C-c C-l a` などを使うには、そのバッファで `M-x sql-set-product` → `sqlcl`
  とする（`sql-find-sqli-buffer` がバッファの `sql-product` で SQLi を探すため）。
  `C-c C-c` での送信は製品が違っても通る
- **PL/SQL ブロックは `/` の行まで含めて送る。** `end;` で終わると SQLcl は
  文が完成していないと見て黙る（プロンプトが出ない）。`/` を送れば実行される
- 送信は `C-c C-c`（段落）/ `C-c C-r`（リージョン）/ `C-c C-n`（1 行）
