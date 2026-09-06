# magit-gitd

Emacs の magit から git の実行を肩代わりする常駐プロセス（Rust）。

magit のリフレッシュが遅い原因は **git ではなく Emacs のプロセス生成コスト**
なので、git を常駐プロセス側から起動して迂回する。さらに結果をキャッシュして
先読みし、変化が無ければ git を 1 回も起動しない。

Emacs 側は `user-lisp/my-gitd.el`。`magit-process-file` に `:around` を張って
ここへ流す。**Windows 専用**（macOS / Linux はプロセス生成が速いので入れる
意味が薄い）。

```
Emacs ──stdio (JSON-RPC 2.0 / Content-Length フレーミング)──> magit-gitd ──spawn──> git
```

## なぜ必要か

同じ `cmd.exe` を起動するのに **PowerShell が約 20 ms、Emacs の
`call-process` は 59～76 ms**（2026-09 実測、3 回反復して再現を確認）。
約 40 ms が Emacs 側のプロセス生成経路のコストで、git にも Defender にも
由来しない。

| | |
|---|---|
| `magit-refresh-buffer` 1 回 | 1669 ms / **git 呼び出し 29 回** |
| → 1 回あたり | 56～58 ms |

時間は呼び出し回数に完全に線形で、リポジトリの規模にはほぼ依存しない。

効かなかった対策（試して確認済み。もう一度試さないこと）:
Defender 除外 / `core.fsmonitor` / `cmd/git.exe` ラッパの回避（magit は既に
回避済み）/ `magit-status-sections-hook` の削減。

## 効果

`magit-refresh-buffer` 1 回（`~/.emacs.d`、28 コマンド）:

| | 時間 | Emacs からの git 起動 | デーモンでの git 起動 |
|---|---|---|---|
| デーモン無効 | 1503 / 1544 ms | 28 | — |
| 段階 2a（素通し） | 683 / 672 ms | 0 | 28 |
| **段階 2b（キャッシュ）** | **56 / 54 ms** | 0 | **0** |
| 段階 2b（0.3 秒前に先読み） | 72 / 54 ms | 0 | 0 |
| 段階 2b（直前に先読み = `g`） | 208 ms | 0 | 28（並列） |

**1.7 秒が 50～70 ms になった。** Rust の `Command` からの spawn は 28.9 ms
（`git status -z --porcelain`）で Emacs の約半分。stdio の往復は **0.13 ms**、
28 回でも 4 ms なので、残る 50 ms はほぼ magit 自身の Elisp（セクションの
構築と描画）。**ここから先はデーモンでは縮まない。**

## ビルド

`tree-sitter/` の文法や `ptyd/` と同じ扱いで、**ソースは git 管理下、
`gitd/target/` は `.gitignore`** して各マシンで作る。

```
M-x my:gitd-build          ; cargo build --release
```

```sh
cd ~/.emacs.d/gitd && cargo build --release
```

**バイナリが無ければ `my:gitd-mode` は何もしない**（`my:gitd--available-p` が
nil を返して素通しになる）ので、まだビルドしていないマシンでは自動的に
従来動作になる。依存は `serde` / `serde_json` のみ。

## Emacs 側から使う

| | |
|---|---|
| `M-x my:gitd-stats` | 経由回数 / キャッシュヒット率 / フォールバック数 / 累計短縮時間 |
| `M-x my:gitd-daemon-stats` | デーモンが持っているリポジトリごとの状態 |
| `M-x my:gitd-restart` | サーキットブレーカが落ちたときの復帰 |
| `M-x my:gitd-mode` | 機能そのものの ON / OFF |

| 変数 | |
|---|---|
| `my:gitd-executable` | 既定 `~/.emacs.d/gitd/target/release/magit-gitd.exe` |
| `my:gitd-cache` | nil にすると段階 2a と同じ素通しプロキシになる |
| `my:gitd-verify` | シャドウモード（後述） |

## プロトコル

stdio 上の JSON-RPC 2.0。フレーミングは LSP 方式
（`Content-Length: N\r\n\r\n<json>`）で、Emacs 側は同梱の `jsonrpc.el` を使う。
`PROTOCOL` は **2**。Emacs 側の `my:gitd-protocol` と不一致なら Emacs は
機能を無効化して「M-x my:gitd-build で作り直してください」と促す。

| メソッド | |
|---|---|
| `initialize` | `{protocol, version, pid, threads}` を返す |
| `env/register` | `{id, env: ["NAME=VALUE", …]}`。`process-environment` をそのまま登録する |
| `git/run` | 下記 |
| `repo/prewarm` | `{repo, token}`。通知。先読みを頼む（応答は待たない） |
| `repo/forget` | `{repo}`。そのリポジトリの状態を捨てる |
| `gitd/stats` | リポジトリごとの token / キャッシュ数 / hits / misses / prewarms |
| `shutdown` | 応答してから終了する |

`git/run` のパラメータと戻り値:

```jsonc
// 要求
{ "program": "…/git.exe",   // Emacs が解決した実体。デーモン側では探さない
  "cwd": "c:/Users/…",       // toplevel ではなく default-directory
  "args": ["--no-pager", "status", "-z", "--porcelain"],
  "env": "e1f2…",            // env/register で登録した id
  "want_stderr": false,      // BUFFER が (t "FILE") のときだけ true
  "repo": "c:/Users/…",      // 監視中のリポジトリのルート（任意）
  "token": 42,               // リポジトリ状態の通し番号（任意）
  "role": "cache" }          // "cache" / "prelude" / なし
// 応答
{ "exit": 0, "stdout": "…base64…", "stderr": null,
  "elapsed_ms": 29, "cached": true }
```

stdout / stderr を base64 で返すのは、git の出力が任意のバイト列を含みうる
ため（JSON 文字列には直接入らない）。

エラーコード:

| | |
|---|---|
| `-32601` / `-32602` | 未知のメソッド / パラメータ不正 |
| `-32001` | 未登録の env id。**git は起動していない**ので Emacs は登録し直して再送する |
| `-32002` | spawn 自体の失敗。同じく git は起動していない |

stdin が EOF になったら終了する。Emacs が死んだときの後始末はこれで足りる。

## 設計上の約束

- **git の「意味」を持たない。** 渡されたものをそのまま実行して返すだけで、
  キャッシュしてよいかどうかも Emacs が `role` で指示する。デーモンが git の
  サブコマンドを解釈し始めると、magit 側の事情（どの引数が書き込みか）が
  2 か所に散らばって必ず食い違う
- **環境変数は `env_clear()` してから Emacs に渡されたものだけを使う。**
  継承した環境が混ざると「デーモン経由のときだけ挙動が違う」バグになる
- 子プロセスはワイド API で起動されるので、cp932 に無い文字を含むパスは
  `call-process` より**むしろ改善**になる

## キャッシュの正しさ（2b でいちばん難しいところ）

**古い答えを返すキャッシュは静かに壊れる。** magit が事実と違う内容を表示し、
しかもユーザはそれに気づけない。そこで無効化を「通知」ではなく
**トークン**で表現している。

Emacs は `git/run` のたびに `repo`（監視中のリポジトリのルート）と `token`
（そのリポジトリ状態の通し番号）を載せてくる。デーモンは
`(repo, token, コマンド)` でキャッシュし、token が違えば問答無用でミスにする。
**無効化通知は存在しない**ので、「通知を 1 つ落とすと永久に古いまま」という
壊れ方をしない。

正しさの条件が「Emacs が変化を漏れなく通知すること」から
**「トークンが古い状態を指し続けないこと」**に変わるのが要点で、後者は
Emacs 側だけで閉じている。token を上げるのは `my-magit-watch.el` の 3 か所:

1. 分類を通った w32notify イベント 1 件ごと（外部からの変更）
2. `magit-pre-refresh-hook` — magit 自身の書き込みと、ユーザの `g`
3. デーモン経由で書き込みコマンドが走ったとき

2 番目が要なのは、`magit-run-git-with-input`（`call-process-region`）と
`magit-start-process`（非同期）が**デーモンを通らない**ため。magit は
コマンドの後に必ず `magit-refresh` を呼ぶのでここで捕まる。
**`g` が必ず本当のことを言う**のもこれで保証される。

**監視が動いていなければ `repo` も `token` も付かず、キャッシュも先読みも
行われない。** キャッシュの寿命が監視の寿命に従属しているのが最大の安全弁。

## 先読み

`repo/prewarm` を受けると、**直前のリフレッシュで実際に来たコマンド列**
（`observed` → `recipe`）を並列に走らせてキャッシュを埋める。magit の内部を
知る必要は無い。同じコマンドが二重に起動しないよう single-flight にして
あるので、先読みが間に合わなくても損はしない（`g` を押した瞬間に頼んでも、
magit の要求は走っている先読みに**合流**する）。

**`update-index --refresh` は先読みの先頭で直列に走らせる**（`role: "prelude"`）。
これを飛ばすと `diff-files` が「stat が古いだけ」のファイルを変更ありと報告し、
その答えがキャッシュに残る。実測:

```
内容を変えずに書き直したあと
  update-index --refresh 無し → diff-files が 3 ファイルを M と報告
  update-index --refresh 後   → diff-files は何も報告しない
```

`git diff`（磁器）は自分で内容を比較するので影響を受けないが、magit の
`magit-unstaged-files` は `diff-files` を使う。prelude の対象はこの 1 つだけで、
デーモンは相変わらず git の意味を知らない。

**レシピが全部キャッシュ済みなら先読みごと打ち切る。** これが無いと prelude
だけが毎回走り、それが `.git` と `.git/index.lock` のイベントを出して、
Emacs 側の監視がまた先読みを頼み、無限に回り続ける（読み取りだけの git でも
ファイル変更イベントは出る。`status --porcelain` ですら index.lock を作る）。

上限（`src/main.rs` の定数）:

| | |
|---|---|
| `CACHE_CAP` 256 | 1 リポジトリあたり。超えたら丸ごと捨てる（LRU を持つ規模ではない） |
| `RECIPE_CAP` 128 | magit のリフレッシュ 1 回は 29 コマンド程度 |
| `PRELUDE_CAP` 4 | 実際には 1 つ |
| `PREWARM_ROUNDS` 3 | ビルド中に延々と回らないように |
| `MAX_THREADS` 8 | 並列度の上限 |

## 安全側の作り（Emacs 側）

- **フォールバック。** バイナリが無い / デーモンが死んだ / 形態が未知なら
  黙って素の `process-file` に戻る。3 回続けて失敗したらそのセッションでは
  使わない（`M-x my:gitd-restart` で復帰）
- **二重実行の防止。** デーモンが応答前に死ぬと git が既に走ったかは
  分からない。読み取り専用なら再実行してよいが、それ以外は再実行せず
  エラーを返す（`git add` を 2 回走らせない）
- **タイムアウトを設けない。** 素の `process-file` にも無いので、挙動を
  変えないことが最も安全。`jsonrpc-request` は `:timeout nil` でタイマーが
  完全に無効になり、待ちは `accept-process-output` なので `C-g` で抜けられる
- **`BUFFER` に整数（`0`）が来たら必ず弾く。** `magit-run-gitk` が使う
  「非同期・出力破棄」の意味で、同期実行すると gitk のウィンドウを閉じるまで
  Emacs が固まる

## 検証はシャドウモードで

```elisp
(setq my:gitd-verify t)
```

読み取り専用コマンドを**デーモン経由と素の `process-file` の両方で実行して
バイト単位で比較**する。差異は `*gitd verify*` に記録される。この設計で唯一
こわいのは「静かに壊れる」ことなので、**壊れていないことを実使用で証明する**
のがこの機能の役目。遅くなるので常用はしない。

書き込みコマンドはシャドウモードにかけられない（2 回走らせるわけには
いかないため）。stage / unstage / commit は GUI プローブで自動検証しているが、
discard / rebase / merge / cherry-pick、コンフリクト中の操作、サブモジュールは
まだ実際に操作して確かめるしかない。

## 文字コードの地雷（3 つとも実際に踏んだ）

Emacs 側の `my:gitd--to-text` が処理する。**`args` / `cwd` / `program` / `env`
の全部に適用すること。**

1. **`process-environment` に JSON に載らない項目がある。** `PSModulePath` が
   OneDrive の「ドキュメント」を ANSI の生バイトのまま含んでおり
   `json-serialize` が `wrong-type-argument json-value-p` で落ちる。
   **PowerShell から Emacs を起動したときだけ再現する**（bash 経由では出ない）
2. **復号に `locale-coding-system` を使うと直らない。** あれは**コンソールの**
   コードページで、PowerShell 7 では `cp65001`（UTF-8）。環境変数ブロックは
   **ANSI コードページ**（`w32-ansi-code-page` = 932）で別物
3. **引数も ANSI に encode されている。** `magit-process-git-arguments` が
   意図的にやっている（Emacs の `call-process` が ANSI API を使うため。
   magit issue #3250）。デーモン境界で復号し直す

## その他の注意

- **`default-directory` は必ず `expand-file-name` する。** Emacs は file
  バッファの `default-directory` を `~/...` に略記することがあり、
  `call-process` は内部で展開するが **Rust の `current_dir` は展開しない**。
  そのまま渡すと `ディレクトリ名が無効です (os error 267)` になる
- **`magit-process-record-invocations` が有効なときは素通しにする。**
  magit の呼び出しログは `magit-process-file` の本体にあるので、`:around` で
  `orig` を呼ばずに済ませると記録されない。同じ理由で
  **`magit-process-file` に後から足した advice も呼ばれない**（テストを書くときは
  `my:gitd-mode` を有効にした**後**に足すこと）

## 既知の課題

- **`C-g` で git が止まらない**（優先度低）。素の `call-process` は `C-g` で
  子プロセスを kill するが、デーモン経由では git が走り切る。対処するなら
  `$/cancel` 通知を足してデーモン側で子を kill する。半端に kill された
  `.git/index` より安全とも言える
- **イベントが落ちるとキャッシュが古いままになりうる**（優先度低）。
  `w32notify` はバッファ溢れでイベントを落とす（実測で 1000 ファイルに対し
  4095/10000）。保険は 2 つあり、`.git` の粗い mtime 更新（`suspect`）から
  フィンガープリントの不一致でトークンが進むことと、**`g` を押せば必ず進む**
  こと。根本的に塞ぐには `ReadDirectoryChangesW` の溢れ通知が要る（段階 2c）

## 関連ドキュメント

| | |
|---|---|
| `docs/magit/magit-auto-refresh-plan.md` | 全体の計画と最初の計測 |
| `docs/magit/magit-gitd-2a-design.md` | 段階 2a（素通しプロキシ）の設計 |
| `docs/magit/magit-gitd-2b-design.md` | 段階 2b（キャッシュ・先読み）の設計 |
| `docs/magit/magit-autorefresh-stage1-design.md` | 監視側（`my-magit-watch.el`） |
