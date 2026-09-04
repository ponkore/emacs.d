# 段階 2a: 素通しプロキシ（magit-gitd）— 基礎情報と構成

作成: 2026-09-03 / 状態: **設計中（未決事項は 8 章）**
前提: [magit-auto-refresh-plan.md](magit-auto-refresh-plan.md)

---

## 1. 2a のゴールと非ゴール

**ゴール = 壊れやすい部分を、キャッシュという別の壊れやすさ抜きで確定させること。**

| | |
|---|---|
| **やる** | JSON-RPC の配管、環境変数、文字コード、終了コード、BUFFER の形態、フォールバック |
| **やる** | 「本物の git と 1 バイトも違わない」ことを実使用で検証する仕組み（7 章） |
| **やらない** | キャッシュ、先読み、ファイル監視、自動リフレッシュ |
| **やらない** | 非同期経路（`magit-start-process`）への介入 |

性能は副産物。**1669 ms → 約 670 ms（2.5 倍）の見込み**だが、2a の成否はそこで測らない。
ここが通れば 2b でキャッシュを足しても、疑う場所が 1 つに絞れる。

---

## 2. 実測で確定した基礎情報

### 2.1 性能（2026-09-02〜03 実測）

| 経路 | 対象 | 1 回あたり |
|---|---|---|
| Emacs → 常駐プロセス stdio 往復 | 1 行投げて 1 行受ける | **0.36 ms** |
| Emacs `call-process` | `git status --porcelain` | 55〜58 ms |
| **Rust `Command`** | `git status -z --porcelain` | **28.9 ms** |
| Rust `Command` | `git rev-parse --show-toplevel` | 21.8 ms |
| Rust `Command` | `git --version` | 15.7 ms |

refresh 1 回分（29 コマンド）を通しで:

| 実行主体 | 時間 |
|---|---|
| Emacs（現状の magit） | **1669 ms** |
| Rust 直列 | **659 ms** ← 2a の見込み |
| Rust 4 並列 | 277 ms |
| Rust 8 並列 | **180 ms** ← 2b の先読みの見込み（裏で走る） |

**Rust からの spawn は Emacs の約半分。** Emacs 固有のプロセス生成コスト（段階 0 で確認した
「同じ `cmd.exe` が PowerShell 20 ms / Emacs 60〜76 ms」）を迂回できるため。

base64 + JSON のコスト（Emacs 側）:

| ペイロード | decode |
|---|---|
| 100 KB | 0.48 ms |
| 1 MB | 2.03 ms |
| 2 MB（`json-parse-string` 込み） | **8.99 ms** |
| 5 MB | 8.44 ms |

**→ base64 + `jsonrpc.el` で確定。** 独自フレーミングは不要（計画書 6.3 を解決）。

### 2.2 magit の実行環境（このマシンでの実値）

```
magit-git-executable
  = C:/Program Files/Git/mingw64/libexec/git-core/git.exe
```

**magit は既に `cmd\git.exe` ラッパを回避している。**
`magit-git-executable` の defcustom が Windows では cygpath 経由で
`libexec/git-core` の実体を解決する（だから段階 0 で実体を指定しても差が出なかった）。

そのかわり **`magit--git-w32-path-hack` による PATH の前置が必須**:

```
PATH=C:\Program Files\Git\mingw64\libexec\git-core;<元の PATH>
```

`libexec/git-core/git.exe` はこの PATH が無いとサブコマンドを見つけられない。
**デーモンはこれを再現しなければならない。**

`magit-process-environment` が足すのは実測でこの 2 つだけ:

```
INSIDE_EMACS=31.1,magit
PATH=<上記のハック>
```

`magit-git-global-arguments`（毎回 args の先頭に付く）:

```
--no-pager --literal-pathspecs -c core.preloadIndex=true -c log.showSignature=false
-c color.ui=false -c color.diff=false -c diff.noPrefix=false -c i18n.logOutputEncoding=UTF-8
```

### 2.3 文字コード（計画書 6.6 のリスクは下がった）

```
magit-git-output-coding-system        = utf-8
magit-process-ensure-unix-line-ending = t
default-process-coding-system         = (undecided-dos . undecided-unix)
(magit--process-coding-system)        = (utf-8-unix . undecided-unix)
```

**git 出力のデコードは `utf-8-unix` 固定。cp932 は絡まない**
（magit が `i18n.logOutputEncoding=UTF-8` を強制しているため）。
デーモンからは **生バイトを base64 で受け取り、Emacs 側で
`(decode-coding-string bytes (car (magit--process-coding-system)))` する**。
値を決め打ちせず、必ずこの関数から取ること。

### 2.4 `magit-process-file` の呼ばれ方（実測）

status 表示 + refresh + 各種読み取りで踏んだ 66 回の内訳:

| BUFFER の形 | 回数 | 意味 | 再現方法 |
|---|---|---|---|
| `(t nil)` | 58 | stdout をカレントバッファへ、stderr は捨てる | point に `insert` |
| `(t "FILE")` | 4 | 上 + stderr をファイルへ | 上 + stderr を**生バイトで**ファイルに書く |
| `nil` | 4 | 全部捨てる | 何もしない |
| バッファオブジェクト | 0 | `magit-call-process`（**書き込み経路**）が使う | そのバッファで `insert` |

- **`INFILE` は読み取り経路では 1 度も使われない。** 使われたら素通し。
- **`DISPLAY` は常に nil。**
- 上記 4 形態以外が来たら **無条件に素通し**（default deny）。とくに:

| 形 | 呼び出し元 | なぜ回してはいけないか |
|---|---|---|
| **整数（`0`）** | `magit-run-gitk` / `-branches` / `-all` | **非同期・出力破棄**の意味。同期実行すると gitk の GUI が閉じるまで Emacs が固まる |

`magit-run-gitk*` は `magit-gitk-executable` を、`magit-patch-id` は `shell-file-name` を
渡してくるので、`program` の一致判定でも弾かれる（二重の防御）。

---

## 3. 構成

### 3.1 ファイル配置（決定）

```
~/.emacs.d/
├── gitd/                       ← Rust クレート（git 管理する）
│   ├── Cargo.toml
│   ├── src/main.rs
│   └── target/                 ← .gitignore（各マシンでビルド）
├── user-lisp/
│   └── my-gitd.el              ← 新規モジュール（init.el の require 列に追加）
└── tmp/
    ├── magit-auto-refresh-plan.md
    └── magit-gitd-2a-design.md
```

`tree-sitter/` の文法と同じ扱いにする — **ソースは管理下、ビルド成果物は各マシンで作る**。
`M-x my:gitd-build`（`my:install-treesit-grammars` と同じノリ）を用意し、
バイナリが無ければ機能は単に無効になる（= 従来動作）。

### 3.2 コンポーネント

```
┌─ Emacs (user-lisp/my-gitd.el) ─────────────────────────────┐
│  my:gitd-mode              全体の ON/OFF (global minor mode) │
│  my:gitd--conn             jsonrpc-process-connection        │
│  my:gitd--routable-p       許可リスト判定 (default deny)     │
│  my:gitd-run               1 回の git 実行                   │
│  advice :around            magit-process-file                │
│  my:gitd--fallback         素の process-file へ戻す          │
│  my:gitd-verify-mode       両方走らせてバイト比較 (7 章)     │
│  my:gitd-stats             回数・時間・フォールバック数      │
└────────────────────────────────────────────────────────────┘
              │ stdio (Content-Length + JSON, 往復 0.36 ms)
┌─ Rust (gitd/) ─────────────────────────────────────────────┐
│  main            stdin から 1 リクエスト読む → 実行 → 返す  │
│                  **2a は単一スレッドでよい**                 │
│                  (magit の呼び出しは同期・直列なので)        │
│  env registry    環境変数セットをハッシュで登録・再利用      │
│  stderr          ログ出力 (Emacs 側でバッファに溜める)       │
└────────────────────────────────────────────────────────────┘
```

**2a を単一スレッドにするのは意図的。** Emacs はどうせ応答を待って
ブロックしているので並行性に価値が無く、スレッドを入れないぶん
「配管の検証」という 2a の目的に集中できる。並列化は 2b の先読みで初めて要る。

---

## 4. プロトコル（2a 確定版）

トランスポート: stdio / フレーミング: LSP 方式（`Content-Length:` + JSON）/
Emacs 側は **`jsonrpc.el`**（同梱、eglot と同じもの）。

### 4.1 `initialize`

```jsonc
// →
{"method":"initialize","params":{"protocol":1,"emacs_pid":12345}}
// ←
{"result":{"protocol":1,"version":"magit-gitd 0.1.0","pid":6789}}
```

`protocol` が一致しなければ Emacs 側は機能を無効化してメッセージを出す
（Lisp とバイナリのバージョン不一致対策）。

### 4.2 `env/register` — 環境変数セットの登録

PATH ハックだけで 3 KB あり、毎回 29 回送るのは無駄なので一度だけ登録する。

```jsonc
// →
{"method":"env/register","params":{"id":"e1a2b3","env":["INSIDE_EMACS=31.1,magit","PATH=...", ...]}}
// ←
{"result":{"ok":true}}
```

`id` は Emacs 側で `(sxhash-equal env)` から作る。
`git/run` で未登録の `id` を使うと `unknown_env` エラーが返るので、
Emacs は登録し直して 1 度だけ再送する。

### 4.3 `git/run` — 本体

```jsonc
// →
{"method":"git/run","params":{
   "program":"C:/Program Files/Git/mingw64/libexec/git-core/git.exe",
   "cwd":"c:/Users/masao/.emacs.d/",      // ★ toplevel ではなく default-directory
   "args":["--no-pager","--literal-pathspecs","-c","core.preloadIndex=true", ...,
           "status","-z","--porcelain","--untracked-files=normal","--"],
   "env":"e1a2b3",
   "want_stderr": true                     // BUFFER が (t "FILE") のときだけ true
 }}
// ←
{"result":{"exit":0,
           "stdout":"<base64>",
           "stderr":"<base64 または null>",
           "elapsed_ms":29}}
```

**`cwd` は toplevel ではなく `default-directory` をそのまま渡す。**
magit はサブディレクトリから git を呼ぶことがあり、`--show-toplevel` の
結果自体がそれに依存するため。

`program` を Emacs から渡すのは、**デーモン側に git の場所を持たせないため**。
`magit-git-executable` の解決ロジック（cygpath 経由）を二重に持ちたくない。

### 4.4 `shutdown`

```jsonc
{"method":"shutdown"}
```

加えて **stdin が EOF になったら必ず終了する**。
Emacs が死んだときの後始末はこれで足りる（親 pid の監視は不要）。

---

## 5. Emacs 側の詳細

### 5.1 ルーティング判定（default deny）

```elisp
(defun my:gitd--routable-p (program infile buffer display)
  (and my:gitd-mode
       (my:gitd--live-p)
       (not my:gitd--suspended-p)                 ; サーキットブレーカ
       (null infile)                              ; 標準入力は素通し
       (null display)
       (not (file-remote-p default-directory))
       (equal program (magit-git-executable))     ; gitk / shell は除外
       (my:gitd--known-buffer-form-p buffer)))    ; 2.4 の 4 形態のみ
                                                  ; ※ 整数 (0) はここで弾かれる
```

**許可リストはここでは使わない**（5.3 参照）。ルーティングは形態だけで決め、
コマンドの種類は「失敗したときに再実行してよいか」の判断にだけ使う。

### 5.2 ルーティング範囲 = `magit-process-file` を通る全部（決定）

書き込み（`magit-call-process` 経由の stage / unstage / discard、`magit-wip--git`）も
デーモンに回す。ただし **以下は必ず素通し**（default deny）:

| 条件 | 理由 |
|---|---|
| `program` ≠ `(magit-git-executable)` | `magit-run-gitk*` は `gitk`、`magit-patch-id` は `shell-file-name` を渡してくる |
| **`BUFFER` が整数（`0` など）** | **非同期・出力破棄**の意味。`magit-run-gitk` が使う。回すと GUI が起動しっぱなしになる |
| `BUFFER` が 2.4 の 4 形態以外 | 再現方法が無い |
| `INFILE` が非 nil | 標準入力の受け渡しは 2a では扱わない |
| `DISPLAY` が非 nil | 同上 |
| `(file-remote-p default-directory)` | TRAMP は対象外 |

なお `magit-run-git-with-input`（stdin を使う経路）は `call-process-region` を使っており
`magit-process-file` を通らないので、そもそも介入しない。
`magit-start-process`（非同期・commit / rebase など）も同様に無関係。

### 5.3 二重実行の防止（★「全部回す」ことへの対応）

**危険は 1 つだけ**: フォールバックで再実行したとき、デーモン側で git が既に走っていた場合。
読み取りなら無害だが、`git add` を 2 回走らせるわけにはいかない。

→ **失敗の種類ごとに「git が起動したか」が確定するかで再実行を決める。**

| 失敗の種類 | git は起動したか | 動作 |
|---|---|---|
| デーモン未起動 / バイナリ無し / `initialize` 失敗 | **確実に未起動** | 素通しで実行（安全） |
| 送信前のエラー（`unknown_env`、シリアライズ失敗） | **確実に未起動** | 素通しで実行（安全） |
| **応答が遅い** | 実行中 | **タイムアウトしない**（下記） |
| デーモンが応答前に異常終了 | **不明** | 読み取り専用リスト内 → 素通しで再実行<br>それ以外 → **再実行せず失敗を返す** + 明示的に通知 |

**タイムアウトを置かないのが肝。** 現状の `process-file` にもタイムアウトは無く、
git がハングすれば Emacs も止まる。**挙動を変えないことが最も安全**で、
これにより新しく増える失敗モードは「デーモンの異常終了」だけに絞られる。
`C-g` で抜けられる点も現状と同じ（`jsonrpc-request` は
`accept-process-output` で待つので `C-g` が効く。要確認）。

計画書 6.5 の「タイムアウト 500 ms」は **この判断で撤回**する。
守りたいのは「デーモンが死んだのに Emacs が永久に待つこと」だけであり、
それは応答待ちではなく**プロセスの死**（`process-live-p` / sentinel）で検出する。

### 5.3.1 読み取り専用コマンドの許可リスト

上表のとおり、許可リストは **ルーティングの可否ではなく「再実行してよいか」の判定**に使う。
判定は `magit-git-global-arguments` を読み飛ばした先の**サブコマンド名**で行う。

```
再実行安全: rev-parse symbolic-ref describe config(--list/--get*) status diff show log
            for-each-ref show-ref ls-files ls-tree cat-file merge-base rev-list
            stash(list のみ) var name-rev check-ignore check-attr count-objects
            update-index(--refresh のみ ※)
それ以外はすべて「再実行しない」（default deny）
```

※ `update-index --refresh` は `.git/index` の stat キャッシュを書き換えるが冪等。
**2b ではキャッシュ対象から外すこと**（副作用があるため）。

このリストは **2b のキャッシュ許可リストとそのまま同じもの**になるので、
ここで作って実使用で揉んでおくことに意味がある。

### 5.4 出力の再現

```elisp
(pcase buffer
  ('nil                       nil)                       ; 捨てる
  (`(t nil)                   (my:gitd--insert stdout))
  (`(t ,(and f (pred stringp)))(my:gitd--insert stdout)
                              (my:gitd--write-raw stderr f))
  ((pred bufferp)             (with-current-buffer buffer (my:gitd--insert stdout))))

(defun my:gitd--insert (raw)
  (insert (decode-coding-string raw (car (magit--process-coding-system)))))
```

- `insert` は **point 位置**に入れる（`process-file` と同じ）。
- stderr のファイル書き出しは **デコードせず生バイト**で（`process-file` と同じ）。
- 戻り値は終了コード（整数）。

### 5.5 フォールバックとサーキットブレーカ

| 事象 | 動作 |
|---|---|
| デーモンが起動していない / バイナリが無い | 静かに素通し（毎回の判定は安い） |
| `initialize` のプロトコル不一致 | 機能を無効化し、`message` で 1 度だけ通知 |
| 送信前のエラー（`unknown_env` 等） | env を登録し直して 1 度だけ再送。なお駄目なら素通し |
| デーモンの異常終了 | 5.3 の表に従う（再実行の可否はコマンド種別で決める） |
| 失敗が 3 回連続 | そのセッションではデーモンを使わない + 1 度だけ通知 |

**タイムアウトは設けない**（5.3 参照）。
サーキットブレーカを「一定時間後に自動復帰」ではなく
「セッション中は諦める」にしたのは、**壊れかけのまま使い続けて
中途半端に二重実行が起きる状態を長引かせないため**。
復帰は `M-x my:gitd-restart` で明示的に行う。

---

## 6. Rust 側の詳細（2a）

```rust
loop {
    let req = read_lsp_frame(stdin)?;   // Content-Length: N \r\n\r\n {json}
    match req.method {
        "initialize"   => respond(protocol_version_info()),
        "env/register" => { envs.insert(id, env); respond(ok()) }
        "git/run"      => {
            let env = envs.get(&p.env).ok_or(unknown_env)?;
            let out = Command::new(&p.program)
                .current_dir(&p.cwd).args(&p.args)
                .env_clear().envs(env)          // ★ 渡された env をそのまま使う
                .output()?;
            respond(json!({ "exit": out.status.code(),
                            "stdout": b64(&out.stdout),
                            "stderr": if p.want_stderr { b64(&out.stderr) } else { null } }))
        }
        "shutdown"     => break,
    }
}
// stdin が EOF になったらループを抜けて終了する
```

依存は最小限に:
`serde` / `serde_json` / `base64`。2a では `notify` も `tokio` も要らない。

**`env_clear()` してから渡された env だけを使う**のが重要。
デーモンが Emacs から継承した環境が混ざると、
「デーモン経由のときだけ挙動が違う」という最悪のバグになる。

---

## 7. 検証方法 — シャドウモード

これが 2a の中心。**`my:gitd-verify-mode` を有効にすると、
ルーティング対象の呼び出しを毎回「デーモン経由」と「素の `process-file`」の両方で実行し、
終了コード・stdout・stderr をバイト単位で比較する。**

- 差異があれば `*gitd-verify*` バッファに記録（コマンド、差分の先頭、位置）
- **両方走らせるので、5.3.1 の読み取り専用リストに載っているコマンドだけを比較する。**
  書き込みコマンドはデーモン経由のみで実行し、比較しない（当然）
- 当然遅くなる。**普段は切っておき、導入直後の 1 週間だけ有効にする**使い方

書き込み経路については比較ができないので、代わりに
**`git status` の前後差分が従来と一致するか**を状態レベルで見る
（stage → status、unstage → status を一巡させるシナリオテスト）。

「静かに壊れる」がこの設計で唯一こわいので、**壊れていないことを実使用で
証明する手段を最初から用意する**。テストリポジトリを作って回すより、
実際の作業で踏むケースのほうが信用できる。

あわせて用意するもの:

- `my:gitd-stats` — ルーティング数 / フォールバック数 / 平均時間 / 累計短縮時間
- 日本語ファイル名・巨大 diff・バイナリファイル・空リポジトリ・
  detached HEAD・コンフリクト中 を含むテストリポジトリでの一巡

---

## 8. 決定事項（2026-09-03）

1. **配置** — `~/.emacs.d/gitd/` に Rust クレートを置く。ソースは git 管理下、
   `gitd/target/` は `.gitignore`。各マシンで `M-x my:gitd-build`（= `cargo build --release`）。
   バイナリが無ければ機能は無効 = 従来動作。`tree-sitter/` 文法と同じ扱い。
2. **ルーティング範囲** — `magit-process-file` を通る**全部**（書き込み含む）。
   ただし 5.2 の除外条件は厳守。二重実行は 5.3 の規則で防ぐ。
3. **命名** — `gitd/` / `magit-gitd.exe` / `user-lisp/my-gitd.el` / 接頭辞 `my:gitd-`。
4. **タイムアウトは設けない**（5.3）。計画書 6.5 の「500 ms」は撤回。
5. **base64 + `jsonrpc.el`**（2.1）。独自フレーミングは不要。

## 9. 実装結果（2026-09-03）

実装したもの:

| | |
|---|---|
| `gitd/Cargo.toml`, `gitd/src/main.rs` | Rust 常駐プロセス（依存は serde / serde_json のみ。base64 は自前 20 行） |
| `user-lisp/my-gitd.el` | Emacs 側。advice / 接続管理 / 正規化 / フォールバック / シャドウモード / 統計 |
| `init.el` | `(require 'my-gitd)` を `my-vc` の次に追加 |
| `.gitignore` | `/gitd/target/` |

### 9.1 性能（`~/.emacs.d`、`magit-refresh-buffer`）

| | 時間 | Emacs からの git 起動 |
|---|---|---|
| デーモン無効（素の magit） | 1455 / 1469 ms | 29 回 |
| **デーモン有効** | **748 / 557 / 639 ms** | **0 回** |
| シャドウモード（両方実行） | 2061 ms | 0 回 |

**2.4 倍。** 予測（659 ms）とほぼ一致した。

### 9.2 正しさ

- **シャドウモードで不一致 0 件。** 読み取り専用コマンド全部について、
  終了コードと stdout がバイト単位で一致。
- **`magit-status` バッファの内容が完全一致**（834 文字、デーモン有/無で同一）。
- スパイクでの個別比較も 11/11 一致。NUL 区切り（`config --list -z`）、
  `%x0c` 区切りの log、非 ASCII 引数、終了コード 128、`cat-file` の
  バイナリ寄り出力を含む。
- 除外条件が全部効くことを確認（整数 BUFFER / INFILE / DISPLAY /
  未知の形態 / git 以外のプログラム）。
- 再実行の安全判定も期待どおり（`config --list` は安全、`config user.name foo`
  は不可、`branch --list` は安全、`branch -D` は不可、`add` / `reset` は不可）。

### 9.3 確認できたこと

- **`C-g` は効く。** `jsonrpc-request` の待ちは
  `(let ((inhibit-quit nil)) (while t (accept-process-output nil 30)))` なので
  `quit` で抜ける。`condition-case` の `error` 節は `quit` を拾わないため、
  そのまま呼び出し元に抜ける（素の `process-file` と同じ）。
- **`:timeout nil` でタイマーが完全に無効になる**（`jsonrpc--async-request-1`
  の `(when timeout ...)`）。設計どおりタイムアウト無しにできる。
- **`jsonrpc.el` のオーバーヘッドは無視できる。** `initialize` の往復が
  **0.13 ms**（生パイプの実測 0.36 ms より速い。あちらは git の仕事を含むため）。

### 9.4 実装中に見つかった問題（いずれも解決済み）

**① `process-environment` に JSON に載らない項目がある**

`PSModulePath` が OneDrive の「ドキュメント」を ANSI の生バイトのまま含んでおり、
`json-serialize` が `wrong-type-argument json-value-p` で落ちた。
**PowerShell から Emacs を起動したときだけ再現する**（bash 経由では出ない）。

**② 復号に使う coding system を間違えると直らない**

`locale-coding-system` は**コンソールの**コードページで、PowerShell 7 では
`cp65001`（UTF-8）。環境変数ブロックは **ANSI コードページ**（`w32-ansi-code-page`
= 932）なので別物。UTF-8 として復号すると生バイトが eight-bit 文字のまま残り、
やはり JSON に載らない。

**③ 引数も ANSI に encode されている**

`magit-process-git-arguments` の docstring と実装:

```elisp
;; On w32, the process arguments *must* be encoded in the
;; current code-page (see #3250).
(let ((coding (intern (format "cp%d" w32-ansi-code-page))))
  (mapcar (##encode-coding-string % coding) args))
```

Emacs の `call-process` が ANSI API を使うため、magit は意図的にこうしている。
デーモン境界では復号し直す必要がある（Rust はワイド API で起動するので、
これで元の文字が復元される。cp932 に無い文字なら**むしろ改善**になる）。

→ ①②③ をまとめて `my:gitd--to-text` が処理する。**`args` / `cwd` /
`program` / `env` の全部に適用すること。**

**④ `magit-run-gitk` は BUFFER に整数 `0` を渡す**

非同期・出力破棄の意味。同期実行すると gitk のウィンドウを閉じるまで
Emacs が固まる。`my:gitd--known-buffer-form-p` で弾く（プログラム名の
判定でも弾かれるので二重の防御）。

### 9.5 残る確認事項

- 実使用での一巡: 空リポジトリ / detached HEAD / コンフリクト中 /
  サブモジュール / 日本語ファイル名 / 巨大 diff / バイナリファイル
- 書き込み経路（stage / unstage / discard）を実際に GUI で操作しての確認。
  batch では `magit-call-process` 経路を踏めていない
- **`C-g` で中断したとき、デーモン側の git は走り切る。**
  素の `call-process` は子プロセスを kill するので、ここだけ挙動が違う。
  書き込みの途中で `C-g` したときに「中断したのに実行されている」ことになる。
  対処するなら `$/cancel` 通知を足してデーモン側で子を kill する。
  （半端に kill された index より安全とも言えるので、優先度は低い）
- 1〜3 万ファイルのリポジトリでの実測

### 9.6 使い方

```
M-x my:gitd-build     ; cargo build --release（各マシンで 1 度）
M-x my:gitd-stats     ; 経由回数 / フォールバック数 / 累計短縮時間
M-x my:gitd-restart   ; サーキットブレーカが落ちたときの復帰
(setq my:gitd-verify t)  ; シャドウモード。導入直後の検証期間だけ
```
