# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

Windows（主）、macOS、Linux 向けの個人 Emacs 設定リポジトリ。Emacs 31.1 を対象とする。

以前は Org-mode のリテラルプログラミング形式（`my-config/init.org` を
`org-babel-load-file` で展開）だったが、Org の恩恵が薄い割にコストが大きかったため
素の Emacs Lisp に戻し、Emacs 31.1 で新設された `user-lisp/` に機能分割してある。

## エントリポイントとアーキテクチャ

- `early-init.el` — `init.el` より前に読まれる。`init.el` では手遅れになる設定だけを置く
  - `package-enable-at-startup` を `nil`（パッケージ管理は straight に一本化）
  - 起動中の GC 抑制（`emacs-startup-hook` で通常値に戻す）
  - `user-lisp-auto-scrape` を `nil`（後述）
  - ツールバー等の非表示を `default-frame-alist` に前倒し（ちらつき回避）
- `init.el` — ブートストラップと読み込み順の宣言のみ
  1. straight.el のブートストラップ
  2. 組み込みを使うパッケージの宣言（`org` / `transient` を `:type built-in`）
  3. use-package の初期化（Emacs 同梱。キーワード順と挙動を調整する）
  4. `site-lisp/` を `load-path` に追加
  5. `(prepare-user-lisp ...)`
  6. `custom.el` の読み込み
  7. `user-lisp/` 各モジュールの `require`（順序は分割前の記述順のまま）
- `user-lisp/` — 設定本体。23 モジュールに分割（下記）
- `custom.el` — `customize` が自動生成するファイル
- `site-lisp/` — パッケージマネージャで入手できないローカルベンダの Emacs Lisp
- `gitd/` — magit の git 実行を肩代わりする常駐プロセス（Rust）。
  ソースは管理下、`gitd/target/` は git 管理外で各マシンでビルドする（後述）
- `ptyd/` — 疑似コンソール（ConPTY）を持って対話 TUI を動かすプロセス（Go）。
  同じく、ソースは管理下で `ptyd/ptyd.exe` は git 管理外（`M-x my:pty-build`）
- `docs/` — 設計メモ・計画・実測の記録（git 管理下）。
  過去に `tmp/` に置いていたものは 2026-09-04 にここへ移した。
  同日にテーマ別のサブディレクトリへ分けた
  - `docs/refactoring/` — `.emacs.d` 以下のリファクタリング（Org からの移行、ベースライン計測）
  - `docs/magit/` — magit の高速化と自動更新（`gitd/`、`my-magit-watch`）
  - `docs/hydra/` — hydra の棚卸しメモ
  - `docs/claude/` — Claude Code を Emacs から使う（`my-claude.el`）
- `docs/_archived/` — 役目を終えた移行スクリプトと旧設定（履歴として保存）
- `tmp/` — 作業用の捨て場。`.gitkeep` 以外は git 管理外
- `docs/_archived/archive-init.org` — Org 方式だった頃の設定（履歴として保存）
- `docs/_archived/extract.el`, `docs/_archived/verify.el`,
  `docs/_archived/split.py`, `docs/_archived/verify-split.el` —
  Org からの抽出・分割に使った検証スクリプト（等価性の証跡）
- `docs/_archived/snapshot.el` — 設定を読み込んだ Emacs の観測可能な状態
  （defcustom 全変数、全 `*-hook` / `*-functions`、全キーバインド、face の
  `theme-face` / `defface` / 実効属性、ロード済み feature）を決定的な順序で
  ダンプする。leaf → use-package 移行の等価性検証に使った。同一設定なら
  2 回採取して差分 0 行になるので、書き換えの前後で diff すれば足りる

  ```sh
  emacs --batch -l early-init.el -l init.el -l docs/_archived/snapshot.el \
        --eval '(my:snapshot-dump "before.txt")'
  ```

## `user-lisp/` の扱い（重要）

Emacs 31.1 の `user-lisp/` は、既定では `package-activate-all` の直後・
`init.el` の読み込み**前**に `prepare-user-lisp` が走り、配下を再帰的に
バイトコンパイルして autoload を生成し `load-path` に追加する。

しかしその時点では straight.el のブートストラップが済んでおらず、
`use-package` も未初期化のため、モジュールが壊れた `.elc` にコンパイルされる。

そのため以下のようにしている：

- `early-init.el` で `user-lisp-auto-scrape` を `nil` にして自動実行を止める
- `init.el` で straight と use-package を用意したあと `(prepare-user-lisp ...)` を明示的に呼ぶ
- **バイトコンパイルはしない**（`prepare-user-lisp` の第 1 引数 JUST-ACTIVATE を `t`）。
  コンパイルすると、パッケージ由来のマクロを `:init` / `:config` で使っている箇所が壊れる。
  コンパイル時点では当該パッケージが未ロードでマクロが未定義のため、
  関数呼び出しとしてコンパイルされてしまう。
  実例: `doom-modeline-def-segment` が関数扱いになり、実行時に引数の
  `my:buffer-encoding` が変数として評価されて void エラーになった。
  `defhydra` や `define-clojure-indent` も同じ問題を持つ。
  起動時間はコンパイルの有無で差が無かった（約 1250ms で同じ）ため確実性を取っている。

モジュールを追加した場合は `init.el` の `require` 列に加える。

## モジュール構成（`user-lisp/`）

| モジュール | 内容 |
|---|---|
| `my-core` | 汎用ヘルパ（`my:pandoc-data-file`、`my:open-file-externally` など）、`s` |
| `my-japanese` | 文字コード、cp932/UTF-8 変換テーブル、Windows IME（tr-ime）、migemo |
| `my-appearance` | フォント、フレーム、modus-vivendi テーマ、doom-modeline、all-the-icons |
| `my-completion` | vertico、consult、marginalia、orderless、corfu、cape |
| `my-keybind` | グローバルキーバインド（`C-h` → `delete-backward-char`、`C-z` → `scroll-down`） |
| `my-editor` | hydra、symbol-overlay、smartparens、whitespace、yasnippet、recentf ほか |
| `my-dired` | dired、hydra-dired、dired-sidebar（`F8`。差分表示は my-vc の diff-hl）、dired-x の上書き対策 |
| `my-text` | org-mode、ox-pandoc、markdown、rst、adoc |
| `my-lang-lisp` | Emacs Lisp、Clojure（cider）、Common Lisp（slime） |
| `my-lang-python` | Python（python-ts-mode、pyvenv、py-isort、blacken） |
| `my-lang-web` | PHP、JavaScript / TypeScript（js-ts-mode / typescript-ts-mode、web-mode、scss） |
| `my-lang-native` | Rust、C++、C#、Go |
| `my-lang-misc` | SQL、bat、Swift、Lua、VisualBasic |
| `my-lsp` | eglot（組み込み、プレフィックス: `C-c l`）、flymake（`C-c !`） |
| `my-fileformat` | yaml、diff、log4j、Dockerfile、vimrc、mayu |
| `my-project` | projectile（プレフィックス: `C-c p`） |
| `my-vc` | magit、diff-hl（`C-c g` の hydra）、Windows の SVN 対応 |
| `my-gitd` | magit の同期 git 実行を常駐プロセス（`gitd/`）に肩代わりさせる。Windows のみ |
| `my-magit-watch` | ワークツリーを監視して magit バッファを自動更新。Windows のみ |
| `my-shell` | exec-path-from-shell、Windows 用 shell 設定 |
| `my-utils` | calendar、open-junk-file、grep/ripgrep、blog 用ヘルパ |
| `my-claude` | Claude Code を stream-json で使う（プレフィクス: `C-c a`） |
| `my-pty` | ConPTY 経由で対話 TUI を動かす（`ptyd/`）。Windows のみ |
| `my-platform` | Windows / macOS 固有設定 |

## LSP サーバ

eglot が使う言語サーバは自分で入れる。2026-08 時点の導入状況:

| 言語 | サーバ | 入れ方 |
|---|---|---|
| TypeScript / JS | typescript-language-server 5.3.0 + **typescript 5.9.3** | `npm i -g typescript@5 typescript-language-server` |
| PHP | intelephense 1.18.5 | `npm i -g intelephense` |
| bash | bash-language-server 5.6.0 | `npm i -g bash-language-server` |
| Rust | rust-analyzer 1.97.1 | `rustup component add rust-analyzer` |
| Python | basedpyright 1.39.10 | `uv tool install basedpyright` |
| Go | gopls 0.23.0 | `go install golang.org/x/tools/gopls@latest` |

### East Asian Ambiguous 幅 (site-lisp/eaw.el)

`site-lisp/eaw.el` は残す。Emacs 31 の組み込み処理では足りないため。

Emacs 31 は `ambiguous-width-chars` を持ち、`cjk-ambiguous-chars-are-wide`
が t なら `use-cjk-char-width-table` がそれを幅 2 にする。日本語環境に
すると自動で適用されるので、組み込みだけでもある程度は効く。

HackGen で実測した結果（GUI）:

| | 文字数 |
|---|---|
| eaw が挙げる ambiguous 文字 | 3666 |
| 組み込みだけで幅 2 になるもの | 2170 |
| **eaw が追加で幅 2 にするもの** | **1496** |

その 1496 文字を実際に描画して測ると:

| 実測幅 | 文字数 | |
|---|---|---|
| 16px（全角） | 335 | eaw が正しい |
| 8px（半角） | 63 | 組み込みが正しい |
| それ以外 | 1098 | 絵文字・麻雀牌など。プロポーショナルなフォールバックで描かれ、`char-width` をどちらにしても桁は揃わない |

桁揃えが成立する 398 文字のうち **84% で eaw のほうが実描画と一致する**。
`○△□★※①→≒` のような日常的な記号は組み込みでも幅 2 になるので、
差が出るのは記号類が中心。

**計測は必ず GUI で行うこと。** Windows の batch では `initial-window-system`
が nil のため `use-cjk-char-width-table` が ambiguous を幅 1 に倒す分岐に入り、
組み込みのカバー範囲を過小評価する（2170 ではなく 1424 に見える）。

### 日本語フォントの全角/半角ピッチ

HackGen は「全角＝半角×2」で設計されているが、**サイズによっては 1px ずれる**。
Windows で実測した結果:

| `:height` | 半角 | 全角 | |
|---|---|---|---|
| 110 / 113 / 116 | 8 | 16 | 一致 |
| **120 / 124** | 8 | **17** | **ずれる** |
| 128 / 130 | 9 | 18 | 一致 |
| 140 | 10 | 20 | 一致 |

以前は 120 を使っていて桁が揃っていなかった。11.6（= 116）にしてある。

**`face-font-rescale-alist` では直せない。** ASCII と日本語が同じフォント
なので、スケールすると両方が同じ比率で縮むだけ。サイズを変えるしかない。
確認は `(string-pixel-width "あ")` と `(string-pixel-width "aa")` の比較で。

### TypeScript は 5.x に固定すること

**`npm i -g typescript` で入る 7.x（Go 実装のネイティブ版）は使えない。**
7.x には `lib/tsserver.js` が無く、typescript-language-server が
`Could not find a valid TypeScript installation` で初期化に失敗する。
`npm i -g typescript@5` を使う。

basedpyright の実行ファイルは `~/scoop/persist/uv/tools/shims`（PATH 済み）。
eglot は pylsp → pyls → basedpyright-langserver の順に探すので、
pylsp を入れるとそちらが優先される点に注意。

### npm グローバルは nvm のバージョンに紐づく

prefix は `~/scoop/apps/nvm/current/nodejs/nodejs`。**nvm で Node を切り替えると
グローバルパッケージも切り替わる**ので、切り替えたら入れ直しが要る。
プロジェクトローカル（`npm i -D`）に寄せると安定する。`add-node-modules-path` が
`node_modules/.bin` を `exec-path` に足すので、ローカル版が優先される。

### 上流の非互換で eglot が黙って壊れることがある

eglot は `eglot--maybe-activate-editing-mode` の中で

```elisp
(eglot--managed-mode)                  ; ここで eglot--managed-mode-hook が走る
(eglot--signal-textDocument/didOpen)   ; ← ここが飛ぶ
(eglot-inlay-hints-mode 1) ...
```

の順に呼ぶ。**フックの中でエラーが出ると `textDocument/didOpen` が送られない**。
接続は成立してモードラインにも出るのに、サーバはバッファの存在を知らないため
診断も補完も一切出ない、という分かりにくい壊れ方をする。

実例: doom-modeline 4.3.0 の eglot セグメントが Emacs 31.1 で無くなった
`jsonrpc--request-continuations` / `eglot--spinner` / `eglot--major-mode` を
呼んでおり、`my-appearance.el` で差し替えている（upstream 未修正）。
同種の症状が出たら、まず `eglot--managed-mode-hook` の中身を疑うこと。

### 【重要】Windows で大文字のドライブレターを返すサーバは診断が出ない

**gopls で実際に踏んだ。** 接続もジャンプも補完も整形も効くのに、
flymake の診断だけが 1 件も出ない、という壊れ方をする。

gopls は `textDocument/publishDiagnostics` の uri を

```
file:///C:/Users/masao/...          ← 大文字 C
```

で返す（eglot が送る `workspaceFolders` は `file:///c%3A/...` と小文字）。
受け取り側の `eglot--flymake-handle-push` は `eglot-uri-to-path` の結果を
`eglot--find-buffer-visiting` に渡すが、そこは `buffer-file-name` との
**文字列 `equal`** で突き合わせる（`file-truename` が遅いので避けている。
bug#70036）。Emacs の `buffer-file-name` はドライブレターが小文字なので
一致せず、診断は `flymake-list-only-diagnostics` に回されて
**警告も出ないまま消える**。

`eglot-uri-to-path` 自身が持つ正規化（`trueroot` で始まるならプロジェクトの
root に置換する）も `string-prefix-p` が大文字小文字を区別するので効かない。

実測（gopls v0.23.0 / Emacs 31.1）:

| | |
|---|---|
| `(eglot-uri-to-path "file:///C:/...")` | `"C:/..."` → `eglot--find-buffer-visiting` は nil |
| advice で `"c:/..."` に直す | **0.5 秒で診断が出る** |

`my-lsp.el` で `eglot-uri-to-path` に `:filter-return` の advice
（`my:eglot-normalize-drive-letter`）を張って、Windows のときだけ
ドライブレターを小文字に揃えている。既に小文字なら no-op なので
他のサーバには影響しない。

**診断だけ出ないときは、まずサーバが返す uri の綴りを疑うこと。**
`eglot-events-buffer-config` を一時的に有効にして
`publishDiagnostics` の uri を見る（既定では `:size 0` で記録されない）。

### php-mode は 1.28 (2026-08) で cc-mode 依存が外れた

`c-set-style` / `c-basic-offset` は使えない（`Buffer ... is not a CC Mode buffer`）。
インデントは `php-mode-coding-style` で指定する。
cc-mode 版が要るときは `php-cc-mode` が別に残っている。

## tree-sitter

メジャーモードは tree-sitter 版（`*-ts-mode`）を使う方針。ただし文法は
共有ライブラリで別途ビルドが必要（**C コンパイラと git が要る**）。
文法が無い環境で `*-ts-mode` に切り替えると何も動かなくなるため、
**従来のモードを残したうえで、文法が実際に使えるときだけ差し替える**形にしてある。

- `my:treesit-remap MODE TS-MODE LANGUAGE`（`my-core.el`）が
  `major-mode-remap-alist` に登録する。文法が無ければ何もしない
- `my:install-treesit-grammars`（`M-x`）で `treesit-language-source-alist` の
  文法をまとめてビルドする。反映には再起動が必要
- **フォントロックやインデントの設定はモードごとに別物**。`csharp-mode` は
  cc-mode 派生（`c-set-offset`）、`csharp-ts-mode` は tree-sitter 派生
  （`csharp-ts-mode-indent-offset`）なので、セットアップ関数を分けてある
- `*-ts-mode` は従来モードのフックを継承しない。`:hook` は
  `((foo-mode-hook foo-ts-mode-hook) . func)` の形で両方に張ること

- **`my:treesit-remap` は必ずトップレベルで呼ぶこと**。`:config` は
  `(eval-after-load '<パッケージ名>)` に包まれるので、そこで差し替えても
  「その回に開いたバッファ」には間に合わない。さらに差し替えが効くと
  従来のモードはもうロードされないため、`:config` は二度と実行されない
- `.tsx` の `auto-mode-alist` 登録は **web-mode のブロックより後**に置くこと。
  `:mode` が先頭に積むので、前に置くと web-mode に負ける

導入済みの文法（`tree-sitter/`、git 管理外）:
bash / c-sharp / css / dockerfile / go / gomod / gowork / html /
javascript / jsdoc / json / python / rust / toml / tsx / typescript / yaml
の 17 個。
`jsdoc` は `js-ts-mode` がコメント解析に `treesit-ensure-installed` するので必要。

コンパイラは scoop の `gcc`（mingw-w64 15.2.0、`~/scoop/apps/gcc/current/bin`）。
Emacs は `cc` → `gcc` → `c99` の順に探すので `gcc` があれば足りる。

## Go

Emacs 31.1 は Go に必要なものをほぼ同梱している。**外部から入れるのは gopls だけ**。
設定は `my-lang-native.el`、gopls への設定は `my-lsp.el` の
`eglot-workspace-configuration` にある。

| 役割 | 使うもの | 備考 |
|---|---|---|
| メジャーモード | 組み込み `go-ts-mode` / `go-mod-ts-mode` / `go-work-ts-mode` | 外部 `go-mode` は入れない（後述） |
| LSP | gopls | eglot に既定エントリがある（設定不要で繋がる） |
| 整形 | gopls 内蔵の gofumpt（`:gofumpt t`） | gofumpt のバイナリは要らない |
| import 整理 | gopls の `source.organizeImports` | goimports のバイナリは要らない |
| 静的解析 | gopls 内蔵の staticcheck（`:staticcheck t`） | flymake（`C-c !`）に出る |
| 追加 lint | golangci-lint 2.13.2（scoop） | `C-c C-l` で `compile` |
| テスト実行 | `go-ts-mode` 組み込みの `C-c C-t t` / `f` / `p` | gotest.el 等は要らない |
| docstring 雛形 | `C-c C-d`（`go-ts-mode-docstring`） | 組み込み |

`go install` したものは `~/go/bin`（PATH 済み）に入る。
**nvm の npm グローバルと違い、Go のバージョンを変えても消えない**が、
`go install` はビルドし直しなので Go を上げたら入れ直しておくのが無難。

### 【重要】外部の `go-mode` を入れてはいけない

Rust / C# は「従来モードを残して `my:treesit-remap` で差し替える」形にしてあるが、
**Go でそれをやると tree-sitter 版に一生切り替わらない**。

`go-ts-mode.el` は autoload で自分の登録を済ませている。

```elisp
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode-maybe))
(add-to-list 'treesit-major-mode-remap-alist '(go-mode . go-ts-mode))
```

- `go-ts-mode-maybe` は文法があれば `go-ts-mode`、無ければ **`fundamental-mode`**
- `treesit-major-mode-remap-alist` が `major-mode-remap-alist` に反映されるのは
  **`treesit-enabled-modes` が非 nil のときだけ**（既定は `nil`）

つまり `go-mode` を `:mode "\\.go\\'"` で足すと、`auto-mode-alist` の先頭に
積まれて必ず `go-mode` が勝ち、remap も起きない。`.tsx` を web-mode より後に
置かねばならないのと同じ罠。

文法が無い環境への保険が要るなら、`go-mode` ではなく
**`treesit-enabled-modes` に `go-ts-mode` を入れる**。こうすると文法が無いときに
`treesit-ensure-installed` が導入を提案する（`treesit-auto-install-grammar` の既定は `ask`）。
このリポジトリでは `my:install-treesit-grammars` で入れる運用にしている。

なお `go-ts-mode.el` は `treesit-language-source-alist` に go / gomod / gowork を
`add-to-list` するが、`my-core.el` はその変数を `setq` で丸ごと上書きするので、
**`my-core.el` 側にも同じ内容を書いておかないと `my:install-treesit-grammars` から
見えない**。commit ハッシュまで一致させること。1 文字でも違うと `add-to-list` の
`equal` 判定をすり抜けて二重登録になり、2 回ビルドされる。

### 保存時は「import 整理 → 整形」の順で呼ぶ

`my:go-before-save`（`before-save-hook`）が 2 つを順に呼ぶ。
逆にすると、あとから足された import 行が整形されないまま残る。

**`eglot-code-actions` を対話的に呼んではいけない。**
INTERACTIVE 非 nil で呼ぶと `eglot--read-execute-code-action` に入り、該当が
0 件のとき `eglot--error` が飛ぶ。`before-save-hook` の中で飛ぶので
**import を整理する必要が無いファイルは保存できなくなる**。
`my:go-organize-imports` は非対話（INTERACTIVE nil）で候補リストを受け取り、
あるときだけ `eglot-execute` する形にしてある。

### インデントはタブ

gofmt がタブなので `go-ts-mode` は `indent-tabs-mode` を `t` にする。
`go-ts-indent-offset` は「タブ何個ぶんか」ではなく桁数なので、`tab-width` と
揃えないと継続行がずれる。既定の 8 は広いので両方 4 にしてある
（ファイルの中身はタブのままなので他のツールとは衝突しない）。

`whitespace-global-modes` に go 系は入っていないので、タブが強調されることはない。

### golangci-lint は flymake に載せない

モジュール全体を型検査するため 1 回が重く、`flymake-no-changes-timeout`（1.0 秒）で
回す用途には向かない。日常の指摘は gopls 内蔵の staticcheck で足りるので、
golangci-lint は `C-c C-l`（`my:go-golangci-lint`）で `go.mod` のあるディレクトリから
`golangci-lint run ./...` を `compile` する形にした。出力は
`main.go:10:5: S1002: ...` の形式なので、`compilation-error-regexp-alist` の
既定（gnu）でそのまま辿れる。

### 検証は GUI で、かつコマンドループを回すこと

`eglot-ensure` は **`post-command-hook` で接続する**。プローブ用の elisp を
`-l` で読ませて一気に実行すると、`find-file` しても永久に繋がらない
（実際に 90 秒待って TIMEOUT した）。`(run-hooks 'post-command-hook)` を
手で 1 回呼ぶ。flymake の診断も同様に `(flymake-start)` を明示的に呼ぶ。

## パッケージ管理

**straight.el に一本化**している（`package.el` は `early-init.el` で無効化済み）。

- 新しいパッケージは該当モジュール内で `(use-package package-name :straight t ...)`
- 組み込みライブラリには `:straight` / `:ensure` を付けない
- Emacs 同梱のものを使いたい場合は `init.el` で
  `(straight-use-package '(NAME :type built-in))` を宣言する（`org`、`transient` が該当）。
  これをしないと依存解決で straight が古い版をビルドして `load-path` に載せてしまう

### 更新状況の棚卸し

`straight/repos/*` を一括で fetch して、手元と upstream の差を見る:

```sh
cd ~/.emacs.d/straight/repos
for d in */; do r="${d%/}"
  case " melpa gnu-elpa-mirror nongnu-elpa emacsmirror-mirror el-get straight.el " in
    *" $r "*) continue;; esac
  ( cd "$r" && git fetch -q origin && git remote set-head origin -a >/dev/null
    up=$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD)
    printf '%-24s behind=%s
' "$r" "$(git rev-list --count HEAD..$up)" )
done
```

更新したあとは **`straight/build` をまるごと消してから起動する**。
straight の変更検知は当てにならない（corfu / doom-modeline で取りこぼした実績あり）。
再ビルドは GUI 起動で数分かかる。

2026-08 の棚卸しでは 93 個中 50 個が遅れていた。**全 50 個を更新済み**
（段階 1〜4 に分けて、各段階で GUI 起動して検証した）。

更新の過程で、設定側の非互換が 2 件と、更新とは無関係の既存バグが 3 件見つかった。
棚卸しは「古いまま放置していると壊れているのに気づけない」ことの確認になった。

### org のクローンは使っていない

`init.el` で `(org :type built-in)` と宣言しているので Emacs 同梱の org を使う。
`straight/repos/org` と `straight/build/org` があっても `load-path` には載らない。
recipe cache には残るため `straight-prune-build` では消えないので、手で消す。
（2026-08 に削除。合わせて 120 MB あった）

### 不要になったパッケージの掃除

```elisp
(straight-prune-build)             ; 今のセッションで使われていない build/ を消す
(straight-remove-unused-repos t)   ; どのビルドからも参照されない repos/ を消す
```

**GUI で起動してから実行すること。** batch では `:if window-system` の
パッケージ（doom-modeline、org-bullets など）が登録されず、
使用中のものまで削除対象になる。
OS 判定で外れるもの（`exec-path-from-shell` は macOS / Linux 専用）も同様に
消えるが、`straight/` は git 管理外なので他マシンには影響しない。

### 更新の手順

straight は自動更新しない。追従が必要なときは：

```elisp
(straight-pull-recipe-repositories)  ; レシピ定義（melpa 等）を更新
(straight-pull-package "NAME")       ; 個別パッケージを更新
```

レシピリポジトリを更新しても、**すでに clone 済みのパッケージ本体は古いまま**
であることに注意。`straight/repos/NAME` の HEAD は clone 時点で止まる。
2026-08 時点で vertico / consult / marginalia / orderless などは
まだ 2021 年のままになっている。

パッケージ本体を更新したあとは **`straight/build/NAME` を消してから起動**する。
straight の変更検知はこれを取りこぼすことがあり、`straight-rebuild-package` でも
再ビルドされない場合がある (corfu の extensions がコピーされない事例があった)。

過去に **レシピリポジトリと straight.el 本体が 2021 年で凍結**しており、
それが「新しいバージョンに追従できていない」原因になっていた。
upstream がデフォルトブランチを `master` → `main` に変えている場合は
`straight/repos/NAME` で手動チェックアウトが必要になることがある（magit で発生）。

## custom.el の扱い

`init.el` の読み込み順は **custom.el → `user-lisp/` の各モジュール**。
つまり同じ変数を両方で設定すると **`user-lisp/` 側が勝つ**。
`custom.el` に書いても効かないので、設定は `user-lisp/` に置くこと。

2026-08 に重複を整理して、`custom.el` に残すのは次の 4 変数だけにした:

- `safe-local-variable-values` … ディレクトリローカル変数の許可リスト（Emacs が書く）
- `warning-suppress-log-types` / `warning-suppress-types` … straight の警告抑制
- `yas-new-snippet-default` … スニペットのテンプレート

face は `rst-level-1`〜`6` の 6 面だけ残した（modus も同じ face を定義するが、
`rst.el` はテーマより後にロードされるため `user` テーマ側が勝ち、実際に効いている）。

**face がテーマに勝つかどうかはロード順で決まる**。テーマより先に定義済みの
face（`font-lock-*` など）はテーマが勝ち、`custom.el` に書いても効かない。
テーマより後にロードされるパッケージの face は `custom.el` 側が勝つ。
確実に当てたいときは `load-theme` のあとに設定する。

`customize` を使うと `custom.el` に書き戻されるので、モジュール側と
重複していないか時々確認する。重複の検出は、`custom.el` の
`custom-set-variables` から変数名を集め、`user-lisp/` の `use-package` を
`macroexpand-1` して出てくる `customize-set-variable` と突き合わせればよい。

なお `use-package` の `:custom` は既定 (`use-package-use-theme` = `t`) では
`custom-theme-set-variables`（`use-package` という擬似テーマ）を使う。これだと
`custom.el` が書く `user` テーマのほうが優先順位が高くなり、上の
「`user-lisp/` 側が勝つ」が逆転してしまう。`init.el` で
`use-package-use-theme` を `nil` にして `customize-set-variable` に戻してある。

## use-package を使うときの注意

設定の記述は **Emacs 同梱の use-package**（`lisp/use-package/`）で行う。
2026-08 に leaf から移行した。leaf は直近 12 ヶ月で 3 コミットまで開発が細り、
日本語圏以外ではほとんど使われていないのに対し、use-package は Emacs 本体に
入っているため腐りようがない、というのが理由。パッケージマネージャは straight の
まま（elpaca への移行は見送り）。設定本体が use-package なら、将来 elpaca や
package.el に移るときも `:straight` の 1 行を差し替えるだけで済む。

### `init.el` で調整している 3 点

素の use-package のままでは leaf と挙動が変わってしまうため、`init.el` で以下を
設定している。**外すと静かに壊れる**ので注意。

| 設定 | 外すとどうなるか |
|---|---|
| `use-package-hook-name-suffix` = `nil` | `:hook (foo-mode-hook . f)` が `foo-mode-hook-hook` に登録される |
| `use-package-use-theme` = `nil` | `:custom` が擬似テーマ経由になり、`custom.el` に負ける（上記） |
| `:straight` を `:unless` の直後へ移動 | `:straight` は `use-package-keywords` の先頭に push されるため `:if` より先に処理され、**`:if` が偽でも `straight-use-package` が走る**。Windows で `exec-path-from-shell`、Linux で `w32-ime` / `tr-ime` まで clone / build しにいく |

### 遅延キーワードが無いブロックには `:defer t` を足す

leaf は `:require t` が無い限り `(require)` を出さないが、**use-package は遅延
キーワード（`:commands` `:bind` `:hook` `:mode` `:after` など）が 1 つも無いと
`(require)` を出す**。インストールするだけのブロックには `:defer t` を付ける。

`:defer t` を付けると `:config` は `(with-eval-after-load '<name>)` に包まれる。
そのパッケージを誰もロードしないなら `:config` は永久に走らないので、
「ロードせずに実行したい設定」は `:init` に置くこと（leaf の `:config` が
インライン実行だったものはここに移す）。

### 名前は実在する feature にする。疑似パッケージは `emacs`

`:hook` / `:bind` / `:mode` などがあると `:config` は
`(eval-after-load '<パッケージ名>)` に包まれる。**名前が実在する feature で
ないと `:config` も `:bind` も永久に適用されない**（leaf でも同じ罠だった）。

- 実在する feature 名を使う（例: `sql-mode` ではなく `sql`）
- OS 別のまとまりなど**疑似パッケージには `emacs` を使う**。`(require 'emacs)`
  は no-op、`(with-eval-after-load 'emacs ...)` は即実行されるので安全

### `:custom-face` は使わない

**use-package の `:custom-face` はテーマに負ける。** 実測:

| 方法 | modus-vivendi が同じ face を定義しているとき |
|---|---|
| `custom-set-faces`（leaf の `:custom-face` 相当） | `theme-face` に `user` が積まれ **自分の指定が勝つ** |
| use-package の `:custom-face`（`face-spec-set` + `face-defface-spec`） | **テーマが勝ち、指定が消える** |
| `face-spec-set` に spec-type `user` を明示 | 同上、**消える** |

そのため `:init` から `custom-set-faces` を直接呼ぶ形にしてある
（`diff-hl` / `highlight-indent-guides` / `doom-modeline` の 3 箇所）。

### `require` できないものには `:no-require t`

use-package は **`require` に失敗すると `:config` ごと実行しない**。
`modus-themes` は `etc/themes/` にあり `load-path` に載っていないため
`(require 'modus-themes)` は失敗する。`:no-require t` が無いと `load-theme` が
呼ばれず、テーマが一切適用されない。

### `:custom` にマイナーモードの変数を書く場合

`customize-set-variable` は `(get VAR 'custom-set)` が未設定のとき
`set-default` にフォールバックするため、**パッケージが未ロードだと変数に `t` が
入るだけでモード関数が呼ばれない**。
実例: `(corfu :custom (global-corfu-mode t))` では corfu が読まれず補完が出なかった。
`:demand t` でロードした上で `:config` から明示的に呼ぶこと。

ただし autoloads に `custom-autoload` が入っている変数（`cua-mode`、
`global-whitespace-mode`、`yas-global-mode` など）は `customize-set-variable` が
パッケージをロードして setter を呼ぶので動く。**動いていることが正しさの証拠に
ならない**点に注意。

### leaf キーワードの対応表

| leaf | use-package |
|---|---|
| `:straight t` | 同じ |
| `:custom (var . val)` | `:custom (var val)`。値の位置は式として評価されるのでバッククォートは不要 |
| `:custom-face (face . '(...))` | 使わない。`:init (custom-set-faces '(face (...)))` |
| `:bind (:foo-map ...)` | `:bind (:map foo-map ...)`。グローバル束縛は `:map` より前に置き、全体を 1 つのリストにまとめる |
| `:require t` | `:demand t` |
| `:require OTHER-FEATURE` | `:demand t` + `:config (require 'OTHER-FEATURE)` |
| `:leaf-defer nil` | 不要（名前を `emacs` にする） |
| `:hydra (name () ...)` | `:init (defhydra name () ...)`。leaf の `:hydra` は init 時にインライン展開されるので `:config` に置くと意味が変わる |
| `:advice (:around f fn)` | `:init (advice-add 'f :around #'fn)`。これも init 時インライン |
| `:global-minor-mode M` | `:config (M 1)` |
| `:diminish t` | `:diminish`（引数なしで `<name>-mode` が対象） |
| `:after a b` | `:after (a b)`。ただし use-package は条件が満たされると `require` するので、leaf と同じく読み込みたくないときは `:defer t` + `:init` |
| `:doc` / `:tag` / `:includes` | 無い。コメントに落とす |
| `:disabled t` | 同じ（両者とも完全な no-op） |

### 到達不能な設定の検出

```elisp
;; 決してロードされない feature に対する eval-after-load を列挙
(dolist (e after-load-alist)
  (let ((f (car e)))
    (when (and (symbolp f) (not (featurep f)) (not (locate-library (symbol-name f))))
      (princ (format "%s\n" f)))))
```

この検出スニペットは `locate-library` が通る（= インストール済みだがロードされない）
パッケージに対する `:after` は拾えない点に注意。実際に GUI 起動して
`(featurep 'FOO)` を確認するのが確実。

### 書き換えたときの検算

`:bind` を `:map` 形式に直すときに閉じ括弧を 1 つ余らせると、`use-package` の
フォームがそこで閉じてしまい、後続の `:custom` の各行がトップレベルの関数呼び出しに
なる（`void-function (dired-sidebar-theme)` のような形で表面化する）。
括弧のバランスは取れているので `check-parens` では検出できない。
**トップレベルのフォーム数を書き換え前と突き合わせる**のが確実。

```sh
emacs --batch --eval '(dolist (f command-line-args-left)
  (with-temp-buffer (insert-file-contents f) (goto-char (point-min))
    (let ((n 0)) (ignore-errors (while t (read (current-buffer)) (setq n (1+ n))))
      (message "%s: %d forms" f n))))' user-lisp/*.el
```

あわせて `docs/_archived/snapshot.el` の前後 diff を取る（前掲）。

## フォントとアイコン

本文フォントは `HackGen`（Windows 12pt / macOS 16pt）。アイコンは `nerd-icons`。

`nerd-icons` は **Nerd Fonts v3** のコードポイント割り当てを前提にしている。
とくに Material Design アイコンは第 15 面 `U+F0001`〜`U+F1AF0` にあり、
v2 世代のパッチ済みフォントはこの面をまるごと持っていない。
このマシンにインストール済みのフォントを実測した結果：

| フォント | 世代 | mdicon (U+F0001) | seti 上位 (U+E6AD) | codicon (U+EA60) |
|---|---|---|---|---|
| `HackGenNerd` / `HackGen35Nerd`（Console 版含む） | v2 | ✗ | ✗ | ✗ |
| `HackGen Console NF` | v3 系 | ○ | ✗ | ○ |
| `Symbols Nerd Font Mono` (`fonts/NFM.ttf`) | v3 | ○ | ○ | ○ |

dired のディレクトリアイコンが `U+E6AD` なので、HackGen 系だけでは豆腐になる。
`fonts/NFM.ttf` を入れてあり、これを使う。

**リポジトリに置いてあるだけでは効かない。OS 側にインストールすること。**
`my:nerd-font-family` はシステムに登録されたフォントの中から選ぶため、
`fonts/NFM.ttf` が未インストールの環境では HackGen 系（v2）にフォールバックし、
dired のディレクトリアイコンだけが豆腐になる（2026-08、macOS で実際に踏んだ）。
clone しただけの新しいマシンでは必ず必要になる手順。

| OS | 導入方法 |
|---|---|
| Windows | `%LOCALAPPDATA%\Microsoft\Windows\Fonts` へコピーし、`HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts` にレジストリ登録（ユーザー単位） |
| macOS | `cp fonts/NFM.ttf ~/Library/Fonts/` のみ。登録作業は不要で、OS が自動で拾う |

どちらもインストール後に **Emacs の再起動が要る**（`font-get-glyphs` の判定は
起動時に済んでいるため）。フォントのファミリ名は `Symbols Nerd Font Mono`
（PostScript 名 `SymbolsNFM`）。macOS には `fc-list` が無いので、入っているかの
確認は `ls ~/Library/Fonts` で足りる。

`my:nerd-font-family`（`user-lisp/my-appearance.el`）が `font-get-glyphs` で
実際のグリフ有無を見て選ぶので、**フォント名を決め打ちしないこと**。
名前で決め打ちすると、v2 のフォントを掴んでアイコンが全滅する。

`fonts/` に置くのは `NFM.ttf` だけ。all-the-icons 用の 6 フォントは
（all-the-icons をやめたので）リポジトリからも Windows からも削除済み。

## Claude Code を Emacs から使う (`my-claude.el`)

Windows の Emacs には PTY が無いので claude の対話 TUI は動かない
（stdin が TTY でないと claude は自動で `--print` に落ちる）。代わりに
**双方向のストリーミング JSON を素のパイプで駆動する**。端末エミュレーションも
常駐プロキシも要らない。

検討の経緯（ConPTY プロキシ方式との比較、PoC の実測）は
`docs/claude/emacs-claude-pty-proxy-study.md`、設計は
`docs/claude/emacs-claude-stream-json-plan.md`。

改善の第 1 弾（作業ディレクトリ・レイアウト・整形・ステータス）は
`docs/claude/emacs-claude-improve-01.md` にまとめてある。

| キー | |
|---|---|
| `C-c a a` | セッションを開き、画面をレイアウトする（`C-u` で立て直す） |
| `C-c a l` | いつでも同じレイアウトに戻す |
| `C-c a e` | 環境（アカウント）を切り替える |
| `C-c a t` | ワークスペースを信頼済みにする（下記） |
| `C-c a c` | 直近の会話を継いで開く（`--continue`） |
| `C-c a r` | 過去のセッションを一覧から選んで再開（`--resume`） |
| `C-c a m` | モデルを変える（会話は `--resume` で継続） |
| `C-c a i` | 入力バッファを開く（レイアウトも組む） |
| `C-c a s` | リージョンを送る（**レイアウトは変えない**） |
| `C-c a k` | 中断 |
| `C-c a q` | セッション終了 |
| `C-c C-z` / `z` | 会話・入力ウィンドウの最大化トグル（`z` は `*claude*` のみ） |
| `TAB` | 畳んだツール出力の全体を別バッファに出す（`*claude*`） |
| `C-c C-c` | 送信（`*claude-input*`。`M-p` / `M-n` で履歴） |

### 作業ディレクトリの決め方

**さかのぼりはしない。**

1. projectile のプロジェクトルート
2. 取れなければ、cwd に `.claude/` があれば cwd
3. どちらも外れたら `y/n` で確認し、拒否されたら `read-directory-name`

`project.el` は見ない（projectile と役目が重なる）。判定は 2 つの関数に
分けてある。**`my:claude--guess-directory` は確認を出さない**版で、
「起動済みのセッションを使い回すだけ」の場面ではこちらを使う。
分けないと `.claude/` の無いディレクトリから `C-c a a` するたびに
`y/n` が出る。

### ウィンドウのレイアウト（`my:claude-layout`）

```
┌──────────────┐
│ 編集中のバッファ │  フレームの 1/2
├──────────────┤
│ *claude*      │  残り − 5 行
├──────────────┤
│ *claude-input*│  5 行（カーソルはここ）
└──────────────┘
```

`my:claude-window-height-ratio`（既定 0.5）と
`my:claude-input-window-height`（既定 5）で変えられる。

**`window-configuration` は退避しない。** 最大化トグルの復帰先も
`C-c a l` も同じ関数を呼ぶだけなので、どこから何度押しても同じ形に
落ち着く。高さは `window-total-height` から採る（`window-body-height`
だとモードラインとヘッダ行を数え落とす）。

### 環境（アカウント）の切り替え

Pro / Enterprise / Max 20x を `CLAUDE_CONFIG_DIR` で使い分けている。
claude はこれを**プロセスの起動時にしか読まない**ので、切り替えは
立て直すことでしか行えない。そのため**セッションは Emacs 全体で 1 つ**に
限っている（複数あるとどちらに送っているのか分からなくなる）。

`my:claude-environments` に `(ラベル . CLAUDE_CONFIG_DIR)` で並べる。
選択時に `claude auth status --json` を呼んで実際のアカウントを見せる
（実測 0.24 秒。結果はキャッシュし、`M-x my:claude-refresh-auth` で捨てる）。

```
personal   pro         ponkore@gmail.com's Organization
jighead    max         masao.kato@jighead.co.jp's Organization
ESC-Web    enterprise  株式会社　熾火
```

ヘッダ行に
`jighead(max) v2.1.260 | claude-opus-5 | ctx 103.2k 52% | (5h 0%)(7d 6%)(reset 09/05 03:00)`
を出す。残量は `rate_limit_event` から取っている。**アカウントを
切り替える判断はこの数字で行う**ので、常に見えるようにしてある。

### ステータスの表示は「ヘッダ行が主、モードラインは控えめ」

`~/.claude/statusline-command.sh` が端末の TUI に出している項目を
Emacs 側で再現してある。**`statusLine` は端末 TUI の機能で、`-p`
（stream-json）経路では発火しない**（実測でイベントに一切現れない）ので、
スクリプトの出力をもらうのではなく同じ情報を stream-json から自前で
組み立てている。

| 項目 | 取得元 |
|---|---|
| claude のバージョン | `system/init` の `claude_code_version` |
| コンテキスト使用量 | `assistant` の `message.usage` の `input_tokens` + `cache_read_input_tokens` + `cache_creation_input_tokens` |
| コンテキスト上限 | `result` の `modelUsage.<model>.contextWindow`（1M 版なら 1000000 が来る） |
| レート上限とリセット | `rate_limit_event` の `unifiedWindows` |

**`claude --version` を別に呼ぶ必要は無い。** statusline スクリプトが
1 時間キャッシュまでして避けていたプロセス起動が、`system/init` に
最初から入っている。

**effort level は stream-json に出てこない**（全イベントの全キーを
列挙して確認）。`permissionMode` / `output_style` / `fast_mode_state` は
あるが `effort` は無い。git ブランチも載せていない（プロセス起動の
コストに見合わない。「`call-process` が Windows で遅い」の節を参照）。

#### 【重要】`header-line-format` に出す `%` は `%%` に escape する

`header-line-format` / `mode-line-format` に**素の文字列**を渡すと、
Emacs が `%` を書式指定子として解釈し、**`%` と直後の 1 文字がまとめて
消える**。`%` の次が空白でも `)` でも同じ。

```
raw       : ... ctx 103.2k 52% | (5h 5%)(7d 8%)(reset 09/04 23:10)
displayed : ... ctx 103.2k 52| (5h 5(7d 8(reset 09/04 23:10)
```

`my:claude--header` は組み立てた**全体**に
`(replace-regexp-in-string "%" "%%" ...)` を掛けている。ディレクトリ名や
モデル名に `%` が入る場合も巻き込まれないよう、個々の `format` ではなく
最後にまとめて掛けるのが正しい。

**この検証は batch ではできない。** `format-mode-line` は batch では
常に `""` を返す。GUI で `(format-mode-line 文字列)` を見ること。

2026-09-04 に発見。`5h 4% 7d 8%` が `5h 47d 8` と表示されていた。
あわせてレート上限の表示を `(5h 5%)(7d 8%)(reset MM/DD HH:MM)` の形に
変えてある（`%` が区切りに埋もれず読めるように）。

モードラインは `[.emacs.d ... $0.12]`（プロジェクト名 / 応答待ち /
累計コスト）だけ。フルパスは `help-echo` に入れてある。ディレクトリを
ヘッダ行とモードラインの両方に出すと、いちばん幅を食う項目が二重に
なるのでヘッダ行から外した。`doom-modeline` は `mode-line-process` を
`process` セグメントでそのまま拾うので、`doom-modeline-def-segment` を
書く必要は無い（セグメント名がバージョンで変わる問題も踏まない）。

### 逐次表示（`my:claude-stream`、既定 t）

`--include-partial-messages` を付けて `stream_event` を拾い、書かれる端から
バッファに流す。イベントの並びは実測でこうなっている。

```
content_block_start (thinking / text / tool_use)
content_block_delta … (thinking_delta / signature_delta /
                       text_delta / input_json_delta)
assistant                  ← そのブロックの確定版
content_block_stop
```

**`assistant` は `content_block_stop` より先に、ブロック 1 つぶんずつ届く。**
そのため text は delta で出し、`assistant` 側では出さない（出すと二重になる）。
tool_use は逆に delta（`input_json_delta`）を捨てて `assistant` の確定版だけ使う。
JSON の断片は揃うまで意味を持たないため。

中断すると `content_block_stop` が来ないことがあるので、`result` を受けた
ところでブロックを閉じる。

`thinking_delta` の本文は **haiku では空文字列で届く**。`my:claude-show-thinking`
を t にしても何も出ないことがある。

### 過去セッションの一覧（`C-c a r`）

`--continue` は「そのディレクトリの直近の 1 つ」しか選べない。
記録ファイルを直接読んで一覧にする。

セッションは
`<CLAUDE_CONFIG_DIR>/projects/<エンコードしたパス>/<session-id>.jsonl`
に貯まる。ディレクトリ名は **ワークスペースのパスの英数字以外をすべて
`-` に置き換えたもの**。`C:/Users/masao/.emacs.d` なら
`C--Users-masao--emacs-d`。手元の 10 個で突き合わせて確かめた
（合わなかった 1 つはドライブレターの大小違いだけで、Windows の
ファイルシステムでは同じ場所を指す）。

#### 【重要】`message.content` は文字列とは限らない

一覧に出すプロンプトを取り出すとき、**文字列だけを見てはいけない。**
ブロックの配列で入っていることがあり、**Emacs から送ったものは必ず配列**。
文字列しか見ないと、自分で作ったセッションが全部「(プロンプトなし)」に
なる。実際にそうなっていた。`my:claude--content-string` が両方を扱う。

1 MB を超えるファイルもあるので、先頭 200 KB / 400 行で打ち切る。

### サブエージェントの表示

サブエージェントの発言は **`parent_tool_use_id` 付きの assistant / user
イベント**として届く。`--forward-subagent-text`（`my:claude-forward-subagent-text`、
既定 t）を付けると増えるが、**付けなくても一部は届く**（実測）。

**`stream_event` に `parent_tool_use_id` が付くことは無い。**
つまりサブエージェントの本文は delta では来ないので、
`streamed-text` を見ずに必ず出す。見てしまうと、本体のブロックが
開いている間はサブエージェントの発言が捨てられる。

表示は字下げ + `my:claude-subagent-face` で本体と区別する。

### ツールの実行結果は既定で全部畳む

`my:claude-tool-result-max-lines` の既定は **0**（= 常に畳む）。
畳んだ行は 1 行の要約になり、`TAB` で全体を `*claude tool output*` に出す。

```
  ● Read(user-lisp/my-claude.el) … 42 行
```

`Read(...)` の中身は `my:claude--tool-summary` の結果だが、
**`tool_result` には入力が入っていない**。`tool_use` を受けた時点で
名前と一緒に要約も覚えておく必要があるので、`tool-names` ハッシュの値は
`(NAME . SUMMARY)` の cons にしてある。

**エラーだけは畳まない**（`my:claude-error-result-max-lines`、既定 30 行まで）。
一律に畳むと「なぜ失敗したか」がその場から消え、雑音を減らすという
目的とは逆にいちばん見たいものが隠れる。

### Edit / Write の差分表示

`tool_use` の入力に `old_string` と `new_string`（Write は `content`）が
そのまま入っているので、行頭に `-` / `+` を付けて色分けする。
`my:claude-diff-max-lines`（既定 30）を超えたら行数だけ知らせる。

**外部の diff は呼ばない。** Windows に入っている保証が無いうえ、
Edit の入力は置換前と置換後がそのまま来るので、行単位で並べれば足りる。

差分に `TAB` は効かない。**「TAB で全体を表示」と案内していたのは嘘**
だった（`my:claude--show-edit` は `my:claude-full` を設定しないので
`ここには折りたたまれた出力が無い` になるだけ）。案内は
`(差分 %d 行。git diff で確認)` に直してある。

### 許可の `permission_suggestions`

要求には `permission_suggestions`（例: `acceptEdits` に切り替える）が
付いてくる。これを `updatedPermissions` に載せて allow を返すと
**claude 側が以後聞いてこなくなる**。実測で 2 回目の `Write` が
聞かれなくなった。

許可プロンプトの `a` がこれを使う。候補が付いていないときだけ
Emacs 側で覚える従来動作に落ちる。

### 会話バッファの markdown 装飾

`my:claude--fontify-markdown` が 3 つを順に行う。**この順でなければ
ならない。**

1. ``` のブロックを塗る。言語指定があればその言語として着色する
2. `|` の表を罫線に組み直す。1 の結果を見てコードブロックの中を避ける
3. 見出しと行中のコード

**font-lock は使わない。** このバッファは `special-mode` 派生で、挿入時に
`font-lock-face` を直に載せているため、font-lock を有効にすると
そちらに上書きされて競合する。ブロックが確定した時点で一度だけ塗る。

塗る位置は 2 か所ある。逐次表示の経路（`content_block_stop`）と、
delta が来ない経路（スラッシュコマンドの `assistant`）。
**どちらか片方だけだと `/context` の見出しが素のままになる。**

#### コードブロックの言語別着色

一時バッファで該当モードを立てて `font-lock-ensure` し、付いた `face` を
`font-lock-face` としてコピーする（org の
`org-src-font-lock-fontify-block` と同じ手口）。フックは
`delay-mode-hooks` で走らせず、全体を `condition-case` で囲んである。

言語 → メジャーモードは **`markdown-get-lang-mode` を流用**する。
`<lang>-mode` / `<lang>-ts-mode` の推測と `fboundp` の確認までやって
くれるので、自前の `my:claude-lang-mode-alist` に書くのは名前が
一致しないもの（`elisp` `sh` `console` `json` …）だけで済む。

**背景色を消さないこと。** `my:claude-code-face` は背景しか持たないので、
構文の face と**並べてリストで**載せる。帰結として `font-lock-face` の
値がリストになるため、「コードブロックの中か」の判定を `eq` で
書けなくなる（`my:claude--code-face-p` を使う）。旧コードのまま
`(eq (get-text-property …) 'my:claude-code-face)` にしておくと、
**コードブロックの中の `# …` が見出しとして塗り直される。**

描画コストは GUI 実測で **250 行のコードブロック 1 個につき 15.8 ms**。
ブロックが確定した時点で 1 回だけなので詰まらない。上限は
`my:claude-fontify-code-max-lines`（既定 300）で押さえてある。

#### 【重要】罫線の表は「罫線素片が 1 文字 2 桁」を勘定に入れる

markdown のパイプ表は罫線（box-drawing）の表に組み直す
（`my:claude-render-tables`、既定 t）。**桁は Emacs の規則で決める。**
`site-lisp/eaw.el` が ambiguous を幅 2 にし、`my-appearance.el` が
罫線素片（JIS X 0208）を HackGen に割り当てるので、**論理幅と実描画幅が
一致する**。claude 側の桁組みには合わせず、セルの中身だけを取り出して
`string-width` で組み直す。

> `my-pty`（端末）で ambiguous を幅 1 に切り替えているのとは**逆の話**。
> あちらは桁を数えているのが conhost なので合わせにいくが、
> こちらは Emacs 自身が数えるので合わせる必要が無い。

罠は列幅の刻み方にある。**セルの詰め物は半角空白（1 桁）だが、罫線は
1 文字で 2 桁ある。** 列幅 `w` に対して `(make-string (+ w 2) ?─)` と
書くと罫線の行だけが倍の長さになる。

```
幅= 44 |┌─────┬────────┬─────┐|   ← 5 文字 = 10 桁
幅= 26 |│ 列  │ 説明   │  値 │|   ← セルは 5 桁
```

`w + 2` が罫線 1 文字の桁数の倍数になるまで列幅を広げて直した。
倍数の判定に使う値は決め打ちせず `(char-width ?─)` を実測する
（eaw を外した Emacs では 1 になる）。

GUI 実測（`string-width` だけでは検算にならないので
`string-pixel-width` も見る）:

```
幅= 28 px= 224 |┌───┬────┬───┐|
幅= 28 px= 224 |│ 列   │ 説明   │   値 │|
幅= 28 px= 224 |├───┼────┼───┤|
幅= 28 px= 224 |│ a    │ あいう │    1 │|
幅= 28 px= 224 |│ bb   │ ○△□ │   22 │|
幅= 28 px= 224 |│ ccc  │ ─│   │  333 │|
幅= 28 px= 224 |└───┴────┴───┘|
```

全角・ambiguous・罫線素片を混ぜても全 7 行が一致する。
`┌┬┐├┼┤└┴┘│─` はすべて `char-width` 2 / 16px。

変換するのは**区切り行（`|---|:---:|`）を伴う表だけ**。無いと
`a | b` のような何気ない行まで拾う。

#### 追従（自動スクロール）はウィンドウごとに判定する

`my:claude--insert` は「末尾を見ているときだけ追従する」。判定は
**`window-point` でウィンドウごとに**行う。バッファの `point` 1 つで
決めていると、会話バッファが 2 つのウィンドウに出たときに
**読み返し中の窓まで末尾へ飛ぶ**（あるいはその逆で、末尾を見ている窓が
追従しない）。レイアウトの最大化トグルや `display-buffer` の再利用で
普通に起きる。

バッファ自身の `point` も別に見る。`save-excursion` は挿入前の位置に
戻す（マーカーの `insertion-type` が nil）ので、末尾にいたぶんは
明示的に `goto-char` しないと追従が切れる。

##### 【重要】会話バッファへの書き込みは必ず `my:claude--at-end` を通す

上の作法をマクロ `my:claude--at-end` に括り出してある。**`save-excursion` +
`goto-char (point-max)` + `insert` を直接書いてはいけない。**

`save-excursion` のマーカーは `insertion-type` が nil なので、**末尾での
挿入では挿入したテキストの前に取り残される**。

```elisp
(save-excursion (goto-char (point-max)) (insert "    +new\n"))
;; => point=5  point-max=14   末尾にいたのに外れる
```

一度外れると `my:claude--insert` の `(>= (point) max)` が偽になるため、
**以後どれだけ流れても二度と追従しない**。1 回の書き込みでその後ずっと
壊れるので、原因になった書き込みから離れたところで表面化する。

末尾を削り直す経路も同じ。削除でマーカーが手前に引かれ、そこへ挿入しても
前に置かれたままになる。

2026-09-04 に `my:claude--insert-diff`（Edit / Write の差分）と
`my:claude--end-paragraph`（段落の整形）が直接書いていたのを直した。
**差分が 1 回出ると自動スクロールが止まる**という壊れ方をしていた。
同じ 6 行を 3 か所に書いていたのが取りこぼしの原因なので、マクロに寄せてある。

GUI 実測（2 窓、修正前 → 修正後）:

| | 修正前 | 修正後 |
|---|---|---|
| `insert` のあと末尾か | t | t |
| `insert-diff` のあと末尾か | **nil**（point=10 / point-max=38） | **t** |
| `end-paragraph` のあと末尾か | **nil** | **t** |
| 読み返し中に動かないか | t | t |
| 末尾を見ている窓だけ追従するか | **nil** | **t** |

#### 入力バッファは markdown-mode 派生

`my:claude-input-mode` は `markdown-mode` から派生させ、着色は
`markdown-fontify-code-blocks-natively` に任せる（会話バッファと違って
font-lock をそのまま使える）。

**`markdown-mode-hook` は走らせない。** `my-text.el` の
`my:setup-markdown-mode` は `.md` ファイルを編集する前提の設定で、
送信用のバッファに持ち込む理由が無い。`define-derived-mode` は親を
`delay-mode-hooks` で包み、最後に `run-mode-hooks` が `run-hooks` で
回すので、モード本体で `(setq-local markdown-mode-hook nil)` すれば
親のフックだけを外せる。

`C-c C-c` は markdown 側では prefix だが、子のキーマップが先に引かれる
ので `my:claude-input-send` が勝つ。`completion-at-point-functions` の
`my:claude--capf`（深さ -100）は**必ず張り直すこと**（落とすと行頭の
`/` が `cape-file` に食われて C: 直下の一覧が出る）。

`markdown-mode` は autoload なので `my-claude.el` から
`(require 'markdown-mode)` する必要は無い。`define-derived-mode` は
親のキーマップを**モード関数の中で** `set-keymap-parent` する
（`derived.el` のコメントが「親がまだロードされていないことがある」と
明記している）。

### セッションの再開とモデルの変更

| | |
|---|---|
| `--continue` | そのディレクトリの直近の会話を継ぐ。Emacs を再起動しても、端末で続けていた会話でも繋がる |
| `--resume <id>` | `session_id` を指定して継ぐ |

どちらも stream-json と併用できる（実測）。`init` イベントの `session_id` を
覚えているので、`C-c a m` は **`--resume` でモデルだけ差し替える**。
Opus と Haiku を行き来しても、それまでの話は消えない（実測で確認）。

**アカウントをまたぐ再開はできない。** セッションの保存先が
`CLAUDE_CONFIG_DIR` の下なので、`C-c a e` で環境を変えると会話は切れる。

### スラッシュコマンドの補完

`initialize` の control_response に `commands`（名前・説明・引数ヒント）が
入っている。実測で 52 個。これを覚えて入力バッファの `completion-at-point`
に流す。

**行頭の `/` だけを対象にすること。** 文中のスラッシュまで拾うと
`src/foo` のようなパスを書くたびに候補が出て邪魔になる。
2 つめの `/` が来たらパスだと見なして手を引き、`cape-file` に譲る。

#### 【重要】補完領域に先頭の `/` を含めること

`/` の**後ろ**から補完領域を始めると接頭辞の長さが 0 になり、
`corfu-auto-prefix`（`my-completion.el` で 1）に満たないという理由で
**corfu の自動補完に捨てられる**。捨てられると次の capf が呼ばれ、
深さ 90 にいる `cape-file` が `/` を絶対パスと解釈して
C: 直下のディレクトリ一覧を出す。実際にそうなっていた。

領域を `/` から取り、候補も `/name` の形にすれば接頭辞長が 1 以上になる。

| 入力 | |
|---|---|
| `/` | claimed（接頭辞長 1、候補 52、corfu の条件を満たす） |
| `/cont` | claimed（接頭辞長 5） |
| `/c/Projects/foo` | 手を引く（`cape-file` がパスとして扱う） |
| `see src/foo` | 手を引く |
| `/context and more` | 手を引く |

capf は深さ `-100` で入れて `cape-file`（90）より確実に先に来るようにしてある。

#### 【重要】許可と拒否で control_response の形が違う

claude が返してくるエラーが契約を明示している。

```
Expected {behavior: 'allow', updatedInput?: object}
      or {behavior: 'deny', message: string}
```

**拒否に `updatedInput` を付けてはいけない。`message` は必須。**
どちらを外しても不正な応答と判定され、claude には「拒否された」ではなく
「許可フックでエラーが起きた」と伝わる。実測:

| 送った形 | claude が受け取った tool_result |
|---|---|
| `{deny, updatedInput}` | `The canUseTool callback returned an invalid permission result. …` |
| `{deny}` だけ | 同上 |
| **`{deny, message}`** | **その message がそのまま届く** |

**ツールが実行されない点はどれも同じなので気づきにくい。**
違いは claude への伝わり方だけで、不正な形だと
「システム側の問題です」と的外れな返事をしてくる。

許可プロンプトの `r`（理由を書いて拒否）はこの `message` に載る。
日本語もそのまま届く。「そのファイルは触らないで、代わりに…」と
書くと claude が別の手を考える。

#### 【重要】スラッシュコマンドは `stream_event` を伴わない

`num_turns=0` で API を通らないため、**`assistant` で本文が来るのに
`stream_event` が 1 つも来ない**。実測:

| 入力 | イベント | assistant 本文 |
|---|---|---|
| `/context` | `assistant` `result` のみ | 6948 文字 |
| `/mcp` | 同上 | 98 文字 |
| `/usage` | 同上 | 855 文字 |
| 普通の質問 | `stream/*` が並ぶ | 135 文字 |

そのため「逐次表示が有効なら `assistant` の text は捨てる」としてはいけない。
`my:claude-stream` ではなく **そのブロックを実際に delta で出したか**
（`streamed-text` フラグ）で判断する。これを間違えると
**`/mcp` などが送信できたのに何も表示されない**。実際にそうなっていた。

スラッシュコマンドは API を消費しない（`$0.0000`）ので気軽に使える。
ただし `/mcp` は「詳細は端末の `/mcp` で」と要約を返すだけで、対話 UI は出ない。
`init` の `terminal_slash_commands`（`doctor` / `color` / `reload-plugins`）は
端末が要るもので、補完の注釈に `[端末専用]` と出るようにしてある。

### 【重要】Emacs から起動すると必ず「信頼されていないワークスペース」になる

`.claude/settings.json` を置いてあるプロジェクトで `C-c a a` すると、
会話バッファにこれが出る。

```
Ignoring 17 permissions.allow entries from .claude/settings.json:
this workspace has not been trusted. ...
set projects["c:/Projects/ESC-Web/WebCoreSystem_v1"].hasTrustDialogAccepted: true
```

原因は **Emacs が子プロセスの作業ディレクトリのドライブレターを小文字にする**こと。
実測（Emacs 31.1 / Windows 11）:

| | |
|---|---|
| `default-directory` | `C:/Projects/Foo/` |
| `expand-file-name` | `C:/Projects/Foo/`（大文字のまま） |
| **子プロセスが見る cwd** | **`c:\Projects\Foo`** |

`default-directory` を大文字にしても変わらない。`make-process` が
作業ディレクトリを設定する経路で小文字になる。

一方、端末で対話的に起動した claude は大文字のまま記録するので、
`.claude.json` の `projects` に**大小 2 つのエントリができる**。

```
C:/Projects/ESC-Web/WebCoreSystem_v1   trusted=True    ← 端末の TUI が書いた
c:/Projects/ESC-Web/WebCoreSystem_v1   trusted=False   ← Emacs 経由で作られた
```

Emacs 側は必ず信頼されていない方を引くため、プロジェクトの
`permissions.allow` がまるごと無視される。**壊れはしないが許可の確認が
増えるだけになる。** gopls が大文字のドライブレターを返して診断が出なかった
のとまったく同じ罠。

`--settings` でファイルや JSON 文字列を明示しても回避できない（実測）。
`-p` は仕様として信頼ダイアログを出さない。

`C-c a t`（`my:claude-trust-workspace`）が
`projects[KEY].hasTrustDialogAccepted` を `t` にする。KEY は claude が
警告で言ってきたものをそのまま使う。**セッションを先に終了させてから
書く**（claude 自身がこのファイルを書き戻すため）。書き換え前に
`.claude.json.bak-my-claude-<時刻>` を作る。

書き戻しは `json-parse-buffer` → `json-serialize` の往復で行う。
69 KB の設定で検証したところ、差分は追加した 1 エントリのみで
`oauthAccount` を含め無傷だった。

> 検証で `equal` を使ってハッシュテーブルを比べてはいけない。
> **Emacs の `equal` はハッシュテーブルの中身を見ない**ので、
> 同一でも nil になる。中身を比べるなら serialize してから。

#### 【重要】既定の環境には `CLAUDE_CONFIG_DIR` を「設定しない」

`~/.claude` を明示的に指定してはいけない。claude は
`$CLAUDE_CONFIG_DIR/.claude.json` を探すが、実体は `~/.claude.json` に
あるため見つからない。実測:

| | `email` / `orgName` | 標準出力 |
|---|---|---|
| 未設定（既定） | `ponkore@gmail.com` / 取れる | JSON のみ |
| `CLAUDE_CONFIG_DIR=~/.claude` | **どちらも `null`** | **警告が混ざる** |

警告は stderr ではなく**標準出力**に出るので、stream-json の途中に
非 JSON の行が混ざることになる。`my:claude-environments` では
既定の環境の CONFIG-DIR を `nil` にすること。

#### 【重要】nil のときは「設定しない」ではなく「消す」

Emacs 自身が `CLAUDE_CONFIG_DIR` の設定された環境から起動されていると、
何もしなければそれを継承する。**「既定（Pro）」を選んだつもりで別の
アカウントに繋がる。** 実際に踏んだ（`personal` が `max` と表示された）。

`my:claude--process-environment` が `setenv` に nil を渡して
明示的に削除している。

### 【重要】起動オプションは 4 つとも省略できない

```
claude -p --verbose --input-format stream-json --output-format stream-json        --permission-prompt-tool stdio
```

| 省略すると | |
|---|---|
| `--verbose` | **即エラー終了**（`--output-format=stream-json requires --verbose`） |
| `--permission-prompt-tool stdio` | **許可要求が黙って自動拒否される** |

後者がとくに厄介。`--permission-prompts` の既定は `host`（= クライアントが答える）
なのに、このオプションが無いと `control_request` が**一度も飛んで来ず**、
`system/permission_denied` が流れてツールが実行されないだけになる。
実測では `Write` が拒否され、付けると `can_use_tool` が届いて許可でき、
ファイルが実際に作られた。**ツールが動かないときの第一容疑者。**

### 【重要】`default-process-coding-system` を束縛して起動する

`my-japanese.el` がグローバルの cdr を cp932 にしているため、束縛せずに
起動すると**標準入力の日本語が壊れる**。この経路は引数ではなく標準入力で
本文を渡すので、`(utf-8-unix . utf-8-unix)` でよい。
「引数は cp932」の話（別節）とは逆になる点に注意。

### 割り込んでもセッションは死なない

`{"type":"control_request","request":{"subtype":"interrupt"}}` を送ると
`control_response` が返り、続けて `result` が
`terminal_reason=aborted_streaming` / `is_error=true` で来る。
**プロセスは生きており、次のターンもそのまま送れる**（実測）。

`result` が `is_error` のときに EOF を送るとプロセスの終了コードは 1 になるが、
異常終了ではない。sentinel で騒がないこと。

### `system/init` はターンごとに来る

起動直後ではなく**最初のメッセージを送ったあとに来る。しかも毎ターン来る**。
バッファに挿すと会話の途中に何度も見出しが混ざるので、`header-line-format`
に出している。

### イベントは `assistant` だけ見れば表示できる

`--include-partial-messages` を付けると `stream_event` でトークン単位に
刻まれて来るが、`assistant` イベントがブロック確定ごとに丸ごと来るので、
逐次表示が要らないうちは `stream_event` を捨ててよい。

### 検証はプローブで安く

`--model haiku --tools ""` にする。Opus だと 1 往復で $0.83 かかった
（大半はシステムプロンプトのキャッシュ作成）。
`my:claude-log` を t にすると生の JSON Lines が残るので、
上流のイベント種別が変わったときに気づける。

## 対話 TUI を Emacs で動かす (`ptyd/` + `my-pty.el`)

Windows の Emacs には PTY が無く `make-process` は常にパイプになるので、
対話 TUI が動かない。`ptyd`（Go）が疑似コンソールを持って子プロセスを
動かし、VT バイト列を stdio で Emacs に流す。表示は term.el に任せる。

```
Emacs ──stdin (JSON Lines)──> ptyd ──ConPTY──> 子プロセス
      <──stdout (生の VT)──        <─────────
      <──stderr (診断の行)──
```

| | |
|---|---|
| `M-x my:pty-build` | `ptyd.exe` を作る（`gitd` と同じく各マシンで） |
| `M-x my:pty-run` | 任意のコマンドを端末で動かす（汎用） |
| `M-x my:claude-term` | claude の TUI を開く |

**バイナリが無ければ `user-error` になるだけ**で、他の設定には影響しない。

stdin だけ JSON にしてあるのは、キー入力のほかに画面サイズを送る必要が
あるため。stdout を生のままにしてあるのは、そちらが本流で量が多く、
base64 と JSON のエスケープを挟む意味が無いから。

### 表示は eat (`my:pty-backend`、既定 `eat`)

term.el では通常の TUI がまともに映らなかった。代替画面 (`ESC[?1049`) も
同期出力も持たず、私用パラメータ付きの CSI (`ESC[>4;2m`) を SGR と
誤解釈する。`--ax-screen-reader` に逃がせば崩れないが、平板で読みにくい。

`eat`（NonGNU ELPA、純 elisp）に差し替えた。**通常モードの TUI が
そのまま出る。** term.el は `my:pty-backend` を `term` にすれば残っている。

| | term.el | eat |
|---|---|---|
| 代替画面 `?1049` | ✗（`?47` のみ） | ○ |
| bracketed paste `?2004` | ✗ | ○ |
| マウス `?1000`〜`?1006` | ✗ | ○ |
| `ESC[>4;2m` | **SGR 0;2 と誤解釈** | 私用パラメータとして別扱い |
| UTF-8 の復号 | `locale-coding-system` 決め打ち | 自前 |
| アプリへの書き込み | `process-send-string`（advice が要る） | **`input-function` パラメータ** |

最後の行が効いた。eat は端末→アプリの書き込みを `input-function` から
出すので、**term.el のときに必要だった advice が丸ごと不要**になる。

プロセスの符号化も逆になる。**eat は復号済みの文字列**を受け取る
（パーサが文字を比較する）が、**term.el は生バイト**を要求して復号を
自分でやる。`:coding` を切り替えている。

### 【重要】起動時のサイズはメジャーモードを立ててから測る

`eat-mode` も `term-mode` も `kill-all-local-variables` を通るので、
**先にヘッダ行や `truncate-lines` を設定しても消える**。実際に消えていた。

順序は「モードを立てる → ヘッダ行と `truncate-lines` → サイズを測る →
端末を作る」。行数は `window-body-height` ではなく
`(floor (window-screen-lines))` で採る（ヘッダ行と端数行を勘定に入れる）。
`pop-to-buffer` で別のウィンドウに移ることがあるので、そのあとにも
`my:pty--sync-size` を呼ぶ。

**ヘッダ行を立てるのはサイズを測る前。** あとから足すと使える行数が 1 減り、
疑似コンソールと Emacs の行数が食い違って、以後の描画が 1 行ずつずれる。

### 【重要】端末を開いている間は ambiguous 幅を 1 に切り替える

**これを入れないとロゴが横に伸び、表の罫線が揃わない。**

claude も conhost も East Asian Ambiguous を **幅 1** として桁を組むが、
`site-lisp/eaw.el` を入れた Emacs はそれらを幅 2 で描く。実測:

| 文字 | この設定 | `emacs -Q` + 日本語環境 |
|---|---|---|
| `█` U+2588（マスコット） | **2** | 1 |
| `▀` U+2580 | **2** | 1 |
| `─` U+2500（罫線） | **2** | 1 |
| `│` U+2502 | **2** | 1 |
| `·` U+00B7 | **2** | 1 |
| `★` U+2605 | **2** | 1 |
| `○` U+25CB | 2 | 2（組み込みでも幅 2） |

同じ画面を WezTerm で出すと正しく揃うので、**ずれているのは Emacs 側だけ**
だと切り分けられる。

`char-width-table` はグローバルで**バッファ単位に変えられない**ため、
`my:pty-narrow-ambiguous`（既定 t）が「最初の端末を開いたら全体を幅 1 に
切り替え、最後の端末を閉じたら戻す」形にしている。切り替えたときは
`message` で知らせる。復帰はプロセスの sentinel とバッファの
`kill-buffer-hook` の両方から呼ぶ。

他のバッファの桁揃えも端末を開いている間だけ変わる。それが困るときは
`my:pty-narrow-ambiguous` を nil にする（端末の見た目は崩れる）。

**`my:pty-narrow-ambiguous` は defcustom なので `M-x` では出てこない。**
開いている端末にその場で反映して見比べたいので、
`M-x my:pty-toggle-ambiguous-width` を用意してある。崩れの原因が eaw か
どうかは、これで切り替えて見比べるのがいちばん早い。

### 【重要】端末バッファでは折り返さない (`truncate-lines` = t)

**折り返すと 1 桁ずれただけで以後の行が全部ずれる。**

`my:pty-narrow-ambiguous` を nil にしたときや、幅の解釈が食い違う文字が
残っているときの保険。折り返すとレイアウトが崩れるが、`truncate-lines`
なら右端が切れるだけで格子は保たれる。

### `⏵` が `[]` になるのはフォントの問題

`glyphless-char-display` の extra slot 0 を eat が `empty-box` にしている
（`eat--setup-glyphless-chars`）。**幅の問題ではない**（U+23F5 は幅 1）。
そのコードポイントのグリフを持つフォントが無いだけ。豆腐ではなく
eat が意図して出している空の箱。

### 【重要】幅表だけでは足りない。フォントも切り替える

`char-width` を 1 にしても、**フォントがその文字を 2 桁ぶんの幅で描けば
見た目はずれる。** GUI では `string-pixel-width` が `char-width` ではなく
フォントの送り幅を返すことからも分かる。`M-x my:pty-toggle-ambiguous-width`
で画面が変わらなかったのはこれが理由。

原因は `my-appearance.el` の

```elisp
(set-fontset-font nil 'japanese-jisx0208 jp-fontspec)  ; jp-fontspec = HackGen
```

`─` (U+2500) は **JIS X 0208 の罫線素片**なので、この行で HackGen に
割り当てられ、全角 16px で描かれる。

実測（`:height` 116、半角 8px / 全角 16px の設定）:

| フォント | `a` | `あ` | `─` | `①` | `★` | |
|---|---|---|---|---|---|---|
| HackGen（通常） | 8 | 16 | **16** | **16** | 16 | 全部ずれる |
| HackGen Console NF | 8 | 16 | 8 | **16** | 16 | 丸数字が残る |
| **Consolas** | 8 | 16 | 8 | **8** | 16 | **最良** |
| HackGen35 Console NF | 8 | 16 | 11 | 9 | — | 3:5 設計で合わない |
| Cascadia Mono | 9 | 16 | 9 | 9 | — | 半角が 9px |

**`my:pty-console-font` の既定は nil（切り替えない）。** 切り替えると
`char-width-table` と同じく **Emacs 全体**のフォントが変わり、編集中の
バッファまで巻き込む。端末の見た目を優先したいときだけ Consolas にする。 Consolas は日本語を
持たないが、`あ` はフォントセットのフォールバックで全角のまま描かれる
（実測で 16px）。`my-appearance.el` のコメントにある「Consolas だと
丸付き数字が半角幅になってしまっている」は、通常の編集では困る挙動だが
**端末では逆にそれが正しい**（claude は `①` を 1 桁として桁を組む）。

**`★` (U+2605) と `※` (U+203B) は手元のどのフォントでも全角。**
これらを含む行だけは揃わない（未解決）。

端末を開いているあいだだけ差し替え、幅表と同じ寿命で、最後の端末を
閉じたら戻す。

#### 【重要】`set-fontset-font` はこの設定では効かない

`nil`（選択フレーム）にも `t`（既定）にも入れ、`clear-face-cache` と
`redraw-display` まで呼んでも、GUI の実測で `font-at` は元のフォントを
返し続け `string-pixel-width` も 16 のままだった。丸数字のレンジだけ
別フォントに回そうとしても同じだった。

**実際に効く経路は `set-face-attribute 'default nil :family`**
（`my-appearance.el` の `emacs-font-setting` と同じ）。こちらに変えたら
1 回で通った。

**したがって端末用に選べるフォントは 1 つだけで、レンジごとの割り当ては
できない。** だから `★` を諦めてでも `①` が直る Consolas を選んでいる。

```
▐ U+2590 width=1 pixel=8 font=HackGen Console NF
```

`:height` は触らないこと。サイズが変わると桁が全部ずれる。

確かめ方（`*claude-term*` で）:

```elisp
(with-current-buffer "*claude-term*"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[─▐█①]" nil t)
    (goto-char (1- (point)))
    (let* ((c (char-after)) (f (font-at (point))))
      (list c (char-width c) (string-pixel-width (string c))
            (and f (font-get f :family))))))
```

`font=` が切り替え先になっていること、`pixel` が 8 であることを見る。

**Nerd Font のアイコン領域（#xe000-#xf8ff）は触らない。**
`my-appearance.el` がそこを別のフォントに回しており、上書きすると
アイコンが豆腐になる。

### 桁が合っているかの確かめ方

見た目が崩れていても、**バッファの中で桁が合っているかは別**。切り分けは
`eat--t-invisible-space` を除いた「見える文字列」の `string-width` を測る。
eat は全角文字の前に invisible な詰め物を入れるので、素の
`buffer-substring` の長さで測ると必ずずれて見える。

既知の表を `powershell -File` で流し込んで実測した結果（端末が生きている
間に測ること。終了すると幅表が戻る）:

| 流したもの | 期待 | 実測 |
|---|---|---|
| `ABCDEFGHIJKLMNOPQRST\|` | 21 | 21 |
| `あいうえおかきくけこ\|` | 21 | 21（filler 10） |
| `────────────────────\|` | 21 | 21 |
| `┌────┬────┐\|` | 12 | 12 |

claude 自身が組んだ表でも、罫線行と内容行が同じ幅になることを確認した。

```
幅= 21 |  │ cd   │ abcd     │|
幅= 21 |  ├──────┼──────────┤|
幅= 21 |  │ ef   │ あいう   │|
```

**つまり ptyd → conhost → eat → バッファは正しい。** 生の VT にも余計な
空白は入っていない（conhost は素通し）。それでも画面が崩れて見えるなら、
残るのは**フォントの描画**。`char-width` が 1 でも、フォールバックの
フォントがその文字を 1 桁ぶんの幅で描くとは限らない。CLAUDE.md の
eaw の節にある「1098 文字はプロポーショナルなフォールバックで描かれ、
`char-width` をどちらにしても桁は揃わない」と同じ話。

GUI での確かめ方:

```elisp
;; 半角 1 桁のピクセル幅と、罫線 1 文字のピクセル幅を比べる
(list (string-pixel-width "a") (string-pixel-width "─")
      (string-pixel-width "○") (string-pixel-width "あ"))
```

`a` が 8 なら、`char-width` 1 の文字は 8、2 の文字は 16 になっているのが
正しい。そうなっていない文字はフォールバックで描かれている。

### 【重要】eaw.el の文字幅表で eat が無限ループする

`site-lisp/eaw.el` が East Asian Ambiguous を幅 2 にしていると、
**eat が claude の TUI 出力の処理から戻ってこない**。実測（同じ 2385 文字を
流し込む）:

| | |
|---|---|
| `emacs -Q` | 完了 |
| `emacs -Q` + `(eaw-fullwidth)` | **戻ってこない** |
| 設定全体 | **戻ってこない** |
| 幅表を戻して流す | 完了 |

`my:pty--narrow-width-table` が `char-width-table` の複製を作り、
`east-asian-ambiguous` の文字を幅 1 に戻す。それを
`eat-term-process-output` と `eat-term-redisplay` の間だけ `let` で束縛する。

**そもそも桁を数えているのは conhost** であり、Windows のコンソールは
ambiguous を幅 1 として扱う。Emacs 側だけ幅 2 で数えると、ループしなかった
としても桁がずれる。端末の中では conhost に合わせるのが正しい。
バッファの外（通常の編集）には影響しない。

### 【重要】`setf (eat-term-parameter …)` は使えない

このリポジトリはバイトコンパイルしない方針なので、`setf` の展開は
**my-pty.el を読み込んだ時点**で起きる。そのとき eat はまだロードされて
おらず、gv のセッタが無いため `void-function \(setf eat-term-parameter\)`
になる。素の関数 `eat-term-set-parameter` を使う。

### 【重要】プリミティブへの advice は native-compile されたコードに効かない

term.el はキーを `process-send-string` で送るので、最初はそれを advice で
包んで JSON に変換しようとした。**まったく効かなかった。**

`term.eln`（native-compile 済み）は**プリミティブを直接呼ぶ**ので、
symbol の function cell に張った advice を素通りする。実際、生の
`echo …
` がそのまま ptyd に届いて
`bad line: invalid character 'e'` になった。

包むなら **Lisp の関数**にする。そちらは symbol 経由で呼ばれる。
term.el が書き込む入口は 4 か所しかない。

| 関数 | いつ通るか |
|---|---|
| `term-send-raw-string` | char モードのキー入力（ほぼ全部） |
| `term-send-string` | 貼り付けなど |
| `term-send-eof` | `C-d` 相当 |
| `term-emulate-terminal` の中 | `ESC[6n`（CPR）への応答。claude は送ってこない（実測 0 回） |

前の 3 つを `:around` で包んでいる。使っているセッションが無くなったら外す。

`my-gitd.el` が `magit-process-file` を、`my-lsp.el` が `eglot-uri-to-path` を
包んでいるのは、どちらも Lisp の関数なので問題ない。

### 以下は `term` バックエンド（退避先）の話

### 【重要】`locale-coding-system` をバッファローカルに上書きする

term.el は復号に `locale-coding-system` を決め打ちしている（31.1 で 5 箇所）。
日本語 Windows では cp932 なので、UTF-8 を吐く TUI の罫線が壊れ、
`args-out-of-range` で落ちる。`my:pty-run` が
`(setq-local locale-coding-system 'utf-8-unix)` を入れている。

### term.el が読めない CSI は ptyd 側で落とす

term.el はプライベートな CSI の目印として `?` しか見ていないため、
`ESC[>4;2m`（modifyOtherKeys）を `>` ごと数値化して SGR 0;2、つまり
「全属性リセット + faint」として実行してしまう。

`ptyd -strip-unsupported-csi` が `ESC[<` `ESC[>` `ESC[=` を落とす。
**`ESC[?` は落とさない**（term.el が正しく扱う）。実測:

| | バイト数 | `ESC[>` | `ESC[<` | `ESC[?` |
|---|---|---|---|---|
| strip なし | 852 | 7 | 3 | 22 |
| strip あり | 801 | 0 | 0 | 22 |

途中で切れたシーケンスは ptyd 側で持ち越す。ConPTY からの読み取りは
任意の位置で切れるので、1 回の Write に収まっている保証が無い。

### 端末経由だと信頼ダイアログが出る

`-p`（案 A）は仕様として信頼ダイアログを飛ばすが、**`my:claude-term` では
本来のダイアログが出る**。ここで `y` を押せば、Emacs 起動時の小文字
ドライブレターのキーで `hasTrustDialogAccepted` が立つので、
案 A 側の「permissions.allow が無視される」警告も消える。

### リサイズ

`window-size-change-functions` でウィンドウの桁数・行数を見て、
`term-reset-size` と `ResizePseudoConsole` の両方を更新する。
実測で `ESC[8;24;80t` が返り、その幅で再描画された。

## プラットフォーム固有の注意事項

- メインは **Windows 11**、パッケージ管理に **Scoop**（`USERPROFILE/scoop/shims` を `exec-path` に追加）
- **`HOME` はユーザー環境変数として `C:\Users\<user>` に設定してある。**
  未設定だと Windows の Emacs は `%APPDATA%` を `~` とみなすため、
  Explorer やスタートメニューから起動したときに `init.el` が見つからない。
  **設定側で `(setenv "HOME" ...)` してはいけない。** `init.el` が読まれる
  時点で `.emacs.d` の探索は終わっているので手遅れであり（`early-init.el`
  でも同じ）、`user-emacs-directory` は展開前の `"~/.emacs.d/"` という文字列の
  ままで Windows の Emacs は `expand-file-name` のたびに `HOME` を読み直すため、
  途中で差し替えると `recentf` / `custom.el` / `straight` の保存先が
  実際に動いている設定とは別のディレクトリになる
- Windows のシェルは Git 付属の `bash.exe`（存在するときだけ設定）、エンコーディングは cp932/UTF-8 混在
- Windows IME 統合には `tr-ime` + `w32-ime`（どちらも straight で導入）。
  2026-08 時点で tr-ime 0.5.0（2022-06）、w32-ime は 2020-11 のコミットが
  それぞれ upstream の最新で、これより新しい版は無い。導入手順
  （`tr-ime-advanced-install` → `default-input-method` → `w32-ime-initialize`）も
  README の推奨どおり
- **モードラインの IME 表示は `w32-ime-input-method-title` で設定する。**
  `w32-ime-mode-line-state-indicator` は w32-ime が自前で `mode-line-format` の
  先頭に差し込むための変数で、`mode-line-format` をまるごと差し替える
  doom-modeline とは併用できない。doom-modeline の `input-method` セグメントは
  `current-input-method-title` を見ており、w32-ime はそこに
  `w32-ime-input-method-title`（既定 nil）を入れる
- `M-\`` と `M-kanji` を `ignore` にしているのは意図的。その組み合わせは
  tr-ime / Windows 側が IME のトグルとして処理するので、Emacs 側では
  何もしないのが正しい。Emacs から切り替えるのは `C-\` と 漢字キー
- macOS / Linux では `exec-path-from-shell` を使用
- OS 判定は `(eq system-type 'windows-nt)` / `'darwin` / `'gnu/linux`。
  ウィンドウシステム判定は `window-system` の `'w32` / `'ns` / `'x` / `'pgtk`

## テーマ

Emacs 31.1 同梱の **modus-themes 5.2.0** を `load-theme` で使う。
`:straight` は付けない（組み込み優先。`org` / `transient` と同じ扱い）。

かつて straight に 2021 年の 1.7.0 が入っていて、そちらが読まれていた。
v2 世代の API（`modus-themes-load-themes` / `modus-themes-load-vivendi` /
`modus-themes-region`）は 5.x には存在しないので、書き換えが要る。

| 旧 (v1/v2) | 新 (v4/v5) |
|---|---|
| `(modus-themes-load-themes)` + `(modus-themes-load-vivendi)` | `(load-theme 'modus-vivendi :no-confirm)` |
| `modus-themes-region '(bg-only …)` | `modus-themes-common-palette-overrides '((fg-region unspecified))` |
| `modus-themes-region '(… no-extend)` | 廃止（移行先なし） |

色の調整はパレットの上書きで行う。パレット名は
`etc/themes/modus-themes.el` の `modus-vivendi-palette` を見る。
`:custom` は `:config` より先に走るので、上書きは `load-theme` に間に合う。

**`:no-require t` が必須。** `modus-themes` は `etc/themes/` にあり `load-path` に
載っていないため `(require 'modus-themes)` は失敗する。use-package は require に
失敗すると `:config` ごと飛ばすので、これが無いと `load-theme` が呼ばれない。

## org のアーカイブ先の `#YM`

アーカイブ先の指定に `#YM` と書くと `YYYY-MM` に展開される。

```org
#+ARCHIVE: %s_#YM_archive::* From %s
```

→ `note.org_2026-08_archive` に `* From note.org` 見出しで格納される。
ファイル名部分でも見出し部分でも使える。

実装は `org-archive--compute-location` への `:filter-args` advice
（`my-text.el`）。旧実装は `org-extract-archive-file` への `:filter-return`
だったが、この関数は org 9.8 で削除された。後継の
`org-archive--compute-location` は戻り値が `(FILE . HEADING)` の cons なので
`:filter-return` は使えず、**入口を `:filter-args` で押さえる**形にしてある。
引数は `::` で区切る前の生の文字列なので戻り値の形に依存しない。

`org-archive-subtree` は
`(or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location)` を
この関数に渡すので、`#+ARCHIVE:` / `ARCHIVE` プロパティ / 変数のどれで
指定しても効く（`org-archive-all-*` からの呼び出しも同様）。

## org でクリップボードの画像を貼る（`M-v`）

org バッファで `M-v` を押すと、クリップボードの画像を
`<buffer-file-name>_assets/`（例: `note.org_assets/`）に保存し、
リンクを挿入してその場でプレビューする（`my:org-yank-image`）。

Emacs 30 で MS-Windows も `yank-media` に対応し、org 9.7 以降が
`image/.*` のハンドラを登録しているので、自前で書くのは保存先と
プレビューだけでよい。**外部プロセスは要らない**。

これで用が足りるため、`powershell.exe` から `ms-screenclip:` を起動して
範囲選択させていた `my:org-screenshot` は削除した（Win+Shift+S で撮ってから
`M-v` で貼れば同じことができる）。`etc/screenclip.ps1` はその名残。

| 変数 | 設定値 |
|---|---|
| `org-yank-image-save-method` | `my:org-image-save-directory`。関数を渡せるのは org 9.8 から |
| `org-yank-image-file-name-function` | `my:org-yank-image-filename` |

- ディレクトリは `org--image-yank-media-handler` が `make-directory` で作るので、
  設定側は名前を返すだけでよい
- リンクが相対パスになるのは `org-link-file-path-type` が既定の `adaptive` で、
  保存先がバッファの下位ディレクトリだから
- ドラッグ&ドロップ（`org--dnd-*`）の保存先も同じ変数を見るので一緒に変わる
- 既定の `org-yank-image-autogen-filename` は **マイクロ秒がファイル名に残らない**。
  `clipboard-…T…%6N` とドットで繋ぐため、`file-name-with-extension` が
  それを拡張子とみなして落とす。結果として秒単位の名前になり、同じ秒に
  2 回貼ると 1 枚目が上書きされる。ハイフンで繋ぐ関数に差し替えてある
- `M-v`（`scroll-down-command`）は org バッファでだけ潰れる。スクロールは
  `C-z`（`my-keybind.el`）が使える。`cua-mode` も `M-v` を
  `delete-selection-repeat-replace-region` に割り当てるが、それが載る
  `cua--cua-keys-keymap` は `cua-enable-cua-keys` が nil なら有効にならない。
  **batch では有効に見える**（`cua--select-keymaps` は `pre-command-hook` で
  走るため、`cua-mode` を有効にした時点の値のまま止まる）ので、
  `key-binding` を batch で確認するときは `(cua--select-keymaps)` を先に呼ぶこと

クリップボードに画像が載っているかは batch でも確認できる:

```sh
emacs --batch --eval '(message "%S" (gui-get-selection (quote CLIPBOARD) (quote TARGETS)))'
```

### 保存時の `_assets/` 整合性チェック

org バッファを保存すると（`after-save-hook`）、`_assets/` があるときだけ
バッファ内のリンクと突き合わせる（`my:org-assets-check-on-save`）。

| 状態 | 動作 |
|---|---|
| `_assets/` にあるがリンクされていない | `map-y-or-n-p` で 1 つずつ確認してごみ箱へ（`y`/`n`/`!`/`q`） |
| リンクはあるが `_assets/` に無い | 保存は成功させ、`message` で警告 |

消す側の判断を誤るとファイルが失われるので、安全側に倒してある。

- **必ず `org-with-wide-buffer` で見る。** ナローイングされたバッファで
  `org-element-parse-buffer` を呼ぶと見えている範囲しか解析されず、
  範囲外からリンクされているファイルを消してしまう
- リンク判定は `org-element` だけに頼らず、**ファイル名がバッファ内に文字列と
  して現れるかも見る**（`my:org-assets--mentioned-p`）。`org-element` は
  コメント行や例示ブロックの中のリンクを拾わないため、それだけだと
  「コメントアウトして退避してある画像」を消してしまう
- 削除は `(delete-file f t)` でごみ箱へ送る。誤って消しても戻せるように
- パスの比較は `file-truename` で正規化し、`file-name-case-insensitive-p` が
  真なら `downcase` する（Windows / macOS）
- `directory-files` の MATCH に文字列先頭アンカー（バックスラッシュ +
  バッククォート）入りの正規表現は書かない。エスケープを
  1 つ落としても静かに「1 件も一致しない」になり、**全リンクが「リンク先が
  無い」と誤判定される**。述語で絞るほうが壊れにくい

`org-save-all-org-buffers` は 1 時間ごとのタイマーからも呼ばれる。そのまま
だとタイマーが `y-or-n-p` を出して作業を止めるので、`:around` advice で
`my:org-assets-inhibit-check` を束縛し、その間はチェックごと飛ばす。
手で `M-x org-save-all-org-buffers` したときも同じく黙って保存する。

## dired で外部アプリを起動する

Excel ブック（`.xls` / `.xlsx` / `.xlsm`）は Emacs で読んでも意味が無いので、
バッファに読み込まず OS のファイル関連付けに渡す。

| | |
|---|---|
| 判定 | `my:dired-external-open-regexp` / `my:dired-external-open-p`（`my-dired.el`） |
| 起動 | `my:open-file-externally`（`my-core.el`）。Windows は `w32-shell-execute`、macOS は `open(1)`、他は `xdg-open` |

拡張子を足したいときは `my:dired-external-open-regexp` に加える。dired と
サイドバーで同じ述語を共有しているので両方に効く。

dired 側は `RET` / `f` / `e` を差し替える（3 つとも同じ `dired-find-file`）。
`o`（other-window）と `v`（view）は素のままにしてある。

### サイドバーは `dired-find-file` を通らない

`dired-sidebar` の `RET` は `dired-sidebar-find-file` なので、`dired-mode-map`
の差し替えでは効かない。しかも入口が 3 つある。

| 入口 | コマンド |
|---|---|
| `RET` / `C-m` | `dired-sidebar-find-file` |
| `C-o` | `dired-sidebar-find-file-alt` → `call-interactively` で上を呼ぶ |
| `mouse-2` | `dired-sidebar-mouse-subtree-cycle-or-find-file` → DIR 引数付きで上を呼ぶ |

3 つまとめて押さえるため、キーではなく `dired-sidebar-find-file` への
`:around` advice にしてある。

**`orig` を呼ぶ前に判定すること。** `dired-sidebar-find-file` はファイルに
対して `get-mru-window` / `next-window` で表示先を選び、空いていなければ
`split-window` までする。外部に投げるだけのファイルでウィンドウ分割を
起こしてはいけない。

これは「別の開き方」ではなくウィンドウ管理のラッパで、サイドバーが
dedicated window であることに由来する。ディレクトリならサイドバーの中で
ルートを差し替え（`dired-sidebar-with-no-dedication` + `find-alternate-file`）、
ファイルなら隣のウィンドウを選んでから `find-file` する。

### 【重要】`dired-x` は `dired-mode-map` を無条件で書き換える

`dired-x.el` はロードされた瞬間に、トップレベルの裸の `define-key` で
`dired-mode-map` を書き換える。`defcustom` による切り替えは無い。

```elisp
(define-key dired-mode-map "F" 'dired-do-find-marked-files)
(define-key dired-mode-map "V" 'dired-do-run-mail)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "\C-x\M-o" 'dired-omit-mode)
```

`use-package dired` の `:bind` は dired のロード時に張られるので、あとから
`dired-x` が読まれると **`V` の `dired-vc-status` が奪われる**。

`dired-x` は明示的に require したつもりが無くても読まれる。入口は 2 つあり、
**どちらも `F8` を通る**。

- `dired-sidebar` の `:config` の `(require 'dired-x)`（`dired-omit-mode` のため）
- サイドバーの `a`（`dired-omit-mode` は `dired-x` で唯一の autoload）

つまり「**`F8` を一度でも押すと、そのセッションでは以後 `V` が効かなくなる**」
という壊れ方をしていた。2026-08-30 に neotree を dired-sidebar に置き換えた
ときからの回帰で、2026-09 に気づいた。

`:defer t` + `:config` の `use-package dired-x` で張り直している。
`eval-after-load` はファイルのロード完了後に走るので、`dired-x` 自身の
`define-key` に必ず勝つ。**`:bind` では駄目**で、`dired-x` のロードとは
無関係に張られてしまい上書きを取り返せない。

`dired-mode-map` に置いたキーが効かないときは、**まず `dired-x` を疑う**こと。
奪われるのは上の 5 つと `*(` / `*O` / `*.`。

## dired の自動更新（2026-09-04）

外部でファイルが増減したら dired の一覧も追随する。`dired-mode-hook` から
`auto-revert-mode` を**バッファローカルに**有効にしている
（`my:dired-auto-revert-setup`、`my-dired.el`）。

### 【重要】`global-auto-revert-non-file-buffers` は使わない

あれは `buffer-stale-function` を持つ非ファイルバッファを**一律に**対象に
するので、効き先が dired の外へ広がる。magit の更新は `my-magit-watch` と
`my-gitd` で自前に組んであり、そこに autorevert を並走させたくない。

実測では magit のバッファは `buffer-stale-function` が既定
（`buffer-stale--default-function`）のままで、`auto-revert--global-add-current-buffer`
は独自の stale 関数を要求する（`autorevert.el:561`）ため、あの変数を t に
しても magit は採用されない。**それでも範囲は広げない**（`buffer-menu` は
`auto-revert-interval` = 1 秒ごとに revert されるし、将来 magit 側が
`buffer-stale-function` を持てば黙って挙動が変わる）。

### ポーリングではない

dired 側は受け入れ準備が済んでいる（`dired.el:2906`）。

```elisp
(setq-local buffer-stale-function #'dired-buffer-stale-p)
(setq-local buffer-auto-revert-by-notification t)
```

`auto-revert-handler` は watch がある間、通知で `auto-revert-notify-modified-p`
が立たない限り `dired-buffer-stale-p` すら呼ばない（`autorevert.el:830`）。
`auto-revert-interval` が 1 でも毎秒 `ls` が走るわけではない。

### 拾えるもの・拾えないもの

**メインディレクトリの `created` / `renamed` / `deleted` だけ**が対象
（`autorevert.el:758`）。

| | |
|---|---|
| ファイルの追加・削除・改名 | **拾う** |
| ファイルの中身・サイズ・更新日時の変化 | 拾わない（**サイズ欄は古いまま**） |
| `i` で挿入したサブディレクトリの中の変化 | 拾わない |
| w32notify がバッファ溢れで落としたイベント | 拾えない |

保険として `dired-auto-revert-buffer` を `dired-directory-changed-p` にして
ある。これは auto-revert とは別物で、**既に開いてある dired バッファを訪ね
直したとき**に、変わっていれば revert する。通知に依存しない経路。

### 壊れないことの根拠

- `dired-revert` は**マーク・隠しサブディレクトリ・point とウィンドウ位置を
  復元する**（`dired.el:2232`）
- wdired 中は `buffer-read-only` が nil になり `dired-buffer-stale-p` が nil を
  返すので、編集中に潰されない
- `auto-revert-verbose` は dired だけ `setq-local` で nil にしてある。
  ファイルバッファ側のメッセージは残る
- `dired-sidebar` も dired バッファなので一緒に効くが、**あちらは
  auto-revert 前提の作り**になっている。`revert-buffer-function` をラップして
  窓の位置を保ち、`auto-revert-verbose` を自分で nil にし、
  `dired-sidebar-delay-auto-revert-updates`（既定 t）で 1.5 秒のアイドル待ちに
  間引く
- `diff-hl-dired-mode` が `dired-after-readin-hook` に載っている（`my-vc.el`）
  ので、自動更新のたびに vc 経由の git 呼び出しが増える。watch は非再帰で
  `.git/` の中の変化は届かないため、git の書き込みで更新が誘発される
  ループにはならない

GUI プローブでの実測:

| | |
|---|---|
| 外部で作ったファイルが出る / 消したファイルが消える | **両方 t** |
| マークの維持 | **t**（`*` 1 個が残った） |
| magit バッファの `auto-revert-mode` / `auto-revert--global-mode` | **どちらも nil** |
| `magit-refresh-buffer` | 従来どおり成功 |

## markdown のプレビューと外部エディタ

似ているが**別経路**の 2 つがある。

| | コマンド | 経路 |
|---|---|---|
| ブラウザで HTML を見る | `markdown-preview`（`C-c C-c p`、hydra の `v`） | `markdown-command`（pandoc）で HTML に変換 → `*markdown-output*` → `browse-url-of-buffer` が一時ファイルに書き出して OS 既定ブラウザで開く |
| 外部エディタで開く | `markdown-open`（`C-c C-c o`、hydra の `O`） | `save-buffer` してから `call-process` で `markdown-open-command` に**元の `.md` のパスを渡す**だけ。pandoc も browse-url も通らない |

`.md` そのものを渡したい相手（MarkText、Typora）は後者。**新しいコマンドを
作る必要は無い。**

`markdown-open-command` は MarkText を先頭に置いてある。

```elisp
(or (executable-find "marktext") ...Typora のパス候補...)
;; => "c:/Users/masao/.local/bin/marktext.cmd"
```

- `executable-find` は Windows では `exec-suffixes`（`.exe` `.com` `.bat`
  `.cmd` `.btm`）を補うので、拡張子なしの `"marktext"` で `.cmd` が見つかる。
  `~/.local/bin` は `exec-path` に入っている
- `marktext.cmd` は `start` で起動して即座に戻るため、`markdown-open` の
  同期 `call-process` でも Emacs は固まらない。**戻らないラッパを
  `markdown-open-command` にすると固まる**
- 以前は Typora のインストールパスを `seq-find` で探すだけだったが、この
  マシンに Typora は無いので結果は `nil` で、`markdown-open` は
  `Variable markdown-open-command must be set` で常に失敗していた

### 【重要】`call-process` の引数は cp932 でエンコードすること

`my-japanese.el` は Windows で `default-process-coding-system` を
`(utf-8 . utf-8)` にしている。**`call-process` の引数はこの cdr で
エンコードされる**が、Emacs のプロセス起動は ANSI API なので、送った
UTF-8 のバイト列が受け取り側で cp932 として解釈される。
結果、**日本語を含むパスは存在しないファイル名になる**。

実測（`KOB00100_チェック仕様・メッセージ一覧.md`、GUI・実設定）:

| `default-process-coding-system` | cmd の `if exist %1` |
|---|---|
| `(utf-8 . utf-8)`（設定のまま） | **MISSING** |
| `(utf-8 . cp932)` | EXIST |
| `emacs -Q`（既定） | EXIST |

**この壊れ方は何の手がかりも残さない。** MarkText は受け取ったパスを
`isMarkdownFile`（存在チェックを含む）で黙って捨て、ログにも書かずに
`startUpAction`（`blank`）へフォールバックする。`start` 経由なので終了
コードも必ず 0。つまり「**MarkText は起動するが空白**」だけが見える。

そのため `markdown-open-command` には**文字列ではなく関数**
（`my:markdown-open-external`）を渡している。文字列だと `markdown-open`
自身が `call-process` するので、束縛する隙が無い。関数の中で
`grep`（`my-utils.el`）や `org-pandoc`（`my-text.el`）と同じく cdr だけ
`locale-coding-system` に戻している。

なお Windows の `my:open-file-externally`（`my-core.el`）は
`w32-shell-execute`（ワイド API）なのでこの問題は無い。

2026-09-04 に `default-process-coding-system` そのものを
`(utf-8 . cp932)` に直したので、`M-x compile` / `shell-command` /
ripgrep など `user-lisp/` の外を通る経路も含めて一掃してある（次節）。
`my:markdown-open-external` の束縛は冗長になったが、macOS / Linux では
`locale-coding-system` が utf-8 で no-op になるため、そのまま残してある。

### `prefer-coding-system` が `default-process-coding-system` を上書きする

**`(setq default-process-coding-system '(utf-8 . utf-8))` は GUI では
2015 年からずっと無意味だった。**

`my-japanese.el` の `*encoding` 相当のブロックでこれを設定しても、後続の
w32 ブロックにある `(prefer-coding-system 'utf-8-unix)` が
`set-default-coding-systems` 経由で `default-process-coding-system` を
`(CODING . CODING)` に書き戻す。実測:

```elisp
(setq default-process-coding-system '(utf-8 . cp932))  ; => (utf-8 . cp932)
(prefer-coding-system 'utf-8-unix)                     ; => (utf-8 . utf-8)
```

`default-file-name-coding-system` が `set-file-name-coding-system 'cp932` で
打ち消されているのとまったく同じ構図で、こちらは打ち消しが無かった。
そのため **cdr を変えるには `prefer-coding-system` より後で入れ直す**
必要がある。

なお w32 ブロックは `:if (eq window-system 'w32)` なので **batch では走らない**。
batch での最終値は前段の `setq` が決める。両方に置いてあるのはそのため。

### `(utf-8 . cp932)` に変えたときの影響（2026-09-04 に GUI で実測）

| 観点 | 変更前 `(utf-8 . utf-8)` | 変更後 `(utf-8 . cp932)` |
|---|---|---|
| 引数（日本語パス）を `if exist` で確認 | **MISSING** | **EXIST** |
| 出力の復号（日本語のコミット件名） | OK | OK |
| 標準入力（`call-process-region` → `git hash-object`） | utf-8 | **cp932 に変わる** |
| pandoc（`markdown-preview`） | OK | OK |
| `shell-command-on-region`（往復） | OK | OK |
| magit | OK | OK |
| `markdown-open`（MarkText） | NG | OK |

cdr は**引数と標準入力の両方**を兼ねるので、標準入力に UTF-8 を要求する
相手には `process-coding-system-alist` で個別に指定する。現状は pandoc
（`markdown-preview` と `org-pandoc` がバッファを `call-process-region` で
流し込む）だけ。

magit は `magit-process-git-arguments` が引数を自分で cp932 に
`encode-coding-string` し（unibyte 文字列になるので二重エンコードは
起きない）、標準入力も `magit-run-git-with-input` が自分で utf-8 に
`encode-coding-region` するので、どちらの設定でも影響を受けない
（magit issue #3250）。**magit だけが壊れていなかったのはこれが理由。**

### シェル経由の経路は `process-coding-system-alist` が優先される

`M-x grep` と `my:ripgrep-regexp` は `compilation-start` 経由で
`shell-file-name`（Git の `bash.exe`）に `-c "コマンド行"` を渡す。
ここは **`default-process-coding-system` を直しても効かない。**
`my-shell.el` の

```elisp
(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
```

が `process-coding-system-alist` に載り、そちらが優先されて car / cdr とも
utf-8 に固定されるため。実測（`検索対象キーワード` を bash に渡して
`od` で見る）:

| | 届いたバイト |
|---|---|
| 期待（UTF-8） | `e6 a4 9c e7 b4 a2 …` |
| そのまま | `e8 ae 80 e6 87 83 ef bd b4 …`（UTF-8 を cp932 と解釈した化け） |
| `coding-system-for-write` = cp932 | **一致** |
| alist を `(utf-8 . cp932)` に差し替え | **一致** |

**alist は書き換えていない。** cdr はコマンド行と標準入力を兼ねるので、
alist を `(utf-8 . cp932)` にすると `M-!` / `M-|`
（`shell-command-on-region`）や `M-x shell` の標準入力まで cp932 になる。
代わりに、**標準入力を使わない grep / ripgrep の側だけ**
`coding-system-for-write` を `locale-coding-system` に束縛した
（`my:grep-with-cp932` と `my:ripgrep-with-cp932`）。`coding-system-for-write`
は alist より強い。非 Windows では `locale-coding-system` が utf-8 なので
no-op になる。

実測（`grep検証/日本語ファイル.md` に日本語の行を置いて検索）:

| | 変更前 | 変更後 |
|---|---|---|
| `M-x grep` で日本語を検索 | **一致なし** | **ヒット** |
| dired から `my:ripgrep-regexp` | **exit code 1** | **ヒット** |
| `M-|`（`shell-command-on-region`）の往復 | OK | OK |

**日本語の検索語で「一致なし」になったら、まずこれを疑うこと。**
grep も rg もエラーを出さず、ただ 0 件を返す。

#### 【重要】束縛はコマンドではなく `ripgrep-regexp` に張る

当初 `my:ripgrep-regexp`（dired の `G`）が本家 `ripgrep-regexp` を
**コピーして**その中でだけ束縛していたため、本家を呼ぶ経路が漏れていた。

| 入口 | 通る関数 | 2026-09-04 まで |
|---|---|---|
| dired の `G` | `my:ripgrep-regexp`（コピー） | 束縛あり → **ヒット** |
| `C-c p s`（`my:projectile-search-dwim`） | `projectile-ripgrep` → **本家** `ripgrep-regexp` | 束縛なし → **0 件** |
| `M-x ripgrep-regexp` | 同上 | 同上 |

`ripgrep-regexp` に `:around` advice（`my:ripgrep-with-cp932`）を張る形に
変え、`my:ripgrep-regexp` は `(dired-current-directory)` を渡すだけの薄い
ラッパにした。`ripgrep-regexp` は autoload なので、定義前に advice を
張っておけば `ripgrep.el` のロード時に引き継がれる（実測で確認）。

**この検証は batch ではできない。** PowerShell 7 から `--batch` で起動すると
`locale-coding-system` が **`cp65001`**（コンソールのコードページ）になり、
束縛が no-op になって修正前後の区別がつかない（4 通り試して全部 0 件になった）。
GUI では `cp932` で、ANSI コードページと一致する。gitd の
`my:gitd-ansi-coding` が `locale-coding-system` を避けているのと同じ話。

GUI プローブでの実測（`~/.emacs.d` で `プロキシ` を検索）:

| | |
|---|---|
| 修正後 `ripgrep-regexp` / `projectile-ripgrep` / `my:ripgrep-regexp` | **3 つともヒット** |
| advice を外した `projectile-ripgrep` | **0 件**（報告された症状を再現） |

## magit の高速化 (`gitd/` + `my-gitd.el`)

magit のリフレッシュが遅い原因は **git ではなく Emacs のプロセス生成コスト**。
`user-lisp/my-gitd.el` が `magit-process-file` に `:around` を張り、
Rust の常駐プロセス（`gitd/`）に git の実行を肩代わりさせる。

計画と実測は `docs/magit/magit-auto-refresh-plan.md` と `docs/magit/magit-gitd-2a-design.md`。

### 遅さの原因（2026-09 実測）

**同じ `cmd.exe` を起動するのに PowerShell が約 20 ms、Emacs の
`call-process` は 59〜76 ms**（3 回反復して再現を確認）。約 40 ms が
Emacs 側のプロセス生成経路のコストで、git にも Defender にも由来しない。

| | |
|---|---|
| `magit-refresh-buffer` 1 回 | 1669 ms / **git 呼び出し 29 回** |
| → 1 回あたり | 56〜58 ms |

**時間は呼び出し回数に完全に線形。** リポジトリの規模にほぼ依存しない固定コスト。

効かなかった対策（試して確認済み。もう一度試さないこと）:

- Defender 除外 — git 固有のコストではないので効かない
- `core.fsmonitor` — 走査時間は減るがプロセス生成コストは変わらない
- `cmd/git.exe` ラッパの回避 — **magit は既に回避済み**。
  `magit-git-executable` の defcustom が Windows では cygpath 経由で
  `mingw64/libexec/git-core/git.exe` を解決する。PowerShell では 47→39 ms と
  効くが、Emacs の 55 ms に埋もれて有意差なし
- `magit-status-sections-hook` の削減 — 16→6 で 1669→1001 ms。表示を
  犠牲にする割に効かない

### 効果

`magit-refresh-buffer` 1 回（`~/.emacs.d`、28 コマンド）:

| | 時間 | Emacs からの git 起動 | デーモンでの git 起動 |
|---|---|---|---|
| デーモン無効 | 1503 / 1544 ms | 28 | — |
| 段階 2a（素通し） | 683 / 672 ms | 0 | 28 |
| **段階 2b（キャッシュ）** | **56 / 54 ms** | 0 | **0** |
| 段階 2b（0.3 秒前に先読み） | 72 / 54 ms | 0 | 0 |
| 段階 2b（直前に先読み = `g`） | 208 ms | 0 | 28（並列） |

**1.7 秒が 50〜70 ms になった。** GUI の実地計測（外部でファイルを変更してから
自動更新が終わるまで）でも 51〜81 ms。

Rust の `Command` からの spawn は 28.9 ms（`git status -z --porcelain`）で
Emacs の約半分。stdio の往復は **0.13 ms**、28 回でも 4 ms なので、
残る 50 ms はほぼ magit 自身の Elisp（セクションの構築と描画）。
**ここから先はデーモンでは縮まない。**

### ビルド

`tree-sitter/` の文法と同じ扱い。**ソースは git 管理下、`gitd/target/` は
`.gitignore`** して各マシンで作る。

```
M-x my:gitd-build     ; cargo build --release
M-x my:gitd-stats     ; 経由回数 / フォールバック数 / 累計短縮時間
M-x my:gitd-restart   ; サーキットブレーカが落ちたときの復帰
(setq my:gitd-verify t)  ; シャドウモード (下記)
```

**バイナリが無ければ `my:gitd-mode` は何もしない**ので、まだビルドしていない
マシンでは自動的に従来動作になる。対象は Windows のみ。

### 文字コードの地雷（3 つとも実際に踏んだ）

`my:gitd--to-text` が処理する。**`args` / `cwd` / `program` / `env` の
全部に適用すること。**

1. **`process-environment` に JSON に載らない項目がある。**
   `PSModulePath` が OneDrive の「ドキュメント」を ANSI の生バイトのまま
   含んでおり `json-serialize` が `wrong-type-argument json-value-p` で落ちる。
   **PowerShell から Emacs を起動したときだけ再現する**（bash 経由では出ない）
2. **復号に `locale-coding-system` を使うと直らない。**
   あれは**コンソールの**コードページで、PowerShell 7 では `cp65001`（UTF-8）。
   環境変数ブロックは **ANSI コードページ**（`w32-ansi-code-page` = 932）で別物。
   UTF-8 として復号すると生バイトが eight-bit 文字のまま残り、やはり載らない
3. **引数も ANSI に encode されている。**
   `magit-process-git-arguments` が意図的にやっている（Emacs の `call-process`
   が ANSI API を使うため。magit issue #3250）。デーモン境界で復号し直す。
   Rust はワイド API で起動するので、cp932 に無い文字ではむしろ改善になる

### `magit-process-file` を横取りするときの注意

同期読み取りは全部この関数を通るので差し込みは 1 箇所で足りる。ただし:

- **`BUFFER` に整数（`0`）が来る。** `magit-run-gitk` が使う
  「非同期・出力破棄」の意味。同期実行すると **gitk のウィンドウを閉じるまで
  Emacs が固まる**。必ず弾くこと
- `magit-run-gitk*` は `magit-gitk-executable`、`magit-patch-id` は
  `shell-file-name` を渡してくる。`(equal program (magit-git-executable))` で弾く
- 実際に来る `BUFFER` は `nil` / `(t nil)` / `(t "FILE")` / バッファ の 4 形態。
  それ以外は素通し（default deny）
- デコードは `(car (magit--process-coding-system))`（実測で `utf-8-unix`）。
  **値を決め打ちせず必ずこの関数から取る**
- `magit-run-git-with-input` は `call-process-region` を使うので通らない。
  `magit-start-process`（非同期）も無関係

### 安全側の作り

- **タイムアウトを設けない。** 素の `process-file` にも無いので、挙動を
  変えないことが最も安全。`jsonrpc-request` は `:timeout nil` でタイマーが
  完全に無効になり、待ちは `accept-process-output` なので `C-g` で抜けられる
- **フォールバック。** バイナリが無い / デーモンが死んだ / 形態が未知なら
  黙って素の `process-file` に戻る。3 回続けて失敗したらそのセッションでは使わない
- **二重実行の防止。** デーモンが応答前に死ぬと git が既に走ったかは分からない。
  読み取り専用なら再実行してよいが、それ以外は再実行せずエラーを返す
  （`git add` を 2 回走らせない）

### 検証はシャドウモードで

`(setq my:gitd-verify t)` にすると、読み取り専用コマンドを**デーモン経由と
素の `process-file` の両方で実行してバイト単位で比較**する。差異は
`*gitd verify*` に記録される。この設計で唯一こわいのは「静かに壊れる」ことなので、
**壊れていないことを実使用で証明する**のがこの機能の役目。遅くなるので常用はしない。

## デーモン側のキャッシュ（段階 2b）

設計と実測は `docs/magit/magit-gitd-2b-design.md`。

### 無効化を「通知」ではなく「トークン」でやる

**古い答えを返すキャッシュは静かに壊れる。** magit が事実と違う内容を表示し、
ユーザはそれに気づけない。そこで無効化通知は**作らなかった**。

`git/run` には毎回 `repo`（監視中のリポジトリのルート）と `token`
（そのリポジトリ状態の通し番号）を載せる。デーモンは
`(repo, token, コマンド)` でキャッシュし、token が違えば問答無用でミスにする。

こうすると正しさの条件が「Emacs が変化を漏れなく通知すること」から
**「トークンが古い状態を指し続けないこと」**に変わる。前者は通知を 1 つ
落とすとそのリポジトリが**永久に**古いままになるが、後者は Emacs 側だけで
閉じており、`my-magit-watch` が既に持っている情報で満たせる。

トークンを進めるのは 3 か所（`my-magit-watch.el`）:

| いつ | 何のため |
|---|---|
| 分類を通ったイベント（`suspect` を除く） | 外部からの変更 |
| `magit-pre-refresh-hook` | magit 自身の書き込みと、ユーザの `g` |
| デーモン経由で書き込みコマンドが走ったとき | 上を待たずに進める |

2 番目が要なのは、`magit-run-git-with-input`（`call-process-region`）と
`magit-start-process`（非同期）が**デーモンを通らない**ため。magit は
コマンドの後に必ず `magit-refresh` を呼ぶのでここで捕まる。
**`g` が必ず本当のことを言う**のもこれで保証される。

**監視が動いていなければ `repo` も `token` も付かず、キャッシュも先読みも
行われない。** `M-x my:magit-watch-mode` で切れば段階 2a と同じ動作に戻る。
キャッシュの寿命が監視の寿命に従属しているのが最大の安全弁。

### 先読みは 2 本目のタイマーで頼む

`my-magit-watch` のタイマーは 2 本ある。どちらもイベントごとに張り直す。

```
       イベント群 ......|
                        |--0.1s--> repo/prewarm を送る
                        |------------0.4s------> magit-refresh-buffer
```

差の 0.3 秒が先読みの持ち時間。デーモンは**直前のリフレッシュで実際に来た
コマンド列を覚えていて**（magit の内部を知る必要がない）、それを 8 並列で
走らせる。0.1 秒待つのは、1 ファイルの保存で w32notify が約 10 件の
イベントを出すため（最初の 1 件で頼むと残り 9 件でトークンが進んで無駄になる）。

同じコマンドが二重に起動しないよう single-flight にしてあるので、先読みが
間に合わなくても損はしない。`g` を押した瞬間に頼んでも、magit の要求は
走っている先読みに**合流**するので、直列 28 回ではなく並列 1 回ぶんで済む。

### `update-index --refresh` は先読みの先頭で走らせる（prelude）

`magit-status-refresh-buffer` は**先頭で** `update-index --refresh` を呼ぶ。
これを飛ばして先読みすると、`diff-files` が「stat が古いだけ」のファイルを
変更ありと報告し、その答えがキャッシュに残る。実測:

```
内容を変えずに書き直したあと
  update-index --refresh 無し → diff-files が 3 ファイルを M と報告
  update-index --refresh 後   → diff-files は何も報告しない
```

`git diff`（磁器）は自分で内容を比較するので影響を受けないが、
magit の `magit-unstaged-files` は `diff-files` を使う。

そこで `role: "prelude"` を作り、Emacs が明示したコマンドだけを先読みの
先頭で直列に走らせる。対象は `update-index --refresh` **ただ 1 つ**。
デーモンは相変わらず git の意味を知らない。

### 【重要】読み取りだけの git もファイル変更イベントを出す

これを見落として、最初の GUI 検証で**自動リフレッシュが 1 回も走らなかった**
（先読み 63 回・リフレッシュ 0 回）。実測:

| コマンド | イベント | 内訳 |
|---|---|---|
| `update-index --refresh`（何もしない場合でも） | 3 | `.git` ×1 / `.git\index.lock` ×2 |
| `status --porcelain` | 4 | `.git` ×2 / `.git\index.lock` ×2 |
| `diff-files -z --name-only` | 1 | `.git` ×1 |
| `rev-parse` / `for-each-ref` | 0 | — |

`status` ですら index.lock を作る。だから先読み（28 コマンド）は必ず
イベントを出し、それがデバウンスを張り直し、0.1 秒後にまた先読みが走る。
**0.4 秒の静けさは永久に来ない。**

分類上これらはすべて `suspect` なので、そこを狙い撃ちして 3 つ直した。

1. **`suspect` ではトークンを進めない**（進めると自分のリフレッシュや
   先読みが自分のキャッシュを壊す）。代わりに `my:magit-watch--fire` が
   フィンガープリントの不一致を見つけたときに進める
2. **`suspect` では既に張ってあるタイマーを延長しない**
3. 1 つの窓では先読みを 1 回だけ頼む。さらに**デーモン側でも、レシピが
   全部キャッシュ済みなら先読みごと打ち切る**（無いと prelude だけが
   毎回走ってイベントを出し続ける）

### `magit-process-record-invocations` は素通しにする

magit の呼び出しログは `magit-process-file` の**本体**にあるので、
`:around` で `orig` を呼ばずに済ませると記録されない。有効なときは
ルーティングしないようにしてある。

同じ理由で **`magit-process-file` に後から足した advice も呼ばれない**。
テストを書くときは `my:gitd-mode` を有効にした**後**に足すこと。

### `default-directory` は必ず `expand-file-name` する

Emacs は file バッファの `default-directory` を `~/...` に略記することがある。
`call-process` は内部で展開するが **Rust の `current_dir` は `~` を展開しない**。
そのまま渡すと `ディレクトリ名が無効です (os error 267)` になる。
段階 2a から入っていたバグで、magit のバッファからしか呼ばれていなかったので
表に出ていなかった。

### 次の段階

**段階 2c で監視を常駐プロセスに移す**（今回は見送った）。速度の目標は
キャッシュと並列化だけで達成できており、`check-ignore` は段階 1 で
キャッシュ済みで定常状態では 0 回しか呼ばれないので、`ignore` crate に
置き換えても速くはならない。移す価値があるのは別の 2 点:

- macOS / Linux 対応（`subtree` 相当が inotify / kqueue に無い）
- `ReadDirectoryChangesW` のバッファ溢れを**検知**できるようになる。
  Win32 API は溢れを通知するが Emacs の `w32notify` はそれを渡さない。
  検知できればトークンを強制的に進められ、「イベントが落ちるとキャッシュが
  古いまま」という唯一の穴が塞がる

## magit の自動更新 (`my-magit-watch.el`)

ワークツリー / インデックス / HEAD の変化を検知して、表示中の magit バッファを
`magit-refresh-buffer` する。Windows のみ、**既定で有効**。
切るときは `M-x my:magit-watch-mode`、様子を見るときは `M-x my:magit-watch-stats`。

段階 2a でリフレッシュが 0.6 秒になったので実用に耐えるようになった。
**2a 無しではこれは入れられなかった**（1.7 秒の固まりが頻発する）。
段階 2b のキャッシュで 50〜70 ms になっている。

設計と実測は `docs/magit/magit-autorefresh-stage1-design.md`。
gitd のキャッシュにトークンを供給する役目も負っている（前節）。

### `w32notify-add-watch` を直接呼ぶこと

`subtree` フラグを渡すと **1 個の watch で配下を再帰的に監視できる**
（追加コスト 0.2 ms、watch 後に作ったディレクトリも届く）。

**`filenotify.el` の `file-notify-add-watch` は `subtree` を渡さない**
（`file-notify--add-watch-w32notify` が `file-name` / `directory-name` /
`size` / `last-write-time` しか組み立てない）ので、汎用 API 経由では非再帰。

### 【重要】batch では検証できない

**w32notify のイベントはコマンドループ経由で配送されるため、`--batch` では
1 件も届かない。** `accept-process-output` や `sit-for` を回しても駄目。
最初 batch で測って全部 0 件になった。

テストは GUI で書く。`emacs -Q -l probe.el` で結果をファイルに書いて
`kill-emacs` する形にしてある。

### 自励振動と二重リフレッシュ

`magit-refresh-buffer` を 1 回走らせるだけで **毎回きっちり 7 件**の
イベントが出る（`.git/index.lock` が 4 件、`.git` ディレクトリ自身が 3 件）。
素直に繋ぐと「イベント → リフレッシュ → イベント」で回り続ける。
さらに magit で stage すると `.git/index` が書かれるが、magit は自分で
リフレッシュ済みなので監視側がもう 1 回走る。

**時刻では区別できない。** イベントは遅れて届くので、magit 自身の書き込みか
外部の変更かを到着時刻から判断することはできない。**内容で見る。**

`.git/index` と `.git/HEAD` の `(mtime . size)` をフィンガープリントとし、
**`magit-refresh-buffer-hook` で毎回取り直す**。このフックは
**自分のリフレッシュでも magit 自身のリフレッシュでも走る**のが肝。

| | |
|---|---|
| magit の stage | git が index を書く → magit がリフレッシュ → そこでスナップショット → あとから届くイベントは必ず一致 → **抑止** |
| 外部の `git add` | スナップショットは前のまま → 一致しない → **リフレッシュ** |

`stat` を 2 回するだけで git は呼ばない。

### イベントは欠落する

1000 ファイル作成に対しイベントは **4095 件**しか届かなかった
（1 ファイル 10 件出るので 1 万件が期待値）。`ReadDirectoryChangesW` の
バッファ溢れで避けられない。**イベントの完全性に依存した設計にはできない。**
差分更新（「このファイルだけ再描画」）のような最適化はやらないこと。

対策として分類に `suspect` を設けた。`.git` ディレクトリ自身や
`.git/**/*.lock` は**それ自体は何も証明しないが「何かは起きた」合図**なので、
拾ってフィンガープリントで判断する。決め手のイベントが落ちても
粗い `.git` の mtime 更新は残りやすい。

### `.gitignore` の判定

監視は `.gitignore` を知らないので、`build/` に 200 ファイル作ると分類後でも
1001 件残る。パスだけのフィルタでは落とせないので git に聞くしかないが、
**イベントごとに聞いてはいけない**。3 段構えで濃縮している。

1. コールバックでは**変化したディレクトリ**をハッシュに入れるだけ
   （ビルドは数千ファイルを出すが**ディレクトリは数個**）
2. デバウンス後に、未知のディレクトリだけを `check-ignore` へ**まとめて 1 回**
3. 結果をキャッシュ。**定常状態では git を 1 回も呼ばない**

実測: `build/out` に 100 ファイルを 3 回書いて、リフレッシュ 0 回、
`check-ignore` は 1 回目だけ。合計 4126 イベントに対しリフレッシュは 7 回。

#### `check-ignore` の呼び方（2 回はまった）

**`magit-git-global-arguments` をそのまま使ってはいけない。**

| 書き方 | 何が起きるか |
|---|---|
| `check-ignore -z -- PATH` | `fatal: -z only makes sense with --stdin` |
| `--literal-pathspecs` 付き | `fatal: pathspec magic not supported by this command: 'literal'` |

どちらも `ignore-errors` で握り潰すと **「何も無視されない」= 安全側に倒れる**ため、
**動いているように見えて 1 件も効いていない**という形で表面化する。

```elisp
(let ((magit-git-global-arguments '("--no-pager" "-c" "core.quotePath=false")))
  (magit-process-git t (list "check-ignore" "--" paths)))
```

`core.quotePath=false` は日本語パスが C 形式でクォートされて突き合わせに
失敗するのを防ぐため。終了コードは 0（該当あり）/ 1（該当なし）/
128 以上（エラー）で、128 以上は 1 度だけ `message` で知らせる。

### 抑止条件に入れてよいもの・いけないもの

**ユーザが操作をやめれば自然に解消するものだけ**を入れる
（ミニバッファ・transient・isearch・キーボードマクロ・リージョン・
`input-pending-p`）。

**`frame-focus-state` を入れてはいけない。** フォーカスが外れている間は
永久に偽のままなので待ち直しが終わらず、**フォーカスを失った時点から
二度と更新されなくなる**（実測で 0.3 秒ごとに再アームし続けた）。
背景の CPU は `my:magit-watch-visible-only` とレート制限で抑える。

### `.lock` の除外は `.git/` 配下に限ること

`index.lock` を落とすために `.lock` で除外したくなるが、ワークツリーには
`Cargo.lock` や `flake.lock` といった**追跡対象のファイル**がある。

### テストを書くときの注意

このマシンは `init.defaultBranch = main`。テスト用リポジトリで
`git checkout master` は失敗する。`-q` で握り潰すと「イベントが来ない」と
誤診する（実際に 1 度誤診した）。`git init -b main` と明示すること。

## モードライン (doom-modeline)

背景色は **modus のパレット上書き**で指定する。Emacs 29 以降 `mode-line` とは
別に `mode-line-active` があり、テーマはそちらを塗るため、`custom-face` で
`mode-line` だけ変えても効かない。

```elisp
(modus-themes-common-palette-overrides
 '((bg-mode-line-active "medium blue")
   (fg-mode-line-active "snow")
   (border-mode-line-active "medium blue")))
```

左端のバー (`doom-modeline-bar`) だけはテーマのアクセント色なので
`custom-set-faces` で別途揃える（`:custom-face` はテーマに負けるので使わない）。`mode-line-inactive` はテーマのまま（灰色）にして、
どのウィンドウが選択中か分かるようにしてある。

**セグメント名はバージョンで変わる。** 4.x で `checker` は `check` に改名された。
古い名前が残っていると `doom-modeline--prepare-segments` が
`"checker is not a defined segment"` で落ち、**モードライン自体が有効にならない**。
利用できるセグメントは `doom-modeline-segments.el` の
`doom-modeline-def-segment` を grep すれば分かる。

## lexical-binding

`early-init.el` / `init.el` / `user-lisp/` すべて `t`。新しいモジュールも `t` で書く。

バイトコンパイルはしない方針（前述）なので、lexical 化の検証は
**一時ディレクトリにコピーしてコンパイルし、`*Compile-Log*` を読む**
という手順で行う。GUI 起動して全パッケージがロードされた状態でやらないと、
パッケージ由来のマクロが未定義で偽の警告が大量に出る。

`reference to free variable` / `assignment to free variable` の大半は
「そのパッケージがコンパイル時に未ロード」というだけで実害はない
（実行時には `defvar` 済みなので special 変数として扱われる）。
注意すべきは `Unused lexical variable` と、
呼び出し元の `let` 束縛を読んでいたクロージャがある場合。

## 設定変更の反映方法

1. `user-lisp/` 配下の該当モジュールを編集する
2. Emacs を再起動する（起動時に自動でバイトコンパイルされる）か、
   編集した式を `C-M-x` で評価する

## 検証方法

GUI 依存の設定（フォント、doom-modeline、IME）は batch では評価されないため、
最終確認は GUI 起動で行うこと。batch での確認は以下：

```sh
emacs --batch --debug-init -l early-init.el -l init.el --eval '(message "OK")'
```

**注意**: batch 実行でも `recentf` と `history`（savehist）は書き換えられる。
検証前にバックアップし、終了後に戻すこと。

## 既知の課題（未対応）

新しく気づいたことはこの節に追記する。

### `my-gitd` 経由だと `C-g` で git が止まらない（2026-09、優先度低）

素の `call-process` は `C-g` で子プロセスを kill するが、デーモン経由では
git が走り切る。書き込みの途中で `C-g` すると「中断したのに実行されている」
ことになる。対処するなら `$/cancel` 通知を足してデーモン側で子を kill する。

半端に kill された `.git/index` より安全とも言えるので、優先度は低いと判断した。

### 自動更新では diff-hl が更新されない（2026-09、仕様）

`my-magit-watch` は `magit-refresh-buffer`（そのバッファだけ）を呼んでおり、
`magit-post-refresh-hook`（diff-hl がぶら下がっている）は走らない。
自動更新のたびに全バッファの差分を取り直すのは重いのでこうしてある。
fringe のマーカーを最新にしたいときは手で `g` を押す。

### 自動更新の `.gitignore` 判定はディレクトリ単位（2026-09、仕様）

追跡対象のディレクトリの中にある無視されるファイル（`src/` の中の `*.log`
など）は落とせず、リフレッシュが走る。`check-ignore` をファイル単位で
呼べば正確になるが、ビルド中のカーディナリティが跳ね上がるので採らない。

### `my-gitd` の書き込み経路の検証は一部だけ（2026-09）

シャドウモード（両方実行してバイト比較）は読み取り専用コマンドにしか使えない。
書き込みを 2 回走らせるわけにはいかないため。

GUI プローブで stage / unstage / commit は自動検証している（段階 2b で追加）。
discard / rebase / merge / cherry-pick、コンフリクト中の操作、サブモジュールは
まだ実際に操作して確かめるしかない。

### イベントが落ちるとキャッシュが古いままになりうる（2026-09、優先度低）

`w32notify` はバッファ溢れでイベントを落とす（段階 1 の実測で 1000 ファイルに
対し 4095/10000）。落ちるとトークンが進まず、デーモンのキャッシュが古いままになる。

保険は 2 つある。

- 決め手のイベントが落ちても `.git` の粗い mtime 更新（`suspect`）は残りやすく、
  そのときはフィンガープリントの不一致でトークンを進める
- **`g` を押せば必ず進む**（`magit-pre-refresh-hook`）。ユーザ側の逃げ道が常にある

根本的に塞ぐには、`ReadDirectoryChangesW` の溢れ通知を受け取る必要がある
（Emacs の `w32notify` は渡してくれない）。段階 2c で監視を常駐プロセスに
移すときの動機のひとつ。

### Emacs の `call-process` が Windows で遅い（2026-09、未調査）

同じ `cmd.exe` を起動するのに PowerShell が約 20 ms、Emacs は 59〜76 ms。
原因は未調査。`my-gitd` はこれを迂回するだけで、直してはいない。
magit 以外（`vc` / `grep` / `projectile`）にも効いているはずなので、
原因が分かれば影響範囲は広い。ただし Emacs 本体の問題である可能性が高く、
手元で解消できる見込みは薄いと考えている。
