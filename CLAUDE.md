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
  3. leaf + leaf-keywords の初期化
  4. `site-lisp/` を `load-path` に追加
  5. `(prepare-user-lisp ...)`
  6. `custom.el` の読み込み
  7. `user-lisp/` 各モジュールの `require`（順序は分割前の記述順のまま）
- `user-lisp/` — 設定本体。20 モジュールに分割（下記）
- `custom.el` — `customize` が自動生成するファイル
- `site-lisp/` — パッケージマネージャで入手できないローカルベンダの Emacs Lisp
- `docs/archive-init.org` — Org 方式だった頃の設定（履歴として保存）
- `docs/extract.el`, `docs/verify.el`, `docs/split.py`, `docs/verify-split.el` —
  Org からの抽出・分割に使った検証スクリプト（等価性の証跡）

## `user-lisp/` の扱い（重要）

Emacs 31.1 の `user-lisp/` は、既定では `package-activate-all` の直後・
`init.el` の読み込み**前**に `prepare-user-lisp` が走り、配下を再帰的に
バイトコンパイルして autoload を生成し `load-path` に追加する。

しかしその時点では straight.el のブートストラップが済んでおらず `leaf` マクロが
未定義のため、leaf を使ったモジュールが関数呼び出しとしてコンパイルされ、
壊れた `.elc` が生成される。

そのため以下のようにしている：

- `early-init.el` で `user-lisp-auto-scrape` を `nil` にして自動実行を止める
- `init.el` で straight/leaf を用意したあと `(prepare-user-lisp ...)` を明示的に呼ぶ
- **バイトコンパイルはしない**（`prepare-user-lisp` の第 1 引数 JUST-ACTIVATE を `t`）。
  コンパイルすると、パッケージ由来のマクロを leaf の `:config` で使っている箇所が壊れる。
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
| `my-core` | 汎用ヘルパ（`my:pandoc-data-file` など）、`s` |
| `my-japanese` | 文字コード、cp932/UTF-8 変換テーブル、Windows IME（tr-ime）、migemo |
| `my-appearance` | フォント、フレーム、modus-vivendi テーマ、doom-modeline、all-the-icons |
| `my-completion` | vertico、consult、marginalia、orderless、corfu、cape |
| `my-keybind` | グローバルキーバインド（`C-h` → `delete-backward-char`、`C-z` → `scroll-down`） |
| `my-editor` | hydra、symbol-overlay、smartparens、whitespace、yasnippet、recentf ほか |
| `my-dired` | dired、hydra-dired、neotree |
| `my-text` | org-mode、ox-pandoc、markdown、rst、adoc |
| `my-lang-lisp` | Emacs Lisp、Clojure（cider）、Common Lisp（slime） |
| `my-lang-python` | Python（python-ts-mode、pyvenv、py-isort、blacken） |
| `my-lang-web` | PHP、JavaScript / TypeScript（js-ts-mode / typescript-ts-mode、web-mode、scss） |
| `my-lang-native` | Rust、C++、C# |
| `my-lang-misc` | SQL、bat、Swift、Lua、VisualBasic |
| `my-lsp` | eglot（組み込み、プレフィックス: `C-c l`）、flymake（`C-c !`） |
| `my-fileformat` | yaml、diff、log4j、Dockerfile、vimrc、mayu |
| `my-project` | projectile（プレフィックス: `C-c p`） |
| `my-vc` | magit、git-gutter（プレフィックス: `C-c g`）、Windows の SVN 対応 |
| `my-shell` | exec-path-from-shell、Windows 用 shell 設定 |
| `my-utils` | calendar、open-junk-file、grep/ripgrep、blog 用ヘルパ |
| `my-platform` | Windows / macOS 固有設定 |

## LSP サーバ

eglot が使う言語サーバは自分で入れる。2026-08 時点の導入状況:

| 言語 | サーバ | 入れ方 |
|---|---|---|
| TypeScript / JS | typescript-language-server 5.3.0 + **typescript 5.9.3** | `npm i -g typescript@5 typescript-language-server` |
| PHP | intelephense 1.18.5 | `npm i -g intelephense` |
| bash | bash-language-server 5.6.0 | `npm i -g bash-language-server` |
| Rust | rust-analyzer 1.97.1 | `rustup component add rust-analyzer` |
| Python | 未導入 | `uv tool install basedpyright` |

### TypeScript は 5.x に固定すること

**`npm i -g typescript` で入る 7.x（Go 実装のネイティブ版）は使えない。**
7.x には `lib/tsserver.js` が無く、typescript-language-server が
`Could not find a valid TypeScript installation` で初期化に失敗する。
`npm i -g typescript@5` を使う。

### npm グローバルは nvm のバージョンに紐づく

prefix は `~/scoop/apps/nvm/current/nodejs/nodejs`。**nvm で Node を切り替えると
グローバルパッケージも切り替わる**ので、切り替えたら入れ直しが要る。
プロジェクトローカル（`npm i -D`）に寄せると安定する。`add-node-modules-path` が
`node_modules/.bin` を `exec-path` に足すので、ローカル版が優先される。

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

- **`my:treesit-remap` は必ずトップレベルで呼ぶこと**。leaf の `:config` は
  `(eval-after-load '<leaf名>)` に包まれるので、そこで差し替えても
  「その回に開いたバッファ」には間に合わない。さらに差し替えが効くと
  従来のモードはもうロードされないため、`:config` は二度と実行されない
- `.tsx` の `auto-mode-alist` 登録は **web-mode の leaf より後**に置くこと。
  `:mode` が先頭に積むので、前に置くと web-mode に負ける

導入済みの文法（`tree-sitter/`、git 管理外）:
bash / c-sharp / css / dockerfile / html / javascript / jsdoc / json /
python / rust / toml / tsx / typescript / yaml の 14 個。
`jsdoc` は `js-ts-mode` がコメント解析に `treesit-ensure-installed` するので必要。

コンパイラは scoop の `gcc`（mingw-w64 15.2.0、`~/scoop/apps/gcc/current/bin`）。
Emacs は `cc` → `gcc` → `c99` の順に探すので `gcc` があれば足りる。

## パッケージ管理

**straight.el に一本化**している（`package.el` は `early-init.el` で無効化済み）。

- 新しいパッケージは該当モジュール内で `(leaf package-name :straight t ...)`
- 組み込みライブラリには `:straight` / `:ensure` を付けない
- Emacs 同梱のものを使いたい場合は `init.el` で
  `(straight-use-package '(NAME :type built-in))` を宣言する（`org`、`transient` が該当）。
  これをしないと依存解決で straight が古い版をビルドして `load-path` に載せてしまう

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

## leaf を使うときの注意

leaf は `:hook` / `:bind` / `:mode` などの遅延キーワードがあると `:config` を
`(eval-after-load '<leaf名>)` で包む。**leaf 名が実在する feature でないと
`:config` も `:bind` も永久に適用されない**。

- 実在する feature 名を使う（例: `sql-mode` ではなく `sql`）
- 疑似パッケージ名（`shell-windows` など OS 別のまとまり）を使う場合は
  `:leaf-defer nil` を付けて遅延を無効化する

`:after FOO` も同じ罠を持つ。leaf は `:config` を `(eval-after-load 'FOO ...)` で
包むため、**FOO がどこからも `require` されない構成だと設定が永久に走らない**。
実例: `*font-setting` が `:after nerd-icons` だったが nerd-icons に `:require t` が
無く、フォント設定が一度も実行されないまま既定の Courier New で起動していた。
`:after` の対象には `:require t` を付けるか、そもそも依存が本当に必要か見直すこと。

`:custom` にマイナーモードの変数を書いても、そのパッケージが未ロードなら
**モードは有効にならない**。`customize-set-variable` は
`(get VAR 'custom-set)` が未設定のとき `set-default` にフォールバックするため、
変数に `t` が入るだけでモード関数が呼ばれない。
実例: `(leaf corfu :custom (global-corfu-mode . t))` では corfu が読まれず、
変数だけ `t` で補完が一切出なかった。
`:require t` でロードした上で `:config` から明示的に呼ぶこと。
他のパッケージが偶然 require してくれている場合 (vertico / yasnippet など) は
動いてしまうので、**動いていることが正しさの証拠にならない**点に注意。

到達不能な設定は次で検出できる：

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
`fonts/NFM.ttf` を入れてあり、これを使う。Windows へのユーザー単位インストールは
`%LOCALAPPDATA%\Microsoft\Windows\Fonts` へのコピーと
`HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts` へのレジストリ登録。

`my:nerd-font-family`（`user-lisp/my-appearance.el`）が `font-get-glyphs` で
実際のグリフ有無を見て選ぶので、**フォント名を決め打ちしないこと**。
名前で決め打ちすると、v2 のフォントを掴んでアイコンが全滅する。

`fonts/` の all-the-icons 用 6 フォントは company-box がまだ使っている。
corfu へ移行したら削除してよい。

## プラットフォーム固有の注意事項

- メインは **Windows 11**、パッケージ管理に **Scoop**（`USERPROFILE/scoop/shims` を `exec-path` に追加）
- **`HOME` はユーザー環境変数として `C:\Users\<user>` に設定してある。**
  未設定だと Windows の Emacs は `%APPDATA%` を `~` とみなすため、
  Explorer やスタートメニューから起動したときに `init.el` が見つからない
- Windows のシェルは Git 付属の `bash.exe`（存在するときだけ設定）、エンコーディングは cp932/UTF-8 混在
- Windows IME 統合には `tr-ime` + `w32-ime`（どちらも straight で導入）
- macOS / Linux では `exec-path-from-shell` を使用
- OS 判定は `(eq system-type 'windows-nt)` / `'darwin` / `'gnu/linux`。
  ウィンドウシステム判定は `window-system` の `'w32` / `'ns` / `'x` / `'pgtk`

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

- dired を開くと `Failed: (git status --porcelain --ignored --untracked-files=normal .)`
  が出る (dired-k)。優先度が低いため保留
- `straight/repos/` のパッケージ本体の多くが 2021 年のまま。必要になった順に
  `git pull` していく方針 (corfu と git-gutter は対応済み)
- `fonts/` の all-the-icons 用 6 フォントは設定から参照されなくなった。
  Windows にインストール済みのものと合わせて削除してよい
- Python の LSP サーバ（basedpyright / pylsp）が未導入
- `straight/build/` に lsp-mode / lsp-ui / lsp-sourcekit / company 系 /
  flycheck 系 / tide / js2-mode / elpy など、
  設定から参照されなくなったパッケージが残っている
- `w32-symlinks` ブロックは `:disabled t`。6 年間タイポで無効だったため、
  グローバル advice を無検証で有効化するのを避けている
- org 9.8 で削除された `org-extract-archive-file` への advice をコメントアウト中
  （アーカイブ先ファイル名の `#YM` 置換が効かない）
- `lexical-binding` は全モジュール `nil`。分割の等価性を優先したため
- `custom.el` に `user-lisp/` 側と重複する設定が多数残っている
