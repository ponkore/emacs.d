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
| `my-dired` | dired、hydra-dired、dired-sidebar（`F8`。差分表示は my-vc の diff-hl） |
| `my-text` | org-mode、ox-pandoc、markdown、rst、adoc |
| `my-lang-lisp` | Emacs Lisp、Clojure（cider）、Common Lisp（slime） |
| `my-lang-python` | Python（python-ts-mode、pyvenv、py-isort、blacken） |
| `my-lang-web` | PHP、JavaScript / TypeScript（js-ts-mode / typescript-ts-mode、web-mode、scss） |
| `my-lang-native` | Rust、C++、C# |
| `my-lang-misc` | SQL、bat、Swift、Lua、VisualBasic |
| `my-lsp` | eglot（組み込み、プレフィックス: `C-c l`）、flymake（`C-c !`） |
| `my-fileformat` | yaml、diff、log4j、Dockerfile、vimrc、mayu |
| `my-project` | projectile（プレフィックス: `C-c p`） |
| `my-vc` | magit、diff-hl（`C-c g` の hydra）、Windows の SVN 対応 |
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
| Python | basedpyright 1.39.10 | `uv tool install basedpyright` |

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
- `warning-suppress-log-types` / `warning-suppress-types` … leaf / straight の警告抑制
- `yas-new-snippet-default` … スニペットのテンプレート

face は `rst-level-1`〜`6` の 6 面だけ残した（modus も同じ face を定義するが、
`rst.el` はテーマより後にロードされるため `user` テーマ側が勝ち、実際に効いている）。

**face がテーマに勝つかどうかはロード順で決まる**。テーマより先に定義済みの
face（`font-lock-*` など）はテーマが勝ち、`custom.el` に書いても効かない。
テーマより後にロードされるパッケージの face は `custom.el` 側が勝つ。
確実に当てたいときは `load-theme` のあとに設定する。

`customize` を使うと `custom.el` に書き戻されるので、モジュール側と
重複していないか時々確認する。重複の検出は、`custom.el` の
`custom-set-variables` から変数名を集め、`user-lisp/` の leaf を
`macroexpand-1` して出てくる `customize-set-variable` と突き合わせればよい。

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

`fonts/` に置くのは `NFM.ttf` だけ。all-the-icons 用の 6 フォントは
（all-the-icons をやめたので）リポジトリからも Windows からも削除済み。

## プラットフォーム固有の注意事項

- メインは **Windows 11**、パッケージ管理に **Scoop**（`USERPROFILE/scoop/shims` を `exec-path` に追加）
- **`HOME` はユーザー環境変数として `C:\Users\<user>` に設定してある。**
  未設定だと Windows の Emacs は `%APPDATA%` を `~` とみなすため、
  Explorer やスタートメニューから起動したときに `init.el` が見つからない
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
`:custom-face` で別途揃える。`mode-line-inactive` はテーマのまま（灰色）にして、
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

- `straight/repos/` のパッケージ本体の多くが 2021 年のまま。必要になった順に
  `git pull` していく方針 (corfu と git-gutter は対応済み)
- `w32-symlinks` ブロックは `:disabled t`。6 年間タイポで無効だったため、
  グローバル advice を無検証で有効化するのを避けている
- org 9.8 で削除された `org-extract-archive-file` への advice をコメントアウト中
  （アーカイブ先ファイル名の `#YM` 置換が効かない）
