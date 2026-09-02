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
- `user-lisp/` — 設定本体。20 モジュールに分割（下記）
- `custom.el` — `customize` が自動生成するファイル
- `site-lisp/` — パッケージマネージャで入手できないローカルベンダの Emacs Lisp
- `docs/archive-init.org` — Org 方式だった頃の設定（履歴として保存）
- `docs/extract.el`, `docs/verify.el`, `docs/split.py`, `docs/verify-split.el` —
  Org からの抽出・分割に使った検証スクリプト（等価性の証跡）
- `docs/snapshot.el` — 設定を読み込んだ Emacs の観測可能な状態
  （defcustom 全変数、全 `*-hook` / `*-functions`、全キーバインド、face の
  `theme-face` / `defface` / 実効属性、ロード済み feature）を決定的な順序で
  ダンプする。leaf → use-package 移行の等価性検証に使った。同一設定なら
  2 回採取して差分 0 行になるので、書き換えの前後で diff すれば足りる

  ```sh
  emacs --batch -l early-init.el -l init.el -l docs/snapshot.el \
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

- **`my:treesit-remap` は必ずトップレベルで呼ぶこと**。`:config` は
  `(eval-after-load '<パッケージ名>)` に包まれるので、そこで差し替えても
  「その回に開いたバッファ」には間に合わない。さらに差し替えが効くと
  従来のモードはもうロードされないため、`:config` は二度と実行されない
- `.tsx` の `auto-mode-alist` 登録は **web-mode のブロックより後**に置くこと。
  `:mode` が先頭に積むので、前に置くと web-mode に負ける

導入済みの文法（`tree-sitter/`、git 管理外）:
bash / c-sharp / css / dockerfile / html / javascript / jsdoc / json /
python / rust / toml / tsx / typescript / yaml の 14 個。
`jsdoc` は `js-ts-mode` がコメント解析に `treesit-ensure-installed` するので必要。

コンパイラは scoop の `gcc`（mingw-w64 15.2.0、`~/scoop/apps/gcc/current/bin`）。
Emacs は `cc` → `gcc` → `c99` の順に探すので `gcc` があれば足りる。

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

あわせて `docs/snapshot.el` の前後 diff を取る（前掲）。

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

現在なし。2026-08 の一連の整理で、棚卸し時に挙げた課題はすべて解消した。

新しく気づいたことはこの節に追記する。
