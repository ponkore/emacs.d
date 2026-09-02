# Emacs 設定リファクタリング計画

- 作成日: 2026-08-29
- 対象: `C:\Users\masao\.emacs.d`
- 現行環境: GNU Emacs 31.1 (Windows 11 / `C:/Apps/emacs/emacs-31.1`)
- 対象プラットフォーム: Windows (主) / macOS / Linux
- 進捗: **取り消し線は 2026-09-02 時点で対応済み**（2 章の課題表と 4 章の作業表）。線の無い行が残作業。

---

## 0. ゴールと全体方針

### ゴール

1. **Org-mode タングル方式 (`init.org` → `init.el`) の廃止**し、素直な Emacs Lisp による初期化に戻す。
2. **現状 2793 行 / 約 104KB の `init.org` の棚卸し**を行い、不具合・警告・古い書き方を洗い出す。
3. Emacs 31.1 で新設された **`user-lisp/` ディレクトリ**に機能単位で分割配置する。

### 全体方針

- **段階的に進める。** 各フェーズ終了時点で必ず「Emacs が起動し、`*Warnings*` が悪化していない」ことを確認する。
- **フェーズ 1 は「等価変換」に徹する。** 挙動を変えず、単に Org からプレーン elisp へ移す。バグ修正はフェーズ 2 以降。
- **git のブランチを切って作業する。** フェーズごとにコミットし、いつでも `master` に戻れるようにする。
- **判断が要る箇所（後述「意思決定ポイント」）は、実装前にユーザーに確認する。**

---

## 1. 現状把握（調査済み事実）

### 1.1 ファイル構成

| パス | 役割 | 状態 |
|---|---|---|
| `init.el` | 2 行のブートストラップ。`org-babel-load-file` で `my-config/init.org` を読む | 廃止対象 |
| `my-config/init.org` | 全設定の正本。2793 行 / 133 個の `emacs-lisp` ブロック | 分割対象 |
| `my-config/init.el` | タングル生成物 (96KB)。`.gitignore` 済み | 削除対象 |
| `custom.el` | `customize` 自動生成 | 継続 |
| `site-lisp/` | ベンダ elisp 9 個 (`eaw.el`, `cp5022x.el`, `visual-basic-mode.el`, `mayu-mode.el`, `w32-symlinks.el`, `ntcmd.el`, `smartchr.el`, `color-theme.el`, `nerd-fonts.el/`) | 一部のみ使用 |
| `find-npm-command.el` | flycheck が `node_modules/.bin` を優先するようにする | **未追跡・どこからも読まれていない（デッド）** |
| `powerline.el` | powerline カスタマイズ | **未追跡・doom-modeline 使用中のためデッド** |
| `init.el~` (99KB) | 旧世代のバックアップ | 削除候補 |
| `eln-cache/` | native-comp キャッシュ | `.gitignore` に未登録 |

### 1.2 Emacs 31.1 の `user-lisp/` 仕様（`startup.el` で確認済み）

- `user-lisp-directory` の既定値は `(locate-user-emacs-file "user-lisp/")`。
- 起動時、`package-activate-all` の**直後**、かつ **`init.el` の読み込み前**に `prepare-user-lisp` が実行される。
- 動作内容:
  1. `user-lisp/` と全サブディレクトリを `load-path` に追加
  2. 全 `.el` を**バイトコンパイル**（タイムスタンプ差分のみ、遅延）＋ native-compile
  3. autoload cookie (`;;;###autoload`) を走査し `.user-lisp-autoloads.el` を生成・ロード
- `user-lisp-auto-scrape` / `user-lisp-directory` / `user-lisp-ignored-directories` は **`early-init.el` で設定する必要がある**（init.el では手遅れ）。
- `prepare-user-lisp` はいつでも手動実行可能（`M-x prepare-user-lisp`、`C-u` 付きで全再コンパイル）。

> **⚠ 最重要の制約**
> `prepare-user-lisp` によるバイトコンパイルは **straight.el のブートストラップより前**に走る。
> したがって `user-lisp/` 配下のファイルで `leaf` マクロをトップレベルに書くと、
> コンパイル時に `leaf` が未定義 → マクロ展開されず関数呼び出しとしてコンパイルされ、**壊れた `.elc` が生成される**。
> → 「意思決定ポイント A」で対処方針を決める必要がある。

> **⚠ もう一つの制約**
> `user-lisp/` は「`load-path` 追加 + autoload 生成」までしかやらない。
> 副作用（設定の適用）を持つモジュールは `init.el` から明示的に `(require 'my-xxx)` する必要がある。

### 1.3 `init.org` のセクション構成（現状）

```
1.  パッケージ管理        straight / leaf / site-lisp / s / custom.el
2.  日本語環境設定        日本語env / encoding / Windows IME (tr-ime) / migemo
3.  フォント設定          フォント / text-scale / all-the-icons (3)
4.  ウィンドウ表示設定    Mac / Windows / 共通 / テーマ(modus)
5.  モードライン          diminish / doom-modeline
6.  補完                  marginalia / vertico / consult / embark / orderless / corfu(無効) / company (3)
7.  基本キーバインド      global-set-keys
8.  エディタ全般設定      hydra / symbol-overlay / smartparens / fci / expand-region / cua /
                          recentf-ext / highlight-indent-guides / whitespace / rainbow-delimiters /
                          yasnippet / anzu / uniquify / scroll / backup / autorevert /
                          global設定 / which-key / C-a 改善
9.  dired                 dired-k / dired + hydra / neotree
10. テキストモード        org / ox-pandoc / ob-mermaid / org-bullets / org-download /
                          markdown / rst / adoc
11. プログラミング言語    elisp / clojure / lisp(slime) / python(elpy) / php / js,ts /
                          rust / c++ / c# / sql / bat / swift / lua / vb / lsp / flycheck
12. 特定ファイルフォーマット yaml / diff / log4j / docker / vimrc / mayu
13. プロジェクト管理      projectile
14. 構成管理              magit / git-gutter / svn(Windows)
15. Shell                 exec-path-from-shell / Windows shell / shell
16. ユーティリティ        calendar / open-junk-file / dashboard(無効) / google / grep-r /
                          ripgrep / myblog-hugo
17. OS 固有               Windows (環境変数 / w32-symlinks / cygwin) / Mac (modifier)
```

---

## 2. 課題の棚卸し（フェーズ 2 の入力）

深刻度: **[致命]** = 起動エラー/機能不全 / **[高]** = 明確なバグ・非推奨 API / **[中]** = 動作するが古い/非効率 / **[低]** = 整理事項

### 2.1 起動時にエラーになりうるもの

| # | 深刻度 | 箇所 | 内容 |
|---|---|---|---|
| ~~A-1~~ | ~~致命~~ | ~~slime~~ | ~~`(inferior-lisp-program . ,(concat (executable-find "ros") " run"))` — roswell 未インストール環境で `executable-find` が nil → `concat` がエラー。**Windows/Linux で起動が壊れる**~~ |
| ~~A-2~~ | ~~致命~~ | ~~python/pyvenv~~ | ~~`(pyvenv-activate (expand-file-name "~/.emacs.d/elpy/rpc-venv"))` を無条件実行。ディレクトリが無いとエラー~~ |
| ~~A-3~~ | ~~高~~ | ~~global-configuraions~~ | ~~`:hook` セクション内に `(split-width-threshold . nil)` — **変数をフックとして登録**しようとしている。明確なバグ~~ |
| ~~A-4~~ | ~~高~~ | ~~yasnippet~~ | ~~`(yatemplatefill-alist)` — 関数名タイポ（正: `yatemplate-fill-alist`）。実行時エラー~~ |
| ~~A-5~~ | ~~高~~ | ~~custom.el~~ | ~~`(load (setq custom-file ...))` — `custom.el` 欠損時にエラー。`(load custom-file t t)` にすべき~~ |
| ~~A-6~~ | ~~高~~ | ~~vertico~~ | ~~`:ensure t`（package.el）で入れた後に `:config` で `straight-use-package '(vertico ...)` を実行 — **二重インストール**。load-path が不定~~ |
| ~~A-7~~ | ~~高~~ | ~~grep-r~~ | ~~`(setq grep-command (cons ...))` — `grep-command` は文字列であるべき箇所に cons を代入。`M-x grep` が壊れる。加えて組み込み `grep-default-command` を再定義している~~ |

### 2.2 タイポ由来で「そもそも動いていない」設定

| # | 深刻度 | 箇所 | 内容 |
|---|---|---|---|
| ~~B-1~~ | ~~高~~ | ~~w32-symlinks~~ | ~~`(eq system-type 'windoows-nt)` — **`windows-nt` のタイポ**。このブロックは一度も有効化されていない~~ |
| ~~B-2~~ | ~~高~~ | ~~backup~~ | ~~`(setq bavckup-inhibited t)` — **`backup-inhibited` のタイポ**。デッドコード~~ |
| ~~B-3~~ | ~~中~~ | ~~flycheck hydra~~ | ~~`(fiycheck-previous-error)` — タイポ~~ |
| ~~B-4~~ | ~~中~~ | ~~markdown hydra~~ | ~~`markdown-insert-imaget` — タイポ（正: `markdown-insert-image`）~~ |
| ~~B-5~~ | ~~低~~ | ~~org~~ | ~~TODO キーワード face の `"gren"` — `"green"` のタイポ~~ |
| ~~B-6~~ | ~~低~~ | ~~lsp-mode~~ | ~~`:commands lsp sp-defered` — `lsp-deferred` のタイポ。また `shell-script-mode-hook` は存在しないモード~~ |
| ~~B-7~~ | ~~低~~ | ~~見出し~~ | ~~`global-configuraions` / `editor global configraiton` — 綴り~~ |
| ~~B-8~~ | ~~高~~ | ~~whitespace~~ | ~~`(whitespace-style . whitespace-style-with-tab)` — 変数の**値**でなく**シンボル**を代入している。さらに `whitespace-style-with-tab` / `-without-tab` は `defvar` されておらず `:custom` で定義しようとしている~~ |

### 2.3 廃止・非推奨 API（バイトコンパイル警告の主因）

| # | 深刻度 | 箇所 | 内容 |
|---|---|---|---|
| ~~C-1~~ | ~~高~~ | ~~7 箇所~~ | ~~`defadvice` / `ad-activate` / `ad-do-it`（Emacs 24.4 で obsolete）: `scroll-up`, `scroll-down`, `org-pandoc-run`, `vc-svn-command`, `grep`, `insert-file-contents-literally`, `minibuffer-complete` → `advice-add` へ移行~~ |
| ~~C-2~~ | ~~高~~ | ~~windows-ime~~ | ~~`input-method-inactivate-hook` は Emacs 24.3 で obsolete → `input-method-deactivate-hook`~~ |
| ~~C-3~~ | ~~高~~ | ~~テーマ~~ | ~~`modus-themes-load-themes` / `modus-themes-load-vivendi` は modus-themes 4.x で**削除済み**。`modus-themes-region` も廃止。かつ modus-themes は Emacs 31 に**同梱**されているので `:straight t` 不要 → `(load-theme 'modus-vivendi t)`~~ |
| ~~C-4~~ | ~~中~~ | ~~日本語env~~ | ~~`default-file-name-coding-system` は obsolete~~（訂正: obsolete ではない。`prefer-coding-system` の副作用を戻していた行だが、`file-name-coding-system` が非 nil なので参照されないフォールバック。docstring の指示どおり `file-name-coding-system` のみ設定する形にして削除） |
| ~~C-5~~ | ~~中~~ | ~~recentf-ext~~ | ~~`(run-with-idle-timer 120 t '(lambda () ...))` — quote された lambda（非推奨）~~ |
| ~~C-6~~ | ~~中~~ | ~~scss-mode~~ | ~~`(previous-line)` を Lisp コード中で使用 → コンパイラ警告。`(forward-line -1)` へ~~ |
| ~~C-7~~ | ~~中~~ | ~~全般~~ | ~~`linum-format` / `linum` face — `linum.el` は Emacs 29 で obsolete。`display-line-numbers` 移行済みなのでデッド設定~~ |
| ~~C-8~~ | ~~中~~ | ~~dired~~ | ~~`magit-status-internal` — 現行 magit では非公開/変更あり → `magit-status-setup-buffer`~~ |
| ~~C-9~~ | ~~中~~ | ~~clojure~~ | ~~`define-clojure-indent` は clojure-mode 5.18+ で非推奨 → `put-clojure-indent` / `.dir-locals.el`~~（訂正: 5.23.0 でも非推奨ではない。非推奨なのは spec のレガシー形式（整数 / `:defn` / 位置リスト）で clojure-mode 6 で削除予定。`put-clojure-indent` + tuple 形式へ移行） |
| ~~C-10~~ | ~~中~~ | ~~w32-symlinks~~ | ~~`custom-set-variables` を `:config` 内で使用 → `custom-file` を汚す。`setopt` へ~~ |
| ~~C-11~~ | ~~低~~ | ~~org-screenshot~~ | ~~`(setq filename ...)` — 未宣言のフリー変数。`let` へ~~ |

### 2.4 パッケージ管理の二重化・古い設定

| # | 深刻度 | 内容 |
|---|---|---|
| ~~D-1~~ | ~~高~~ | ~~**straight.el と package.el が併走**している。`:straight t` と `:ensure t` が混在（`vertico`, `cljstyle-format`, `calendar`, `org-download`, `dashboard`, `cp5022x` が `:ensure`）。どちらかに統一が必要~~ |
| ~~D-2~~ | ~~高~~ | ~~`package-archives` に **marmalade**（2017 年に停止）と **orgmode.org/elpa**（廃止済み）が残っている → `package-refresh-contents` が遅延/失敗~~ |
| ~~D-3~~ | ~~中~~ | ~~`(package-initialize)` の明示呼び出しと `package-refresh-contents` を起動時に実行 → 起動が遅い。Emacs 27+ では不要~~ |
| ~~D-4~~ | ~~高~~ | ~~**組み込みライブラリに `:straight t` / `:ensure t` が付いている**: `uniquify`, `whitespace`, `rst`, `cc-mode`, `bat-mode`, `calendar`, `csharp-mode`（Emacs 29+ で組み込み）。straight がクローンを試みて警告/失敗する~~（訂正: straight は `(:type built-in)` と解決するのでクローンも失敗もしない。実害は誤った表示とレシピ検索のみ） |
| ~~D-5~~ | ~~中~~ | ~~`leaf sql-mode` / `leaf text-scale` / `leaf ripgrep*` / `leaf grep-r` など、**存在しない feature 名**を leaf ブロック名にしている（動作はするが `:require` 等と噛み合わない）~~ |
| ~~D-6~~ | ~~中~~ | ~~straight の bootstrap URL が `raxod502/straight.el`（現在は `radian-software/straight.el`。リダイレクトで動くが要更新）~~ |
| ~~D-7~~ | ~~中~~ | ~~`embark` は `:disabled t` なのに `embark-consult` は有効 → 結局 embark が入る~~ |

### 2.5 Org-mode タングル方式に起因する問題（フェーズ 1 の動機）

| # | 内容 |
|---|---|
| ~~E-1~~ | ~~`init.el` が `org-babel-load-file` を呼ぶため、**組み込み org が必ず先にロードされる**。その後 `(leaf org :straight t)` で org を入れると**バージョン混在**（org の古典的破損パターン）~~ |
| ~~E-2~~ | ~~`(leaf org :after ox-pandoc)` により、org の設定適用が ox-pandoc のロード待ちになる。さらに `:config` で `(org-pandoc-startup-check)` を無条件実行~~ |
| ~~E-3~~ | ~~タングルのため**起動のたびに Org パーサが走る**（初回/更新時）。素の `.el` + バイトコンパイルに比べ確実に遅い~~ |
| ~~E-4~~ | ~~タングル生成物 `my-config/init.el` (96KB) はバイトコンパイルされていない~~ |
| ~~E-5~~ | ~~設定の一部だけを評価する `C-c C-c` ワークフローが実質使われていない（ユーザー談）~~ |

### 2.6 グローバルフックの副作用が強すぎる設定

| # | 深刻度 | 内容 |
|---|---|---|
| ~~F-1~~ | ~~高~~ | ~~`(before-save-hook . prettier-js)` を **グローバル**に登録（tide ブロック内）。全ファイルの保存時に prettier が走る~~ |
| ~~F-2~~ | ~~高~~ | ~~`(before-save-hook . py-isort-before-save)` も**グローバル**登録~~ |
| ~~F-3~~ | ~~高~~ | ~~`delete-file-if-no-contents` を `after-save-hook` に**グローバル**登録 — 空で保存したファイルを**問答無用で削除**する。事故リスク~~（グローバル登録は維持し、`y-or-n-p` の確認・ごみ箱経由・narrowing バグ修正で安全化） |
| ~~F-4~~ | ~~中~~ | ~~`smartparens-global-strict-mode` — strict モードのグローバル適用は強すぎる。Emacs 30+ の `electric-pair-mode` で足りる可能性~~（`smartparens-global-mode` + Lisp 系メジャーモードのみ `smartparens-strict-mode` に変更。`electric-pair-mode` へは移行せず smartparens を継続） |
| ~~F-5~~ | ~~中~~ | ~~web-mode / scss-mode の `:config` トップレベルで `(prettier-js-mode)` / `(yas-minor-mode)` を呼んでいる → **ロード時のカレントバッファ**に対して実行されてしまう~~ |
| ~~F-6~~ | ~~中~~ | ~~`elpy-enable` を `:init` で実行 → 起動時に elpy 一式を eager load~~ |
| ~~F-7~~ | ~~中~~ | ~~`(require 'ripgrep)` を projectile の `:preface` で実行 → 起動時 eager load。しかも `ripgrep` パッケージ自体は宣言されていない~~ |
| ~~F-8~~ | ~~中~~ | ~~`tr-ime-advanced-install` を毎起動で実行（本来は一度きりのインストーラ）~~ |

### 2.7 ハードコードされたパス（マルチプラットフォーム対応の障害）

| # | 箇所 | 内容 |
|---|---|---|
| ~~G-1~~ | ~~Windows shell~~ | ~~`"C:/Users/masao/scoop/shims"` — ユーザー名直書き~~ |
| ~~G-2~~ | ~~site-lisp~~ | ~~`(expand-file-name "~/.emacs.d/site-lisp")` — `user-emacs-directory` を使うべき（他に recentf, migemo 辞書, config-sqlplus.el, elpy rpc-venv も同様）~~ |
| G-3 | markdown | `markdown-command` が `~/AppData/Roaming/pandoc/metadata.yml` 固定、`markdown-open-command` が `c:/Program Files/Typora/Typora.exe` 固定 → mac/Linux で壊れる。加えて `--self-contained` は pandoc 3 で非推奨（→ `--embed-resources --standalone`） |
| ~~G-4~~ | ~~ox-pandoc~~ | ~~`reference-doc` が `~/AppData/Roaming/pandoc/custom-reference.docx` 固定~~ |
| ~~G-5~~ | ~~open-junk-file~~ | ~~`"~/Library/CloudStorage/Dropbox-個人用/junk/..."` — **macOS 専用パス**。Windows で `C-x j` が壊れる~~ |
| G-6 | SQL | `"c:/Apps/Oracle/sqlplus.exe"` / `~/Applications/Oracle/instantclient_10_2` |
| ~~G-7~~ | ~~swift~~ | ~~`lsp-sourcekit-executable` が Xcode 固定パス~~ |
| G-8 | フレーム | `initial-frame-alist` の `(left . 670)` `(width . 136)` — モニタ構成依存 |

### 2.8 Linux 未対応

| # | 内容 |
|---|---|
| ~~H-1~~ | ~~OS 分岐が `darwin` と `windows-nt` の 2 つのみ。**`gnu/linux` の分岐が一切ない**~~ |
| ~~H-2~~ | ~~フレーム設定 (`frame-setting-mac` / `frame-setting-windows`) に Linux 版がなく、Linux では `default-frame-alist` が未設定~~ |
| ~~H-3~~ | ~~フォント設定 `setup-font` が `ns` / `w32` のみ分岐。Linux (`x` / `pgtk`) で**フォントが設定されない**~~ |
| ~~H-4~~ | ~~`exec-path-from-shell` が `darwin` 限定。Linux の GUI 起動でも必要になるケースがある~~ |
| ~~H-5~~ | ~~`frame-setting-windows` に `ns-transparent-titlebar`（macOS 専用パラメータ）が混入~~ |
| ~~H-6~~ | ~~`org-screenshot` は `import` (ImageMagick) で Linux 対応済みだが、Windows 分岐がない~~ |

### 2.9 「今風の書き方」への更新候補（要意思決定）

| # | 現状 | 選択肢 |
|---|---|---|
| ~~I-1~~ | ~~`all-the-icons` (+ `-dired` / `-ibuffer` / `company-box` 連携)~~ | ~~`nerd-icons` へ移行。doom-modeline 4.x は nerd-icons 前提。`site-lisp/nerd-fonts.el/` があるので移行途中と推測~~ |
| ~~I-2~~ | ~~`lsp-mode` + `lsp-ui`~~ | ~~組み込み `eglot` (Emacs 29+)~~ |
| ~~I-3~~ | ~~`flycheck` + `flycheck-pos-tip` + `flycheck-inline`~~ | ~~組み込み `flymake`。なお pos-tip と inline の**同時有効化は表示が二重**になる~~ |
| ~~I-4~~ | ~~`company` + `company-box` + `company-quickhelp`~~ | ~~`corfu` + `cape` + `kind-icon`（`corfu` 設定はコメントアウト済みで移行意図あり）~~ |
| ~~I-5~~ | ~~`js2-mode` / `typescript-mode` / `web-mode` + `tide`~~ | ~~`js-ts-mode` / `typescript-ts-mode` / `tsx-ts-mode` (tree-sitter, Emacs 29+) + eglot~~ |
| ~~I-6~~ | ~~`elpy` (メンテ停滞)~~ | ~~`python-ts-mode` + eglot(pylsp/pyright) + `ruff`~~ |
| ~~I-7~~ | ~~`fill-column-indicator` (fci-mode)~~ | ~~組み込み `display-fill-column-indicator-mode` (Emacs 27+)~~ |
| ~~I-8~~ | ~~`cua-mode` (矩形選択目的)~~ | ~~組み込み `rectangle-mark-mode` (`C-x SPC`)~~ |
| ~~I-9~~ | ~~`dired-k` (アーカイブ済み)~~ | ~~`diff-hl-dired` / `dired-git-info`~~ |
| ~~I-10~~ | ~~`neotree`~~ | ~~組み込み `project.el` + `dired-sidebar` / `treemacs`~~ |
| ~~I-11~~ | ~~`git-gutter`~~ | ~~`diff-hl`~~ |
| ~~I-12~~ | ~~`defadvice` による scroll-up/down 拡張~~ | ~~`scroll-error-top-bottom` を `t` にするだけで代替可能~~ |
| ~~I-13~~ | ~~`recentf-ext`~~ | ~~組み込み `recentf` + `savehist`~~ |
| ~~I-14~~ | ~~`csharp-mode` (外部)~~ | ~~組み込み `csharp-mode` / `csharp-ts-mode`~~ |
| ~~I-15~~ | ~~`google` 検索関数~~ | ~~Shift_JIS エンコード + `http://` と完全に時代遅れ。`webjump` / 削除~~ |
| ~~I-16~~ | ~~`eaw.el` (site-lisp)~~ | ~~Emacs の East Asian Ambiguous 幅処理が改善されている可能性。要検証~~ |
| ~~I-17~~ | ~~`w32-ime` / `tr-ime`~~ | ~~tr-ime 4.x の推奨手順に合わせて書き換え（`w32-ime-initialize` の要否確認）~~ |
| ~~I-18~~ | ~~`(set-frame-parameter nil 'alpha 85)`~~ | ~~`alpha-background` (Emacs 29+) — 文字は透過させず背景のみ~~ |

### 2.10 その他

| # | 内容 |
|---|---|
| ~~J-1~~ | ~~`early-init.el` が存在しない。GC チューニング / パッケージ初期化抑制 / フレーム設定前倒しができていない。**`user-lisp-*` の設定にも必須**~~ |
| J-2 | `(setenv "HOME" (getenv "USERPROFILE"))` を init.el 内で実行 — HOME は `.emacs.d` の探索に既に使われた後なので手遅れ |
| ~~J-3~~ | ~~`orderless` が `completion-styles '(orderless)` のみ。`basic` フォールバックがなく TRAMP / ファイル名補完で問題が出やすい~~ |
| ~~J-4~~ | ~~`marginalia` を `vertico-after-init-hook` という自作フック経由で有効化する回りくどい構造~~ |
| ~~J-5~~ | ~~`helm-minibuffer-set-up-hook` への `add-hook` — helm 未使用~~ |
| ~~J-6~~ | ~~`leaf ripgrep*` の `my:ripgrep-regexp` は定義のみで、dired は `ripgrep-regexp`（本家）にバインドしている。デッドコード~~ |
| ~~J-7~~ | ~~`find-npm-command.el` / `powerline.el` がどこからも読まれていない~~ |
| ~~J-8~~ | ~~`.gitignore` に `eln-cache/` `init.el~` `tmp/` が無い~~ |
| J-9 | `("C-x n" . myblog-hugo/create-draft)` が narrowing プレフィックス `C-x n` を潰している。`("C-x C-e" . compile)` が `eval-last-sexp` を潰している（意図的なら維持） |
| ~~J-10~~ | ~~`inhibit-startup-echo-area-message . -1` — 本来はログイン名の文字列を渡す必要がある~~ |
| ~~J-11~~ | ~~`auto-save-list-file-name` / `auto-save-list-file-prefix` の設定が 2 ブロックに重複~~ |
| ~~J-12~~ | ~~`shell-file-name` を Windows で `bash.exe` に変更 — `call-process-shell-command` を使う一部パッケージが壊れる可能性あり。`explicit-shell-file-name` のみに留める案~~ |

---

## 3. 意思決定ポイント（実装前に確認したいこと）

### A. `user-lisp/` と `leaf` / straight.el の共存方式 【最重要】

`prepare-user-lisp` は straight.el ブートストラップ**前**に走るため、`user-lisp/` 内で `leaf` をそのまま使うと壊れる。

- **A-1 案: `user-lisp-auto-scrape` を `nil` にする**
  `early-init.el` で `nil` にし、`init.el` で straight/leaf をブートストラップした後に `(prepare-user-lisp)` を手動呼び出し。
  → leaf を使い続けられる。`user-lisp/` の恩恵（load-path・autoload・バイトコンパイル）も得られる。**推奨**
- **A-2 案: `user-lisp/` を標準準拠のライブラリ置き場にする**
  `leaf` を使わず、組み込み `use-package` (`(eval-when-compile (require 'use-package))`) + `setopt` + `with-eval-after-load` で書く。
  → 完全に標準準拠。ただし全設定の書き換えが必要で、フェーズ 1 の「等価変換」を逸脱する。
- **A-3 案: `user-lisp/` を使わず `my-config/*.el` を `load` する**
  最も保守的。Emacs 31 の新機能は使わない。

### B. パッケージマネージャの一本化

- **B-1 案: straight.el に統一**（現状に一番近い。`:ensure t` を全て `:straight t` へ）
- **B-2 案: 組み込み `package.el` + `use-package` + `package-vc` に統一**（Emacs 30+ の標準。straight/leaf を捨てる）
- **B-3 案: `elpaca` へ移行**

### C. 移行の深さ（フェーズ 2 の範囲）

- **C-1 案: バグ修正と非推奨 API の置換のみ**（2.1〜2.4, 2.6〜2.8, 2.10 のみ対応。2.9 のスタック刷新はやらない）**推奨・低リスク**
- **C-2 案: 上記 + 組み込み機能への部分移行**（fci→display-fill-column-indicator、cua→rectangle-mark、scroll advice 撤去 など安全なものだけ）
- **C-3 案: LSP/補完/checker スタックを全面刷新**（lsp-mode→eglot、flycheck→flymake、company→corfu、all-the-icons→nerd-icons、tide→tree-sitter+eglot）

### D. `site-lisp/` の扱い

`user-lisp/` のサブディレクトリとして取り込むか、従来通り `site-lisp/` を別管理にするか。
（`user-lisp/` 配下に置くと自動バイトコンパイル対象になり、古いベンダコードで警告が大量に出る可能性あり。`user-lisp-ignored-directories` で除外も可能）

---

## 4. 実施計画

### フェーズ 0: 準備

| # | 作業 | 完了条件 |
|---|---|---|
| ~~0-1~~ | ~~作業ブランチ作成 `git switch -c refactor/de-org-tangle`~~ | ~~ブランチ作成済み~~ |
| ~~0-2~~ | ~~**現状のベースライン取得**: `emacs --debug-init` で起動し `*Warnings*` / `*Messages*` を `tmp/baseline-warnings.txt` に保存~~ | ~~現状の警告一覧が記録される~~ |
| 0-3 | 起動時間のベースライン計測（`emacs-init-time`） | 数値が記録される |
| ~~0-4~~ | ~~動作確認チェックリスト（6 章）の現状の○×を埋める~~ | ~~チェックリスト完成~~ |
| ~~0-5~~ | ~~`.gitignore` に `eln-cache/`, `init.el~`, `tmp/`, `user-lisp/.user-lisp-autoloads.el` を追加~~ | ~~`git status` がクリーン~~ |
| ~~0-6~~ | ~~意思決定ポイント A〜D をユーザーと合意~~ | ~~方針確定~~ |

### フェーズ 1: Org タングルの廃止（等価変換）

**方針: 挙動は一切変えない。バグもタイポもこの段階では残す。**

| # | 作業 | 完了条件 |
|---|---|---|
| ~~1-1~~ | ~~`init.org` の全 `emacs-lisp` ブロックを、Org 見出し階層に対応するコメント付きで 1 本の `my-config/init-main.el` に抽出~~ | ~~ブロック数 133 が全て転記される~~ |
| ~~1-2~~ | ~~抽出結果を旧タングル生成物 `my-config/init.el` と diff して**等価であることを確認**~~ | ~~意味的差分ゼロ~~ |
| ~~1-3~~ | ~~`init.el` を書き換え: `org-babel-load-file` → `(load (expand-file-name "init-main.el" my-config-dir))`~~ | ~~起動する~~ |
| ~~1-4~~ | ~~起動確認。`*Warnings*` をフェーズ 0 のベースラインと比較~~ | ~~**警告が増えていない**こと~~ |
| ~~1-5~~ | ~~6 章の動作確認チェックリストを実行~~ | ~~フェーズ 0 と同じ結果~~ |
| ~~1-6~~ | ~~`my-config/init.org` を `docs/init.org.archive` 等に退避（履歴として残す）、`my-config/init.el` を削除~~ | ~~ファイル整理完了~~ |
| ~~1-7~~ | ~~コミット~~ | — |

### フェーズ 2: 課題の是正

**フェーズ 1 で作った 1 本の `.el` に対して 2 章の課題を順に潰す。**
分割前に潰すことで「分割による退行」と「修正による退行」を切り分けられる。

| # | 作業 | 対応する課題 |
|---|---|---|
| ~~2-1~~ | ~~**起動エラー要因の除去**（`executable-find` の nil ガード、pyvenv の存在チェック、`custom.el` の noerror、`grep-command`、vertico の二重インストール、`:hook` に紛れた変数）~~ | ~~2.1 (A-1〜A-7)~~ |
| 2-2 | **タイポ修正** | 2.2 (B-1〜B-8) |
| 2-3 | **非推奨 API の置換**（`defadvice`→`advice-add`、`input-method-deactivate-hook`、modus-themes 4.x 対応、`magit-status-setup-buffer` 等） | 2.3 (C-1〜C-11) |
| 2-4 | **パッケージマネージャの一本化**（意思決定 B に従う。死んだ ELPA の削除、組み込みライブラリからの `:straight`/`:ensure` 除去、bootstrap URL 更新） | 2.4 (D-1〜D-7) |
| 2-5 | **グローバルフックの局所化**（prettier-js / py-isort をメジャーモードフックへ、`delete-file-if-no-contents` の扱いを確認、`:config` トップレベルのモード起動を除去、`tr-ime-advanced-install` の条件付き化） | 2.6 (F-1〜F-8) |
| 2-6 | **パスのハードコード解消**（`user-emacs-directory` / `getenv` ベース、存在チェック付き、OS 分岐） | 2.7 (G-1〜G-8) |
| ~~2-7~~ | ~~**`early-init.el` の新設**（GC チューニング、`package-enable-at-startup`、`user-lisp-*`、フレーム設定の前倒し、Windows の HOME 設定）~~ | ~~2.10 (J-1, J-2)~~ |
| ~~2-8~~ | ~~**Linux 分岐の追加**（フォント、フレーム、`exec-path-from-shell`、スクリーンショット）~~ | ~~2.8 (H-1〜H-6)~~ |
| ~~2-9~~ | ~~**デッドコードの削除**（`find-npm-command.el` / `powerline.el` / `init.el~` / helm フック / 未使用 `my:ripgrep-regexp` / google 検索など、ユーザー確認の上で）~~ | ~~2.10 (J-5〜J-7)~~ |
| ~~2-10~~ | ~~(意思決定 C-2 / C-3 を選んだ場合) スタックの近代化~~ | ~~2.9 (I-1〜I-18)~~ |
| ~~2-11~~ | ~~各ステップごとに起動確認 + 警告比較 + チェックリスト実行、こまめにコミット~~ | — |

**完了条件: `*Warnings*` が空、または残る警告が全て説明可能。**

### フェーズ 3: `user-lisp/` への機能分割

意思決定 A の結果に応じて構成を決めるが、A-1 案（`user-lisp-auto-scrape` = nil）を前提とした想定構成:

```
.emacs.d/
├── early-init.el              ; GC, user-lisp-*, package抑制, フレーム前倒し, HOME(Windows)
├── init.el                    ; ブートストラップ + require の並び（20〜40行）
├── custom.el
├── site-lisp/                 ; ベンダ elisp（自動コンパイル対象外に置く）
└── user-lisp/
    ├── my-core.el             ; パッケージ管理(straight/leaf), 汎用ヘルパ, パス定義
    ├── my-platform.el         ; OS 判定, exec-path, shell (win/mac/linux)
    ├── my-japanese.el         ; encoding, cp5022x, eaw, migemo
    ├── my-ime.el              ; Windows IME (tr-ime)
    ├── my-appearance.el       ; フォント, フレーム, テーマ, modeline, icons
    ├── my-editor.el           ; 汎用エディタ設定, whitespace, パーレン, スクロール, backup
    ├── my-keybind.el          ; グローバルキーバインド
    ├── my-completion.el       ; vertico/consult/orderless/marginalia/corfu or company
    ├── my-dired.el            ; dired, hydra-dired, neotree
    ├── my-vc.el               ; magit, git-gutter(or diff-hl), svn
    ├── my-project.el          ; projectile (or project.el)
    ├── my-lsp.el              ; lsp-mode(or eglot), flycheck(or flymake)
    ├── my-snippet.el          ; yasnippet
    ├── my-text.el             ; org, markdown, rst, adoc
    ├── my-lang-lisp.el        ; elisp, clojure, common lisp
    ├── my-lang-web.el         ; js/ts/tsx, css/scss, php
    ├── my-lang-python.el      ; python
    ├── my-lang-native.el      ; c/c++, c#, rust, swift
    ├── my-lang-data.el        ; sql, yaml, docker, log4j, bat, vb, lua, vimrc, mayu
    └── my-utils.el            ; calendar, open-junk-file, grep/ripgrep, myblog-hugo
```

| # | 作業 | 完了条件 |
|---|---|---|
| ~~3-1~~ | ~~`early-init.el` に `user-lisp-auto-scrape` / `user-lisp-directory` / `user-lisp-ignored-directories` を設定（意思決定 A に従う）~~ | — |
| ~~3-2~~ | ~~`user-lisp/` を作成し、**1 ファイルずつ**切り出す。各ファイル先頭に `;;; -*- lexical-binding: t -*-` とファイルヘッダ、末尾に `(provide 'my-xxx)`~~ | ~~各ファイルが単体でバイトコンパイル可能~~ |
| ~~3-3~~ | ~~`init.el` に `(require 'my-xxx)` を**依存順**に並べる（core → platform → japanese → appearance → editor → …）~~ | ~~起動する~~ |
| ~~3-4~~ | ~~1 ファイル切り出すごとに起動確認 + 警告比較~~ | ~~退行なし~~ |
| 3-5 | 全モジュールで `C-u M-x prepare-user-lisp` を実行し、**バイトコンパイル警告をゼロにする** | 警告ゼロ |
| ~~3-6~~ | ~~相互依存が発生した箇所を整理（`with-eval-after-load` / `:after` で解決し、`require` の循環を作らない）~~ | ~~循環なし~~ |
| 3-7 | 起動時間を再計測しフェーズ 0 と比較 | 悪化していない（改善が期待される） |
| ~~3-8~~ | ~~`CLAUDE.md` を新構成に合わせて更新~~ | ~~ドキュメント整合~~ |
| 3-9 | macOS / Linux での起動確認（可能なら） | 各 OS で起動 |
| ~~3-10~~ | ~~`master` へマージ~~ | — |

---

## 5. リスクと緩和策

| リスク | 緩和策 |
|---|---|
| フェーズ 1 で設定が抜け落ちる | 旧タングル生成物 `my-config/init.el` との diff で機械的に検証。ブロック数 133 を突合 |
| 修正と分割を同時にやって原因切り分け不能になる | フェーズ 1（等価変換）→ 2（修正）→ 3（分割）を厳密に分ける |
| straight のビルドキャッシュ破損で起動不能 | `straight/` は `.gitignore` 済み。`straight-freeze-versions` でロックファイルを先に取得しておく |
| `user-lisp/` の自動バイトコンパイルが straight より先に走り壊れる | 意思決定 A で対処方針を確定（A-1 案なら `user-lisp-auto-scrape` = nil） |
| 他 OS (mac/Linux) で退行 | OS 依存部を `my-platform.el` に集約し、`(eq system-type ...)` を 1 箇所に閉じ込める。パス類は必ず存在チェック付きに |
| 作業途中で Emacs が起動しなくなり、作業自体ができなくなる | `emacs -Q` で作業する運用を確保。`init.el` 先頭に `(setq debug-on-error t)` を一時的に置く |

---

## 6. 動作確認チェックリスト（各フェーズ末で実行）

### 起動
- [ ] `emacs` が GUI で起動する（エラーダイアログなし）
- [ ] `*Warnings*` バッファが空、または既知の警告のみ
- [ ] `M-x emacs-init-time` が許容範囲
- [ ] `emacs --batch -l init.el` がエラーなく終了する

### 日本語環境
- [ ] 日本語ファイル（UTF-8 / cp932 / EUC-JP）が正しく読める
- [ ] cp932 ファイルの保存で文字化けしない
- [ ] modeline のエンコーディング表示（U/E/J/S）が正しい
- [ ] (Windows) IME の ON/OFF がカーソル色に反映される
- [ ] (Windows) minibuffer 突入時に IME が OFF になる
- [ ] `migemo` によるローマ字→日本語検索が動く

### 表示
- [ ] フォントが適用されている（ASCII と日本語の幅が 1:2）
- [ ] テーマ (modus-vivendi) が適用されている
- [ ] doom-modeline が表示される（アイコン含む）
- [ ] 行番号 / hl-line / whitespace 表示が正しい

### 補完
- [ ] `M-x` で vertico が起動する
- [ ] `C-s` (my:consult-line) が動く
- [ ] `C-x b` (consult-buffer) が動く
- [ ] `C-x C-r` (consult-recent-file) が動く
- [ ] company (または corfu) の補完が出る

### 編集
- [ ] `C-h` = backward-delete、`C-z` = scroll-down
- [ ] `C-a` の行頭/インデント先頭トグル
- [ ] yasnippet 展開 (`C-<tab>`)
- [ ] `<f2>` で hydra-zoom
- [ ] `C-=` で expand-region

### ファイル/プロジェクト
- [ ] dired が開き、`.` で hydra-dired が出る
- [ ] `<f8>` で neotree
- [ ] `C-c p` で projectile コマンドマップ
- [ ] `C-c p s` で ripgrep 検索

### VCS
- [ ] `M-x magit-status` が動く
- [ ] git-gutter (または diff-hl) のマーカーが出る
- [ ] `C-c g` で hydra-git-gutter

### 各言語モード（該当するもののみ）
- [ ] Emacs Lisp: 補完・eldoc・`C-c RET` マクロ展開
- [ ] Org: 開ける・エクスポート・org-bullets
- [ ] Markdown: `C-c .` で hydra-markdown
- [ ] TypeScript/TSX: 構文強調・LSP/tide・flycheck
- [ ] PHP: LSP (intelephense)・flycheck
- [ ] Python: 補完・flycheck
- [ ] Clojure: cider-jack-in
- [ ] SQL: `C-c "` / `C-c ,`

### Shell
- [ ] `M-x shell` が起動し、日本語が化けない
- [ ] `M-!` (shell-command) が動く
- [ ] `M-x compile` が動く

---

## 7. 次のアクション

1. **意思決定ポイント A〜D についてユーザーに確認する。**（特に A と B）
2. 合意後、フェーズ 0（ベースライン取得）から着手する。
