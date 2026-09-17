<!-- -*- gfm -*- -->

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

@README.md

## この文書の役割

3 つに分けてある。**同じことを 2 か所に書かない。**

| | 読者 | 答える問い |
|---|---|---|
| [README.md](README.md) | 人 | 何ができる / どう入れる / どこに何がある |
| **CLAUDE.md（これ）** | エージェント | **触るときに何を壊すな** |
| [docs/](docs/README.md) | 掘る人 | なぜそうしたか / その根拠の数字 |

**README.md は上で import してある。** モジュール構成・キーバインド・外部依存・
セットアップ手順はそちらに書いてあるので、ここには複製しない。

ここに書くのは次の 4 つだけ。

1. **知らずに書くと壊れ、かつエラーが出ないこと**
2. **決定事項**（再提案すると、その都度ひっくり返そうとして時間を溶かす）
3. **検証の作法**
4. 既知の課題

`【重要】` は 1 のうちでも**とくに静かに壊れる**ものに付けてある。**多用しない。**
全部が重要だと何も重要でなくなる。

新しく分かったことの追記先は、**横断する規約なら §1、1 機能に閉じた詳細なら
`docs/` の該当ファイル**（結論だけ §3 に 1〜3 行 + リンク）。

---

# 1. 作業の規約

## 反映と検証

設定を変えたら、該当モジュールを編集して再起動するか、編集した式を `C-M-x` で
評価する。batch での素通し確認は次の 1 行。

```sh
emacs --batch --debug-init -l early-init.el -l init.el --eval '(message "OK")'
```

**`emacs` が PATH に無いマシンがある。** 実行ファイルの場所はマシンごとに違い、
このリポジトリは Windows / macOS / Linux で共有しているので、**ここには書かない**。
環境側（`CLAUDE_CONFIG_DIR` 配下のメモリ）に置いてある。

**batch 実行でも `recentf` と `history`（savehist）は書き換えられる。**
検証前にバックアップし、終了後に戻すこと。

### 【重要】batch では確かめられないもの

下のどれかに触れる検証は、**GUI で実際に測らないと嘘の結論が出る**。
しかも多くは「エラーにならず、それらしい値が返る」形で外れる。

| 見たいもの | batch だとどうなるか |
|---|---|
| `format-mode-line`（`header-line-format` / `mode-line-format`） | **常に `""` を返す**。`%` の escape も face の生死も分からない |
| フォントの幅・`font-at`・`string-pixel-width` | フォントが無い。`char-width` だけ見ても描画幅は分からない |
| face の実効属性、テーマとの優先順位 | |
| `w32notify` のイベント | **コマンドループ経由で配送されるので 1 件も届かない** |
| `eglot-ensure` の接続 | `post-command-hook` で繋ぐので永久に繋がらない（`(run-hooks 'post-command-hook)` を手で呼ぶ） |
| `flymake` の初回チェック | **バッファが実際に表示されてから**走る。`switch-to-buffer` してから `(flymake-start nil t)` |
| `straight-prune-build` / `straight-remove-unused-repos` | `:if window-system` のパッケージが登録されず、使用中のものまで削除対象になる |
| `locale-coding-system` | PowerShell から起動すると **`cp65001`**（GUI は `cp932`）。cp932 への束縛が no-op になって修正前後の区別がつかない |
| `cua--select-keymaps` | `pre-command-hook` で走るので、`key-binding` は `cua-mode` 有効化時点の値のまま |
| `use-cjk-char-width-table` | `initial-window-system` が nil だと ambiguous を幅 1 に倒す分岐に入る |
| `line-move-visual` の効き | バッファをウィンドウに出さないと折り返しが再現できない |

### 【重要】`emacs -Q` で再現しないものがある

この設定は `w32-get-true-file-attributes` を nil にしている（`my-platform.el`）ため
**inode が常に 0**。`emacs -Q` では正しい値が返るので、**`-Q` で書いたコードを
持ち込むと静かに壊れる**。詳細は §1「Windows 固有」。

### プローブの作法

- **必ず `condition-case` で囲み、`unwind-protect` で `kill-emacs` する。**
  タイマーの中でエラーが出ると GUI の Emacs がそのまま残り、外からは
  「固まった」ようにしか見えない
- **`inhibit-interaction` を立てるか外すかを意識する。** batch の `yes-or-no-p` は
  stdin を待つのでプローブが固まる。逆にミニバッファの読み取りを検証したいときは
  外す必要がある
- **検証用のディレクトリは毎回ユニークな名前で作る。** dired バッファを kill した
  直後は w32notify の watch が握っていて `Permission denied` で消せないことがある

### 【重要】「設定されたか」と「効いたか」は別々に見る

変数の値を確認しても、それが効いていることの証明にはならない。実際に 3 回誤診した。

| | 「設定されたか」 | 「効いたか」 |
|---|---|---|
| `*scratch*` の `trusted-content` | `:all` が入っているか | **`flymake-disabled-backends` が nil か** |
| カレンダーの祝日 | `calendar-check-holidays` が返す値 | **`overlays-in` に `holiday` face があるか**（マークは overlay。`font-lock-face` を見ても分からない） |
| editorconfig の `indent_size` | 値が 4 か | **その変数がバッファローカルか**（グローバル値も 4 なので値だけでは区別が付かない） |

flymake ではもう 1 つ。**バックエンドを外しても、報告済みの診断はそのバックエンドが
再報告しない限り残る。** 「外れているか」はバックエンドの一覧で、「消えたか」は
診断で、別々に見ること。

### 書き換えたときの検算

`:bind` を `:map` 形式に直すときに閉じ括弧を 1 つ余らせると、`use-package` の
フォームがそこで閉じ、後続の `:custom` の各行がトップレベルの関数呼び出しになる
（`void-function (dired-sidebar-theme)` のような形で表面化する）。
括弧のバランスは取れているので **`check-parens` では検出できない。**
**トップレベルのフォーム数を書き換え前と突き合わせる**のが確実。

```sh
emacs --batch --eval '(dolist (f command-line-args-left)
  (with-temp-buffer (insert-file-contents f) (goto-char (point-min))
    (let ((n 0)) (ignore-errors (while t (read (current-buffer)) (setq n (1+ n))))
      (message "%s: %d forms" f n))))' user-lisp/*.el
```

あわせて `docs/_archived/snapshot.el` の前後 diff を取る。defcustom 全変数・
全フック・全キーバインド・face・ロード済み feature を決定的な順序でダンプするので、
同一設定なら差分 0 行になる。

```sh
emacs --batch -l early-init.el -l init.el -l docs/_archived/snapshot.el \
      --eval '(my:snapshot-dump "before.txt")'
```

### 到達不能な設定の検出

```elisp
;; 決してロードされない feature に対する eval-after-load を列挙
(dolist (e after-load-alist)
  (let ((f (car e)))
    (when (and (symbolp f) (not (featurep f)) (not (locate-library (symbol-name f))))
      (princ (format "%s\n" f)))))
```

`locate-library` が通る（= インストール済みだがロードされない）パッケージに対する
`:after` は拾えない。実際に GUI 起動して `(featurep 'FOO)` を確認するのが確実。

---

## 文字コード

→ 詳細と実測: [docs/japanese/encoding.md](docs/japanese/encoding.md)

Windows では `default-process-coding-system` が `(utf-8 . cp932)`。
**cdr は「引数」と「標準入力」の両方を兼ねる。**

| 経路 | どう決まるか |
|---|---|
| `call-process` / `start-process` の**引数** | `default-process-coding-system` の cdr = **cp932**。Emacs のプロセス起動は ANSI API なのでこれが正しい |
| 標準入力 | 同じ cdr。**UTF-8 を要求する相手（pandoc）は `process-coding-system-alist` で個別指定** |
| シェル経由（`M-x grep` / ripgrep / `compilation-start`） | **`process-coding-system-alist` が優先**（`my-shell.el` が `.*sh\.exe` を utf-8 に固定）。個別に `coding-system-for-write` を束縛する |
| `w32-shell-execute`（`my:open-file-externally`） | ワイド API なので影響を受けない |
| url.el（`my-htnblog`）などプロセスを通らない経路 | `encode-coding-string` で明示するしかない |
| claude（`my-claude`）の stdin | 起動時に `(utf-8-unix . utf-8-unix)` を**束縛する** |

### 【重要】日本語で「一致なし」になったら、まずこれを疑う

grep も rg も**エラーを出さず 0 件を返す**。`my:grep-with-cp932` /
`my:ripgrep-with-cp932`（`my-utils.el`）が `coding-system-for-write` を
`locale-coding-system` に束縛して直してある。

**束縛はコマンドではなく `ripgrep-regexp` に張ること。** コピーした
`my:ripgrep-regexp` の中だけで束縛していた時期があり、`projectile-ripgrep` と
`M-x ripgrep-regexp` が漏れていた。

### 【重要】`prefer-coding-system` が後から上書きする

`my-japanese.el` の w32 ブロックにある `(prefer-coding-system 'utf-8-unix)` が
`set-default-coding-systems` 経由で `default-process-coding-system` を
`(CODING . CODING)` に書き戻す。**cdr を変えたいなら、それより後で入れ直す。**

```elisp
(setq default-process-coding-system '(utf-8 . cp932))  ; => (utf-8 . cp932)
(prefer-coding-system 'utf-8-unix)                     ; => (utf-8 . utf-8)
```

w32 ブロックは `:if (eq window-system 'w32)` なので **batch では走らない**。
batch での最終値は前段の `setq` が決める。両方に置いてあるのはそのため。

### 【重要】検証で `call-process` から git にブランチを作らせない

日本語のブランチ名を `call-process` の引数で渡すと**引数の側で化ける**
（3 通り試して全部同じ結果）。読み取り側は正しいのに壊れて見えるので、
検証では `.git/HEAD` を直接書くこと。

---

## use-package と straight

**パッケージ管理は straight.el に一本化**している（`package.el` は
`early-init.el` で無効化済み）。設定の記述は **Emacs 同梱の use-package**。

- 新しいパッケージは該当モジュール内で `(use-package NAME :straight t ...)`
- 組み込みライブラリには `:straight` / `:ensure` を付けない
- Emacs 同梱のものを使いたい場合は `init.el` で
  `(straight-use-package '(NAME :type built-in))` を宣言する（`org` / `transient`）。
  これをしないと依存解決で straight が古い版をビルドして `load-path` に載せる
- `straight/repos/org` と `straight/build/org` があっても `load-path` には載らない
  （`:type built-in` のため）。recipe cache には残るので `straight-prune-build` では
  消えず、手で消す

### 【重要】`init.el` で調整している 3 点

素の use-package のままでは挙動が変わる。**外すと静かに壊れる。**

| 設定 | 外すとどうなるか |
|---|---|
| `use-package-hook-name-suffix` = `nil` | `:hook (foo-mode-hook . f)` が `foo-mode-hook-hook` に登録される |
| `use-package-use-theme` = `nil` | `:custom` が擬似テーマ経由になり、`custom.el` の `user` テーマに負ける |
| `:straight` を `:unless` の直後へ移動 | `:straight` は `use-package-keywords` の先頭に push されるため `:if` より先に処理され、**`:if` が偽でも `straight-use-package` が走る**（Windows で `exec-path-from-shell`、Linux で `w32-ime` / `tr-ime` まで clone / build しにいく） |

### 遅延キーワードが無いブロックには `:defer t` を足す

use-package は遅延キーワード（`:commands` `:bind` `:hook` `:mode` `:after` など）が
1 つも無いと `(require)` を出す。インストールするだけのブロックには `:defer t` を。

`:defer t` を付けると `:config` は `(with-eval-after-load '<name>)` に包まれる。
**そのパッケージを誰もロードしないなら `:config` は永久に走らない**ので、
「ロードせずに実行したい設定」は `:init` に置くこと。

### 【重要】名前は実在する feature にする。疑似パッケージは `emacs`

`:hook` / `:bind` / `:mode` などがあると `:config` は
`(eval-after-load '<パッケージ名>)` に包まれる。**名前が実在する feature で
ないと `:config` も `:bind` も永久に適用されない。**

- 実在する feature 名を使う（例: `sql-mode` ではなく `sql`）
- OS 別のまとまりなど**疑似パッケージには `emacs` を使う**。`(require 'emacs)` は
  no-op、`(with-eval-after-load 'emacs ...)` は即実行されるので安全

### 【重要】`:custom` にマイナーモードの変数を書く場合

`customize-set-variable` は `(get VAR 'custom-set)` が未設定のとき `set-default` に
フォールバックするため、**パッケージが未ロードだと変数に `t` が入るだけで
モード関数が呼ばれない**。`:demand t` でロードした上で `:config` から明示的に呼ぶ。

実例: `(corfu :custom (global-corfu-mode t))` では corfu が読まれず補完が出なかった。
`editorconfig-mode` も同じ。

ただし autoloads に `custom-autoload` が入っている変数（`cua-mode`、
`global-whitespace-mode`、`yas-global-mode` など）は動く。
**動いていることが正しさの証拠にならない**点に注意。

### `:custom-face` は使わない（テーマに負ける）

| 方法 | modus-vivendi が同じ face を定義しているとき |
|---|---|
| `custom-set-faces` | `theme-face` に `user` が積まれ **自分の指定が勝つ** |
| use-package の `:custom-face` | **テーマが勝ち、指定が消える** |
| `face-spec-set` に spec-type `user` を明示 | 同上、**消える** |

`:init` から `custom-set-faces` を直接呼ぶ形にしてある（`diff-hl` /
`highlight-indent-guides` / `doom-modeline` の 3 箇所）。

### `require` できないものには `:no-require t`

use-package は **`require` に失敗すると `:config` ごと実行しない**。
`modus-themes` は `etc/themes/` にあり `load-path` に載っていないため
`(require 'modus-themes)` は失敗する。`:no-require t` が無いと `load-theme` が
呼ばれず、テーマが一切適用されない。

→ leaf からの移行時の対応表: [docs/refactoring/leaf-to-use-package.md](docs/refactoring/leaf-to-use-package.md)

→ 棚卸し・掃除・更新の手順: [docs/packages/straight-maintenance.md](docs/packages/straight-maintenance.md)

---

## `user-lisp/` の扱い

Emacs 31.1 の `user-lisp/` は、既定では `package-activate-all` の直後・
`init.el` の読み込み**前**に `prepare-user-lisp` が走る。その時点では straight の
ブートストラップが済んでおらず `use-package` も未初期化なので、モジュールが
壊れた `.elc` にコンパイルされる。そのため:

- `early-init.el` で `user-lisp-auto-scrape` を `nil` にして自動実行を止める
- `init.el` で straight と use-package を用意したあと `(prepare-user-lisp t)` を
  明示的に呼ぶ。**第 1 引数 JUST-ACTIVATE が `t` = バイトコンパイルしない**

**バイトコンパイルしないのは決定事項。** 速くならず、`load-prefer-newer` が既定
nil であることと `C-M-x` で評価しながら書く運用が噛み合わなくなる。
→ 実測: [docs/refactoring/no-bytecompile.md](docs/refactoring/no-bytecompile.md)

モジュールを追加した場合は `init.el` の `require` 列に加える。

---

## `setq` と `setq-local`

### 【重要】モードのフックの `setq` はグローバル値を潰すことがある

`line-move-visual` は**自動バッファローカルでない**ため、フックの中の `setq` は
`default-value` を書き換える。**`.md` を一度でも開くと、そのセッションの全バッファで
`C-n` / `C-p` が論理行移動になっていた**（2026-09-14 に発見・削除）。

判定は `local-variable-if-set-p`。

| 変数 | `local-variable-if-set-p` | フック内の `setq` の効き先 |
|---|---|---|
| `truncate-lines` / `word-wrap` / `fill-column` | **t** | そのバッファだけ |
| `indent-tabs-mode` / `tab-width` / `case-fold-search` | **t** | そのバッファだけ |
| **`line-move-visual`** | **nil** | **全バッファ** |
| **`scroll-margin`** | **nil** | **全バッファ** |

**バッファ単位に効かせたいなら必ず `setq-local` と書く。** `setq` でよいのは
本当にグローバルに効かせたいときだけ。

**この壊れ方は値を見ても気づけない。** そのバッファでは望みどおりの値になって
いるので、フックも変数も正しく見える。おかしいのは**別のバッファ**で、しかも
`.md` を開くまでは正常なので、原因と症状が時間的にも場所的にも離れる。

### 【重要】存在しない変数への `customize-set-variable` は黙って通る

`mark-holidays-in-calendar` は Emacs 23 で `calendar-mark-holidays-flag` に
改名され obsolete alias も無い。`customize-set-variable` は defcustom でない変数にも
`set-default` するので、**警告も出ないまま同名の変数が 1 つ増えるだけ**だった。
同じことが `cape-dabbrev-min-length`（cape から変数自体が消えた）でも起きていた。

**見分け方は `(get 'VAR 'custom-type)` が nil かどうか。**
`boundp` は自分で作ってしまった変数にも t を返すので判定に使えない。

---

## `custom.el` とテーマの優先順位

読み込み順は **`custom.el` → `user-lisp/` の各モジュール**。同じ変数を両方で
設定すると **`user-lisp/` 側が勝つ**。`custom.el` に書いても効かないので、
設定は `user-lisp/` に置くこと。

`custom.el` に残してあるのは 4 変数だけ（`safe-local-variable-values` /
`warning-suppress-log-types` / `warning-suppress-types` / `yas-new-snippet-default`）と、
`rst-level-1`〜`6` の 6 面。

**face がテーマに勝つかどうかはロード順で決まる。** テーマより先に定義済みの face
（`font-lock-*` など）はテーマが勝ち、`custom.el` に書いても効かない。テーマより後に
ロードされるパッケージの face は `custom.el` 側が勝つ（`rst.el` がこれ）。
確実に当てたいときは `load-theme` のあとに設定する。

`customize` を使うと `custom.el` に書き戻されるので、モジュール側と重複して
いないか時々確認する。`custom.el` の `custom-set-variables` から変数名を集め、
`user-lisp/` の `use-package` を `macroexpand-1` して出てくる
`customize-set-variable` と突き合わせればよい。

---

## Windows 固有

### 【重要】この設定では `file-equal-p` が使えない

`my-platform.el` が `w32-get-true-file-attributes` を `nil` にしている
（`file-attributes` を速くするため）。すると **inode が常に 0 で返る**。
`file-equal-p` は inode とボリューム ID の組で比べるので、**同じドライブにある
ファイル / ディレクトリはすべて「同じ」と判定される。**

```elisp
(file-attribute-file-identifier (file-attributes "…/b-project/"))  ; => (0 2431202897)
(file-equal-p "…/b-project/" "…/y/src/")                           ; => t
```

**`emacs -Q` では再現しない。** 実際に `my-claude.el` で別プロジェクトの `C-c a a` が
同じセッションに解決された。

パスの同一判定は**文字列で**行う。`expand-file-name` + `file-name-as-directory` で
揃え、`file-name-case-insensitive-p` が真なら `string-equal-ignore-case`。
stat を打たないので速くもある。

`file-truename` は inode を見ないのでこの問題を踏まない（`my-dired.el` の
`_assets/` 整合性チェックが使っている）。

### 【重要】ドライブレターの大小が食い違う

Emacs は**子プロセスの作業ディレクトリのドライブレターを小文字にする**。
`default-directory` を大文字にしても変わらない（`make-process` が
`directory-file-name` と同じ経路で組み立てるため）。**Lisp 側に逃げ道は無い。**

| 式 | 値 |
|---|---|
| `(expand-file-name "C:/Users/masao/.emacs.d/")` | `C:/...`（明示した大文字は保つ） |
| `(expand-file-name "~/.emacs.d/")` | **`c:/...`** |
| `(directory-file-name "C:/Users/masao/.emacs.d/")` | **`c:/...`** |

逆に gopls は `publishDiagnostics` の uri を大文字で返す。どちらも「文字列 `equal`
で突き合わせるところ」で静かに外れる。

- claude: `cmd.exe /d /c cd /d <PATH> && ...` を挟んで大文字に正規化する
  → [docs/claude/my-claude.md](docs/claude/my-claude.md)
- gopls: `eglot-uri-to-path` に `:filter-return` advice で小文字へ揃える
  → [docs/lsp/eglot-and-flymake.md](docs/lsp/eglot-and-flymake.md)
- `directory-abbrev-alist` は `abbreviate-file-name` が `case-fold-search` を
  `(file-name-case-insensitive-p filename)` に束縛するので、**ここだけは自動で吸収される**

### `call-process` が遅い（未解決）

同じ `cmd.exe` を起動するのに PowerShell が約 20 ms、Emacs の `call-process` は
**59〜76 ms**。約 40 ms が Emacs 側のプロセス生成経路のコスト。
`gitd/` はこれを迂回するだけで、直してはいない。

そのため **Windows では「子プロセスを起こさずに済ませられないか」を先に考える。**
実例: git のブランチ名は `.git/HEAD` を読めば取れる（`call-process git` の 55.6 ms に
対し 0.061 ms で **1300 倍**）。

### `w32notify` はイベントを落とす

1000 ファイル作成に対しイベントは **4095 件**しか届かなかった
（`ReadDirectoryChangesW` のバッファ溢れ。1 万件が期待値）。
**イベントの完全性に依存した設計にはできない。**

### `HOME` は環境変数で設定する。設定側で `setenv` しない

`init.el` が読まれる時点で `.emacs.d` の探索は終わっているので手遅れ
（`early-init.el` でも同じ）。しかも `user-emacs-directory` は展開前の
`"~/.emacs.d/"` という文字列のままで、Windows の Emacs は `expand-file-name` の
たびに `HOME` を読み直すため、途中で差し替えると `recentf` / `custom.el` /
`straight` の保存先が実際に動いている設定とは別のディレクトリになる。

### IME

`M-\`` と `M-kanji` を `ignore` にしているのは**意図的**。その組み合わせは
tr-ime / Windows 側が IME のトグルとして処理するので、Emacs 側では何もしないのが
正しい。Emacs から切り替えるのは `C-\` と 漢字キー。

**モードラインの IME 表示は `w32-ime-input-method-title` で設定する。**
`w32-ime-mode-line-state-indicator` は w32-ime が自前で `mode-line-format` の先頭に
差し込むための変数で、`mode-line-format` をまるごと差し替える doom-modeline とは
併用できない。

### OS 判定

`(eq system-type 'windows-nt)` / `'darwin` / `'gnu/linux`。
ウィンドウシステムは `window-system` の `'w32` / `'ns` / `'x` / `'pgtk`。

---

## lexical-binding

`early-init.el` / `init.el` / `user-lisp/` すべて `t`。新しいモジュールも `t` で書く。

バイトコンパイルしない方針なので、lexical 化の検証は**一時ディレクトリにコピーして
コンパイルし `*Compile-Log*` を読む**。GUI 起動して全パッケージがロードされた状態で
やらないと、パッケージ由来のマクロが未定義で偽の警告が大量に出る。

`reference to free variable` / `assignment to free variable` の大半は
「そのパッケージがコンパイル時に未ロード」というだけで実害はない。注意すべきは
`Unused lexical variable` と、呼び出し元の `let` 束縛を読んでいたクロージャ。

---

# 2. 決定事項（再提案禁止）

調べ直した結果として**そうしないことに決めた**もの。理由は各リンク先にある。
**同じ提案を繰り返さないこと。**

| 決定 | いつ | 理由（要約） |
|---|---|---|
| **`user-lisp/` / `site-lisp/` をバイトコンパイルしない** | 2026-09-05 | `.elc` あり 0.993 s / なし 0.995 s。**得るものが 0 ms** で、失うものは具体的にある → [docs](docs/refactoring/no-bytecompile.md) |
| **`site-lisp/eaw.el` を残す** | | 組み込みの `cjk-ambiguous-chars-are-wide` では **1496 文字足りない**。桁揃えが成立する 398 文字のうち **84% で eaw のほうが実描画と一致** |
| **`site-lisp/cp5022x.el` を残し、MELPA 版に乗り換えない** | 2026-09-09 | Emacs 同梱の `cp51932.el` は**翻訳テーブルだけ**で `define-coding-system` が無い。MELPA 版は**バイト単位で同一**（upstream は 2012 年で停止、fork 3 つも中身同じ）。乗り換えると lexical-binding cookie を失って警告が増えるだけ |
| **外部の `go-mode` を入れない** | | `go-ts-mode.el` の autoload と競合し、**tree-sitter 版に一生切り替わらない**。保険が要るなら `treesit-enabled-modes` に `go-ts-mode` を入れる → [docs](docs/languages/go.md) |
| **`global-auto-revert-non-file-buffers` は使わない** | 2026-09-04 | 効き先が dired の外へ広がる。magit の更新は自前に組んであり並走させたくない。`dired-mode-hook` からバッファローカルに `auto-revert-mode` |
| **magit の遅さに Defender 除外 / `core.fsmonitor` / git ラッパ回避は効かない** | 2026-09 | 原因は git ではなく Emacs のプロセス生成。3 つとも試して有意差なし |
| **`magit-status-sections-hook` を削らない** | 2026-09 | 16 → 6 で 1669 → 1001 ms。表示を犠牲にする割に効かない |
| **段階 2c（監視を常駐プロセスへ）は見送り** | 2026-09 | 速度目標はキャッシュと並列化で達成済み。移す価値は macOS / Linux 対応と溢れ検知にある |
| **dired の自動更新を `my-dired-watch` に一本化しない（段階 2 は不採用）** | 2026-09-15 | 実装して測った上で戻した。**簡単にならない**（w32notify 依存で Windows 専用なので `my-dired.el` の分岐も autorevert を返す後始末も消せず、差し引き +66 行）。**反映が 4.3 ms → 335 ms** に遅くなる（autorevert は通知で即座に revert、こちらは 0.3 秒のデバウンス待ち）。`dired-sidebar` の throttle（1.5 秒アイドル・可視時のみ）も通らなくなる。得るのは watch 1 本（張るコストは 0.058 ms）だけ |
| **dired の追従を常駐プロセス（gitd 統合）にしない** | 2026-09-15 | コストは**全部 Emacs の中**にある。4862 件で 648 ms のうちデーモンが肩代わりできるのは stat の 98 ms だけで、残りは ls-lisp の整形・挿入・アイコン。**検知そのものも既に Emacs まで届いている**（autorevert が捨てているだけ）。統合すれば `my:gitd--disabled` と `PROTOCOL` を共有し、**git の失敗で dired が止まる** → [docs](docs/dired/dired-extensions.md) |
| **elpaca へは移行しない（straight のまま）** | 2026-08 | 設定本体が use-package なら `:straight` の 1 行を差し替えるだけで済む |
| **`:custom-face` は使わない** | | テーマに負ける（§1「use-package と straight」） |
| **`window-configuration` は退避しない**（my-claude） | | 最大化トグルの復帰先も `C-c a l` も同じ関数を呼ぶだけなので、どこから何度押しても同じ形に落ち着く |
| **画像の送信で「プレースホルダが消えている」と警告しない**（my-claude） | | 貼り直しは「消す → 貼る」の 2 手なので、正常な操作のたびに必ず出ていた |
| **golangci-lint を flymake に載せない** | | 1 回が重く `flymake-no-changes-timeout`（1.0 秒）で回す用途に向かない。`C-c C-l` で `compile` |
| **`elisp-flymake-byte-compile-load-path` に `user-lisp/` を足さない** | 2026-09-09 | 偽診断が別の偽診断に入れ替わるだけで、所要 0.51 → 1.10 秒、`recentf` / `history` を毎回書き戻す副作用が増える → [docs](docs/lsp/eglot-and-flymake.md) |
| **`libxml-parse-xml-region` に切り替えない**（my-htnblog） | | 名前空間の prefix を落とすので `app:control` が取れなくなる。速度差は 0.001 秒 |
| **`my:pty-console-font` の既定は nil（フォントを切り替えない）** | | 切り替えると `char-width-table` と同じく **Emacs 全体**のフォントが変わり、編集中のバッファまで巻き込む |

---

# 3. モジュール別の注意

各モジュールについて、**書く前に知っておかないと壊すこと**だけ。
設計と実測は `docs/` にある。

## `my-core` — tree-sitter の差し替え

→ [docs/languages/tree-sitter.md](docs/languages/tree-sitter.md)

- **`my:treesit-remap` は必ずトップレベルで呼ぶ。** `:config` は
  `(eval-after-load '<パッケージ名>)` に包まれるので、そこで差し替えても
  「その回に開いたバッファ」には間に合わない。さらに差し替えが効くと従来のモードは
  もうロードされないため、**`:config` は二度と実行されない**
- **`*-ts-mode` は従来モードのフックを継承しない。** `:hook` は
  `((foo-mode-hook foo-ts-mode-hook) . func)` の形で両方に張る
- **フォントロックやインデントの設定はモードごとに別物。** `csharp-mode` は
  cc-mode 派生（`c-set-offset`）、`csharp-ts-mode` は tree-sitter 派生
  （`csharp-ts-mode-indent-offset`）。セットアップ関数を分けること
- **`.tsx` の `auto-mode-alist` 登録は web-mode のブロックより後に置く**
  （`:mode` が先頭に積むので、前に置くと web-mode に負ける）
- `my-core.el` は `treesit-language-source-alist` を `setq` で丸ごと上書きする。
  **`go-ts-mode.el` が `add-to-list` する go / gomod / gowork を、commit ハッシュまで
  一致させて書いておくこと。** 1 文字でも違うと `equal` 判定をすり抜けて二重登録になり
  2 回ビルドされる

## `my-japanese` — eaw / cp5022x

→ [docs/japanese/eaw-and-cp5022x.md](docs/japanese/eaw-and-cp5022x.md)

どちらも組み込みでは足りないので残す（§2）。

- **ambiguous 幅の計測は必ず GUI で行う。** batch では `initial-window-system` が nil の
  ため `use-cjk-char-width-table` が幅 1 に倒す分岐に入り、組み込みのカバー範囲を
  過小評価する（2170 ではなく 1424 に見える）
- `my-japanese.el` が `(define-coding-system-alias 'euc-jp 'cp51932)` と
  `set-coding-system-priority` で cp5022x を使っている
- **Emacs 本体に取り込まれたかは `(featurep 'cp5022x)` では分からない**
  （site-lisp 側が必ず先に provide する）。`emacs -Q` で見ること

```sh
emacs -Q --batch --eval '(message "%S %S" (coding-system-p (quote cp51932)) (coding-system-p (quote cp50220)))'
# 31.1 では nil nil
```

## `my-appearance` — フォント・テーマ・モードライン

→ [docs/appearance/fonts-theme-modeline.md](docs/appearance/fonts-theme-modeline.md)

### フォント名を決め打ちしない

`my:nerd-font-family` が `font-get-glyphs` で**実際のグリフ有無を見て選ぶ**。
名前で決め打ちすると、Nerd Fonts v2 のフォント（`HackGenNerd` など）を掴んで
アイコンが全滅する。Material Design アイコン（第 15 面 `U+F0001`〜）と
seti 上位（`U+E6AD`）を持つのは `Symbols Nerd Font Mono` だけ。

### 日本語フォントの全角/半角ピッチはサイズで 1px ずれる

HackGen は「全角＝半角×2」で設計されているが、Windows 実測:

| `:height` | 半角 | 全角 | |
|---|---|---|---|
| 110 / 113 / 116 | 8 | 16 | 一致 |
| **120 / 124** | 8 | **17** | **ずれる** |
| 128 / 130 | 9 | 18 | 一致 |

11.6（= 116）にしてある。**`face-font-rescale-alist` では直せない**
（ASCII と日本語が同じフォントなので両方が同じ比率で縮む）。確認は
`(string-pixel-width "あ")` と `(string-pixel-width "aa")` の比較で。

### `set-fontset-font` はこの設定では効かない

`nil` にも `t` にも入れ、`clear-face-cache` と `redraw-display` まで呼んでも
`font-at` は元のフォントを返し続ける。**実際に効く経路は
`set-face-attribute 'default nil :family`**（`emacs-font-setting` と同じ）。
したがって**レンジごとの割り当てはできない**。

**Nerd Font のアイコン領域（`#xe000`-`#xf8ff`）は触らない。**
上書きするとアイコンが豆腐になる。

### テーマ（modus-themes 5.2.0、Emacs 31.1 同梱）

`:straight` は付けない（組み込み優先。`org` / `transient` と同じ扱い）。
**`:no-require t` が必須**（§1）。色の調整は
`modus-themes-common-palette-overrides` で行う。パレット名は
`etc/themes/modus-themes.el` の `modus-vivendi-palette` を見る。
`:custom` は `:config` より先に走るので上書きは `load-theme` に間に合う。

v2 世代の API（`modus-themes-load-themes` / `-load-vivendi` / `modus-themes-region`）は
5.x には存在しない。

### doom-modeline

- 背景色は**modus のパレット上書き**で指定する。Emacs 29 以降 `mode-line` とは別に
  **`mode-line-active`** があり、テーマはそちらを塗るので `mode-line` だけ変えても効かない
- 左端のバー（`doom-modeline-bar`）はテーマのアクセント色なので `custom-set-faces` で別途
- **セグメント名はバージョンで変わる。** 4.x で `checker` は `check` に改名された。
  古い名前が残っていると `doom-modeline--prepare-segments` が落ち、
  **モードライン自体が有効にならない**。使える名前は
  `doom-modeline-segments.el` の `doom-modeline-def-segment` を grep する
- eglot セグメントが Emacs 31.1 で無くなった関数を呼ぶので差し替えてある（下記 `my-lsp`）

## `my-completion` — vertico / consult / corfu

→ [docs/editing/completion.md](docs/editing/completion.md)

### 【重要】`consult-source-recent-file` は開いているファイルを落とす

組み込みのソースは `:items` の中で `consult--buffer-file-hash` を引き、
**既にバッファで開いているファイルを一覧から除外する**。`consult-buffer` では
正しいが、**ファイルを開く入口でこれをやると、開いているものだけ選べなくなる。**

`C-x C-r`（`my:consult-recent-file-or-bookmark`）は `recentf-list` をそのまま出す
`my:consult--source-recent-file` を使う。**差が「いま開いている数」なので、
開いていなければ気づけない**（実測で 195 対 194）。

`consult--multi` は autoload されていないので、コマンドの側で `(require 'consult)`。

### capf のトラブルは「どこで打ち切られたか」を見る

corfu は `run-hook-wrapped` で capf を前から回し、最初に候補を返したところで止まる。

- **後ろの capf でエラーが出ても、前で候補が出ていれば表示は正常。**
  「補完は効いているのにエラーが出る」ときはこれ
- 逆に**前の capf がエラーを投げると、後ろの候補ごと失われる**

再現は corfu と同じ経路を通すのが確実（`completion-at-point` を対話的に呼ぶと
`corfu--capf-wrapper` を経由しないので条件が変わる）。

```elisp
(corfu--protect
 (lambda ()
   (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper 1)))
```

戻り値の `car` が採用された capf。

### `text-mode` の ispell 補完は切ってある

`ispell-alternate-dictionary` の既定値は `/usr/dict/words` などを探す `cond` なので
**Windows では必ず nil** になり `ispell-lookup-words` が `error` を投げる。
corfu がそれを拾って `*Messages*` に backtrace を流し、`message-log-max` が 1000 なので
**他のメッセージがほぼ全部押し流される**（実測で 1000 行中 915 行がこれ）。

`my-text.el` で `text-mode-ispell-word-completion` を nil にして capf ごと外してある。
**変数を変えても、既に text-mode 派生になっているバッファには効かない**
（`add-hook` はモードを立てた時点で済んでいる）。

## `my-editor` — editorconfig / C-e

→ [docs/editing/editorconfig.md](docs/editing/editorconfig.md) / [docs/editing/line-movement.md](docs/editing/line-movement.md)

### editorconfig（Emacs 30 で本体入り。`:straight` は付けない）

- **効くのは `.editorconfig` があるディレクトリだけ。** 上へ辿って探すので、
  置き場所がそのまま効き先になる。プロジェクト直下に置くと兄弟プロジェクトに届かない
- フックを 2 つ足すだけ（`hack-dir-local-get-variables-functions` /
  `auto-coding-functions`）。**dir-local として通るので `.dir-locals.el` のほうが優先**、
  メジャーモードのフックが `setq` したものには勝つ
- **`indent_size` は `csharp-ts-mode` には届かない。** Emacs 31 では
  `csharp-ts-mode` が `csharp-mode` の派生なので `editorconfig-indentation-alist` の
  `(csharp-mode c-basic-offset)` に当たり、**ts 版が見ない変数**に入る。
  `my:csharp-ts-mode-setup` で
  `(setq-local editorconfig-indent-size-vars '(csharp-ts-indent-offset))` を足してある
- **`end_of_line` は BOM 付きファイルには効かない**（`find-auto-coding` が先に
  `auto-coding-regexp-alist` を見る）。逆に BOM 無し CRLF のファイルは `^M` が
  バッファに残る（壊れてはいない）
- 範囲を絞る defcustom（`editorconfig-exclude-regexps` / `-exclude-modes`）は
  本体に入るときに落ちた
- **`:custom` に `editorconfig-mode` を書いてはいけない**（§1）。`:demand t` + `:config`

### `C-e`（`my:end-of-visual-line`）は 3 つの罠を踏んでいる

素の `end-of-visual-line` をそのまま使うと壊れる。

1. **`truncate-lines` が t でも右端で止まる。** `(vertical-motion (cons (window-width) 0))`
   だけなので、折り返さないバッファでも x = ウィンドウ幅で止まる。
   「折り返していなければ `move-end-of-line` と同じ」は**誤り**
2. **`word-wrap` が nil のとき、折り返し位置で次の視覚行の先頭に着く。**
   point としては行末なのに、ブロックカーソルは次の行の 1 桁目に描かれる。
   判定は空白の有無ではなく**「視覚行の先頭に着いたか」**で行う
3. **畳んだ領域を飛び越える。** org の畳んだ見出しでは改行ごと不可視になり
   見出しと配下が 1 視覚行になるので、**サブツリーの末尾**に着く。
   論理行の末尾を越えたときだけ引き戻す

**org の remap は当てにできない。** org は `move-end-of-line` を `org-end-of-line` に
remap して避けているが、`C-e` を別のコマンドに張り替えると remap を経由しない。

**桁は自分で数えないこと。** `display-line-numbers-mode` が有効だと実際に使える幅が
減る（`current-column` は 139 なのに描画は col 143）。

## `my-dired`

→ [docs/dired/dired-extensions.md](docs/dired/dired-extensions.md)

### 【重要】`dired-x` は `dired-mode-map` を無条件で書き換える

ロードされた瞬間にトップレベルの裸の `define-key` で `F` / `V` / `M-!` / `M-(` /
`C-x M-o` を奪う。`defcustom` による切り替えは無い。`use-package dired` の `:bind` は
dired のロード時に張られるので、あとから `dired-x` が読まれると負ける。

`dired-x` は明示的に require したつもりが無くても読まれる（`dired-sidebar` の
`:config` と、サイドバーの `a`）。つまり「**`F8` を一度でも押すと、そのセッションでは
以後 `V` が効かなくなる**」という壊れ方をしていた。

`:defer t` + `:config` の `use-package dired-x` で張り直してある。
**`:bind` では駄目**（`dired-x` のロードとは無関係に張られて上書きを取り返せない）。

**`dired-mode-map` に置いたキーが効かないときは、まず `dired-x` を疑う。**

### サイドバーは `dired-find-file` を通らない

`RET` / `C-o` / `mouse-2` の 3 つとも `dired-sidebar-find-file` を通るので、
キーではなくそこに `:around` advice を張る。**`orig` を呼ぶ前に判定すること**
（あの関数はファイルに対して `split-window` までする）。

### 【重要】`dired-readin` は mtime を一覧の**後**に記録する（消えた行が残る）

`set-visited-file-modtime` を `dired-readin-insert` の後で呼ぶので、その間に
ファイルが消えると「古い一覧 + 最新の mtime」になり、`dired-buffer-stale-p` が
**以後永久に nil を返す**。vim の `writebackup`（`a.txt~` を作ってすぐ消す）で
実際に踏んだ。`my:dired-readin-modtime-fix` が `:around` で mtime を読み込み前の
値に差し替えてある。**この advice を外すと、消えたファイルの行が `g` を押すまで
残る。**

あわせて、**ディレクトリの mtime は遅れて更新される**。0.4 秒空ければ作成も
削除も必ず反映されるが、通知の直後に測ると落ちることがある（実測）。

### サイズ・日時の追従は `my-dired-watch`（別モジュール）

autorevert が拾うのは**行が増減する変化だけ**（`created` / `renamed` /
`deleted`）。中身・サイズ・日時・属性は `my-dired-watch` が別経路で拾い、
**その行だけ `dired-relist-entry` で貼り替える**。

- **貼り替えの間は `dired-after-readin-hook` を nil に束縛する。**
  `nerd-icons-dired--refresh` はバッファ全体を舐め直すので、1 行しか変えて
  いなくても 4862 件で 453 ms 払う（外すと 0.71 ms）
- **そのぶんアイコンは自分で付け直す。** nerd-icons のアイコンは行の上の
  overlay（`evaporate` が t）で、`delete-region` で消える
- **`dired-relist-entry` を呼ぶ前に `buffer-read-only` を見る。**
  あの関数は自分で `buffer-read-only` を nil に束縛するので、wdired 中でも
  素通しで書き換えてしまう
- **行が無ければ何もしない。** 無いと `dired-add-entry` が行を作るが、それは
  新規ファイルの追加（autorevert の担当）で、しかもアイコンが付かない
- **サブディレクトリの行は追従しない**（非再帰の `ReadDirectoryChangesW` は
  配下の変化を親に報告しない。実測で 0 件）。`subtree` を足せば拾えるが、
  ビルド中に毎秒数千件を呼び込むので取らない

### `diff-hl-dired` の再入に注意

vc-git の `dir-status-files` はプロセスを 6 回前後リレーし、Windows では 1 チェーン
0.5〜1 秒かかる。auto-revert で再入すると**両チェーンが同じ一時バッファを
`erase-buffer` し合う**。`my-vc.el` に 2 つ対処が入っている。

### 【重要】VC マークが更新されるのは「一覧を読み直したとき」だけ

`diff-hl-dired-update` は `dired-after-readin-hook` にしか載っていない。
**行が増減しない変化（commit / stage / checkout、既存ファイルの書き換え）では
マークが古いまま残る。** 上流の `diff-hl-magit-post-refresh` は
`buffer-file-name` を持つバッファしか見ないので dired を埋めてくれない。

`my-vc.el` の `my:diff-hl-dired-update-repo` を `magit-post-refresh-hook` と
`vc-checkin-hook` に載せて、**そのリポジトリ配下の表示中の dired バッファ**で
取り直している（段階 1）。**表示していないバッファと外部の git には追従しない**
（段階 2 は未着手）。

`diff-hl-dired-update` は**テキストを触らない**（overlay の消去と貼り直しだけ）
ので、`my-dired-watch` と違って行単位にする必要は無い。むしろ
`diff-hl-dired-clear` がバッファ全体を消すところから始まるので**できない**。
→ [docs](docs/dired/dired-extensions.md)

## `my-text` — org / markdown

→ [docs/text/org-extensions.md](docs/text/org-extensions.md) / [docs/text/markdown.md](docs/text/markdown.md)

### 【重要】`org-element` だけに頼らない（`_assets/` の整合性チェック）

- **必ず `org-with-wide-buffer` で見る。** ナローイングされたバッファで
  `org-element-parse-buffer` を呼ぶと見えている範囲しか解析されず、範囲外から
  リンクされているファイルを消してしまう
- **ファイル名がバッファ内に文字列として現れるかも見る。** `org-element` は
  コメント行や例示ブロックの中のリンクを拾わないので、それだけだと
  「コメントアウトして退避してある画像」を消す
- `directory-files` の MATCH に文字列先頭アンカー入りの正規表現を書かない。
  エスケープを 1 つ落としても静かに「1 件も一致しない」になり、
  **全リンクが「リンク先が無い」と誤判定される**

### `markdown-preview` と `markdown-open` は別経路

| | 経路 |
|---|---|
| `C-c C-c p`（ブラウザ） | pandoc で HTML 化 → `browse-url-of-buffer` |
| `C-c C-c o`（外部エディタ） | `save-buffer` → `call-process` に**元の `.md` のパスを渡すだけ** |

`.md` そのものを渡したい相手（MarkText / Typora）は後者。**新しいコマンドを
作る必要は無い。** `markdown-open-command` は**文字列ではなく関数**
（`my:markdown-open-external`）を渡している。文字列だと `markdown-open` 自身が
`call-process` するので、文字コードを束縛する隙が無い。

**この壊れ方は何の手がかりも残さない。** MarkText は受け取ったパスを黙って捨て、
`start` 経由なので終了コードも必ず 0。「MarkText は起動するが空白」だけが見える。

### `#+FOLD_REGION:` — isearch が overlay を残す

`isearch-invisible` の既定は `open` なので、畳んだ中に検索が入ると isearch は範囲を
一時的に開く。このとき **overlay は消えず `invisible` プロパティだけが nil になる**。
**「overlay があるか」で判定してはいけない**（判定を誤って「そのバッファでは二度と
畳めない」状態になった）。

検証での注意: **`#+FOLD_REGION` は "OLD" を含む。** `case-fold-search` は org バッファで
t なので、プローブに `(search-forward "old")` と書くとキーワード行にマッチして誤診する。

### org のアーカイブ先 `#YM`

`org-archive--compute-location` への `:filter-args` advice。旧実装が使っていた
`org-extract-archive-file` は org 9.8 で削除された。後継は戻り値が `(FILE . HEADING)` の
cons なので `:filter-return` は使えず、**入口を `:filter-args` で押さえる**形にしてある。

## `my-lsp` — eglot / flymake

→ [docs/lsp/eglot-and-flymake.md](docs/lsp/eglot-and-flymake.md)

### 【重要】上流の非互換で eglot が黙って壊れる

`eglot--maybe-activate-editing-mode` は `(eglot--managed-mode)` →
`(eglot--signal-textDocument/didOpen)` の順に呼ぶ。**フックの中でエラーが出ると
`didOpen` が送られない**。接続は成立してモードラインにも出るのに、サーバはバッファの
存在を知らないため診断も補完も一切出ない、という分かりにくい壊れ方をする。

実例: doom-modeline 4.3.0 の eglot セグメント（`my-appearance.el` で差し替え済み）。
**同種の症状が出たら、まず `eglot--managed-mode-hook` の中身を疑う。**

### 【重要】診断だけ出ないときはサーバが返す uri の綴りを疑う

gopls は大文字のドライブレターで返す（§1「Windows 固有」）。
`eglot-events-buffer-config` を一時的に有効にして `publishDiagnostics` の uri を見る
（既定では `:size 0` で記録されない）。

### elisp の flymake は「信頼されたバッファ」でしか動かない

`trusted-content` の例外は `user-init-file`（`init.el`）だけ。`early-init.el` /
`user-lisp/` / `site-lisp/` は `my-lsp.el` の `:custom` で登録してある。
`~/.emacs.d/` を丸ごと信頼させると `straight/repos/` まで対象になるので広げない。

**`*scratch*` の設定は `prog-mode-hook` に depth `-100` で載せる。**
`flymake-mode` は有効化した時点でチェックを 1 回走らせるが、`run-mode-hooks` は
**親のフックを子のフックより先に**回すので、`lisp-interaction-mode-hook` では
間に合わない。`init.el` で `elisp-flymake-byte-compile` を外す処理も同じ理由で
`prog-mode-hook` + depth `-100`。

### php-mode は 1.28 で cc-mode 依存が外れた

`c-set-style` / `c-basic-offset` は使えない。インデントは `php-mode-coding-style`。

## `my-vc` / `my-gitd` / `my-magit-watch`

→ [docs/magit/gitd-and-autorefresh.md](docs/magit/gitd-and-autorefresh.md)

`gitd` を触るときに最低限知っておくこと。

- **`magit-process-file` の `BUFFER` に整数（`0`）が来る。** `magit-run-gitk` が使う
  「非同期・出力破棄」の意味。同期実行すると **gitk を閉じるまで Emacs が固まる**
- **`default-directory` は必ず `expand-file-name` する。** Emacs は `~/...` に
  略記することがあり、Rust の `current_dir` は `~` を展開しない
- **キャッシュの無効化は「通知」ではなく「トークン」。** 通知を 1 つ落とすと
  そのリポジトリが永久に古いままになるが、トークンなら Emacs 側だけで閉じる。
  **監視が動いていなければキャッシュも先読みも行われない**（寿命の従属が最大の安全弁）
- **読み取りだけの git もファイル変更イベントを出す**（`status --porcelain` ですら
  `index.lock` を作る）。これを見落として自動更新が 1 回も走らなくなった
- **`magit-refresh-buffer` を 1 回走らせるだけで毎回きっちり 7 件**のイベントが出る。
  自分の書き込みか外部の変更かは**時刻では区別できない**ので、`.git/index` と
  `.git/HEAD` の `(mtime . size)` で見る
- **`check-ignore` に `magit-git-global-arguments` をそのまま使わない**
  （`-z` と `--literal-pathspecs` の両方で fatal になる）。握り潰すと
  「何も無視されない」= 安全側に倒れるため、**動いているように見えて 1 件も効かない**
- **`.lock` の除外は `.git/` 配下に限る**（ワークツリーには `Cargo.lock` がある）
- **抑止条件に `frame-focus-state` を入れてはいけない。** フォーカスが外れている間は
  永久に偽なので、**その時点から二度と更新されなくなる**
- テスト用リポジトリは `git init -b main`（このマシンは `init.defaultBranch = main`）

## `my-claude`

→ [docs/claude/my-claude.md](docs/claude/my-claude.md)

1400 行あるので、触る前にそちらを読むこと。ここには入口だけ。

- **会話バッファへの書き込みは必ず `my:claude--at-end` を通す。** 挿入位置
  （`my:claude--output-end`。`point-max` ではない）・read-only 化・undo の 3 つを
  引き受けている。`insert` を直接書くと**書きかけの入力を壊す**
- **起動オプションは 4 つとも省略できない。** とくに
  `--permission-prompt-tool stdio` が無いと**許可要求が黙って自動拒否される**
  （ツールが動かないときの第一容疑者）
- **`default-process-coding-system` を束縛して起動する**（stdin は utf-8）
- **AskUserQuestion の答えは `deny` の `message` に載せる**のが唯一の回答経路
- **許可の拒否に `updatedInput` を付けてはいけない。`message` は必須**
- バッファ名にプロジェクト名が入るので、**「claude のバッファか」を名前で判定しない**
  （`my:claude--buffer-p` はメジャーモードで見る）
- **セッションの生死はプロセスとバッファの両方**で見る（`:buffer` が nil なので
  会話バッファを kill してもプロセスは生き残る）
- **応答待ち（`busy`）は `my:claude--set-busy` でしか変えない。** nil / `t` /
  `asking` の 3 値で、**真偽値ではない**。ヘッダ行の点を動かすタイマーの
  入り切りをこの関数がやっているので、直に `setf` すると点が止まらなくなる。
  ミニバッファで待つ区間は `my:claude--with-asking` で包む（`C-g` で
  `asking` が残ると、そのセッションは以後ずっと点が止まったままになる）

## `my-pty`

→ [docs/pty/my-pty.md](docs/pty/my-pty.md)

- **`setf (eat-term-parameter …)` は使えない。** バイトコンパイルしない方針なので
  `setf` の展開は my-pty.el の読み込み時に起きるが、そのとき eat は未ロードで
  gv のセッタが無い。素の関数 `eat-term-set-parameter` を使う
- **プリミティブへの advice は native-compile されたコードに効かない。**
  `term.eln` はプリミティブを直接呼ぶので、symbol の function cell に張った advice を
  素通りする。包むなら **Lisp の関数**にする
- **起動時のサイズはメジャーモードを立ててから測る**（`eat-mode` は
  `kill-all-local-variables` を通る）。ヘッダ行を立てるのもサイズを測る前
- **eaw.el の幅表のままだと eat が無限ループする。** 端末の中では conhost に合わせて
  ambiguous を幅 1 にする

## `my-htnblog`

→ [docs/htnblog/my-htnblog.md](docs/htnblog/my-htnblog.md)

- **`url-request-data` は unibyte にする**（`encode-coding-string` を通さないと化ける）
- **CDATA に `]]>` が現れたら分割する**（`]]]]><![CDATA[>`）。本文は自由に書くので
  必ず起こりうる
- **曜日は `format-time-string` の `%a` に頼らない**（`system-time-locale` 次第で英語）
- **`defvar-local` の世代カウンタは `permanent-local` にする。**
  `define-derived-mode` は `kill-all-local-variables` を通る

## `my-utils` — カレンダーの祝日

→ [docs/misc/calendar-holidays.md](docs/misc/calendar-holidays.md) / [docs/misc/pdf-preview.md](docs/misc/pdf-preview.md)

**日本の祝日は Emacs 本体に入っていない。** 31.1 の `lisp/calendar/` を `japan` で
grep しても 1 件も出ない。`japanese-holidays`（emacs-jp）が事実上唯一の選択肢で、
upstream は 2020-12 で止まっているが**祝日法が 2021 年以降変わっていないため
現行法に完全対応している**（2026 年の 18 件が内閣府の一覧と一致）。

### 【重要】2 箇所とも設定しないと 1 つも出ない

長いあいだ祝日が表示されていなかった。原因は 2 つあり**どちらもエラーを出さない**。

1. `mark-holidays-in-calendar` という変数は**存在しない**（§1「`setq` と `setq-local`」）
2. `calendar-holidays` に `japanese-holidays` を接続する行が要る

```elisp
(customize-set-variable
 'calendar-holidays (append japanese-holidays
                            holiday-local-holidays
                            holiday-other-holidays))
```

**`:custom` ではなく `:config` に置く**（`:custom` は `require` より前に展開されるので
値の式にある `japanese-holidays` が void になる）。あわせて `:after calendar` +
`:demand t`。マークは `calendar-generate` の中で走るので、遅延ロードでは**初回の
表示に間に合わない**。

## `my-platform` — `~/Projects` のジャンクション

→ [docs/misc/projects-junction.md](docs/misc/projects-junction.md)

`bookmarks` を git 管理下に置いて mac / Linux と共有するための仕込み。
**移植可能な省略形は `~` ただ 1 つ**なので、home の外にある `c:/Projects/...` は
そのままでは共有できない。役割は 2 つに分かれ、**片方だけでは成立しない。**

| | 担当 |
|---|---|
| ジャンクション / symlink | `~/Projects` を実在させる = **読む側**（展開） |
| `directory-abbrev-alist` | `c:/Projects/...` と書かせない = **書く側**（省略） |

`abbreviate-file-name` だけがこの変数を見る。**`expand-file-name` は見ない**ので
一方向にしか効かない。

### 【重要】FROM の末尾のスラッシュを省かない

`directory-abbrev-apply` は FROM を素の正規表現として使い、境界を見ない。

| FROM | `c:/Projects/ESC-Web/` | `c:/ProjectsOld/foo/` |
|---|---|---|
| `\`c:/Projects` | `~/Projects/ESC-Web/` | **`~/ProjectsOld/foo/`** |
| **`\`c:/Projects/`** | `~/Projects/ESC-Web/` | `c:/ProjectsOld/foo/` |

**存在しないパスができるのにエラーは出ない。** `abbreviated-home-dir` は
`directory-abbrev-make-regexp` が境界を付けるが、**手書きのエントリには付かない**。

他マシンでは alist は要らない（`~/Projects` を実在させるだけ）。
**`~/Projects` が無いマシンでは、共有したブックマークは開けない。**

## `my-lang-native` — Go

→ [docs/languages/go.md](docs/languages/go.md)

- **外部の `go-mode` を入れない**（§2）
- **保存時は「import 整理 → 整形」の順**（逆にすると、あとから足された import 行が
  整形されないまま残る）
- **`eglot-code-actions` を対話的に呼んではいけない。** INTERACTIVE 非 nil だと
  該当 0 件のとき `eglot--error` が飛び、`before-save-hook` の中なので
  **import を整理する必要が無いファイルは保存できなくなる**

---

# 4. 既知の課題（未対応）

新しく気づいたことはこの節に追記する。

### `my-gitd` 経由だと `C-g` で git が止まらない（2026-09、優先度低）

素の `call-process` は `C-g` で子プロセスを kill するが、デーモン経由では git が
走り切る。書き込みの途中で `C-g` すると「中断したのに実行されている」ことになる。
半端に kill された `.git/index` より安全とも言えるので、優先度は低いと判断した。

### 自動更新では diff-hl が更新されない（2026-09、仕様）

`my-magit-watch` は `magit-refresh-buffer`（そのバッファだけ）を呼ぶので、
`magit-post-refresh-hook`（diff-hl がぶら下がっている）は走らない。
fringe のマーカーを最新にしたいときは手で `g` を押す。

**dired の VC マークも同じ理由で追従しない**（2026-09-17）。`magit` の操作と
`vc-checkin` は段階 1 で拾うようにしたが、**外部の git（ターミナルや
Claude Code からの commit）と自動更新は拾わない**。表示していない dired
バッファも対象外。段階 2（`my:magit-watch--refresh` への相乗り + dired から
監視登録）は未着手。→ [docs](docs/dired/dired-extensions.md)

### 自動更新の `.gitignore` 判定はディレクトリ単位（2026-09、仕様）

追跡対象のディレクトリの中にある無視されるファイル（`src/` の中の `*.log` など）は
落とせず、リフレッシュが走る。`check-ignore` をファイル単位で呼べば正確になるが、
ビルド中のカーディナリティが跳ね上がるので採らない。

### `my-gitd` の書き込み経路の検証は一部だけ（2026-09）

シャドウモード（`my:gitd-verify` = t で両方実行してバイト比較）は読み取り専用
コマンドにしか使えない。stage / unstage / commit は GUI プローブで自動検証して
いるが、discard / rebase / merge / cherry-pick、コンフリクト中の操作、
サブモジュールはまだ実際に操作して確かめるしかない。

### イベントが落ちるとキャッシュが古いままになりうる（2026-09、優先度低）

`w32notify` はバッファ溢れでイベントを落とす（§1「Windows 固有」）。保険は 2 つ。
決め手のイベントが落ちても `.git` の粗い mtime 更新（`suspect`）は残りやすく、
そのときはフィンガープリントの不一致でトークンを進める。そして**`g` を押せば
必ず進む**。根本的に塞ぐには `ReadDirectoryChangesW` の溢れ通知を受け取る必要がある。

### Emacs の `call-process` が Windows で遅い（2026-09、未調査）

原因は未調査。`my-gitd` はこれを迂回するだけで直してはいない。magit 以外
（`vc` / `grep` / `projectile`）にも効いているはずなので、原因が分かれば影響範囲は
広い。ただし Emacs 本体の問題である可能性が高く、手元で解消できる見込みは薄い。

### `★` と `※` は端末で桁が揃わない（2026-09、未解決）

U+2605 と U+203B は手元のどのフォントでも全角。`my-pty` でこれらを含む行だけは
揃わない。→ [docs/pty/my-pty.md](docs/pty/my-pty.md)
