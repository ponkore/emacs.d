---
name: emacs-eval
description: 起動中の Emacs (server) に emacsclient 経由で S 式を評価させる。GUI でしか観測できない値の実測（フォント幅・format-mode-line・face・ウィンドウ・実際に効いているキーバインド）、書き換えた関数やモジュールの再評価（再起動なしで反映）、生きた設定に対する試行錯誤に使う。「GUI で確認」「実際に効いているか見たい」「再起動せずに反映」「emacs で評価して」と言われたとき、および batch (emacs --batch) では確かめられない項目を検証するときに使う。
---

# 生きている Emacs に式を評価させる

`emacsclient -e` で、**いま動いている Emacs** に S 式を送って評価させ、戻り値を受け取る。

このリポジトリの CLAUDE.md には「**この検証は batch ではできない**」という注意が
繰り返し出てくる（`format-mode-line` は batch では常に `""`、w32notify の
イベントはコマンドループ経由なので `--batch` に届かない、`use-cjk-char-width-table`
の分岐が `initial-window-system` で変わる、`:if window-system` のパッケージが
登録されない、など）。**それらはすべてこの経路で観測できる。**

## 前提

```bash
bash .claude/skills/emacs-eval/ec.sh '(emacs-version)'
```

これが通れば準備完了。`emacsclient` は PATH に無いので `ec.sh` が
`C:/Apps/emacs/emacs-31.1/bin/emacsclient.exe` を探しにいく（`EMACSCLIENT`
環境変数で上書き可）。

`user-lisp/my-utils.el` の `my:server-start-maybe` が `emacs-startup-hook` から
server を立てるので、Emacs を起動すれば自動で繋がる。

繋がらないときに考えられるのは次のどちらか。**どちらもユーザーに伝えるだけに
して、こちらから Emacs を起動したり設定を書き換えたりしない。**

- この設定が入る前に起動した Emacs がまだ動いている（`M-x server-start` で立つ）
- **別の Emacs が先に待ち受けている。** `my:server-start-maybe` は
  `server-running-p` が nil のときだけ起動するのでソケットを奪わない。その代わり、
  2 つ目以降の Emacs には emacsclient が届かない。繋がった先が意図した Emacs か
  怪しいときは `(emacs-pid)` と `(buffer-list)` で確かめる。

## 使い方

| | |
|---|---|
| `ec.sh '式'` | 式を評価して結果を出す |
| `ec.sh -f FILE` | FILE 内の全ての式を順に評価し、**最後の値**を出す |
| `ec.sh -l FILE` | FILE を `load-file` する（モジュールの再読込） |
| `ec.sh -n ...` | ガード（`inhibit-interaction`）を外す。原則使わない |

結果は `pp` された Lisp オブジェクトで返る。エラーは `*ERROR*: メッセージ` を
標準エラーに出し、終了コードは 1。

```bash
$ bash .claude/skills/emacs-eval/ec.sh '(list (string-pixel-width "a") (string-pixel-width "あ"))'
(8 16)
```

以下このファイルでは `ec.sh` と略記する。実際は
`bash .claude/skills/emacs-eval/ec.sh` を Bash ツールで実行する
（作業ディレクトリが `.emacs.d` でなければ絶対パスで
`bash /c/Users/masao/.emacs.d/.claude/skills/emacs-eval/ec.sh`）。
`-f` / `-l` に渡すファイルパスは相対でも絶対でもよい。

**式の中では `'` ではなく `(quote x)` / `(function f)` と書く。** シェルの
シングルクォートの中にシングルクォートは置けない。`'` をそのまま書きたい
（長い式、`#'` を多用する式）なら、`-f` でファイルに書いて渡す。

## 【重要】文字コードの境界は「cp932 に有るか」で決まる

`my-japanese.el` が `default-process-coding-system` の cdr を cp932 にしており、
server の接続プロセスもそれを継承する（実測で `(raw-text-unix . cp932)`）。
`ec.sh` は戻り値を cp932 → UTF-8 に直してから出すので日本語は読めるが、
**cp932 に無い文字は往路・復路とも `?` に潰れる**（不可逆）。

| | ASCII | 日本語・`①`（cp932 内） | 絵文字・`𠮷`（cp932 外） |
|---|---|---|---|
| 引数で式を渡す（`ec.sh '式'`） | ○ | ○ | **`?` になる** |
| ファイルで式を渡す（`ec.sh -f`） | ○ | ○ | **○** |
| 戻り値を標準出力で受け取る | ○ | ○ | **`?` になる** |
| 戻り値をファイルに書いて Read する | ○ | ○ | **○** |

実測（`ec.sh '(length "🙂")'` → `2`、`ec.sh -f` で同じ式 → `1`）。

**cp932 の外を扱うとき、および結果が長いときは、必ずファイル経由にする。**

```bash
cat > tmp/probe.el <<'EOF'
(let ((coding-system-for-write 'utf-8-unix))
  (with-temp-file "c:/Users/masao/.emacs.d/tmp/ec-out.txt"
    (insert (format "%S" 調べたい式)))
  "-> tmp/ec-out.txt")
EOF
bash .claude/skills/emacs-eval/ec.sh -f tmp/probe.el
```

書けたら `tmp/ec-out.txt` を Read で読む。`tmp/` は CLAUDE.md が定める
「作業用の捨て場」（`.gitkeep` 以外は git 管理外）なので、プローブと出力は
そこに置く。ファイルのパスは Emacs に渡す側では **Windows 形式**
（`c:/...`）で書くこと。`/c/...` は Emacs が解釈しない（`-f` / `-l` に渡す
パスは `ec.sh` が `cygpath` で変換する）。

## 安全に使うための約束

**これはユーザーが今まさに使っている Emacs である。** 壊すと作業中の
バッファごと失われる。

- **`kill-emacs` / `save-buffers-kill-emacs` を送らない。**
- **ユーザーのバッファを書き換えない・保存しない。** 観測は
  `buffer-substring-no-properties` などの読み取りに留める。書き換えが要る
  検証は `with-temp-buffer` の中で完結させる。
- **`current-buffer` / `selected-window` は不定。** 送った時点でユーザーが
  何を選んでいるかは分からない（同じセッションでも `*claude*` だったり
  `*Warnings*` だったりした）。観測対象は `with-current-buffer` /
  `with-selected-window` で明示する。**どちらが要るかは調べる対象で変わる**
  （下の `format-mode-line` と `font-at` を参照）。
- **ハングさせない。** `ec.sh` は式を `(let ((inhibit-interaction t)) ...)` で
  包むので、`y-or-n-p` や `read-string` はミニバッファを開かずに
  `*ERROR*: User interaction while inhibited` で落ちる。この保険が無いと
  Emacs はミニバッファ待ちのまま固まり、ユーザーが `C-g` するまで戻らない。
  **`-n` を使うときはこれを承知の上で。**
- `inhibit-interaction` でも**無限ループや I/O 待ちは止められない**。
  重い式・止まりうる式は `(with-timeout (5 'timeout) ...)` で包む。
- ユーザーが Emacs 内から Claude Code を使っている場合（`my-claude.el`）、
  Emacs を止めると Claude 自身の応答処理も止まる。

## レシピ

### GUI でしか観測できない値

```bash
# フォントの実描画幅（CLAUDE.md「日本語フォントの全角/半角ピッチ」の検算）
ec.sh '(list (string-pixel-width "a") (string-pixel-width "あ") (string-pixel-width "─"))'
# => (8 16 16)

# ヘッダ行・モードラインの「実際に表示される文字列」。% の escape や face の
# 生き死には、組み立てた文字列を見ても分からない。batch では常に "" が返る
ec.sh '(format-mode-line "50%% done")'

# 実際に効いているキーバインド（dired-x に奪われていないか等）
ec.sh '(with-current-buffer "*claude*" (key-binding (kbd "TAB")))'
```

#### 【重要】`with-current-buffer` では `:eval` の列が黙って消える

`format-mode-line` は BUFFER 引数を省略すると **選択ウィンドウのバッファ**で
`:eval` を評価する（CLAUDE.md「`format-mode-line` は選択ウィンドウのバッファで
`:eval` を評価する」）。**server 経由ではこの罠を必ず踏む**。emacsclient が
繋いだ時点の選択ウィンドウは、ユーザーが見ているものとは限らない
（実測では `*Warnings*` だった）。

```bash
# ✗ :eval の列（ブランチ・コスト）が消える。エラーは出ない
ec.sh '(substring-no-properties (with-current-buffer "*claude*" (format-mode-line header-line-format)))'
# => "... | .emacs.d |  | claude-opus-5[1m] (high) | ctx 137.1k 14% | (5h 2%)(7d 0%)(...)"

# ○ ウィンドウごと選ぶ
ec.sh '(substring-no-properties (with-selected-window (get-buffer-window "*claude*") (format-mode-line header-line-format)))'
# => "... | .emacs.d | master | claude-opus-5[1m] (high) | ... | $0.00 ..."

# ○ BUFFER 引数を渡す（ウィンドウに出ていないバッファでも可）
ec.sh '(substring-no-properties (format-mode-line (buffer-local-value (quote header-line-format) (get-buffer "*claude*")) nil nil (get-buffer "*claude*")))'
```

#### `font-at` は 3 つの条件が揃わないと動かない

「`char-width` は 1 なのにフォントが 2 桁で描く」を調べるとき（CLAUDE.md
「幅表だけでは足りない。フォントも切り替える」）に使う。

1. **そのウィンドウが表示しているバッファが current** であること
   （`with-current-buffer` を省くと `Specified window is not displaying the
   current buffer`）
2. POS に**文字が実在する**こと（`point-max` は `Args out of range`）
3. WINDOW を渡すこと

```bash
ec.sh '(let ((w (get-buffer-window "*claude*")))
         (with-current-buffer (window-buffer w)
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward "[─│あ]" nil t)
               (let* ((p (1- (point))) (c (char-after p)) (f (font-at p w)))
                 (list c (char-width c) (string-pixel-width (string c))
                       (and f (font-get f :family))))))))'
# => (12354 2 16 HackGen)
```

ウィンドウに出ていないバッファでは測れない。文字の幅だけなら
`string-pixel-width` で足りる（ウィンドウ不要）。

### 書き換えたコードを再起動せずに反映する

```bash
# 関数ひとつを差し替える（いちばん安全で速い）
ec.sh '(defun my:foo (x) (* x 2))'

# モジュールごと読み直す
ec.sh -l user-lisp/my-claude.el
```

`-l`（`load-file`）の注意:

- **`defvar` / `defcustom` は再ロードしても値が変わらない**（既に bound なので
  初期値の式が評価されない）。実測で確認済み。新しい既定値を反映したいときは
  `(makunbound 'VAR)` してから読み直す。`C-M-x`（`eval-defun`）が
  対話操作で強制再評価しているのと同じ話。
- `use-package` ブロックは丸ごと再評価される。`:straight t` があると
  `straight-use-package` が再度走る（clone 済みなので速い）。`:config` は
  `with-eval-after-load` に包まれているので、**そのパッケージが未ロードなら
  実行されない**。
- `:hook` の `add-hook` は同じ関数なら重複しないが、`:init` に書いた
  `advice-add` や `defhydra` は再実行される。副作用のあるモジュールは
  読み直しより Emacs 再起動のほうが確実なことがある。
- **バイトコンパイルはしない方針**（CLAUDE.md 参照）なので `.elc` の心配は無い。

### 長い結果・エラーの詳細を読む

戻り値は 1 行の Lisp オブジェクトとして `\n` がエスケープされた形で返るため、
複数行の内容は読みにくい。ファイルに書いて Read する。

```bash
# *Messages* の末尾を読む
cat > tmp/probe.el <<'EOF'
(let ((coding-system-for-write 'utf-8-unix))
  (with-temp-file "c:/Users/masao/.emacs.d/tmp/ec-out.txt"
    (insert (with-current-buffer "*Messages*"
              (buffer-substring-no-properties
               (max (point-min) (- (point-max) 4000)) (point-max)))))
  "-> tmp/ec-out.txt")
EOF
ec.sh -f tmp/probe.el
```

`message` や `princ` の出力はクライアントには返らない（実測）。
`*Messages*` を経由するか、値として返すこと。

### バックトレースを取る

エラーメッセージだけで足りないときに。`handler-bind` は `condition-case` より
**内側**に置くこと（外側だと `condition-case` が先に捕まえてハンドラが走らない）。

```elisp
(let ((bt nil))
  (condition-case e
      (handler-bind ((error (lambda (_)
                              (setq bt (backtrace-to-string
                                        (seq-take (backtrace-get-frames 'backtrace-get-frames) 8))))))
        (調べたい式))
    (error
     (let ((coding-system-for-write 'utf-8-unix))
       (with-temp-file "c:/Users/masao/.emacs.d/tmp/ec-out.txt"
         (insert (error-message-string e) "\n" (or bt ""))))
     "-> tmp/ec-out.txt")))
```

先頭の 5 フレームはハンドラ自身なので、実際の失敗箇所はその下に出る。

### 状態の観測

```bash
ec.sh '(list (frame-parameter nil (quote name)) (length (frame-list)) (length (buffer-list)))'
ec.sh '(seq-take (mapcar (function buffer-name) (buffer-list)) 20)'
ec.sh '(list (featurep (quote my-claude)) (featurep (quote eat)))'
ec.sh '(let (r) (dolist (h (list (quote prog-mode-hook) (quote dired-mode-hook)) r) (push (cons h (symbol-value h)) r)))'
```

## 検証を報告するときの注意

- **観測できたことだけを書く。** 「変数が設定されている」ことは
  「機能が効いている」ことの証明にならない（CLAUDE.md の
  `flymake-disabled-backends` の例）。効いているかは結果の側で見る。
- **式を組み立てて 1 回流して終わりにしない。** 同じ観点を修正前後で 2 回測り、
  差が出ることを確かめる。
- ユーザーの Emacs はセッションを跨いで状態が積み上がっている。
  `ec.sh` で定義した一時的な関数や変数は、確認が済んだら
  `(fmakunbound 'name)` / `(makunbound 'name)` で片付ける。
