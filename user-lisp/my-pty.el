;;; my-pty.el --- ConPTY を借りて Emacs で対話 TUI を動かす  -*- lexical-binding: t -*-
;;; Commentary:
;; Windows の Emacs には PTY が無く、`make-process' は常にパイプになる。
;; そのため対話 TUI (claude、ssh、python の REPL など) が動かない。
;;
;; `ptyd/' (Go) が疑似コンソール (ConPTY) を持って子プロセスを動かし、
;; その VT バイト列を stdio 経由で Emacs に流す。Emacs 側は term.el の
;; 端末エミュレータでバッファに再現する。
;;
;;     Emacs ──stdin (JSON Lines)──> ptyd ──ConPTY──> 子プロセス
;;           <──stdout (生の VT)──        <─────────
;;           <──stderr (診断の行)──
;;
;; **バイナリが無ければ何もしない。** ビルドしていないマシンでは
;; `my:pty-run' が user-error になるだけで、他の設定には影響しない。
;;
;; 検討と実測は docs/claude/emacs-claude-pty-proxy-study.md。
;;; Code:

(require 'term)
(require 'subr-x)

;; 端末エミュレータ。term.el は 1990 年代の実装で代替画面 (ESC[?1049) も
;; 同期出力も持たず、私用パラメータ付きの CSI (ESC[>4;2m) を SGR と
;; 誤解釈する。eat は純 elisp でそれらを正しく扱う。
(use-package eat
  :straight t
  :defer t)

;;; --------------------------------------------------
;;; カスタマイズ
;;; --------------------------------------------------

(defgroup my:pty nil
  "ConPTY 経由で対話 TUI を動かす。"
  :group 'processes
  :prefix "my:pty-")

(defcustom my:pty-directory
  (expand-file-name "ptyd/" user-emacs-directory)
  "`ptyd' のソースディレクトリ。"
  :type 'directory)

(defcustom my:pty-executable
  (expand-file-name "ptyd/ptyd.exe" user-emacs-directory)
  "`ptyd' の実行ファイル。`M-x my:pty-build' で作る。"
  :type 'string)

(defcustom my:pty-backend 'eat
  "端末の描画に使うもの。

`eat' … 純 elisp の端末エミュレータ (NonGNU ELPA)。代替画面・
bracketed paste・マウス・私用パラメータ付き CSI に対応していて、
UTF-8 の復号も自前で行う。**こちらが既定。**

`term' … Emacs 同梱の term.el。上のどれにも対応しておらず、
`ESC[>4;2m' を「全属性リセット + faint」として実行してしまう。
ptyd 側で削って誤魔化す必要がある。退避先として残してある。"
  :type '(choice (const :tag "eat" eat) (const :tag "term.el" term)))

(defcustom my:pty-narrow-ambiguous t
  "非 nil なら、端末を開いている間だけ ambiguous 幅を 1 に戻す。

**`char-width-table' はグローバルなので、端末バッファだけ変えることが
できない。** そのため一時的に全体を切り替えて、最後の端末を閉じたときに
戻す。切り替えたときは `message' で知らせる。

必要な理由。claude も conhost も East Asian Ambiguous を **幅 1** として
桁を組むが、`site-lisp/eaw.el' を入れた Emacs はそれらを幅 2 で描く。
実測 (この設定 / eaw 無し):

  U+2588 █ (マスコット)  2 / 1
  U+2500 ─ (罫線)        2 / 1
  U+00B7 ·               2 / 1
  U+2605 ★               2 / 1

その結果、ロゴが横に伸び、表の罫線が揃わない。WezTerm で同じ画面を出すと
正しく揃うので、ずれているのは Emacs 側だけだと分かる。

nil にすると eaw の幅のまま。端末の見た目は崩れるが、他のバッファの
桁揃えは変わらない。"
  :type 'boolean)

(defcustom my:pty-console-font "HackGen Console NF"
  "端末を開いている間だけ使うフォント。nil なら切り替えない。

**幅表を直すだけでは足りない。** `char-width' を 1 にしても、フォントが
その文字を 2 桁ぶんの幅で描けば見た目はずれる。GUI では
`string-pixel-width' が `char-width' ではなく**フォントの送り幅**を返す
ことからも分かる。実測 (`:height' 116、半角 8px / 全角 16px の設定):

  HackGen Console NF     a=8 ─= 8 ○= 8 あ=16   ← これが正解
  Consolas               a=8 ─= 8 ○= 8 あ=16   (日本語を持たない)
  HackGen35 Console NF   a=8 ─=11 ○= 9 あ=16   (3:5 設計なので合わない)
  Cascadia Mono          a=9 ─= 9 ○= 9 あ=16   (半角が 9px で合わない)
  HackGen (通常の設定)   a=8 ─=16 ○=16 あ=16   ← ずれる

HackGen の Console 版は ambiguous 幅の文字を半角にした派生なので、
半角 8 / 全角 16 という手元のメトリクスをそのまま保てる。"
  :type '(choice (const :tag "切り替えない" nil) string))

(defcustom my:pty-term-name "xterm-256color"
  "子プロセスに渡す TERM (`term' バックエンドのとき)。

term.el の既定は `eterm-color' だが、それを知らない相手のほうが多い。
どのみち VT を解釈するのは conhost なので、広く知られている名前にする。
`eat' のときは `eat-term-name' を使う。"
  :type 'string)

(defcustom my:pty-map-alt-screen t
  "非 nil なら `ESC[?1049' を `ESC[?47' に読み替えて ptyd に渡す。

term.el は代替画面として `?47' しか実装していない。いまどきの TUI が使う
`?1049' は無視されるので、そのままだと代替画面に入ったまま戻らない。
**読み替えても完全にはならない** (`?1049h' は画面消去も含むが `?47h' は
含まない)。term.el 以外に差し替えるときは nil にする。"
  :type 'boolean)

(defcustom my:pty-strip-unsupported-csi t
  "非 nil なら `ESC[<' `ESC[>' `ESC[=' を ptyd 側で落とす。

term.el はプライベートな CSI の目印として `?' しか見ていないため、
`ESC[>4;2m' (modifyOtherKeys) を SGR 0;2、つまり「全属性リセット +
faint」として実行してしまう。`?' 付きは正しく扱うので落とさない。
term.el 以外のエミュレータに差し替えるときは nil にする。"
  :type 'boolean)

;;; --------------------------------------------------
;;; ビルド
;;; --------------------------------------------------

;;;###autoload
(defun my:pty-build ()
  "`ptyd/' を go でビルドする。`gitd/' と同じく各マシンで作る。"
  (interactive)
  (unless (executable-find "go")
    (user-error "go が見つかりません"))
  (let ((default-directory my:pty-directory))
    (compile "go build -o ptyd.exe .")))

(defun my:pty-available-p ()
  "ptyd が使えるか。"
  (and (eq system-type 'windows-nt)
       (file-executable-p my:pty-executable)))

;;; --------------------------------------------------
;;; プロトコル (Emacs -> ptyd)
;;; --------------------------------------------------

(defvar my:pty--processes nil
  "生きている ptyd プロセスのリスト。")

(defun my:pty--send-json (proc obj)
  "OBJ を 1 行の JSON にして PROC に送る。"
  (when (process-live-p proc)
    ;; プロセスの coding は binary にしてあるので自分で UTF-8 にする。
    (my:pty--raw-send proc
                      (encode-coding-string (concat (json-serialize obj) "\n")
                                            'utf-8-unix))))

(defun my:pty--send-input (proc string)
  "STRING をキー入力として PROC に送る。

eat はキーイベントを既にバイト列 (unibyte) にして渡してくるので、
そのときは encode しない。二重に encode すると日本語が壊れる。"
  (my:pty--send-json
   proc
   `((op . "i")
     (d . ,(base64-encode-string
            (if (multibyte-string-p string)
                (encode-coding-string string 'utf-8)
              string)
            t)))))

(defun my:pty-send-resize (proc cols rows)
  "PROC の疑似コンソールを COLS x ROWS にする。"
  (my:pty--send-json proc `((op . "r") (cols . ,cols) (rows . ,rows))))

;;; term.el はキーを `process-send-string' で送る。こちらの stdin は
;;; JSON なので、そのままでは通らない。
;;;
;;; 【重要】`process-send-string' を advice で包んでも効かない。
;;; **native-compile されたコードはプリミティブを直接呼ぶ**ので、
;;; term.el (term.eln) からの呼び出しは advice を素通りする。
;;; 実際、生の `echo …' がそのまま ptyd に届いて
;;; `bad line: invalid character' になった。
;;;
;;; プリミティブではなく **term.el 側の Lisp 関数**を包む。
;;; そちらは symbol 経由で呼ばれるので native-compile でも効く。
;;; 書き込む入口は term.el 全体で 4 か所しかない。

(defun my:pty--our-proc (proc)
  "PROC が ptyd のものなら返す。"
  (and (processp proc) (process-get proc 'my:pty) proc))

(defun my:pty--raw-send (proc string)
  "PROC へ STRING をそのまま書く。"
  (process-send-string proc string))

(defun my:pty--advice-send-raw-string (orig chars)
  "`term-send-raw-string' の差し替え。char モードのキー入力はここを通る。"
  (let ((proc (my:pty--our-proc (get-buffer-process (current-buffer)))))
    (if (not proc)
        (funcall orig chars)
      (goto-char (process-mark proc))
      (my:pty--send-input proc chars))))

(defun my:pty--advice-send-string (orig proc str)
  "`term-send-string' の差し替え。貼り付けなどはここを通る。"
  (if (my:pty--our-proc proc)
      (my:pty--send-input proc str)
    (funcall orig proc str)))

(defun my:pty--advice-send-eof (orig &rest args)
  "`term-send-eof' の差し替え。疑似端末では C-d を送るのが相当。"
  (let ((proc (my:pty--our-proc (get-buffer-process (current-buffer)))))
    (if proc
        (my:pty--send-input proc "\C-d")
      (apply orig args))))

(defun my:pty--enable-eat-advice ()
  (advice-add 'eat-term-process-output :around #'my:pty--eat-width-advice)
  (advice-add 'eat-term-redisplay :around #'my:pty--eat-width-advice)
  (add-hook 'window-size-change-functions #'my:pty--sync-size))

(defun my:pty--disable-eat-advice ()
  (unless my:pty--processes
    (advice-remove 'eat-term-process-output #'my:pty--eat-width-advice)
    (advice-remove 'eat-term-redisplay #'my:pty--eat-width-advice)
    (remove-hook 'window-size-change-functions #'my:pty--sync-size)))

(defun my:pty--enable-advice ()
  (advice-add 'term-send-raw-string :around #'my:pty--advice-send-raw-string)
  (advice-add 'term-send-string :around #'my:pty--advice-send-string)
  (advice-add 'term-send-eof :around #'my:pty--advice-send-eof)
  (add-hook 'window-size-change-functions #'my:pty--sync-size))

(defun my:pty--disable-advice ()
  "使っているセッションが無くなったら外す。"
  (unless my:pty--processes
    (advice-remove 'term-send-raw-string #'my:pty--advice-send-raw-string)
    (advice-remove 'term-send-string #'my:pty--advice-send-string)
    (advice-remove 'term-send-eof #'my:pty--advice-send-eof)
    (remove-hook 'window-size-change-functions #'my:pty--sync-size)))

;;; --------------------------------------------------
;;; 起動
;;; --------------------------------------------------

(defvar-local my:pty--process nil
  "このバッファの ptyd プロセス。")

(defun my:pty--window-size (buffer)
  "BUFFER を表示しているウィンドウの (桁 . 行)。

【重要】まだ表示されていなければ、ここで表示してしまう。
サイズを決め打ちで起動すると、**子プロセスと term.el で幅が食い違う**。
子は自分の幅で折り返しの有無を決めるので、実際の幅が狭いと term.el 側で
余分に折り返され、行がずれる。以後の絶対カーソル移動が 1 行ずつずれ、
`ESC[K' が効くべき場所からずれるため、古い文字が消えずに新しい文字と
重なる。画面のあちこちが二重に見えるのはこれ。"
  (let ((win (or (get-buffer-window buffer t) (display-buffer buffer))))
    (if win
        (with-selected-window win
          ;; 行数は `window-screen-lines' で採る。`window-body-height' と
          ;; 違ってヘッダ行や端数行を勘定に入れてくれる。**ヘッダ行を
          ;; 立てるならこれを呼ぶ前に**。あとから足すと 1 行ぶんずれる。
          (cons (max 20 (window-max-chars-per-line win))
                (max 5 (floor (window-screen-lines)))))
      (cons 100 30))))

(defvar my:pty--narrow-width-table nil
  "East Asian Ambiguous を幅 1 に戻した `char-width-table' の複製。")

(defun my:pty--narrow-width-table ()
  "端末バッファで使う文字幅表。

【重要】`site-lisp/eaw.el' が ambiguous 幅の文字を幅 2 にしているが、
**この表を使うと eat が無限ループする**。実測 (claude の TUI 出力
2385 文字を流し込む):

  emacs -Q                      … 完了
  emacs -Q + (eaw-fullwidth)    … **戻ってこない**
  幅表を戻して流す              … 完了

そもそも桁を数えているのは conhost であり、Windows のコンソールは
ambiguous を幅 1 として扱う。Emacs 側だけ幅 2 で数えると、ループ
しなかったとしても桁がずれる。**端末の中では conhost に合わせる**のが
正しい。バッファの外 (通常の編集) には影響しない。"
  (or my:pty--narrow-width-table
      (setq my:pty--narrow-width-table
            (let ((tbl (copy-sequence char-width-table)))
              (when (boundp 'east-asian-ambiguous)
                (dolist (c east-asian-ambiguous)
                  (aset tbl c 1)))
              tbl))))

(defconst my:pty--ambiguous-ranges
  '((#x00a1 . #x00ff)   ; ラテン補助 (· ° ± × ÷ § ¶)
    (#x2000 . #x206f)   ; 一般句読点 (… † ‡ ‘ ’ “ ”)
    (#x2100 . #x214f)   ; 文字様記号
    (#x2150 . #x218f)   ; 数字に準じるもの
    (#x2190 . #x21ff)   ; 矢印 (→ ←)
    (#x2460 . #x24ff)   ; 囲み英数字 (①②③)
    (#x2500 . #x257f)   ; 罫線素片 (─ │ ┌ ┐)
    (#x2580 . #x259f)   ; ブロック要素 (█ ▀ ▐)
    (#x25a0 . #x25ff)   ; 幾何学模様 (○ △ □ ◇)
    (#x2600 . #x26ff))  ; その他の記号 (★ ☆ ※)
  "端末のあいだフォントを差し替える範囲。

ambiguous 幅の文字が多く、かつ claude の TUI が実際に使うところ。
Nerd Font のアイコン領域 (#xe000-#xf8ff) は **触らない**。
`my-appearance.el' がそこを別のフォントに回しており、上書きすると
アイコンが豆腐になる。")

(defvar my:pty--saved-jp-font nil
  "端末を開く前の日本語フォント。閉じるときに戻す。")

(defun my:pty--apply-font (family)
  "FAMILY を日本語と ambiguous の範囲に割り当てる。"
  (let ((spec (font-spec :family family)))
    (dolist (cs '(japanese-jisx0208 japanese-jisx0212 japanese-jisx0213-1
                  japanese-jisx0213-2 japanese-jisx0213.2004-1
                  katakana-jisx0201))
      (set-fontset-font nil cs spec))
    (dolist (range my:pty--ambiguous-ranges)
      (set-fontset-font nil range spec))
    (redraw-display)))

(defun my:pty--console-font-on ()
  "端末用のフォントに切り替える。"
  (when (and my:pty-narrow-ambiguous
             my:pty-console-font
             (display-graphic-p)
             (null my:pty--saved-jp-font)
             (find-font (font-spec :family my:pty-console-font)))
    ;; 既定のフォント名を控える。`my-appearance.el' は ascii と日本語に
    ;; 同じファミリを使っているので、これで戻せる。
    (setq my:pty--saved-jp-font (face-attribute 'default :family))
    (my:pty--apply-font my:pty-console-font)))

(defun my:pty--console-font-off ()
  "元のフォントに戻す。端末が 1 つも無くなったときだけ。"
  (when (and my:pty--saved-jp-font (null my:pty--processes))
    (my:pty--apply-font my:pty--saved-jp-font)
    (setq my:pty--saved-jp-font nil)))

(defvar my:pty--saved-width-table nil
  "`my:pty-narrow-ambiguous' で退避した元の `char-width-table'。")

(defun my:pty--narrow-widths-on ()
  "ambiguous 幅を 1 に切り替える。"
  (when (and my:pty-narrow-ambiguous (null my:pty--saved-width-table))
    (setq my:pty--saved-width-table char-width-table)
    (setq char-width-table (my:pty--narrow-width-table))
    (my:pty--console-font-on)
    (redraw-display)
    (message "端末のあいだ ambiguous を幅 1 / フォントを %s にしました"
             (or (and my:pty--saved-jp-font my:pty-console-font) "そのまま"))))

(defun my:pty--narrow-widths-off ()
  "元の幅に戻す。端末が 1 つも無くなったときだけ。"
  (when (and my:pty--saved-width-table (null my:pty--processes))
    (setq char-width-table my:pty--saved-width-table)
    (setq my:pty--saved-width-table nil)
    (my:pty--console-font-off)
    (redraw-display)
    (message "ambiguous の幅とフォントを元に戻しました")))

;;;###autoload
(defun my:pty-toggle-ambiguous-width ()
  "East Asian Ambiguous を幅 1 にするかどうかを切り替える。

`my:pty-narrow-ambiguous' は defcustom なので `M-x' では出てこない。
開いている端末にその場で反映したいので、コマンドを用意してある。
端末の見た目が eaw のせいで崩れているのかどうかを、切り替えて
見比べるのがいちばん早い。"
  (interactive)
  (setq my:pty-narrow-ambiguous (not my:pty-narrow-ambiguous))
  (if my:pty-narrow-ambiguous
      (my:pty--narrow-widths-on)
    ;; 端末が開いていても戻す。
    (when my:pty--saved-width-table
      (setq char-width-table my:pty--saved-width-table
            my:pty--saved-width-table nil)
      (when my:pty--saved-jp-font
        (my:pty--apply-font my:pty--saved-jp-font)
        (setq my:pty--saved-jp-font nil))
      (redraw-display)))
  (message "ambiguous 幅: %s%s"
           (if my:pty-narrow-ambiguous "1 (端末に合わせる)" "eaw のまま")
           (if my:pty--processes " / 開いている端末に反映しました" ""))
  ;; 幅が変わったので端末側も測り直す。
  (my:pty--sync-size))

(defun my:pty--our-terminal-p (terminal)
  "TERMINAL が ptyd に繋がっているか。"
  (ignore-errors
    (let ((proc (eat-term-parameter terminal 'eat--process)))
      (and (processp proc) (process-get proc 'my:pty)))))

(defun my:pty--eat-width-advice (orig terminal &rest args)
  "eat の処理の間だけ文字幅表を conhost に合わせる。"
  (if (my:pty--our-terminal-p terminal)
      (let ((char-width-table (my:pty--narrow-width-table)))
        (apply orig terminal args))
    (apply orig terminal args)))

(defun my:pty--setup-eat (buf cols rows)
  "BUF に eat の端末を作る。`eat-mode' は先に立てておくこと。"
  (with-current-buffer buf
    (setq eat-terminal (eat-term-make buf (point)))
    (eat-semi-char-mode)
    (eat-term-resize eat-terminal cols rows)
    ;; ここが肝。端末 -> アプリの書き込みは `input-function' から出るので、
    ;; term.el のように送信関数を advice で包む必要が無い。
    (eat-term-set-parameter eat-terminal 'input-function #'my:pty--eat-input)
    (eat-term-set-parameter eat-terminal 'set-cursor-function #'eat--set-cursor)
    (eat-term-set-parameter eat-terminal 'grab-mouse-function #'eat--grab-mouse)
    (eat-term-set-parameter eat-terminal 'manipulate-selection-function #'eat--manipulate-kill-ring)
    (eat-term-set-parameter eat-terminal 'ring-bell-function #'eat--bell)
    (eat-term-set-parameter eat-terminal 'set-cwd-function #'eat--set-cwd)
    (eat-term-set-parameter eat-terminal 'ui-command-function #'eat--handle-uic)
    (when (fboundp 'eat--set-term-sixel-params)
      (eat--set-term-sixel-params))))

(defun my:pty--eat-input (terminal input)
  "eat から来た INPUT を ptyd に送る。"
  (when-let* ((proc (eat-term-parameter terminal 'eat--process)))
    (my:pty--send-input proc input)))

(defun my:pty--setup-term (buf cols rows)
  "BUF の term.el 側のサイズを決める。`term-mode' は先に立てておくこと。"
  (with-current-buffer buf
    (term-reset-size rows cols)))

;;;###autoload
(defun my:pty-run (name command &optional dir env header)
  "COMMAND を ConPTY 経由で動かし、端末のバッファを返す。

NAME はバッファ名 (`*NAME*' になる)、COMMAND は文字列のリスト、
DIR は作業ディレクトリ。ENV は非 nil なら `process-environment' を
**丸ごと** それに差し替える。追加ではなく差し替えなのは、環境変数を
消す必要がある場合があるため。HEADER はヘッダ行 (サイズを測る前に
立てる必要があるのでここで受け取る)。"
  (unless (my:pty-available-p)
    (user-error "ptyd が無い。M-x my:pty-build でビルドしてください (%s)"
                my:pty-executable))
  (when (eq my:pty-backend 'eat) (require 'eat))
  (let* ((eatp (eq my:pty-backend 'eat))
         (bufname (format "*%s*" name))
         (buf (get-buffer-create bufname))
         (dir (expand-file-name (or dir default-directory)))
         size cols rows proc)
    (with-current-buffer buf
      (let ((old (get-buffer-process buf)))
        (when old (delete-process old)))
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq default-directory dir)
      ;; 【重要】メジャーモードを先に立てる。`eat-mode' も `term-mode' も
      ;; `kill-all-local-variables' を通るので、あとから立てると
      ;; ヘッダ行も `truncate-lines' も消える。実際に消えていた。
      (if eatp (eat-mode) (term-mode))
      (when header (setq header-line-format header))
      ;; 端末は自分で桁を管理するので折り返させない。
      ;; **折り返すと 1 桁ずれただけで以後の行が全部ずれる。**
      ;; eaw で ambiguous を幅 2 にしていると、eat が幅 1 で組んだ行を
      ;; Emacs は幅 2 で描くため、どうしても溢れることがある
      ;; (claude も conhost も幅 1 を前提にしているので eat 側は 1 が正しい)。
      ;; そのときは右端が切れるだけで、レイアウトは保たれる。
      (setq-local truncate-lines t)
      (unless eatp
        ;; 【重要】term.el は復号に `locale-coding-system' を決め打ちしている。
        ;; 日本語 Windows では cp932 なので UTF-8 の罫線が壊れて落ちる。
        (setq-local locale-coding-system 'utf-8-unix)))
    ;; 【重要】サイズを決める前にウィンドウを確保する。決め打ちで起動すると
    ;; 子と Emacs 側で幅が食い違い、行がずれて画面が二重に見える。
    (setq size (my:pty--window-size buf) cols (car size) rows (cdr size))
    (if eatp
        (my:pty--setup-eat buf cols rows)
      (my:pty--setup-term buf cols rows))
    (setq proc
          (let ((process-environment
                 (append (list (format "TERM=%s" (if eatp (eat-term-name)
                                                   my:pty-term-name))
                               (format "COLUMNS=%d" cols)
                               (format "LINES=%d" rows))
                         (or env process-environment)))
                (inhibit-eol-conversion t)
                (default-directory dir))
            (make-process
             :name name
             :buffer buf
             :connection-type 'pipe
             :noquery t
             ;; eat は **復号済みの文字列**を受け取る作り (パーサが文字を
             ;; 比較する)。term.el は逆に生バイトを要求し、復号を自分でやる。
             :coding (if eatp '(utf-8-unix . binary) '(binary . binary))
             :command (append
                       (list my:pty-executable
                             "-cols" (number-to-string cols)
                             "-rows" (number-to-string rows)
                             "-dir" dir)
                       ;; eat は私用パラメータ (? > =) を別扱いするので、
                       ;; ptyd 側で削ったり読み替えたりする必要が無い。
                       (unless eatp
                         (append
                          (when my:pty-strip-unsupported-csi
                            (list "-strip-unsupported-csi"))
                          (when my:pty-map-alt-screen
                            (list "-map-alt-screen"))))
                       (list "--")
                       command)
             :stderr (my:pty--stderr-buffer name)
             :filter (if eatp #'eat--filter #'term-emulate-terminal)
             :sentinel #'my:pty--sentinel)))
    (process-put proc 'my:pty t)
    (process-put proc 'my:pty-backend my:pty-backend)
    (push proc my:pty--processes)
    (if eatp (my:pty--enable-eat-advice) (my:pty--enable-advice))
    (my:pty--narrow-widths-on)
    (with-current-buffer buf
      (setq my:pty--process proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (if eatp
          (progn
            (eat-term-set-parameter eat-terminal 'eat--process proc)
            (eat-term-set-parameter eat-terminal 'eat--input-process proc)
            (eat-term-set-parameter eat-terminal 'eat--output-process proc)
            (eat-term-redisplay eat-terminal))
        (setq-local term-ptyp t)
        (term-char-mode))
      (my:pty-mode 1))
    buf))

(defun my:pty--stderr-buffer (name)
  "ptyd 自身の診断を入れるバッファ。"
  (let ((buf (get-buffer-create (format " *%s-stderr*" name))))
    (with-current-buffer buf (setq buffer-read-only nil))
    buf))

(defun my:pty--sentinel (proc msg)
  (setq my:pty--processes (delq proc my:pty--processes))
  (my:pty--disable-advice)
  (my:pty--disable-eat-advice)
  (my:pty--narrow-widths-off)
  (if (eq (process-get proc 'my:pty-backend) 'eat)
      (eat--sentinel proc msg)
    (term-sentinel proc msg)))

;;; --------------------------------------------------
;;; リサイズ
;;; --------------------------------------------------

(defun my:pty--sync-size (&optional _frame)
  "表示中の ptyd バッファのサイズを揃える。"
  (dolist (proc my:pty--processes)
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (when-let* ((win (get-buffer-window buf t)))
          (let* ((cols (max 20 (window-max-chars-per-line win)))
                 (rows (max 5 (window-body-height win))))
            (with-current-buffer buf
              (if (eq (process-get proc 'my:pty-backend) 'eat)
                  (let ((size (eat-term-size eat-terminal)))
                    (unless (and (= cols (car size)) (= rows (cdr size)))
                      (eat-term-resize eat-terminal cols rows)
                      (eat-term-redisplay eat-terminal)
                      (my:pty-send-resize proc cols rows)))
                (unless (and (= cols term-width) (= rows term-height))
                  (term-reset-size rows cols)
                  (my:pty-send-resize proc cols rows))))))))))

(define-minor-mode my:pty-mode
  "ptyd 経由の端末バッファであることを示す。"
  :lighter " pty"
  (if my:pty-mode
      (progn
        (add-hook 'kill-buffer-hook #'my:pty--kill-child nil t)
        ;; ウィンドウが変わったときにも合わせる。
        ;; `window-size-change-functions' はフレームのサイズが変わったときしか
        ;; 走らないので、別のウィンドウに出し直しただけでは効かない。
        (add-hook 'window-configuration-change-hook #'my:pty--sync-size nil t)
        (add-hook 'post-command-hook #'my:pty--pin-point nil t))
    (remove-hook 'kill-buffer-hook #'my:pty--kill-child t)
    (remove-hook 'window-configuration-change-hook #'my:pty--sync-size t)
    (remove-hook 'post-command-hook #'my:pty--pin-point t)))

(defun my:pty--pin-point ()
  "カーソルを端末のカーソル位置に戻す。

端末なので、クリックした場所にカーソルが残るのはおかしい。ただし
範囲選択中は動かさない (コピーができなくなるため)。
入力自体は `term-send-raw-string' がプロセスマークへ移動してから送るので
元々ずれないが、**見た目のカーソルが別の場所にある**のが紛らわしい。"
  ;; eat は自分でカーソルを管理するので何もしない。
  (when (and my:pty-mode
             (eq my:pty-backend 'term)
             (not (region-active-p))
             (derived-mode-p 'term-mode)
             (term-in-char-mode)
             (process-live-p my:pty--process))
    (let ((m (process-mark my:pty--process)))
      (when (and (marker-position m) (/= (point) m))
        (goto-char m)))))

(defun my:pty--kill-child ()
  "バッファを閉じたら子プロセスも終わらせる。"
  (setq my:pty--processes (delq my:pty--process my:pty--processes))
  (my:pty--narrow-widths-off)
  (when (process-live-p my:pty--process)
    (my:pty--send-json my:pty--process '((op . "q")))
    (run-at-time 0.5 nil
                 (lambda (p) (when (process-live-p p) (delete-process p)))
                 my:pty--process)))

;;; --------------------------------------------------
;;; claude を端末で動かす (案 C)
;;; --------------------------------------------------

(defcustom my:pty-claude-screen-reader nil
  "非 nil なら claude に `--ax-screen-reader' を渡す。

代替画面・マウス・同期出力・24bit カラーが消え、上から下に流れる
平文になる。実測でバイト数は約 1/4 になるが、見た目は相当に平板。

**`term' バックエンドではこれを t にしないと表示が崩れる。**
`eat' は代替画面を扱えるので nil のままでよい (既定)。"
  :type 'boolean)

;;;###autoload
(defun my:claude-term ()
  "claude の対話 TUI を ConPTY 経由で開く。

案 A (`my:claude'、stream-json) と違って claude 本体の UI がそのまま出る。
アカウントの選択は案 A と共有している。"
  (interactive)
  (require 'my-claude)
  (let* ((env (my:claude--read-environment))
         (dir (my:claude--project-directory))
         (config (my:claude--config-dir env))
         (buf (my:pty-run
               "claude-term"
               (append (list my:claude-executable)
                       (when my:pty-claude-screen-reader
                         (list "--ax-screen-reader"))
                       (when my:claude-model
                         (list "--model" my:claude-model)))
               dir
               ;; CLAUDE_CONFIG_DIR は「設定しない」ではなく「消す」
               ;; 必要がある。Emacs が継承していると、既定の環境を
               ;; 選んだつもりで別のアカウントに繋がる。my-claude.el が
               ;; 同じ理由で用意している組み立てをそのまま使う。
               (my:claude--process-environment config)
               ;; ヘッダ行はサイズを測る前に立てる。あとから足すと
               ;; 使える行数が 1 減って、疑似コンソールと食い違う。
               (format "claude-term: %s | %s" env
                       (abbreviate-file-name (directory-file-name dir))))))
    (pop-to-buffer buf)
    ;; `pop-to-buffer' で別のウィンドウに移ることがあるので測り直す。
    (my:pty--sync-size)
    buf))

(provide 'my-pty)
;;; my-pty.el ends here
