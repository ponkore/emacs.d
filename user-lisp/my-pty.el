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

(defcustom my:pty-term-name "xterm-256color"
  "子プロセスに渡す TERM。

term.el の既定は `eterm-color' だが、それを知らない相手のほうが多い。
どのみち VT を解釈するのは conhost なので、広く知られている名前にする。"
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
  "STRING をキー入力として PROC に送る。"
  (my:pty--send-json
   proc
   `((op . "i")
     (d . ,(base64-encode-string (encode-coding-string string 'utf-8) t)))))

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
        (cons (max 20 (window-max-chars-per-line win))
              (max 5 (window-body-height win)))
      (cons 100 30))))

;;;###autoload
(defun my:pty-run (name command &optional dir env)
  "COMMAND を ConPTY 経由で動かし、term のバッファを返す。

NAME はバッファ名 (`*NAME*' になる)、COMMAND は文字列のリスト、
DIR は作業ディレクトリ、ENV は追加の環境変数 (\"K=V\" のリスト)。"
  (unless (my:pty-available-p)
    (user-error "ptyd が無い。M-x my:pty-build でビルドしてください (%s)"
                my:pty-executable))
  (let* ((bufname (format "*%s*" name))
         (buf (get-buffer-create bufname))
         (dir (expand-file-name (or dir default-directory)))
         size cols rows proc)
    (with-current-buffer buf
      (let ((old (get-buffer-process buf)))
        (when old (delete-process old)))
      (let ((inhibit-read-only t)) (erase-buffer))
      (term-mode)
      (setq default-directory dir)
      ;; 【重要】term.el は復号に `locale-coding-system' を決め打ちしている。
      ;; 日本語 Windows では cp932 なので、UTF-8 を吐く TUI の罫線が壊れ、
      ;; args-out-of-range で落ちる。バッファローカルに上書きする。
      (setq-local locale-coding-system 'utf-8-unix))
    ;; サイズはウィンドウに合わせたいが、まだ表示されていないので既定で始める。
    (setq size (my:pty--window-size buf) cols (car size) rows (cdr size))
    (with-current-buffer buf (term-reset-size rows cols))
    (setq proc
          (let ((process-environment
                 (append (list (format "TERM=%s" my:pty-term-name)
                               (format "COLUMNS=%d" cols)
                               (format "LINES=%d" rows))
                         (or env process-environment)))
                ;; VT はバイト列。復号は term.el に任せる。
                (coding-system-for-read 'binary)
                (coding-system-for-write 'binary)
                (inhibit-eol-conversion t)
                (default-directory dir))
            (make-process
             :name name
             :buffer buf
             :connection-type 'pipe
             :noquery t
             :coding '(binary . binary)
             :command (append
                       (list my:pty-executable
                             "-cols" (number-to-string cols)
                             "-rows" (number-to-string rows)
                             "-dir" dir)
                       (when my:pty-strip-unsupported-csi
                         (list "-strip-unsupported-csi"))
                       (when my:pty-map-alt-screen
                         (list "-map-alt-screen"))
                       (list "--")
                       command)
             :stderr (my:pty--stderr-buffer name)
             :filter #'term-emulate-terminal
             :sentinel #'my:pty--sentinel)))
    (process-put proc 'my:pty t)
    (push proc my:pty--processes)
    (my:pty--enable-advice)
    (with-current-buffer buf
      (setq my:pty--process proc)
      (setq-local term-ptyp t)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (term-char-mode)
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
  (term-sentinel proc msg))

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
              (unless (and (= cols term-width) (= rows term-height))
                (term-reset-size rows cols)
                (my:pty-send-resize proc cols rows)))))))))

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
  (when (and my:pty-mode
             (not (region-active-p))
             (term-in-char-mode)
             (process-live-p my:pty--process))
    (let ((m (process-mark my:pty--process)))
      (when (and (marker-position m) (/= (point) m))
        (goto-char m)))))

(defun my:pty--kill-child ()
  "バッファを閉じたら子プロセスも終わらせる。"
  (when (process-live-p my:pty--process)
    (my:pty--send-json my:pty--process '((op . "q")))
    (run-at-time 0.5 nil
                 (lambda (p) (when (process-live-p p) (delete-process p)))
                 my:pty--process)))

;;; --------------------------------------------------
;;; claude を端末で動かす (案 C)
;;; --------------------------------------------------

(defcustom my:pty-claude-screen-reader t
  "非 nil なら claude に `--ax-screen-reader' を渡す。

代替画面・マウス・同期出力・24bit カラーが消え、上から下に流れる
平文になる。term.el で扱えない機能がほぼ全部落ちるので、この経路では
既定で有効にする。実測でバイト数は約 1/4 になった。"
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
               (my:claude--process-environment config))))
    (with-current-buffer buf
      (setq header-line-format (format "claude-term: %s | %s" env
                                       (abbreviate-file-name
                                        (directory-file-name dir)))))
    (pop-to-buffer buf)))

(provide 'my-pty)
;;; my-pty.el ends here
