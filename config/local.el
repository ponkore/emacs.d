;;;
;;; my prefer key binding
;;;
(mapcar
 '(lambda (l) (global-set-key (first l) (second l)))
 '(("\C-h" delete-backward-char)
   ("\C-z" scroll-down)
   ("\e?" apropos)
   ("\C-x\C-e" compile)
   ("\C-x\C-n" next-error)
   ("\C-x\C-v" find-file-other-window)
   ("\C-x=" count-lines-page)
   ("\C-xl" goto-line)
   ("\C-xg" grep)
   ("\e\C-g" keyboard-quit)              ; init.el の設定をもとに戻す
   ("\C-x!" shell-command)
   ("\C-x|" shell-command-on-region)
   ("\eh" backward-kill-word)
   ("%" my-match-paren)
   ))


;;;
;;; Windows 用設定はこちらにまとめる
;;;
(when (eq window-system 'w32)
  ;; cygwin mount initialize
  (load (expand-file-name "~/.emacs.d/config/builtins/gnupack-init.el"))
  ;; IME on/off key bind
  (global-set-key (kbd "M-`") 'toggle-input-method)
  ;; Windows dired quick hack: open any documents with external command.
  (defvar cygwin-start-command "cygstart.exe" "Open a file with suitable windows command.")
  (defun dired-open-external ()
    "Open current line of dired buffer with external (windows) command."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (if (file-directory-p file)
          (start-process "explorer" nil "explorer.exe" file)
        (start-process "cygstart" nil cygwin-start-command file))))
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external))))


;;;
;;; Mac 用設定
;;;
(when (eq window-system 'ns)
  ;; process-coding-system を utf-8 にする。(その他は設定不要？)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))


;;;
;;; Mac でフォントを変えてみる
;;;
(when (and (eq window-system 'ns) (>= emacs-major-version 23))
  (cocoa-emacs-font-setting 12))


;;; modeline に column-number は表示しない
(column-number-mode 0)
;;; modeline に line-number は表示しない
(line-number-mode 0)
;;; modeline に 現在の関数名を表示しない
(which-function-mode 0)
;;; scroll bar を表示しない
(scroll-bar-mode 0)
;;; startup message を表示しない
(setq inhibit-startup-message t)
;;; 行番号の表示
(global-linum-mode t)      ; デフォルトで linum-mode を有効にする
(setq linum-format "%5d ") ; 5 桁分の領域を確保して行番号のあとにスペースを入れる
;;; 選択行のハイライト表示の face
(custom-set-faces '(hl-line ((t (:underline "textBackgroundColor")))))


;;; clojure-mode
;; clojure 編集用のモード & nREPL インタフェース
;;
(el-get 'sync '(clojure-mode nrepl))
;;; for compojure indent
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
;;; easy to use (nrepl-jack-in)
(define-key clojure-mode-map (kbd "C-c M-j") 'nrepl-jack-in)

;;; color-theme
;;
;;
(el-get 'sync '(color-theme))
(color-theme-dark-laptop)


;;; csharp-mode
;; C# 編集用のモード
;(el-get 'sync '(csharp-mode))
; なぜか余計な flymake がついてくるので一旦とりやめ。


;;; ntcmd
;; .cmd, .bat 編集用のモード
;;
(el-get 'sync '(ntcmd))


;;; scheme-complete
;;
;;
;(el-get 'sync '(scheme-complete))


;;; ssh
;;
;;
(el-get 'sync '(ssh))


;;; yaml-mode
;; yaml 編集用のモード
;;
(el-get 'sync '(yaml-mode))


;;; yasnippet
;;
;;
(el-get 'sync '(yasnippet))
(require 'yasnippet)
(yas-reload-all)


;;; auto-complete
;;
;;
(require 'auto-complete-config)
(ac-config-default)


;;; paredit
;;
;;
(el-get 'sync '(paredit))
(setq lisp-hook-fn (lambda () (paredit-mode) (yas-minor-mode) (eldoc-mode)))
(add-hook 'lisp-mode-hook lisp-hook-fn)
(add-hook 'emacs-lisp-mode-hook lisp-hook-fn)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode) (yas-minor-mode)))


;;; ac-nrepl
;;
;;
(el-get 'sync '(ac-nrepl))
(require 'ac-nrepl)
;; nREPL バッファも auto-complete したいので以下の hook を入れる
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))


;;; SLIME and ac-slime
(el-get 'sync '(slime ac-slime))

;; Clozure CL (SLIME)
(setq ccl-root
      (cond
       ((eq window-system 'ns) "/opt/local")
       ((eq system-type 'berkeley-unix) "/usr/local")
       (t "C:/Apps/ccl")))
(setq inferior-lisp-program
      (cond
       ((eq window-system 'ns) (concat ccl-root "/bin/ccl64 -K utf-8"))
       ((eq system-type 'berkeley-unix) (concat ccl-root "/bin/ccl -K utf-8"))
       (t (concat ccl-root "/wx86cl.exe -K utf-8"))))
(setq hyperspec-root
      (cond
       ((eq window-system 'ns) (concat ccl-root "/share/doc/lisp/HyperSpec-7-0/HyperSpec"))
       ((eq system-type 'berkeley-unix) (concat ccl-root "/share/doc/clisp-hyperspec/HyperSpec"))
       (t (concat ccl-root "/../HyperSpec"))))
(require 'slime-autoloads)
(require 'hyperspec)
;;;
(setq common-lisp-hyperspec-root (concat "file://" (expand-file-name (concat hyperspec-root "/")))
      common-lisp-hyperspec-symbol-table (expand-file-name (concat hyperspec-root "/Data/Map_Sym.txt")))
(setq slime-net-coding-system 'utf-8-unix)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))
(slime-setup '(slime-repl slime-fancy slime-banner))
(add-to-list 'auto-mode-alist '("\\.asd$" . common-lisp-mode))


;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;; gist
(el-get 'sync '(logito pcache gh gist))


;;; expand-region
(el-get 'sync '(expand-region))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;; gtags
;;
;;
(el-get 'sync '(gtags))
(require 'gtags)
(global-set-key "\M-." 'gtags-find-tag)     ;関数の定義元へ
(global-set-key "\M-," 'gtags-find-rtag)    ;関数の参照先へ
(global-set-key "\M-s" 'gtags-find-symbol)  ;変数の定義元/参照先へ
(global-set-key "\M-f" 'gtags-find-file)    ;ファイルにジャンプ
(global-set-key "\C-t" 'gtags-pop-stack)   ;前のバッファに戻る


;;; for SQL mode (My Office PC Oracle setting)
(when (eq system-type 'windows-nt)
  (setq sql-oracle-program "c:/Apps/Oracle/instantclient_11_2/sqlplus.exe"))
(when (or (eq system-type 'berkeley-unix) (eq system-type 'darwin))
  (let ((oracle-home (expand-file-name "~/Applications/Oracle/instantclient_10_2")))
    (setenv "NLS_LANG" "JAPANESE_JAPAN.UTF8")
    (setenv "DYLD_LIBRARY_PATH" oracle-home)
    (setenv "LD_LIBRARY_PATH" oracle-home)
    (setq sql-oracle-program (concat oracle-home "/sqlplus"))))


;;;
;;; Japanese Calendar
;;;
(load "config/packages/japanese-holidays")
(add-hook 'calendar-mode-hook
          (lambda ()
            (setq calendar-holidays
                  (append japanese-holidays local-holidays other-holidays))))
(setq mark-holidays-in-calendar t)
;; “きょう”をマークするには以下の設定を追加します。
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; 日曜日を赤字にする場合、以下の設定を追加します。
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)
