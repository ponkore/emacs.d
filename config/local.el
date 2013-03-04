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
   ("\C-x\C-f" helm-find-files)
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


;; Windows/Mac dired quick hack: open any documents with external command.
(defvar open-directory-command "cygstart.exe" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "cygstart.exe" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))

;;;
;;; Windows 用設定はこちらにまとめる
;;;
(when (eq system-type 'windows-nt)
  ;; IME on/off key bind
  (global-set-key (kbd "M-`") 'toggle-input-method)
  ;; dired hack
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external))))


;;;
;;; Mac 用設定
;;;
(when (eq system-type 'darwin)
  ;; process-coding-system を utf-8 にする。(その他は設定不要？)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; dired hack
  (setq open-directory-command "open")
  (setq open-file-command "open")
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external))))


;;;
;;; Mac でフォントを変えてみる
;;;
(when (and (eq window-system 'ns) (>= emacs-major-version 23))
  (cocoa-emacs-font-setting 12))


;;;
;;; Emacs24 以上でテーマを変えてみる
;;;
(when (string< "24" emacs-version)
  (load-theme 'dichromacy))


;;;
;;; customize dired
;;;
;(el-get 'sync '(dired-details))
;(dired-details-install)


;;;
;;; for helm
;;;
(setq helm-for-files-preferred-list
      '(helm-c-source-buffers-list
        helm-c-source-bookmarks         ; bookmark の順位を上げた
        helm-c-source-recentf
        helm-c-source-file-cache
        helm-c-source-files-in-current-dir
        helm-c-source-locate))

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
;(el-get 'sync '(color-theme))
;(color-theme-dark-laptop)


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
;; customize...
(setq ac-auto-show-menu 0.4)
(setq ac-quick-help-delay 1.0)


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


;;; css mode hack (http://d.hatena.ne.jp/sugyan/20120107/1325938248)
(defvar ac-source-css-property-names
  '((candidates . (loop for property in ac-css-property-alist
                        collect (car property)))))
(defun my-css-mode-hook ()
  (add-to-list 'ac-sources 'ac-source-css-property)
  (add-to-list 'ac-sources 'ac-source-css-property-names))
(add-hook 'css-mode-hook 'my-css-mode-hook)

;;; scss-mode
(el-get 'sync '(scss-mode))
;; ;; CSS
(defun my-css-electric-pair-brace ()
  (interactive)
  (insert "{")(newline-and-indent)
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)(indent-for-tab-command)
  )

(defun my-semicolon-ret ()
  (interactive)
  (insert ";")
  (newline-and-indent))

;; ;; scss-mode
;; ;; https://github.com/antonj/scss-mode
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
(add-to-list 'auto-mode-alist '("\\.\\(scss\\|css\\)\\'" . scss-mode))
(add-to-list 'ac-modes 'scss-mode)
(add-hook 'scss-mode-hook
          (lambda ()
            (ac-css-mode-setup)
            (define-key scss-mode-map "\M-{" 'my-css-electric-pair-brace)
            (define-key scss-mode-map ";" 'my-semicolon-ret)
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)
            (setq ac-sources '(ac-source-css-property
                               ac-source-css-property-names
                               ac-source-yasnippet
                               ;; ac-source-words-in-same-mode-buffers
                               ac-source-words-in-all-buffer
                               ac-source-dictionary))
;            (flymake-mode nil) ;; flymake は日をあらためてちゃんと設定しよう
            (yas-minor-mode)))


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
;;; set Oracle as default SQL product.
(setq sql-product 'oracle)
(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))


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


;;;
;;; Google Search via Browser
;;;
(require 'browse-url)
(require 'thingatpt)

;; w3m-url-encode-string の rename 版 (w3m.el を入れてないから)
(defun my-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)               ; newline
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             ((char-equal ch ?\x20)     ; space
              "+")
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                  nil))))

;; google で検索。引数無しだと mini-buffer で編集できる。
(defun google (str &optional flag)
  "google で検索。引数無しだと mini-buffer で編集できる。"
  (interactive
   (list (cond ((or
                 ;; mouse drag の後で呼び出された場合
                 (eq last-command 'mouse-drag-region)
                 ;; region が活性
                 (and transient-mark-mode mark-active)
                 ;; point と mark を入れ替えた後
                 (eq last-command 'exchange-point-and-mark))
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))
               (t (thing-at-point 'word)))
         current-prefix-arg))
  (unless flag
    (setq str (read-from-minibuffer "Search word: " str)))
  (browse-url
   (concat
    "http://www.google.com/search?q="
    (my-url-encode-string str 'shift_jis)
    "&hl=ja&ie=Shift_JIS&lr=lang_ja")))

;;;
(load "config/packages/helm-hosen.el" t)
