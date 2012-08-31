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
   ("\C-x!" shell-command)
   ("\C-x|" shell-command-on-region)
   ("\eh" backward-kill-word)
   ("%" my-match-paren)
   ))


;;;
;;; バックスラッシュを入力したい(emacs-23.x on cocoa mac では ¥ が入力されてしまう)
;;;
(define-key global-map [?¥] [?\\])
(define-key global-map [?\C-¥] [?\C-\\])
(define-key global-map [?\M-¥] [?\M-\\])
(define-key global-map [?\C-\M-¥] [?\C-\M-\\])


;;;
;;; Mac でフォントを変えてみる
;;;
(when (and (eq window-system 'ns) (>= emacs-major-version 23))
  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 140)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))


;;; modeline に column-number は表示しない
(column-number-mode 0)
;;; modeline に line-number は表示しない
(line-number-mode 0)
;;; modeline に 現在の関数名を表示しない
(which-function-mode 0)
;;; scroll bar を表示しない
(scroll-bar-mode 0)
;;; 行番号の表示
(global-linum-mode t)      ; デフォルトで linum-mode を有効にする
(setq linum-format "%5d ") ; 5 桁分の領域を確保して行番号のあとにスペースを入れる


;;; clojure-mode
;; clojure 編集用のモード & nREPL インタフェース
;;
(el-get 'sync '(clojure-mode))
(el-get 'sync '(nrepl))


;;; color-theme
;;
;;
(el-get 'sync '(color-theme))
(color-theme-dark-laptop)


;;; csharp-mode
;; C# 編集用のモード
;(el-get 'sync '(csharp-mode))
; なぜか余計な flymake がついてくるので一旦とりやめ。


;;; markdown-mode
;; markdown 編集用のモード
(el-get 'sync '(markdown-mode))


;;; ntcmd
;; .cmd, .bat 編集用のモード
;;
(el-get 'sync '(ntcmd))


;;; scheme-complete
;;
;;
(el-get 'sync '(scheme-complete))


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


;;; paredit
;;
;;
(el-get 'sync '(paredit))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode)))

;;; auto-complete
;;
;;
(global-auto-complete-mode t)


;;;
;;; Japanese Calendar
;;;
(add-to-list 'load-path (expand-file-name "packages"))
(require 'japanese-holidays)
(add-hook 'calendar-load-hook
          (lambda ()
            (require 'japanese-holidays)
            (setq calendar-holidays
                  (append japanese-holidays local-holidays other-holidays))))
(setq mark-holidays-in-calendar t)
;; “きょう”をマークするには以下の設定を追加します。
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; 日曜日を赤字にする場合、以下の設定を追加します。
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)
