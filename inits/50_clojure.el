(require 'cider)
(require 'clojure-mode)
(require 'midje-mode)

(define-clojure-indent
  (defroutes 'defun)
  (tabular 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
;;; easy to use (cider-jack-in)
(define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in)

;;; when windows, remove ^M in nrepl buffer
;;; thanx http://takeisamemo.blogspot.jp/2013/10/clojureemacs-windowsnreplm.html
(when (eq system-type 'windows-nt)
  (defun remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))
  (add-hook 'cider-mode-hook 'remove-dos-eol))

;; ac-nrepl
(require 'ac-nrepl)

;; nREPL バッファも auto-complete したいので以下の hook を入れる
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
