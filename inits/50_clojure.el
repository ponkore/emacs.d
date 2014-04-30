(require 'cider)
(require 'clojure-mode)
(require 'midje-mode)
(require 'smartparens)

(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-print-length 50)       ; the default is nil, no limit
(setq cider-repl-result-prefix ";; => ")

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

;;; when windows, remove ^M in nrepl buffer
;;; thanx http://takeisamemo.blogspot.jp/2013/10/clojureemacs-windowsnreplm.html
(when (eq system-type 'windows-nt)
  (defun remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))
  (add-hook 'cider-mode-hook 'remove-dos-eol))

;; smartparens
(add-hook 'cider-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
