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

(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'cider-mode-hook (lambda ()
                               (auto-complete-mode)
                               (ac-cider-compliment-setup)
                               (smartparens-strict-mode)
                               (yas-minor-mode)))
(add-hook 'cider-repl-mode-hook (lambda ()
                                  (auto-complete-mode)
                                  (ac-cider-compliment-repl-setup)))
