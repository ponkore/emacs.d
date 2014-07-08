(add-to-list 'auto-mode-alist '("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode))

(eval-after-load "clojure-mode"
  '(progn
     (require 'smartparens)
     (add-hook 'clojure-mode-hook 'paredit-mode)
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
     ))

(eval-after-load "cider"
  '(progn
     (add-hook 'cider-mode-hook (lambda ()
                                  (auto-complete-mode)
                                  (ac-cider-compliment-setup)
                                  (smartparens-strict-mode)
                                  (yas-minor-mode)))
     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (auto-complete-mode)
                                       (ac-cider-compliment-repl-setup)))
     (setq cider-popup-stacktraces t)
     (setq cider-repl-popup-stacktraces t)
     (setq cider-auto-select-error-buffer t)
     (setq cider-repl-print-length 50)       ; the default is nil, no limit
     (setq cider-repl-result-prefix ";; => ")
     (setq nrepl-sync-request-timeout 20)))
