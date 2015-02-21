(add-to-list 'auto-mode-alist '("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode))

(eval-after-load "clojure-mode"
  '(progn
     (require 'smartparens)
     (add-hook 'clojure-mode-hook 'yas-minor-mode)
     (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
     (define-clojure-indent
       (defroutes 'defun)
       (tabular 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2)
       (fact 2))))

(eval-after-load "cider"
  '(progn
     (require 'company)
     (add-hook 'cider-repl-mode-hook #'company-mode)
     (add-hook 'cider-mode-hook #'company-mode)

     (setq cider-show-error-buffer t)
     (setq cider-auto-select-error-buffer t)
     (setq cider-repl-result-prefix ";; => ")
     (setq nrepl-sync-request-timeout 40)
     (setq nrepl-hide-special-buffers t)))
