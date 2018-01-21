(use-package clojure-mode
  :defer t
  :mode (("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode))
  :config
  (require 'smartparens)
  (require 'yasnippet)
  (require 'projectile)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook (lambda () (define-key clojure-mode-map (kbd "C-c t") 'projectile-toggle-between-implementation-and-test)))
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
       (componentWillMount 'defun)
       (componentDidMount 'defun)
       (componentWillUnmount 'defun)
       (render 'defun)
       (fact 'defun)
       (do-transaction 'defun)))

(use-package cider
  :defer t
  :config
  (require 'company)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-result-prefix ";; => ")
  (setq nrepl-sync-request-timeout 40)
  (setq nrepl-hide-special-buffers t))
