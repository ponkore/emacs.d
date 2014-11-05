(add-to-list 'auto-mode-alist '("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode))

(eval-after-load "clojure-mode"
  '(progn
     (require 'smartparens)
     (add-hook 'clojure-mode-hook 'paredit-mode)
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
       (context 2))))

(eval-after-load "cider"
  '(progn
     (require 'ac-cider)
     ;;(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
     (add-hook 'cider-mode-hook 'ac-cider-setup)

     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'cider-mode))

     (defun set-auto-complete-as-completion-at-point-function ()
       (setq completion-at-point-functions '(auto-complete)))
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
     (setq cider-show-error-buffer t)
     (setq cider-auto-select-error-buffer t)
     (setq cider-repl-print-length 50)  ; the default is nil, no limit
     (setq cider-repl-result-prefix ";; => ")
     (setq nrepl-sync-request-timeout 40)))
