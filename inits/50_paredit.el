;;; paredit
;;
;;
(require 'paredit)
(setq lisp-hook-fn (lambda () (paredit-mode) (yas-minor-mode) (eldoc-mode)))
(add-hook 'lisp-mode-hook lisp-hook-fn)
(add-hook 'emacs-lisp-mode-hook lisp-hook-fn)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode) (yas-minor-mode)))
