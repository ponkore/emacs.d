(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "ros run")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy slime-banner slime-company))
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))
