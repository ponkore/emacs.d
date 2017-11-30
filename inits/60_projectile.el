(use-package projectile
  :config
  (global-set-key (kbd "<f12>") 'projectile-toggle-between-implementation-and-test)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (defun helm-projectile-ag ()
    "Projectileと連携"
    (interactive)
    (helm-ag (projectile-project-root)))
  (define-key projectile-mode-map (kbd "s s") #'helm-projectile-ag))

(projectile-global-mode)
