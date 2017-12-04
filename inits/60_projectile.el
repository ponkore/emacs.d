;;;
;;; projectile
;;;
(require 'projectile)
(global-set-key (kbd "<f12>") 'projectile-toggle-between-implementation-and-test)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(defun helm-projectile-ag ()
    "Projectileと連携"
    (interactive)
    (helm-ag (projectile-project-root)))
(projectile-global-mode)
