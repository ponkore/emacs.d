;;; Auto Complete
;; 自動補完

(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
;; customize...
(setq ac-auto-show-menu 0.4)
(setq ac-quick-help-delay 1.0)

(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)))
