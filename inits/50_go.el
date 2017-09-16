(use-package go-mode
  :defer t
  :config
  (add-to-list 'exec-path (expand-file-name "~/.go/bin"))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))
  ;; gocode
  (add-to-list 'load-path (expand-file-name "~/.go/src/github.com/nsf/gocode/emacs-company"))
  (require 'company)
  (require 'company-go)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection)))))
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))
