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
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))
