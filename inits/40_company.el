(require 'company)

(global-set-key (kbd "C-c y") 'company-yasnippet)
(add-hook 'after-init-hook 'global-company-mode)
