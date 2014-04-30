;;; company --- COMPlete ANYthing autocompleting framework
;;
(eval-after-load 'company '(add-to-list 'company-backends 'company-cider))
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
