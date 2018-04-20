;;
;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
;;
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(use-package flycheck
  :defer t
  :config
  (require 'flycheck-pos-tip)
  (custom-set-variables
   '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  ;; (flycheck-add-next-checker 'javascript-jshint
  ;;                            'javascript-gjslint)
  )
