(require 'tide)

(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq tide-format-options '(:indentSize 2 :tabSize 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.json" . javascript-mode))
