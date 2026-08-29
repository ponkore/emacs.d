;;; my-lang-web.el --- Web 系 (PHP / JavaScript / TypeScript)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] PHP

(leaf php-mode
  :mode ("\\.\\(cgi\\|phpm\\|inc\\)\\'" . php-mode)
  :straight t
  :after lsp-mode
  :custom
  (ac-php-debug-flag . nil)
  (php-manual-url . 'ja)
  (php-mode-coding-style . 'psr2)
  :hook
  (php-mode-hook . (lambda ()
                     (c-set-style "bsd")
                     (company-mode t)
                     (subword-mode 1)
                     (setq indent-tabs-mode t)
                     (setq tab-width 4)
                     (setq c-basic-offset 4)
                     (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")
                     ;; (ac-php-core-eldoc-setup)
                     ;; (add-to-list 'company-backends 'company-ac-php-backend)
                     ;; (make-local-variable 'company-backends)
                     ;; (require 'flycheck-phpstan)
                     (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
                     ;; (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
                     ;; (setq flycheck-phpcs-standard "PSR2")
                     (flycheck-mode t)))
  (php-mode-hook . lsp-deferred)
  :config
  ;; (leaf company-php
  ;;   :straight t)
  (setq lsp-intelephense-files-associations ["*.php" "*.phpm" "*.inc"])
  :bind
  (:php-mode-map
   (";" . self-insert-command)
   ("{" . self-insert-command)
   ;; ("[" . #'(smartchr "[]" "array()" "[[]]"))
   ;; ("]" . #'(smartchr "array " "]" "]]"))
   ;; ("C-}" . cedit-barf)
   ;; ("C-)" . cedit-slurp)
   ;; ("M-." . ac-php-find-symbol-at-point)
   ;; ("M-," . ac-php-location-stack-back)
   ("C-c C--" . php-current-class)
   ("C-c C-=" . php-current-namespace)))

(leaf flycheck-phpstan
  :straight t)

;;; [3] JavaScript / TypeScript

;;
;; javascript / typescript
;;
(leaf add-node-modules-path
  :straight t
  :commands add-node-modules-path)

(leaf prettier-js
  :straight t
  :diminish t
  ;; :commands prettier-js-mode
  ;; :custom
  ;; (prettier-js-args . ("--print-width" "120"
  ;;                      "--single-quote" "true"
  ;;                      "--trailing-comma" "none"
  ;;                      "--tab-width" "2"))
  )

(leaf tide
  :straight t
  :commands setup-tide-mode
  :after typescript-mode company flycheck
  :custom
  (typescript-indent-level . 2)
  (js-indent-level . 2)
  (js2-basic-offset . 2)
  (web-mode-code-indent-offset . 2)
  (web-mode-markup-indent-offset . 2)
  (tide-format-options . '(:indentSize 2 :tabSize 2))
  ;; aligns annotation to the right hand side
  (company-tooltip-align-annotations . t)
  :hook
  (typescript-mode . setup-tide-mode)
  (typescript-mode . tide-hl-identifier-mode)
  ;; formats the buffer before saving
                                        ;(before-save-hook . tide-format-before-save)
  ;; (before-save-hook . prettier-js) は全ファイルの保存時に prettier を
  ;; 走らせてしまうグローバル登録だった。setup-tide-mode の中で
  ;; prettier-js-mode を有効にしており、そちらがバッファローカルに
  ;; before-save-hook を張るので不要。
  :config
  (defun setup-tide-mode ()
    (interactive)
    (add-node-modules-path)
    (tide-setup)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-mode +1)
    ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-check-syntax-automatically '(idle-change))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)
    ;;
    (prettier-js-mode)))

(leaf typescript-mode
  :straight t
  :hook (typescript-mode-hook . setup-tide-mode))

(leaf web-mode
  :straight t
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.njk\\'" . web-mode))
  :hook
  (web-mode-hook . (lambda ()
                     (when (string-equal "tsx" (file-name-extension buffer-file-name))
                       (setup-tide-mode))
                     (setq tab-width 4)
                     (indent-tabs-mode 0)
                     (whitespace-mode)))
  :config
  ;; enable typescript-tslint checker
  ;; なお (prettier-js-mode) を :config トップレベルで呼んでいたが、これは
  ;; web-mode のロード時点のカレントバッファに対して実行されてしまうため削除した。
  ;; tsx は上の :hook から setup-tide-mode 経由で prettier-js-mode が有効になる。
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(leaf js2-mode
  :straight t
  :mode
  ("\\.js"   . js2-mode)
  ("\\.json" . javascript-mode)
  ("\\.cjs"   . js2-mode)
  :hook
  (js2-mode-hook . (lambda ()
                     (setq tab-width 4)
                     (indent-tabs-mode)
                     (whitespace-mode))))

(leaf scss-mode
  :straight t
  :mode ("\\.\\(scss\\|css\\)\\'" . scss-mode)
  :custom
  (scss-compile-at-save . nil) ;; 自動コンパイルをオフにする
  (css-indent-offset . 2)
  (scss-compile-at-save . nil)
  ;; (yas-minor-mode) は :config トップレベルにあり、scss-mode のロード時点の
  ;; カレントバッファに対して実行されてしまっていた。:hook へ移動する。
  :hook (scss-mode-hook . yas-minor-mode)
  :bind
  (:scss-mode-map
   ("\M-{" . my:css-electric-pair-brace)
   (";" . my:semicolon-ret))
  :config
  (defun my:css-electric-pair-brace ()
    (interactive)
    (insert "{")
    (newline-and-indent)
    (newline-and-indent)
    (insert "}")
    (indent-for-tab-command)
    (forward-line -1)
    (indent-for-tab-command))
  (defun my:semicolon-ret ()
    (interactive)
    (insert ";")
    (newline-and-indent)))

(provide 'my-lang-web)
;;; my-lang-web.el ends here
