;;; css mode hack (http://d.hatena.ne.jp/sugyan/20120107/1325938248)
;; (defvar ac-source-css-property-names
;;   '((candidates . (loop for property in ac-css-property-alist
;;                         collect (car property)))))
;; (defun my-css-mode-hook ()
;;   (add-to-list 'ac-sources 'ac-source-css-property)
;;   (add-to-list 'ac-sources 'ac-source-css-property-names))
;; (add-hook 'css-mode-hook 'my-css-mode-hook)

;; CSS
(defun my-css-electric-pair-brace ()
  (interactive)
  (insert "{")
  (newline-and-indent)
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(defun my-semicolon-ret ()
  (interactive)
  (insert ";")
  (newline-and-indent))

;; scss-mode
;; https://github.com/antonj/scss-mode
(use-package scss-mode
  :defer t
  :mode (("\\.\\(scss\\|css\\)\\'" . scss-mode))
  :config
  (setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
  ;; (ac-css-mode-setup)
  (define-key scss-mode-map "\M-{" 'my-css-electric-pair-brace)
  (define-key scss-mode-map ";" 'my-semicolon-ret)
  (setq css-indent-offset 2)
  (setq scss-compile-at-save nil)
  ;; (setq ac-sources '(ac-source-css-property
  ;;                    ac-source-css-property-names
  ;;                    ac-source-yasnippet
  ;;                    ;; ac-source-words-in-same-mode-buffers
  ;;                    ac-source-words-in-all-buffer
  ;;                    ac-source-dictionary))
  ;; (flymake-mode nil) ;; flymake は日をあらためてちゃんと設定しよう
  (yas-minor-mode))
