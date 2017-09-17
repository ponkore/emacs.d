(require 'company)

(global-set-key (kbd "C-c y") 'company-yasnippet)
(add-hook 'after-init-hook 'global-company-mode)

(company-quickhelp-mode 1)

;;
;; http://qiita.com/syohex/items/8d21d7422f14e9b53b17
;;
(global-set-key (kbd "C-M-i") 'company-complete)

;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; C-sで絞り込む
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

;; TABで候補を設定
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

(setq company-tooltip-limit 20)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

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
    (t (:inherit company-tooltip-selection))))
 '(company-scrollbar-fg
   ((t (:background "orange"))))
 '(company-scrollbar-bg
   ((t (:background "gray40")))))
