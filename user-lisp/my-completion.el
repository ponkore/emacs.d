;;; my-completion.el --- 補完 (vertico / consult / company など)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; 補完
;;; --------------------------------------------------

;;; [3] marginalia

(leaf marginalia
  :straight t)

;;; [3] vertico

(leaf vertico
  ;; 以前は :ensure t (package.el) で入れた上に :config でも
  ;; straight-use-package を呼んでおり、同じパッケージを二重に導入していた。
  ;; extensions 込みのレシピを :straight で最初から宣言する形に統一する。
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :custom
  (vertico-mode . t)
  (vertico-cycle . t)
  ;; 補完候補を最大20行まで表示する
  (vertico-count . 20)
  :hook
  (emacs-startup-hook . vertico-after-init-hook)
  :commands vertico-previous vertico-next
  :bind
  (:vertico-map
   ("C-r" . vertico-previous) ;; C-s/C-rで行を移動できるようにする
   ("C-s" . vertico-next)
   ("C-z" . vertico-scroll-down)
   ("C-v" . vertico-scroll-up))
  :advice
  (:around vertico--format-candidate
           (lambda (orig cand prefix suffix index start)
             (setq cand (funcall orig cand prefix suffix index start))
             (concat
              (if (= vertico--index index)
                  (propertize " " 'face 'vertico-current) ;; "» "
                "   ")
              cand)))
  :config
  (defun vertico-after-init-hook ()
    (marginalia-mode))
  ;; dirty hack...
  (define-key vertico-map (kbd "C-l") 'vertico-directory-delete-char))

(leaf vertico-directory
  :straight t
  :after vertico
  :commands
  vertico-directory-delete-char
  vertico-directory-enter
  vertico-directory-delete-word
  vertico-directory-tidy
  :bind
  (:vertico-map
   ("C-l" . vertico-directory-delete-char)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  `(file-name-shadow-properties . '(invisible t intangible t))
  :config
  (file-name-shadow-mode +1))

;;; [3] consult

(leaf consult
  :straight t
  :bind
  (("C-s" . my:consult-line)
   ("C-x C-r" . consult-recent-file)
   ("C-x l" . consult-goto-line)
   ("C-x b" . consult-buffer))
  :custom
  `((consult-preview-raw-size . 1024000)
    (consult-narrow-key . "<"))
  :init
  ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
  (defun my:consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line))))

;;; [3] embark

(leaf embark
  :straight t
  :disabled t
  :after consult
  :bind (("C-S-a" . embark-act)))

(leaf embark-consult
  :straight t)

;;; [3] orderless

(leaf orderless
  :straight t
  :custom
  ;; 補完スタイルにorderlessを利用する
  `((completion-styles . '(orderless))
    (orderless-matching-styles . '(orderless-prefixes
                                   orderless-regexp
                                   orderless-initialism
                                   orderless-literal))))

;;; [3] corfu (現在未使用)

;; (leaf corfu
;;   :straight t
;;   :commands corfu-global-mode
;;   :custom
;;   (corfu-cycle . t) ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto . t)  ;; Enable auto completion
;;   ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
;;   ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
;;   ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   :init
;;   (corfu-global-mode))

;;; [3] company

;;; [4] company 本体

(leaf company
  :straight t
  :diminish t
  :bind
  ("C-c y" . company-yasnippet)
  ("C-M-i" . company-complete-common-or-cycle)
  (:company-active-map
   ;; C-n, C-pで補完候補を次/前の候補を選択
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ;; C-sで絞り込む
   ("C-s" . company-filter-candidates)
   ;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
   ("<tab>" . company-complete-common-or-cycle)
   ;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
   ("C-h" . nil)
   ;; ドキュメント表示
   ("M-d" . company-show-doc-buffer))
  (:company-search-map
   ;; C-n, C-pで補完候補を次/前の候補を選択
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  (:emacs-lisp-mode-map
   ("C-M-i" . company-complete)) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  :custom
  `((company-idle-delay . 0.5)
    (company-echo-delay . 0.5)
    (company-minimum-prefix-length . 1) ;; 1文字入力で補完されるように
    (company-selection-wrap-around . t) ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
    (company-tooltip-limit . 20)
    (company-tooltip-align-annotations . t)
    (company-transformers . '(company-sort-by-occurrence))
    (company-begin-commands . '(self-insert-command))
    (global-company-mode . t)
    ;; (company-box-background . '((t (:inherit company-tooltip :background "midnight blue"))))
    ;; (company-preview . '((t (:foreground "darkgray" :underline t))))
    ;; (company-preview-common . '((t (:inherit company-preview))))
    ;; (company-scrollbar-bg . '((t (:background "gray40"))))
    ;; (company-scrollbar-fg . '((t (:background "orange"))))
    ;; (company-tooltip . '((t (:background "lightgray" :foreground "black"))))
    ;; (company-tooltip-common . '((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
    ;; (company-tooltip-common-selection . '((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
    ;; (company-tooltip-selection . '((t (:background "steelblue" :foreground "black"))))
    )
  :config
  ;; http://misohena.jp/blog/2021-08-08-emacs-company-mode-settings.html
  ;; 無選択状態の時にTABやRETが入力されたら、そのバッファのモード本来のTABやRETを実行する。
  (defun my-company-complete-respecting-user-input (&rest args)
    "ユーザー入力を尊重した補完を行う。"
    (interactive)
    (if (null company-selection)
        ;; モード本来の割り当てを実行する。
        (progn
          (company-abort)
          (company--unread-this-command-keys))
      ;; companyの(リマップ元の)コマンドを実行する。
      (apply this-original-command args)))
  (define-key company-active-map [remap company-complete-selection]
              ;;RETに割り当てられているコマンドをリマップ
              'my-company-complete-respecting-user-input)
  (define-key company-active-map [remap company-complete-common]
              ;;TABに割り当てられているコマンドをリマップ
              'my-company-complete-respecting-user-input))

;;; [4] company-quickhelp

(leaf company-quickhelp
  :straight t
  :custom
  (company-quickhelp-color-foreground . "black")
  :bind (:company-active-map
         :package company
         ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode-hook . company-quickhelp-mode))

;;; [4] company-box

(leaf company-box
  :straight t
  :diminish t
  :after all-the-icons
  :hook
  (company-mode-hook . company-box-mode)
  (global-company-mode-hook . company-box-mode)
  :custom
  (company-box-doc-enable . t)
  (company-box-show-single-candidate . t)
  (company-box-max-candidates . 50)
  (company-box-background . '((t (:inherit company-tooltip :background "midnight blue"))))
  (company-box-icons-alist . 'company-box-icons-all-the-icons)
  (company-box-backends-colors . '((company-yasnippet . (:candidate "yellow" :annotation some-face))
                                   (company-elisp . (:icon "yellow" :selected
                                                           (:background "orange" :foreground "black")))
                                   (company-dabbrev . "purple")))
  :config
  ;; great configuration for company-box with all-the-icons
  ;; https://ladicle.com/post/config/#company
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-fileicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.7  :v-adjust -0.15))
          (Text          . ,(all-the-icons-faicon   "book"                     :height 0.68 :v-adjust -0.15))
          (Method        . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Function      . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Constructor   . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
          (Field         . ,(all-the-icons-faicon   "tags"                     :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
          (Variable      . ,(all-the-icons-faicon   "tag"                      :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face))
          (Class         . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01  :face 'font-lock-constant-face))
          (Interface     . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01))
          (Module        . ,(all-the-icons-octicon  "package"                  :height 0.7  :v-adjust -0.15))
          (Property      . ,(all-the-icons-octicon  "package"                  :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
          (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.7  :v-adjust -0.15))
          (Value         . ,(all-the-icons-material "format_align_right"       :height 0.7  :v-adjust -0.15 :face 'font-lock-constant-face))
          (Enum          . ,(all-the-icons-material "storage"                  :height 0.7  :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.7  :v-adjust -0.15))
          (Snippet       . ,(all-the-icons-faicon   "code"                     :height 0.7  :v-adjust 0.02  :face 'font-lock-variable-name-face))
          (Color         . ,(all-the-icons-material "palette"                  :height 0.7  :v-adjust -0.15))
          (File          . ,(all-the-icons-faicon   "file-o"                   :height 0.7  :v-adjust -0.05))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.7  :v-adjust -0.15))
          (Folder        . ,(all-the-icons-octicon  "file-directory"           :height 0.7  :v-adjust -0.05))
          (EnumMember    . ,(all-the-icons-material "format_align_right"       :height 0.7  :v-adjust -0.15 :face 'all-the-icons-blueb))
          (Constant      . ,(all-the-icons-faicon   "tag"                      :height 0.7  :v-adjust -0.05))
          (Struct        . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01  :face 'font-lock-constant-face))
          (Event         . ,(all-the-icons-faicon   "bolt"                     :height 0.7  :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator      . ,(all-the-icons-fileicon "typedoc"                  :height 0.65 :v-adjust 0.05))
          (TypeParameter . ,(all-the-icons-faicon   "hashtag"                  :height 0.65 :v-adjust 0.07  :face 'font-lock-const-face))
          (Template      . ,(all-the-icons-faicon   "code"                     :height 0.7  :v-adjust 0.02  :face 'font-lock-variable-name-face)))))

(provide 'my-completion)
;;; my-completion.el ends here
