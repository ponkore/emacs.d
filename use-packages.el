(require 'leaf)

(leaf leaf-keywords
  :require t
  :config
  (leaf-keywords-init))

(leaf diminish :straight t)
(leaf hydra :straight t)

;;
;; ivy (https://qiita.com/blue0513/items/c0dc35a880170997c3f5)
;;
(leaf *interactive-search
  :config

  (leaf ivy
    :straight t
    :diminish t
    :custom
    (ivy-use-virtual-buffers . t)
    (ivy-height . 20)
    (ivy-extra-directories . nil)
    (ivy-wrap . t)
    (ivy-re-builders-alist . '((t . ivy--regex-plus)))
    (ivy-extra-directories . '("../" "./"))
    (ivy-count-format . "(%d/%d) ")
    (ivy-format-functions-alist . '((t . ivy-format-function-arrow)))
    :bind (:ivy-minibuffer-map ("C-l" . ivy-backward-delete-char))
    :hook (emacs-startup-hook . ivy-mode)
    :config
    (when (require 'ivy-hydra nil t)
      (setq ivy-read-action-function #'ivy-hydra-read-action))
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (defface my-ivy-arrow-visible
      '((((class color) (background light)) :foreground "orange")
        (((class color) (background dark)) :foreground "#EE6363"))
      "Face used by Ivy for highlighting the arrow.")
    (defface my-ivy-arrow-invisible
      '((((class color) (background light)) :foreground "#FFFFFF")
        (((class color) (background dark)) :foreground "#31343F"))
      "Face used by Ivy for highlighting the invisible arrow."))

  (leaf ivy-rich
    :straight t
    :hook (emacs-startup-hook . ivy-rich-mode)
    :config
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))

    (let ((rich-transformer-config '(:columns
                                     ((ivy-rich-switch-buffer-icon :width 2)
                                      (ivy-rich-candidate (:width 30))
                                      (ivy-rich-switch-buffer-size (:width 7))
                                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                                     :predicate
                                     (lambda (cand) (get-buffer cand)))))
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer
                 rich-transformer-config)
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer-other-window
                 rich-transformer-config)))

  (leaf counsel
    :straight t
    :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-x C-b" . counsel-ibuffer)
    ("C-x C-r" . counsel-recentf)
    :custom
    (counsel-find-file-ignore-regexp . (regexp-opt '("./" "../")))
    :config
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (progn
                  (mapcar #'substring-no-properties recentf-list) ;; no need?
                  (mapcar #'abbreviate-file-name recentf-list)) ;; ~/
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'ad:counsel-recentf))

  (leaf swiper
    :straight t
    :bind
    ("C-s" . swiper)
    (:swiper-map ("C-w" . ivy-yank-word))
    :custom (swiper-include-line-number-in-search . t))

  ;; TODO migemo + swiper（日本語をローマ字検索できるようになる）
  ;; (require 'avy-migemo)
  ;; (avy-migemo-mode 1)
  ;; (require 'avy-migemo-e.g.swiper)
  )

(leaf *modelines
  :config

  (leaf doom-modeline
    :straight t
    :commands (doom-modeline-def-modeline)
    :custom
    (doom-modeline-buffer-file-name-style . 'truncate-with-project)
    (doom-modeline-icon . t)
    (doom-modeline-major-mode-icon . nil)
    (doom-modeline-minor-modes . nil)
    (line-number-mode . 0)
    (column-number-mode . 0)
    :hook (emacs-startup-hook . doom-modeline-mode)
    :config
    (doom-modeline-def-modeline
      'main
      ;; '(workspace-number bar window-number evil-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
      '(bar matches buffer-info buffer-position selection-info)
      '(misc-info debug minor-modes input-method major-mode process vcs checker))))

(leaf *utility-package
  :config

  (leaf all-the-icons
    :straight t
    :custom
    (all-the-icons-scale-factor . 1.0)
    :config
    (when window-system
      (defun my-ivy-format-function-arrow (cands)
        "Transform CANDS into a string for minibuffer."
        (ivy--format-function-generic
         (lambda (str)
           (concat (all-the-icons-faicon
                    "hand-o-right"
                    :v-adjust -0.2 :face 'my-ivy-arrow-visible)
                   " " (ivy--add-face str 'ivy-current-match)))
         (lambda (str)
           (concat (all-the-icons-faicon
                    "hand-o-right" :face 'my-ivy-arrow-invisible) " " str))
         cands
         "\n"))
      (require 'ivy nil t)
      (require 'all-the-icons-ivy nil t)
      (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))
      (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-projectile-switch-project)
      (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-ibuffer)
      (all-the-icons-ivy-setup)
      (setq ivy-format-functions-alist '((t . ivy-format-function-arrow)))))

  (leaf s
    :straight t
    :commands s-join))

(leaf *major-mode
  :config

  (leaf magit
    :straight t
    :hook (magit-mode-hook . my:magit-setup-diff)
    :config
    (defun magit-expand-git-file-name--msys (args)
      "Handle Msys directory names such as /c/* by changing them to C:/*"
      (let ((filename (car args)))
        (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
          (setq filename (concat (match-string 1 filename) ":/"
                                 (match-string 2 filename))))
        (list filename)))
    (advice-add 'magit-expand-git-file-name :filter-args #'magit-expand-git-file-name--msys)
    ;; diff関連の設定
    ;; 2012-04-02
    (defun my:magit-setup-diff ()
      ;; diffを表示しているときに文字単位での変更箇所も強調表示する
      ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
      ;; 2012-04-02
      (setq magit-diff-refine-hunk 'all)
      ;; diff用のfaceを設定する
      ;; 2012-04-02
      (diff-mode-setup-faces)))

  (leaf markdown-mode
    :straight t
    :mode ("\\.\\(markdown\\|md\\)\\.txt\\'" . markdown-mode)
    :config
    (defface markdown-inline-code-face
      '((t (:inherit (markdown-code-face font-lock-constant-face))))
      "Face for inline code."
      :group 'markdown-faces))

  (use-package rst
    :straight t
    :mode ("\\.\\(rst|rest\\)$" . rst-mode)
    :hook (rst-mode-hook . (lambda ()
                             (setq indent-tabs-mode nil)
                             (setq frame-background-mode 'dark))))
  ;; (let ((i 1))
  ;;   (while (<= i rst-level-face-max)
  ;;     (let ((face-name (intern (format "rst-level-%d-face" i))))
  ;;       (set-face-background face-name nil)
  ;;       (setq i (1+ i)))))
  )

(leaf *minor-mode
  :config

  (leaf yasnippet
    :straight t
    :bind (:yas-minor-mode-map
           ("TAB" . nil)
           ("<tab>" . nil)
           ("<C-tab>" . yas-expand)
           ("C-x i i" . yas-insert-snippet)
           ("C-x i n" . yas-new-snippet)
           ("C-x i v" . yas-visit-snippet-file))
    :commands yas-expand yas-global-mode yas-insert-snippet yas-visit-snippet-file
    :hook (emacs-startup-hook . yas-global-mode))

  (leaf symbol-overlay
    :straight t
    :bind
    ("M-i" . symbol-overlay-put)
    (:symbol-overlay-map
     ("p" . symbol-overlay-jump-prev)
     ("n" . symbol-overlay-jump-next)
     ("C-g" . symbol-overlay-remove-all))
    :hook
    (prog-mode-hook . symbol-overlay-mode)
    (markdown-mode-hook . symbol-overlay-mode))

  (leaf smartparens
    :straight t
    :commands smartparens-global-mode sp-local-pairs
    :hook (emacs-startup-hook . smartparens-global-mode)
    :config
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "`" nil :actions nil))

  (leaf expand-region
    :straight t
    :commands er/expand-region
    :bind ("C-=" . er/expand-region))
  )

(leaf *company-packages
  :config

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
    :hook
    (emacs-startup-hook . global-company-mode)
    :custom
    (company-idle-delay . 0)
    (company-echo-delay . 0)
    (company-minimum-prefix-length . 1) ;; 1文字入力で補完されるように
    (company-selection-wrap-around . t) ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
    (company-tooltip-limit . 20)
    (company-tooltip-align-annotations . t)
    (company-begin-commands . '(self-insert-command)))

  (leaf company-quickhelp
    :straight t
    :custom
    (company-quickhelp-color-foreground . "black")
    :bind (:company-active-map
           :package company
           ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode-hook . company-quickhelp-mode))

  (leaf company-box
    :straight t
    :after all-the-icons
    :hook
    (company-mode-hook . company-box-mode)
    (global-company-mode-hook . company-box-mode)
    :custom
    (company-box-doc-enable . t)
    (company-box-show-single-candidate . t)
    (company-box-max-candidates . 50)
    (company-box-icons-alist . 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)

    ;; great configuration for company-box with all-the-icons
    ;; https://ladicle.com/post/config/#company
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))
  )

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (require 'cl-lib)
  (require 'color)
  ;;(global-rainbow-delimiters-mode)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))

(use-package open-junk-file
  :bind (("C-x j" . open-junk-file))
  :config
  (setq open-junk-file-format "~/Downloads/junk/%Y-%m-%d-%H%M%S."))

(use-package dired
  :init
  ;; バージョン管理システム
  ;; diredから適切なバージョン管理システムの*-statusを起動
  (defun dired-vc-status (&rest args)
    (interactive)
    (let ((path (find-path-in-parents (dired-current-directory)
                                      '(".git" ".svn"))))
      (cond ((null path)
             (message "not version controlled."))
            ((string-match-p "\\.svn$" path)
             (svn-status (file-name-directory path)))
            ((string-match-p "\\.git$" path)
             (magit-status-internal (file-name-directory path))))))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "V" 'dired-vc-status)))
  ;;
  ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
  ;;
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (setq dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (setq dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (setq dired-isearch-filenames t)
  ;;
  ;; dired-k
  ;;
  (require 'dired-k)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (define-key dired-mode-map "K" 'dired-k)
  (define-key dired-mode-map "g" 'dired-k))

(use-package org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq org-directory "~/Downloads/junk")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))))

;;;
;;; projectile
;;;
(use-package projectile
  :bind (("<f12>" . projectile-toggle-between-implementation-and-test))
  :config
  (projectile-mode 1))

;;
;; shell-mode
;;
(use-package shell
  :defer t
  :init
  ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
  (add-hook 'shell-mode-hook
            (lambda ()
              (face-remap-set-base 'comint-highlight-prompt :inherit nil))))

;;;;;;;;;;;;;;;
;; PROGMODES ;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
;; Elisp用の設定
;; 2012-03-18
(use-package emacs-lisp-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "ros run")
  (slime-setup '(slime-repl slime-fancy slime-banner slime-company))
  (bind-keys :map company-active-map
             ("C-d" . 'company-show-doc-buffer)
             ("M-." . 'company-show-location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clojure-mode
  :defer t
  :mode (("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode))
  :config
  (require 'yasnippet)
  (require 'projectile)
  (require 'flycheck-joker)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook (lambda () (define-key clojure-mode-map (kbd "C-c t") 'projectile-toggle-between-implementation-and-test)))
  (define-clojure-indent
       (defroutes 'defun)
       (tabular 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2)
       (componentWillMount 'defun)
       (componentDidMount 'defun)
       (componentWillUnmount 'defun)
       ;; for om.next
       (ident 'defun)
       (query 'defun)
       (params 'defun)
       (render 'defun)
       ;;
       (fact 'defun)
       (do-transaction 'defun)))

(use-package cider
  :defer t
  :config
  (require 'company)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-result-prefix ";; => ")
  (setq nrepl-sync-request-timeout 40)
  (setq nrepl-hide-special-buffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; http://nobunaga.hatenablog.jp/entry/2017/09/24/221004
;; https://qiita.com/ignorant/items/50f8eb2852d0f0214659
;;
;; M-x jedi:install-server
(use-package company-jedi
  :ensure t
  :after company
  :config
  (setq jedi:complete-on-dot t)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

;; https://elpy.readthedocs.io/en/latest/index.html
(use-package elpy
  :config
  (package-initialize)
  (elpy-enable)
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "~/.pyenv/shims/python3"))

;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python3" . python-mode)
;;   :config
;;   ;; prefer python3
;;   (setq python-shell-interpreter "python3")
;;   ;; https://github.com/jorgenschaefer/elpy/issues/887
;;   (setq python-shell-completion-native-enable nil)
;;   ;; https://emacs.stackexchange.com/questions/16361/how-to-automatically-run-inferior-process-when-loading-major-mode
;;   (defun my-run-python ()
;;     (save-selected-window
;;       (switch-to-buffer-other-window (process-buffer (python-shell-get-or-create-process (python-shell-parse-command))))))
;;   (add-hook 'python-mode-hook 'my-run-python))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; php
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode
  :defer t
  :init
  (add-hook 'php-mode-hook
            (lambda ()
              ;; http://oh-sky.hatenablog.com/entry/2013/07/07/004651
              (setq tab-width 4)
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setup racer (see https://github.com/racer-rust/emacs-racer)
;;
(use-package rust-mode
  :defer t
  :config
  (require 'racer)
  (require 'flycheck)
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/"))
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-mode)))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (require 'company)
  (require 'eldoc)
  (add-hook 'racer-mode-hook (lambda ()
                               (company-mode)
                               (eldoc-mode)))
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key rust-mode-map (kbd "C-c d") #'racer-describe)
  (setq rust-format-on-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cc-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C言語と同じような構文のプログラミング言語用の設定
;; 2012-03-18
(use-package cc-mode
  :init
  ;; c-modeやc++-modeなどcc-modeベースのモード共通の設定
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; BSDスタイルをベースにする
              (c-set-style "bsd")

              ;; スペースでインデントをする
              (setq indent-tabs-mode nil)

              ;; インデント幅を2にする
              (setq c-basic-offset 2)

              ;; 自動改行（auto-new-line）と
              ;; 連続する空白の一括削除（hungry-delete）を
              ;; 有効にする
              (c-toggle-auto-hungry-state 1)

              ;; CamelCaseの語でも単語単位に分解して編集する
              ;; GtkWindow         => Gtk Window
              ;; EmacsFrameClass   => Emacs Frame Class
              ;; NSGraphicsContext => NS Graphics Context
              (subword-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package csharp-mode
  :defer t
  :config
  ;; C# 編集用のモード (flymake は一旦やめておく)
  (require 'yasnippet)
  (defun my-csharp-mode-fn ()
    "my function that runs when csharp-mode is initialized for a buffer."
    (turn-on-font-lock)
    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
    (setq indent-tabs-mode nil) ;; tabs are evil
    (yas-minor-mode-on)
    (setq comment-column 40)
    (setq c-basic-offset 4)
    ;; (font-lock-add-magic-number)
    ;; オフセットの調整
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    ;; see http://qiita.com/masnagam/items/e3313dc9a66bd7fd76fa
    (setq csharp-want-imenu nil))
  (add-hook 'csharp-mode-hook '(lambda() (my-csharp-mode-fn))))
;; see http://ongaeshi.hatenablog.com/entry/20110116/1295187496

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sql-mode
  :defer t
  :mode (("\\.ddl$" . sql-mode))
  :init
  (setq sql-product 'postgres)
  (add-hook 'sql-mode-hook
            (lambda ()
              (yas-minor-mode-on)
              (setq indent-tabs-mode nil)
              (define-key sql-mode-map (kbd "C-c \"") 'wrap-double-quote-thing-at-symbol)
              (define-key sql-mode-map (kbd "C-c ,") 'move-trailing-comma-to-line-start)))
  :config
  (require 'yasnippet)
  (defun oracle-settings ()
    "setup oracle sql environment"
    ;; for SQL mode (My Office PC Oracle setting)
    (when (eq system-type 'windows-nt)
      (setq sql-oracle-program "c:/Apps/Oracle/sqlplus.exe")
      ;; 新規作成のときだけ cp932 にする
      (add-hook 'sql-mode-hook (lambda ()
                                 (unless (file-exists-p (buffer-file-name (current-buffer)))
                                   (set-buffer-file-coding-system 'cp932)
                                   (set-buffer-modified-p nil)))))
    ;; on Mac, set environment variables
    (when (or (eq system-type 'berkeley-unix) (eq system-type 'darwin))
      (let ((oracle-home (expand-file-name "~/Applications/Oracle/instantclient_10_2")))
        (setenv "NLS_LANG" "JAPANESE_JAPAN.UTF8")
        (setenv "DYLD_LIBRARY_PATH" oracle-home)
        (setenv "LD_LIBRARY_PATH" oracle-home)
        (setq sql-oracle-program (concat oracle-home "/sqlplus"))))
    ;; set Oracle as default SQL product.
    (setq sql-product 'oracle)
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (setq comint-output-filter-functions 'comint-truncate-buffer)
                (toggle-truncate-lines t)
                (when (eq system-type 'windows-nt)
                  (set-buffer-process-coding-system 'cp932 'cp932))
                (comint-send-string (get-buffer-process (current-buffer)) "
ALTER SESSION SET NLS_DATE_FORMAT='YYYY/MM/DD'
/
set linesize 1000
set trimspool on
set timing on
set pagesize 1000
")))
    ;; only for my office environment
    (load (expand-file-name "~/.emacs.d/config-sqlplus.el") t)
    ;; customize font-lock
    (font-lock-add-keywords 'sql-mode
                            '(("\"\\([^\"]*\\)\"" . 'font-lock-constant-face)
                              ("\\<Hgs\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)
                              ("\\<R[LSC][0-9][A-Z]\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package java-mode
  :defer t
  :mode (("\\.java$" . java-mode))
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (setq tab-width 4))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(use-package tide
  :config
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(use-package typescript-mode
  :defer t
  :init
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package js2-mode
  :mode (("\\.js"   . js2-mode)
         ("\\.json" . javascript-mode))
  :config
  (add-hook 'js2-mode-hook #'setup-tide-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css mode hack (http://d.hatena.ne.jp/sugyan/20120107/1325938248)
;; (defvar ac-source-css-property-names
;;   '((candidates . (loop for property in ac-css-property-alist
;;                         collect (car property)))))
;; (defun my-css-mode-hook ()
;;   (add-to-list 'ac-sources 'ac-source-css-property)
;;   (add-to-list 'ac-sources 'ac-source-css-property-names))
;; (add-hook 'css-mode-hook 'my-css-mode-hook)

;; CSS
;; scss-mode
;; https://github.com/antonj/scss-mode
(use-package scss-mode
  :defer t
  :mode (("\\.\\(scss\\|css\\)\\'" . scss-mode))
  :config
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(use-package bat-mode
  :if (eq system-type 'windows-nt)
  :defer t
  :config
  (setq bat-font-lock-keywords
        (eval-when-compile
          (let ((COMMANDS
                 '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
                   "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
                   "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
                   "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
                   "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
                   "ver" "vol" "xcopy"))
                (CONTROLFLOW
                 '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
                   "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
                (UNIX
                 '("bash" "cat" "cp" "fgrep" "grep" "ls" "sed" "sh" "mv" "rm")))
            `(("\\_<\\(call\\|goto\\)\\_>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
               (2 font-lock-constant-face t))
              ("^:[^:].*"
               . 'bat-label-face)
              ("\\_<\\(defined\\|set\\)\\_>[ \t]*\\(\\w+\\)"
               (2 font-lock-variable-name-face))
              ("%\\([A-Za-z0-9_]+\\)%?"
               (1 font-lock-variable-name-face))
              ("!\\([A-Za-z0-9_]+\\)!?"        ; delayed-expansion !variable!
               (1 font-lock-variable-name-face))
              ("[ =][-/]+\\([A-Za-z0-9_]\\)"
               (1 font-lock-type-face append))
              (,(concat "\\_<" (regexp-opt COMMANDS) "\\_>") . font-lock-builtin-face)
              (,(concat "\\_<" (regexp-opt CONTROLFLOW) "\\_>") . font-lock-keyword-face)
              (,(concat "\\_<" (regexp-opt UNIX) "\\_>") . font-lock-warning-face))))))

;;
;; Visual Basic Mode - visual-basic-mode.el
;;
(use-package visual-basic-mode
  :mode (("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . visual-basic-mode))
  :config
  (setq visual-basic-mode-indent 4)
  (add-hook 'visual-basic-mode-hook
            '(lambda () (setq mode-name "vb"))))

;;
;; gocode coding system
;;
(use-package company-go
  :if (eq system-type 'windows-nt)
  :defer t
  :config
  (defadvice company-go--invoke-autocomplete (around company-go--invoke-autocomplete-adv activate)
    (let ((old-default-process-coding-system default-process-coding-system))
      (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
      ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))
  (ad-activate-regexp "company-go--invoke-autocomplete-adv"))

(use-package go-mode
  :if (eq system-type 'windows-nt)
  :defer t
  :config
  (defun gofmt ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘gofmt-command` (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args`."
  (interactive)
  (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (if gofmt-show-errors (get-buffer-create "*Gofmt Errors*")))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        our-gofmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-truename buffer-file-name)))))
          (setq our-gofmt-args (append our-gofmt-args
                                       gofmt-args
                                       (list "-w" tmpfile)))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'call-process gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already gofmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied gofmt"))
                (if errbuf (gofmt--kill-error-buffer errbuf)))
            (message "Could not apply gofmt")
            (if errbuf (gofmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))
  (defun godef--call (point)
  "Call godef, acquiring definition position and expression
description at POINT."
  (if (not (buffer-file-name (go--coverage-origin-buffer)))
      (error "Cannot use godef on a buffer without a file name")
    (let ((outbuf (generate-new-buffer "*godef*"))
          (coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix))
      (prog2
          (call-process-region (point-min)
                               (point-max)
                               godef-command
                               nil
                               outbuf
                               nil
                               "-i"
                               "-t"
                               "-f"
                               (file-truename (buffer-file-name (go--coverage-origin-buffer)))
                               "-o"
                               ;; Emacs point and byte positions are 1-indexed.
                               (number-to-string (1- (position-bytes point))))
          (with-current-buffer outbuf
            (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (kill-buffer outbuf))))))
