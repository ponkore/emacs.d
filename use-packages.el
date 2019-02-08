;; Helm
;; (some settings derived from https://abicky.net/2014/01/04/170448/ )
(use-package helm
  :bind (("C-c h"   . helm-mini)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("M-y"     . helm-show-kill-ring)
         ("C-c i"   . helm-imenu)
         ("C-x b"   . helm-for-files)
         ("M-y"     . helm-show-kill-ring))
  :commands (helm-previous-page helm-execute-persistent-action
                                helm-next-source
                                helm-previous-source)
  :config
  (require 'helm-config)
  (require 'helm-files)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-o") nil)
  (bind-keys :map helm-map
             ("C-h"   . delete-backward-char)
             ("C-z"   . helm-previous-page)
             ("C-l"   . helm-execute-persistent-action)
             ("C-M-n" . helm-next-source)
             ("C-M-p" . helm-previous-source))
  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))
  ;; Disable helm in some functions
  (require 'helm-mode)
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))

  ;; For find-file etc.
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-bookmarks         ; bookmark の順位を上げた
          helm-source-recentf
          helm-source-file-cache
          helm-source-files-in-current-dir
          helm-source-locate))

  (defun helm-action-copy-selected (msg)
    (save-excursion
      (with-temp-buffer
        (erase-buffer)
        (insert msg)
        (copy-region-as-kill (point-min) (point-max))
        (message (format "%s" msg)))))

  ;;
  ;; deprecated, don't work.
  ;;
  ;; (defvar helm-string-cache (make-hash-table :test 'equal))
  ;;
  ;; (defun read-buf-string (fname)
  ;;   (let ((result (gethash fname helm-string-cache)))
  ;;     (if result result
  ;;       (save-excursion
  ;;         (with-temp-buffer
  ;;           (let* ((buf (set-buffer (find-file-noselect fname)))
  ;;                  (ret (buffer-string)))
  ;;             (kill-buffer buf)
  ;;             (puthash fname ret helm-string-cache)
  ;;             ret))))))
  ;; (defun make-helm-source-from-file (source-name filename execute-action)
  ;;   (when (file-exists-p filename)
  ;;     `((name . ,source-name)
  ;;       (candidates-in-buffer)
  ;;       (init . (lambda () (helm-init-candidates-in-buffer 'global (read-buf-string ,filename))))
  ;;       (action . ,execute-action))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  helm-git-project (http://syohex.hatenablog.com/entry/20121207/1354885367)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun helm-c-sources-git-project-for (pwd)
    (loop for elt in
          '(("Modified files" . "--modified")
            ("Untracked files" . "--others --exclude-standard")
            ("All controlled files in this project" . nil))
          for title  = (format "%s (%s)" (car elt) pwd)
          for option = (cdr elt)
          for cmd    = (format "git ls-files %s" (or option ""))
          collect
          (helm-build-sync-source title
            :candidates (unless (and (not option) (helm-candidate-buffer))
                          (with-current-buffer (helm-candidate-buffer 'global)
                            (call-process-shell-command cmd nil t nil)
                            (list (buffer-string)))))))

  (defun helm-git-project-topdir ()
    (file-name-as-directory
     (let* ((one-line (replace-regexp-in-string
                       "\n" ""
                       (shell-command-to-string "git rev-parse --show-toplevel"))))
       (replace-regexp-in-string "^/\\([a-z]\\)/" "\\1:/" one-line))))

  (defun helm-git-project ()
    (interactive)
    (let ((topdir (helm-git-project-topdir)))
      (unless (file-directory-p topdir)
        (error "I'm not in Git Repository!!"))
      (let* ((default-directory topdir))
        (helm :sources (helm-c-sources-git-project-for default-directory)
              :buffer "*helm git project*"))))
  )

(use-package helm-ag
  :config
  ;; http://emacs.rubikitch.com/helm-ag/
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;; 現在のシンボルをデフォルトのクエリにする
  (setq helm-ag-insert-at-point 'symbol)
  (defun helm-ag-dot-emacs ()
    ".emacs.d以下を検索"
    (interactive)
    (helm-ag "~/.emacs.d/")))

(use-package helm-gtags
  :commands helm-gtags-mode
  :init
  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'csharp-mode-hook 'helm-gtags-mode)
  :config
  ;; Set key bindings
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)  ;; original : M-t
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-rtag) ;; original : M-r
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))
;;
(use-package markdown-mode
  :mode (("\\.\\(markdown\\|md\\)\\.txt\\'" . markdown-mode))
  :init
  (defface markdown-inline-code-face
    '((t (:inherit (markdown-code-face font-lock-constant-face))))
    "Face for inline code."
    :group 'markdown-faces))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

(use-package rst
  :defer t)

;; (let ((i 1))
;;   (while (<= i rst-level-face-max)
;;     (let ((face-name (intern (format "rst-level-%d-face" i))))
;;       (set-face-background face-name nil)
;;       (setq i (1+ i)))))

(use-package open-junk-file
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

;;;
;;; yasnippet
;;;
(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; my-yas/prompt(http://syohex.hatenablog.com/entry/20121207/1354885367)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun my-yas/prompt (prompt choices &optional display-fn)
    (let* ((names (loop for choice in choices
                        collect (or (and display-fn (funcall display-fn choice))
                                    choice)))
           (selected (helm-other-buffer
                      `(((name . ,(format "%s" prompt))
                         (candidates . names)
                         (action . (("Insert snippet" . (lambda (arg) arg))))))
                      "*helm yas/prompt*")))
      (if selected
          (let ((n (position selected names :test 'equal)))
            (nth n choices))
        (signal 'quit "user quit!"))))
  ;; (require 'helm-c-yasnippet)
  ;; (setq helm-yas-space-match-any-greedy t) ;[default: nil]
  ;; (global-set-key (kbd "C-c y") 'helm-yas-complete)
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file))

;;(yas-global-mode 1)

;; smartparens に移行してみる
(use-package smartparens-config
  :config
  (smartparens-global-mode t))

;;; expand-region
(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

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
  (defun helm-projectile-ag ()
    "Projectileと連携"
    (interactive)
    (helm-ag (projectile-project-root)))
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (projectile-global-mode))

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
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
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
  (require 'yasnippet)
  (add-hook 'sql-mode-hook
            (lambda ()
              (yas-minor-mode-on)
              (setq indent-tabs-mode nil)
              (define-key sql-mode-map (kbd "C-c \"") 'wrap-double-quote-thing-at-symbol)
              (define-key sql-mode-map (kbd "C-c ,") 'move-trailing-comma-to-line-start))))

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
