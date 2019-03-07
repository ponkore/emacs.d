;;
;; ivy (https://qiita.com/blue0513/items/c0dc35a880170997c3f5)
;;
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20) ;; minibufferのサイズを拡大！（重要）
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file) ;; find-fileもcounsel任せ！
         ("C-x C-r" . counsel-recentf))
  :config
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../"))))

(use-package swiper
  :bind (("C-s" . swiper))
  :config
  (setq swiper-include-line-number-in-search t) ;; line-numberでも検索可能
  ;; ;; migemo + swiper（日本語をローマ字検索できるようになる）
  ;; (require 'avy-migemo)
  ;; (avy-migemo-mode 1)
  ;; (require 'avy-migemo-e.g.swiper)
  )

(use-package symbol-overlay
  :bind (("M-i" . symbol-overlay-put))
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all) ;; ハイライトキャンセル
  )

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

;;;
;;; yasnippet
;;;
(use-package yasnippet
  ;; :bind (("C-x i i" . yas-insert-snippet)
  ;;        ("C-x i n" . yas-new-snippet)
  ;;        ("C-x i v" . yas-visit-snippet-file))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
  )

;; smartparens に移行してみる
(use-package smartparens-config
  :config
  (smartparens-global-mode t))

;;; expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region))
  ;; :init
  ;; (global-set-key (kbd "C-=") 'er/expand-region)
  )

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
