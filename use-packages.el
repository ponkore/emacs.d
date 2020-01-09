(require 'leaf)

(leaf leaf-keywords
  :require t
  :config
  (leaf-keywords-init))

(leaf diminish :straight t)
(leaf hydra :straight t)

(leaf *encoding
  :config
  (leaf encoding-mac
    :if (eq system-type 'darwin)
    :config
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))
  (leaf encoding-windows
    :if (eq system-type 'windows-nt)
    :config
    (setq default-process-coding-system '(utf-8 . utf-8))))

(leaf *font-setting
  :config
  (defun emacs-font-setting (font-name size)
    "Set emacs japanese fonts."
    ;; Note:
    ;; https://qiita.com/melito/items/238bdf72237290bc6e42
    ;; [NG] ricty だと、[] の下が欠ける
    ;; (set-frame-font "ricty-12")
    ;; [NG] noto mono だと全角文字が半角の２倍幅になっていない
    ;; (set-frame-font "noto mono-10")
    ;; [△] Consolas & Meiryoke_Console だと丸付き数字(①等)が半角幅になってしまっている
    ;; [△] Inconsolata & Meiryoke_Console だと全角○が半角幅になってしまっている
    ;; [△] Meiryoke_Console 統一だと文字幅問題はないが、行高さが詰まりすぎ、O0liの区別がつきにくい
    ;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
    ;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
    (let* ((asciifont font-name)
           (jpfont font-name)
           (h (round (* size 10)))
           (fontspec (font-spec :family asciifont))
           (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil '(#x0080 . #x024F) fontspec)
      (set-fontset-font nil '(#x0370 . #x03FF) fontspec)
      (when (require 'all-the-icons nil t)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-material-family)) nil 'append)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-faicon-family)) nil 'append)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-octicon-family)) nil 'append)
	(set-fontset-font nil 'unicode (font-spec :family (all-the-icons-wicon-family)) nil 'append))
      (setq face-font-rescale-alist '((font-name . 1.0)))))
  (when (eq system-type 'darwin)
    (emacs-font-setting "Ricty Diminished" 16))
  (when (eq system-type 'windows-nt)
    (emacs-font-setting "Hackgen" 10)))

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
    :hook (emacs-startup-hook . doom-modeline-mode)
    :config
    (line-number-mode 0)
    (column-number-mode 0)
    (which-function-mode 0)
    (doom-modeline-def-modeline
      'main
      ;; '(workspace-number bar window-number evil-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
      '(bar matches buffer-info buffer-position selection-info)
      '(misc-info debug minor-modes input-method major-mode process vcs checker))))

(leaf *utility-package
  :config

  (leaf all-the-icons-ivy
    :straight t)

  (leaf all-the-icons
    :straight t
    :after all-the-icons-ivy ivy
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
      (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))
      (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-projectile-switch-project)
      (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-ibuffer)
      (all-the-icons-ivy-setup)
      (setq ivy-format-functions-alist '((t . ivy-format-function-arrow)))))

  (leaf s
    :straight t
    :commands s-join)

  ;;   ;;
  ;;   ;; exec path setting ( http://qiita.com/catatsuy/items/3dda714f4c60c435bb25 )
  ;;   ;;
  ;;   (defun set-exec-path-from-shell-PATH ()
  ;;     "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  ;; This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  ;;     (interactive)
  ;;     (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "/bin/bash --login -i -c 'echo $PATH'"))))
  ;;       (setenv "PATH" path-from-shell)
  ;;       (setq exec-path (split-string path-from-shell path-separator))))
  ;;   (set-exec-path-from-shell-PATH)

  (leaf exec-path-from-shell
    :straight t
    :commands exec-path-from-shell-copy-envs
    :config
    (mapc #'(lambda (f)
              (add-to-list 'exec-path (expand-file-name f)))
          (s-split ":" (exec-path-from-shell-getenv "PATH")))
    (let ((envs '("GOROOT" "GOPATH")))
      (exec-path-from-shell-copy-envs envs))))

(leaf *dired
  :config
  (leaf dired-k
    :straight t)
  (leaf dired
    :commands dired-vc-status
    :bind
    (:dired-mode-map
     ("V" . dired-vc-status)
     ("K" . dired-k)
     ("g" . dired-k))
    :hook
    (dired-mode-hook . dired-k)
    (dired-initial-position-hook . dired-k)
    :custom
    ;;
    ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
    ;;
    ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
    (dired-dwim-target . t)
    ;; ディレクトリを再帰的にコピーする
    (dired-recursive-copies . 'always)
    ;; diredバッファでC-sした時にファイル名だけにマッチするように
    (dired-isearch-filenames . t)
    :config
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
	       (magit-status-internal (file-name-directory path)))))))
  (leaf vc-windows
    :if (eq system-type 'windows-nt)
    :hook
    ;; svn log の出力は cp932
    (vc-svn-log-view-mode-hook . (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))
    :config
    ;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
    ;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)
    (defadvice vc-svn-command (around vc-svn-coding-system-setup compile)
      (let ((old-default-process-coding-system default-process-coding-system))
	(setq default-process-coding-system '(utf-8 . cp932))
	ad-do-it
	(setq default-process-coding-system old-default-process-coding-system)))
    (ad-activate-regexp "vc-svn-coding-system-setup")))

(leaf *major-mode
  :config

  (leaf *shell
    :config
    (leaf shell-windows
      :if (eq system-type 'windows-nt)
      :hook
      (shell-mode-hook . (lambda ()
                           ;; シェルモードの入出力文字コード(cp932 -> utf-8)
                           ;; (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
                           ;; (set-buffer-file-coding-system    'utf-8-unix)
                           (set-buffer-process-coding-system 'cp932 'cp932)
                           (set-buffer-file-coding-system    'cp932)))
      :custom
      (explicit-shell-file-name . "bash.exe")
      (shell-command-switch . "-c")
      (shell-file-name . "bash.exe")
      :config
      (require 'shell)
      ;; (M-! and M-| and compile.el)
      (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
      ;; エスケープシーケンス処理の設定
      (autoload 'ansi-color-for-comint-mode-on "ansi-color"
        "Set `ansi-color-for-comint-mode' to t." t))
    (leaf shell
      :hook
      ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
      (shell-mode-hook . (lambda ()
                           (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
      ;; or shellモードの時の^M抑制 (どっちが正しい？)
      ;; (comint-output-filter-functions . shell-strip-ctrl-m nil t)
      :config
      ;; shell-modeでの補完 (for drive letter)
      (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")))

  (leaf *org-mode
    :config
    (leaf org
      :straight t
      :mode ("\\.org$" . org-mode)
      :hook (org-mode-hook . turn-on-font-lock)
      :custom (;; org-mode内部のソースを色付けする
               (org-src-fontify-natively . t)
               ;; org-modeの開始時に、行の折り返しを無効にする。
               (org-startup-truncated . t)
               ;; follow-linkから戻ることを可能とする。
               (org-return-follows-link . t)

               (org-refile-use-outline-path . 'file)
               (org-outline-path-complete-in-steps . nil)
               (org-log-done . t)
               (org-todo-keywords . '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

               (org-indent-indentation-per-level . 0)
               (org-adapt-indentation . nil)
               (org-clock-clocked-in-display . 'none)
               (org-clock-out-remove-zero-time-clocks . t))
      :config
      ;; 一時間に一回、org-modeの全てのバッファを保存する。
      (run-at-time "00:59" 3600 #'org-save-all-org-buffers)
      (leaf *org-local-functions
        :config
        (defun my:org-add-ymd-to-archive (name)
          "replace anchor to YYYY-MM string"
          (let* ((ymd (format-time-string "%Y-%m")))
            (replace-regexp-in-string "#YM" ymd name)))
        (advice-add 'org-extract-archive-file :filter-return #'my:org-add-ymd-to-archive)))
    (leaf org-bullets
      :straight t
      :custom (org-bullets-bullet-list . '("" "" "" "" "" "" ""))
      :hook (org-mode-hook . org-bullets-mode)))

  (leaf magit
    :straight t
    :hook (magit-mode-hook . my:magit-setup-diff)
    :commands magit-status-internal
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

  (leaf rst
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

  (leaf *emacs-lisp
    :config
    (leaf elisp-mode
      :require t
      :preface
      (defun my:emacs-lisp-hooks ()
        (setq-local company-idle-delay 0.2)
        (setq-local company-backends '(company-semantic company-files company-elisp))
        (setq-local show-paren-style 'expression)
        ;; (set-newline-and-indent)
	)
      :hook
      (emacs-lisp-mode-hook . my:emacs-lisp-hooks)))

  (leaf *lisp
    :config
    (leaf slime
      :straight t
      :commands slime-setup
      :after company
      :custom
      (inferior-lisp-program . "ros run")
      :bind
      (:company-active-map
       ("C-d" . company-show-doc-buffer)
       ("M-." . company-show-location))
      :config
      (slime-setup '(slime-repl slime-fancy slime-banner slime-company))))

  (leaf *clojure
    :config
    (leaf clojure-mode
      :straight t
      :commands define-clojure-indent
      :mode ("\\(default\\|user\\|emacs\\)\.\\(behaviors\\|keymap\\)" . clojure-mode)
      :hook
      (clojure-mode-hook . yas-minor-mode)
      (clojure-mode-hook . smartparens-strict-mode)
      (clojure-mode-hook . flycheck-mode)
      :config
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

    (leaf cider
      :straight t
      :hook
      (cider-repl-mode-hook . company-mode)
      (cider-mode-hook . company-mode)
      :custom
      (cider-show-error-buffer . t)
      (cider-auto-select-error-buffer . t)
      (cider-repl-result-prefix . ";; => ")
      (nrepl-sync-request-timeout . 40)
      (nrepl-hide-special-buffers . t))

    (leaf cider-lein-command-on-windows
      :if (eq system-type 'windows-nt)
      :config
      ;; on Windows, use lein.bat instead of lein shell script.
      (setq cider-lein-command "lein.bat")))

  (leaf *python
    :config
    (leaf pyvenv
      :straight t
      :config
      ;; (pyvenv-activate my:virtualenv-path)
      nil)
    (leaf python
      :mode ("\\.py$" . python-mode)
      :hook
      (python-mode-hook . my:python-mode-hook-0)
      :preface
      (defun my:python-mode-hook-0 ()
        (setq-local indent-tabs-mode nil)
        (flycheck-mode +1)))
;;     (leaf company-jedi
;; ;; http://nobunaga.hatenablog.jp/entry/2017/09/24/221004
;; ;; https://qiita.com/ignorant/items/50f8eb2852d0f0214659
;; ;;
;; ;; M-x jedi:install-server
;;       :straight t
;;       :after company
;;       :custom
;;       (jedi:complete-on-dot . t)
;;       :hook (python-mode-hook . (lambda () (add-to-list 'company-backends 'company-jedi))))
    (leaf elpy
      ;; https://elpy.readthedocs.io/en/latest/index.html
      :straight t
      :config
      (package-initialize)
      (elpy-enable)
      (setenv "WORKON_HOME" "~/.pyenv/versions/")
      (setq elpy-rpc-backend "jedi")
      (setq python-shell-interpreter "~/.pyenv/shims/python3")))

  (leaf *php
    :config
    (leaf php-mode
      :straight t
      :hook
      (php-mode-hook . (lambda ()
                         ;; http://oh-sky.hatenablog.com/entry/2013/07/07/004651
                         (setq tab-width 4)
                         (setq indent-tabs-mode nil)
                         (setq c-basic-offset 4)))))

  (leaf javascript/typescript
    :config
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

    (leaf tide
      :straight t
      :custom
      (typescript-indent-level . 2)
      (js-indent-level . 2)
      (js2-basic-offset . 2)
      (web-mode-code-indent-offset . 2)
      (web-mode-markup-indent-offset . 2)
      (tide-format-options . '(:indentSize 2 :tabSize 2))
      ;; aligns annotation to the right hand side
      (company-tooltip-align-annotations . t))

    (leaf typescript-mode
      :straight t
      :hook (typescript-mode-hook . setup-tide-mode))

    (leaf web-mode
      :straight t
      :mode ("\\.tsx\\'" . web-mode)
      :after tide
      :hook
      (web-mode-hook . (lambda ()
                         (when (string-equal "tsx" (file-name-extension buffer-file-name))
                           (setup-tide-mode))))
      ;; formats the buffer before saving
      (before-save-hook . tide-format-before-save)
      :config
      ;; enable typescript-tslint checker
      (flycheck-add-mode 'typescript-tslint 'web-mode))

    (leaf js2-mode
      :straight t
      :mode
      ("\\.js"   . js2-mode)
      ("\\.json" . javascript-mode)
      :hook (js2-mode-hook . setup-tide-mode)))

  (leaf scss-mode
    :straight t
    :mode ("\\.\\(scss\\|css\\)\\'" . scss-mode)
    :custom
    (scss-compile-at-save . nil) ;; 自動コンパイルをオフにする
    (css-indent-offset . 2)
    (scss-compile-at-save . nil)
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
      (previous-line)
      (indent-for-tab-command))
    (defun my:semicolon-ret ()
      (interactive)
      (insert ";")
      (newline-and-indent))
    ;; (setq ac-sources '(ac-source-css-property
    ;;                    ac-source-css-property-names
    ;;                    ac-source-yasnippet
    ;;                    ;; ac-source-words-in-same-mode-buffers
    ;;                    ac-source-words-in-all-buffer
    ;;                    ac-source-dictionary))
    (yas-minor-mode))

  (leaf *rust
    :config
    (leaf racer
      :straight t)
    (leaf rust-mode
      :straight t
      :after racer
      :custom
      (racer-cmd . `(expand-file-name "~/.cargo/bin/racer"))
      (racer-rust-src-path . `(expand-file-name "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/"))
      (rust-format-on-save . t)
      :hook
      (rust-mode-hook . (lambda () (racer-mode) (flycheck-mode)))
      (flycheck-mode-hook . flycheck-rust-setup)
      (racer-mode-hook . (lambda () (company-mode) (eldoc-mode)))
      :bind
      (:rust-mode-map
       ("TAB" . company-indent-or-complete-common)
       ("C-c d" . racer-describe))))

  (leaf c/c++
    :config
    (leaf cc-mode
      :straight t
      :hook (c-mode-common-hook . my:cc-mode-setup)
      :config
      (defun my:cc-mode-setup ()
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

  (leaf csharp-mode
    :straight t
    :hook (csharp-mode-hook . my:csharp-mode-setup)
    :config
    (defun my:csharp-mode-setup ()
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
      (setq csharp-want-imenu nil)))

  (leaf sql-mode
    :mode ("\\.ddl$" . sql-mode)
    :custom
    (sql-product . 'postgres)
    :bind
    (:sql-mode-map
     ("C-c \"" . wrap-double-quote-thing-at-symbol)
     ("C-c ," . move-trailing-comma-to-line-start))
    :hook
    (sql-mode-hook . (lambda ()
                       (yas-minor-mode-on)
                       (setq indent-tabs-mode nil)))
    :config
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

  (leaf bat-mode
    :if (eq system-type 'windows-nt)
    :straight t
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

  (leaf lua-mode
    :straight t
    :mode (".nyagos" . lua-mode))

  (leaf visual-basic-mode
    ;; in site-lisp
    :mode ("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . visual-basic-mode)
    :hook (visual-basic-mode-hook . (lambda () (setq mode-name "vb")))
    :config
    (setq visual-basic-mode-indent 4))

  (leaf mayu-mode
    ;; in site-lisp
    :mode ("\\.\\(mayu\\)\\'" . mayu-mode))

  (leaf *calendar
    :config
    (leaf japanese-holidays
      :straight t
      :hook (calendar-mode-hook . init-japanese-holidays)
      :config
      (defun init-japanese-holidays()
	(setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
              (append japanese-holidays holiday-local-holidays holiday-other-holidays))
	(setq mark-holidays-in-calendar t) ; 祝日をカレンダーに表示
	;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
	;; 変数はデフォルトで設定済み
	(setq japanese-holiday-weekend '(0 6)     ; 土日を祝日として表示
              japanese-holiday-weekend-marker     ; 土曜日を水色で表示
              '(holiday nil nil nil nil nil japanese-holiday-saturday))
	(add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
	(add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
	;; “きょう”をマークするには以下の設定を追加します。
	(add-hook 'calendar-today-visible-hook 'calendar-mark-today))))
  ;; end of major-mode
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

  (leaf rainbow-delimiters
    :straight t
    :hook (prog-mode-hook . rainbow-delimiters-mode)
    :config
      (require 'cl-lib)

      (require 'color)
      ;;(global-rainbow-delimiters-mode)
      (cl-loop for index from 1 to rainbow-delimiters-max-face-count
	       do
	       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
		 (cl-callf color-saturate-name (face-foreground face) 30))))

  (leaf flycheck-pos-tip
    :straight t)

  (leaf flycheck
    :straight t
    :commands flycheck-mode
    :after flycheck-pos-tip
    :custom
    (flycheck-disabled-checkers . '(javascript-jshint javascript-jscs))
    (flycheck-display-errors-function . #'flycheck-pos-tip-error-messages)
    ;; (flycheck-add-next-checker 'javascript-jshint
    ;;                            'javascript-gjslint)
    :hydra
    (hydra-flycheck nil
                    "
      Navigate Error^^    Miscellaneous
      ---------------------------------------------------
      [_k_] Prev          [_c_] Clear
      [_j_] Next
      [_f_] First Error   [_q_] Quit
      [_l_] Lask Error
      "
                    ("j" flycheck-next-error)
                    ("k" flycheck-previous-error)
                    ("f" flycheck-first-error)
                    ("l" (progn (goto-char (point-max)) (fiycheck-previous-error)))
                    ("c" flycheck-clear)
                    ("q" nil)))

  (leaf whitespace
    ;;
    ;; whitespace ( http://qiita.com/catatsuy/items/55d50d13ebc965e5f31e )
    ;;
    :straight t
    :config
    (defvar whitespace-style-with-tab '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
    (defvar whitespace-style-without-tab '(face spaces space-mark trailing space-before-tab space-after-tab::space))
    ;; default setting
    (setq whitespace-style whitespace-style-without-tab)
    ;;
    (defun toggle-tab-mark ()
      (interactive)
      (if (equal whitespace-style whitespace-style-with-tab)
	  (setq whitespace-style whitespace-style-without-tab)
	(setq whitespace-style whitespace-style-with-tab)))
    (setq whitespace-space-regexp "\\(\x3000+\\)")
    (setq whitespace-display-mappings
	  '((space-mark ?\x3000 [?\□])
            (tab-mark   ?\t   [?\xBB ?\t])))
    (global-whitespace-mode t)
    (set-face-attribute 'whitespace-trailing nil
			:foreground "DeepPink"
			:underline t)
    (set-face-attribute 'whitespace-tab nil
			:foreground "LightSkyBlue"
			:underline t)
    (set-face-attribute 'whitespace-space nil
			:foreground "GreenYellow"
			:weight 'bold))

  (leaf cua-mode
    :custom
    (cua-enable-cua-keys . nil)
    :config
    (cua-mode t))

  (leaf recentf-ext
    :straight t
    :custom
    (recentf-max-saved-items . 200)
    (recentf-save-file . `(expand-file-name "~/.emacs.d/recentf"))
    (recentf-auto-cleanup . 10)
    :config
    ;; 最近開いたファイルを保存する数を増やす
    (setq recentf-exclude `("r:/.+$"
                            "s:/.+$"
                            "p:/.+$"
                            ,(concat (expand-file-name "~/") ".emacs.d/elpa/.*$")
                            ,(expand-file-name "~/.emacs.d/recentf")))
    ;; from http://qiita.com/itiut@github/items/d917eafd6ab255629346
    (defmacro with-suppressed-message (&rest body)
      "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
      (declare (indent 0))
      (let ((message-log-max nil))
	`(with-temp-message (or (current-message) "") ,@body)))
    (setq recentf-auto-save-timer (run-with-idle-timer 120 t '(lambda () (with-suppressed-message (recentf-save-list)))))
    (recentf-mode 1))

  ;; end of minor-mode
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

(leaf projectile
  :straight t
  :commands projectile-register-project-type projectile-toggle-between-implementation-and-test
  :hook
  (emacs-startup-hook . projectile-mode)
  :bind
  (:projectile-command-map
   ("s" . my:projectile-search-dwim)
   ("<f12>" . projectile-toggle-between-implementation-and-test))
  :custom
  (projectile-enable-idle-timer . nil)
  (projectile-enable-caching . t)
  (projectile-completion-system . 'ivy)
  :preface
  (defun my:projectile-search-dwim (search-term)
    "Merge version to search document via grep/ag/rg.
      Use fast alternative if it exists, fallback grep if no alternatives in system.
      "
    (interactive (list (projectile--read-search-string-with-default
                        "Dwim search for")))
    (cond
     ((and (featurep 'ripgrep) (executable-find "rg")) (projectile-ripgrep search-term))
     ((executable-find "ag") (projectile-ag search-term))
     (t (projectile-grep search-term))))
  ;; :config
  ;; (projectile-register-project-type
  ;;  'yarn
  ;;  '("package.json")
  ;;  :compile "yarn build"
  ;;  :test "yarn test"
  ;;  :run "yarn start"
  ;;  :test-suffix ".test")
  )

(leaf anzu
  :straight t
  :config
  (global-anzu-mode 1))

(leaf open-junk-file
  :straight t
  :bind ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format . "~/Downloads/junk/%Y-%m-%d-%H%M%S."))

(leaf windows-ime
  :if (eq system-type 'windows-nt)
  :after *encoding
  :config

  ;; 日本語入力のための設定
  (set-keyboard-coding-system 'cp932)

  (prefer-coding-system 'utf-8-dos)
  (set-file-name-coding-system 'cp932)

  ;; 標準IMEの設定
  (setq default-input-method "W32-IME")

  ;; IME状態のモードライン表示 (TODO: doom-modeline に細工が必要)
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  ;; IMEの初期化
  (w32-ime-initialize)

  ;; IME OFF時の初期カーソルカラー
  ;; (set-cursor-color "red")
  ;; IME ON/OFF時のカーソルカラー
  ;; (add-hook 'input-method-activate-hook (lambda() (set-cursor-color "green")))
  ;; (add-hook 'input-method-inactivate-hook (lambda() (set-cursor-color "red")))

  ;; バッファ切り替え時にIME状態を引き継ぐ
  (setq w32-ime-buffer-switch-p nil)

  ;; IME on/off key bind
  (global-set-key (kbd "M-`") 'toggle-input-method)

  ;; minibuffer に入った時、IME を OFF にする
  (add-hook 'minibuffer-setup-hook (lambda () (deactivate-input-method)))
  (add-hook 'helm-minibuffer-set-up-hook (lambda () (deactivate-input-method))))

(leaf *frame-setting
  :config
  (leaf frame-setting-mac
    :if (eq system-type 'darwin)
    :config
    (setq initial-frame-alist
	  (append
	   '((ns-transparent-titlebar . t) ;; タイトルバーを透過
             (vertical-scroll-bars . nil) ;; スクロールバーを消す
             ;; (ns-appearance . dark) ;; 26.1 {light, dark}
             (internal-border-width . 0))))
    (setq default-frame-alist initial-frame-alist))
  (leaf frame-setting-common
    :after frame-setting-mac
    :config
    ;; フレームタイトルの設定
    (setq frame-title-format "%b")
    ;; 背景の透明度
    (set-frame-parameter nil 'alpha 90)
    ;; カーソル点滅表示
    (blink-cursor-mode 0)
    ;; メニューバーを消す
    (menu-bar-mode -1)
    ;; ツールバーを消す
    (tool-bar-mode -1)
    ;; scroll bar を表示しない
    (scroll-bar-mode 0)
    ;; スクロール時のカーソル位置の維持
    (setq scroll-preserve-screen-position t)
    ;; スクロール行数（一行ごとのスクロール）
    (setq vertical-centering-font-regexp ".*")
    (setq scroll-conservatively 35)
    (setq scroll-margin 0)
    (setq scroll-step 1)
    ;; 画面スクロール時の重複行数
    (setq next-screen-context-lines 1)
    ;; バッファ中の行番号表示
    (global-linum-mode t)
    ;; 行番号のフォーマット
    ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
    (set-face-attribute 'linum nil :height 0.8)
    (setq linum-format "%5d")))

(leaf buffer
  :config
  ;; バッファ画面外文字の切り詰め表示
  (setq truncate-lines nil)
  ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
  (setq truncate-partial-width-windows t)
  ;; 同一バッファ名にディレクトリ付与
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  ;; バッファの先頭までスクロールアップ
  (defadvice scroll-up (around scroll-up-around)
    (interactive)
    (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
      (goto-char (point-max))
      (let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
        ;;(goto-line start_num )
        (goto-char (point-min))
        (forward-line (1- start_num))
        (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
          (if (< (- (- end_num start_num) (window-height)) 0)
              (goto-char (point-max))
            ad-do-it)) )) )
  (ad-activate 'scroll-up)

  ;; バッファの最後までスクロールダウン
  (defadvice scroll-down (around scroll-down-around)
    (interactive)
    (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
      (if (< start_num (window-height))
          (goto-char (point-min))
        ad-do-it) ))
  (ad-activate 'scroll-down))

(leaf startup
  :config
  ;; 起動メッセージの非表示
  (setq inhibit-startup-message t)
  ;; スタートアップ時のエコー領域メッセージの非表示
  (setq inhibit-startup-echo-area-message -1))

(leaf backup
  :config
  ;; バックアップファイルを作らない
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  ;; 変更ファイルのバックアップ
  (setq make-backup-files nil)
  ;; 変更ファイルの番号つきバックアップ
  (setq version-control nil)
  ;; 編集中ファイルのバックアップ
  (setq auto-save-list-file-name nil)
  (setq auto-save-list-file-prefix nil)
  ;; 編集中ファイルのバックアップ先
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  ;; 編集中ファイルのバックアップ間隔（秒）
  (setq auto-save-timeout 30)
  ;; 編集中ファイルのバックアップ間隔（打鍵）
  (setq auto-save-interval 500)
  ;; 終了時にオートセーブファイルを消す
  (setq delete-auto-save-files t)
  ;; バックアップ世代数
  (setq kept-old-versions 1)
  (setq kept-new-versions 2)
  ;; 上書き時の警告表示
  ;; (setq trim-versions-without-asking nil)
  ;; 古いバックアップファイルの削除
  (setq delete-old-versions t))

(leaf w32-symlinks
  :if (eq system-type 'windoows-nt)
  :config
  (custom-set-variables '(w32-symlinks-handle-shortcuts t))
  (require 'w32-symlinks)

  (defadvice insert-file-contents-literally
      (before insert-file-contents-literally-before activate)
    (set-buffer-multibyte nil))

  (defadvice minibuffer-complete (before expand-symlinks activate)
    (let ((file (expand-file-name
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
      (when (file-symlink-p file)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (w32-symlinks-parse-symlink file))))))

(leaf cygwin
  :if (eq system-type 'windows-nt)
  :config
  (setq cygwin-mount-cygwin-bin-directory (concat (getenv "CYGWIN_DIR") "\\bin"))
  ;; (require 'setup-cygwin)
  ;; (load "config/builtins/setup-cygwin")
  (file-name-shadow-mode -1))

(leaf global-set-keys
  :config
  (mapcar
   '(lambda (l) (global-set-key (first l) (second l)))
   '(("\C-h" delete-backward-char)
     ("\C-z" scroll-down)
     ("\e?" apropos)
     ("\C-x\C-e" compile)
     ("\C-x\C-n" next-error)
     ("\C-x\C-v" find-file-other-window)
     ("\C-x=" count-lines-page)
     ("\C-xn" myblog-hugo/create-draft)
     ("\C-xl" goto-line)
     ("\C-xg" grep)
     ("\C-xt" toggle-truncate-lines)
     ("\e\C-g" keyboard-quit)              ; init.el の設定をもとに戻す
     ("\C-x!" shell-command)
     ("\C-x|" shell-command-on-region)
     ("\eh" backward-kill-word)
     ("%" my-match-paren)
     ))
  (defmacro foo (key fun) `(global-set-key (kbd ,key) (function ,fun)))
  (foo "C-x C-;" my-insert-datetime))

(leaf global-configuration
  :config
  ;; 画像ファイルを表示
  (auto-image-file-mode t)
  ;; evalした結果を全部表示
  (setq eval-expression-print-length nil)
  ;; 括弧
  ;; 対応する括弧を光らせる。
  (show-paren-mode 1)
  ;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
  (setq show-paren-style 'mixed)
  ;; startup message を表示しない
  (setq inhibit-startup-message t)
  ;; 行の先頭でC-kを一回押すだけで行全体を消去する
  (setq kill-whole-line t)
  ;; 最終行に必ず一行挿入する
  ;; (setq require-final-newline t)
  ;; バッファの最後でnewlineで新規行を追加するのを禁止する
  (setq next-line-add-newlines nil)
  ;; 補完時に大文字小文字を区別しない
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  ;; ;; 部分一致の補完機能を使う
  ;; ;; p-bでprint-bufferとか
  ;; ;; 2012-08-08
  ;; ;; Emacs 24ではデフォルトで有効になっていて、`partial-completion-mode'は
  ;; ;; なくなっている。カスタマイズする場合は以下の変数を変更する。
  ;; ;;   * `completion-styles'
  ;; ;;   * `completion-pcm-complete-word-inserts-delimiters'
  ;; (if (fboundp 'partial-completion-mode)
  ;;     (partial-completion-mode t))
  ;; ;; 補完可能なものを随時表示
  ;; ;; 少しうるさい
  ;; (icomplete-mode 1)
  ;; 履歴数を大きくする
  (setq history-length 500)
  ;; ミニバッファの履歴を保存する
  (savehist-mode 1)
  ;; 圧縮
  ;; gzファイルも編集できるようにする
  (auto-compression-mode t)
  ;; diff
  ;; ediffを1ウィンドウで実行
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのオプション
  (setq diff-switches '("-u" "-p" "-N"))
  ;; バッファ名
  ;; ファイル名が重複していたらディレクトリ名を追加する。
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; shebangがあるファイルを保存すると実行権をつける。
  ;; 2012-03-15
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  ;; リージョンの大文字小文字変換を有効にする。
  ;; C-x C-u -> upcase
  ;; C-x C-l -> downcase
  ;; 2011-03-09
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; kill
  ;; 2012-09-01
  ;; Emacs 24からクリップボードだけ使うようになっているので
  ;; Emacs 23のようにprimary selectionを使うように変更
  ;;   * killしたらprimary selectionにだけ入れる（Xの場合のみ）
  ;;   * yankするときはprimary selectionのものを使う
  (setq x-select-enable-primary t)
  (when (eq window-system 'x)
    (setq x-select-enable-clipboard nil))
  ;; M-kanji is undefined に対する対策
  (global-set-key [M-kanji] 'ignore)
  (add-hook 'message-mode-hook (lambda () (yas-minor-mode)))
  ;; lock file を作らない
  (setq create-lockfiles nil)
  ;; ファイル終端の改行文字を自動入力しない
  ;; https://windymelt.hatenablog.com/entry/2014/09/01/145343
  (setq-default require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;;
  (setq indent-tabs-mode nil))
