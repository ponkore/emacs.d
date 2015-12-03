;;
;; ~/.emacs.d/init.el
;;

;; only for my office environment
(load (expand-file-name "~/.emacs.d/config-proxy.el") t)
;;

;;
;; initialize Emacs package system.
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; initialize init-loader
;;
(require 'cl)

(defvar installing-package-list
  '(;; ここに使っているパッケージを書く。
    anzu
    cider
    cider-decompile
    cider-spy
    clojure-mode
    clojure-snippets
    clojure-mode-extra-font-locking
    company
    company-quickhelp
    csharp-mode
    dash
    dired-k
    direx
    direx-grep
    elpy
    epl
    expand-region
    flx
    flx-ido
    flycheck
    flycheck-pos-tip
    flycheck-pyflakes
    fsharp-mode
    haml-mode
    helm
    helm-ag
    helm-gtags
    helm-migemo
    helm-package
    helm-projectile
    init-loader
    japanese-holidays
    javap-mode
    js2-mode
    less-css-mode
    lua-mode
    magit
    markdown-mode
    migemo
    open-junk-file
    packed
    pastels-on-dark-theme
    pkg-info
    popup
    popwin
    powerline
    projectile
    psvn
    py-autopep8
    recentf-ext
    sass-mode
    scss-mode
    smartparens
    sql-indent
    sqlup-mode
    ssh
    ssh-config-mode
    yasnippet
    yesql-ghosts
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;; ~/.emacs.d/site-lisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-truncate-lines t t)
 '(safe-local-variable-values (quote ((emacs-lisp-docstring-fill-column . 75))))
 '(w32-symlinks-handle-shortcuts t)
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0")
 '(yas-prompt-functions (quote (my-yas/prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "DodgerBlue1"))))
 '(font-lock-preprocessor-face ((t (:foreground "steel blue"))))
 '(font-lock-variable-name-face ((t (:foreground "DodgerBlue1"))))
 '(hl-line ((t (:underline "textBackgroundColor")))))
(put 'narrow-to-region 'disabled nil)
