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
    ac-js2
    ac-nrepl
    anzu
    auto-compile
    cider
    cider-decompile
    cider-tracing
    clojure-mode
    clojure-test-mode
    clojure-snippets
    csharp-mode
    dash
    dired-details
    epl
    expand-region
    fsharp-mode
    git-commit-mode
    git-rebase-mode
    haml-mode
    helm
    helm-ag
    helm-gtags
    helm-package
    init-loader
    japanese-holidays
    javap-mode
    js2-mode
    magit
    magit-commit-training-wheels
    magit-log-edit
    magit-push-remote
    markdown-mode
    midje-mode
    midje-test-mode
    packed
    page-break-lines
    paredit
    pkg-info
    popup
    popwin
    psvn
    sass-mode
    scss-mode
    sql-indent
    ssh
    ssh-config-mode
    yasnippet
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
