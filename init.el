;;
;; ~/.emacs.d/init.el
;;
(when (eq window-system 'w32)
  (setq w32-get-true-file-attributes nil)
  ;; for my home's win10 environment.
  (setenv "HOME" (getenv "USERPROFILE")))

;; only for my office environment
(load (expand-file-name "~/.emacs.d/config-proxy.el") t)
;;

;;
;; initialize Emacs package system.
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; try straight
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)
;; (use-package init-loader)

;;
;; initialize init-loader
;;
(require 'cl)

;; Avoid to write `package-selected-packages` in init.el
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(let ((not-installed (loop for x in package-selected-packages
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

(load-theme 'pastels-on-dark t)
(enable-theme 'pastels-on-dark)

(init-loader-load "~/.emacs.d/inits")

(load (expand-file-name "~/.emacs.d/use-packages.el") t)
