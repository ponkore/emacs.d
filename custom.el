;;
;;
;;
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
;;
;;
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-truncate-lines t t)
 '(package-selected-packages
   (quote
    (use-package groovy-mode graphviz-dot-mode company-racer racer yesql-ghosts yaml-mode swiper ssh sqlup-mode sql-indent smartparens smart-tabs-mode scss-mode sass-mode recentf-ext rainbow-delimiters py-autopep8 psvn projectile-codesearch powerline popwin pastels-on-dark-theme paredit packed ox-gfm ox-asciidoc open-junk-file markdown-mode magit lua-mode less-css-mode js2-mode japanese-holidays init-loader helm-projectile helm-package helm-migemo helm-gtags helm-ag fsharp-mode flycheck-rust flycheck-pyflakes flycheck-pos-tip flx-ido expand-region evalator-clojure elscreen-buffer-group elpy dired-k csharp-mode clojure-mode-extra-font-locking cider-spy cider-decompile cargo auto-complete anzu adoc-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (emacs-lisp-docstring-fill-column . 75))))
 '(w32-symlinks-handle-shortcuts t)
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0")
 '(yas-prompt-functions (quote (my-yas/prompt))))

