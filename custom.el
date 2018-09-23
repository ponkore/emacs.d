;;
;;
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
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
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-truncate-lines t t)
 '(package-selected-packages
   (quote
    (slime slime-company vue-mode htmlize smarty-mode company-php php-mode meghanada flycheck-joker web-mode tide typescript company-jedi jedi swift-mode rg go-eldoc go-mode go-projectile use-package graphviz-dot-mode company-racer racer yesql-ghosts yaml-mode swiper ssh sqlup-mode sql-indent smartparens smart-tabs-mode scss-mode sass-mode recentf-ext rainbow-delimiters py-autopep8 psvn projectile-codesearch powerline popwin pastels-on-dark-theme paredit packed ox-gfm ox-asciidoc open-junk-file markdown-mode magit lua-mode less-css-mode js2-mode japanese-holidays init-loader helm-projectile helm-migemo helm-gtags helm-ag fsharp-mode flycheck-rust flycheck-pyflakes flycheck-pos-tip flx-ido expand-region evalator-clojure elscreen-buffer-group elpy dired-k csharp-mode clojure-mode-extra-font-locking cider-spy cider-decompile cargo auto-complete anzu adoc-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-default-cljs-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
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

