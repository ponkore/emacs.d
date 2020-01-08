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
 '(hl-line ((t (:underline "textBackgroundColor"))))
 '(rst-level-1-face ((t (:foreground "LightSkyBlue"))) t)
 '(rst-level-2-face ((t (:foreground "LightGoldenrod"))) t)
 '(rst-level-3-face ((t (:foreground "Cyan1"))) t)
 '(rst-level-4-face ((t (:foreground "chocolate1"))) t)
 '(rst-level-5-face ((t (:foreground "PaleGreen"))) t)
 '(rst-level-6-face ((t (:foreground "Aquamarine"))) t))
 ;; (rst-level-7-face ((t (:foreground "LightSteelBlue"))) t)  ;; メモ
 ;; (rst-level-8-face ((t (:foreground "LightSalmon"))) t)
(put 'narrow-to-region 'disabled nil)
;;
;;
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-scale-factor 1.0)
 '(column-number-mode 0)
 '(company-begin-commands (quote (self-insert-command)))
 '(company-box-doc-enable t)
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(company-box-max-candidates 50)
 '(company-box-show-single-candidate t)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-foreground "black")
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(counsel-find-file-ignore-regexp nil)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project))
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-truncate-lines t t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-extra-directories (quote ("../" "./")))
 '(ivy-format-functions-alist (quote ((t . ivy-format-function-arrow))))
 '(ivy-height 20)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(line-number-mode 0)
 '(package-selected-packages
   (quote
    (diminish leaf-keywords leaf doom-modeline company-box smex ivy-hydra which-key-posframe all-the-icons all-the-icons-dired all-the-icons-ivy cider tide company-quickhelp magit inf-clojure let-alist editorconfig dart-mode avy symbol-overlay counsel counsel-projectile ivy-yasnippet kotlin-mode arduino-mode company-arduino log4j-mode git-gutter-fringe git-gutter-fringe+ slime slime-company vue-mode htmlize smarty-mode company-php meghanada flycheck-joker web-mode typescript company-jedi jedi swift-mode rg go-eldoc go-projectile use-package graphviz-dot-mode company-racer racer yaml-mode ssh sqlup-mode sql-indent smartparens smart-tabs-mode scss-mode sass-mode recentf-ext rainbow-delimiters py-autopep8 psvn projectile-codesearch powerline popwin pastels-on-dark-theme paredit packed ox-gfm ox-asciidoc open-junk-file markdown-mode lua-mode less-css-mode js2-mode japanese-holidays init-loader flycheck-rust flycheck-pyflakes flycheck-pos-tip flx-ido expand-region elscreen-buffer-group elpy dired-k csharp-mode clojure-mode-extra-font-locking auto-complete anzu adoc-mode)))
 '(projectile-completion-system (quote ivy) t)
 '(projectile-enable-caching t t)
 '(projectile-enable-idle-timer nil t)
 '(regexp-opt nil t)
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (cider-ns-refresh-after-fn . "reloaded.repl/resume")
     (cider-ns-refresh-before-fn . "reloaded.repl/suspend")
     (cider-default-cljs-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (emacs-lisp-docstring-fill-column . 75))))
 '(swiper-include-line-number-in-search t)
 '(w32-symlinks-handle-shortcuts t)
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0")
 '(yas-prompt-functions (quote (my-yas/prompt))))
