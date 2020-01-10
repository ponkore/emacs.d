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
 '(mode-line ((t (:background "violet red" :foreground "black" :box -1))))
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
 '(cider-auto-select-error-buffer t t)
 '(cider-repl-result-prefix ";; => " t)
 '(cider-show-error-buffer t t)
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
 '(css-indent-offset 2 t)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(dired-recursive-copies (quote always))
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project))
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(explicit-shell-file-name "bash.exe")
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-truncate-lines t t)
 '(inferior-lisp-program "ros run" t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-extra-directories (quote ("../" "./")))
 '(ivy-format-functions-alist (quote ((t . ivy-format-function-arrow))))
 '(ivy-height 20)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(js-indent-level 2 t)
 '(js2-basic-offset 2 t)
 '(line-number-mode 0)
 '(nrepl-hide-special-buffers t t)
 '(nrepl-sync-request-timeout 40 t)
 '(open-junk-file-format "~/Downloads/junk/%Y-%m-%d-%H%M%S." t)
 '(org-adapt-indentation nil)
 '(org-bullets-bullet-list (quote ("" "" "" "" "" "" "")))
 '(org-clock-clocked-in-display (quote none))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-download-image-dir "./img" t)
 '(org-indent-indentation-per-level 0 t)
 '(org-log-done t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path (quote file))
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-startup-truncated t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))))
 '(package-selected-packages
   (quote
    (init-loader leaf leaf-keywords pastels-on-dark-theme)))
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching t)
 '(projectile-enable-idle-timer nil)
 '(recentf-max-saved-items 200)
 '(recentf-save-file "c:/Users/fj2260ec/.emacs.d/recentf")
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
 '(scss-compile-at-save nil t)
 '(shell-command-switch "-c" t)
 '(shell-file-name "bash.exe")
 '(sql-product (quote postgres) t)
 '(swiper-include-line-number-in-search t)
 '(tide-format-options (quote (:indentSize 2 :tabSize 2)) t)
 '(typescript-indent-level 2 t)
 '(w32-symlinks-handle-shortcuts t)
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-markup-indent-offset 2 t)
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0")
 '(yas-prompt-functions (quote (my-yas/prompt))))
