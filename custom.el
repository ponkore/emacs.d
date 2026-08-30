;;; custom.el --- customize が生成する設定 -*- lexical-binding: t -*-
;;
;;
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rst-level-1 ((t (:foreground "gray10" :background "gray80" :height 1.4 :weight bold))))
 '(rst-level-2 ((t (:foreground "gray10" :background "gray80" :height 1.2 :weight bold))))
 '(rst-level-3 ((t (:background "grey15" :height 1.1))))
 '(rst-level-4 ((t (:background "grey15" :height 1.1))))
 '(rst-level-5 ((t (:background "grey15" :height 1.1))))
 '(rst-level-6 ((t (:background "grey15" :height 1.1))))
)
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
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0))
     (cider-default-cljs-repl . shadow)
     (clojurescript-mode . cljstyle-format-on-save)
     (clojure-mode . cljstyle-format-on-save)
     (cider-shadow-cljs-default-options . "app")
     (php-project-root . auto)
     (phpstan-executable . "phpstan")
     (phpstan-level . 7)
     (phpstan-config-file root . ".phpstan/phpstan-custom.neon")
     (phpstan-working-dir root . ".phpstan")
     (phpstan-executable . "C:/Apps/phpstan/vendor/bin/phpstan.bat")
     (php-project-root . git)
     (phpstan-memory-limit . "1G")
     (phpstan-executable quote docker)
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (checkdoc-package-keywords-flag)
     (cider-ns-refresh-after-fn . "reloaded.repl/resume")
     (cider-ns-refresh-before-fn . "reloaded.repl/suspend")
     (cider-default-cljs-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (emacs-lisp-docstring-fill-column . 75)))
 '(warning-suppress-log-types '((straight)))
 '(warning-suppress-types '((straight)))
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0"))
