;;; my-lang-web.el --- Web 系 (PHP / JavaScript / TypeScript)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; [3] PHP

(use-package php-mode
  :mode ("\\.\\(cgi\\|phpm\\|inc\\)\\'" . php-mode)
  :straight t
  :custom
  ;; ac-php-debug-flag は ac-php 用。ac-php はもう使っていないので削除した。
  (php-manual-url 'ja)
  (php-mode-coding-style 'psr2)
  :hook
  (php-mode-hook . (lambda ()
                     ;; php-mode 1.28 (2026-08) で cc-mode 依存が外れ、
                     ;; php-base-mode 由来の独自インデントになった。
                     ;; (c-set-style "bsd") と c-basic-offset はエラーになり、
                     ;; そこで php-mode-hook が中断していた
                     ;; (Buffer index.php is not a CC Mode buffer)。
                     ;; インデントは php-mode-coding-style で指定する。
                     (subword-mode 1)
                     (setq indent-tabs-mode t)
                     (setq tab-width 4)
                     (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")
                     ;; (ac-php-core-eldoc-setup)
                     ;; (add-to-list 'company-backends 'company-ac-php-backend)
                     ;; (make-local-variable 'company-backends)
                     ;; intelephense の files.associations。lsp-mode の
                     ;; lsp-intelephense-files-associations に相当する。
                     ;; eglot はサーバ設定を eglot-workspace-configuration で渡す。
                     (setq-local eglot-workspace-configuration
                                 '(:intelephense
                                   (:files (:associations ["*.php" "*.phpm" "*.inc"]))))))
  ;; 診断は eglot が flymake 経由で出すので flycheck の設定は要らなくなった。
  (php-mode-hook . eglot-ensure)
  :bind
  (:map php-mode-map
   (";" . self-insert-command)
   ("{" . self-insert-command)
   ;; ("[" . #'(smartchr "[]" "array()" "[[]]"))
   ;; ("]" . #'(smartchr "array " "]" "]]"))
   ;; ("C-}" . cedit-barf)
   ;; ("C-)" . cedit-slurp)
   ;; ("M-." . ac-php-find-symbol-at-point)
   ;; ("M-," . ac-php-location-stack-back)
   ("C-c C--" . php-current-class)
   ("C-c C-=" . php-current-namespace)))

;; flycheck-phpstan は導入するだけで有効化されておらず (require は
;; コメントアウトされていた)、実際には一度も動いていなかったため削除した。
;; phpstan を使いたくなったら flymake-phpstan を入れる。

;;; [3] JavaScript / TypeScript

;;
;; javascript / typescript
;;
(use-package add-node-modules-path
  :straight t
  :commands add-node-modules-path)

(use-package prettier-js
  :straight t
  ;; use-package は遅延キーワードが無いと (require) を出すので :defer t が要る
  :defer t
  :diminish
  ;; :commands prettier-js-mode
  ;; :custom
  ;; (prettier-js-args . ("--print-width" "120"
  ;;                      "--single-quote" "true"
  ;;                      "--trailing-comma" "none"
  ;;                      "--tab-width" "2"))
  )

;;; [4] TypeScript / JavaScript

;; tide は廃止した。tsserver とのやりとりを tide が独自に持つ必要はもう無く、
;; 型情報・補完・診断は eglot (typescript-language-server) から得られる。
;; tide は flycheck と密結合しており、flymake への移行の妨げにもなっていた。
;;
;; メジャーモードは tree-sitter 版 (typescript-ts-mode / tsx-ts-mode /
;; js-ts-mode) を使う。ただし文法が入っていない環境では *-ts-mode は
;; そもそも起動できないため、従来のモードを残したうえで
;; my:treesit-remap で差し替える形にしてある。

(use-package flymake-eslint
  ;; 旧 (flycheck-add-mode 'javascript-eslint 'web-mode) の置き換え。
  ;; eslint はプロジェクトの node_modules/.bin にあるので、
  ;; add-node-modules-path のあとに有効化する。
  :straight t
  :commands flymake-eslint-enable
  :custom
  (flymake-eslint-defer-binary-check t))

(defun my:web-lang-setup ()
  "JS / TS 系メジャーモード共通のセットアップ。"
  (add-node-modules-path)
  (setq-local tab-width 2)
  (eglot-ensure)
  (when (executable-find "eslint")
    (flymake-eslint-enable))
  (prettier-js-mode))

(use-package js
  ;; 組み込みの js-mode。以前は js2-mode を使っていたが、js-mode 側が
  ;; 十分に育っており、tree-sitter 版 (js-ts-mode) への差し替えもしやすい。
  :mode (("\\.\\(js\\|cjs\\|mjs\\)\\'" . js-mode)
         ("\\.json\\'" . js-json-mode))
  :custom
  (js-indent-level 2)
  :hook ((js-mode-hook js-ts-mode-hook) . my:web-lang-setup))

(use-package typescript-mode
  ;; 文法が入っていない環境向けのフォールバック。文法があれば
  ;; typescript-ts-mode (組み込み) に差し替わる。
  :straight t
  :mode ("\\.\\(ts\\|mts\\|cts\\)\\'" . typescript-mode)
  :custom
  (typescript-indent-level 2)
  :hook ((typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook)
         . my:web-lang-setup))

;; use-package の :config は (eval-after-load '<パッケージ名> ...) に包まれるので、
;; そこでメジャーモードを差し替えても「その回に開いたバッファ」には
;; 間に合わない。そもそも差し替えが効くと従来のモードはロードされなく
;; なるため、差し替えとインデント設定はトップレベルで済ませる。
(setq typescript-ts-mode-indent-offset 2)
(my:treesit-remap 'js-mode 'js-ts-mode 'javascript)
(my:treesit-remap 'js-json-mode 'json-ts-mode 'json)
(my:treesit-remap 'typescript-mode 'typescript-ts-mode 'typescript)

(use-package web-mode
  :straight t
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.njk\\'" . web-mode))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :hook
  (web-mode-hook . (lambda ()
                     (setq tab-width 4)
                     (indent-tabs-mode 0)
                     (whitespace-mode))))

;; .tsx は文法があれば tsx-ts-mode、無ければ web-mode で開く。
;; web-mode の :mode が auto-mode-alist の先頭に .tsx を積むので、
;; それより後に登録する必要がある。
(when (my:treesit-available-p 'tsx)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package scss-mode
  :straight t
  :mode ("\\.\\(scss\\|css\\)\\'" . scss-mode)
  :custom
  (scss-compile-at-save nil) ;; 自動コンパイルをオフにする
  (css-indent-offset 2)
  (scss-compile-at-save nil)
  ;; (yas-minor-mode) は :config トップレベルにあり、scss-mode のロード時点の
  ;; カレントバッファに対して実行されてしまっていた。:hook へ移動する。
  :hook (scss-mode-hook . yas-minor-mode)
  :bind
  (:map scss-mode-map
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
    (forward-line -1)
    (indent-for-tab-command))
  (defun my:semicolon-ret ()
    (interactive)
    (insert ";")
    (newline-and-indent)))

(provide 'my-lang-web)
;;; my-lang-web.el ends here
