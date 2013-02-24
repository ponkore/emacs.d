;;; el-get
;; 複数のソースからパッケージをインストールできるパッケージ管理システム
;; 2012-03-15
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  ;;; @ponkore, force master branch.
  (setq el-get-install-branch "master")
  ;;;
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))
;; レシピ置き場
(add-to-list 'el-get-recipe-path
             (concat (file-name-directory load-file-name) "/el-get/recipes"))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path
             "~/.emacs.d/config/el-get/local-recipes")


;;; grep-edit
;; *grep*で編集できるようにする
(el-get 'sync '(grep-edit))
(add-hook 'grep-setup-hook
          (lambda ()
            (define-key grep-mode-map
              (kbd "C-c C-c") 'grep-edit-finish-edit)))


;;; ポップアップ
;; 2012-03-16
(el-get 'sync '(popup))


;;; Auto Complete
;; 自動補完
(el-get 'sync '(auto-complete))
(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)))


;;; Helm
;; iswitchbの代わり
(let ((original-browse-url-browser-function browse-url-browser-function))
  (el-get 'sync '(helm))
  (require 'helm-config)
;   (anything-set-anything-command-map-prefix-key 'anything-command-map-prefix-key "C-c C-<SPC>")
  (global-set-key (kbd "C-c h") 'helm-mini)
  (helm-mode)
  (define-key global-map (kbd "C-x b") 'helm-for-files)
  (define-key global-map (kbd "C-x C-b") 'helm-for-files)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-map (kbd "C-z") 'helm-previous-page)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-o") nil)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (setq browse-url-browser-function original-browse-url-browser-function))


;;; Migemo
;; ローマ字で日本語をインクリメンタルサーチする。
;; 2012-03-19
;; インストールされていたら有効にする。
;(require 'migemo nil t)


;;; rabbit-mode
;; 2012-03-16
;(el-get 'sync '(rabbit-mode))


;;; run-test
;; テスト実行
(el-get 'sync '(run-test))


;;; Magit
;; 2012-03-24
(load "config/packages/magit")


;;; rst-mode
;; reStructuredText編集用のモード
;; 2012-03-24
(el-get 'sync '(rst-mode))


;;; textile-mode
;; Textile編集用のモード
;; 2012-04-11
(el-get 'sync '(textile-mode))


;;; js2-mode
;; 2012-04-05
(load "config/packages/js2-mode")


;;; coffe-mode
;; CoffeeScript編集用のモード
;; 2012-04-04
(el-get 'sync '(coffee-mode))


;;; less-css-mode
;; LESS編集用のモード
;; 2012-09-23
(el-get 'sync '(less-css-mode))


;;; Milkode
;; ソースコード検索エンジン
;; 2012-09-28
(el-get 'sync '(milkode))


;;; markdown-mode
;; Markdown編集用のモード
;; 2012-10-15
(el-get 'sync '(markdown-mode))
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\.txt\\'" . markdown-mode))


;;; 追加の設定
;; 個別の設定があったら読み込む
;; 2012-03-15
(load "config/packages/local" t)
