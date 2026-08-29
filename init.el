;;; init.el --- Emacs 初期化ファイル -*- lexical-binding: t -*-
;;; Commentary:
;; 設定本体は my-config/init-main.el にある。
;; 以前は my-config/init.org を org-babel-load-file で展開していたが、
;; Org-mode の恩恵が薄い割にコストが大きいため素の Emacs Lisp に戻した。
;;; Code:

(defvar my-config-dir (expand-file-name "my-config/" user-emacs-directory)
  "個人設定ファイルを置くディレクトリ.")

(load (expand-file-name "init-main" my-config-dir) nil 'nomessage)

;;; init.el ends here
