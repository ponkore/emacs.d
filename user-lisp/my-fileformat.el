;;; my-fileformat.el --- 特定ファイルフォーマット  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 特定ファイルフォーマット
;;; --------------------------------------------------

;;; [3] yaml

(leaf yaml-mode
  :straight t)

;;; [3] diff

(leaf diff-mode
  :hook
  (diff-mode-hook . my:diff-mode-setup-faces)
  (diff-mode-hook . my:diff-mode-refine-automatically)
  :config
  (defun my:diff-mode-setup-faces ()
    ;; 追加された行は緑で表示
    (set-face-attribute 'diff-added nil :foreground "white" :background "dark green")
    ;; 削除された行は赤で表示
    (set-face-attribute 'diff-removed nil :foreground "white" :background "dark red")
    ;; 文字単位での変更箇所は色を反転して強調
    ;; ;; 2012-04-02 // 2018-05-30 emacs 26.1 でエラーになるのでコメントアウト
    ;; (set-face-attribute 'diff-refine-change nil :foreground nil :background nil :weight 'bold :inverse-video t)
    )
  (defun my:diff-mode-refine-automatically ()
    (diff-auto-refine-mode t)))

;;; [3] log4j

(leaf log4j-mode
  :straight t)

;;; [3] Dockerfile / docker-compose.yml

(leaf dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(leaf docker-compose-mode
  :straight t)

;;; [3] vimrc

(leaf vimrc-mode
  :straight t)

;;; [3] mayu

(leaf mayu-mode
  ;; in site-lisp
  :mode ("\\.\\(mayu\\)\\'" . mayu-mode))

;;; [3] xxx

(provide 'my-fileformat)
;;; my-fileformat.el ends here
