;;; my-fileformat.el --- 特定ファイルフォーマット  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 特定ファイルフォーマット
;;; --------------------------------------------------

;;; [3] yaml

(use-package yaml-mode
  :straight t
  :defer t)

;;; [3] diff

(use-package diff-mode
  :hook
  (diff-mode-hook . my:diff-mode-setup-faces)
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
  ;; my:diff-mode-refine-automatically (diff-auto-refine-mode を呼ぶだけの関数)
  ;; は削除した。diff-auto-refine-mode は Emacs 27.1 で obsolete になり、
  ;; 「set `diff-refine' instead」と言われる。その diff-refine は既定値が
  ;; font-lock で、文字単位の強調は最初から有効。つまりこの関数は不要だった。
  )

;;; [3] log4j

(use-package log4j-mode
  :straight t
  :defer t)

;;; [3] Dockerfile / docker-compose.yml

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :straight t
  :defer t)

;;; [3] vimrc

(use-package vimrc-mode
  :straight t
  :defer t)

;;; [3] mayu

(use-package mayu-mode
  ;; in site-lisp
  :mode ("\\.\\(mayu\\)\\'" . mayu-mode))

;;; [3] xxx

(provide 'my-fileformat)
;;; my-fileformat.el ends here
