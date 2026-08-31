;;; my-keybind.el --- グローバルキーバインド  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 基本キーバインド
;;; --------------------------------------------------

;;; [3] global-set-keys

;; 疑似パッケージ名は use-package では emacs を使う。
;; 実在しない feature 名にすると :config が with-eval-after-load に包まれて
;; 永久に実行されない (leaf の :leaf-defer nil と同じ罠)。
(use-package emacs
  :bind
  (("C-h" . delete-backward-char)
   ;; scroll-error-top-bottom (my-editor.el) は scroll-up-command /
   ;; scroll-down-command にしか効かないので、生の scroll-down ではなく
   ;; コマンド版に割り当てる。
   ("C-z" . scroll-down-command)
   ("ESC ?" . apropos)
   ("C-x C-e" . compile)
   ("C-x C-n" . next-error)
   ("C-x C-v" . find-file-other-window)
   ("C-x n" . myblog-hugo/create-draft)
   ;; ("C-x l" . goto-line)
   ("C-x =" . my:count-lines-buffer)
   ("C-x g" . grep)
   ("C-x t" . toggle-truncate-lines)
   ("ESC C-g" . keyboard-quit)
   ("C-x !" . shell-command)
   ("C-x |" . shell-command-on-region)
   ("ESC h" . backward-kill-word)
   ("%" . my:match-paren)
   ("C-x C-;" . my:insert-datetime)
   ("C-x C-M-r" . revert-buffer)
   ([M-kanji] . ignore)  ;; M-kanji is undefined に対する対策
   ("M-`" . ignore))
  :init
  (defun my:count-lines-buffer ()
    (interactive)
    (message (format "lines: %d" (count-lines (point-min) (point-max)))))
  (defun my:match-paren (arg)
    "Go to the matching parenthesis if on parenthesis otherwise insert %."
    (interactive "p")
    (cond
     ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
     ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
     (t (self-insert-command (or arg 1)))))
  (defun my:insert-datetime ()
    (interactive)
    (insert (format-time-string "%Y/%m/%d %T"))))

(provide 'my-keybind)
;;; my-keybind.el ends here
