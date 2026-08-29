;;; my-keybind.el --- グローバルキーバインド  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; 基本キーバインド
;;; --------------------------------------------------

;;; [3] global-set-keys

(leaf global-set-keys
  :bind
  ("C-h" . delete-backward-char)
  ("C-z" . scroll-down)
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
  ("M-`" . ignore)
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
