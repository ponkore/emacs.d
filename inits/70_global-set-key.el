;;
;; my prefer key binding
;;
(mapcar
 '(lambda (l) (global-set-key (first l) (second l)))
 '(("\C-h" delete-backward-char)
   ("\C-z" scroll-down)
   ("\e?" apropos)
   ("\C-x\C-e" compile)
   ("\C-x\C-n" next-error)
   ("\C-x\C-v" find-file-other-window)
   ("\C-x=" count-lines-page)
   ("\C-xn" myblog-hugo/create-draft)
   ("\C-xl" goto-line)
   ("\C-xg" grep)
   ("\C-xt" toggle-truncate-lines)
   ("\e\C-g" keyboard-quit)              ; init.el の設定をもとに戻す
   ("\C-x!" shell-command)
   ("\C-x|" shell-command-on-region)
   ("\eh" backward-kill-word)
   ("%" my-match-paren)
   ))

(defmacro foo (key fun) `(global-set-key (kbd ,key) (function ,fun)))
(foo "C-x C-;" my-insert-datetime)
