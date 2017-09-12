;;; 文字列
;;; 2012-03-24

;; 文字列の先頭と最後にある一連の空白をすべて削除する
;; 2012-03-24
(defun string-strip (string)
  (replace-regexp-in-string "\\`[ \r\n]*\\|[ \r\n]*\\'" "" string))


;;; パス
;;; 2011-03-19

;; directoryの中にbase-names内のパスが含まれていたらその絶対パスを返す。
;; 含まれていなかったらdirectoryの親のディレクトリを再帰的に探す。
;; 2011-03-19
(defun find-path-in-parents (directory base-names)
  (or (find-if 'file-exists-p
               (mapcar (lambda (base-name)
                         (concat directory base-name))
                       base-names))
      (if (string= directory "/")
          nil
        (let ((parent-directory (substring directory 0 -1)))
          (find-path-in-parents parent-directory base-names)))))

;;
;; http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
;;
(defun my-goto-line-beginning-or-indent (&optional $position)
  (interactive)
  (or $position (setq $position (point)))
  (let (($starting-position (progn (back-to-indentation) (point))))
    (if (eq $starting-position $position)
        (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

;;
;;
;;
(defun wrap-double-quote-thing-at-symbol ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds))
         (str (thing-at-point 'symbol))
         (wrapped (format "\"%s\"" str)))
    (delete-region start end)
    (insert wrapped)
    (goto-char (+ 2 end))))

;;
;;
;;
(defun move-trailing-comma-to-line-start ()
  (interactive)
  (let* ((eol (save-excursion (end-of-line) (point)))
         (pt (re-search-forward ",[ \t]*$" eol t)))
    (when pt
      (goto-char (- pt 1))
      (delete-char 1)
      (forward-line)
      (let* ((eol (save-excursion (end-of-line) (point)))
             (pt (re-search-forward "^[ \t]*--" eol t)))
        (when pt (forward-line)))
      (let* ((eol (save-excursion (end-of-line) (point))))
        (when (= eol pt) (forward-line)))
      (insert "  ,")
      (just-one-space)
      )
    ))

;;
;; see http://d.hatena.ne.jp/kitokitoki/20100225/p1
;;
(defun my-pp-macroexpand-last-sexp ()
  (interactive)
  (if (thing-at-point-looking-at "\(")
      (save-excursion
        (forward-list)
        (pp-macroexpand-last-sexp nil))
    (pp-macroexpand-last-sexp nil)))

(add-hook 'lisp-interaction-mode-hook
          (lambda() (define-key lisp-interaction-mode-map (kbd "C-c RET") 'my-pp-macroexpand-last-sexp)))
(add-hook 'emacs-lisp-mode-hook
          (lambda() (define-key emacs-lisp-mode-map (kbd "C-c RET") 'my-pp-macroexpand-last-sexp)))

;;
;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
;;
(defun delete-file-if-no-contents ()
    (let ((file (buffer-file-name (current-buffer))))
      (when (= (point-min) (point-max))
        (delete-file file)
        (message (concat "File: " file " deleted.")))))
(add-hook 'after-save-hook 'delete-file-if-no-contents)
