
;;; match-paren
(defun my-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
   (t (self-insert-command (or arg 1)))))

;;; read plain file
(defun read-file-and-list-each-lines (filename)
  (save-excursion
    (let* ((buffer (find-file-noselect filename))
           (ret nil))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^.+$" nil t)
        (setq ret (cons (match-string-no-properties 0) ret)))
      (kill-buffer buffer)
      (reverse ret))))

;; (defun read-file-and-list-each-lines (filename)
;;   (save-excursion
;;     (let* ((buffer (find-file-noselect filename))
;;            (ret nil))
;;       (with-current-buffer buffer
;;         (setq ret (split-string (buffer-string) "[\r\n]")))
;;       (kill-buffer buffer)
;;       ret)))

;;; insert current datetime
(defun my-insert-datetime ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %T")))
