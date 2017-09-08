(use-package org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq org-directory "~/Downloads/junk")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))))

  ;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
(defun delete-file-if-no-contents ()
    (let ((file (buffer-file-name (current-buffer))))
      (when (= (point-min) (point-max))
        (delete-file file)
        (message (concat "File: " file " deleted.")))))
(add-hook 'after-save-hook 'delete-file-if-no-contents)

(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/Downloads/junk/%Y-%m-%d-%H%M%S."))
