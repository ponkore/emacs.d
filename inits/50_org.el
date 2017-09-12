(use-package org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq org-directory "~/Downloads/junk")
  (setq org-agenda-files (list org-directory))
  (setq org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))))
