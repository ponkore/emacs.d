(require 'org)

(setq org-src-fontify-natively t)
(setq org-directory "~/Downloads/junk")
(setq org-agenda-files (list org-directory))

(require 'open-junk-file)
(setq open-junk-file-format "~/Downloads/junk/%Y-%m-%d-%H%M%S.")

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
