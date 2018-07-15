(defvar myblog-hugo/base-directory-format-string "~/blog/myblog-hugo/content/post/%Y-%m/%d/"
  "format string for post directory. use this with `format-time-string'")

(defvar myblog-hugo/draft-directory "~/blog/drafts/"
  "draft directory for myblog-hugo. draft file for markdown, thumbnails")

(defvar myblog-hugo/draft-template "+++
shortname = \"\"
title = \"\"
description = \"\"
date = \"%Y-%m-%dT%H:%M:%S+09:00\"
categories = [\"Programming\"]
tags = [\"\"]
archives = [\"%Y-%m\"]
url = \"post/%Y-%m/%d/{{shortname}}\"
thumbnail = \"/img/%Y-%m/%d/{{shortname}}.png\"
+++

<!--more-->
"
  "template string for post's default markdown text. use this with `format-time-string', and replace {{post-title}}.")

(defun myblog-hugo/create-draft ()
  "create a hugo draft file with default template."
  (interactive)
  (let* ((draft-filename (format-time-string "%Y-%m-%d-%H%M%S.md" (current-time)))
         (filename (concat myblog-hugo/draft-directory draft-filename))
         (directory (file-name-directory filename))
         (draft-content myblog-hugo/draft-template)
         (buf (set-buffer (find-file-noselect filename t))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert draft-content)
      ;; (basic-save-buffer)
      (switch-to-buffer buf)
      (goto-char (point-max)))))

(defun myblog-hugo/get-shortname ()
  "frontmatter にある shortname を取得する"
  (goto-char (point-min))
  (when (re-search-forward "shortname* = *\"\\(.*\\)\"" nil t)
    (let* ((matched (match-string-no-properties 1)))
      matched)))

(defun myblog-hugo/apply-current-time (field-name end)
  "frontmatter にある keyword = format の format に現在時刻を適用する。"
  (let* ((left-part (concat "\\(" field-name " *= *\\[?"))
         (right-part (concat "\"" "\\)" "\\(.+\\)" "\\(\"\\]?\\)")))
    (when (re-search-forward (concat left-part right-part) end t)
      (let* ((matched (match-string-no-properties 2))
             (formatted (format-time-string matched (current-time))))
        (replace-match (concat "\\1" formatted "\\3"))
        (goto-char (point-min))))))

(defun myblog-hugo/apply-shortname (shortname end)
  "frontmatter に含まれる {{shortname}} を置き換える"
  (goto-char (point-min))
  (while (re-search-forward "{{shortname}}" end t)
    (replace-match shortname)))

(defun myblog-hugo/publish ()
  "publish current draft buffer to hugo post directory."
  (interactive)
  (let* ((end)
         (post-destdir (format-time-string myblog-hugo/base-directory-format-string (current-time)))
         (shortname (downcase (myblog-hugo/get-shortname)))
         (destfile (concat post-destdir "/" shortname ".md")))
    (goto-char (point-min))
    (re-search-forward "\\+\\+\\+")
    (forward-char)
    (re-search-forward "\\+\\+\\+")
    (forward-char -3)
    (setq end (point))
    ;;
    (goto-char (point-min))
    (myblog-hugo/apply-current-time "date" end)
    (myblog-hugo/apply-current-time "archives" end)
    (myblog-hugo/apply-current-time "url" end)
    (myblog-hugo/apply-current-time "thumbnail" end)
    (myblog-hugo/apply-shortname shortname end)
    ;;
    (unless (file-exists-p post-destdir)
      (make-directory post-destdir t))
    (set-visited-file-name destfile)
    (basic-save-buffer)))
