;;; Helm
;; iswitchbの代わり
(let ((original-browse-url-browser-function browse-url-browser-function))
  (require 'helm)
  (require 'helm-config)
;   (anything-set-anything-command-map-prefix-key 'anything-command-map-prefix-key "C-c C-<SPC>")
  (global-set-key (kbd "C-c h") 'helm-mini)
  (helm-mode)
  (define-key global-map (kbd "C-x b") 'helm-for-files)
  (define-key global-map (kbd "C-x C-b") 'helm-for-files)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-map (kbd "C-z") 'helm-previous-page)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-o") nil)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (setq browse-url-browser-function original-browse-url-browser-function))

(setq helm-for-files-preferred-list
      '(helm-c-source-buffers-list
        helm-c-source-bookmarks         ; bookmark の順位を上げた
        helm-c-source-recentf
        helm-c-source-file-cache
        helm-c-source-files-in-current-dir
        helm-c-source-locate))

(defun helm-action-copy-selected (msg)
  (save-excursion
    (with-temp-buffer
      (erase-buffer)
      (insert msg)
      (let ((start (goto-char (point-min)))
            (end (goto-char (point-max))))
        (copy-region-as-kill start end))
      (message (format "%s" msg)))))

(defun make-helm-source-from-file (source-name filename execute-action)
  (when (file-exists-p filename)
    (let ((l (mapcar (lambda (v) (list v)) (read-file-and-list-each-lines filename))))
      `((name . ,source-name)
        (candidates . ,l)
        (action . execute-action)))))
