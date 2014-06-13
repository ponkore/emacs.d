;;; Helm
;;; (some settings derived from http://d.hatena.ne.jp/a_bicky/20140104/1388822688 )
(let ((original-browse-url-browser-function browse-url-browser-function))
  (require 'helm)
  (require 'helm-config)
;   (anything-set-anything-command-map-prefix-key 'anything-command-map-prefix-key "C-c C-<SPC>")
  (global-set-key (kbd "C-c h") 'helm-mini)
  (helm-mode)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)
;  (define-key global-map (kbd "C-x b") 'helm-for-files)
  (define-key global-map (kbd "C-x C-b") 'helm-for-files)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-z") 'helm-previous-page)
  (define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-o") nil)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (setq browse-url-browser-function original-browse-url-browser-function))

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

;; Disable helm in some functions
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

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
        (action . ,execute-action)))))
