;; process-coding-system を utf-8 にする。(その他は設定不要？)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; dired hack
(defvar open-directory-command "open" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "open" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external)))

(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)
