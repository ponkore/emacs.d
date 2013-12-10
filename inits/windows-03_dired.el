;; Windows dired quick hack: open any documents with external command.
(defvar open-directory-command "cygstart.exe" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "cygstart.exe" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))
