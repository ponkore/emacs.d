;; helm-source for developing
(defun read-file-and-list-each-lines (filename)
  (save-excursion
    (let* ((buffer (find-file-noselect filename))
           (ret nil))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^.+$" nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0)))
          (setq ret (append ret (list (list (buffer-substring beg end)))))))
      (kill-buffer buffer)
      ret)))

(defun make-helm-source-from-file (filename source-name)
  (let ((l (read-file-and-list-each-lines filename)))
    `((name . ,source-name)
      (candidates . ,l)
      (action . message))))

(defvar helm-root-user-zai-source
  (make-helm-source-from-file "C:/repo/hosen-tools/helm-root-user-zai.txt"
                              "rootユーザ(在)"))

(defvar helm-root-user-kan-source
  (make-helm-source-from-file "C:/repo/hosen-tools/helm-root-user-kan.txt"
                              "rootユーザ(幹)"))

(defvar helm-senmei-source
  (make-helm-source-from-file "C:/repo/hosen-tools/helm-線名.txt"
                              "線名(在)"))

(defvar helm-eki-zai-source
  (make-helm-source-from-file "C:/repo/hosen-tools/helm-駅.txt"
                              "駅(在)"))

(defvar helm-kanrishitsu-eki-zai-source
  (make-helm-source-from-file "C:/repo/hosen-tools/helm-管理室駅.txt"
                              "管理室＿駅(在)"))

(defun helm-root-user ()
  (interactive)
  (helm-other-buffer
   '(helm-root-user-zai-source helm-root-user-kan-source) "*helm root user*"))

(defun helm-hosen-zai ()
  (interactive)
  (helm-other-buffer
   '(helm-senmei-source
     helm-eki-zai-source
     helm-kanrishitsu-eki-zai-source)
   "*helm zai*"))
