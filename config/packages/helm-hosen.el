;;
;; helm-source for developing
;;
(defun make-helm-source-from-file (source-name filename)
  (let ((l (mapcar (lambda (v) (list v)) (read-file-and-list-each-lines filename))))
    `((name . ,source-name)
      (candidates . ,l)
      (action . message))))

(defvar helm-hosen-configured nil)
(defvar helm-root-user-zai-source nil)
(defvar helm-root-user-kan-source nil)
(defvar helm-senmei-source nil)
(defvar helm-eki-zai-source nil)
(defvar helm-kanrishitsu-eki-zai-source nil)

(defun helm-hosen-config ()
  (interactive)
  (unless helm-hosen-configured
    (setq helm-root-user-zai-source
          (make-helm-source-from-file "rootユーザ(在)"
                                      "C:/repo/hosen-tools/helm-root-user-zai.txt"))
    (setq helm-root-user-kan-source
          (make-helm-source-from-file "rootユーザ(幹)"
                              "C:/repo/hosen-tools/helm-root-user-kan.txt"))
    (setq helm-senmei-source
          (make-helm-source-from-file "線名(在)"
                                      "C:/repo/hosen-tools/helm-線名.txt"))
    (setq helm-eki-zai-source
          (make-helm-source-from-file "駅(在)"
                                      "C:/repo/hosen-tools/helm-駅.txt"))
    (setq helm-kanrishitsu-eki-zai-source
          (make-helm-source-from-file "管理室＿駅(在)"
                              "C:/repo/hosen-tools/helm-管理室駅.txt"))
    (setq helm-hosen-configured t)))

(defun helm-root-user ()
  (interactive)
  (helm-other-buffer
   '(helm-root-user-zai-source helm-root-user-kan-source)
   "*helm root user*"))

(defun helm-hosen-zai ()
  (interactive)
  (helm-other-buffer
   '(helm-senmei-source
     helm-eki-zai-source
     helm-kanrishitsu-eki-zai-source)
   "*helm zai*"))

(helm-hosen-config)
