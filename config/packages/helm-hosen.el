;;
;; helm-source for developing
;;

(require 'helm)

(defun helm-hosen-action (msg)
  (save-excursion
    (with-current-buffer (get-buffer-create "*hosen*")
      (erase-buffer)
      (insert msg)
      (message (format "%s" msg)))))

(defun make-helm-source-from-file (source-name filename)
  (when (file-exists-p filename)
    (let ((l (mapcar (lambda (v) (list v)) (read-file-and-list-each-lines filename))))
      `((name . ,source-name)
        (candidates . ,l)
        (action . helm-hosen-action)))))

(defvar helm-tools-dir (expand-file-name "~/.emacs.d/hosen-tools"))

(defvar helm-root-user-zai-source nil)
(defvar helm-root-user-kan-source nil)
(defvar helm-senmei-source nil)
(defvar helm-eki-zai-source nil)
(defvar helm-kanrishitsu-eki-zai-source nil)
(defvar helm-senmei-eki-zai-source nil)
(defvar helm-kanrishitsu-kyokai-zai-source nil)

(defun helm-hosen-config ()
  (interactive)
  (progn
    (setq helm-root-user-zai-source
          (make-helm-source-from-file "rootユーザ(在)" (concat helm-tools-dir "/helm-root-user-zai.txt")))
    (setq helm-root-user-kan-source
          (make-helm-source-from-file "rootユーザ(幹)" (concat helm-tools-dir "/helm-root-user-kan.txt")))
    (setq helm-senmei-source
          (make-helm-source-from-file "線名(在)" (concat helm-tools-dir "/helm-線名.txt")))
    (setq helm-eki-zai-source
          (make-helm-source-from-file "駅(在)" (concat helm-tools-dir "/helm-駅.txt")))
    (setq helm-kanrishitsu-eki-zai-source
          (make-helm-source-from-file "管理室＿駅(在)" (concat helm-tools-dir "/helm-管理室駅.txt")))
    (setq helm-senmei-eki-zai-source
          (make-helm-source-from-file "線名＿駅(在)" (concat helm-tools-dir "/helm-線名-駅.txt")))
    (setq helm-kanrishitsu-kyokai-zai-source
          (make-helm-source-from-file "管理室境界(在)" (concat helm-tools-dir "/helm-管理室境界.txt")))))

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
     helm-kanrishitsu-kyokai-zai-source
     helm-kanrishitsu-eki-zai-source
     helm-senmei-eki-zai-source)
   "*helm zai*"))

(helm-hosen-config)

(provide 'helm-hosen)
