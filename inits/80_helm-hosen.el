;;
;; helm-source for developing
;;

(defvar helm-hosen-tools-dir (expand-file-name "~/.emacs.d/hosen-tools"))

(defun make-helm-hosen-source-from-file (source-name filename)
  (make-helm-source-from-file source-name (concat helm-hosen-tools-dir "/" filename) 'helm-action-copy-selected))

(defvar helm-hosen-initialized nil)

(defvar helm-root-user-zai-source          nil)
(defvar helm-root-user-kan-source          nil)
(defvar helm-senmei-source                 nil)
(defvar helm-eki-zai-source                nil)
(defvar helm-kanrishitsu-eki-zai-source    nil)
(defvar helm-senmei-eki-zai-source         nil)
(defvar helm-kanrishitsu-kyokai-zai-source nil)
(defvar helm-ronbutsu-henkan-source        nil)

(defun helm-hosen-initialize
  ()
  (unless helm-hosen-initialized
    (setq helm-root-user-zai-source          (make-helm-hosen-source-from-file "rootユーザ(在)" "helm-root-user-zai.txt"))
    (setq helm-root-user-kan-source          (make-helm-hosen-source-from-file "rootユーザ(幹)" "helm-root-user-kan.txt"))
    (setq helm-senmei-source                 (make-helm-hosen-source-from-file "線名(在)"       "helm-線名.txt"))
    (setq helm-eki-zai-source                (make-helm-hosen-source-from-file "駅(在)"         "helm-駅.txt"))
    (setq helm-kanrishitsu-eki-zai-source    (make-helm-hosen-source-from-file "管理室＿駅(在)" "helm-管理室駅.txt"))
    (setq helm-senmei-eki-zai-source         (make-helm-hosen-source-from-file "線名＿駅(在)"   "helm-線名-駅.txt"))
    (setq helm-kanrishitsu-kyokai-zai-source (make-helm-hosen-source-from-file "管理室境界(在)" "helm-管理室境界.txt"))
    (setq helm-ronbutsu-henkan-source        (make-helm-hosen-source-from-file "論物変換"       "helm-dict.txt"))
    (setq helm-hosen-initialized t)))

(defun helm-root-user ()
  (interactive)
  (helm-hosen-initialize)
  (helm-other-buffer
   '(helm-root-user-zai-source helm-root-user-kan-source)
   "*helm root user*"))

(defun helm-hosen-zai ()
  (interactive)
  (helm-hosen-initialize)
  (helm-other-buffer
   '(helm-senmei-source
     helm-eki-zai-source
     helm-kanrishitsu-kyokai-zai-source
     helm-kanrishitsu-eki-zai-source
     helm-senmei-eki-zai-source)
   "*helm zai*"))

(defun helm-ronbutsu-henkan ()
  (interactive)
  (helm-hosen-initialize)
  (helm-other-buffer
   '(helm-ronbutsu-henkan-source)
   "*helm ronbutsu henkan*"))

(provide 'helm-hosen)
