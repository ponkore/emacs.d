;;
;; helm-source for developing
;;

(defvar helm-hosen-tools-dir (expand-file-name "~/.emacs.d/hosen-tools"))

(defun make-helm-hosen-source-from-file (source-name filename)
  (make-helm-source-from-file source-name (concat helm-hosen-tools-dir "/" filename) 'helm-action-copy-selected))

(defun helm-root-user ()
  (interactive)
  (helm-other-buffer
   (list (make-helm-hosen-source-from-file "rootユーザ(在)" "helm-root-user-zai.txt")
         (make-helm-hosen-source-from-file "rootユーザ(幹)" "helm-root-user-kan.txt"))
   "*helm root user*"))

(defun helm-hosen-zai ()
  (interactive)
  (helm-other-buffer
   (list (make-helm-hosen-source-from-file "線名(在)"       "helm-線名.txt")
         (make-helm-hosen-source-from-file "駅(在)"         "helm-駅.txt")
         (make-helm-hosen-source-from-file "管理室境界(在)" "helm-管理室境界.txt")
         (make-helm-hosen-source-from-file "管理室＿駅(在)" "helm-管理室駅.txt")
         (make-helm-hosen-source-from-file "線名＿駅(在)"   "helm-線名-駅.txt"))
   "*helm zai*"))

(defun helm-ronbutsu-henkan ()
  (interactive)
  (helm-other-buffer
   (list (make-helm-hosen-source-from-file "論物変換" "helm-dict.txt"))
   "*helm ronbutsu henkan*"))

;;
;; start system
;;

(defvar internet-explore-program "C:/Program Files/Internet Explorer/iexplore.exe")
(defvar hgs-app-line-re "^\\([^\s]+\\)\s+\\([^\s]+\\)\s+\\([^\s]+\\)$")

(defun invoke-hgs-app (msg)
  (if (string-match hgs-app-line-re msg)
      (let* ((m1 (substring msg (match-beginning 1) (match-end 1)))
             (m2 (substring msg (match-beginning 2) (match-end 2)))
             (url (substring msg (match-beginning 3) (match-end 3))))
        (start-process "Internet Explore" nil internet-explore-program url)
        (message (concat "running " url)))
    (message msg)))

(defun helm-hgs-app ()
  (interactive)
  (helm-other-buffer
   (list (make-helm-source-from-file "保線業務管理システム" (concat helm-hosen-tools-dir "/" "helm-hosen-app-url.txt")
                                     'invoke-hgs-app))
   "*helm hgs app"))

(provide 'helm-hosen)
