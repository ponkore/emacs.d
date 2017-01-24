;;
;; helm-source for developing
;;

(defvar helm-hosen-tools-dir (expand-file-name "~/.emacs.d/hosen-tools"))

(defun make-helm-hosen-source-from-file (source-name filename)
  (helm-build-in-file-source source-name (concat helm-hosen-tools-dir "/" filename)
                             :action 'helm-action-copy-selected))

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

;;
;; table info selection
;;

(defvar hgs-tables-line-re "^\\([^\s]+\\)\s+")
(defvar hgs-tables-base-dir "C:/repo/hgs/Tables")

(defun helm-table-select-action (zaikan msg)
  (if (string-match hgs-tables-line-re msg)
      (let* ((table-name (substring msg (match-beginning 1) (match-end 1)))
             (file-path (concat hgs-tables-base-dir "/" zaikan "/" table-name ".table.txt")))
        (if (file-exists-p file-path)
            (find-file-other-window file-path)
          (message (concat file-path " not found."))))
    (message msg)))

(add-hook
 'text-mode-hook
 (lambda ()
   ;; .table.txt の場合は truncate-line する
   (when (and (buffer-file-name)
              (string= (substring-no-properties (buffer-file-name) -10) ".table.txt"))
     (toggle-truncate-lines 1)
     (setq buffer-read-only t))))

(defun helm-tables ()
  (interactive)
  (helm
   :sources (list (helm-build-in-file-source "テーブル(在)"
                    (concat helm-hosen-tools-dir "/" "helm-tables-zai.txt")
                    :action (lambda (msg) (helm-table-select-action "zai" msg)))
                  (helm-build-in-file-source "テーブル(幹)"
                    (concat helm-hosen-tools-dir "/" "helm-tables-kan.txt")
                    :action (lambda (msg) (helm-table-select-action "kan" msg))))
   :buffer "*helm tables*"))

(defun helm-table-alias ()
  (interactive)
  (helm
   :sources (list (helm-build-in-file-source "テーブル別名"
                    (concat helm-hosen-tools-dir "/" "SX080_010_テーブル別名.txt")))
   :buffer "*helm table alias*"))

(defun helm-hanyo-kubun ()
  (interactive)
  (helm
   :sources (list (helm-build-in-file-source "汎用区分"
                    (concat helm-hosen-tools-dir "/" "helm-hanyo-kubun.txt")))
   :buffer "*helm hanyo kubun*"))

(defun helm-ronbutsu-henkan ()
  (interactive)
  (helm
   :sources (list (helm-build-in-file-source "論物変換"
                    (concat helm-hosen-tools-dir "/" "helm-dict.txt")))
   :buffer "*helm ronbutsu henkan*"))

(defun helm-hosen-function-list ()
  (interactive)
  (helm
   :sources (list (helm-build-in-file-source "画面一覧"
                    (concat helm-hosen-tools-dir "/" "B010_180_画面一覧(工事).txt")))))

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
  (helm
   :sources (list (helm-build-in-file-source "保線業務管理システム"
                    (concat helm-hosen-tools-dir "/" "helm-hosen-app-url.txt")
                    :action 'invoke-hgs-app))
   :buffer "*helm hgs app"))

(provide 'helm-hosen)
