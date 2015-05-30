;;; Magit
;;; 2012-03-23

(eval-after-load "magit"
  '(progn
     (defun magit-expand-git-file-name--msys (args)
       "Handle Msys directory names such as /c/* by changing them to C:/*"
       (let ((filename (car args)))
         (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
           (setq filename (concat (match-string 1 filename) ":/"
                                  (match-string 2 filename))))
         (list filename)))
     (advice-add 'magit-expand-git-file-name :filter-args #'magit-expand-git-file-name--msys)))

;; コミットメッセージをhelmで挿入できるようにする
(defvar helm-c-source-git-commit-messages
  '((name . "Git Commit Messages")
    (candidates . helm-c-git-commit-messages-candidates)
    (action . (("Insert" . (lambda (str) (insert str)))))
;    (migemo)
    (multiline))
  "Source for browsing and inserting commit messages.")

(defun helm-c-git-commit-messages-candidates ()
  (let* ((messages-string
          (shell-command-to-string "\\git \\log -50 --format=\"%x00%B\""))
         (raw-messages (string-to-list (split-string messages-string "\0")))
         (messages (mapcar (lambda (raw-message)
                             (string-strip raw-message))
                           raw-messages)))
    (remove-if (lambda (message)
                 (string-equal message ""))
               messages)))

(defun helm-git-commit-messages ()
  "`helm' for git commit messages."
  (interactive)
  (helm-other-buffer 'helm-c-source-git-commit-messages
                     "*helm commit messages*"))

;; (defun magit-enable-helm ()
;;   ;; 過去のコミットメッセージを挿入
;;   (define-key magit-log-edit-mode-map (kbd "C-c i") 'helm-git-commit-messages))

;; (add-hook 'magit-mode-hook 'magit-enable-helm)

;; diff関連の設定
;; 2012-04-02
(defun magit-setup-diff ()
  ;; diffを表示しているときに文字単位での変更箇所も強調表示する
  ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
  ;; 2012-04-02
  (setq magit-diff-refine-hunk 'all)
  ;; diff用のfaceを設定する
  ;; 2012-04-02
  (diff-mode-setup-faces)
  ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
  ;; 2012-04-02
  (set-face-attribute 'magit-item-highlight nil :inherit nil))

(add-hook 'magit-mode-hook 'magit-setup-diff)
