;;; Magit
;;; 2012-03-23

(setq magit-last-seen-setup-instructions "1.4.0")

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

;; diff関連の設定
;; 2012-04-02
(defun magit-setup-diff ()
  ;; diffを表示しているときに文字単位での変更箇所も強調表示する
  ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
  ;; 2012-04-02
  (setq magit-diff-refine-hunk 'all)
  ;; diff用のfaceを設定する
  ;; 2012-04-02
  (diff-mode-setup-faces))

(add-hook 'magit-mode-hook 'magit-setup-diff)
