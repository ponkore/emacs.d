;;; extract.el --- init.org の emacs-lisp ブロックを 1 本の .el に抽出 -*- lexical-binding: t -*-
;; 使い方: emacs -Q --batch -l tmp/extract.el
;; 組み込み org のみを使い、ユーザー設定は読まない。

(require 'org)
(require 'org-element)
(require 'subr-x)

(defvar ex/src "my-config/init.org")
(defvar ex/dst "my-config/init-main.el")

(defun ex/dedent (s)
  "S の全行から共通の先頭インデントを取り除く（org-babel-tangle 相当）。"
  (let* ((lines (split-string s "\n"))
         (min-indent
          (apply #'min 9999
                 (delq nil
                       (mapcar (lambda (l)
                                 (unless (string-empty-p (string-trim l))
                                   (string-match "\\`[ \t]*" l)
                                   (length (match-string 0 l))))
                               lines)))))
    (if (or (zerop min-indent) (= min-indent 9999))
        s
      (mapconcat (lambda (l)
                   (if (<= (length l) min-indent)
                       (string-trim-right l)
                     (substring l min-indent)))
                 lines "\n"))))

(defun ex/headline-path (el)
  "EL を含む見出しのパスを (レベル . 見出し文字列) のリストで返す。"
  (let (path (p (org-element-property :parent el)))
    (while p
      (when (eq (org-element-type p) 'headline)
        (push (cons (org-element-property :level p)
                    (substring-no-properties
                     (org-element-property :raw-value p)))
              path))
      (setq p (org-element-property :parent p)))
    path))

(let* ((default-directory (expand-file-name "."))
       (buf (find-file-noselect ex/src))
       (blocks 0)
       (last-path nil)
       (out (generate-new-buffer "*out*")))
  (with-current-buffer buf
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (sb)
        (when (equal (org-element-property :language sb) "emacs-lisp")
          (let* ((body (ex/dedent (org-element-property :value sb)))
                 (path (ex/headline-path sb)))
            ;; 見出しが変わったら見出しコメントを出す
            (unless (equal path last-path)
              (with-current-buffer out
                (dolist (h path)
                  ;; 直前と共通の親見出しは再出力しない
                  (unless (member h last-path)
                    (let ((lv (car h)) (title (cdr h)))
                      (insert "\n")
                      (cond
                       ((= lv 1) (insert ";;; ==================================================\n"
                                         ";;; " title "\n"
                                         ";;; ==================================================\n"))
                       ((= lv 2) (insert ";;; --------------------------------------------------\n"
                                         ";;; " title "\n"
                                         ";;; --------------------------------------------------\n"))
                       (t (insert ";;; [" (number-to-string lv) "] " title "\n")))))))
              (setq last-path path))
            (when (string-empty-p (string-trim body))
              (setq body nil))
            (when body
              (setq blocks (1+ blocks))
              (with-current-buffer out
                (insert "\n" (string-trim-right body) "\n"))))))))
  (with-current-buffer out
    (goto-char (point-min))
    (insert ";;; init-main.el --- Emacs 個人設定本体 -*- lexical-binding: nil -*-\n"
            ";;; Commentary:\n"
            ";; my-config/init.org から機械的に抽出したもの（等価変換）。\n"
            ";; 抽出元のリビジョン: refactor/de-org-tangle ブランチ時点\n"
            ";;; Code:\n")
    (write-region (point-min) (point-max) ex/dst))
  (message "extracted %d blocks -> %s" blocks ex/dst))

;;; extract.el ends here
