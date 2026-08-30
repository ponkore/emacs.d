;;; verify.el --- 抽出結果と旧タングル出力の等価性を S 式で検証 -*- lexical-binding: t -*-
;; 使い方: emacs -Q --batch -l tmp/verify.el

(defun vf/read-forms (file)
  "FILE の全トップレベル S 式をリストで返す。"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (condition-case nil
                            (read (current-buffer))
                          (end-of-file nil)))
        (push form forms))
      (nreverse forms))))

(let* ((old (vf/read-forms "my-config/init.el"))       ; org-babel-tangle 出力
       (new (vf/read-forms "my-config/init-main.el"))) ; 今回の抽出結果
  (princ (format "old (tangled) forms : %d\n" (length old)))
  (princ (format "new (extracted) forms: %d\n" (length new)))
  ;; new の先頭には provide が無い分の差など無いはずだが、念のため要素ごとに比較
  (if (equal old new)
      (princ "\nRESULT: IDENTICAL  (全トップレベル S 式が完全一致)\n")
    (progn
      (princ "\nRESULT: DIFFERENT\n")
      (let ((n (max (length old) (length new)))
            (diff 0))
        (dotimes (i n)
          (let ((o (nth i old)) (w (nth i new)))
            (unless (equal o w)
              (setq diff (1+ diff))
              (when (<= diff 10)
                (princ (format "\n--- form #%d differs ---\nOLD: %.400S\nNEW: %.400S\n"
                               i o w))))))
        (princ (format "\ntotal differing forms: %d\n" diff))))))

;;; verify.el ends here
