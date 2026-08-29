;;; -*- lexical-binding: t -*-
(defun vs/read-forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (push form forms))
      (nreverse forms))))
(defvar vs/modules
  '("my-core" "my-japanese" "my-appearance" "my-completion" "my-keybind"
    "my-editor" "my-dired" "my-text" "my-lang-lisp" "my-lang-python"
    "my-lang-web" "my-lang-native" "my-lang-misc" "my-lsp" "my-fileformat"
    "my-project" "my-vc" "my-shell" "my-utils" "my-platform"))
(defun vs/mod (m) (vs/read-forms (expand-file-name (concat m ".el") "user-lisp")))
(defun vs/nomy (forms)
  (seq-remove (lambda (f)
                (and (consp f)
                     (or (eq (car f) 'prepare-user-lisp)
                         (and (memq (car f) '(require provide))
                              (let ((a (cadr f)))
                                (and (consp a) (eq (car a) 'quote)
                                     (string-prefix-p "my-" (symbol-name (cadr a)))))))))
              forms))
(let* ((old (vs/read-forms "tmp/init-before-split.el"))
       (initf (vs/read-forms "init.el"))
       ;; init.el から (provide 'init) を除いた本体
       (head (seq-remove (lambda (f) (equal f '(provide 'init))) (vs/nomy initf)))
       ;; 実際の読み込み順を再現:
       ;;   init.el の 1-88 相当 → my-core → custom.el → 残りモジュール
       ;; head は [1-88 の форм..., custom-file setq, load] の順で並んでいるので
       ;; custom の 2 форм の前に my-core を差し込む
       (ncustom 2)
       (head-main (butlast head ncustom))
       (head-custom (last head ncustom))
       (rest (apply #'append (mapcar #'vs/mod (cdr vs/modules))))
       (combined (append head-main (vs/nomy (vs/mod "my-core")) head-custom
                         (vs/nomy rest) (list '(provide 'init)))))
  (princ (format "old      : %d forms\n" (length old)))
  (princ (format "combined : %d forms\n" (length combined)))
  (if (equal old combined)
      (princ "\nRESULT: IDENTICAL\n")
    (let ((n (max (length old) (length combined))) (d 0))
      (dotimes (i n)
        (unless (equal (nth i old) (nth i combined))
          (setq d (1+ d))
          (when (<= d 3)
            (princ (format "\n#%d\nOLD: %.200S\nNEW: %.200S\n" i (nth i old) (nth i combined))))))
      (princ (format "\nRESULT: DIFFERENT (%d)\n" d)))))
