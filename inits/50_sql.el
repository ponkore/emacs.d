;;; for SQL mode (My Office PC Oracle setting)
(when (eq system-type 'windows-nt)
  (setq sql-oracle-program "c:/Apps/Oracle/sqlplus.exe"))

(when (or (eq system-type 'berkeley-unix) (eq system-type 'darwin))
  (let ((oracle-home (expand-file-name "~/Applications/Oracle/instantclient_10_2")))
    (setenv "NLS_LANG" "JAPANESE_JAPAN.UTF8")
    (setenv "DYLD_LIBRARY_PATH" oracle-home)
    (setenv "LD_LIBRARY_PATH" oracle-home)
    (setq sql-oracle-program (concat oracle-home "/sqlplus"))))

;;; set Oracle as default SQL product.
(setq sql-product 'oracle)
(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))

;; does not work below.
;; (when (eq system-type 'windows-nt)
;;   (add-to-list 'process-coding-system-alist '("sqlplus" cp932 . cp932)))
(defun sql-set-sqli-oracle-hook ()
  ""
  (setq comint-output-filter-functions 'comint-truncate-buffer)
  (when (eq system-type 'windows-nt) (set-buffer-process-coding-system 'cp932 'cp932)))

(add-hook 'sql-interactive-mode-hook 'sql-set-sqli-oracle-hook)
(add-hook 'sql-mode-hook (lambda () (yas-minor-mode-on)))
