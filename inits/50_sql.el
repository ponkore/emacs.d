(use-package sql-mode
  :defer t
  :mode (("\\.ddl$" . sql-mode))
  :init
  ;; for SQL mode (My Office PC Oracle setting)
  (when (eq system-type 'windows-nt)
    (setq sql-oracle-program "c:/Apps/Oracle/sqlplus.exe")
    ;; 新規作成のときだけ cp932 にする
    (add-hook 'sql-mode-hook (lambda ()
                               (unless (file-exists-p (buffer-file-name (current-buffer)))
                                 (set-buffer-file-coding-system 'cp932)
                                 (set-buffer-modified-p nil)))))

  ;; on Mac, set environment variables
  (when (or (eq system-type 'berkeley-unix) (eq system-type 'darwin))
    (let ((oracle-home (expand-file-name "~/Applications/Oracle/instantclient_10_2")))
      (setenv "NLS_LANG" "JAPANESE_JAPAN.UTF8")
      (setenv "DYLD_LIBRARY_PATH" oracle-home)
      (setenv "LD_LIBRARY_PATH" oracle-home)
      (setq sql-oracle-program (concat oracle-home "/sqlplus"))))

  ;; set Oracle as default SQL product.
  (setq sql-product 'oracle)

  ;; does not work below.
  ;; (when (eq system-type 'windows-nt)
  ;;   (add-to-list 'process-coding-system-alist '("sqlplus" cp932 . cp932)))
  (defun sql-set-sqli-oracle-hook ()
    ""
    (setq comint-output-filter-functions 'comint-truncate-buffer)
    (toggle-truncate-lines t)
    (when (eq system-type 'windows-nt) (set-buffer-process-coding-system 'cp932 'cp932))
    (comint-send-string (get-buffer-process (current-buffer)) "
ALTER SESSION SET NLS_DATE_FORMAT='YYYY/MM/DD'
/
set linesize 1000
set trimspool on
set timing on
set pagesize 1000
")
    )
  (require 'yasnippet)
  (add-hook 'sql-interactive-mode-hook 'sql-set-sqli-oracle-hook)
  (add-hook 'sql-mode-hook (lambda ()
                             (yas-minor-mode-on)
                             (setq indent-tabs-mode nil)
                             (define-key sql-mode-map (kbd "C-c \"") 'wrap-double-quote-thing-at-symbol)
                             (define-key sql-mode-map (kbd "C-c ,") 'move-trailing-comma-to-line-start)))

  ;; only for my office environment
  (load (expand-file-name "~/.emacs.d/config-sqlplus.el") t)

  ;; customize font-lock
  (font-lock-add-keywords 'sql-mode '(("\"\\([^\"]*\\)\"" . 'font-lock-constant-face)
                                      ("\\<Hgs\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)
                                      ("\\<R[LSC][0-9][A-Z]\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)))
  )
