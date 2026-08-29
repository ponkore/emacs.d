;;; my-lang-misc.el --- その他の言語 (SQL / bat / Swift / Lua / VB)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] SQL

(leaf sql
  ;; leaf 名は "sql-mode" だったが、それは feature 名ではない
  ;; (ライブラリは sql.el)。leaf は :bind / :config を
  ;; (eval-after-load '<leaf名>) で遅延させるため、存在しない feature を
  ;; 待ち続けて :config も :bind も永久に適用されていなかった。
  ;; その結果 oracle-settings 等が未定義で、C-c " / C-c , も未バインドだった。
  ;; 実在する feature 名 sql に直すことで正しく遅延適用される。
  :mode ("\\.ddl$" . sql-mode)
  :custom
  (sql-product . 'postgres)
  :bind
  (:sql-mode-map
   ("C-c \"" . wrap-double-quote-thing-at-symbol)
   ("C-c ," . move-trailing-comma-to-line-start))
  :hook
  (sql-mode-hook . (lambda ()
                     (yas-minor-mode-on)
                     (setq indent-tabs-mode nil)))
  :config
  (defun oracle-settings ()
    "setup oracle sql environment"
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
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (setq comint-output-filter-functions 'comint-truncate-buffer)
                (toggle-truncate-lines t)
                (when (eq system-type 'windows-nt)
                  (set-buffer-process-coding-system 'cp932 'cp932))
                (comint-send-string (get-buffer-process (current-buffer)) "
ALTER SESSION SET NLS_DATE_FORMAT='YYYY/MM/DD'
/
set linesize 1000
set trimspool on
set timing on
set pagesize 1000
")))
    ;; only for my office environment
    (load (expand-file-name "config-sqlplus.el" user-emacs-directory) t)
    ;; customize font-lock
    (font-lock-add-keywords 'sql-mode
                            '(("\"\\([^\"]*\\)\"" . 'font-lock-constant-face)
                              ("\\<Hgs\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)
                              ("\\<R[LSC][0-9][A-Z]\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face))))
  (defun wrap-double-quote-thing-at-symbol ()
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (str (thing-at-point 'symbol))
           (wrapped (format "\"%s\"" str)))
      (delete-region start end)
      (insert wrapped)
      (goto-char (+ 2 end))))

  (defun move-trailing-comma-to-line-start ()
    (interactive)
    (let* ((eol (save-excursion (end-of-line) (point)))
           (pt (re-search-forward ",[ \t]*$" eol t)))
      (when pt
        (goto-char (- pt 1))
        (delete-char 1)
        (forward-line)
        (let* ((eol (save-excursion (end-of-line) (point)))
               (pt (re-search-forward "^[ \t]*--" eol t)))
          (when pt (forward-line)))
        (let* ((eol (save-excursion (end-of-line) (point))))
          (when (= eol pt) (forward-line)))
        (insert "  ,")
        (just-one-space)))))

;;; [3] bat

(leaf bat-mode
  :if (eq system-type 'windows-nt)
  :config
  (setq bat-font-lock-keywords
        (eval-when-compile
          (let ((COMMANDS
                 '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
                   "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
                   "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
                   "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
                   "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
                   "ver" "vol" "xcopy"))
                (CONTROLFLOW
                 '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
                   "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
                (UNIX
                 '("bash" "cat" "cp" "fgrep" "grep" "ls" "sed" "sh" "mv" "rm")))
            `(("\\_<\\(call\\|goto\\)\\_>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
               (2 font-lock-constant-face t))
              ("^:[^:].*"
               . 'bat-label-face)
              ("\\_<\\(defined\\|set\\)\\_>[ \t]*\\(\\w+\\)"
               (2 font-lock-variable-name-face))
              ("%\\([A-Za-z0-9_]+\\)%?"
               (1 font-lock-variable-name-face))
              ("!\\([A-Za-z0-9_]+\\)!?"        ; delayed-expansion !variable!
               (1 font-lock-variable-name-face))
              ("[ =][-/]+\\([A-Za-z0-9_]\\)"
               (1 font-lock-type-face append))
              (,(concat "\\_<" (regexp-opt COMMANDS) "\\_>") . font-lock-builtin-face)
              (,(concat "\\_<" (regexp-opt CONTROLFLOW) "\\_>") . font-lock-keyword-face)
              (,(concat "\\_<" (regexp-opt UNIX) "\\_>") . font-lock-warning-face))))))

;;; [3] swift

(leaf swift-mode
  :straight t
  :hook (swift-mode-hook . (lambda () (lsp))))

(leaf lsp-sourcekit
  ;; sourcekit-lsp は macOS (Xcode) 専用
  :if (eq system-type 'darwin)
  :straight t
  :after lsp-mode
  :config
  (let ((sourcekit (or (executable-find "sourcekit-lsp")
                       "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
    (setq lsp-sourcekit-executable sourcekit)))

;;; [3] lua

(leaf lua-mode
  :straight t
  :mode (".nyagos" . lua-mode))

;;; [3] VisualBasic

(leaf visual-basic-mode
  ;; in site-lisp
  :mode ("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . visual-basic-mode)
  :hook (visual-basic-mode-hook . (lambda () (setq mode-name "vb")))
  :config
  (setq visual-basic-mode-indent 4))

(provide 'my-lang-misc)
;;; my-lang-misc.el ends here
