;; IME on/off key bind
(global-set-key (kbd "M-`") 'toggle-input-method)

;; minibuffer に入った時、IME を OFF にする
(add-hook 'minibuffer-setup-hook
          (lambda () (deactivate-input-method)))
(add-hook 'helm-minibuffer-set-up-hook
          (lambda () (deactivate-input-method)))

;;
;; dired
;;
;; Windows dired quick hack: open any documents with external command.
(defvar open-directory-command "start" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "start" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external)))

;;
;; vc
;;
;; svn log の出力は cp932
(add-hook 'vc-svn-log-view-mode-hook (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))

;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)

(defadvice vc-svn-command (around vc-svn-coding-system-setup compile)
  (let ((old-default-process-coding-system default-process-coding-system))
    (setq default-process-coding-system '(utf-8 . cp932))
    ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))

(ad-activate-regexp "vc-svn-coding-system-setup")

;;
;; mayu
;;
(require 'mayu-mode)
(add-to-list 'auto-mode-alist '("\\.\\(mayu\\)\\'" . mayu-mode))

;;
;; clojure
;;
;; on Windows, use lein.bat instead of lein shell script.
(setq cider-lein-command "lein.bat")

;;
;; F# 編集用のモード
;;
(setq inferior-fsharp-program "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsi.exe\"")
(setq fsharp-compiler "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsc.exe\"")

(defun inferior-fsharp-mode-hook-fn ()
  ""
  ;; (setq comint-output-filter-functions 'comint-truncate-buffer)
  (set-buffer-process-coding-system 'cp932 'cp932))

(add-hook 'inferior-fsharp-mode-hooks 'inferior-fsharp-mode-hook-fn)

;;
;; sql
;;
;;(modify-coding-system-alist 'file ".*\\.sql$" 'cp932-dos)
