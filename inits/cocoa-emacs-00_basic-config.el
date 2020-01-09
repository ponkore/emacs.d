;; process-coding-system を utf-8 にする。(その他は設定不要？)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; dired hack
(defvar open-directory-command "open" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "open" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external)))

(set-frame-parameter nil 'alpha 90)

(setq initial-frame-alist
      (append
       '((ns-transparent-titlebar . t) ;; タイトルバーを透過
         (vertical-scroll-bars . nil) ;; スクロールバーを消す
         ;; (ns-appearance . dark) ;; 26.1 {light, dark}
         (internal-border-width . 0))))

(setq default-frame-alist initial-frame-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fsharp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; F# 編集用のモード

(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/bin/fsharpc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; exec path setting ( http://qiita.com/catatsuy/items/3dda714f4c60c435bb25 )
;;
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "/bin/bash --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
