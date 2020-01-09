;; process-coding-system を utf-8 にする。(その他は設定不要？)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(set-frame-parameter nil 'alpha 90)

(setq initial-frame-alist
      (append
       '((ns-transparent-titlebar . t) ;; タイトルバーを透過
         (vertical-scroll-bars . nil) ;; スクロールバーを消す
         ;; (ns-appearance . dark) ;; 26.1 {light, dark}
         (internal-border-width . 0))))

(setq default-frame-alist initial-frame-alist)
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
