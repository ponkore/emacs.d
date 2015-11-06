;;; grep
;; 再帰的にgrep
;; 2011-02-18
(require 'grep)

(if (eq window-system 'w32)
    (setq grep-command-before-query "yagrep -nH -r -e ")
  (setq grep-command-before-query "grep -nH -r -e "))

(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;; (defadvice grep (around grep-coding-system-setup compile)
;;   "When a prefix argument given, specify coding-system-for-read."
;;   (let ((coding-system-for-read
;;          (if current-prefix-arg
;;              (read-coding-system "coding system: ")
;;            coding-system-for-read)))
;;     ad-do-it))

;; (defadvice grep (around grep-coding-system-setup compile)
;;   "When a prefix argument given, specify coding-system-for-read."
;;   (let ((coding-system-for-read 'utf-8))
;;     ad-do-it))
(defadvice grep (around grep-coding-system-setup compile)
  ""
  (let ((old-default-process-coding-system default-process-coding-system))
    (setq default-process-coding-system '(utf-8 . cp932))
    ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))
;; (ad-activate-regexp "grep-coding-system-setup")
;; (ad-deactivate-regexp "grep-coding-system-setup")
