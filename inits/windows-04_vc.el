;; svn log の出力は cp932
(add-hook 'vc-svn-log-view-mode-hook (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))

;; vc.el のどこかの関数に defadvice するとよさそう
;; (setq default-process-coding-system '(utf-8 . cp932))
;; (defadvice vc-svn-command (around vc-svn-coding-system-setup compile)
;;   (let ((coding-system-for-read '(utf-8 . cp932)))
;;     ad-do-it))
;; (ad-activate-regexp "vc-svn-coding-system-setup")
;; (add-to-list 'process-coding-system-alist '("svn" utf-8 . cp932))
