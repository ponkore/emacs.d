;; svn log の出力は cp932
(add-hook 'vc-svn-log-view-mode-hook (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))
