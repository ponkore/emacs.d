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
