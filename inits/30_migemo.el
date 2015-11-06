;; ----------------------------------------
;; MIGEMO (http://blawat2015.no-ip.com/~mieki256/diary/201312203.html)
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  ;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; (setq migemo-options '("-q" "--emacs"))

  ;; migemo-dict のパスを指定
  ;; (setq migemo-dictionary "C~/.emacs.d/migemo-dict/utf-8")
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/dict/utf-8/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)

  ;; 辞書の文字コードを指定．
  (setq migemo-coding-system 'utf-8-unix)
  ;; (setq migemo-coding-system 'euc-jp-unix)

  ;; キャッシュ機能を利用する
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)

  (load-library "migemo")

  ;; 起動時に初期化も行う
  (migemo-init)

  ;; emacs 24.3 で C-s を働かせるための設定
;;  (setq search-whitespace-regexp nil)

  ;; (eval-after-load "migemo"
  ;;   '(defadvice isearch-search (around migemo-search-ad activate)
  ;; 	 "adviced by migemo."
  ;; 	 (let ((saved-isearch-lax-whitespace isearch-lax-whitespace))
  ;; 	   (when migemo-isearch-enable-p
  ;; 		 (setq migemo-do-isearch t)
  ;; 		 (setq isearch-lax-whitespace nil))
  ;; 	   (unwind-protect
  ;; 		   ad-do-it
  ;; 		 (setq migemo-do-isearch nil)
  ;; 		 (setq isearch-lax-whitespace saved-isearch-lax-whitespace))))
  ;;   )

  ;; 起動時に off にする
  ;; (setq migemo-isearch-enable-p nil)
)
