;;; text-mode
;; テキスト編集用のモード
;; 2012-03-18

;; text-modeでバッファーを開いたときに行う設定
(add-hook
 'text-mode-hook
 (lambda ()
   ;; スペースでインデントする
   (setq indent-tabs-mode nil)
   ;; .table.txt の場合は truncate-line する
   (when (string= (substring-no-properties (buffer-file-name) -10) ".table.txt")
     (toggle-truncate-lines 1)
     (setq buffer-read-only t))))
