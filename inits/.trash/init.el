;;; ディレクトリ
;; diredを便利にする
(require 'dired-x)
;; diredから"r"でファイル名をインライン編集する
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


;;; 現在の関数名をウィンドウ上部に表示する。
;; 2011-03-15
(which-function-mode 1)
