;;; スペルチェック
;;; 2011-03-09

;; M-TABのキーバインドを変更しない
;; 2011-03-27
(setq flyspell-use-meta-tab nil)
;; デフォルトで自動スペルチェック機能を有効にする
(setq-default flyspell-mode t)
;; スペルチェックには英語の辞書を使う
(setq ispell-dictionary "american")
