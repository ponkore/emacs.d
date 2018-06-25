;; ricty だと、[] の下が欠ける
;;(set-frame-font "ricty-12")

;; noto mono だと全角文字が半角の２倍幅になっていない
;;(set-frame-font "noto mono-10")

;; 結局 MeiryoKe_Console にした
;; https://qiita.com/melito/items/238bdf72237290bc6e42
;;(set-face-attribute 'default nil :family "Inconsolata" :height 110)
(set-face-attribute 'default nil :family "Consolas" :height 108)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
(setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))
;;あいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえお
;;1234567890123456789012345678901234567890123456789012345678901234567890
