;; ricty だと、[] の下が欠ける
;;(set-frame-font "ricty-12")
;; noto mono だと全角文字が半角の２倍幅になっていない
;;(set-frame-font "noto mono-10")

;; https://qiita.com/melito/items/238bdf72237290bc6e42
;;(set-face-attribute 'default nil :family "Inconsolata" :height 110)
;;(set-face-attribute 'default nil :family "Consolas" :height 108)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;;(set-face-attribute 'default nil :family "Inconsolata" :height 100)
(defun windows-emacs-font-setting (size)
  "Set windows emacs japanese fonts(Consolas & Meiryoke_Console mix Version)."
  (let* ((asciifont "Meiryoke_Console")
         (jpfont "Meiryoke_Console")
         (h (* size 10))
         (fontspec)
         (jp-fontspec))
    (set-face-attribute 'default nil :family asciifont :height h)
    (setq fontspec (font-spec :family asciifont))
    (setq jp-fontspec (font-spec :family jpfont))
    (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)
    (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.0)))))

(windows-emacs-font-setting 12)
;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお
;;1234567890 1234567890 1234567890 1234567890 1234567890 1234567890
