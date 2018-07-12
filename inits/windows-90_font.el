;; https://qiita.com/melito/items/238bdf72237290bc6e42

;; [NG] ricty だと、[] の下が欠ける
;; (set-frame-font "ricty-12")
;; [NG] noto mono だと全角文字が半角の２倍幅になっていない
;; (set-frame-font "noto mono-10")
;; [△] Consolas & Meiryoke_Console だと丸付き数字(①等)が半角幅になってしまっている
;; [△] Inconsolata & Meiryoke_Console だと全角○が半角幅になってしまっている
;; [△] Meiryoke_Console 統一だと文字幅問題はないが、行高さが詰まりすぎ、O0liの区別がつきにくい

;;(set-face-attribute 'default nil :family "Inconsolata" :height 110)
;;(set-face-attribute 'default nil :family "Consolas" :height 108)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
(defun windows-emacs-font-setting (size)
  "Set windows emacs japanese fonts(Meiryoke_Console mix Version)."
  (let* ((asciifont "Meiryoke_Console")
         (jpfont "Meiryoke_Console")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)
    (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.0)))))

(windows-emacs-font-setting 10)
;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
