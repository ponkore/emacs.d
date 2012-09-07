;;; match-paren
(defun my-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
   (t (self-insert-command (or arg 1)))))

;;;
(defun cocoa-emacs-font-setting (size)
  "Set cocoa emacs japanese fonts."
  (let* ((asciifont "Menlo")
         (jpfont "Hiragino Maru Gothic ProN")
         (h (* size 10))
         (fontspec)
         (jp-fontspec))
    (set-face-attribute 'default nil :family asciifont :height h)
    (setq fontspec (font-spec :family asciifont))
    (setq jp-fontspec (font-spec :family jpfont))
    (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
)

(defun cocoa-emacs-font-setting-old-version (size)
  "Set cocoa emacs japanese fonts(old-version)."
  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 140)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))
