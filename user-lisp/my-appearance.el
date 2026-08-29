;;; my-appearance.el --- フォント・フレーム・テーマ・モードライン  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; フォント設定
;;; --------------------------------------------------

;;; [3] フォント設定

(leaf *font-setting
  :if window-system
  :after all-the-icons
  :config
  (defun emacs-font-setting (font-name size)
    "Set emacs japanese fonts."
    ;; Note:
    ;; https://qiita.com/melito/items/238bdf72237290bc6e42
    ;; [NG] noto mono だと全角文字が半角の２倍幅になっていない
    ;; (set-frame-font "noto mono-10")
    ;; [△] Consolas & Meiryoke_Console だと丸付き数字(①等)が半角幅になってしまっている
    ;; [△] Inconsolata & Meiryoke_Console だと全角○が半角幅になってしまっている
    ;; [△] Meiryoke_Console 統一だと文字幅問題はないが、行高さが詰まりすぎ、O0liの区別がつきにくい
    ;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
    ;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
    ;;
    ;; JIS第２水準：Ricty / HackGenNerd は〇、Ricty Diminished は×
    ;; Italic: Ricty Diminished / PlemolJP は〇、Ricty / HackGenNerd は×
    ;;    ただし、Ricty Diminished で×は半角になってしまう
    ;; HackGenNerd の Nerd フォントは、一部漢字コードに割当たっている
    (let* ((asciifont font-name)
           (jpfont font-name)
           (h (round (* size 10)))
           (ascii-fontspec (font-spec :family asciifont))
           (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      ;; Japanese
      (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      ;; Latin with pronounciation annotations
      (set-fontset-font nil '(#x0080 . #x024F) ascii-fontspec)
      ;; Math symbols
      (set-fontset-font nil '(#x2200 . #x22FF) ascii-fontspec)
      ;; Greek
      (set-fontset-font nil '(#x0370 . #x03FF) ascii-fontspec)
      ;; Some Icons (Nerd Font / Powerline の私用領域)
      ;; この範囲を ascii-fontspec (= HackGen) に割り当てていたが、
      ;; HackGen には Nerd グリフが無いため豆腐になっていた。
      ;; (以前は HackGenNerd を全体に使っていたので問題が出ていなかった)
      ;; 本文は HackGen のまま、この範囲だけ Nerd グリフを持つフォントに回す。
      (let ((nerd (seq-find (lambda (f) (member f (font-family-list)))
                            '("HackGenNerd" "HackGen35Nerd"
                              "HackGen Console NF" "Symbols Nerd Font Mono"))))
        (set-fontset-font nil '(#xE0A0 . #xEEE0)
                          (font-spec :family (or nerd asciifont))))
      ;; all-the-icons-font (下記設定を入れると、いろんなアイコンがおかしくなってしまう)
      ;; (setq range '(#xe000 . #xf8ff))
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-material-family)) nil 'append)
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-faicon-family)) nil 'append)
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-octicon-family)) nil 'append)
      ;; (set-fontset-font nil range (font-spec :family (all-the-icons-wicon-family)) nil 'append)
      ;; org-bullets で使うフォントだけ all-the-icons- の font を割り当てる
      (set-fontset-font nil '(#xf219 . #xf219) (font-spec :family (all-the-icons-faicon-family)) nil 'append)
      (set-fontset-font nil '(#xe3d0 . #xe3d6) (font-spec :family (all-the-icons-material-family)) nil 'append)
      (when (eq window-system 'ns)
        (set-fontset-font t '(#x1f300 . #x1f9ff) "Apple Color Emoji" nil 'append)
        (set-fontset-font t '(#x1fa70 . #x1fbff) "Apple Color Emoji" nil 'append)
        (set-fontset-font t '(#x1f900 . #x1f9e0) "Apple Color Emoji" nil 'append))
      (when (eq window-system 'w32)
        (set-fontset-font t '(#x1f300 . #x1f9ff) "Segoe UI Emoji" nil 'append)
        (set-fontset-font t '(#x1fa70 . #x1fbff) "Segoe UI Emoji" nil 'append)
        (set-fontset-font t '(#x1f900 . #x1f9e0) "Segoe UI Emoji" nil 'append))
      (setq face-font-rescale-alist `((,font-name . 1.0)))))

  (defun setup-font ()
    (interactive)
    ;; 以前は ns / w32 しか分岐が無く、Linux (x / pgtk) では
    ;; フォントが一切設定されなかった。
    (pcase window-system
      ('ns  (emacs-font-setting "HackGen" 16))   ;; previous: "Ricty"
      ('w32 (emacs-font-setting "HackGen" 12))   ;; previous: ("HackGenNerd" 11)
      ((or 'x 'pgtk)
       ;; Linux では入っているものを順に探す
       (let ((family (seq-find (lambda (f) (member f (font-family-list)))
                               '("HackGen" "HackGenNerd" "Ricty"
                                 "Noto Sans Mono CJK JP" "DejaVu Sans Mono"))))
         (when family (emacs-font-setting family 12))))))
  (setup-font))

;;; [3] text-scale

(leaf text-scale
  :hydra (hydra-zoom ()
                     "Zoom"
                     ("g" text-scale-increase "in")
                     ("l" text-scale-decrease "out")
                     ("r" (text-scale-set 0) "reset")
                     ("0" (text-scale-set 0) :bind nil :exit t))
  :bind ("<f2>" . hydra-zoom/body))

;;; [3] all-the-icons

;;; [4] all-the-icons

(leaf all-the-icons
  :straight t
  ;; :after all-the-icons-ivy ivy
  :custom
  (all-the-icons-scale-factor . 1.0)
  :config
  ;; (when window-system
  ;;   (defun my-ivy-format-function-arrow (cands)
  ;;     "Transform CANDS into a string for minibuffer."
  ;;     (ivy--format-function-generic
  ;;      (lambda (str)
  ;;        (concat (all-the-icons-faicon
  ;;                 "hand-o-right"
  ;;                 :v-adjust -0.2 :face 'my-ivy-arrow-visible)
  ;;                " " (ivy--add-face str 'ivy-current-match)))
  ;;      (lambda (str)
  ;;        (concat (all-the-icons-faicon
  ;;                 "hand-o-right" :face 'my-ivy-arrow-invisible) " " str))
  ;;      cands
  ;;      "\n"))
  ;;   (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))
  ;;   (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-projectile-switch-project)
  ;;   (add-to-list 'all-the-icons-ivy-buffer-commands 'counsel-ibuffer)
  ;;   (all-the-icons-ivy-setup)
  ;;   (setq ivy-format-functions-alist '((t . ivy-format-function-arrow))))

  ;; override for .tsx
  (defun all-the-icons--web-mode (&optional family arg-overrides)
    "Return icon or FAMILY for `web-mode' based on `web-mode-content-type'.
Providing ARG-OVERRIDES will modify the creation of the icon."
    (let ((non-nil-args (cl-reduce (lambda (acc it) (if it (append acc (list it)) acc)) arg-overrides :initial-value '())))
      (cond
       ((equal web-mode-content-type "tsx")
        (if family (all-the-icons-fileicon-family) (apply 'all-the-icons-fileicon (append '("typescript") non-nil-args))))
       ((equal web-mode-content-type "jsx")
        (if family (all-the-icons-fileicon-family) (apply 'all-the-icons-fileicon (append '("jsx-2") non-nil-args))))
       ((equal web-mode-content-type "javascript")
        (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("javascript") non-nil-args))))
       ((equal web-mode-content-type "json")
        (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("less") non-nil-args))))
       ((equal web-mode-content-type "xml")
        (if family (all-the-icons-faicon-family) (apply 'all-the-icons-faicon (append '("file-code-o") non-nil-args))))
       ((equal web-mode-content-type "css")
        (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("css3") non-nil-args))))
       (t
        (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("html5") non-nil-args))))))))

;;; [4] all-the-icons-dired

(leaf all-the-icons-dired
  :straight t
  :after all-the-icons
  :custom (all-the-icons-dired-monochrome . nil)
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :config
  (add-to-list 'all-the-icons-extension-icon-alist
               '("tsx" all-the-icons-fileicon "typescript" :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("inc" all-the-icons-fileicon "php" :face all-the-icons-lsilver))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("phpm" all-the-icons-fileicon "php" :face all-the-icons-lsilver)))

;;; [4] all-the-icons-ibuffer

(leaf all-the-icons-ibuffer
  :straight t
  :after all-the-icons
  :init
  (all-the-icons-ibuffer-mode 1)
  :bind (("C-x C-b" . ibuffer)))

;;; --------------------------------------------------
;;; ウィンドウ表示設定
;;; --------------------------------------------------

;;; [3] Mac用

;; 注意: default-frame-alist は early-init.el でも設定している
;; (ツールバー等の非表示)。setq で丸ごと上書きするとそれが消えるため、
;; 以下ではいずれも append で既存の値を残している。

(leaf frame-setting-mac
  :if (eq system-type 'darwin)
  :config
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           ;; (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0)
           (top . 0)
           (width . 180)
           (height . 83))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] Windows用

(leaf frame-setting-windows
  :if (eq system-type 'windows-nt)
  :config
  (setq initial-frame-alist
        (append
         ;; ns-transparent-titlebar は macOS 専用パラメータなので削除した
         '((vertical-scroll-bars . nil) ;; スクロールバーを消す
           (internal-border-width . 0)
           ;; position
           (top . 40)
           (left . 670)
           (width . 136)
           (height . 50))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] Linux用

(leaf frame-setting-linux
  ;; これまで Linux の分岐が無く default-frame-alist が未設定だった
  :if (memq system-type '(gnu/linux berkeley-unix))
  :config
  (setq initial-frame-alist
        (append
         '((vertical-scroll-bars . nil)
           (internal-border-width . 0)
           (top . 0)
           (left . 0)
           (width . 160)
           (height . 50))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] 共通

(leaf frame-setting-common
  :config
  ;; フレームタイトルの設定
  (setq frame-title-format "%b")
  ;; 背景の透明度
  (set-frame-parameter nil 'alpha 85)
  ;; scroll bar を表示しない
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
  ;; 行番号のface
  ;; TODO: `linum` という face が Emacs29? からなくなった
  ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
  ;; (set-face-attribute 'linum nil :height 0.8)
  )

;;; [3] テーマ

;; https://zenn.dev/lambdagonbei/articles/1b2bce27673078
(leaf modus-themes
  :straight t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; (leaf color-theme-sanityinc-tomorrow
;;   :straight t
;;   :config
;;   ;; (load-theme 'pastels-on-dark t)
;;   ;; (enable-theme 'pastels-on-dark)
;;   (color-theme-sanityinc-tomorrow-blue))

;;; --------------------------------------------------
;;; モードライン
;;; --------------------------------------------------

;;; [3] diminish

(leaf diminish :straight t)

;;; [3] doom-modeline

(leaf doom-modeline
  :straight t
  :if window-system
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style . 'truncate-with-project)
  (doom-modeline-icon . t)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-minor-modes . t)
  (doom-modeline-buffer-encoding . t)
  `(doom-modeline-icon . ,(display-graphic-p))
  :custom-face
  (mode-line                       . '((t (:background "medium blue" :foreground "snow" :box nil)))) ;; firebrick3
  (doom-modeline-buffer-minor-mode . '((t (:inherit mode-line :slant normal))))
  :hook (emacs-startup-hook . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (which-function-mode 0)
  ;;
  (doom-modeline-def-segment my:buffer-encoding
    "Displays the encoding and eol style of the buffer."
    (when doom-modeline-buffer-encoding
      (propertize
       (concat
        (let ((sys (coding-system-plist buffer-file-coding-system)))
          (cond ((memq (plist-get sys :category)
                       '(coding-category-undecided coding-category-utf-8))
                 " U")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided japanese-iso-8bit))
                 " E")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided iso-2022-jp))
                 " J")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided japanese-shift-jis japanese-cp932))
                 " S")
                (t " =")))
        (pcase (coding-system-eol-type buffer-file-coding-system)
          (0 "")
          (1 ".CRLF")
          (2 ".CR")))
       'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
       'help-echo 'mode-line-mule-info-help-echo
       'mouse-face '(:box 0)
       'local-map mode-line-coding-system-map)))
  ;;
  (doom-modeline-def-modeline
    'main
    ;; '(workspace-number bar window-number evil-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(bar my:buffer-encoding matches buffer-info buffer-position selection-info major-mode vcs)
    '(misc-info debug minor-modes "-" input-method process checker)))

(provide 'my-appearance)
;;; my-appearance.el ends here
