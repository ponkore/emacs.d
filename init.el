;;; init.el --- My init.el  -*- lexical-binding: t; indent-tabs-mode: nil -*-

;; Copyright (C) 2020  Masao KATO

;; Author: Masao KATO <ponkore@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; only for my office environment
(load (expand-file-name "~/.emacs.d/config-proxy.el") t)

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("marmalade" . "https://marmalade-repo.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;;
;; initialize straight
;;
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(when (>= (string-to-number emacs-version) 27)
  (setq byte-compile-warnings '(cl-functions)))

;; Avoid to write `package-selected-packages` in init.el
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; ~/.emacs.d/site-lisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(leaf diminish :straight t)
(leaf hydra :straight t)

;;
;; ~/.emacs.d/init.el
;;
(when (eq system-type 'windows-nt)
  (setq w32-get-true-file-attributes nil)
  (setenv "HOME" (getenv "USERPROFILE")))

(leaf *japanese-env
  :config
  ;; 日本語環境
  (setenv "LANG" "ja_JP.UTF-8")

  ;; Localeに合わせた環境の設定
  (set-locale-environment nil)

  ;; 機種依存文字
  (leaf cp5022x
    :ensure t
    :require t
    :config
    ;; charset と coding-system の優先度設定
    (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                          'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
    (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932))

  (define-coding-system-alias 'euc-jp 'cp51932)

  ;; decode-translation-table の設定
  (coding-system-put 'euc-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'iso-2022-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; encode-translation-table の設定
  (coding-system-put 'euc-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'iso-2022-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'cp932 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; East Asian Ambiguous
  (defun set-east-asian-ambiguous-width (width)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (dolist (range
               '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                        (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                        #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                        (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                        (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                        #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                        (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                        (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                        (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                        #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                        (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                        #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                        (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
                        (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                        (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                        (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                        #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                        #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                        #x212B (#x2153 . #x2154) (#x215B . #x215E)
                        (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                        (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                        (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                        #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                        (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                        (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                        (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                        (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                        #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                        (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
                        (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                        (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                        (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
                        (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                        (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                        #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
                        (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                        (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
                        #xFFFD
                        ))
        (set-char-table-range table range width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table)))

  (set-east-asian-ambiguous-width 2)

  ;; 全角チルダ/波ダッシュをWindowsスタイルにする
  (let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
    (mapc
     (lambda (coding-system)
       (coding-system-put coding-system :decode-translation-table table)
       (coding-system-put coding-system :encode-translation-table table)
       )
     '(utf-8 cp932 utf-16le)))

  ;; cp932エンコード時の表示を「P」とする
  (coding-system-put 'cp932 :mnemonic ?P)
  (coding-system-put 'cp932-dos :mnemonic ?P)
  (coding-system-put 'cp932-unix :mnemonic ?P)
  (coding-system-put 'cp932-mac :mnemonic ?P)

  ;; PuTTY 用の terminal-coding-system の設定
  (apply 'define-coding-system 'utf-8-for-putty
         "UTF-8 (translate jis to cp932)"
         :encode-translation-table
         (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
         (coding-system-plist 'utf-8))
  (set-terminal-coding-system 'utf-8-for-putty))

(leaf *encoding
  :config
  (leaf encoding-mac
    :if (eq system-type 'darwin)
    :config
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))
  (leaf encoding-windows
    :if (eq system-type 'windows-nt)
    :config
    (setq default-process-coding-system '(utf-8 . utf-8))))

(leaf *font-setting
  :if window-system
  :after all-the-icons
  :config
  (defun emacs-font-setting (font-name size)
    "Set emacs japanese fonts."
    ;; Note:
    ;; https://qiita.com/melito/items/238bdf72237290bc6e42
    ;; [NG] ricty だと、[] の下が欠ける
    ;; (set-frame-font "ricty-12")
    ;; [NG] noto mono だと全角文字が半角の２倍幅になっていない
    ;; (set-frame-font "noto mono-10")
    ;; [△] Consolas & Meiryoke_Console だと丸付き数字(①等)が半角幅になってしまっている
    ;; [△] Inconsolata & Meiryoke_Console だと全角○が半角幅になってしまっている
    ;; [△] Meiryoke_Console 統一だと文字幅問題はないが、行高さが詰まりすぎ、O0liの区別がつきにくい
    ;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
    ;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
    (let* ((asciifont font-name)
           (jpfont font-name)
           (h (round (* size 10)))
           (fontspec (font-spec :family asciifont))
           (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil '(#x0080 . #x024F) fontspec)
      (set-fontset-font nil '(#x0370 . #x03FF) fontspec)
      ;; all-the-icons-font
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-material-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-faicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-octicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-wicon-family)) nil 'append)
      (setq face-font-rescale-alist '((font-name . 1.0)))))
  (defun setup-font ()
    (interactive)
    (when (eq system-type 'darwin)
      (emacs-font-setting "Ricty Diminished" 16))
    ;; (when (eq system-type 'windows-nt)
    ;;   (emacs-font-setting "HackgenNerd" 10))
    (when (eq system-type 'windows-nt)
      (emacs-font-setting "Ricty Diminished" 12)))
  (setup-font))

(leaf *modifier
  :config
  (leaf *modifier-macos
    :if (eq system-type 'darwin)
    :config
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom (auto-revert-interval . 1)
  :global-minor-mode global-auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *interactive-search-next
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (leaf marginalia
    :straight t)

  (leaf vertico
    :straight t
    :custom
    (vertico-mode . t)
    (vertico-cycle . t)
    ;; 補完候補を最大20行まで表示する
    (vertico-count . 20)
    :hook
    (emacs-startup-hook . vertico-after-init-hook)
    :commands vertico-previous vertico-next
    :bind
    (:vertico-map
     ("C-r" . vertico-previous) ;; C-s/C-rで行を移動できるようにする
     ("C-s" . vertico-next))
    :advice
    (:around vertico--format-candidate
             (lambda (orig cand prefix suffix index _start)
                  (setq cand (funcall orig cand prefix suffix index _start))
                  (concat
                   (if (= vertico--index index)
                       (propertize "» " 'face 'vertico-current)
                     "  ")
                   cand)))
    :config
    (defun vertico-after-init-hook ()
      (marginalia-mode))
    ;; add extension
    (straight-use-package '(vertico :files (:defaults "extensions/*")
                                    :includes (vertico-buffer
                                               vertico-directory
                                               vertico-flat
                                               vertico-indexed
                                               vertico-mouse
                                               vertico-quick
                                               vertico-repeat
                                               vertico-reverse)))
    ;; dirty hack...
    (define-key vertico-map (kbd "C-l") 'vertico-directory-delete-char))

  (leaf vertico-directory
    :straight t
    :after vertico
    :commands
    vertico-directory-delete-char
    vertico-directory-enter
    vertico-directory-delete-word
    vertico-directory-tidy
    :bind
    (:vertico-map
     ("C-l" . vertico-directory-delete-char)
     ("RET" . vertico-directory-enter)
     ("DEL" . vertico-directory-delete-char)
     ("M-DEL" . vertico-directory-delete-word))
    :hook
    (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :custom
    `(file-name-shadow-properties . '(invisible t intangible t))
    :config
    (file-name-shadow-mode +1))

  (leaf consult
    :straight t
    :bind
    (("C-s" . my:consult-line)
     ("C-x C-r" . consult-recent-file)
     ("C-x l" . consult-goto-line)
     ("C-x b" . consult-buffer))
    :custom
    `((consult-preview-raw-size . 1024000)
      (consult-preview-key . ,(kbd "C-M-p"))
      (consult-narrow-key . "<"))
    :init
    ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
    (defun my:consult-line (&optional at-point)
      "Consult-line uses things-at-point if set C-u prefix."
      (interactive "P")
      (if at-point
          (consult-line (thing-at-point 'symbol))
        (consult-line))))

  (leaf embark
    :straight t
    :after consult
    :config
    (leaf embark-consult
      :straight t)
    :bind (("C-S-a" . embark-act)))

  (leaf orderless
    :straight t
    :custom
    ;; 補完スタイルにorderlessを利用する
    `((completion-styles . '(orderless))
      (orderless-matching-styles . '(orderless-prefixes
                                     orderless-flex
                                     orderless-regexp
                                     orderless-initialism
                                     orderless-literal))))

  ;; (leaf corfu
  ;;   :straight t
  ;;   :commands corfu-global-mode
  ;;   :custom
  ;;   (corfu-cycle . t) ;; Enable cycling for `corfu-next/previous'
  ;;   (corfu-auto . t)  ;; Enable auto completion
  ;;   ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;;   ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;;   ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;;   :init
  ;;   (corfu-global-mode))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ivy (https://qiita.com/blue0513/items/c0dc35a880170997c3f5)
;;
(leaf *interactive-search
  :disabled t
  :config

  (leaf ivy
    :straight t
    :diminish t
    :custom
    (ivy-use-virtual-buffers . t)
    (ivy-height . 20)
    (ivy-extra-directories . nil)
    (ivy-wrap . t)
    (ivy-re-builders-alist . '((t . ivy--regex-plus)))
    (ivy-extra-directories . '("../" "./"))
    (ivy-count-format . "(%d/%d) ")
    (ivy-format-functions-alist . '((t . ivy-format-function-arrow)))
    :bind (:ivy-minibuffer-map ("C-l" . ivy-backward-delete-char))
    :hook (emacs-startup-hook . ivy-mode)
    :config
    (when (require 'ivy-hydra nil t)
      (setq ivy-read-action-function #'ivy-hydra-read-action))
    (when (setq enable-recursive-minibuffers t)
      (minibuffer-depth-indicate-mode 1))
    (defface my-ivy-arrow-visible
      '((((class color) (background light)) :foreground "orange")
        (((class color) (background dark)) :foreground "#EE6363"))
      "Face used by Ivy for highlighting the arrow.")
    (defface my-ivy-arrow-invisible
      '((((class color) (background light)) :foreground "#FFFFFF")
        (((class color) (background dark)) :foreground "#31343F"))
      "Face used by Ivy for highlighting the invisible arrow."))

  (leaf ivy-hydra
    :straight t)

  (leaf ivy-rich
    :straight t
    :hook (emacs-startup-hook . ivy-rich-mode)
    :config
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))

    (let ((rich-transformer-config '(:columns
                                     ((ivy-rich-switch-buffer-icon :width 2)
                                      (ivy-rich-candidate (:width 30))
                                      (ivy-rich-switch-buffer-size (:width 7))
                                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                                     :predicate
                                     (lambda (cand) (get-buffer cand)))))
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer
                 rich-transformer-config)
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer-other-window
                 rich-transformer-config)))

  (leaf counsel
    :straight t
    :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-x C-b" . counsel-ibuffer)
    ("C-x C-r" . counsel-recentf)
    :custom
    (counsel-find-file-ignore-regexp . (regexp-opt '("./" "../")))
    :config
    (defun ad:counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: "
                (progn
                  (mapcar #'substring-no-properties recentf-list) ;; no need?
                  (mapcar #'abbreviate-file-name recentf-list)) ;; ~/
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    (advice-add 'counsel-recentf :override #'ad:counsel-recentf))

  (leaf swiper
    :straight t
    :bind
    ("C-s" . swiper)
    (:swiper-map ("C-w" . ivy-yank-word))
    :custom (swiper-include-line-number-in-search . t))

  ;; TODO migemo + swiper（日本語をローマ字検索できるようになる）
  ;; (require 'avy-migemo)
  ;; (avy-migemo-mode 1)
  ;; (require 'avy-migemo-e.g.swiper)
  )

(leaf doom-modeline
  :straight t
  :if window-system
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style . 'truncate-with-project)
  (doom-modeline-major-mode-icon . nil)
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
    '(bar my:buffer-encoding matches buffer-info buffer-position selection-info)
    '(misc-info debug minor-modes "-" input-method major-mode process vcs checker)))

(leaf *utility-package
  :config

  ;; (leaf all-the-icons-ivy
  ;;   :straight t)

  (leaf all-the-icons
    :straight t
    ;; :after all-the-icons-ivy ivy
    :custom
    (all-the-icons-scale-factor . 1.0)
    ;; :config
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
    )

  (leaf s
    :straight t
    :commands s-join s-split)

  (leaf *setup-exec-path
    :if (not (eq system-type 'windows-nt))
    :config
    (leaf exec-path-from-shell
      :straight t
      :commands exec-path-from-shell-getenv)
    (defun setup-exec-path ()
      (mapc #'(lambda (f)
                (add-to-list 'exec-path (expand-file-name f)))
            (s-split ":" (exec-path-from-shell-getenv "PATH")))
      (setenv "PATH" (s-join ":" exec-path)))
    (setup-exec-path)))

(leaf *dired
  :config
  (leaf dired-k
    :straight t)
  (leaf dired
    :commands dired-vc-status
    :bind
    (:dired-mode-map
     ("V" . dired-vc-status)
     ("K" . dired-k)
     ("G" . ripgrep-regexp)
     ("g" . my:dired-revert-buffer)
     ("." . hydra-dired/body))
    :hook
    (dired-mode-hook . dired-k)
    (dired-initial-position-hook . dired-k)
    :custom
    ;;
    ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
    ;;
    ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
    (dired-dwim-target . t)
    ;; ディレクトリを再帰的にコピーする
    (dired-recursive-copies . 'always)
    ;; diredバッファでC-sした時にファイル名だけにマッチするように
    (dired-isearch-filenames . t)
    ;;
    (ls-lisp-dirs-first . t)
    :config
    (defun my:dired-revert-buffer ()
      (interactive)
      (revert-buffer)
      (dired-k))
    ;; バージョン管理システム
    ;; diredから適切なバージョン管理システムの*-statusを起動
    (defun find-path-in-parents (directory base-names)
      (or (find-if 'file-exists-p
                   (mapcar (lambda (base-name) (concat directory base-name)) base-names))
          (if (string= directory "/")
              nil
            (let ((parent-directory (substring directory 0 -1)))
              (find-path-in-parents parent-directory base-names)))))
    ;;
    (defun dired-vc-status (&rest args)
      (interactive)
      (let ((path (find-path-in-parents (dired-current-directory) '(".git" ".svn"))))
        (cond ((null path)
               (message "not version controlled."))
              ((string-match-p "\\.svn$" path)
               (svn-status (file-name-directory path)))
              ((string-match-p "\\.git$" path)
               (magit-status-internal (file-name-directory path))))))
    ;;
    :hydra
    (hydra-dired (:hint nil :color pink)
                 "
_+_ mkdir   _v_iew         _m_ark         _z_ip     _w_ get filename
_C_opy      view _o_ther   _U_nmark all   un_Z_ip   _W_ get fullpath
_D_elete    open _f_ile    _u_nmark       _s_ort    _g_ revert buffer
_R_ename    ch_M_od        _t_oggle       _e_dit    _[_ hide detail     _._togggle hydra
"
                 ("[" dired-hide-details-mode)
                 ("+" dired-create-directory)
                 ("RET" dired-open-in-accordance-with-situation :exit t)
                 ("f" dired-open-in-accordance-with-situation :exit t)
                 ("C" dired-do-copy)   ;; Copy all marked files
                 ("D" dired-do-delete)
                 ("M" dired-do-chmod)
                 ("m" dired-mark)
                 ("o" dired-view-file-other-window :exit t)
                 ("?" dired-summary :exit t)
                 ("R" dired-do-rename)
                 ("a" dired-list-all-mode)
                 ("g" revert-buffer)
                 ("e" wdired-change-to-wdired-mode :exit t)
                 ("s" dired-sort-toggle-or-edit)
                 ;; ("T" counsel-tramp :exit t)
                 ("t" dired-toggle-marks)
                 ("U" dired-unmark-all-marks)
                 ("u" dired-unmark)
                 ("v" dired-view-file :exit t)
                 ("w" dired-copy-filename-as-kill)
                 ("W" dired-get-fullpath-filename)
                 ("z" dired-zip-files)
                 ("Z" dired-do-compress)
                 ;; ("F" my:finder-app)
                 ;; ("T" my:iterm-app)
                 ("q" nil)
                 ("." nil :color blue)))

  (leaf vc-windows
    :if (eq system-type 'windows-nt)
    :hook
    ;; svn log の出力は cp932
    (vc-svn-log-view-mode-hook . (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))
    :config
    ;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
    ;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)
    (defadvice vc-svn-command (around vc-svn-coding-system-setup compile)
      (let ((old-default-process-coding-system default-process-coding-system))
        (setq default-process-coding-system '(utf-8 . cp932))
        ad-do-it
        (setq default-process-coding-system old-default-process-coding-system)))
    (ad-activate-regexp "vc-svn-coding-system-setup")))

(leaf *major-mode
  :config

  (leaf *shell
    :config
    (leaf shell-windows
      :if (eq system-type 'windows-nt)
      :hook
      (shell-mode-hook . (lambda ()
                           ;; シェルモードの入出力文字コード(cp932 -> utf-8)
                           ;; (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
                           ;; (set-buffer-file-coding-system    'utf-8-unix)
                           (set-buffer-process-coding-system 'cp932 'cp932)
                           (set-buffer-file-coding-system    'cp932)))
      :config
      (require 'shell)
      (setq explicit-shell-file-name "bash.exe")
      (setq shell-command-switch "-c")
      (setq shell-file-name "bash.exe")
      ;; (M-! and M-| and compile.el)
      (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
      ;; エスケープシーケンス処理の設定
      (autoload 'ansi-color-for-comint-mode-on "ansi-color"
        "Set `ansi-color-for-comint-mode' to t." t))
    (leaf shell
      :hook
      ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
      (shell-mode-hook . (lambda ()
                           (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
      ;; or shellモードの時の^M抑制 (どっちが正しい？)
      ;; (comint-output-filter-functions . shell-strip-ctrl-m nil t)
      :config
      ;; shell-modeでの補完 (for drive letter)
      (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")))

  (leaf *org-mode
    :config
    (leaf org
      :straight t
      :mode ("\\.org$" . org-mode)
      :hook (org-mode-hook . turn-on-font-lock)
      :custom
      ;; org-mode内部のソースを色付けする
      (org-src-fontify-natively . t)
      ;; org-modeの開始時に、行の折り返しを無効にする。
      (org-startup-truncated . t)
      ;; follow-linkから戻ることを可能とする。
      (org-return-follows-link . t)
      (org-refile-use-outline-path . 'file)
      (org-outline-path-complete-in-steps . nil)
      (org-log-done . t)
      (org-todo-keywords . '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
      (org-indent-indentation-per-level . 0)
      (org-adapt-indentation . nil)
      (org-clock-clocked-in-display . 'none)
      (org-clock-out-remove-zero-time-clocks . t)
      :config
      ;; 一時間に一回、org-modeの全てのバッファを保存する。
      (run-at-time "00:59" 3600 #'org-save-all-org-buffers)
      (leaf *org-local-functions
        :config
        (defun my:org-add-ymd-to-archive (name)
          "replace anchor to YYYY-MM string"
          (let* ((ymd (format-time-string "%Y-%m")))
            (replace-regexp-in-string "#YM" ymd name)))
        (advice-add 'org-extract-archive-file :filter-return #'my:org-add-ymd-to-archive)
        ;; screenshot: https://ladicle.com/post/config/
        (defun my:org-screenshot ()
          "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
          (interactive)
          (org-display-inline-images)
          (setq filename
                (concat
                 (make-temp-name
                  (concat (file-name-nondirectory (buffer-file-name))
                          "_imgs/"
                          (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
          (unless (file-exists-p (file-name-directory filename))
            (make-directory (file-name-directory filename)))
          ;; take screenshot
          (if (eq system-type 'darwin)
              (call-process "screencapture" nil nil nil "-i" filename))
          (if (eq system-type 'gnu/linux)
              (call-process "import" nil nil nil filename))
          ;; insert into file if correctly taken
          (if (file-exists-p filename)
              (insert (concat "[[file:" filename "]]"))))))
    (leaf org-bullets
      :straight t
      :if window-system
      :custom (org-bullets-bullet-list . '("" "" "" "" "" "" ""))
      :hook (org-mode-hook . org-bullets-mode))
    (leaf org-beautify-theme
      :straight t
      :disabled t
      :config
      (load-theme 'org-beautify t))
    (leaf org-download
      :ensure t
      :custom
      (org-download-image-dir . "./img")))

  (leaf magit
    :straight t
    :hook (magit-mode-hook . my:magit-setup-diff)
    :commands magit-status-internal git-commit-mode git-commit-mode-hook
    :advice (:filter-args magit-expand-git-file-name magit-expand-git-file-name--msys)
    :config
    (defun magit-expand-git-file-name--msys (args)
      "Handle Msys directory names such as /c/* by changing them to C:/*"
      (let ((filename (car args)))
        (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
          (setq filename (concat (match-string 1 filename) ":/"
                                 (match-string 2 filename))))
        (list filename)))
    ;; diff関連の設定
    (defun my:magit-setup-diff ()
      ;; diffを表示しているときに文字単位での変更箇所も強調表示する
      ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
      (setq magit-diff-refine-hunk 'all)
      ;; diff用のfaceを設定する
      (my:diff-mode-setup-faces)))

  (leaf markdown-mode
    :straight t
    :mode ("\\.\\(markdown\\|md\\|mkd\\)\\'" . gfm-mode)
    :preface
    (defun my:setup-markdown-mode ()
      (setq line-move-visual nil)
      (setq truncate-lines nil)
      (electric-indent-local-mode -1))
    :bind
    (:markdown-mode-map ("C-c ." . hydra-markdown/body))
    :hook
    (markdown-mode-hook . my:setup-markdown-mode)
    (gfm-mode-hook      . my:setup-markdown-mode)
    :config
    (setq markdown-command (concat "pandoc -F pandoc-crossref --template=default.html --self-contained -s --from gfm+footnotes --to html --metadata " (expand-file-name "~/AppData/Roaming/pandoc/metadata.yml")))
    :custom
    (markdown-open-command . "c:/Program Files/Typora/Typora.exe")
    (markdown-use-pandoc-style-yaml-metadata . t)
    (markdown-header-scaling . nil)
    :hydra
    (hydra-markdown (:hint nil :exit t)
      "
^Format^      ^Insert^        ^Head.Foot^     ^Code.Link^      ^Move^           ^Pndoc
^^^^^^-----------------------------------------------------------------------------------
_s_torong     _b_lockquote    H1~H6:_a_uto    _c_ode block     _p_romote        _H_tml
italic:_/_    pre:_:_         _f_ootnote      code i_n_line    _d_emote         _P_DF
リスト:_._    _t_able         _r_eference     _l_ink           _j_:move-up      _D_ocx
取消線:_x_    hr:_-_          _i_mage         _u_ri            _k_:move-down    Pre_v_iew"
      ("s" markdown-insert-bold)
      ("/" markdown-insert-italic)
      ("-" markdown-insert-hr)
      ("x" markdown-insert-strike-through)
      ("b" markdown-insert-blockquote)
      (":" markdown-insert-pre)
      ("t" markdown-insert-table)
      ("c" markdown-insert-gfm-code-block)
      ("n" markdown-insert-code)
      ("K" markdown-insert-kbd)
      ("a" markdown-insert-header-dwim)
      ("1" markdown-insert-header-atx-1)
      ("2" markdown-insert-header-atx-2)
      ("3" markdown-insert-header-atx-3)
      ("4" markdown-insert-header-atx-4)
      ("5" markdown-insert-header-atx-5)
      ("6" markdown-insert-header-atx-6)
      ("." markdown-insert-list-item)
      ("i" markdown-insert-imaget)
      ("l" markdown-insert-link)
      ("u" markdown-insert-uri)
      ("f" markdown-insert-footnote)
      ("r" markdown-insert-reference-link-dwim)
      ("p" markdown-promote)
      ("d" markdown-demote)
      ("j" markdown-move-down)
      ("k" markdown-move-up)
      ;; Pandoc
      ("H" md2html :exit t)
      ("P" md2pdf :exit t)
      ("D" md2docx :exit t)
      ("v" markdown-preview :exit t)))

  (leaf rst
    :straight t
    :mode ("\\.\\(rst|rest\\)$" . rst-mode)
    :bind
    (:rst-mode-map
     ;; remove rst-deprecated-* bindings
     ("C-c C-b" . nil)
     ("C-c C-d" . nil)
     ("C-c C-e" . nil)
     ("C-c C-f" . nil)
     ("C-c TAB" . nil)
     ("C-c RET" . nil)
     ("C-c C-n" . nil)
     ("C-c C-p" . nil)
     ("C-c C-s" . nil)
     ("C-c C-u" . nil)
     ("C-c C-v" . nil)
     ("C-c C-w" . nil)
     ("C-c 1" . nil)
     ("C-c 2" . nil)
     ("C-c 3" . nil)
     ("C-c 4" . nil)
     ("C-c 5" . nil)
     ("C-c C-l <t>" . nil)
     ("C-c C-r <t>" . nil)
     ("C-c C-a <t>" . nil))
    :hook (rst-mode-hook . (lambda ()
                             (setq indent-tabs-mode nil)
                             (setq frame-background-mode 'dark))))

  (leaf adoc-mode
    :straight t)

  ;; (let ((i 1))
  ;;   (while (<= i rst-level-face-max)
  ;;     (let ((face-name (intern (format "rst-level-%d-face" i))))
  ;;       (set-face-background face-name nil)
  ;;       (setq i (1+ i)))))

  (leaf elisp-mode
    :require t
    :preface
    (defun my:emacs-lisp-hooks ()
      (setq-local company-idle-delay 0.2)
      (setq-local company-backends '(company-semantic company-files company-elisp))
      (setq-local show-paren-style 'expression))
    ;; (set-newline-and-indent)
    :hook
    (emacs-lisp-mode-hook . my:emacs-lisp-hooks))

  (leaf *lisp
    :config
    (leaf slime-company
      :straight t)
    (leaf slime
      :straight t
      :commands slime-setup
      :custom
      `(inferior-lisp-program . ,(concat (executable-find "ros") " run"))
      :bind
      (:company-active-map
       ("C-d" . company-show-doc-buffer)
       ("M-." . company-show-location))
      :config
      (slime-setup '(slime-repl slime-fancy slime-banner slime-company)))
    (leaf pretty-print
      :hook
      (lisp-interaction-mode-hook . (lambda() (define-key lisp-interaction-mode-map (kbd "C-c RET") 'my:pp-macroexpand-last-sexp)))
      (emacs-lisp-mode-hook . (lambda() (define-key emacs-lisp-mode-map (kbd "C-c RET") 'my:pp-macroexpand-last-sexp)))
      :preface
      (defun my:pp-macroexpand-last-sexp ()
        (interactive)
        (if (thing-at-point-looking-at "\(")
            (save-excursion
              (forward-list)
              (pp-macroexpand-last-sexp nil))
          (pp-macroexpand-last-sexp nil)))))

  (leaf *clojure
    :config
    (leaf clojure-mode
      :straight t
      :commands define-clojure-indent
      :mode ("\\(default\\|user\\|emacs\\)\\.\\(behaviors\\|keymap\\)" . clojure-mode)
      :hook
      (clojure-mode-hook . yas-minor-mode)
      (clojure-mode-hook . smartparens-strict-mode)
      (clojure-mode-hook . flycheck-mode)
      :config
      (define-clojure-indent
        (defroutes 'defun)
        (tabular 'defun)
        (GET 2)
        (POST 2)
        (PUT 2)
        (DELETE 2)
        (HEAD 2)
        (ANY 2)
        (context 2)
        (componentWillMount 'defun)
        (componentDidMount 'defun)
        (componentWillUnmount 'defun)
        ;; for om.next
        (ident 'defun)
        (query 'defun)
        (params 'defun)
        (render 'defun)
        ;;
        (fact 'defun)
        (do-transaction 'defun))
      (eldoc-mode +1)
      (leaf flycheck-clj-kondo
        :straight t))

    (leaf cider
      :straight t
      :bind ("C-c M-j" . cider-jack-in)
      :hook
      (cider-repl-mode-hook . company-mode)
      (cider-mode-hook . company-mode)
      :custom
      (cider-show-error-buffer . t)
      (cider-auto-select-error-buffer . t)
      (cider-repl-result-prefix . ";; => ")
      (nrepl-sync-request-timeout . 40)
      (nrepl-hide-special-buffers . t))

    (leaf cider-lein-command-on-windows
      :if (eq system-type 'windows-nt)
      :config
      ;; on Windows, use lein.bat instead of lein shell script.
      (setq cider-lein-command "lein.bat")))

  (leaf *python
    :config
    (leaf pyvenv
      :straight t
      :config
      ;; (pyvenv-activate my:virtualenv-path)
      nil)
    (leaf python
      :mode ("\\.py$" . python-mode)
      :hook
      (python-mode-hook . my:python-mode-hook-0)
      :preface
      (defun my:python-mode-hook-0 ()
        (setq-local indent-tabs-mode nil)
        (flycheck-mode +1)))
    ;;     (leaf company-jedi
    ;; ;; http://nobunaga.hatenablog.jp/entry/2017/09/24/221004
    ;; ;; https://qiita.com/ignorant/items/50f8eb2852d0f0214659
    ;; ;;
    ;; ;; M-x jedi:install-server
    ;;       :straight t
    ;;       :after company
    ;;       :custom
    ;;       (jedi:complete-on-dot . t)
    ;;       :hook (python-mode-hook . (lambda () (add-to-list 'company-backends 'company-jedi))))

    (leaf jedi
      :straight t)

    (leaf elpy
      ;; https://elpy.readthedocs.io/en/latest/index.html
      :straight t
      :after python-mode
      :config
      (elpy-enable)
      (setenv "WORKON_HOME" "~/.pyenv/pyenv-win/versions/") ;; windows
      (setq elpy-rpc-virtualenv-path 'default)
      (setq elpy-rpc-backend "jedi")
      (pyvenv-workon (expand-file-name "~/.emacs.d/elpy/rpc-venv"))
      ;; (setq python-shell-interpreter "~/.pyenv/shims/python3")
      ))

  (leaf *php
    :config
    (leaf php-mode
      :mode ("\\.\\(cgi\\|phpm\\|inc\\)\\'" . php-mode)
      :straight t
      :custom
      (ac-php-php-executable . "c:/Apps/php-7.4.22-Win32-vc15-x64/php.exe")
      (flycheck-php-phpcs-executable . "phpcs")
      (ac-php-debug-flag . nil)
      :hook
      (php-mode-hook . (lambda ()
                         (company-mode t)
                         (subword-mode 1)
                         (setq tab-width 4)
                         (setq indent-tabs-mode nil)
                         (setq c-basic-offset 4)
                         (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")
                         (ac-php-core-eldoc-setup)
                         (add-to-list 'company-backends 'company-ac-php-backend)
                         (make-local-variable 'company-backends)
                         ;; (require 'flycheck-phpstan)
                         (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
                         ;; (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
                         (setq flycheck-phpcs-standard "PSR2")
                         (flycheck-mode t)))
      :config
      (leaf company-php
        :straight t)
      (leaf flycheck-phpstan
        :straight t)
      :custom
      (php-manual-url . 'ja)
      (php-mode-coding-style . 'psr2)
      :bind
      (:php-mode-map
       (";" . self-insert-command)
       ("{" . self-insert-command)
       ;; ("[" . #'(smartchr "[]" "array()" "[[]]"))
       ;; ("]" . #'(smartchr "array " "]" "]]"))
       ;; ("C-}" . cedit-barf)
       ;; ("C-)" . cedit-slurp)
       ("M-." . ac-php-find-symbol-at-point)
       ("M-," . ac-php-location-stack-back)
       ("C-c C--" . php-current-class)
       ("C-c C-=" . php-current-namespace))))

  (leaf javascript/typescript
    :config
    (leaf add-node-modules-path
      :straight t
      :commands add-node-modules-path)
    (leaf prettier-js
      :straight t
      :commands prettier-js-mode)
    (leaf tide
      :straight t
      :commands setup-tide-mode
      :after typescript-mode company flycheck
      :custom
      (typescript-indent-level . 2)
      (js-indent-level . 2)
      (js2-basic-offset . 2)
      (web-mode-code-indent-offset . 2)
      (web-mode-markup-indent-offset . 2)
      (tide-format-options . '(:indentSize 2 :tabSize 2))
      ;; aligns annotation to the right hand side
      (company-tooltip-align-annotations . t)
      :hook
      (typescript-mode . tide-setup)
      (typescript-mode . tide-hl-identifier-mode)
      ;; formats the buffer before saving
      (before-save-hook . tide-format-before-save)
      :config
      (defun setup-tide-mode ()
        (interactive)
        (add-node-modules-path)
        (tide-setup)
        (flycheck-add-mode 'javascript-eslint 'web-mode)
        (flycheck-mode +1)
        ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
        (setq flycheck-check-syntax-automatically '(idle-change))
        (eldoc-mode +1)
        (tide-hl-identifier-mode +1)
        ;; company is an optional dependency. You have to
        ;; install it separately via package-install
        ;; `M-x package-install [ret] company`
        (company-mode +1)
        ;;
        (prettier-js-mode)))
    (leaf typescript-mode
      :straight t
      :hook (typescript-mode-hook . setup-tide-mode)))

  (leaf web-mode
    :straight t
    :mode ("\\.tsx\\'" . web-mode)
    :hook
    (web-mode-hook . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode))))
    :config
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (prettier-js-mode))

  (leaf js2-mode
    :straight t
    :mode
    ("\\.js"   . js2-mode)
    ("\\.json" . javascript-mode))

  (leaf scss-mode
    :straight t
    :mode ("\\.\\(scss\\|css\\)\\'" . scss-mode)
    :custom
    (scss-compile-at-save . nil) ;; 自動コンパイルをオフにする
    (css-indent-offset . 2)
    (scss-compile-at-save . nil)
    :bind
    (:scss-mode-map
     ("\M-{" . my:css-electric-pair-brace)
     (";" . my:semicolon-ret))
    :config
    (defun my:css-electric-pair-brace ()
      (interactive)
      (insert "{")
      (newline-and-indent)
      (newline-and-indent)
      (insert "}")
      (indent-for-tab-command)
      (previous-line)
      (indent-for-tab-command))
    (defun my:semicolon-ret ()
      (interactive)
      (insert ";")
      (newline-and-indent))
    ;; (setq ac-sources '(ac-source-css-property
    ;;                    ac-source-css-property-names
    ;;                    ac-source-yasnippet
    ;;                    ;; ac-source-words-in-same-mode-buffers
    ;;                    ac-source-words-in-all-buffer
    ;;                    ac-source-dictionary))
    (yas-minor-mode))

  (leaf *rust
    :config
    (leaf racer
      :straight t)
    (leaf rust-mode
      :straight t
      :after racer
      :custom
      `(racer-cmd . ,(expand-file-name "~/.cargo/bin/racer"))
      `(racer-rust-src-path . ,(expand-file-name "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/"))
      (rust-format-on-save . t)
      :hook
      (rust-mode-hook . (lambda () (racer-mode) (flycheck-mode)))
      (flycheck-mode-hook . flycheck-rust-setup)
      (racer-mode-hook . (lambda () (company-mode) (eldoc-mode)))
      :bind
      (:rust-mode-map
       ("TAB" . company-indent-or-complete-common)
       ("C-c d" . racer-describe))))

  (leaf c/c++
    :config
    (leaf cc-mode
      :straight t
      :hook (c-mode-common-hook . my:cc-mode-setup)
      :config
      (defun my:cc-mode-setup ()
        ;; BSDスタイルをベースにする
        (c-set-style "bsd")
        ;; スペースでインデントをする
        (setq indent-tabs-mode nil)
        ;; インデント幅を2にする
        (setq c-basic-offset 2)
        ;; 自動改行（auto-new-line）と
        ;; 連続する空白の一括削除（hungry-delete）を
        ;; 有効にする
        (c-toggle-auto-hungry-state 1)
        ;; CamelCaseの語でも単語単位に分解して編集する
        ;; GtkWindow         => Gtk Window
        ;; EmacsFrameClass   => Emacs Frame Class
        ;; NSGraphicsContext => NS Graphics Context
        (subword-mode 1))))

  (leaf csharp-mode
    :straight t
    :hook (csharp-mode-hook . my:csharp-mode-setup)
    :config
    (defun my:csharp-mode-setup ()
      "my function that runs when csharp-mode is initialized for a buffer."
      (turn-on-font-lock)
      (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
      (setq indent-tabs-mode nil) ;; tabs are evil
      (yas-minor-mode-on)
      (setq comment-column 40)
      (setq c-basic-offset 4)
      ;; (font-lock-add-magic-number)
      ;; オフセットの調整
      (c-set-offset 'substatement-open 0)
      (c-set-offset 'case-label '+)
      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'arglist-close 0)
      ;; see http://qiita.com/masnagam/items/e3313dc9a66bd7fd76fa
      (setq csharp-want-imenu nil)))

  (leaf sql-mode
    :mode ("\\.ddl$" . sql-mode)
    :custom
    (sql-product . 'postgres)
    :bind
    (:sql-mode-map
     ("C-c \"" . wrap-double-quote-thing-at-symbol)
     ("C-c ," . move-trailing-comma-to-line-start))
    :hook
    (sql-mode-hook . (lambda ()
                       (yas-minor-mode-on)
                       (setq indent-tabs-mode nil)))
    :config
    (defun oracle-settings ()
      "setup oracle sql environment"
      ;; for SQL mode (My Office PC Oracle setting)
      (when (eq system-type 'windows-nt)
        (setq sql-oracle-program "c:/Apps/Oracle/sqlplus.exe")
        ;; 新規作成のときだけ cp932 にする
        (add-hook 'sql-mode-hook (lambda ()
                                   (unless (file-exists-p (buffer-file-name (current-buffer)))
                                     (set-buffer-file-coding-system 'cp932)
                                     (set-buffer-modified-p nil)))))
      ;; on Mac, set environment variables
      (when (or (eq system-type 'berkeley-unix) (eq system-type 'darwin))
        (let ((oracle-home (expand-file-name "~/Applications/Oracle/instantclient_10_2")))
          (setenv "NLS_LANG" "JAPANESE_JAPAN.UTF8")
          (setenv "DYLD_LIBRARY_PATH" oracle-home)
          (setenv "LD_LIBRARY_PATH" oracle-home)
          (setq sql-oracle-program (concat oracle-home "/sqlplus"))))
      ;; set Oracle as default SQL product.
      (setq sql-product 'oracle)
      (add-hook 'sql-interactive-mode-hook
                (lambda ()
                  (setq comint-output-filter-functions 'comint-truncate-buffer)
                  (toggle-truncate-lines t)
                  (when (eq system-type 'windows-nt)
                    (set-buffer-process-coding-system 'cp932 'cp932))
                  (comint-send-string (get-buffer-process (current-buffer)) "
ALTER SESSION SET NLS_DATE_FORMAT='YYYY/MM/DD'
/
set linesize 1000
set trimspool on
set timing on
set pagesize 1000
")))
      ;; only for my office environment
      (load (expand-file-name "~/.emacs.d/config-sqlplus.el") t)
      ;; customize font-lock
      (font-lock-add-keywords 'sql-mode
                              '(("\"\\([^\"]*\\)\"" . 'font-lock-constant-face)
                                ("\\<Hgs\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face)
                                ("\\<R[LSC][0-9][A-Z]\\w+\\>\.\\<\\w+\\>" . 'font-lock-builtin-face))))
    (defun wrap-double-quote-thing-at-symbol ()
      (interactive)
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (str (thing-at-point 'symbol))
             (wrapped (format "\"%s\"" str)))
        (delete-region start end)
        (insert wrapped)
        (goto-char (+ 2 end))))

    (defun move-trailing-comma-to-line-start ()
      (interactive)
      (let* ((eol (save-excursion (end-of-line) (point)))
             (pt (re-search-forward ",[ \t]*$" eol t)))
        (when pt
          (goto-char (- pt 1))
          (delete-char 1)
          (forward-line)
          (let* ((eol (save-excursion (end-of-line) (point)))
                 (pt (re-search-forward "^[ \t]*--" eol t)))
            (when pt (forward-line)))
          (let* ((eol (save-excursion (end-of-line) (point))))
            (when (= eol pt) (forward-line)))
          (insert "  ,")
          (just-one-space)))))

  (leaf bat-mode
    :if (eq system-type 'windows-nt)
    :straight t
    :config
    (setq bat-font-lock-keywords
          (eval-when-compile
            (let ((COMMANDS
                   '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
                     "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
                     "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
                     "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
                     "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
                     "ver" "vol" "xcopy"))
                  (CONTROLFLOW
                   '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
                     "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
                  (UNIX
                   '("bash" "cat" "cp" "fgrep" "grep" "ls" "sed" "sh" "mv" "rm")))
              `(("\\_<\\(call\\|goto\\)\\_>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
                 (2 font-lock-constant-face t))
                ("^:[^:].*"
                 . 'bat-label-face)
                ("\\_<\\(defined\\|set\\)\\_>[ \t]*\\(\\w+\\)"
                 (2 font-lock-variable-name-face))
                ("%\\([A-Za-z0-9_]+\\)%?"
                 (1 font-lock-variable-name-face))
                ("!\\([A-Za-z0-9_]+\\)!?"        ; delayed-expansion !variable!
                 (1 font-lock-variable-name-face))
                ("[ =][-/]+\\([A-Za-z0-9_]\\)"
                 (1 font-lock-type-face append))
                (,(concat "\\_<" (regexp-opt COMMANDS) "\\_>") . font-lock-builtin-face)
                (,(concat "\\_<" (regexp-opt CONTROLFLOW) "\\_>") . font-lock-keyword-face)
                (,(concat "\\_<" (regexp-opt UNIX) "\\_>") . font-lock-warning-face))))))

  (leaf lua-mode
    :straight t
    :mode (".nyagos" . lua-mode))

  (leaf visual-basic-mode
    ;; in site-lisp
    :mode ("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . visual-basic-mode)
    :hook (visual-basic-mode-hook . (lambda () (setq mode-name "vb")))
    :config
    (setq visual-basic-mode-indent 4))

  (leaf mayu-mode
    ;; in site-lisp
    :mode ("\\.\\(mayu\\)\\'" . mayu-mode))

  (leaf calendar
    :ensure t
    :custom
    (mark-holidays-in-calendar . t) ; 祝日をカレンダーに表示
    (calendar-month-name-array . ["01" "02" "03" "04" "05" "06"
                                 "07" "08" "09" "10" "11" "12" ])
    (calendar-day-name-array   . ["日" "月" "火" "水" "木" "金" "土"])
    (calendar-day-header-array . ["日" "月" "火" "水" "木" "金" "土"])
    (calendar-week-start-day   . 0)) ;; 日曜開始

;;   (leaf japanese-holidays
;;     :straight t
;;     :custom
;;     (japanese-holiday-weekend . '(0 6)) ; 土日を祝日として表示
;;     (japanese-holiday-weekend-marker . '(holiday nil nil nil nil nil japanese-holiday-saturday)) ; 土曜日を水色で表示
;; ;;    `((calendar-holidays . ,(append japanese-holidays holiday-local-holidays holiday-other-holidays))) ; 他の国の祝日も表示させたい場合は適当に調整
;;     :hook
;;     (calendar-today-visible-hook . japanese-holiday-mark-weekend)
;;     (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
;;     (calendar-today-visible-hook . calendar-mark-today))

  (leaf diff-mode
    :hook
    (diff-mode-hook . my:diff-mode-setup-faces)
    (diff-mode-hook . my:diff-mode-refine-automatically)
    :config
    (defun my:diff-mode-setup-faces ()
      ;; 追加された行は緑で表示
      (set-face-attribute 'diff-added nil :foreground "white" :background "dark green")
      ;; 削除された行は赤で表示
      (set-face-attribute 'diff-removed nil :foreground "white" :background "dark red")
      ;; 文字単位での変更箇所は色を反転して強調
      ;; ;; 2012-04-02 // 2018-05-30 emacs 26.1 でエラーになるのでコメントアウト
      ;; (set-face-attribute 'diff-refine-change nil :foreground nil :background nil :weight 'bold :inverse-video t)
      )
    (defun my:diff-mode-refine-automatically ()
      (diff-auto-refine-mode t)))

  (leaf yaml-mode
    :straight t)

  (leaf log4j-mode
    :straight t)

  (leaf dockerfile-mode
    :straight t
    :mode ("Dockerfile\\'" . dockerfile-mode))

  (leaf docker-compose-mode
    :straight t)
  ;; end of major-mode
  )

(leaf *minor-mode
  :config

  (leaf yasnippet
    :straight t
    :bind (:yas-minor-mode-map
           ("TAB" . nil)
           ("<tab>" . nil)
           ("<C-tab>" . yas-expand)
           ("C-x i i" . yas-insert-snippet)
           ("C-x i n" . yas-new-snippet)
           ("C-x i v" . yas-visit-snippet-file)
           ("C-x i l" . yas-describe-tables))
    :commands yas-expand yas-global-mode yas-insert-snippet yas-visit-snippet-file
    :hook (emacs-startup-hook . yas-global-mode))

  (leaf symbol-overlay
    :straight t
    :bind
    ("M-i" . symbol-overlay-put)
    (:symbol-overlay-map
     ("p" . symbol-overlay-jump-prev)
     ("n" . symbol-overlay-jump-next)
     ("C-g" . symbol-overlay-remove-all))
    :hook
    (prog-mode-hook . symbol-overlay-mode)
    (markdown-mode-hook . symbol-overlay-mode))

  (leaf smartparens
    :straight t
    :commands smartparens-global-mode sp-local-pairs
    :hook (emacs-startup-hook . smartparens-global-mode)
    :config
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
    (sp-local-pair 'lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "`" nil :actions nil)
    (sp-local-pair 'clojure-mode "'" nil :actions nil)
    (sp-local-pair 'clojure-mode "`" nil :actions nil)
    (sp-local-pair 'cider-repl-mode "'" nil :actions nil)
    (sp-local-pair 'cider-repl-mode "`" nil :actions nil))

  (leaf expand-region
    :straight t
    :commands er/expand-region
    :bind ("C-=" . er/expand-region))

  (leaf rainbow-delimiters
    :straight t
    :hook (prog-mode-hook . rainbow-delimiters-mode)
    :config
    (require 'cl-lib)

    (require 'color)
    ;;(global-rainbow-delimiters-mode)
    (cl-loop for index from 1 to rainbow-delimiters-max-face-count
             do
             (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
               (cl-callf color-saturate-name (face-foreground face) 30))))

  (leaf flycheck-pos-tip
    :straight t)

  (leaf flycheck
    :straight t
    :commands flycheck-mode flycheck-add-mode
    :hook (flycheck-mode-hook . flycheck-pos-tip-mode)
    :custom
    (flycheck-disabled-checkers . '(javascript-jshint javascript-jscs))
    (flycheck-display-errors-function . #'flycheck-pos-tip-error-messages)
    :hydra
    (hydra-flycheck nil
                    "
      Navigate Error^^    Miscellaneous
      ---------------------------------------------------
      [_k_] Prev          [_c_] Clear
      [_j_] Next
      [_f_] First Error   [_q_] Quit
      [_l_] Lask Error
      "
                    ("j" flycheck-next-error)
                    ("k" flycheck-previous-error)
                    ("f" flycheck-first-error)
                    ("l" (progn (goto-char (point-max)) (fiycheck-previous-error)))
                    ("c" flycheck-clear)
                    ("q" nil)))

  (leaf whitespace
    ;;
    ;; whitespace ( http://qiita.com/catatsuy/items/55d50d13ebc965e5f31e )
    ;;
    :straight t
    :custom
    `((whitespace-style-with-tab . '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
      (whitespace-style-without-tab . '(face spaces space-mark trailing space-before-tab space-after-tab::space))
      ;; default setting
      (whitespace-style . whitespace-style-with-tab)
      (whitespace-space-regexp . "\\(\x3000+\\)")
      (whitespace-display-mappings . '((space-mark ?\x3000 [?\□])
                                       (tab-mark   ?\t   [?\xBB ?\t]))))
    :config
    ;;
    (defun toggle-tab-mark ()
      (interactive)
      (if (equal whitespace-style whitespace-style-with-tab)
          (setq whitespace-style whitespace-style-without-tab)
        (setq whitespace-style whitespace-style-with-tab)))
    (global-whitespace-mode t)
    (set-face-attribute 'whitespace-trailing nil :foreground "DeepPink" :underline t)
    (set-face-attribute 'whitespace-tab nil :foreground "LightSkyBlue" :underline t)
    (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :weight 'bold))

  (leaf fill-column-indicator
    :straight t
    :hook
    (markdown-mode-hook . fci-mode)
    (git-commit-mode-hook . fci-mode))

  (leaf git-gutter
    :straight t
    :bind
    ;; hydra-git-gutter起動のキーバインド
    ("C-c g" . hydra-git-gutter/body)
    :custom
    (git-gutter:modified-sign . "~")
    (git-gutter:added-sign    . "+")
    (git-gutter:deleted-sign  . "-")
    (git-gutter:window-width  . 0)
    :custom-face
    (git-gutter:modified . '((t (:background "#f1fa8c"))))
    (git-gutter:added    . '((t (:background "#50fa7b"))))
    (git-gutter:deleted  . '((t (:background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1)
    ;; git-gutter:popup-hunkをそのまま割り当てるとdiffウィンドウを閉じれないので
    ;; トグルできる関数を定義
    (defun git-gutter:toggle-popup-hunk ()
      "Toggle git-gutter hunk window."
      (interactive)
      (if (window-live-p (git-gutter:popup-buffer-window))
          (delete-window (git-gutter:popup-buffer-window))
        (git-gutter:popup-hunk)))
    :hydra
    (hydra-git-gutter nil
                      "git hunk"
                      ("p" git-gutter:previous-hunk "previous")
                      ("n" git-gutter:next-hunk "next")
                      ("s" git-gutter:stage-hunk "stage")
                      ("r" git-gutter:revert-hunk "revert")
                      ("SPC" git-gutter:toggle-popup-hunk "toggle diffinfo")))

  (leaf highlight-indent-guides
    :straight t
    :hook
    (yaml-mode-hook . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled . t)
    (highlight-indent-guides-responsive   . t)
    (highlight-indent-guides-method       . 'character)
    (highlight-indent-guides-character    . ?|)
    :custom-face
    (highlight-indent-guides-odd-face       . '((t (:background "darkgray"))))
    (highlight-indent-guides-even-face      . '((t (:background "dimgray"))))
    (highlight-indent-guides-character-face . '((t (:background "dimgray")))))

  (leaf cua-mode
    :custom
    (cua-mode . t)
    (cua-enable-cua-keys . nil))

  (leaf recentf-ext
    :straight t
    :custom
    (recentf-max-saved-items . 200)
    `(recentf-save-file . ,(expand-file-name "~/.emacs.d/recentf"))
    ;; (recentf-auto-cleanup . 10)
    :config
    ;; 最近開いたファイルを保存する数を増やす
    (setq recentf-exclude `("r:/.+$"
                            "s:/.+$"
                            "p:/.+$"
                            ,(concat (expand-file-name "~/") ".emacs.d/elpa/.*$")
                            ,(expand-file-name "~/.emacs.d/recentf")
                            ))
    ;; from http://qiita.com/itiut@github/items/d917eafd6ab255629346
    (defmacro with-suppressed-message (&rest body)
      "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
      (declare (indent 0))
      (let ((message-log-max nil))
        `(with-temp-message (or (current-message) "") ,@body)))
    (setq recentf-auto-save-timer (run-with-idle-timer 120 t '(lambda () (with-suppressed-message (recentf-save-list)))))
    (recentf-mode 1))

  ;; end of minor-mode
  )

(leaf *company-packages
  :config

  (leaf company
    :straight t
    :diminish t
    :bind
    ("C-c y" . company-yasnippet)
    ("C-M-i" . company-complete-common-or-cycle)
    (:company-active-map
     ;; C-n, C-pで補完候補を次/前の候補を選択
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)
     ;; C-sで絞り込む
     ("C-s" . company-filter-candidates)
     ;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
     ("<tab>" . company-complete-common-or-cycle)
     ;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
     ("C-h" . nil)
     ;; ドキュメント表示
     ("M-d" . company-show-doc-buffer))
    (:company-search-map
     ;; C-n, C-pで補完候補を次/前の候補を選択
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))
    (:emacs-lisp-mode-map
     ("C-M-i" . company-complete)) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
    :hook
    (emacs-startup-hook . global-company-mode)
    :custom
    `((company-idle-delay . 0)
      (company-echo-delay . 0)
      (company-minimum-prefix-length . 1) ;; 1文字入力で補完されるように
      (company-selection-wrap-around . t) ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
      (company-tooltip-limit . 20)
      (company-tooltip-align-annotations . t)
      (company-begin-commands . '(self-insert-command))
      ;; (company-box-background . '((t (:inherit company-tooltip :background "midnight blue"))))
      ;; (company-preview . '((t (:foreground "darkgray" :underline t))))
      ;; (company-preview-common . '((t (:inherit company-preview))))
      ;; (company-scrollbar-bg . '((t (:background "gray40"))))
      ;; (company-scrollbar-fg . '((t (:background "orange"))))
      ;; (company-tooltip . '((t (:background "lightgray" :foreground "black"))))
      ;; (company-tooltip-common . '((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
      ;; (company-tooltip-common-selection . '((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
      ;; (company-tooltip-selection . '((t (:background "steelblue" :foreground "black"))))
      ))

  (leaf company-quickhelp
    :straight t
    :custom
    (company-quickhelp-color-foreground . "black")
    :bind (:company-active-map
           :package company
           ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode-hook . company-quickhelp-mode))

  (leaf company-box
    :straight t
    :after all-the-icons
    :hook
    (company-mode-hook . company-box-mode)
    (global-company-mode-hook . company-box-mode)
    :custom
    (company-box-doc-enable . t)
    (company-box-show-single-candidate . t)
    (company-box-max-candidates . 50)
    (company-box-icons-alist . 'company-box-icons-all-the-icons)
    (company-box-background . '((t (:inherit company-tooltip :background "midnight blue"))))

    (company-box-backends-colors . '((company-yasnippet . (:candidate "yellow" :annotation some-face))
                                     (company-elisp . (:icon "yellow" :selected
                                                             (:background "orange" :foreground "black")))
                                     (company-dabbrev . "purple")))
    :config
    ;; great configuration for company-box with all-the-icons
    ;; https://ladicle.com/post/config/#company
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.7  :v-adjust -0.15))
            (Text          . ,(all-the-icons-faicon   "book"                     :height 0.68 :v-adjust -0.15))
            (Method        . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function      . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor   . ,(all-the-icons-faicon   "cube"                     :height 0.7  :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field         . ,(all-the-icons-faicon   "tags"                     :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable      . ,(all-the-icons-faicon   "tag"                      :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class         . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01  :face 'font-lock-constant-face))
            (Interface     . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01))
            (Module        . ,(all-the-icons-octicon  "package"                  :height 0.7  :v-adjust -0.15))
            (Property      . ,(all-the-icons-octicon  "package"                  :height 0.7  :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.7  :v-adjust -0.15))
            (Value         . ,(all-the-icons-material "format_align_right"       :height 0.7  :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum          . ,(all-the-icons-material "storage"                  :height 0.7  :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.7  :v-adjust -0.15))
            (Snippet       . ,(all-the-icons-faicon   "code"                     :height 0.7  :v-adjust 0.02  :face 'font-lock-variable-name-face))
            (Color         . ,(all-the-icons-material "palette"                  :height 0.7  :v-adjust -0.15))
            (File          . ,(all-the-icons-faicon   "file-o"                   :height 0.7  :v-adjust -0.05))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.7  :v-adjust -0.15))
            (Folder        . ,(all-the-icons-octicon  "file-directory"           :height 0.7  :v-adjust -0.05))
            (EnumMember    . ,(all-the-icons-material "format_align_right"       :height 0.7  :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant      . ,(all-the-icons-faicon   "tag"                      :height 0.7  :v-adjust -0.05))
            (Struct        . ,(all-the-icons-faicon   "clone"                    :height 0.65 :v-adjust 0.01  :face 'font-lock-constant-face))
            (Event         . ,(all-the-icons-faicon   "bolt"                     :height 0.7  :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator      . ,(all-the-icons-fileicon "typedoc"                  :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon   "hashtag"                  :height 0.65 :v-adjust 0.07  :face 'font-lock-const-face))
            (Template      . ,(all-the-icons-faicon   "code"                     :height 0.7  :v-adjust 0.02  :face 'font-lock-variable-name-face))))))

(leaf projectile-ripgrep
  :straight t)

(leaf projectile
  :straight t
  :commands projectile-register-project-type projectile-toggle-between-implementation-and-test
  :hook
  (emacs-startup-hook . projectile-mode)
  :bind
  (:projectile-command-map
   ("s" . my:projectile-search-dwim)
   ("<f12>" . projectile-toggle-between-implementation-and-test))
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-idle-timer . nil)
  (projectile-enable-caching . t)
  ;; (projectile-completion-system . 'ivy)
  :preface
  (require 'ripgrep)
  (defun my:projectile-search-dwim (search-term)
    "Merge version to search document via grep/ag/rg.
      Use fast alternative if it exists, fallback grep if no alternatives in system.
      "
    (interactive (list (projectile--read-search-string-with-default
                        "Dwim search for")))
    (cond
     ((and (featurep 'ripgrep) (executable-find "rg")) (projectile-ripgrep search-term))
     ((executable-find "ag") (projectile-ag search-term))
     (t (projectile-grep search-term)))))

(leaf anzu
  :straight t
  :config
  (global-anzu-mode 1))

(leaf open-junk-file
  :straight t
  :bind ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format . "~/Downloads/junk/%Y-%m-%d-%H%M%S."))

(leaf windows-ime
  :if (eq window-system 'w32)
  ;; :after *encoding
  :config
  ;; 日本語入力のための設定
  (set-keyboard-coding-system 'cp932)

  (prefer-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'cp932)

  ;; tr-ime setup
  (tr-ime-advanced-install)

  ;; 標準IMEの設定
  (setq default-input-method "W32-IME")

  ;; IME状態のモードライン表示 (TODO: doom-modeline に細工が必要)
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  ;; IMEの初期化
  (w32-ime-initialize)

  ;; IME 制御 (yes/no などの入力の時に IME を off にする)
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)

  ;; IME OFF時の初期カーソルカラー
  (set-cursor-color "white")
  ;; IME ON/OFF時のカーソルカラー
  (add-hook 'input-method-activate-hook (lambda() (set-cursor-color "green")))
  (add-hook 'input-method-inactivate-hook (lambda() (set-cursor-color "white")))

  ;; バッファ切り替え時にIME状態を引き継ぐ
  (setq w32-ime-buffer-switch-p nil)

  ;; IME on/off key bind
  (global-set-key (kbd "M-`") 'toggle-input-method)

  ;; minibuffer に入った時、IME を OFF にする
  (add-hook 'minibuffer-setup-hook (lambda () (deactivate-input-method)))
  (add-hook 'helm-minibuffer-set-up-hook (lambda () (deactivate-input-method))))

(leaf *frame-setting
  :config
  (leaf frame-setting-mac
    :if (eq system-type 'darwin)
    :config
    (setq initial-frame-alist
          (append
           '((ns-transparent-titlebar . t) ;; タイトルバーを透過
             (vertical-scroll-bars . nil) ;; スクロールバーを消す
             ;; (ns-appearance . dark) ;; 26.1 {light, dark}
             (internal-border-width . 0))))
    (setq default-frame-alist initial-frame-alist))

  (leaf frame-setting-windows
    :if (eq system-type 'windows-nt)
    :config
    (setq initial-frame-alist
          (append
           '((ns-transparent-titlebar . t) ;; タイトルバーを透過
             (vertical-scroll-bars . nil) ;; スクロールバーを消す
             ;; (ns-appearance . dark) ;; 26.1 {light, dark}
             (internal-border-width . 0)
             ;; position
             (top . 40)
             (left . 670)
             (width . 136)
             (height . 50))))
    (setq default-frame-alist initial-frame-alist))

  (leaf frame-setting-common
    :config
    ;; フレームタイトルの設定
    (setq frame-title-format "%b")
    ;; 背景の透明度
    (set-frame-parameter nil 'alpha 95)
    ;; scroll bar を表示しない
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
    ;; 行番号のface
    ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
    (set-face-attribute 'linum nil :height 0.8))

  ;; theme
  (leaf color-theme-sanityinc-tomorrow
    :straight t
    :config
    ;; (load-theme 'pastels-on-dark t)
    ;; (enable-theme 'pastels-on-dark)
    (color-theme-sanityinc-tomorrow-blue)))

(leaf migemo
  :straight t
  :if (executable-find "cmigemo")
  :commands migemo-init
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  ;; (migemo-options . '("-q" "--emacs" "-i" "\g"))
  ;; (migemo-options . '("-q" "--emacs" "-i" "\a"))
  (migemo-dictionary . `(expand-file-name "~/.emacs.d/migemo/utf-8/migemo-dict"))
  ;; (migemo-dictionary . "C~/.emacs.d/migemo-dict/utf-8")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system 'utf-8-unix)
  ;; 遅いのを防ぐためにキャッシュする。
  (migemo-use-pattern-alist . t)
  (migemo-use-frequent-pattern-alist . t)
  (migemo-pattern-alist-length . 1024)
  :config
  (migemo-init))

(leaf buffer
  :config
  ;; 同一バッファ名にディレクトリ付与
  (leaf uniquify
    :straight t
    :custom
    (uniquify-buffer-name-style . 'post-forward-angle-brackets)
    (uniquify-ignore-buffers-re . "*[^*]+*"))
  ;; バッファの先頭までスクロールアップ
  (defadvice scroll-up (around scroll-up-around)
    (interactive)
    (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
      (goto-char (point-max))
      (let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
        ;;(goto-line start_num )
        (goto-char (point-min))
        (forward-line (1- start_num))
        (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
          (if (< (- (- end_num start_num) (window-height)) 0)
              (goto-char (point-max))
            ad-do-it)) )) )
  (ad-activate 'scroll-up)
  ;; バッファの最後までスクロールダウン
  (defadvice scroll-down (around scroll-down-around)
    (interactive)
    (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
      (if (< start_num (window-height))
          (goto-char (point-min))
        ad-do-it) ))
  (ad-activate 'scroll-down))

(leaf backup
  :config
  ;; バックアップファイルを作らない
  (setq bavckup-inhibited t)
  ;; 編集中ファイルのバックアップ
  (setq auto-save-list-file-name nil)

  )

(leaf w32-symlinks
  :if (eq system-type 'windoows-nt)
  :config
  (custom-set-variables '(w32-symlinks-handle-shortcuts t))
  (require 'w32-symlinks)

  (defadvice insert-file-contents-literally
      (before insert-file-contents-literally-before activate)
    (set-buffer-multibyte nil))

  (defadvice minibuffer-complete (before expand-symlinks activate)
    (let ((file (expand-file-name
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
      (when (file-symlink-p file)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (w32-symlinks-parse-symlink file))))))

(leaf cygwin
  :if (eq system-type 'windows-nt)
  :config
  (setq cygwin-mount-cygwin-bin-directory (concat (getenv "CYGWIN_DIR") "\\bin"))
  ;; (require 'setup-cygwin)
  ;; (load "config/builtins/setup-cygwin")
  (file-name-shadow-mode -1))

(leaf global-set-keys
  :bind
  ("C-h" . delete-backward-char)
  ("C-z" . scroll-down)
  ("ESC ?" . apropos)
  ("C-x C-e" . compile)
  ("C-x C-n" . next-error)
  ("C-x C-v" . find-file-other-window)
  ("C-x n" . myblog-hugo/create-draft)
  ;; ("C-x l" . goto-line)
  ("C-x g" . grep)
  ("C-x t" . toggle-truncate-lines)
  ("ESC C-g" . keyboard-quit)
  ("C-x !" . shell-command)
  ("C-x |" . shell-command-on-region)
  ("ESC h" . backward-kill-word)
  ("%" . my:match-paren)
  ("C-x C-;" . my:insert-datetime)
  ("C-x C-M-r" . revert-buffer)
  ([M-kanji] . ignore)  ;; M-kanji is undefined に対する対策
  :init
  (defun my:match-paren (arg)
    "Go to the matching parenthesis if on parenthesis otherwise insert %."
    (interactive "p")
    (cond
     ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
     ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
     (t (self-insert-command (or arg 1)))))
  (defun my:insert-datetime ()
    (interactive)
    (insert (format-time-string "%Y/%m/%d %T"))))

(leaf global-configuration
  :custom
  ;; 起動メッセージの非表示
  (inhibit-startup-message . t)
  ;; スタートアップ時のエコー領域メッセージの非表示
  (inhibit-startup-echo-area-message . -1)
  ;; バッファ画面外文字の切り詰め表示
  (truncate-lines . nil)
  ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
  (truncate-partial-width-windows . t)
  ;; カーソル点滅表示
  (blink-cursor-mode . 0)
  ;; メニューバーを消す
  (menu-bar-mode . nil)
  ;; ツールバーを消す
  (tool-bar-mode . nil)
  ;; スクロール時のカーソル位置の維持
  (scroll-preserve-screen-position . t)
  ;; スクロール行数（一行ごとのスクロール）
  (vertical-centering-font-regexp . ".*")
  (scroll-conservatively . 35)
  (scroll-margin . 0)
  (scroll-step . 1)
  ;; 画面スクロール時の重複行数
  (next-screen-context-lines . 1)
  ;; バッファ中の行番号表示
  (global-linum-mode . t)
  ;; 下線を引く
  (global-hl-line-mode . t)
  ;; 行番号のフォーマット
  (linum-format . "%5d")
  ;; 画像ファイルを表示
  (auto-image-file-mode . t)
  ;; evalした結果を全部表示
  (eval-expression-print-length . nil)
  ;; 対応する括弧を光らせる。
  (show-paren-mode . t)
  ;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
  (show-paren-style . 'mixed)
  ;; startup message を表示しない
  (inhibit-startup-message . t)
  ;; 行の先頭でC-kを一回押すだけで行全体を消去する
  (kill-whole-line . t)
  ;; 最終行に必ず一行挿入する
  ;; (require-final-newline . t)
  ;; バッファの最後でnewlineで新規行を追加するのを禁止する
  (next-line-add-newlines . nil)
  ;; 補完時に大文字小文字を区別しない
  (completion-ignore-case . t)
  (read-file-name-completion-ignore-case . t)
  ;; 履歴数を大きくする
  (history-length . 500)
  ;; ミニバッファの履歴を保存する
  (savehist-mode . t)
  ;; 圧縮
  ;; gzファイルも編集できるようにする
  (auto-compression-mode . t)
  ;; diff
  ;; ediffを1ウィンドウで実行
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  ;; diffのオプション
  (diff-switches . '("-u" "-p" "-N"))
  ;; lock file を作らない
  (create-lockfiles . nil)
  ;; ファイル終端の改行文字を自動入力しない
  ;; https://windymelt.hatenablog.com/entry/2014/09/01/145343
  (require-final-newline . nil)
  (mode-require-final-newline . nil)
  ;;
  (indent-tabs-mode . nil)
  ;; backup 関連
  (auto-save-default . nil)
  ;; 変更ファイルのバックアップ
  (make-backup-files . nil)
  ;; 変更ファイルの番号つきバックアップ
  (version-control . nil)
  (auto-save-list-file-prefix . nil)
  ;; 編集中ファイルのバックアップ先(TODO)
  ;; (auto-save-file-name-transforms . ((".*" ,temporary-file-directory t)))
  ;; 編集中ファイルのバックアップ間隔（秒）
  (auto-save-timeout . 30)
  ;; 編集中ファイルのバックアップ間隔（打鍵）
  (auto-save-interval . 500)
  ;; 終了時にオートセーブファイルを消す
  (delete-auto-save-files . t)
  ;; バックアップ世代数
  (kept-old-versions . 1)
  (kept-new-versions . 2)
  ;; 上書き時の警告表示
  ;; (trim-versions-without-asking . nil)
  ;; 古いバックアップファイルの削除
  (delete-old-versions . t)
  ;;
  :hook
  ;; shebangがあるファイルを保存すると実行権をつける。
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  ;;
  (message-mode-hook . (lambda () (yas-minor-mode)))
  ;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
  (after-save-hook . delete-file-if-no-contents)
  :config
  ;; バッファ名
  ;; ファイル名が重複していたらディレクトリ名を追加する。
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; リージョンの大文字小文字変換を有効にする。
  ;; C-x C-u -> upcase
  ;; C-x C-l -> downcase
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; kill
  ;; Emacs 24からクリップボードだけ使うようになっているので
  ;; Emacs 23のようにprimary selectionを使うように変更
  ;;   * killしたらprimary selectionにだけ入れる（Xの場合のみ）
  ;;   * yankするときはprimary selectionのものを使う
  (setq x-select-enable-primary t)
  (when (eq window-system 'x)
    (setq x-select-enable-clipboard nil))
  ;;
  ;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
  (defun delete-file-if-no-contents ()
    (let ((file (buffer-file-name (current-buffer))))
      (when (= (point-min) (point-max))
        (delete-file file)
        (message (concat "File: " file " deleted."))))))

(leaf dashboard
  :when (version<= "25.1" emacs-version)
  :ensure t
  :custom ((dashboard-items . '((recents . 15)
                                (projects . 5)
                                (bookmarks . 5)
                                ;; (agenda . 5)
                                )))
  :config
  (dashboard-setup-startup-hook))

(leaf *etc
  :config
  (leaf **misc-utility
    :config
    (defun read-file-and-list-each-lines (filename)
      (save-excursion
        (let* ((buffer (find-file-noselect filename))
               (ret nil))
          (set-buffer buffer)
          (goto-char (point-min))
          (while (re-search-forward "^.+$" nil t)
            (setq ret (cons (match-string-no-properties 0) ret)))
          (kill-buffer buffer)
          (reverse ret)))))

  (leaf *count-words-in-buffer
    :bind
    ("C-x =" . count-words-in-buffer)
    :init
    (defun count-words-in-buffer ()
      (interactive)
      (save-excursion
        (count-words--message "Region" (point-min) (point-max)))))

  (leaf goto-line-beginning-or-indent
    ;; http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
    :bind
    ("C-a" . my:goto-line-beginning-or-indent)
    :init
    (defun my:goto-line-beginning-or-indent (&optional $position)
      (interactive)
      (or $position (setq $position (point)))
      (let (($starting-position (progn (back-to-indentation) (point))))
        (if (eq $starting-position $position)
            (move-beginning-of-line 1)))))

  (leaf myblog-hugo
    :config
    (defvar myblog-hugo/base-directory-format-string "~/blog/myblog-hugo/content/post/%Y-%m/%d/"
      "format string for post directory. use this with `format-time-string'")

    (defvar myblog-hugo/draft-directory "~/blog/drafts/"
      "draft directory for myblog-hugo. draft file for markdown, thumbnails")

    (defvar myblog-hugo/draft-template "+++
shortname = \"\"
title = \"\"
description = \"\"
date = \"%Y-%m-%dT%H:%M:%S+09:00\"
categories = [\"Programming\"]
tags = [\"\"]
archives = [\"%Y-%m\"]
url = \"post/%Y-%m/%d/{{shortname}}\"
thumbnail = \"/img/%Y-%m/%d/{{shortname}}.png\"
+++

<!--more-->
"
      "template string for post's default markdown text. use this with `format-time-string', and replace {{post-title}}.")

    (defun myblog-hugo/create-draft ()
      "create a hugo draft file with default template."
      (interactive)
      (let* ((draft-filename (format-time-string "%Y-%m-%d-%H%M%S.md" (current-time)))
             (filename (concat myblog-hugo/draft-directory draft-filename))
             (directory (file-name-directory filename))
             (draft-content myblog-hugo/draft-template)
             (buf (set-buffer (find-file-noselect filename t))))
        (with-current-buffer buf
          (goto-char (point-min))
          (insert draft-content)
          ;; (basic-save-buffer)
          (switch-to-buffer buf)
          (goto-char (point-max)))))

    (defun myblog-hugo/get-shortname ()
      "frontmatter にある shortname を取得する"
      (goto-char (point-min))
      (when (re-search-forward "shortname* = *\"\\(.*\\)\"" nil t)
        (let* ((matched (match-string-no-properties 1)))
          matched)))

    (defun myblog-hugo/apply-current-time (field-name end)
      "frontmatter にある keyword = format の format に現在時刻を適用する。"
      (let* ((left-part (concat "\\(" field-name " *= *\\[?"))
             (right-part (concat "\"" "\\)" "\\(.+\\)" "\\(\"\\]?\\)")))
        (when (re-search-forward (concat left-part right-part) end t)
          (let* ((matched (match-string-no-properties 2))
                 (formatted (format-time-string matched (current-time))))
            (replace-match (concat "\\1" formatted "\\3"))
            (goto-char (point-min))))))

    (defun myblog-hugo/apply-shortname (shortname end)
      "frontmatter に含まれる {{shortname}} を置き換える"
      (goto-char (point-min))
      (while (re-search-forward "{{shortname}}" end t)
        (replace-match shortname)))

    (defun myblog-hugo/publish ()
      "publish current draft buffer to hugo post directory."
      (interactive)
      (let* ((end)
             (post-destdir (format-time-string myblog-hugo/base-directory-format-string (current-time)))
             (shortname (downcase (myblog-hugo/get-shortname)))
             (destfile (concat post-destdir "/" shortname ".md")))
        (goto-char (point-min))
        (re-search-forward "\\+\\+\\+")
        (forward-char)
        (re-search-forward "\\+\\+\\+")
        (forward-char -3)
        (setq end (point))
        ;;
        (goto-char (point-min))
        (myblog-hugo/apply-current-time "date" end)
        (myblog-hugo/apply-current-time "archives" end)
        (myblog-hugo/apply-current-time "url" end)
        (myblog-hugo/apply-current-time "thumbnail" end)
        (myblog-hugo/apply-shortname shortname end)
        ;;
        (unless (file-exists-p post-destdir)
          (make-directory post-destdir t))
        (set-visited-file-name destfile)
        (basic-save-buffer))))

  (leaf google-search
    :config
    ;;
    ;; Google Search via Browser
    ;;
    (require 'browse-url)
    (require 'thingatpt)

    ;; w3m-url-encode-string の rename 版 (w3m.el を入れてないから)
    (defun my-url-encode-string (str &optional coding)
      (apply (function concat)
             (mapcar
              (lambda (ch)
                (cond
                 ((eq ch ?\n)               ; newline
                  "%0D%0A")
                 ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
                  (char-to-string ch))      ; printable
                 ((char-equal ch ?\x20)     ; space
                  "+")
                 (t
                  (format "%%%02X" ch))))   ; escape
              ;; Coerce a string to a list of chars.
              (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                      nil))))

    ;; google で検索。引数無しだと mini-buffer で編集できる。
    (defun google (str &optional flag)
      "google で検索。引数無しだと mini-buffer で編集できる。"
      (interactive
       (list (cond ((or
                     ;; mouse drag の後で呼び出された場合
                     (eq last-command 'mouse-drag-region)
                     ;; region が活性
                     (and transient-mark-mode mark-active)
                     ;; point と mark を入れ替えた後
                     (eq last-command 'exchange-point-and-mark))
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
                   (t (thing-at-point 'word)))
             current-prefix-arg))
      (unless flag
        (setq str (read-from-minibuffer "Search word: " str)))
      (browse-url
       (concat
        "http://www.google.com/search?q="
        (my-url-encode-string str 'shift_jis)
        "&hl=ja&ie=Shift_JIS&lr=lang_ja"))))

  (leaf grep-r
    :config
    ;; 再帰的にgrep
    (require 'grep)

    (when-let (cmd (or (executable-find "yagrep")
                       (executable-find "grep")))
      (setq grep-command-before-query (concat cmd " -nH -r -e ")))

    (defun grep-default-command ()
      (if current-prefix-arg
          (let ((grep-command-before-target
                 (concat grep-command-before-query
                         (shell-quote-argument (grep-tag-default)))))
            (cons (if buffer-file-name
                      (concat grep-command-before-target
                              " *."
                              (file-name-extension buffer-file-name))
                    (concat grep-command-before-target " ."))
                  (+ (length grep-command-before-target) 1)))
        (car grep-command)))
    (setq grep-command (cons (concat grep-command-before-query " .")
                             (+ (length grep-command-before-query) 1)))

    ;; (defadvice grep (around grep-coding-system-setup compile)
    ;;   "When a prefix argument given, specify coding-system-for-read."
    ;;   (let ((coding-system-for-read
    ;;          (if current-prefix-arg
    ;;              (read-coding-system "coding system: ")
    ;;            coding-system-for-read)))
    ;;     ad-do-it))

    ;; (defadvice grep (around grep-coding-system-setup compile)
    ;;   "When a prefix argument given, specify coding-system-for-read."
    ;;   (let ((coding-system-for-read 'utf-8))
    ;;     ad-do-it))
    (defadvice grep (around grep-coding-system-setup compile)
      ""
      (let ((old-default-process-coding-system default-process-coding-system))
        (setq default-process-coding-system '(utf-8 . cp932))
        ad-do-it
        (setq default-process-coding-system old-default-process-coding-system)))
    ;; (ad-activate-regexp "grep-coding-system-setup")
    ;; (ad-deactivate-regexp "grep-coding-system-setup")
    )

  (leaf ripgrep*
    :init
    (defun my:ripgrep-regexp (regexp &optional args)
      "Run a ripgrep search with `REGEXP' rooted at `.'.
`ARGS' provides Ripgrep command line arguments."
      (interactive
       (list (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
      (let ((default-directory (dired-current-directory)))
        (compilation-start
         (mapconcat 'identity
                    (append (list ripgrep-executable)
                            ripgrep-arguments
                            args
                            ripgrep--base-arguments
                            (when ripgrep-highlight-search '("--color=always"))
                            (when (and case-fold-search
                                       (isearch-no-upper-case-p regexp t))
                              '("--ignore-case"))
                            '("--")
                            (list (shell-quote-argument regexp) ".")) " ")
         'ripgrep-search-mode))))
  )
