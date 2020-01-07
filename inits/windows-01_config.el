;; IME on/off key bind
(global-set-key (kbd "M-`") 'toggle-input-method)

;; minibuffer に入った時、IME を OFF にする
(add-hook 'minibuffer-setup-hook
          (lambda () (deactivate-input-method)))
(add-hook 'helm-minibuffer-set-up-hook
          (lambda () (deactivate-input-method)))

;;
;; dired
;;
;; Windows dired quick hack: open any documents with external command.
(defvar open-directory-command "start" "Open a directory with suitable windows/mac command.")
(defvar open-file-command "start" "Open a file with suitable windows/mac command.")
(defun dired-open-external ()
  "Open current line of dired buffer with external (windows) command."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (start-process "dir" nil open-directory-command file)
      (start-process "file" nil open-file-command file))))
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map " " 'dired-open-external)))

;;
;; vc
;;
;; svn log の出力は cp932
(add-hook 'vc-svn-log-view-mode-hook (lambda () (set-buffer-process-coding-system 'cp932 'cp932)))

;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)

(defadvice vc-svn-command (around vc-svn-coding-system-setup compile)
  (let ((old-default-process-coding-system default-process-coding-system))
    (setq default-process-coding-system '(utf-8 . cp932))
    ad-do-it
    (setq default-process-coding-system old-default-process-coding-system)))

(ad-activate-regexp "vc-svn-coding-system-setup")

;;
;; mayu
;;
(require 'mayu-mode)
(add-to-list 'auto-mode-alist '("\\.\\(mayu\\)\\'" . mayu-mode))

;;
;; clojure
;;
;; on Windows, use lein.bat instead of lein shell script.
(setq cider-lein-command "lein.bat")

;;
;; F# 編集用のモード
;;
(setq inferior-fsharp-program "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsi.exe\"")
(setq fsharp-compiler "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsc.exe\"")

(defun inferior-fsharp-mode-hook-fn ()
  ""
  ;; (setq comint-output-filter-functions 'comint-truncate-buffer)
  (set-buffer-process-coding-system 'cp932 'cp932))

(add-hook 'inferior-fsharp-mode-hooks 'inferior-fsharp-mode-hook-fn)

;;
;; sql
;;
;;(modify-coding-system-alist 'file ".*\\.sql$" 'cp932-dos)

;;
;; font
;;
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
  "Set windows emacs japanese fonts."
  (let* ((asciifont "Hackgen") ;; was Meiryoke_Console
         (jpfont "Hackgen") ;; was Meiryoke_Console
         (h (round (* size 10)))
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
    (when (require 'all-the-icons nil t)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-material-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-faicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-octicon-family)) nil 'append)
      (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-wicon-family)) nil 'append))
    (setq face-font-rescale-alist '(("Hackgen" . 1.0))))) ;; was MeiryoKe_Console

(windows-emacs-font-setting 10)
;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
