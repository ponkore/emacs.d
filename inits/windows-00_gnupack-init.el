; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ------------------------------------------------------------------------
;; @ coding system

   ;; 日本語入力のための設定
   (set-keyboard-coding-system 'cp932)

   (prefer-coding-system 'utf-8-dos)
   (set-file-name-coding-system 'cp932)
;;   (setq default-process-coding-system '(cp932 . cp932))
   (setq default-process-coding-system '(utf-8 . utf-8))

;; ------------------------------------------------------------------------
;; @ ime

   ;; 標準IMEの設定
   (setq default-input-method "W32-IME")

   ;; IME状態のモードライン表示
   (setq-default w32-ime-mode-line-state-indicator "[Aa]")
   (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

   ;; IMEの初期化
   (w32-ime-initialize)

   ;; IME OFF時の初期カーソルカラー
   ;; (set-cursor-color "red")

   ;; IME ON/OFF時のカーソルカラー
   ;; (add-hook 'input-method-activate-hook
   ;;           (lambda() (set-cursor-color "green")))
   ;; (add-hook 'input-method-inactivate-hook
   ;;           (lambda() (set-cursor-color "red")))

   ;; バッファ切り替え時にIME状態を引き継ぐ
   (setq w32-ime-buffer-switch-p nil)

;; ------------------------------------------------------------------------
;; @ encode

   ;; PuTTY 用の terminal-coding-system の設定
   (apply 'define-coding-system 'utf-8-for-putty
      "UTF-8 (translate jis to cp932)"
      :encode-translation-table
      (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
      (coding-system-plist 'utf-8))
   (set-terminal-coding-system 'utf-8-for-putty)

   ;; emacs-w3m
   (eval-after-load "w3m"
     '(when (coding-system-p 'cp51932)
        (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

   ;; Gnus
   (eval-after-load "mm-util"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

   ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
   (eval-after-load "mcs-20"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mime-charset-coding-system-alist
             '(iso-2022-jp . cp50220))))

;; ------------------------------------------------------------------------
;; @ frame

   ;; フレームタイトルの設定
   (setq frame-title-format "%b")

;; ------------------------------------------------------------------------
;; @ buffer

   ;; バッファ画面外文字の切り詰め表示
   (setq truncate-lines nil)

   ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
   (setq truncate-partial-width-windows t)

   ;; 同一バッファ名にディレクトリ付与
   (require 'uniquify)
   (setq uniquify-buffer-name-style 'forward)
   (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
   (setq uniquify-ignore-buffers-re "*[^*]+*")

;; ------------------------------------------------------------------------
;; @ fringe

   ;; バッファ中の行番号表示
   (global-linum-mode t)

   ;; 行番号のフォーマット
   ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
   (set-face-attribute 'linum nil :height 0.8)
   (setq linum-format "%4d")

;; ------------------------------------------------------------------------
;; @ modeline

   ;; 行番号の表示
;   (line-number-mode t)

   ;; 列番号の表示
;   (column-number-mode t)

   ;; 時刻の表示
   (when (eq system-type 'windows-nt)
     (require 'time)
     (setq display-time-24hr-format t)
     (setq display-time-string-forms '(24-hours ":" minutes))
     (display-time-mode t))

;; ------------------------------------------------------------------------
;; @ cursor

   ;; カーソル点滅表示
   (blink-cursor-mode 0)

   ;; スクロール時のカーソル位置の維持
   (setq scroll-preserve-screen-position t)

   ;; スクロール行数（一行ごとのスクロール）
   (setq vertical-centering-font-regexp ".*")
   (setq scroll-conservatively 35)
   (setq scroll-margin 0)
   (setq scroll-step 1)

   ;; 画面スクロール時の重複行数
   (setq next-screen-context-lines 1)

;; ------------------------------------------------------------------------
;; @ default setting

   ;; 起動メッセージの非表示
   (setq inhibit-startup-message t)

   ;; スタートアップ時のエコー領域メッセージの非表示
   (setq inhibit-startup-echo-area-message -1)

;; ------------------------------------------------------------------------
;; @ backup

   ;; 変更ファイルのバックアップ
   (setq make-backup-files nil)

   ;; 変更ファイルの番号つきバックアップ
   (setq version-control nil)

   ;; 編集中ファイルのバックアップ
   (setq auto-save-list-file-name nil)
   (setq auto-save-list-file-prefix nil)

   ;; 編集中ファイルのバックアップ先
   (setq auto-save-file-name-transforms
         `((".*" ,temporary-file-directory t)))

   ;; 編集中ファイルのバックアップ間隔（秒）
   (setq auto-save-timeout 30)

   ;; 編集中ファイルのバックアップ間隔（打鍵）
   (setq auto-save-interval 500)

   ;; バックアップ世代数
   (setq kept-old-versions 1)
   (setq kept-new-versions 2)

   ;; 上書き時の警告表示
   ;; (setq trim-versions-without-asking nil)

   ;; 古いバックアップファイルの削除
   (setq delete-old-versions t)

;; ------------------------------------------------------------------------
;; @ scroll

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
   (ad-activate 'scroll-down)

;; ------------------------------------------------------------------------
;; @ print

   (setq ps-print-color-p t
         ps-lpr-command "gswin32c.exe"
         ps-multibyte-buffer 'non-latin-printer
         ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
         printer-name nil
         ps-printer-name nil
         ps-printer-name-option nil
         ps-print-header nil          ; ヘッダの非表示
         )

;; ------------------------------------------------------------------------
;; @ setup-cygwin
   (setq cygwin-mount-cygwin-bin-directory
         (concat (getenv "CYGWIN_DIR") "\\bin"))
  ;(require 'setup-cygwin)
  ;(load "config/builtins/setup-cygwin")
   (file-name-shadow-mode -1)

;; ------------------------------------------------------------------------
;; @ shell
   (require 'shell)
   (setq explicit-shell-file-name "bash.exe")
   (setq shell-command-switch "-c")
   (setq shell-file-name "bash.exe")

   ;; (M-! and M-| and compile.el)
   (setq shell-file-name "bash.exe")
   (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)

   ;; shellモードの時の^M抑制
   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

   ;; shell-modeでの補完 (for drive letter)
   (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

   ;; エスケープシーケンス処理の設定
   (autoload 'ansi-color-for-comint-mode-on "ansi-color"
             "Set `ansi-color-for-comint-mode' to t." t)

   (add-hook 'shell-mode-hook (lambda ()
                                ;; シェルモードの入出力文字コード(cp932 -> utf-8)
                                (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
                                (set-buffer-file-coding-system    'utf-8-unix)
                                ))

;; ------------------------------------------------------------------------
;; @ w32-symlinks

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
         (insert (w32-symlinks-parse-symlink file)))))
