;;; my-japanese.el --- 日本語環境 (encoding / IME / migemo)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; 日本語環境設定
;;; --------------------------------------------------

;;; [3] 日本語環境設定

(leaf *japanese-env
  :config
  ;; 日本語環境
  (setenv "LANG" "ja_JP.UTF-8")

  ;; Localeに合わせた環境の設定
  (set-locale-environment nil)

  ;; eaw
  (require 'eaw)
  (eaw-fullwidth)
  ;; (leaf eaw
  ;;   :require t
  ;;   (eaw-fullwidth))

  ;; 機種依存文字
  (leaf cp5022x
    ;; site-lisp/cp5022x.el を使う (elpa 版は使われていなかった)
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

;;; [3] encoding設定

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

;;; [3] 日本語入力サポート(Windows)

;; tr-ime / w32-ime はこれまでどの leaf ブロックでも宣言されておらず、
;; elpa/ に残っていた 2020〜2021 年版が package-activate-all によって
;; 暗黙に有効化されるのに依存していた。straight で明示的に導入する。
;; 導入手順 (tr-ime-advanced-install -> default-input-method -> w32-ime-initialize)
;; は現行版でも変わっていない。
(leaf w32-ime
  :if (eq system-type 'windows-nt)
  :straight t)

(leaf tr-ime
  :if (eq system-type 'windows-nt)
  :straight t
  :after w32-ime)

(leaf windows-ime
  :if (eq window-system 'w32)
  ;; :after *encoding
  :config
  ;; 日本語入力のための設定
  (set-keyboard-coding-system 'cp932)

  (prefer-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'cp932)
  (setq default-file-name-coding-system 'cp932)

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
  (add-hook 'input-method-activate-hook (lambda () (set-cursor-color "green")))
  ;; input-method-inactivate-hook は Emacs 24.3 で input-method-deactivate-hook に
  ;; 改名され、Emacs 31 では別名ごと削除されている。そのため add-hook が
  ;; 誰も実行しない変数を作るだけになり、IME を OFF にしてもカーソルが
  ;; 緑のまま白に戻らなかった。
  (add-hook 'input-method-deactivate-hook (lambda () (set-cursor-color "white")))

  ;; バッファ切り替え時にIME状態を引き継ぐ
  (setq w32-ime-buffer-switch-p nil)

  ;; IME on/off key bind
  (global-set-key (kbd "M-`") 'toggle-input-method)

  ;; minibuffer に入った時、IME を OFF にする
  (add-hook 'minibuffer-setup-hook (lambda () (deactivate-input-method)))
  (add-hook 'helm-minibuffer-set-up-hook (lambda () (deactivate-input-method))))

;;; [3] migemo

(leaf migemo
  :straight t
  :if (executable-find "cmigemo")
  :commands migemo-init
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  ;; (migemo-options . '("-q" "--emacs" "-i" "\g"))
  ;; (migemo-options . '("-q" "--emacs" "-i" "\a"))
  `((migemo-dictionary . ,(expand-file-name "migemo/utf-8/migemo-dict" user-emacs-directory)))
  ;; (migemo-dictionary . "C~/.emacs.d/migemo-dict/utf-8")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  ;; 遅いのを防ぐためにキャッシュする。
  (migemo-use-pattern-alist . t)
  (migemo-use-frequent-pattern-alist . t)
  (migemo-pattern-alist-length . 1024)
  :config
  (migemo-init))

(provide 'my-japanese)
;;; my-japanese.el ends here
