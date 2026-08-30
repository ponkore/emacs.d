;;; my-japanese.el --- 日本語環境 (encoding / IME / migemo)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
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

  ;; East Asian Ambiguous 幅 (site-lisp/eaw.el)
  ;;
  ;; Emacs 31 は ambiguous-width-chars という文字テーブルを持ち、
  ;; cjk-ambiguous-chars-are-wide が t なら use-cjk-char-width-table が
  ;; それを幅 2 にする。上の set-locale-environment で日本語環境になると
  ;; setup-japanese-environment-internal 経由で自動的に適用される。
  ;; つまり組み込みだけでもある程度は効く。
  ;;
  ;; ただし HackGen での実測では、まだ eaw を入れる価値がある:
  ;;   eaw が挙げる ambiguous 文字            3666
  ;;   組み込みだけで幅 2 になるもの          2170
  ;;   eaw が追加で幅 2 にするもの            1496
  ;; その 1496 文字を実際に描画して幅を測ると
  ;;   16px (全角) 335 …… eaw が正しい
  ;;    8px (半角)  63 …… 組み込みが正しい
  ;;   それ以外   1098 …… 絵文字・麻雀牌など。プロポーショナルな
  ;;                       フォールバックで描かれるため、char-width を
  ;;                       どちらにしても桁は揃わない
  ;; 桁揃えが成立する 398 文字のうち 84% で eaw のほうが実描画と一致する。
  ;; ○△□★※①→≒ のような日常的な記号は組み込みでも幅 2 になるので、
  ;; 差が出るのは記号類が中心。
  ;;
  ;; なお eaw を外したいときは組み込みの cjk-ambiguous-chars-are-wide が
  ;; 対応する設定項目になる。
  (require 'eaw)
  (eaw-fullwidth)

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
;; 2026-08 に確認したところ、tr-ime は 0.5.0 (2022-06)、w32-ime は 2020-11 の
;; コミットが、それぞれ upstream の最新だった。より新しい版は存在しない。
;; 導入手順 (tr-ime-advanced-install -> default-input-method -> w32-ime-initialize)
;; も README の推奨どおりで、変更の必要はない。
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

  ;; IME 状態のモードライン表示
  ;;
  ;; w32-ime-mode-line-state-indicator(-list) は w32-ime が自前で
  ;; mode-line-format の先頭に差し込むための変数で、mode-line-format を
  ;; まるごと差し替える doom-modeline とは併用できない (差し込みが消える)。
  ;; doom-modeline の input-method セグメントは current-input-method-title を
  ;; 見ており、w32-ime はそこに w32-ime-input-method-title を代入する。
  ;; この変数は既定が nil なので、何も設定しないとモードラインに何も出ない。
  ;; ここを設定するのが doom-modeline 側に出す正しい方法。
  (setq w32-ime-input-method-title "[あ]")

  ;; IMEの初期化
  (w32-ime-initialize)

  ;; IME 制御 (yes/no などの入力の時に IME を off にする)
  ;;
  ;; wrap-function-to-control-ime は 2020 年に
  ;; w32-ime-wrap-function-to-control-ime へ改名され、旧名は
  ;; define-obsolete-function-alias で残っているだけ (警告が出る)。
  ;; 第 2 引数以降も現行版ではダミーで、渡しても何の効果もない
  ;; (中身は単なる advice-add になっている)。
  (dolist (fn '(universal-argument
                read-string
                read-char
                read-from-minibuffer
                y-or-n-p
                yes-or-no-p
                map-y-or-n-p))
    (w32-ime-wrap-function-to-control-ime fn))

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

  ;; IME の on/off は C-\ (toggle-input-method) と 漢字キーで行う。
  ;; ここには (global-set-key (kbd "M-`") 'toggle-input-method) があったが、
  ;; my-keybind.el が後から M-` を ignore に割り当てるため死んでいた。
  ;; M-` (= Alt + 半角/全角) と M-kanji を ignore にしているのは意図的で、
  ;; その組み合わせは tr-ime / Windows 側が IME のトグルとして処理するため、
  ;; Emacs 側では何もしないのが正しい。

  ;; minibuffer に入った時、IME を OFF にする
  ;; helm-minibuffer-set-up-hook にも同じものを足していたが、helm は
  ;; 導入していないので何も起きないデッドコードだった。
  (add-hook 'minibuffer-setup-hook (lambda () (deactivate-input-method))))

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
