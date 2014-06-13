;;; 画像
;; 画像ファイルを表示
(auto-image-file-mode t)


;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)


;;; カーソル
;; カーソルの点滅を止める
(blink-cursor-mode 0)


;;; eval
;; evalした結果を全部表示
(setq eval-expression-print-length nil)


;; 括弧
;; 対応する括弧を光らせる。
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)

;;; modeline に column-number は表示しない
(column-number-mode 0)

;;; modeline に line-number は表示しない
(line-number-mode 0)

;;; modeline に 現在の関数名を表示しない
(which-function-mode 0)

;;; scroll bar を表示しない
(scroll-bar-mode 0)

;;; startup message を表示しない
(setq inhibit-startup-message t)

;;; 行番号の表示
(global-linum-mode t)      ; デフォルトで linum-mode を有効にする
(setq linum-format "%5d ") ; 5 桁分の領域を確保して行番号のあとにスペースを入れる

;;; 選択行のハイライト表示の face
(custom-set-faces '(hl-line ((t (:underline "textBackgroundColor")))))


;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)


;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
;; 2012-08-08
;; Emacs 24ではデフォルトで有効になっていて、`partial-completion-mode'は
;; なくなっている。カスタマイズする場合は以下の変数を変更する。
;;   * `completion-styles'
;;   * `completion-pcm-complete-word-inserts-delimiters'
(if (fboundp 'partial-completion-mode)
    (partial-completion-mode t))
;; 補完可能なものを随時表示
;; 少しうるさい
(icomplete-mode 1)


;;; 履歴
;; 履歴数を大きくする
(setq history-length 500)
;; ミニバッファの履歴を保存する
(savehist-mode 1)
;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 500)


;;; 圧縮
;; gzファイルも編集できるようにする
(auto-compression-mode t)


;;; diff
;; ediffを1ウィンドウで実行
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのオプション
(setq diff-switches '("-u" "-p" "-N"))


;;; バッファ名
;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;; shebangがあるファイルを保存すると実行権をつける。
;; 2012-03-15
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;;; リージョンの大文字小文字変換を有効にする。
;; C-x C-u -> upcase
;; C-x C-l -> downcase
;; 2011-03-09
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;; kill
;; 2012-09-01
;; Emacs 24からクリップボードだけ使うようになっているので
;; Emacs 23のようにprimary selectionを使うように変更
;;   * killしたらprimary selectionにだけ入れる（Xの場合のみ）
;;   * yankするときはprimary selectionのものを使う
(setq x-select-enable-primary t)
(when (eq window-system 'x)
  (setq x-select-enable-clipboard nil))


;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;; M-kanji is undefined に対する対策
(global-set-key [M-kanji] 'ignore)
