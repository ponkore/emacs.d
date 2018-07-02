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

;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入する
;; (setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)


;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
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

;; from http://qiita.com/itiut@github/items/d917eafd6ab255629346
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; 自動保存
(when (require 'recentf-ext nil t)
  ;; 最近開いたファイルを保存する数を増やす
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
  (setq recentf-exclude `("r:/.+$"
                          "s:/.+$"
                          "c:/repo/hgs/10_サブ内環境構築.+$"
                          "c:/repo/hgs/Patches.+$"
                          "c:/repo/hgs/メッセージ申請"
                          "p:/.+$" "c:/repo/hgs/src-arch.+*"
                          ,(concat (expand-file-name "~/") ".emacs.d/elpa/.*$")
                          ,(expand-file-name "~/.emacs.d/recentf")))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 120 t '(lambda () (with-suppressed-message (recentf-save-list)))))
  (recentf-mode 1))


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


(add-hook 'message-mode-hook (lambda () (yas-minor-mode)))


;;; lock file を作らない
(setq create-lockfiles nil)


;;
;; whitespace ( http://qiita.com/catatsuy/items/55d50d13ebc965e5f31e )
;;
(require 'whitespace)

;;(setq whitespace-style '(face tabs tab-mark spaces space-mark lines-tail trailing space-before-tab space-after-tab::space))
(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
;;(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
;;(setq whitespace-style '(face trailing space-before-tab space-after-tab::space))

(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])))

(global-whitespace-mode t)

(set-face-attribute 'whitespace-trailing nil
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :foreground "GreenYellow"
                    :weight 'bold)

(require 'smartparens)
(smartparens-global-mode 1)

;; ファイル終端の改行文字を自動入力しない
;; https://windymelt.hatenablog.com/entry/2014/09/01/145343
(setq-default require-final-newline nil)
(setq mode-require-final-newline nil)

;;
(setq indent-tabs-mode nil)