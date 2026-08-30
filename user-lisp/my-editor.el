;;; my-editor.el --- エディタ全般の設定  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; エディタ全般設定
;;; --------------------------------------------------

;;; [3] hydra

(leaf hydra :straight t)

;;; [3] symbol-overlay

(leaf symbol-overlay
  :straight t
  :diminish t
  :bind
  ("M-i" . symbol-overlay-put)
  (:symbol-overlay-map
   ("p" . symbol-overlay-jump-prev)
   ("n" . symbol-overlay-jump-next)
   ("C-g" . symbol-overlay-remove-all))
  :hook
  (prog-mode-hook . symbol-overlay-mode)
  (markdown-mode-hook . symbol-overlay-mode))

;;; [3] smartparens

(leaf smartparens
  :straight t
  :diminish t
  :require smartparens-config
  :hook (emacs-startup-hook . smartparens-global-strict-mode))

;;; [3] fill-column-indicator

(leaf fill-column-indicator
  :straight t
  :hook
  (markdown-mode-hook . fci-mode)
  (git-commit-mode-hook . fci-mode))

;;; [3] expand-region

(leaf expand-region
  :straight t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;;; [3] cua-mode

(leaf cua-mode
  :custom
  (cua-mode . t)
  (cua-enable-cua-keys . nil))

;;; [3] recentf-ext

(leaf recentf-ext
  :straight t
  :custom
  (recentf-max-saved-items . 200)
  `(recentf-save-file . ,(expand-file-name "recentf" user-emacs-directory))
  ;; (recentf-auto-cleanup . 10)
  :config
  ;; 最近開いたファイルを保存する数を増やす
  (setq recentf-exclude `("r:/.+$"
                          "s:/.+$"
                          "p:/.+$"
                          ,(concat (regexp-quote (expand-file-name "elpa/" user-emacs-directory)) ".*$")
                          ,(concat (regexp-quote (expand-file-name "straight/" user-emacs-directory)) ".*$")
                          ,(expand-file-name "recentf" user-emacs-directory)
                          ))
  ;; from http://qiita.com/itiut@github/items/d917eafd6ab255629346
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  ;; quote された lambda は非推奨のため #' に変更
  (setq recentf-auto-save-timer
        (run-with-idle-timer 120 t
                             (lambda () (with-suppressed-message (recentf-save-list)))))
  (recentf-mode 1))

;;; [3] highlight-indent-guides

(leaf highlight-indent-guides
  :straight t
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive   . t)
  (highlight-indent-guides-method       . 'fill)
  (highlight-indent-guides-character    . ?|)
  :custom-face
  (highlight-indent-guides-odd-face       . '((t (:background "darkgray"))))
  (highlight-indent-guides-even-face      . '((t (:background "dimgray"))))
  (highlight-indent-guides-character-face . '((t (:background "dimgray")))))

;;; [3] whitespace

(leaf whitespace
  ;;
  ;; whitespace ( http://qiita.com/catatsuy/items/55d50d13ebc965e5f31e )
  ;;
  :straight t
  :diminish t
  :custom
  `((whitespace-style-with-tab . '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
    (whitespace-style-without-tab . '(face spaces space-mark trailing space-before-tab space-after-tab::space))
    ;; default setting
    (whitespace-style . whitespace-style-with-tab)
    (whitespace-space-regexp . "\\(\x3000+\\)")
    (whitespace-display-mappings . '((space-mark ?\x3000 [?\□])
                                     (tab-mark   ?\t   [?\xBB ?\t])))
    (whitespace-global-modes . '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode php-mode))
    (global-whitespace-mode . t))
  :config
  ;;
  (defun toggle-tab-mark ()
    (interactive)
    (if (equal whitespace-style whitespace-style-with-tab)
        (setq whitespace-style whitespace-style-without-tab)
      (setq whitespace-style whitespace-style-with-tab)))
  (set-face-attribute 'whitespace-trailing nil :foreground "DeepPink" :underline nil)
  (set-face-attribute 'whitespace-tab nil :foreground "LightSkyBlue" :underline nil)
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil :background "Black"))

;;; [3] rainbow-delimiters

(leaf rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;; [3] yasnippet

(leaf yasnippet
  :straight t
  :diminish t
  :custom ((yas-indent-line . 'fixed)
           (yas-global-mode . t))
  :bind (:yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)
         ("<C-tab>" . yas-expand)
         ("C-x i i" . yas-insert-snippet)
         ("C-x i n" . yas-new-snippet)
         ("C-x i v" . yas-visit-snippet-file)
         ("C-x i l" . yas-describe-tables)
         ("C-x i g" . yas-reload-all))
  :config
  (leaf yasnippet-snippets
    :straight t)
  (leaf yatemplate
    :straight t
    :config (yatemplate-fill-alist))
  ;; 以前は company-backends に company-yasnippet を混ぜ込んでいたが、
  ;; corfu へ移行したのでやめた。スニペットの補完は my-completion.el の
  ;; yasnippet-capf が capf として供給する。
  )

;;; [3] anzu

(leaf anzu
  :straight t
  :diminish t
  :config
  (global-anzu-mode 1))

;;; [3] 同一バッファ名にディレクトリ付与

;; 同一バッファ名にディレクトリ付与
(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re . "*[^*]+*"))

;;; [3] スクロール設定

(leaf buffer
  ;; TODO: 組み込みの `scroll-error-top-bottom' を t にすれば
  ;; scroll-up-command / scroll-down-command について同じ効果が得られる。
  ;; ただしこの設定は生の scroll-up / scroll-down を対象にしており
  ;; (C-z が scroll-down に直接バインドされている)、挙動が完全には一致しないため
  ;; ここでは既存の動作を保ったまま advice-add へ置き換えるにとどめる。
  :config
  ;; バッファの末尾までスクロールできないときは末尾へ飛ぶ
  (defun my:scroll-up-to-bottom (orig &rest args)
    "残り行数が 1 画面に満たなければ point-max へ移動する。"
    (let ((start-num (1+ (count-lines (point-min) (point)))))
      (goto-char (point-max))
      (let ((end-num (1+ (count-lines (point-min) (point)))))
        (goto-char (point-min))
        (forward-line (1- start-num))
        (if (< (- (- end-num start-num) (window-height)) 0)
            (goto-char (point-max))
          (apply orig args)))))
  (advice-add 'scroll-up :around #'my:scroll-up-to-bottom)
  ;; バッファの先頭までスクロールできないときは先頭へ飛ぶ
  (defun my:scroll-down-to-top (orig &rest args)
    "先頭から 1 画面以内にいるなら point-min へ移動する。"
    (let ((start-num (1+ (count-lines (point-min) (point)))))
      (if (< start-num (window-height))
          (goto-char (point-min))
        (apply orig args))))
  (advice-add 'scroll-down :around #'my:scroll-down-to-top))

;;; [3] バックアップファイルを作らない

(leaf backup
  :config
  ;; バックアップファイルを作らない (bavckup-inhibited のタイポで無効だった)
  ;; auto-save-list-file-name はここにあったが、関連する
  ;; auto-save-list-file-prefix が global-configuraions 側にあり
  ;; 2 ブロックに分かれていた。まとめてそちらへ移した。
  (setq backup-inhibited t))

;;; [3] 保存時バッファ内容が空であればファイルを削除

;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する (after-save-hook に以下の関数を追加)
(defun delete-file-if-no-contents ()
  (let ((file (buffer-file-name (current-buffer))))
    (when (= (point-min) (point-max))
      (delete-file file)
      (message (concat "File: " file " deleted.")))))

;;; [3] autorevert

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom (auto-revert-interval . 1)
  :global-minor-mode global-auto-revert-mode)

;;; [3] editor global configraiton

(leaf global-configuraions
  ;; leaf は :hook / :bind / :mode などがあると :config を
  ;; (eval-after-load '<leaf名>) で包んで遅延させる。この leaf 名は
  ;; 実在する feature ではないため、:config が永久に実行されなかった。
  ;; :leaf-defer nil で遅延を無効化する。
  :leaf-defer nil
  :custom
  ;; 起動メッセージの非表示
  (inhibit-startup-message . t)
  ;; スタートアップ時のエコー領域メッセージの非表示は下の :config で行う。
  ;; (inhibit-startup-echo-area-message . -1) は値が誤りで効いていなかった
  ;; バッファ画面外文字の切り詰め表示
  (truncate-lines . nil)
  ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
  (truncate-partial-width-windows . t)
  ;; カーソル点滅表示
  (blink-cursor-mode . nil)
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
  ;; (global-linum-mode . t)
  (global-display-line-numbers-mode . 1)
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
  ;; (inhibit-startup-message . t) は上で設定済みなので削除した
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
  ;; 水平方向への（賢い）分割をやめる
  ;; (もともと :hook セクションに書かれており、leaf が
  ;;  (add-hook 'split-width-threshold ...) と解釈して値を壊していた)
  (split-width-threshold . nil)
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
  ;; 自動保存リストのファイルを作らない (prefix と name の両方を無効化)
  (auto-save-list-file-prefix . nil)
  (auto-save-list-file-name . nil)
  ;; 編集中ファイルのバックアップ先(TODO)
  ;; `((auto-save-file-name-transforms . ((".*" ,temporary-file-directory t))))
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
  :hook
  ;; shebangがあるファイルを保存すると実行権をつける。
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  ;;
  (message-mode-hook . (lambda () (yas-minor-mode)))
  ;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
  (after-save-hook . delete-file-if-no-contents)
  :config
  ;; リージョンの大文字小文字変換を有効にする。
  ;; C-x C-u -- upcase
  ;; C-x C-l -- downcase
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; 起動時のエコー領域メッセージ
  ;; ("For information about GNU Emacs ...") を出さない。
  ;; この変数は startup.el 側で意図的に抑止しにくくしてあり、
  ;;   - customize で保存した (saved-value プロパティがある) 値がログイン名と一致
  ;;   - もしくは user-init-file に (setq ... "ユーザー名") が literal で書いてある
  ;; のどちらかでないと効かない。leaf の :custom は customize-set-variable
  ;; (theme-value) なので前者を満たさず、値も -1 で誤っていた。
  ;; ユーザー名を直書きしたくないので saved-value を立てて前者を満たす。
  (setq inhibit-startup-echo-area-message (user-login-name))
  (put 'inhibit-startup-echo-area-message 'saved-value
       (list (custom-quote (user-login-name)))))

;;; [3] which-key

(leaf which-key
  :straight t
  :config
  (which-key-mode))

;;; [3] 行頭への移動(C-a)の改善

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

(provide 'my-editor)
;;; my-editor.el ends here
