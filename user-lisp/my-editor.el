;;; my-editor.el --- エディタ全般の設定  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; エディタ全般設定
;;; --------------------------------------------------

;;; [3] symbol-overlay

(use-package symbol-overlay
  :straight t
  :diminish
  :bind
  (("M-i" . symbol-overlay-put)
   :map symbol-overlay-map
   ("p" . symbol-overlay-jump-prev)
   ("n" . symbol-overlay-jump-next)
   ("C-g" . symbol-overlay-remove-all))
  :hook
  (prog-mode-hook . symbol-overlay-mode)
  (markdown-mode-hook . symbol-overlay-mode))

;;; [3] smartparens

;; 計画書の F-4。以前は emacs-startup-hook で smartparens-global-strict-mode を
;; 有効にしていた。strict モードは 11 個の編集コマンドを remap する:
;;   delete-backward-char / backward-delete-char / backward-delete-char-untabify
;;                                            -> sp-backward-delete-char
;;   delete-char / delete-forward-char        -> sp-delete-char
;;   kill-line                                -> sp-kill-hybrid-sexp
;;   kill-whole-line                          -> sp-kill-whole-line
;;   kill-region / delete-region              -> sp-kill-region / sp-delete-region
;;   kill-word / backward-kill-word           -> sp-kill-word / sp-backward-kill-word
;; これがグローバルにかかるため、org や markdown や shell のバッファでも
;; C-h / C-d / C-k / C-w / M-d / M-DEL が「括弧の対応を壊す削除を拒否する」版に
;; 置き換わっていた。括弧の釣り合いが意味を持つのは Lisp 系だけなので、
;; ペアの自動挿入 (smartparens-global-mode) は全体に残し、strict は Lisp 系の
;; メジャーモードだけに絞る。
(use-package smartparens
  :straight t
  :diminish
  ;; leaf の :require smartparens-config 相当。smartparens-config は
  ;; 既定のペア定義を入れるもので、smartparens をロードしてから読む。
  :demand t
  :hook
  (emacs-startup-hook . smartparens-global-mode)
  ;; Lisp 系の strict はここに集約する (clojure-mode の指定も my-lang-lisp.el
  ;; から移した)。REPL 側も対象にしないと、以前グローバルで効いていたぶんが
  ;; 落ちてしまうので含める。
  ((emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    inferior-emacs-lisp-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook
    slime-repl-mode-hook)
   . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

;;; [3] fill-column の目印

;; 外部の fill-column-indicator (fci-mode) から組み込みの
;; display-fill-column-indicator-mode (Emacs 27+) へ移行した。
;; fci-mode は縦線をオーバーレイで自前描画するため重く、
;; 他のオーバーレイと干渉することがあった。組み込みは表示エンジン側で
;; 描画する。列は display-fill-column-indicator-column が既定 t なので
;; fill-column に従う (fci-mode と同じ)。
;; markdown-mode では邪魔になるので外した (git-commit は 50/72 桁の目安として残す)。
(use-package display-fill-column-indicator
  :hook
  (git-commit-mode-hook . display-fill-column-indicator-mode))

;;; [3] expand-region

(use-package expand-region
  :straight t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;;; [3] cua-mode

;; 矩形選択のために有効化している (cua-enable-cua-keys nil なので
;; C-x / C-c / C-v が切り取り・コピー・貼り付けに化けることはない)。
;; 組み込みの rectangle-mark-mode (C-x SPC) で置き換える案もあったが、
;; cua-mode 自体が組み込みなので依存は減らず、C-RET の矩形編集は
;; rectangle-mark-mode より高機能なのでこのまま使う。
;;
;; なお cua-mode は cua-enable-cua-keys に関係なく
;; scroll-up-command / scroll-down-command を cua-scroll-up / -down へ
;; リマップする (cua-base.el)。これらは「これ以上スクロールできなければ
;; 端へ移動」する挙動を持つので、スクロール設定の scroll-error-top-bottom と
;; 実質同じ結果になる。
(use-package cua-base
  ;; leaf 名は cua-mode だったが、feature 名は cua-base。
  ;; :custom しか無いので leaf では名前の誤りが表面化していなかった。
  ;; customize-set-variable が custom-autoload 経由で cua-base を
  ;; ロードしてモードを有効にするので :defer t でよい。
  :defer t
  :custom
  (cua-mode t)
  (cua-enable-cua-keys nil))

;;; [3] recentf

;; recentf-ext (2013 年で更新停止) をやめて組み込みの recentf にした。
;; recentf-ext がやっていたのは実質この 2 つだけなので下の :config に
;; 取り込んである (obsolete な cl ライブラリを require していた点も解消)。
;;   - dired のディレクトリを recentf に加える
;;   - 表示中のファイルバッファを最近使ったものとして押し上げる
(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
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

  ;; --- 以下 2 つは recentf-ext から取り込んだもの ---
  ;; dired で開いたディレクトリも履歴に入れる
  (defun my:recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook #'my:recentf-add-dired-directory)

  ;; ウィンドウに出ているファイルを「最近使った」扱いにする。
  ;; recentf は本来ファイルを開いた時点でしか記録しないので、
  ;; 開きっぱなしのバッファに戻ってきても順位が上がらない。
  ;; 元の recentf-ext は add-to-list でフックに積んでいたが add-hook を使う。
  (defun my:recentf-push-buffers-in-frame ()
    (walk-windows
     (lambda (win)
       (when-let* ((file (buffer-local-value 'buffer-file-name (window-buffer win))))
         (recentf-add-file file)))))
  (add-hook 'window-configuration-change-hook #'my:recentf-push-buffers-in-frame)

  (recentf-mode 1))

;;; [3] highlight-indent-guides

(use-package highlight-indent-guides
  :straight t
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive   t)
  (highlight-indent-guides-method       'fill)
  (highlight-indent-guides-character    ?|)
  ;; use-package の :custom-face は face-spec-set (defface spec) を使うため
  ;; テーマの theme-face に負ける。leaf の :custom-face は custom-set-faces
  ;; (user テーマ) でテーマに勝つので、そちらに揃える。
  :init
  (custom-set-faces
   '(highlight-indent-guides-odd-face       ((t (:background "darkgray"))))
   '(highlight-indent-guides-even-face      ((t (:background "dimgray"))))
   '(highlight-indent-guides-character-face ((t (:background "dimgray"))))))

;;; [3] whitespace

(use-package whitespace
  ;;
  ;; whitespace ( http://qiita.com/catatsuy/items/55d50d13ebc965e5f31e )
  ;;
  ;; :straight t は外した (計画書の D-4)。whitespace は Emacs 同梱で、
  ;; straight もレシピを (:type built-in) と解決して clone も build もして
  ;; いなかった (straight/repos, straight/build のどちらにも無い)。
  ;; 実害は無かったが「外部から入れている」という誤った表示になるうえ、
  ;; 起動のたびにレシピ検索が走る。組み込みは無記述にする方針に合わせる。
  :diminish
  :custom
  ((whitespace-style-with-tab '(face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
   (whitespace-style-without-tab '(face spaces space-mark trailing space-before-tab space-after-tab::space))
   ;; default setting
   (whitespace-style whitespace-style-with-tab)
   (whitespace-space-regexp "\\(\x3000+\\)")
   (whitespace-display-mappings '((space-mark ?\x3000 [?\□])
                                    (tab-mark   ?\t   [?\xBB ?\t])))
   (whitespace-global-modes '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode php-mode))
   (global-whitespace-mode t))
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

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;; [3] yasnippet

(use-package yasnippet
  :straight t
  :diminish
  :custom ((yas-indent-line 'fixed)
           (yas-global-mode t))
  :bind (:map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)
         ("<C-tab>" . yas-expand)
         ("C-x i i" . yas-insert-snippet)
         ("C-x i n" . yas-new-snippet)
         ("C-x i v" . yas-visit-snippet-file)
         ("C-x i l" . yas-describe-tables)
         ("C-x i g" . yas-reload-all))
  :config
  (use-package yasnippet-snippets
    :straight t
    :defer t)
  (use-package yatemplate
    :straight t
    :config (yatemplate-fill-alist))
  ;; 以前は company-backends に company-yasnippet を混ぜ込んでいたが、
  ;; corfu へ移行したのでやめた。スニペットの補完は my-completion.el の
  ;; yasnippet-capf が capf として供給する。
  )

;;; [3] anzu

(use-package anzu
  :straight t
  :diminish
  :config
  (global-anzu-mode 1))

;;; [3] 同一バッファ名にディレクトリ付与

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "*[^*]+*"))

;;; [3] スクロール設定

;; バッファの端まで来たときに point を端へ飛ばす挙動は、組み込みの
;; scroll-error-top-bottom で得られる。以前は scroll-up / scroll-down に
;; advice を当てて自前で実装していたが、コア関数への advice を 2 つ
;; 抱えることになるうえ、C-z が生の scroll-down に割り当てられていた。
;;
;; 挙動の違い: 旧実装は「残りが 1 画面未満なら即座に端へ飛ぶ」
;; だったのに対し、組み込みは「まず普通にスクロールし、端に達した状態で
;; もう一度押すと端へ飛ぶ」。Emacs 標準の挙動はこちら。
;; これに伴い my-keybind.el の C-z を scroll-down-command に変えてある
;; (scroll-error-top-bottom は *-command 側にしか効かないため)。
;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :custom
  (scroll-error-top-bottom t))

;;; [3] バックアップファイルを作らない

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :config
  ;; バックアップファイルを作らない (bavckup-inhibited のタイポで無効だった)
  ;; auto-save-list-file-name はここにあったが、関連する
  ;; auto-save-list-file-prefix が別ブロック (下の editor global configuration)
  ;; 側にあり 2 ブロックに分かれていた。まとめてそちらへ移した。
  (setq backup-inhibited t))

;;; [3] 保存時バッファ内容が空であればファイルを削除

;; いちいち消すのも面倒なので、内容が 0 ならファイルごと削除する
;; (after-save-hook に以下の関数を追加)
;;
;; 計画書の F-3。対象は全ファイルのままにしたが、3 点を安全側に倒した。
;;
;;   - 判定を (= (point-min) (point-max)) から (zerop (buffer-size)) に変えた。
;;     前者は narrowing の影響を受けるため、中身のあるファイルでも可視領域が
;;     空になっている状態で保存すると、ファイルごと消えていた。
;;   - delete-by-moving-to-trash は既定が nil で、その場合 delete-file は
;;     ゴミ箱を経由せず完全削除になる。この関数の中だけ t に束縛して
;;     ごみ箱へ送る (Windows では system-move-file-to-trash が使われる)。
;;   - 削除前に y-or-n-p で確認する。
;;   - buffer-file-name の nil ガードを足した。
(defun delete-file-if-no-contents ()
  (let ((file (buffer-file-name)))
    (when (and file
               (zerop (buffer-size))
               (y-or-n-p (format "空です。%s を削除しますか? " file)))
      (let ((delete-by-moving-to-trash t))
        (delete-file file t))
      (message "Deleted (trash): %s" file))))

;;; [3] autorevert

;; revert buffers when files on disk change (leaf の :doc / :tag は
;; use-package に無いのでコメントにした)
(use-package autorevert
  :custom (auto-revert-interval 1)
  ;; leaf の :global-minor-mode 相当
  :config (global-auto-revert-mode 1))

;;; [3] editor global configuration

;; 疑似パッケージなので use-package の名前は emacs にする。
;; 実在しない feature 名にすると :config が with-eval-after-load に包まれて
;; 永久に実行されない (leaf で :leaf-defer nil を付けていたのと同じ理由)。
(use-package emacs
  :custom
  ;; 起動メッセージの非表示
  (inhibit-startup-message t)
  ;; スタートアップ時のエコー領域メッセージの非表示は下の :config で行う。
  ;; (inhibit-startup-echo-area-message . -1) は値が誤りで効いていなかった
  ;; バッファ画面外文字の切り詰め表示
  (truncate-lines nil)
  ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
  (truncate-partial-width-windows t)
  ;; カーソル点滅表示
  (blink-cursor-mode nil)
  ;; メニューバーを消す
  (menu-bar-mode nil)
  ;; ツールバーを消す
  (tool-bar-mode nil)
  ;; スクロール時のカーソル位置の維持
  (scroll-preserve-screen-position t)
  ;; スクロール行数（一行ごとのスクロール）
  (vertical-centering-font-regexp ".*")
  (scroll-conservatively 35)
  (scroll-margin 0)
  (scroll-step 1)
  ;; 画面スクロール時の重複行数
  (next-screen-context-lines 1)
  ;; バッファ中の行番号表示は :hook で行う (下記)。
  ;; global-display-line-numbers-mode だと IBuffer や dired、*Help* など
  ;; 編集しないバッファにまで行番号が出てしまう。
  ;; 旧 linum.el 時代の (global-linum-mode . t) / (linum-format "%5d") は、
  ;; linum.el が Emacs 29 で廃止されて以降きいていないデッド設定なので削除した。
  ;; 桁幅を固定したくなったら display-line-numbers-width (nil = 内容に応じて自動)。
  ;; 下線を引く
  (global-hl-line-mode t)
  ;; 画像ファイルを表示
  (auto-image-file-mode t)
  ;; evalした結果を全部表示
  (eval-expression-print-length nil)
  ;; 対応する括弧を光らせる。
  (show-paren-mode t)
  ;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
  (show-paren-style 'mixed)
  ;; (inhibit-startup-message . t) は上で設定済みなので削除した
  ;; 行の先頭でC-kを一回押すだけで行全体を消去する
  (kill-whole-line t)
  ;; 最終行に必ず一行挿入する
  ;; (require-final-newline . t)
  ;; バッファの最後でnewlineで新規行を追加するのを禁止する
  (next-line-add-newlines nil)
  ;; 補完時に大文字小文字を区別しない
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; 履歴数を大きくする
  (history-length 500)
  ;; ミニバッファの履歴を保存する
  (savehist-mode t)
  ;; 圧縮
  ;; gzファイルも編集できるようにする
  (auto-compression-mode t)
  ;; 水平方向への（賢い）分割をやめる
  ;; (もともと :hook セクションに書かれており、leaf が
  ;;  (add-hook 'split-width-threshold ...) と解釈して値を壊していた)
  (split-width-threshold nil)
  ;; diff
  ;; ediffを1ウィンドウで実行
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのオプション
  (diff-switches '("-u" "-p" "-N"))
  ;; lock file を作らない
  (create-lockfiles nil)
  ;; ファイル終端の改行文字を自動入力しない
  ;; https://windymelt.hatenablog.com/entry/2014/09/01/145343
  (require-final-newline nil)
  (mode-require-final-newline nil)
  ;;
  (indent-tabs-mode nil)
  ;; backup 関連
  (auto-save-default nil)
  ;; 変更ファイルのバックアップ
  (make-backup-files nil)
  ;; 変更ファイルの番号つきバックアップ
  (version-control nil)
  ;; 自動保存リストのファイルを作らない (prefix と name の両方を無効化)
  (auto-save-list-file-prefix nil)
  (auto-save-list-file-name nil)
  ;; 編集中ファイルのバックアップ先(TODO)
  ;; ((auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))
  ;; 編集中ファイルのバックアップ間隔（秒）
  (auto-save-timeout 30)
  ;; 編集中ファイルのバックアップ間隔（打鍵）
  (auto-save-interval 500)
  ;; 終了時にオートセーブファイルを消す
  (delete-auto-save-files t)
  ;; バックアップ世代数
  (kept-old-versions 1)
  (kept-new-versions 2)
  ;; 上書き時の警告表示
  ;; (trim-versions-without-asking . nil)
  ;; 古いバックアップファイルの削除
  (delete-old-versions t)
  :hook
  ;; 行番号は「編集するモード」でだけ表示する。
  ;; prog-mode / text-mode / conf-mode の派生モードが対象になるので、
  ;; special-mode 派生 (IBuffer, dired, *Help*, magit など) には出ない。
  (prog-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode)
  (conf-mode-hook . display-line-numbers-mode)
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

(use-package which-key
  :straight t
  :config
  (which-key-mode))

;;; [3] 行頭への移動(C-a)の改善

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
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
