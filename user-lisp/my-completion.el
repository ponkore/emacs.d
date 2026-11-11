;;; my-completion.el --- 補完 (vertico / consult / corfu など)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 補完
;;; --------------------------------------------------

;;; [3] marginalia

(use-package marginalia
  :straight t
  :defer t)

;;; [3] vertico

(use-package vertico
  ;; 以前は :ensure t (package.el) で入れた上に :config でも
  ;; straight-use-package を呼んでおり、同じパッケージを二重に導入していた。
  ;; extensions 込みのレシピを :straight で最初から宣言する形に統一する。
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :custom
  (vertico-mode t)
  (vertico-cycle t)
  ;; 補完候補を最大20行まで表示する
  (vertico-count 20)
  :hook
  (emacs-startup-hook . vertico-after-init-hook)
  :commands vertico-previous vertico-next
  :bind
  (:map vertico-map
   ("C-r" . vertico-previous) ;; C-s/C-rで行を移動できるようにする
   ("C-s" . vertico-next)
   ("C-z" . vertico-scroll-down)
   ("C-v" . vertico-scroll-up))
  ;; leaf の :advice は init 時にインライン展開される (eval-after-load に
  ;; 包まれない) ので :init に置く。
  :init
  (advice-add 'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (setq cand (funcall orig cand prefix suffix index start))
                (concat
                 (if (= vertico--index index)
                     (propertize " " 'face 'vertico-current) ;; "» "
                   "   ")
                 cand)))
  :config
  (defun vertico-after-init-hook ()
    (marginalia-mode))
  ;; C-l の define-key ("dirty hack" とコメントされていた) は削除した。
  ;; vertico-directory 側の :bind が効くようになったので不要。
  )

(use-package vertico-directory
  :straight t
  ;; :after vertico + :bind だけだと、キー割り当ての実体化が
  ;; vertico-directory のロード待ちになる。ところがロードの契機は
  ;; そのキーしか無いので、永久に有効にならない。実際 RET / DEL / M-DEL は
  ;; 未割り当てのままで、:hook の rfn-eshadow-update-overlay も
  ;; :config の file-name-shadow-mode も走っていなかった
  ;; (vertico 側の :config にあった C-l の define-key は、これに気づいた
  ;;  誰かが「dirty hack」として足したものと思われる)。
  ;; 明示的にロードする。
  :demand t
  :after vertico
  :commands
  vertico-directory-delete-char
  vertico-directory-enter
  vertico-directory-delete-word
  vertico-directory-tidy
  :bind
  (:map vertico-map
   ("C-l" . vertico-directory-delete-char)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (file-name-shadow-properties '(invisible t intangible t))
  :config
  (file-name-shadow-mode +1))

;;; [3] consult

(use-package consult
  :straight t
  :bind
  (("C-s" . my:consult-line)
   ;; consult は自分ではキーを割り当てない (README が勧める割り当て例があるだけ)。
   ;; isearch の中の M-s l は、isearch の検索文字列を初期入力にして
   ;; consult-line に移る (consult-line が `isearch-mode' を見て拾う)。
   ;; `isearch-mode-map' に置かないと、isearch が先に終わってから呼ばれるので拾えない
   ("M-s l" . consult-line)
   :map isearch-mode-map
   ("M-s l" . consult-line)
   :map global-map
   ("C-x C-r" . my:consult-recent-file-or-bookmark)
   ("C-x l" . consult-goto-line)
   ("C-x b" . consult-buffer))
  :custom
  ;; consult-preview-raw-size は廃止され、consult-preview-partial-size に
  ;; なった (大きいファイルを部分的にプレビューする閾値)。
  ;; 現行の既定値は 1MB で、ここで設定していた 1024000 とほぼ同じ。
  (consult-preview-partial-size 1024000)
  (consult-narrow-key "<")
  ;; C-x b を「今のタブ (フレーム) で表示したバッファ」だけに絞る。
  ;; 既定は #'buffer-list で全バッファが混ざる。`consult--frame-buffer-list'
  ;; はフレームパラメータの `buffer-list' / `buried-buffer-list' を返すもので、
  ;; tab-bar がこれをタブごとに退避・復元する (wc-bl / wc-bbl) ため、
  ;; 何もしなくてもそのままタブ単位になる。
  ;;
  ;; 他のタブのバッファは `consult-source-other-buffer' から引ける。
  ;; `consult-narrow-key' が `<' なので `< o'。このソースは :enabled が
  ;; 「`consult-buffer-list-function' が #'buffer-list 以外か」なので、
  ;; **この 1 行だけで自動的に有効になる**。
  ;;
  ;; 【重要】**単独では効かない。** `tab-bar-new-tab-to' は buffer-list に
  ;; 一切触らないので、新しいタブは前のタブのバッファをそのまま引き継ぐ。
  ;; my-editor.el の `my:tab-bar-clear-buffer-list' と 2 つで 1 組。
  ;;
  ;; なお C-x C-r (`my:consult-recent-file-or-bookmark') は recentf を
  ;; そのまま出すので、こちらの影響を受けない (下のコメント参照)。
  ;; ファイルを開く入口はタブで絞らない、という切り分けにしてある。
  (consult-buffer-list-function #'consult--frame-buffer-list)
  :config
  ;; 設定は `this-command' で引かれるので、C-s (my:consult-line) と
  ;; M-s l (consult-line) の両方に付ける
  (consult-customize consult-line my:consult-line :keymap my:consult-line-map)
  :init
  ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
  (defun my:consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))

  ;; ---- consult-line の中の C-w (isearch の C-w と同じ動き) ----
  ;;
  ;; C-w で、呼び出したバッファのカーソル位置から次の単語の終わりまでを入力に足す。
  ;; 続けて押すと 1 単語ずつ伸びる。その直後の DEL は、最後に足した単語をまるごと
  ;; 消して 1 つ前に戻る (それ以外の DEL はふつうの 1 文字削除)。
  ;;
  ;; 何か打ってから C-w したときは、今の一致の終わりから続ける。プレビューが
  ;; カーソルを一致の先頭に置く (`consult-point-placement' の既定 match-beginning)
  ;; ので、そこに入力の文字数を足したところ。**一致が入力そのものである
  ;; (部分一致) ことが前提**で、orderless のように空白で区切ると位置がずれる。

  (defvar my:consult-line--origin nil
    "consult-line を呼んだときのカーソル位置 (入力が空のときの C-w の起点)。
プレビューがカーソルを動かすので、呼ぶ前に覚えておく。")

  (defvar my:consult-line--next nil
    "続けて C-w したときに、次の単語を取り始める位置。")

  (defvar my:consult-line--yanks nil
    "C-w で足した分の取り消し用。(足す前の入力の長さ . 足す前の `my:consult-line--next') の列。")

  (defun my:consult-line--remember-origin (&rest _)
    (setq my:consult-line--origin (point)))
  (advice-add 'consult-line :before #'my:consult-line--remember-origin)

  (defun my:consult-line-yank-word ()
    "呼び出したバッファから次の単語を入力に足す (isearch の C-w)。"
    (interactive)
    (let* ((input (minibuffer-contents-no-properties))
           (win (minibuffer-selected-window))
           (buf (window-buffer win))
           ;; C-w か、C-w を取り消した DEL の直後なら続きから
           (chain (and (memq last-command '(my:consult-line-yank-word my:consult-line-delete))
                       my:consult-line--yanks
                       my:consult-line--next)))
      (unless chain
        (setq my:consult-line--yanks nil))
      (let* ((base (cond (chain my:consult-line--next)
                         ((string-empty-p input) (or my:consult-line--origin (window-point win)))
                         (t (+ (window-point win) (length input)))))
             (end (with-current-buffer buf
                    (save-excursion
                      (goto-char (min base (point-max)))
                      (forward-word 1)
                      (point)))))
        (when (> end base)
          (push (cons (length input) my:consult-line--next) my:consult-line--yanks)
          (goto-char (point-max))
          (insert (with-current-buffer buf (buffer-substring-no-properties base end)))
          (setq my:consult-line--next end)))))

  (defun my:consult-line-delete ()
    "C-w の直後なら、最後に足した単語を消す。それ以外は 1 文字削除。"
    (interactive)
    (if (and (memq last-command '(my:consult-line-yank-word my:consult-line-delete))
             my:consult-line--yanks)
        ;; 全部取り消したら (列が空になったら)、次の C-w は今の一致から数え直す
        (pcase-let ((`(,len . ,next) (pop my:consult-line--yanks)))
          (delete-region (+ (minibuffer-prompt-end) len) (point-max))
          (setq my:consult-line--next next))
      (call-interactively #'delete-backward-char)))

  (defvar-keymap my:consult-line-map
    :doc "consult-line のミニバッファだけで効くキー。"
    "C-w" #'my:consult-line-yank-word
    "DEL" #'my:consult-line-delete)

  (defvar my:consult--recent-file-or-bookmark-history nil
    "`my:consult-recent-file-or-bookmark' のミニバッファ履歴。")

  (defvar my:consult--source-recent-file
    (list :name     "Recent File"
          :narrow   ?f
          :category 'file
          :face     'consult-file
          :history  'file-name-history
          :state    #'consult--file-state
          :enabled  (lambda () recentf-mode)
          :items    (lambda ()
                      (mapcar #'consult--fast-abbreviate-file-name
                              (bound-and-true-p recentf-list))))
    "`my:consult-recent-file-or-bookmark' 用の recentf ソース。

**`consult-source-bookmark' と違って組み込みのものを使わない。**
`consult-source-recent-file' は `consult--buffer-file-hash' で
**既に開いているファイルを一覧から除外する**。`consult-buffer' では
それらが Buffer ソースの側に出るので正しいが、こちらは recentf が
すべてなので、除外すると開いているファイルが選べなくなる。
`consult-recent-file' と同じく `recentf-list' をそのまま出す。

`:state' は `consult--file-state' (`consult--define-state' が作る)。
プレビューと、確定時の `consult--file-action' を兼ねる。開いてある
ファイルを選んだときはそのバッファがそのまま使われる。")

  (defun my:consult-recent-file-or-bookmark ()
    "最近開いたファイルとブックマークから選ぶ。

`consult-recent-file' の代わり。`consult--multi' で 2 つのソースを
束ねてあるので、`consult-narrow-key' (`<') に続けて `f' でファイル、
`m' でブックマークだけに絞れる。

`consult-buffer' でも同じ 2 つは選べるが、あちらはバッファが混ざる。
ファイルを開く入口としてはこちらを使う。"
    (interactive)
    ;; `consult--multi' は autoload されていないので明示的に読む。
    (require 'consult)
    (consult--multi '(my:consult--source-recent-file consult-source-bookmark)
                    :prompt "Recent file / bookmark: "
                    :require-match t
                    :sort nil
                    :history 'my:consult--recent-file-or-bookmark-history)))

;;; [3] embark

(use-package embark
  :straight t
  :disabled t
  :after consult
  :bind (("C-S-a" . embark-act)))

(use-package embark-consult
  :straight t
  :defer t)

;;; [3] 補完スタイル

;; orderless はやめた (2026-11-11)。空白を AND 区切りにする絞り込みが手に
;; なじまなかったのと、consult-line の C-w (上) が「一致 = 入力そのもの」を
;; 前提にしているため。
;;
;; substring を先頭に置く。consult-line の候補は行なので、入力が行の
;; **どこかに**含まれれば残ってほしい。basic (先頭一致) を先に置くと、
;; 先頭が一致する行が 1 つでもあればそこで決まり、ほかの行が落ちる。
;; partial-completion は `f-b' で foo-bar を引くためのもの。
(use-package emacs
  :custom
  (completion-styles '(substring partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; [3] corfu

;; company / company-quickhelp / company-box から corfu に移行した。
;; corfu は completion-at-point-functions (capf) をそのまま使うので、
;; company の backend という独自機構が要らない。追加の補完源は cape が
;; capf として供給する。company-box が持っていた all-the-icons の
;; アイコン定義も不要になった (nerd-icons-corfu が出す)。

(use-package corfu
  :straight (corfu :files (:defaults "extensions/corfu-*.el")
                   :includes (corfu-popupinfo corfu-history corfu-info corfu-quick))
  ;; :custom で (global-corfu-mode t) と書いても、パッケージが未ロードだと
  ;; customize-set-variable は変数に t を代入するだけでモード関数を呼ばない。
  ;; corfu を引っぱってくる他のパッケージも無いので、明示的にロードして
  ;; :config で有効化する。
  :demand t
  :custom
  (;; 候補の一番下で次に進むと一番上に戻る (旧 company-selection-wrap-around)
   (corfu-cycle t)
   ;; 自動で補完を開始する (旧 company-idle-delay / -minimum-prefix-length)
   (corfu-auto t)
   (corfu-auto-delay 0.5)
   (corfu-auto-prefix 1)
   ;; 旧 company-tooltip-limit
   (corfu-count 20)
   ;; 何も選択していない状態から始める。RET の扱いは下記 :preface 参照
   (corfu-preselect 'prompt)
   (corfu-on-exact-match nil))
  :preface
  ;; http://misohena.jp/blog/2021-08-08-emacs-company-mode-settings.html
  ;; 無選択状態の時に RET が入力されたら、そのバッファのモード本来の RET を
  ;; 実行する。company 時代の my-company-complete-respecting-user-input と
  ;; 同じ意図。corfu--index は内部変数だが選択状態を知る手段が他に無い。
  ;; -1 が「プロンプト (= 入力そのもの) を選択中」を意味する。
  (defun my:corfu-insert-respecting-user-input ()
    "候補を選んでいなければ、そのモード本来のキー割り当てを実行する。"
    (interactive)
    (if (>= (or (bound-and-true-p corfu--index) -1) 0)
        (call-interactively #'corfu-insert)
      (corfu-quit)
      (setq unread-command-events
            (append (listify-key-sequence (this-command-keys)) nil))))
  :bind
  (:map corfu-map
   ;; C-n, C-p で候補を上下する (旧 company-active-map と同じ)
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ;; C-h はグローバルで delete-backward-char に割り当てているので外す
   ("C-h" . nil)
   ("RET" . my:corfu-insert-respecting-user-input)
   ;; ドキュメント表示 (旧 company-show-doc-buffer / company-quickhelp)
   ("M-d" . corfu-popupinfo-toggle)
   ("M-h" . corfu-popupinfo-toggle)
   ;; 定義位置の表示 (旧 company-show-location)
   ("M-." . corfu-info-location))
  :hook
  ;; ドキュメントをポップアップで出す (旧 company-quickhelp / company-box-doc)
  (corfu-mode-hook . corfu-popupinfo-mode)
  :config
  (global-corfu-mode +1)
  ;; 選択履歴で候補を並べ替える (旧 company-sort-by-occurrence に相当)
  (corfu-history-mode +1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; [4] cape

;; company の backend に相当する補完源を capf として供給する。

(use-package cape
  :straight t
  ;; leaf は (require) を出さず :config をインラインで実行していた。
  ;; cape-file / cape-dabbrev は autoload なのでフックに積むだけならロードは
  ;; 要らない。:defer t + :init で同じにする。
  :defer t
  :init
  ;; cape-dabbrev-min-length は削除した。**いまの cape にこの変数は無い**
  ;; (cape.el を min-length で grep して 0 件)。customize-set-variable は
  ;; defcustom でない名前にも set-default するので、警告も出ないまま同名の
  ;; 変数が 1 つ増えるだけだった (custom-type が nil で確認)。CLAUDE.md の
  ;; mark-holidays-in-calendar と同じ罠。
  ;; 実効の最小接頭辞は corfu-auto-prefix (1) だけで決まる。

  ;; メジャーモード固有の capf が先に来るよう、深さを指定して末尾側に置く。
  (add-hook 'completion-at-point-functions #'cape-file 90)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 91))

;;; [4] nerd-icons-corfu

(use-package nerd-icons-corfu
  :straight t
  :after (corfu nerd-icons)
  ;; leaf は :after のときも (require) を出さないが、use-package は :after の
  ;; 条件が満たされると require する。そのままだと起動時に nerd-icons-corfu が
  ;; ロードされてしまう。nerd-icons-corfu-formatter は autoload なので、corfu が
  ;; 初めて候補を出すときに読めば足りる。:defer t + :init で leaf に揃える。
  :defer t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; [4] yasnippet-capf

;; 旧 company-yasnippet。my-editor.el で company-backends を書き換えて
;; スニペットを混ぜていたのをやめ、capf として供給する。

(use-package yasnippet-capf
  :straight t
  :after yasnippet
  :bind ("C-c y" . my:complete-yasnippet)
  :preface
  (defun my:complete-yasnippet ()
    "スニペットだけを対象に補完する (旧 company-yasnippet)。"
    (interactive)
    (cape-interactive #'yasnippet-capf))
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf 92))

(provide 'my-completion)
;;; my-completion.el ends here
