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
   ("C-x C-r" . consult-recent-file)
   ("C-x l" . consult-goto-line)
   ("C-x b" . consult-buffer))
  :custom
  ;; consult-preview-raw-size は廃止され、consult-preview-partial-size に
  ;; なった (大きいファイルを部分的にプレビューする閾値)。
  ;; 現行の既定値は 1MB で、ここで設定していた 1024000 とほぼ同じ。
  (consult-preview-partial-size 1024000)
  (consult-narrow-key "<")
  :init
  ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
  (defun my:consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line))))

;;; [3] embark

(use-package embark
  :straight t
  :disabled t
  :after consult
  :bind (("C-S-a" . embark-act)))

(use-package embark-consult
  :straight t
  :defer t)

;;; [3] orderless

(use-package orderless
  :straight t
  :defer t
  :custom
  ;; 補完スタイルにorderlessを利用する
  ;; orderless 単体だとファイル名補完や capf の一部が期待どおりに動かない
  ;; ため、フォールバックとして basic を残す (orderless 公式の推奨)。
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-prefixes
                               orderless-regexp
                               orderless-initialism
                               orderless-literal)))

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
  :custom
  (cape-dabbrev-min-length 2)
  :init
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
