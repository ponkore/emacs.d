;;; my-completion.el --- 補完 (vertico / consult / corfu など)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; 補完
;;; --------------------------------------------------

;;; [3] marginalia

(leaf marginalia
  :straight t)

;;; [3] vertico

(leaf vertico
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
  (vertico-mode . t)
  (vertico-cycle . t)
  ;; 補完候補を最大20行まで表示する
  (vertico-count . 20)
  :hook
  (emacs-startup-hook . vertico-after-init-hook)
  :commands vertico-previous vertico-next
  :bind
  (:vertico-map
   ("C-r" . vertico-previous) ;; C-s/C-rで行を移動できるようにする
   ("C-s" . vertico-next)
   ("C-z" . vertico-scroll-down)
   ("C-v" . vertico-scroll-up))
  :advice
  (:around vertico--format-candidate
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
  ;; dirty hack...
  (define-key vertico-map (kbd "C-l") 'vertico-directory-delete-char))

(leaf vertico-directory
  :straight t
  :after vertico
  :commands
  vertico-directory-delete-char
  vertico-directory-enter
  vertico-directory-delete-word
  vertico-directory-tidy
  :bind
  (:vertico-map
   ("C-l" . vertico-directory-delete-char)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  `(file-name-shadow-properties . '(invisible t intangible t))
  :config
  (file-name-shadow-mode +1))

;;; [3] consult

(leaf consult
  :straight t
  :bind
  (("C-s" . my:consult-line)
   ("C-x C-r" . consult-recent-file)
   ("C-x l" . consult-goto-line)
   ("C-x b" . consult-buffer))
  :custom
  `((consult-preview-raw-size . 1024000)
    (consult-narrow-key . "<"))
  :init
  ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
  (defun my:consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line))))

;;; [3] embark

(leaf embark
  :straight t
  :disabled t
  :after consult
  :bind (("C-S-a" . embark-act)))

(leaf embark-consult
  :straight t)

;;; [3] orderless

(leaf orderless
  :straight t
  :custom
  ;; 補完スタイルにorderlessを利用する
  `(;; orderless 単体だとファイル名補完や capf の一部が期待どおりに動かない
    ;; ため、フォールバックとして basic を残す (orderless 公式の推奨)。
    (completion-styles . '(orderless basic))
    (completion-category-overrides . '((file (styles basic partial-completion))))
    (orderless-matching-styles . '(orderless-prefixes
                                   orderless-regexp
                                   orderless-initialism
                                   orderless-literal))))

;;; [3] corfu

;; company / company-quickhelp / company-box から corfu に移行した。
;; corfu は completion-at-point-functions (capf) をそのまま使うので、
;; company の backend という独自機構が要らない。追加の補完源は cape が
;; capf として供給する。company-box が持っていた all-the-icons の
;; アイコン定義も不要になった (nerd-icons-corfu が出す)。

(leaf corfu
  :straight (corfu :files (:defaults "extensions/corfu-*.el")
                   :includes (corfu-popupinfo corfu-history corfu-info corfu-quick))
  ;; :custom で (global-corfu-mode . t) と書いても、パッケージが未ロードだと
  ;; customize-set-variable は変数に t を代入するだけでモード関数を呼ばない。
  ;; corfu を引っぱってくる他のパッケージも無いので、明示的にロードして
  ;; :config で有効化する。
  :require t
  :custom
  (;; 候補の一番下で次に進むと一番上に戻る (旧 company-selection-wrap-around)
   (corfu-cycle . t)
   ;; 自動で補完を開始する (旧 company-idle-delay / -minimum-prefix-length)
   (corfu-auto . t)
   (corfu-auto-delay . 0.5)
   (corfu-auto-prefix . 1)
   ;; 旧 company-tooltip-limit
   (corfu-count . 20)
   ;; 何も選択していない状態から始める。RET の扱いは下記 :preface 参照
   (corfu-preselect . 'prompt)
   (corfu-on-exact-match . nil))
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
  (:corfu-map
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

(leaf cape
  :straight t
  :custom
  (cape-dabbrev-min-length . 2)
  :config
  ;; メジャーモード固有の capf が先に来るよう、深さを指定して末尾側に置く。
  (add-hook 'completion-at-point-functions #'cape-file 90)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 91))

;;; [4] nerd-icons-corfu

(leaf nerd-icons-corfu
  :straight t
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; [4] yasnippet-capf

;; 旧 company-yasnippet。my-editor.el で company-backends を書き換えて
;; スニペットを混ぜていたのをやめ、capf として供給する。

(leaf yasnippet-capf
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
