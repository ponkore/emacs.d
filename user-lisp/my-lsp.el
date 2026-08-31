;;; my-lsp.el --- LSP (eglot) と flymake  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; [3] LSP (eglot)

;; lsp-mode / lsp-ui から Emacs 31.1 同梱の eglot に移行した。
;; eglot は capf (corfu) / flymake / eldoc という組み込みの仕組みに乗るため、
;; lsp-ui のような独自 UI を持たない分だけ設定が薄くなる。
;; eglot は組み込みなので :straight は付けない。

(use-package eglot
  :custom
  ;; 最後のバッファを閉じたらサーバを落とす
  (eglot-autoshutdown t)
  ;; イベントログはメモリを食うだけなので無効にする
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; プロジェクト外のファイルへ飛んだ先でも eglot を効かせる
  (eglot-extend-to-xref t)
  :bind
  ;; lsp-mode の lsp-keymap-prefix "C-c l" に相当するものを自前で用意する。
  ;; eglot にはプレフィックスキーの仕組みが無い。
  (:map eglot-mode-map
   ("C-c l r" . eglot-rename)
   ("C-c l a" . eglot-code-actions)
   ("C-c l f" . eglot-format)
   ("C-c l d" . eldoc-doc-buffer)
   ("C-c l h" . eglot-inlay-hints-mode)
   ("C-c l R" . eglot-reconnect)
   ("C-c l q" . eglot-shutdown))
  :hook
  (sh-mode-hook . eglot-ensure)
  :config
  ;; PHP は intelephense を使う。eglot の既定は phpactor なので差し替える。
  (add-to-list 'eglot-server-programs
               '((php-mode phps-mode php-ts-mode) . ("intelephense" "--stdio")))
  ;; Swift。eglot には既定のエントリが無い。sourcekit-lsp は macOS (Xcode) 付属で、
  ;; PATH に無いことが多いので Xcode の中を直接見に行く。
  (add-to-list 'eglot-server-programs
               `((swift-mode)
                 . (,(or (executable-find "sourcekit-lsp")
                         (and (eq system-type 'darwin)
                              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
                         "sourcekit-lsp")))))

;;; [3] flymake

;; flycheck / flycheck-pos-tip / flycheck-inline から組み込みの flymake に
;; 移行した。eglot が診断を flymake 経由で出すため、LSP と構文チェックを
;; 二重に持たなくて済む。
;; エラーの行末表示は Emacs 30 で入った
;; flymake-show-diagnostics-at-end-of-line が flycheck-inline の代わりになる。

(use-package flymake
  :custom
  ;; 旧 flycheck-check-syntax-automatically '(idle-change) 相当
  (flymake-no-changes-timeout 1.0)
  (flymake-fringe-indicator-position 'right-fringe)
  ;; 旧 flycheck-inline / flycheck-pos-tip 相当
  (flymake-show-diagnostics-at-end-of-line 'short)
  :hook
  (prog-mode-hook . flymake-mode)
  :bind
  ;; flycheck の慣例だった C-c ! をそのまま使う。
  ;; なお旧設定の hydra-flycheck はどこにも割り当てられておらず、
  ;; 6 年間呼び出す手段が無かった。今回は C-c ! h に割り当てる。
  (:map flymake-mode-map
   ("C-c ! n" . flymake-goto-next-error)
   ("C-c ! p" . flymake-goto-prev-error)
   ("C-c ! l" . flymake-show-buffer-diagnostics)
   ("C-c ! P" . flymake-show-project-diagnostics)
   ("C-c ! h" . hydra-flymake/body))
  ;; leaf の :hydra は init 時にインライン展開される (eval-after-load に
  ;; 包まれない)。use-package では :init に置いて同じ挙動にする。
  ;; :config にすると flymake がロードされるまで hydra-flymake/body が
  ;; 定義されず、C-c ! h が効かなくなる。
  :init
  (defhydra hydra-flymake nil
    "
      Navigate Error^^    Miscellaneous
      ---------------------------------------------------
      [_k_] Prev          [_l_] Buffer diagnostics
      [_j_] Next          [_L_] Project diagnostics
      [_f_] First Error   [_q_] Quit
      [_e_] Last Error
      "
    ("j" flymake-goto-next-error)
    ("k" flymake-goto-prev-error)
    ("f" (progn (goto-char (point-min)) (flymake-goto-next-error)))
    ("e" (progn (goto-char (point-max)) (flymake-goto-prev-error)))
    ("l" flymake-show-buffer-diagnostics :exit t)
    ("L" flymake-show-project-diagnostics :exit t)
    ("q" nil)))

(provide 'my-lsp)
;;; my-lsp.el ends here
