;;; my-lsp.el --- LSP (eglot) と flycheck  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] LSP (eglot)

;; lsp-mode / lsp-ui から Emacs 31.1 同梱の eglot に移行した。
;; eglot は capf (corfu) / flymake / eldoc という組み込みの仕組みに乗るため、
;; lsp-ui のような独自 UI を持たない分だけ設定が薄くなる。
;; eglot は組み込みなので :straight は付けない。

(leaf eglot
  :custom
  ;; 最後のバッファを閉じたらサーバを落とす
  (eglot-autoshutdown . t)
  ;; イベントログはメモリを食うだけなので無効にする
  (eglot-events-buffer-config . '(:size 0 :format full))
  ;; プロジェクト外のファイルへ飛んだ先でも eglot を効かせる
  (eglot-extend-to-xref . t)
  :bind
  ;; lsp-mode の lsp-keymap-prefix "C-c l" に相当するものを自前で用意する。
  ;; eglot にはプレフィックスキーの仕組みが無い。
  (:eglot-mode-map
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

;;; [3] flycheck-pos-tip

(leaf flycheck-pos-tip
  :straight t)

;;; [3] flycheck

(leaf flycheck
  :straight t
  :commands flycheck-mode flycheck-add-mode
  :hook ((flycheck-mode-hook . flycheck-pos-tip-mode)
         (prog-mode-hook . flycheck-mode))
  :custom
  (flycheck-disabled-checkers . '(javascript-jshint javascript-jscs))
  (flycheck-display-errors-function . #'flycheck-pos-tip-error-messages)
  :config (leaf flycheck-inline
            :straight t
            :hook (flycheck-mode-hook . flycheck-inline-mode))
  :hydra
  (hydra-flycheck nil
                  "
      Navigate Error^^    Miscellaneous
      ---------------------------------------------------
      [_k_] Prev          [_c_] Clear
      [_j_] Next
      [_f_] First Error   [_q_] Quit
      [_l_] Lask Error
      "
                  ("j" flycheck-next-error)
                  ("k" flycheck-previous-error)
                  ("f" flycheck-first-error)
                  ("l" (progn (goto-char (point-max)) (flycheck-previous-error)))
                  ("c" flycheck-clear)
                  ("q" nil)))

(provide 'my-lsp)
;;; my-lsp.el ends here
