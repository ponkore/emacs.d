;;; my-lsp.el --- LSP と flycheck  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] LSP

(leaf lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (php-mode-hook . lsp-deferred)
  (sh-mode-hook . lsp)
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :commands lsp lsp-deferred lsp-enable-which-key-integration)

(leaf lsp-ui
  :straight t
  :commands lsp-ui-mode)

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
