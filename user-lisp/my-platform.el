;;; my-platform.el --- OS 固有設定 (Windows / macOS)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; Windows環境固有
;;; --------------------------------------------------

;;; [3] 環境設定

(when (eq system-type 'windows-nt)
  (setq w32-get-true-file-attributes nil)
  (setenv "HOME" (getenv "USERPROFILE")))

;;; [3] w32-symlinks

(leaf w32-symlinks
  ;; TODO: :if が 'windoows-nt というタイポだったため、このブロックは
  ;; 一度も実行されたことがない。タイポは直したが、中身が
  ;; insert-file-contents-literally と minibuffer-complete への
  ;; グローバル advice であり、無検証で有効化するのは危険なので
  ;; 明示的に無効化しておく。必要になったら :disabled t を外して検証すること。
  ;; あわせて custom-set-variables (custom.el を汚す) は setopt へ、
  ;; defadvice は advice-add へ書き換えが必要。
  :disabled t
  :if (eq system-type 'windows-nt)
  :config
  (custom-set-variables '(w32-symlinks-handle-shortcuts t))
  (require 'w32-symlinks)

  (defadvice insert-file-contents-literally
      (before insert-file-contents-literally-before activate)
    (set-buffer-multibyte nil))

  (defadvice minibuffer-complete (before expand-symlinks activate)
    (let ((file (expand-file-name
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
      (when (file-symlink-p file)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (w32-symlinks-parse-symlink file))))))

;;; [3] cygwin (削除済み)

;; cygwin ブロックは完全に死んでいたので削除した:
;;   - CYGWIN_DIR が未設定で、cygwin-mount-cygwin-bin-directory は
;;     バックスラッシュ + bin という壊れた値になっていた
;;   - cygwin-mount パッケージは導入されていない (関数も未定義)
;;   - (require 'setup-cygwin) と load はコメントアウト済みで、
;;     setup-cygwin というファイル自体が存在しない
;;   - Cygwin 自体が入っていない (シェルは Git 付属の bash)
;;
;; 唯一生きていたのが (file-name-shadow-mode -1) で、これが
;; my-completion.el の vertico-directory 側の (file-name-shadow-mode +1) を
;; 打ち消していた (my-platform は init.el の最後に読まれるため後勝ち)。
;; 削除により、ミニバッファでファイル名の重複部分が隠れるようになる。

;;; --------------------------------------------------
;;; Mac環境固有
;;; --------------------------------------------------

;;; [3] modifier

(leaf *modifier
  :config
  (leaf *modifier-macos
    :if (eq system-type 'darwin)
    :config
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta)))

(provide 'my-platform)
;;; my-platform.el ends here
