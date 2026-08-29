;;; my-platform.el --- OS 固有設定 (Windows / macOS)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
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

;;; [3] cygwin

(leaf cygwin
  :if (eq system-type 'windows-nt)
  :config
  (setq cygwin-mount-cygwin-bin-directory (concat (getenv "CYGWIN_DIR") "\\bin"))
  ;; (require 'setup-cygwin)
  ;; (load "config/builtins/setup-cygwin")
  (file-name-shadow-mode -1))

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
