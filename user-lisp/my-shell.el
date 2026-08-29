;;; my-shell.el --- Shell 関連  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; Shell
;;; --------------------------------------------------

;;; [3] exec-path-from-shell

(leaf exec-path-from-shell
  :straight t
  ;; macOS 限定だったが、Linux でも GUI 起動時はログインシェルの
  ;; 環境変数を引き継がないため必要になる。
  :if (memq system-type '(darwin gnu/linux berkeley-unix))
  :config
  (exec-path-from-shell-initialize))

;;; [3] Windows環境用 Shell

(leaf shell-windows
  ;; leaf は :hook / :bind / :mode などがあると :config を
  ;; (eval-after-load '<leaf名>) で包んで遅延させる。この leaf 名は
  ;; 実在する feature ではないため、:config が永久に実行されなかった。
  ;; :leaf-defer nil で遅延を無効化する。
  :leaf-defer nil
  :if (eq system-type 'windows-nt)
  :hook
  (shell-mode-hook . (lambda ()
                       ;; シェルモードの入出力文字コード(cp932 -> utf-8)
                       ;; (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
                       ;; (set-buffer-file-coding-system    'utf-8-unix)
                       (set-process-coding-system 'cp932 'cp932)
                       (set-buffer-file-coding-system    'cp932)))
  :config
  ;; ユーザー名を直書きしていたので USERPROFILE 基準にする
  (let ((shims (expand-file-name "scoop/shims" (getenv "USERPROFILE"))))
    (when (file-directory-p shims)
      (add-to-list 'exec-path shims)))
  (require 'shell)
  ;; ここは上記の理由で今まで一度も実行されていなかった。有効になると
  ;; shell-file-name が bash になり M-! / M-x compile / M-x grep の
  ;; 実行シェルが変わるため、bash.exe が実在するときだけ設定する。
  ;; (見つからない場合は Emacs 既定の cmdproxy のままにする)
  (when-let* ((bash (executable-find "bash.exe")))
    (setq explicit-shell-file-name bash)
    (setq shell-command-switch "-c")
    (setq shell-file-name bash))
  ;; (M-! and M-| and compile.el)
  (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
  ;; エスケープシーケンス処理の設定
  (autoload 'ansi-color-for-comint-mode-on "ansi-color"
    "Set `ansi-color-for-comint-mode' to t." t))

;;; [3] Shell

(leaf shell
  :hook
  ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
  (shell-mode-hook . (lambda ()
                       (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
  ;; or shellモードの時の^M抑制 (どっちが正しい？)
  ;; (comint-output-filter-functions . shell-strip-ctrl-m nil t)
  :config
  ;; shell-modeでの補完 (for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-"))

(provide 'my-shell)
;;; my-shell.el ends here
