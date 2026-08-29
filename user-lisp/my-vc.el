;;; my-vc.el --- 構成管理 (magit / git-gutter / SVN)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; 構成管理
;;; --------------------------------------------------

;;; [3] magit

(leaf magit
  :straight t
  :hook (magit-mode-hook . my:magit-setup-diff)
  ;; magit-status-internal は magit 4.x で削除済み。
  ;; git-commit-mode-hook はコマンドではなく変数なので :commands から外す。
  :commands magit-status-setup-buffer git-commit-mode
  :advice (:filter-args magit-expand-git-file-name magit-expand-git-file-name--msys)
  :config
  (defun magit-expand-git-file-name--msys (args)
    "Handle Msys directory names such as /c/* by changing them to C:/*"
    (let ((filename (car args)))
      (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
        (setq filename (concat (match-string 1 filename) ":/"
                               (match-string 2 filename))))
      (list filename)))
  ;; diff関連の設定
  (defun my:magit-setup-diff ()
    ;; diffを表示しているときに文字単位での変更箇所も強調表示する
    ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
    (setq magit-diff-refine-hunk 'all)
    ;; diff用のfaceを設定する
    (my:diff-mode-setup-faces)))

;;; [3] git-gutter

(leaf git-gutter
  :straight t
  ;; :bind があるため leaf が :config を (eval-after-load 'git-gutter ...) で
  ;; 包む。git-gutter を読み込む他のパッケージが無く、:config の
  ;; (global-git-gutter-mode +1) が一度も実行されていなかった。
  :require t
  :bind
  ;; hydra-git-gutter起動のキーバインド
  ("C-c g" . hydra-git-gutter/body)
  :custom
  (git-gutter:modified-sign . "~")
  (git-gutter:added-sign    . "+")
  (git-gutter:deleted-sign  . "-")
  (git-gutter:window-width  . 0)
  :custom-face
  (git-gutter:modified . '((t (:background "#f1fa8c"))))
  (git-gutter:added    . '((t (:background "#50fa7b"))))
  (git-gutter:deleted  . '((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1)
  ;; git-gutter:popup-hunkをそのまま割り当てるとdiffウィンドウを閉じれないので
  ;; トグルできる関数を定義
  (defun git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter hunk window."
    (interactive)
    (if (window-live-p (git-gutter:popup-buffer-window))
        (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))
  :hydra
  (hydra-git-gutter nil
                    "git hunk"
                    ("p" git-gutter:previous-hunk "previous")
                    ("n" git-gutter:next-hunk "next")
                    ("s" git-gutter:stage-hunk "stage")
                    ("r" git-gutter:revert-hunk "revert")
                    ("SPC" git-gutter:toggle-popup-hunk "toggle diffinfo")))

;;; [3] Windows 環境でのSVN support

(leaf vc-windows
  ;; leaf は :hook / :bind / :mode などがあると :config を
  ;; (eval-after-load '<leaf名>) で包んで遅延させる。この leaf 名は
  ;; 実在する feature ではないため、:config が永久に実行されなかった。
  ;; :leaf-defer nil で遅延を無効化する。
  :leaf-defer nil
  :if (eq system-type 'windows-nt)
  :hook
  ;; svn log の出力は cp932
  (vc-svn-log-view-mode-hook . (lambda () (set-process-coding-system 'cp932 'cp932)))
  :config
  ;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
  ;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)
  (defun my:vc-svn-command-with-cp932 (orig &rest args)
    (let ((default-process-coding-system '(utf-8 . cp932)))
      (apply orig args)))
  (advice-add 'vc-svn-command :around #'my:vc-svn-command-with-cp932))

(provide 'my-vc)
;;; my-vc.el ends here
