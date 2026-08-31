;;; my-vc.el --- 構成管理 (magit / diff-hl / SVN)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 構成管理
;;; --------------------------------------------------

;;; [3] magit

(use-package magit
  :straight t
  :hook (magit-mode-hook . my:magit-setup-diff)
  ;; magit-status-internal は magit 4.x で削除済み。
  ;; git-commit-mode-hook はコマンドではなく変数なので :commands から外す。
  :commands magit-status-setup-buffer git-commit-mode
  ;; leaf の :advice は init 時にインライン展開される (eval-after-load に
  ;; 包まれない) ので :init に置く。advice-add は対象が未定義でも登録でき、
  ;; magit のロード時に有効になる。
  :init
  (advice-add 'magit-expand-git-file-name :filter-args
              #'magit-expand-git-file-name--msys)
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

;;; [3] diff-hl (差分表示)

;; git-gutter と dired-k を diff-hl に統一した。
;;   git-gutter -> バッファの fringe に差分マーカー
;;   dired-k    -> dired の VC 状態マーカー
;; どちらも diff-hl が担う。diff-hl は vc 経由なので git / svn / hg を
;; 同じ仕組みで扱える (git-gutter の git-gutter:handled-backends は既定 '(git)
;; で、このリポジトリでは SVN 対応も入れているのに効いていなかった)。
;;
;; なお dired-k が持っていたファイルサイズ・更新日時の色分けは diff-hl には
;; 無いので失われる。VC 状態の表示だけになる。
;;
;; git-gutter は行頭に "~ + -" の文字を出していたが、diff-hl は fringe に
;; ビットマップを描く。色は face の foreground で決まるので、
;; git-gutter で背景色に使っていた色をそのまま foreground に移した。

(use-package diff-hl
  :straight t
  ;; :bind / :hook があると use-package が :config を eval-after-load で包む。
  ;; diff-hl を読み込む他のパッケージが無いので明示的にロードする
  ;; (leaf では :require t だったもの)。
  :demand t
  :bind
  ;; hydra 起動のキーバインド (git-gutter 時代と同じ C-c g)
  ("C-c g" . hydra-diff-hl/body)
  :custom
  ;; 保存前のバッファでも差分を追う (git-gutter と同じ感覚にする)
  (diff-hl-flydiff-delay 0.5)
  ;; use-package の :custom-face は face-spec-set (defface spec) を使うため
  ;; modus-vivendi の theme-face に負けて色が反映されない。leaf の
  ;; :custom-face は custom-set-faces (user テーマ) で、こちらはテーマに勝つ。
  ;; 同じ挙動にするため custom-set-faces を直接呼ぶ。
  :init
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "#50fa7b"))))
   '(diff-hl-change ((t (:foreground "#f1fa8c"))))
   '(diff-hl-delete ((t (:foreground "#ff79c6")))))
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  ;; magit の操作後にマーカーを更新する
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  ;; leaf の :hydra 相当 (init 時にインライン展開される)。
  :init
  (defhydra hydra-diff-hl nil
                 "diff hunk"
    ("p" diff-hl-previous-hunk "previous")
    ("n" diff-hl-next-hunk "next")
    ("s" diff-hl-stage-dwim "stage")
    ("r" diff-hl-revert-hunk "revert")
    ("SPC" diff-hl-show-hunk "show diff")))

;;; [3] Windows 環境でのSVN support

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
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
