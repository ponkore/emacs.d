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

;;; [3] w32-symlinks (削除済み)

;; site-lisp/w32-symlinks.el (874 行, 2002〜2005 年, EmacsWiki 由来) と
;; その設定ブロックを削除した。Windows の .lnk を magic file name handler で
;; シンボリックリンクのように扱うもので、ヘッダには「NTEmacs 21 と一緒に
;; 使うことを意図している」と書かれていた。
;;
;; 削除の理由:
;;   - :if が 'windoows-nt というタイポで 6 年間一度も実行されていなかった
;;   - 前提が失われている。当時「Emacs に無い」とされていた dired-do-symlink は
;;     Emacs 31 に組み込みで存在し、Windows にも本物の NTFS symlink がある
;;   - 実測すると 2005 年のパーサは現代の .lnk を取りこぼす。
;;     実在の 8 件で試して 1 件 (Brother Utilities.lnk) が
;;     c:/Users/Program Files (x86)/... という存在しないパスを返した
;;   - 動かすには insert-file-contents-literally へのグローバル advice が必須
;;     だった。一時バッファが multibyte だと署名判定に失敗するためだが、
;;     この advice は呼ばれるたびにカレントバッファを unibyte にしてしまう
;;   - minibuffer-complete へのグローバル advice も持っており、vertico を
;;     使っている現在では冗長かつ干渉の恐れがある
;;   - このマシンの .lnk は Downloads/old-desktop に 23 件あるだけで、
;;     21 件が exe、2 件がフォルダ。Emacs から辿りたいものではない

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

;; leaf では *modifier / *modifier-macos の二重の疑似パッケージだったが、
;; 外側は :config を持つだけで何もしていなかったので 1 つにまとめた。
(use-package emacs
  :if (eq system-type 'darwin)
  :config
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(provide 'my-platform)
;;; my-platform.el ends here
