;;; early-init.el --- init.el より前に読まれる設定 -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs 27 以降、init.el より前に読み込まれるファイル。
;; パッケージシステムの初期化、GC、フレーム、user-lisp/ の扱いなど、
;; init.el では手遅れになる設定だけをここに置く。
;;; Code:

;; ---------------------------------------------------------------
;; package.el を無効化する
;; ---------------------------------------------------------------
;; パッケージ管理は straight.el に一本化しているため、package.el による
;; 自動有効化 (package-activate-all) は不要。
;; elpa/ には 2020〜2021 年で更新の止まった残骸が 19 個あり、有効化されると
;; その autoloads ファイルが lexical-binding cookie 欠落の警告を出していた。
;;
;; 以前は tr-ime / w32-ime だけがこの自動有効化に依存していたが、
;; init.el 側で straight による明示宣言に切り替えたため不要になった。
;; IME が straight 版で動作することを確認済みで、elpa/ ディレクトリは削除済み。
(setq package-enable-at-startup nil)

;; ---------------------------------------------------------------
;; 起動中だけ GC を抑制する
;; ---------------------------------------------------------------
;; 起動処理中に GC が何度も走るのを避け、起動後に通常値へ戻す。
(defvar my:gc-cons-threshold-original gc-cons-threshold)
(defvar my:file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; 起動中はファイル名ハンドラの照合も不要
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (max my:gc-cons-threshold-original
                                         (* 32 1024 1024))
                  gc-cons-percentage 0.1
                  file-name-handler-alist my:file-name-handler-alist-original)))

;; ---------------------------------------------------------------
;; user-lisp/ (Emacs 31.1 の新機能)
;; ---------------------------------------------------------------
;; 既定では package-activate-all の直後、init.el を読む「前」に
;; prepare-user-lisp が走り、user-lisp/ 配下を再帰的にバイトコンパイルして
;; autoload を生成する。
;; しかしその時点では straight.el のブートストラップが済んでおらず leaf マクロが
;; 未定義のため、leaf を使ったモジュールが関数呼び出しとしてコンパイルされ
;; 壊れた .elc ができてしまう。
;; そのため自動実行は切り、init.el 側で straight/leaf を用意した後に
;; (prepare-user-lisp) を明示的に呼ぶ。
;; これらの変数は early-init でしか設定できない。
(setq user-lisp-auto-scrape nil)

;; ---------------------------------------------------------------
;; フレームの初期設定
;; ---------------------------------------------------------------
;; ツールバー等をここで消しておくと、起動時に一瞬表示されてから消える
;; ちらつきを避けられる (init.el の :custom では描画後になる)。
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;;; early-init.el ends here
