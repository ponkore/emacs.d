;;; init.el --- Emacs 初期化ファイル -*- lexical-binding: t -*-
;;; Commentary:
;; 個人 Emacs 設定の本体。
;;
;; 以前は my-config/init.org を org-babel-load-file で init.el に展開していたが、
;; Org-mode の恩恵が薄い割にコストが大きいため素の Emacs Lisp に戻した。
;; 展開元は docs/archive-init.org に退避してある（抽出手順は docs/extract.el）。

;;; Code:

;;; ==================================================
;;; configuration
;;; ==================================================

;;; --------------------------------------------------
;;; パッケージ管理
;;; --------------------------------------------------

;;; [3] straight

(eval-when-compile
  (require 'cl-lib))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; (straight-pull-recipe-repositories)

;; org は Emacs 31.1 同梱のもの (9.8.7) を使う。
;; ox-pandoc / org-bullets / org-download などが org に依存しているため、
;; ここで built-in と宣言しておかないと straight が古い org (9.5.1) を
;; 依存解決でビルドして load-path に載せてしまう。
(straight-use-package '(org :type built-in))

;; transient も Emacs 31.1 に同梱されている (0.13.3)。
;; straight 側には 2021 年の 0.3.7 が残っており、magit の .elc が
;; 組み込み transient のマクロで展開された状態でビルドされると
;; 実行時に "Symbol's function definition is void: transient--set-layout"
;; になっていた。組み込みに一本化する。
(straight-use-package '(transient :type built-in))

;;; [3] leaf

(eval-and-compile
  ;; package.el のアーカイブ定義。
  ;; marmalade は 2017 年に停止、orgmode.org/elpa も廃止済みで、
  ;; 残しておくと package-refresh-contents が失敗/遅延するため削除した。
  ;; パッケージ導入自体は straight に一本化しているが、elpa/ 配下に残っている
  ;; 既存パッケージ (tr-ime / w32-ime など) は Emacs 27 以降の
  ;; package-activate-all が init.el より前に有効化するので、その分は残す。
  (customize-set-variable
   'package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("melpa"  . "https://melpa.org/packages/")))
  ;; (package-initialize) は Emacs 27 以降 package-activate-all が
  ;; init.el 読み込み前に実行するため不要。
  ;; (package-refresh-contents) も leaf を straight で入れるので不要。

  ;; leaf 本体と、:straight 等のキーワードを提供する leaf-keywords を導入する。
  ;; leaf-keywords-init を呼ぶまでは :straight キーワードが使えないため、
  ;; ここは straight-use-package を直接呼ぶ。
  ;; 以前は leaf 本体だけ straight、leaf-keywords と hydra 等は :ensure t
  ;; (package.el) という混在で、elpa/ 側に 2020 年の leaf 4.2.7 と
  ;; straight 側の leaf 4.5.5 が同居する版ズレ状態だった。
  (dolist (pkg '(leaf leaf-keywords
                 ;; :hydra :el-get :blackout などを使うためのオプション
                 hydra el-get blackout leaf-tree leaf-convert))
    (straight-use-package pkg))
  (require 'leaf-keywords)
  (leaf-keywords-init))

;;; [3] site-lisp 以下を読み込む

(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))


;;; --------------------------------------------------
;;; user-lisp/ の読み込み
;;; --------------------------------------------------

;; early-init.el で user-lisp-auto-scrape を nil にしてある。
;; 既定では straight のブートストラップ前に prepare-user-lisp が走り、
;; leaf マクロが未定義のままバイトコンパイルされて壊れた .elc ができるため。
;; ここまでで straight と leaf が使える状態になっているので明示的に呼ぶ。
;; user-lisp/ 配下は再帰的にバイトコンパイルされ load-path に追加される。
;; 第 1 引数 JUST-ACTIVATE を t にして、バイトコンパイルと autoload 走査を
;; 行わず load-path への追加だけをさせている。
;;
;; バイトコンパイルすると、パッケージ由来のマクロを leaf の :config で
;; 使っている箇所が壊れる。コンパイル時点では当該パッケージが未ロードで
;; マクロが未定義のため、関数呼び出しとしてコンパイルされてしまうため。
;; 例: doom-modeline-def-segment が関数扱いになり、実行時に引数の
;;     my:buffer-encoding が変数として評価されて void エラーになる。
;; (defhydra や define-clojure-indent なども同じ問題を持つ)
;;
;; 起動時間を実測したところコンパイルの有無で差が無かったため
;; (約 1250ms で同じ)、確実性を取ってコンパイルしない。
;; モジュールは init.el から明示的に require しているので autoload も不要。
(prepare-user-lisp t)

(require 'my-core)   ; 汎用ヘルパと基礎ライブラリ

;;; [3] custom.el

;; Avoid to write `package-selected-packages` in init.el
;; custom.el が無い環境でもエラーにしない (NOERROR を渡す)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)


;; user-lisp/ 配下のモジュールを読み込む。
;; 順序は分割前の init.el の記述順と同じ。
(require 'my-japanese)   ; 日本語環境 (encoding / IME / migemo)
(require 'my-appearance)   ; フォント・フレーム・テーマ・モードライン
(require 'my-completion)   ; 補完 (vertico / consult / company など)
(require 'my-keybind)   ; グローバルキーバインド
(require 'my-editor)   ; エディタ全般の設定
(require 'my-dired)   ; dired と neotree
(require 'my-text)   ; テキストモード (org / markdown / rst / adoc)
(require 'my-lang-lisp)   ; Lisp 系 (Emacs Lisp / Clojure / Common Lisp)
(require 'my-lang-python)   ; Python
(require 'my-lang-web)   ; Web 系 (PHP / JavaScript / TypeScript)
(require 'my-lang-native)   ; ネイティブ系 (Rust / C++ / C#)
(require 'my-lang-misc)   ; その他の言語 (SQL / bat / Swift / Lua / VB)
(require 'my-lsp)   ; LSP と flycheck
(require 'my-fileformat)   ; 特定ファイルフォーマット
(require 'my-project)   ; プロジェクト管理 (projectile)
(require 'my-vc)   ; 構成管理 (magit / git-gutter / SVN)
(require 'my-shell)   ; Shell 関連
(require 'my-utils)   ; ユーティリティ
(require 'my-platform)   ; OS 固有設定 (Windows / macOS)

;;; --------------------------------------------------
;;; end
;;; --------------------------------------------------

(provide 'init)
;;; init.el ends here
