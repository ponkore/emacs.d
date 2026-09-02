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

;;; [3] package.el のアーカイブ定義

;; パッケージ導入は straight に一本化しており、package.el 自体は
;; early-init.el で無効化してある (package-enable-at-startup nil)。
;; ここの定義は M-x list-packages などを手で使うときのためだけに残す。
;; marmalade は 2017 年に停止、orgmode.org/elpa も廃止済みなので外してある。
(customize-set-variable
 'package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                     ("melpa"  . "https://melpa.org/packages/")))

;;; [3] use-package

;; 設定の記述は use-package で行う。2026-08 に leaf から移行した
;; (leaf は 3 コミット/年まで開発が細っており、use-package は Emacs 29 以降
;;  本体に同梱されている。31.1 では lisp/use-package/)。
;; 同梱なので straight で入れる必要はない。
;;
;; パッケージ導入は :straight t を明示する。このキーワードは
;; straight-use-package-mode が use-package-keywords に追加する
;; (straight-use-package-version の既定値が 'straight のため)。
;; :ensure は素の package.el (無効化済み) に流れてしまうので使わない。
;; 組み込みパッケージは無記述のままでよい。
;;
;; leaf との差異でとくに効きやすいものは CLAUDE.md にまとめてある。要点:
;;   - 遅延キーワード (:commands :bind :hook :mode :after など) が 1 つも
;;     無いブロックは (require) が出る。leaf は出さないので :defer t を足す
;;   - 疑似パッケージの名前には emacs を使う。実在しない feature 名だと
;;     :config が with-eval-after-load に包まれて永久に実行されない
;;   - :custom-face は使わない。テーマに負けるので custom-set-faces を直接呼ぶ
;;   - require できないもの (modus-themes) には :no-require t が要る
(require 'use-package)

;; :hook にフック変数名そのものを書けるようにする。
;; 既定値 "-hook" のままだと (foo-mode-hook . f) が foo-mode-hook-hook に
;; なってしまう。移行元の leaf の記述はすべて完全なフック変数名なので、
;; サフィックスの自動付与は無効にする。
(setq use-package-hook-name-suffix nil)

;; :straight を :if / :when / :unless より後に処理させる。
;; straight-use-package-mode は :straight を use-package-keywords の先頭に
;; push するため、既定では :if が偽でも straight-use-package が走り、
;; そのプラットフォームで使わないパッケージまで clone / build されてしまう
;; (Windows で exec-path-from-shell、Linux で w32-ime / tr-ime など)。
;; leaf は :if が偽なら straight-use-package ごと実行しなかったので揃える。
;; :custom / :bind / :config はこれより後なので、導入のタイミングは変わらない。
(setq use-package-keywords
      (let* ((ks (delq :straight (copy-sequence use-package-keywords)))
             (pos (1+ (seq-position ks :unless))))
        (append (seq-take ks pos) '(:straight) (seq-drop ks pos))))

;; :custom を leaf と同じ customize-set-variable にする。
;; 既定値 t のままだと :custom は custom-theme-set-variables 経由 (use-package
;; という擬似テーマ) になり、custom.el が書き込む user テーマのほうが優先順位が
;; 高くなる。つまり「custom.el と user-lisp/ で同じ変数を設定すると
;; user-lisp/ 側が勝つ」という現在の前提が静かに逆転する。
;; leaf の :custom は customize-set-variable なので、そちらに揃える。
(setq use-package-use-theme nil)

;;; [3] site-lisp 以下を読み込む

(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))


;;; --------------------------------------------------
;;; user-lisp/ の読み込み
;;; --------------------------------------------------

;; early-init.el で user-lisp-auto-scrape を nil にしてある。
;; 既定では straight のブートストラップ前に prepare-user-lisp が走ってしまい、
;; その時点では straight も use-package も無いため、モジュールが壊れた .elc に
;; コンパイルされる。ここまでで両方が使える状態になっているので明示的に呼ぶ。
;; user-lisp/ 配下は再帰的にバイトコンパイルされ load-path に追加される。
;; 第 1 引数 JUST-ACTIVATE を t にして、バイトコンパイルと autoload 走査を
;; 行わず load-path への追加だけをさせている。
;;
;; バイトコンパイルすると、パッケージ由来のマクロを use-package の
;; :init / :config で使っている箇所が壊れる。コンパイル時点では当該パッケージが
;; 未ロードでマクロが未定義のため、関数呼び出しとしてコンパイルされてしまうため。
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
(require 'my-text)   ; テキストモード (org / markdown / rst)
(require 'my-lang-lisp)   ; Lisp 系 (Emacs Lisp / Clojure / Common Lisp)
(require 'my-lang-python)   ; Python
(require 'my-lang-web)   ; Web 系 (PHP / JavaScript / TypeScript)
(require 'my-lang-native)   ; ネイティブ系 (Rust / C++ / C#)
(require 'my-lang-misc)   ; その他の言語 (SQL / bat / Swift / Lua / VB)
(require 'my-lsp)   ; LSP と flycheck
(require 'my-fileformat)   ; 特定ファイルフォーマット
(require 'my-project)   ; プロジェクト管理 (projectile)
(require 'my-vc)   ; 構成管理 (magit / git-gutter / SVN)
(require 'my-gitd)   ; magit の git 実行を常駐プロセスに肩代わりさせる
(require 'my-magit-watch)   ; ワークツリーの変化で magit を自動更新 (既定は無効)
(require 'my-shell)   ; Shell 関連
(require 'my-utils)   ; ユーティリティ
(require 'my-platform)   ; OS 固有設定 (Windows / macOS)

;;; --------------------------------------------------
;;; end
;;; --------------------------------------------------

(provide 'init)
;;; init.el ends here
