;;; my-project.el --- プロジェクト管理 (projectile)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; プロジェクト管理
;;; --------------------------------------------------

;;; [3] project.el（eglot が使うプロジェクト境界）

;; 【重要】`eglot' はここで決まるルートの**全ディレクトリ**を監視する。
;;
;; `eglot--watch-globs' は `workspace/didChangeWatchedFiles' の動的登録を
;; 受けると、プロジェクト配下のディレクトリ 1 つにつき
;; `file-notify-add-watch' を 1 つ張る。しかも**登録 ID ごとに別の集合**を
;; 作るので同じディレクトリが登録数ぶん重複する。Windows では
;; **watch 1 本につき OS スレッドが 1 本**（w32notify）なので、ここが
;; 膨らむと Emacs が固まる。
;;
;; 実測（2026-10-28、`oracled/` を置いて `.go` を開いたとき）:
;;
;;   gopls の root が ~/.emacs.d になり watch 6751 本（登録 4 つ ×
;;   約 1700。`.emacs.d` の実ディレクトリ 4973 のうち 3913 が straight/）
;;   + 別プロジェクトの TS サーバ 3249 本 = ちょうど 10000 本で
;;   `eglot-max-file-watches' に到達。Emacs のスレッド 7345 本、
;;   ハンドル 28105。上限に達すると eglot は警告のあと
;;   `jsonrpc-error' を投げ、**その登録ぶんの watch を全部捨てる**ので、
;;   数千本張ってから全部外すだけの時間になる。
;;
;; `go.mod` / `Cargo.toml` をルートの目印に足すと、`oracled/` `ptyd/`
;; `gitd/` がそれぞれ別プロジェクトになり、gopls の root が
;; `oracled/`（数十ディレクトリ）に収まる。
;;
;; 【重要】マーカーを足すだけでは効かない。`project-find-functions' の
;; **先頭が `project-projectile'** なので、`project-try-vc'（マーカーを
;; 見る側）まで到達しないため（実測: マーカーを設定しても
;; `oracled/cmd/oracled/` のルートが `~/.emacs.d/` のままだった。
;; `project-try-vc--search' を直に呼ぶと `~/.emacs.d/oracled/` を返す）。
;;
;; eglot には `eglot-lsp-context'（LSP の文脈でプロジェクトを探している
;; 間だけ非 nil）という、まさにこのための仕掛けがある。これを見て
;; **eglot のときだけ** VC + マーカーで決める関数を先頭に足す。
;; `C-c p`（projectile）も `C-x p`（project.el）も挙動は変わらない。
(declare-function project-try-vc "project" (dir))

(defun my:project-root-for-eglot (dir)
  "eglot がプロジェクトを探しているときだけ VC + マーカーで決める。
`project-vc-extra-root-markers' に足した `go.mod' / `Cargo.toml' が
効くようになるので、`~/.emacs.d/oracled/' のような入れ子のモジュールが
それ自体のルートになる。それ以外の場面では nil を返して、後続の
`project-projectile' に任せる。"
  (and (bound-and-true-p eglot-lsp-context)
       (fboundp 'project-try-vc)
       (project-try-vc dir)))

;; depth を負にして `project-projectile' より前に置く
(add-hook 'project-find-functions #'my:project-root-for-eglot -10)

(use-package project
  :defer t
  :custom
  (project-vc-extra-root-markers '("go.mod" "Cargo.toml")))

;;; [3] projectile

;; :defer t は必須。leaf は :require t が無い限り (require) を出さないが、
;; use-package は遅延キーワードが 1 つも無いと (require) を出してしまう。
;; ここで eager load すると projectile と ripgrep が起動時に読まれる。
;; 実際に使う projectile-ripgrep コマンドは straight の autoload で引ける。
(use-package projectile-ripgrep
  :straight t
  :defer t)

(use-package projectile
  :straight t
  :commands projectile-register-project-type projectile-toggle-between-implementation-and-test
  :hook
  (emacs-startup-hook . projectile-mode)
  ;; use-package の :bind は 1 つのリストにまとめる。:map より前に書いたものが
  ;; グローバル、後に書いたものがそのマップへのバインドになる。
  :bind
  (("C-c p" . projectile-command-map)
   :map projectile-command-map
   ("s" . my:projectile-search-dwim)
   ("<f12>" . projectile-toggle-between-implementation-and-test))
  :custom
  ;; projectile-enable-idle-timer は upstream から削除された
  ;; (アイドル時のキャッシュ更新の仕組み自体が無くなった)。
  (projectile-enable-caching t)
  (projectile-mode-line-prefix " P")
  ;; (projectile-completion-system 'ivy)
  ;; :preface で (require 'ripgrep) しており起動時に eager load していた。
  ;; ripgrep は projectile-ripgrep の依存として入るので、使用時に読めば足りる。
  :preface
  (defun my:projectile-search-dwim (search-term)
    "Search documents with rg, ag or grep.
Use the fastest alternative available on the system, falling back to
grep when none is installed."
    (interactive (list (projectile--read-search-string-with-default
                        "Dwim search for")))
    (cond
     ((executable-find "rg")
      (require 'ripgrep nil t)
      (projectile-ripgrep search-term))
     ((executable-find "ag") (projectile-ag search-term))
     (t (projectile-grep search-term)))))

(provide 'my-project)
;;; my-project.el ends here
