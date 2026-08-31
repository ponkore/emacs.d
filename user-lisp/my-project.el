;;; my-project.el --- プロジェクト管理 (projectile)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; プロジェクト管理
;;; --------------------------------------------------

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
    "Merge version to search document via grep/ag/rg.
      Use fast alternative if it exists, fallback grep if no alternatives in system.
      "
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
