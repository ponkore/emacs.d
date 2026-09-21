;;; my-fileformat.el --- 特定ファイルフォーマット  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 特定ファイルフォーマット
;;; --------------------------------------------------

;;; [3] yaml

(use-package yaml-mode
  :straight t
  :defer t)

;;; [3] diff

(use-package diff-mode
  :hook
  (diff-mode-hook . my:diff-mode-setup-faces)
  :config
  (defun my:diff-mode-setup-faces ()
    ;; 追加された行は緑で表示
    (set-face-attribute 'diff-added nil :foreground "white" :background "dark green")
    ;; 削除された行は赤で表示
    (set-face-attribute 'diff-removed nil :foreground "white" :background "dark red")
    ;; 文字単位での変更箇所は色を反転して強調
    ;; ;; 2012-04-02 // 2018-05-30 emacs 26.1 でエラーになるのでコメントアウト
    ;; (set-face-attribute 'diff-refine-change nil :foreground nil :background nil :weight 'bold :inverse-video t)
    )
  ;; my:diff-mode-refine-automatically (diff-auto-refine-mode を呼ぶだけの関数)
  ;; は削除した。diff-auto-refine-mode は Emacs 27.1 で obsolete になり、
  ;; 「set `diff-refine' instead」と言われる。その diff-refine は既定値が
  ;; font-lock で、文字単位の強調は最初から有効。つまりこの関数は不要だった。
  )

;;; [3] log4j

(use-package log4j-mode
  :straight t
  :defer t)

;;; [3] Dockerfile / docker-compose.yml

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :straight t
  :defer t)

;;; [3] vimrc

(use-package vimrc-mode
  :straight t
  :defer t)

;;; [3] XML (MSBuild のプロジェクトファイル)

;; .xml / .xsl / .svg は Emacs 既定で nxml-mode になる (auto-mode-alist の
;; 登録は xml-mode 名だが、nxml-mode.el が (defalias 'xml-mode 'nxml-mode)
;; しているので実体は nxml-mode)。MSBuild は拡張子が独自なので登録が無い。
;; 31.1 に入っているのは新形式のソリューション .slnx だけ。
(use-package nxml-mode
  :mode ("\\.csproj\\'" . nxml-mode))

;;; [3] CSV / TSV

;; .tsv は csv-mode の autoload で tsv-mode になる。csv-align-mode は
;; display プロパティで見た目だけ揃えるので、ファイルの中身は変わらない。
;; tsv-mode は csv-mode の派生なので csv-mode-hook だけに張る。
;; tsv-mode-hook にも張ると 2 回走り、トグルの csv-header-line が消える。
(use-package csv-mode
  :straight t
  :hook
  (csv-mode-hook . my:csv-mode-setup)
  :custom
  (csv-align-max-width 60)
  :config
  (defun my:csv-mode-setup ()
    (csv-align-mode 1)
    (csv-header-line 1)
    ;; 横に長い表は折り返さない
    (setq-local truncate-lines t)))

;;; [3] xxx

(provide 'my-fileformat)
;;; my-fileformat.el ends here
