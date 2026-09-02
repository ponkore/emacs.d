;;; my-lang-lisp.el --- Lisp 系 (Emacs Lisp / Clojure / Common Lisp)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; プログラミング言語
;;; --------------------------------------------------

;;; [3] Emacs lisp

(use-package elisp-mode
  :demand t
  :preface
  (defun my:emacs-lisp-hooks ()
    ;; corfu 移行前は company-backends に company-elisp などを指定していた。
    ;; elisp-mode は elisp-completion-at-point を capf として提供するので
    ;; 追加の指定は要らない。ポップアップを出すまでの間隔だけ短くする。
    (setq-local corfu-auto-delay 0.2)
    (setq-local show-paren-style 'expression))
  ;; (set-newline-and-indent)
  :hook
  (emacs-lisp-mode-hook . my:emacs-lisp-hooks))

;;; [3] Clojure

;;
;; clojure
;;
(use-package clojure-mode
  :straight t
  :mode ("\\(default\\|user\\|emacs\\)\\.\\(behaviors\\|keymap\\)" . clojure-mode)
  :hook
  (clojure-mode-hook . yas-minor-mode)
  ;; (clojure-mode-hook . smartparens-strict-mode) は my-editor.el の
  ;; smartparens ブロックへ移した (計画書の F-4)。Lisp 系の strict 指定を
  ;; 1 箇所にまとめてある。
  ;; (clojure-mode-hook . cljstyle-format-on-save-mode) は削除した。
  ;; cljstyle-format はどのレシピリポジトリにも無く導入されていないのに
  ;; フックに残っていたため、Clojure ファイルを開くたびに
  ;;   File mode specification error: (error "Autoloading file ...
  ;;    failed to define function cljstyle-format-on-save-mode")
  ;; になっていた。しかもフックはそこで中断するので、この後ろに
  ;; 登録されていた smartparens-strict-mode と yas-minor-mode も
  ;; Clojure バッファでは効いていなかった。
  ;; 使いたい場合は明示的なレシピ (:straight (cljstyle-format :type git
  ;; :host github :repo "...")) を書いて導入すること。
  :config
  ;; インデント指定 (計画書の C-9)。
  ;;
  ;; define-clojure-indent は clojure-mode 5.23.0 でも obsolete ではない
  ;; (clojure-mode 自身が今も使っている)。ただし put-clojure-indent への薄い
  ;; ラッパでしかないので、公開 API のほうを直接呼ぶ形にした。
  ;;
  ;; 本題は spec の書式のほう。整数・:defn・位置リストというレガシー形式は
  ;; clojure-mode 6 で削除される予定なので、clojure-ts-mode / cljfmt と共通の
  ;; tuple 形式に書き換える。(defroutes 'defun) -> ((:inner 0))、
  ;; (GET 2) -> ((:block 2))。
  ;;
  ;; とくに 'defun (クォートしたシンボル) は :defn と違って
  ;; clojure--indent-spec-to-modern が変換できず、clojure-get-indent-spec が
  ;; モダン形式として不正な defun をそのまま返していた。インデント自体は
  ;; レガシーのバックトラックエンジンが defun を :defn と同じに扱うので
  ;; 動いていたが、その経路が消えれば壊れる。
  (dolist (sym '(defroutes tabular
                 componentWillMount componentDidMount componentWillUnmount
                 ;; for om.next
                 ident query params render
                 ;;
                 fact do-transaction))
    (put-clojure-indent sym '((:inner 0))))
  (dolist (sym '(GET POST PUT DELETE HEAD ANY context))
    (put-clojure-indent sym '((:block 2))))
  ;; (eldoc-mode +1) は削除した (計画書 F-5 の取りこぼし)。
  ;; :config のトップレベルなので、clojure-mode がロードされた瞬間の
  ;; カレントバッファ (Clojure ファイルとは限らない) で eldoc-mode が
  ;; 有効になっていた。そのうえ global-eldoc-mode が既定で t なので、
  ;; Clojure バッファの eldoc はもともと有効で効果もない。
  )

(use-package flymake-kondor
  ;; 旧 flycheck-clj-kondo。clj-kondo の診断を flymake に流す。
  ;; clojure-mode-hook の flycheck-mode は prog-mode-hook の flymake-mode に
  ;; 置き換わったので不要になった。
  :straight t
  :hook ((clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook)
         . flymake-kondor-setup))

(use-package cider
  :straight t
  :bind ("C-c M-j" . cider-jack-in)
  ;; cider-repl-mode-hook / cider-mode-hook で company-mode を有効化していたが、
  ;; corfu は global-corfu-mode で全バッファに効くので不要になった。
  :custom
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-result-prefix ";; => ")
  ;; custom.el にだけ書かれていたのをこちらへ移した
  (nrepl-sync-request-timeout 40)
  (nrepl-hide-special-buffers t)
  :config
  (add-to-list 'completion-category-defaults '(cider (styles basic))))

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :if (eq system-type 'windows-nt)
  :config
  ;; on Windows, use lein.bat instead of lein shell script.
  (setq cider-lein-command "lein.bat"))

;;; [3] Lisp

;;
;; lisp
;;
(use-package slime
  ;; MELPA の既定レシピに contrib/slime-xterm-color.el の除外を足したもの。
  ;;
  ;; あのファイルはトップレベルで
  ;;   (if (< emacs-major-version 29)
  ;;       (require 'xterm-color)
  ;;     (use-package xterm-color :ensure t))
  ;; と書いている。use-package はマクロなのでバイトコンパイル時に展開され、
  ;; :ensure t がその場で package-install を走らせる。この設定は package.el を
  ;; 無効化していてアーカイブ情報も無いため、ELPA への接続待ちで固まり、
  ;; slime のビルドが終わらなくなる (contrib の .elc が 1 つだけ生成されない)。
  ;;
  ;; slime-xterm-color は slime-fancy にも slime.el にも参照が無いオプトインの
  ;; contrib で、下の slime-setup でも使っていないため除外して問題ない。
  :straight (slime
             :type git :host github :repo "slime/slime"
             :files ("*.el"
                     ("lib" "lib/hyperspec.el")
                     "swank"
                     "*.lisp"
                     "*.asd"
                     "doc/slime.texi"
                     "doc/slime.info"
                     "doc/dir"
                     "ChangeLog"
                     ("contrib" "contrib/*")
                     (:exclude "contrib/test" "contrib/Makefile"
                               "contrib/slime-xterm-color.el")))
  :commands slime-setup
  :custom
  ;; roswell が無い環境では (concat nil " run") がエラーにならず " run" という
  ;; 壊れた値になっていた (nil は空シーケンスとして concat に受理される)。
  ;; ros が見つかったときだけ "ros run" を使い、無ければ処理系を直接探す。
  ;; use-package の :custom は値の位置を式として評価するので、
  ;; leaf のときのようなバッククォートは要らない。
  (inferior-lisp-program (let ((ros (executable-find "ros")))
                           (cond (ros (concat ros " run"))
                                 ((executable-find "sbcl") "sbcl")
                                 ((executable-find "ccl") "ccl")
                                 (t "sbcl"))))
  :config
  ;; slime-company は corfu 移行にともない外した。slime-fancy が
  ;; slime-complete-symbol を capf として提供するので corfu から使える。
  (slime-setup '(slime-repl slime-fancy slime-banner)))

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :hook
  (lisp-interaction-mode-hook . (lambda() (define-key lisp-interaction-mode-map (kbd "C-c RET") 'my:pp-macroexpand-last-sexp)))
  (emacs-lisp-mode-hook . (lambda() (define-key emacs-lisp-mode-map (kbd "C-c RET") 'my:pp-macroexpand-last-sexp)))
  :preface
  (defun my:pp-macroexpand-last-sexp ()
    (interactive)
    (if (thing-at-point-looking-at "\(")
        (save-excursion
          (forward-list)
          (pp-macroexpand-last-sexp nil))
      (pp-macroexpand-last-sexp nil))))

(provide 'my-lang-lisp)
;;; my-lang-lisp.el ends here
