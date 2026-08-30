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
  :commands define-clojure-indent
  :mode ("\\(default\\|user\\|emacs\\)\\.\\(behaviors\\|keymap\\)" . clojure-mode)
  :hook
  (clojure-mode-hook . yas-minor-mode)
  (clojure-mode-hook . smartparens-strict-mode)
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
  (define-clojure-indent
   (defroutes 'defun)
   (tabular 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (context 2)
   (componentWillMount 'defun)
   (componentDidMount 'defun)
   (componentWillUnmount 'defun)
   ;; for om.next
   (ident 'defun)
   (query 'defun)
   (params 'defun)
   (render 'defun)
   ;;
   (fact 'defun)
   (do-transaction 'defun))
  (eldoc-mode +1)
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
  :straight t
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
