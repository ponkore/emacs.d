;;; my-lang-lisp.el --- Lisp 系 (Emacs Lisp / Clojure / Common Lisp)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; プログラミング言語
;;; --------------------------------------------------

;;; [3] Emacs lisp

(leaf elisp-mode
  :require t
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
(leaf clojure-mode
  :straight t
  :commands define-clojure-indent
  :mode ("\\(default\\|user\\|emacs\\)\\.\\(behaviors\\|keymap\\)" . clojure-mode)
  :hook
  (clojure-mode-hook . yas-minor-mode)
  (clojure-mode-hook . smartparens-strict-mode)
  (clojure-mode-hook . flycheck-mode)
  (clojure-mode-hook . cljstyle-format-on-save-mode)
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
  ;; (cljstyle-format-on-save-mode t)
  )

(leaf flycheck-clj-kondo
  :straight t)

(leaf cljstyle-format
  :straight t)

(leaf cider
  :straight t
  :bind ("C-c M-j" . cider-jack-in)
  ;; cider-repl-mode-hook / cider-mode-hook で company-mode を有効化していたが、
  ;; corfu は global-corfu-mode で全バッファに効くので不要になった。
  :custom
  (cider-show-error-buffer . t)
  (cider-auto-select-error-buffer . t)
  (cider-repl-result-prefix . ";; => ")
  ;; (nrepl-sync-request-timeout . 40)
  (nrepl-hide-special-buffers . t)
  :config
  (add-to-list 'completion-category-defaults '(cider (styles basic))))

(leaf cider-lein-command-on-windows
  :if (eq system-type 'windows-nt)
  :config
  ;; on Windows, use lein.bat instead of lein shell script.
  (setq cider-lein-command "lein.bat"))

;;; [3] Lisp

;;
;; lisp
;;
(leaf slime
  :straight t
  :commands slime-setup
  :custom
  ;; roswell が無い環境では (concat nil " run") がエラーにならず " run" という
  ;; 壊れた値になっていた (nil は空シーケンスとして concat に受理される)。
  ;; ros が見つかったときだけ "ros run" を使い、無ければ処理系を直接探す。
  `(inferior-lisp-program . ,(let ((ros (executable-find "ros")))
                               (cond (ros (concat ros " run"))
                                     ((executable-find "sbcl") "sbcl")
                                     ((executable-find "ccl") "ccl")
                                     (t "sbcl"))))
  :config
  ;; slime-company は corfu 移行にともない外した。slime-fancy が
  ;; slime-complete-symbol を capf として提供するので corfu から使える。
  (slime-setup '(slime-repl slime-fancy slime-banner)))

(leaf pretty-print
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
