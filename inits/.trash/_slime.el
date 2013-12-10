;;; SLIME and ac-slime
(require 'slime)
(require 'ac-slime)

;; Clozure CL (SLIME)
(setq ccl-root
      (cond
       ((eq window-system 'ns) "/opt/local")
       ((eq system-type 'berkeley-unix) "/usr/local")
       (t "C:/Apps/ccl")))
(setq inferior-lisp-program
      (cond
       ((eq window-system 'ns) (concat ccl-root "/bin/ccl64 -K utf-8"))
       ((eq system-type 'berkeley-unix) (concat ccl-root "/bin/ccl -K utf-8"))
       (t (concat ccl-root "/wx86cl.exe -K utf-8"))))
(setq hyperspec-root
      (cond
       ((eq window-system 'ns) (concat ccl-root "/share/doc/lisp/HyperSpec-7-0/HyperSpec"))
       ((eq system-type 'berkeley-unix) (concat ccl-root "/share/doc/clisp-hyperspec/HyperSpec"))
       (t (concat ccl-root "/../HyperSpec"))))
(require 'slime-autoloads)
(require 'hyperspec)
;;;
(setq common-lisp-hyperspec-root (concat "file://" (expand-file-name (concat hyperspec-root "/")))
      common-lisp-hyperspec-symbol-table (expand-file-name (concat hyperspec-root "/Data/Map_Sym.txt")))
(setq slime-net-coding-system 'utf-8-unix)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))
(slime-setup '(slime-repl slime-fancy slime-banner))
(add-to-list 'auto-mode-alist '("\\.asd$" . common-lisp-mode))
