;;
;; setup racer (see https://github.com/racer-rust/emacs-racer)
;;
(setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
(setq racer-rust-src-path (expand-file-name "~/.rust-src/src/"))
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-mode)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             (eldoc-mode)))
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

