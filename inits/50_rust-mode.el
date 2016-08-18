;;
;; setup racer (see https://github.com/racer-rust/emacs-racer)
;;
(setq racer-cmd "/Users/masao/.cargo/bin/racer")
(setq racer-rust-src-path "/Users/masao/.rust-src/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
