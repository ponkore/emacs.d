;;
;; setup racer (see https://github.com/racer-rust/emacs-racer)
;;
(use-package rust-mode
  :defer t
  :config
  (require 'racer)
  (require 'flycheck)
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/.rust-src/src/"))
  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (flycheck-mode)))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (require 'company)
  (require 'eldoc)
  (add-hook 'racer-mode-hook (lambda ()
                               (company-mode)
                               (eldoc-mode)))
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
