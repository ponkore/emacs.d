(defun my-php-mode-setup ()
  "My PHP-mode hook."
  (subword-mode 1)
  (setq show-trailing-whitespace t)

  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

  (require 'flycheck-phpstan)
  (flycheck-mode t)
  (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  (add-to-list 'flycheck-disabled-checkers 'php-phpcs))

(use-package php-mode
  :hook ((php-mode . my-php-mode-setup))
  :custom
  (php-manual-url 'ja)
  (php-mode-coding-style 'psr2)
  (php-mode-template-compatibility nil)
  :config
  (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
  (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
  ;; (bind-key "C-}" 'cedit-barf php-mode-map)
  ;; (bind-key "C-)" 'cedit-slurp php-mode-map)
  (bind-key "C-c C-c" 'psysh-eval-region         php-mode-map)
  (bind-key "<f6>" 'phpunit-current-project      php-mode-map)
  (bind-key "C-c C--" 'php-current-class php-mode-map)
  (bind-key "C-c C-=" 'php-current-namespace php-mode-map))
