
;; http://oh-sky.hatenablog.com/entry/2013/07/07/004651
(add-hook 'php-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 4)))
