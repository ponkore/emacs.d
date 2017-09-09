(use-package csharp-mode
  :defer t
  :config
  ;; C# 編集用のモード (flymake は一旦やめておく)
  (require 'yasnippet)
  (defun my-csharp-mode-fn ()
    "my function that runs when csharp-mode is initialized for a buffer."
    (turn-on-font-lock)
    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
    (setq indent-tabs-mode nil) ;; tabs are evil
    (yas-minor-mode-on)
    (my-csharp-mode-fn)
    (setq comment-column 40)
    (setq c-basic-offset 4)
    ;; (font-lock-add-magic-number)
    ;; オフセットの調整
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    ;; see http://qiita.com/masnagam/items/e3313dc9a66bd7fd76fa
    (setq csharp-want-imenu nil))
  (add-hook 'csharp-mode-hook '(lambda() (my-csharp-mode-fn))))
;; see http://ongaeshi.hatenablog.com/entry/20110116/1295187496
