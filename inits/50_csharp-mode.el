;; C# 編集用のモード
;(require csharp-mode)
; csharp-mode は、なぜか余計な flymake がついてくるので一旦とりやめ。
(add-to-list 'auto-mode-alist '("\\.cs$" . c++-mode))
(setq c++-mode-hook-fn (lambda () (setq tab-width 4) (setq indent-tabs-mode t)))
(add-hook 'c++-mode-hook c++-mode-hook-fn)
