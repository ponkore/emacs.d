;; C# 編集用のモード (flymake は一旦やめておく)

(defun my-csharp-mode-fn ()
  "my function that runs when csharp-mode is initialized for a buffer."
  (turn-on-font-lock)
  (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
  (setq indent-tabs-mode nil) ;; tabs are evil
  (yas/minor-mode-on))

(add-hook 'csharp-mode-hook 'my-csharp-mode-fn t)

;; http://ongaeshi.hatenablog.com/entry/20110116/1295187496
(add-hook 'csharp-mode-hook
          '(lambda()
             (setq comment-column 40)
             (setq c-basic-offset 4)
             ;; (font-lock-add-magic-number)
             ;; オフセットの調整
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'case-label '+)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close 0)))
