;; C# 編集用のモード (flymake は一旦やめておく)

(defun my-csharp-mode-fn ()
  "my function that runs when csharp-mode is initialized for a buffer."
  (turn-on-font-lock)
  (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
  (setq indent-tabs-mode nil) ;; tabs are evil
  (yas/minor-mode-on))

(add-hook 'csharp-mode-hook 'my-csharp-mode-fn t)

