;; F# 編集用のモード

(setq inferior-fsharp-program "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsi.exe\"")
(setq fsharp-compiler "\"C:\\Program Files\\Microsoft F#\\v4.0\\Fsc.exe\"")

(defun inferior-fsharp-mode-hook-fn ()
  ""
  ;; (setq comint-output-filter-functions 'comint-truncate-buffer)
  (set-buffer-process-coding-system 'cp932 'cp932))

(add-hook 'inferior-fsharp-mode-hooks 'inferior-fsharp-mode-hook-fn)
