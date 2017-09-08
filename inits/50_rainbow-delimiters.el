(use-package rainbow-delimiters
  :config
  (require 'cl-lib)
  (require 'color)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;;(global-rainbow-delimiters-mode)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))
