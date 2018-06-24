;;
;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
;;
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;;
;; http://cortyuming.hateblo.jp/entry/20121021/p1
;;
;; (add-hook 'shell-mode-hook
;;           '(lambda ()
;;              (progn
;;                (define-key shell-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
;;                (define-key shell-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
;;                (set-buffer-process-coding-system 'utf-8-hfs 'utf-8-hfs)
;;                (require 'ansi-color)
;;                (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;                  "Set `ansi-color-for-comint-mode' to t." t)
;;                ;; ;; "Emacs ANSI colors"
;;                ;; ;; http://tapoueh.org/blog/2011/07/29-emacs-ansi-colors.html
;;                (setq ansi-color-names-vector
;;                      (vector
;;                       ;; (frame-parameter nil 'background-color)
;;                       "#444444"
;;                       "#f57900" "#8ae234" "#edd400" "#729fcf"
;;                       "#ad7fa8" "#00cdcd" "#eeeeec")
;;                      ansi-term-color-vector ansi-color-names-vector
;;                      ansi-color-map (ansi-color-make-color-map))
;;                )))
