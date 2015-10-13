;;; yasnippet
;;
;;
(require 'yasnippet)
(yas-reload-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-yas/prompt (http://syohex.hatenablog.com/entry/20121207/1354885367)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  choice)))
         (selected (helm-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*helm yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))

(custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

;; (require 'helm-c-yasnippet)
;; (setq helm-yas-space-match-any-greedy t) ;[default: nil]
;; (global-set-key (kbd "C-c y") 'helm-yas-complete)
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(custom-set-variables '(yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0"))

(yas-global-mode 1)
