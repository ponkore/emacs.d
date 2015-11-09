;; IME on/off key bind
(global-set-key (kbd "M-`") 'toggle-input-method)

;; minibuffer に入った時、IME を OFF にする
(add-hook 'minibuffer-setup-hook
          (lambda () (deactivate-input-method)))
(add-hook 'helm-minibuffer-set-up-hook
          (lambda () (deactivate-input-method)))
