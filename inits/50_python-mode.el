;;
;; https://qiita.com/ignorant/items/50f8eb2852d0f0214659
;;
;; (setenv "PYTHONPATH" "/Users/masao/.emacs.d/jedi-backend/lib/python3.6/site-packages/")
;;
;; pip3 install virtualenv jedi epc
;; M-x jedi:install-server
(use-package company-jedi
  :ensure t
  :after company
  :config
  (setq jedi:complete-on-dot t)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; prefer python3
  (setq python-shell-interpreter "python3")
  ;; https://github.com/jorgenschaefer/elpy/issues/887
  (setq python-shell-completion-native-enable nil)
  ;; https://emacs.stackexchange.com/questions/16361/how-to-automatically-run-inferior-process-when-loading-major-mode
  (defun my-run-python ()
    (save-selected-window
      (switch-to-buffer-other-window (process-buffer (python-shell-get-or-create-process (python-shell-parse-command))))))
  (add-hook 'python-mode-hook 'my-run-python))
