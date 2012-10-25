;;
;; Visual Basic Mode - visual-basic-mode.el
;;
(add-to-list 'load-path "~/.emacs.d/config/packages")
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq visual-basic-mode-indent 4)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . 
                                visual-basic-mode)) auto-mode-alist))
(add-hook 'visual-basic-mode-hook
          '(lambda () (setq mode-name "vb")))


;;; psvn-mode
;;
;; 2012-10-25
(el-get 'sync '(psvn))
