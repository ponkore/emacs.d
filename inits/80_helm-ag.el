;; http://emacs.rubikitch.com/helm-ag/

(setq helm-ag-base-command "rg --vimgrep --no-heading")

;;; 現在のシンボルをデフォルトのクエリにする
(setq helm-ag-insert-at-point 'symbol)

(defun helm-ag-dot-emacs ()
  ".emacs.d以下を検索"
  (interactive)
  (helm-ag "~/.emacs.d/"))
