(use-package dired-mode
  :init
  ;; バージョン管理システム
  ;; diredから適切なバージョン管理システムの*-statusを起動
  (defun dired-vc-status (&rest args)
    (interactive)
    (let ((path (find-path-in-parents (dired-current-directory)
                                      '(".git" ".svn"))))
      (cond ((null path)
             (message "not version controlled."))
            ((string-match-p "\\.svn$" path)
             (svn-status (file-name-directory path)))
            ((string-match-p "\\.git$" path)
             (magit-status-internal (file-name-directory path))))))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "V" 'dired-vc-status)))
  ;;
  ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
  ;;
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (setq dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (setq dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (setq dired-isearch-filenames t)
  ;;
  ;; dired-k
  ;;
  (require 'dired-k)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (define-key dired-mode-map "K" 'dired-k)
  (define-key dired-mode-map "g" 'dired-k))
