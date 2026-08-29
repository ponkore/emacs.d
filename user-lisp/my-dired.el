;;; my-dired.el --- dired と neotree  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; dired
;;; --------------------------------------------------

;;; [3] dired

(leaf dired-k
  :straight t)

(leaf dired
  :commands dired-vc-status
  :bind
  (:dired-mode-map
   ("V" . dired-vc-status)
   ("K" . dired-k)
   ("G" . ripgrep-regexp)
   ("g" . my:dired-revert-buffer)
   ("." . hydra-dired/body))
  :hook
  (dired-mode-hook . dired-k)
  (dired-initial-position-hook . dired-k)
  :custom
  ;;
  ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
  ;;
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (dired-dwim-target . t)
  ;; ディレクトリを再帰的にコピーする
  (dired-recursive-copies . 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (dired-isearch-filenames . t)
  ;;
  (ls-lisp-dirs-first . t)
  :config
  (defun my:dired-revert-buffer ()
    (interactive)
    (revert-buffer)
    (dired-k))
  ;; バージョン管理システム
  ;; diredから適切なバージョン管理システムの*-statusを起動
  (defun find-path-in-parents (directory base-names)
    (or (cl-find-if 'file-exists-p
                    (mapcar (lambda (base-name) (concat directory base-name)) base-names))
        (if (string= directory "/")
            nil
          (let ((parent-directory (substring directory 0 -1)))
            (find-path-in-parents parent-directory base-names)))))
  ;;
  (defun dired-vc-status (&rest args)
    (interactive)
    (let ((path (find-path-in-parents (dired-current-directory) '(".git" ".svn"))))
      (cond ((null path)
             (message "not version controlled."))
            ((string-match-p "\\.svn$" path)
             (svn-status (file-name-directory path)))
            ((string-match-p "\\.git$" path)
             (magit-status-internal (file-name-directory path))))))
  ;;
  :hydra
  (hydra-dired (:hint nil :color pink)
               "
_+_ mkdir   _v_iew         _m_ark         _z_ip     _w_ get filename
_C_opy      view _o_ther   _U_nmark all   un_Z_ip   _W_ get fullpath
_D_elete    open _f_ile    _u_nmark       _s_ort    _g_ revert buffer
_R_ename    ch_M_od        _t_oggle       _e_dit    _[_ hide detail     _._togggle hydra
"
               ("[" dired-hide-details-mode)
               ("+" dired-create-directory)
               ("RET" dired-open-in-accordance-with-situation :exit t)
               ("f" dired-open-in-accordance-with-situation :exit t)
               ("C" dired-do-copy)   ;; Copy all marked files
               ("D" dired-do-delete)
               ("M" dired-do-chmod)
               ("m" dired-mark)
               ("o" dired-view-file-other-window :exit t)
               ("?" dired-summary :exit t)
               ("R" dired-do-rename)
               ("a" dired-list-all-mode)
               ("g" revert-buffer)
               ("e" wdired-change-to-wdired-mode :exit t)
               ("s" dired-sort-toggle-or-edit)
               ;; ("T" counsel-tramp :exit t)
               ("t" dired-toggle-marks)
               ("U" dired-unmark-all-marks)
               ("u" dired-unmark)
               ("v" dired-view-file :exit t)
               ("w" dired-copy-filename-as-kill)
               ("W" dired-get-fullpath-filename)
               ("z" dired-zip-files)
               ("Z" dired-do-compress)
               ;; ("F" my:finder-app)
               ;; ("T" my:iterm-app)
               ("q" nil)
               ("." nil :color blue)))

;;; [3] neotree

(leaf neotree
  :straight t
  :bind (("<f8>" . neotree-toggle)
         (:neotree-mode-map
          ;; ("RET" . neotree-enter-hide)  ;; ファイルを開く時自動で neotree を閉じる。あまり便利じゃなかったので一旦コメントアウト
          ("a" . neotree-hidden-file-toggle)
          ("<left>" . neotree-select-up-node)
          ("<right>" . neotree-change-root)))
  :hook (neo-after-create-hook . (lambda (_) (if (display-graphic-p) (call-interactively 'neotree-text-scale))))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Change neotree's font size
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/218
  (defun neotree-text-scale ()
    "Text scale for neotree."
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1)
    (message nil))
  ;; neotree enter hide
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/77
  (defun neo-open-file-hide (full-path &optional arg)
    "Open file and hiding neotree.
     The description of FULL-PATH & ARG is in `neotree-enter'."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun neotree-enter-hide (&optional arg)
    "Neo-open-file-hide if file, Neo-open-dir if dir.
     The description of ARG is in `neo-buffer--execute'."
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))

(provide 'my-dired)
;;; my-dired.el ends here
