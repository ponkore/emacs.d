;;; my-dired.el --- dired と dired-sidebar  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; dired
;;; --------------------------------------------------

;;; [3] dired

;; dired-k は diff-hl に統合した (my-vc.el の diff-hl-dired-mode)。
;; dired-k は 2021 年から更新が止まり emacsorphanage に移されていた。
;; ファイルサイズ・更新日時の色分けは diff-hl には無いので失われる。

(use-package dired
  :commands dired-vc-status
  :bind
  (:map dired-mode-map
   ("V" . dired-vc-status)
   ;; 本家 ripgrep-regexp は検索ディレクトリも聞いてくる。dired では
   ;; そのバッファのディレクトリで検索したいので my:ripgrep-regexp を使う
   ;; (my-utils.el で定義)。従来ここが本家に割り当たっていたため、
   ;; せっかく定義した my:ripgrep-regexp が使われていなかった。
   ("G" . my:ripgrep-regexp)
   ("." . hydra-dired/body))
  :custom
  ;;
  ;; http://qiita.com/l3msh0@github/items/8665122e01f6f5ef502f
  ;;
  ;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
  (dired-dwim-target t)
  ;; ディレクトリを再帰的にコピーする
  (dired-recursive-copies 'always)
  ;; diredバッファでC-sした時にファイル名だけにマッチするように
  (dired-isearch-filenames t)
  ;;
  (ls-lisp-dirs-first t)
  :config
  ;; my:dired-revert-buffer (g に割り当てていた revert-buffer + dired-k) は
  ;; 削除した。dired 既定の g (revert-buffer) で dired-after-readin-hook が
  ;; 走り dired-k-no-revert が呼ばれるので、明示的な呼び出しは二重起動になる。
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
  (defun dired-vc-status (&rest _args)
    (interactive)
    (let ((path (find-path-in-parents (dired-current-directory) '(".git" ".svn"))))
      (cond ((null path)
             (message "not version controlled."))
            ((string-match-p "\\.svn$" path)
             (svn-status (file-name-directory path)))
            ((string-match-p "\\.git$" path)
             ;; magit-status-internal は magit 4.x で削除された。
             ;; 現在の入口は magit-status-setup-buffer。
             (magit-status-setup-buffer (file-name-directory path))))))
  ;;
  ;; leaf の :hydra は init 時にインライン展開されるので :init に置く。
  :init
  (defhydra hydra-dired (:hint nil :color pink)
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

;;; [3] dired-sidebar

;; neotree から移行した。neotree をやめた理由:
;;   - 手元のクローンが 2020-03 で止まっていた
;;   - neo-theme 'icons が all-the-icons を要求するため、all-the-icons を
;;     外した時点で F8 が
;;       Package `all-the-icons' isn't installed
;;     でエラーになり、サイドバーが開けなくなっていた
;;
;; dired-sidebar はサイドバーのバッファ自体が dired バッファなので、
;; dired 用に設定してあるもの (キーバインド、nerd-icons-dired、
;; diff-hl-dired) がそのまま効く。依存も emacs + compat だけで増えない。
;; treemacs も候補だったが、独自のバッファ・キーマップ・アイコン体系を持ち
;; 依存が 4 つ増えるため、この設定の作りには合わないと判断した。

(use-package dired-sidebar
  :straight t
  :commands dired-sidebar-toggle-sidebar
  :bind
  (("<f8>" . dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
   ;; neotree の a (隠しファイルの表示切替) 相当。
   ;; dired-omit-mode は dired-x (組み込み) のもの。
   ("a" . dired-omit-mode)
   ;; neotree の <left> (親ノードへ) 相当。既定では ^ と - にもある。
   ("<left>" . dired-sidebar-up-directory)
   ;; ディレクトリ行でその場にサブツリーを展開する (もう一度押すと畳む)。
   ;; 既定では TAB にも割り当てられている。
   ;; ルートごと移動したいときは RET (dired-sidebar-find-file)。
   ("<right>" . dired-sidebar-subtree-toggle))
  :custom
  ;; nerd-icons-dired を使ってアイコンを出す (他の箇所と同じ体系)
  (dired-sidebar-theme 'nerd-icons)
  (dired-sidebar-width 35)
  ;; neotree では開くたびに text-scale を 1 段階下げていた。
  ;; dired-sidebar は dired-sidebar-face を buffer-face-mode で当てる。
  (dired-sidebar-use-custom-font t)
  ;; ファイルを開いてもサイドバーは開いたままにする
  ;; (neotree の neotree-enter-hide は「あまり便利じゃなかった」と
  ;;  コメントされ、割り当てもされていなかったので同じ方針にする)
  (dired-sidebar-close-sidebar-on-file-open nil)
  ;; 選択中のバッファのファイルをサイドバー上で追いかける
  (dired-sidebar-should-follow-file t)
  :config
  ;; --- clone-buffer 経由でサイドバーを作ると dired-mode-hook が壊れる件 ---
  ;;
  ;; そのディレクトリの dired バッファを表示している状態で F8 を押すと、
  ;; dired-sidebar-get-or-create-buffer は既存の dired バッファを乗っ取らない
  ;; ように clone-buffer でコピーを作る (bin -> bin<2>)。
  ;; ところが clone-buffer は major-mode を設定してからバッファローカル変数を
  ;; コピーするので、dired-mode-hook が走る時点では dired-subdir-alist が
  ;; まだ空のまま、しかしバッファには一覧のテキストが入っている、という
  ;; 中途半端な状態になる。
  ;;
  ;; nerd-icons-dired-mode は有効化時に nerd-icons-dired--refresh を呼び、
  ;; そこから dired-get-filename -> dired-current-directory と辿るので
  ;;   No subdir-alist in bin<2>
  ;; で失敗する (通常の dired ではフックが走る時点でバッファが空なので
  ;;  refresh が空振りし、問題が表面化しない)。
  ;;
  ;; クローン中はフックを走らせず、subdir-alist を作ってから改めて走らせる。
  ;; 上流が直したらこの advice は削除してよい。
  (defun my:dired-sidebar-clone-hook-fix (orig root)
    "clone-buffer 経由のときだけ dired-mode-hook を後回しにする。"
    (if (or (get-buffer (dired-sidebar-buffer-name root))
            (not (eq (current-buffer) (dired-noselect root))))
        ;; 既存バッファを返すだけ、もしくはリネームで済む経路はそのまま
        (funcall orig root)
      (let ((buffer (let ((dired-mode-hook nil)) (funcall orig root))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (unless dired-subdir-alist (dired-build-subdir-alist))
            (run-hooks 'dired-mode-hook)))
        buffer)))
  (advice-add 'dired-sidebar-get-or-create-buffer
              :around #'my:dired-sidebar-clone-hook-fix)

  ;; dired-sidebar-face は defface だが、dired-sidebar-set-font は
  ;;   (when (bound-and-true-p dired-sidebar-face)
  ;;     (setq-local buffer-face-mode-face dired-sidebar-face) ...)
  ;; と「変数」として読み、buffer-face-mode-face に渡している。
  ;; つまり :custom-face ではなく face 属性のプロパティリストを
  ;; 変数に入れる必要がある。
  (setq dired-sidebar-face '(:height 0.9))
  ;; dired-omit-mode のため
  (require 'dired-x))

;;; [4] dired-subtree

;; dired-sidebar のサブツリー展開 (dired-sidebar-want-subtree) は
;; dired-subtree が入っているときだけ有効になる。入れていないと
;; TAB や <right> はディレクトリへ移動するだけで、ツリーとして
;; その場に展開できない。
(use-package dired-subtree
  :straight t
  :after dired
  :custom
  ;; 階層ごとの背景色の差を付けない (modus のテーマに任せる)
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
   ("<tab>" . dired-subtree-toggle)
   ("<backtab>" . dired-subtree-cycle)))

(provide 'my-dired)
;;; my-dired.el ends here
