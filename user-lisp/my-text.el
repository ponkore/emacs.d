;;; my-text.el --- テキストモード (org / markdown / rst / adoc)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;; ox-pandoc / markdown-mode の :custom で my:pandoc-data-file と s-join を使う。
;; leaf の :custom はマクロ展開時にバッククォートを評価するため、
;; バイトコンパイル時にもこれらが定義済みである必要がある。
(require 'my-core)

;;; --------------------------------------------------
;;; テキストモード
;;; --------------------------------------------------

;;; [3] org-mode

(leaf org
  ;; Emacs 31.1 同梱の org (9.8.7) を使う。
  ;; 以前は :straight t で org 9.5.1 (2021年) を入れていたが、
  ;; init.org のタングルで組み込み org が先にロードされるため版が混在していた。
  ;; :mode 指定も組み込みの auto-mode-alist で足りるので外した。
  :hook (org-mode-hook . turn-on-font-lock)
  :custom
  ;; org-mode内部のソースを色付けする
  (org-src-fontify-natively . t)
  ;; org-modeの開始時に、行の折り返しを無効にする。
  (org-startup-truncated . t)
  ;; follow-linkから戻ることを可能とする。
  (org-return-follows-link . t)
  (org-refile-use-outline-path . 'file)
  (org-outline-path-complete-in-steps . nil)
  (org-log-done . t)
  ;; (org-todo-keywords . '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keywords . '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)")
                         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
  (org-todo-keyword-faces . '(("TODO" :foreground "red" :weight bold)
                              ("STARTED" :foreground "cornflower blue" :weight bold)
                              ("DONE" :foreground "green" :weight bold)
                              ("WAITING" :foreground "orange" :weight bold)
                              ("HOLD" :foreground "magenta" :weight bold)
                              ("CANCELLED" :foreground "green" :weight bold)
                              ("MEETING" :foreground "green" :weight bold)))
  (org-indent-indentation-per-level . 0)
  (org-adapt-indentation . nil)
  (org-clock-clocked-in-display . 'none)
  (org-clock-out-remove-zero-time-clocks . t)
  :config
  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 #'org-save-all-org-buffers)
  ;; local functions
  (defun my:org-add-ymd-to-archive (name)
    "replace anchor to YYYY-MM string"
    (let* ((ymd (format-time-string "%Y-%m")))
      (replace-regexp-in-string "#YM" ymd name)))
  ;; TODO: org 9.8 で `org-extract-archive-file' が削除された。
  ;; 後継は `org-archive--compute-location' だが、戻り値が文字列ではなく
  ;; (FILE . HEADING) の cons なので上の :filter-return advice はそのまま使えない。
  ;; またプライベート関数 (--) を advise するのは脆いため、一旦無効化して先送りする。
  ;; (advice-add 'org-extract-archive-file :filter-return #'my:org-add-ymd-to-archive)
  ;; screenshot: https://ladicle.com/post/config/
  (defun my:org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer and insert a link to this file."
    (interactive)
    (org-display-inline-images)
    ;; filename はグローバル変数に代入されていた (let 束縛が無かった)
    (let ((filename
           (concat
            (make-temp-name
             (concat (file-name-nondirectory (buffer-file-name))
                     "_imgs/"
                     (format-time-string "%Y%m%d_%H%M%S_"))) ".png")))
      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename)))
      ;; take screenshot
      (if (eq system-type 'darwin)
          (call-process "screencapture" nil nil nil "-i" filename))
      (if (eq system-type 'gnu/linux)
          (call-process "import" nil nil nil filename))
      ;; insert into file if correctly taken
      (if (file-exists-p filename)
          (insert (concat "[[file:" filename "]]")))))
  ;; update todo summary
  (defun my:org-buffer-calc-summary ()
    (save-excursion
      (goto-char (point-min))
      (let ((results nil))
        (while (re-search-forward "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" nil t)
          (setq results (append results
                                (list (cons
                                       (string-to-number
                                        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                                       (string-to-number
                                        (buffer-substring-no-properties (match-beginning 2) (match-end 2)))))))
          (goto-char (point)))
        (cl-reduce (lambda (a b)
                     (let ((tmp-a (+ (car a) (car b)))
                           (tmp-b (+ (cdr a) (cdr b))))
                       (cons tmp-a tmp-b))) results))))
  (defun my:org-buffer-calc-summary--update-summary ()
    (interactive)
    (let ((result (my:org-buffer-calc-summary))
          (saved-point (point)))
      (goto-char (point-min))
      (when (re-search-forward "<[^/]*/[^>]*>")
        (delete-region (match-beginning 0) (match-end 0))
        (let* ((a (car result))
               (b (cdr result))
               (percent (/ (* 100 a) b)))
          (insert "<" (number-to-string a) "/" (number-to-string b) "=" (number-to-string percent) "%>")))
      (goto-char saved-point))
    nil))

(leaf ox-pandoc
  ;; https://taipapamotohus.com/post/org-mode_paper_4/
  :straight t
  ;; 以前は org 側が :after ox-pandoc かつ :config で (org-pandoc-startup-check) を
  ;; 呼んで ox-pandoc を強制ロードしていたが、依存の向きが逆だった。
  ;; org のロード後に ox-pandoc を読む形に直す。
  :after org
  :require t
  :commands org-pandoc-startup-check
  :custom
  `(;; default options for all output formats
    (org-pandoc-options . '((standalone . t)))
    ;; cancel above settings only for 'docx' format
    (org-pandoc-options-for-docx . '((standalone . nil)
                                     (reference-doc . ,(my:pandoc-data-file "custom-reference.docx"))))
    ;; special settings for beamer-pdf and latex-pdf exporters
    (org-pandoc-options-for-beamer-pdf . '((pdf-engine . "xelatex")))
    (org-pandoc-options-for-latex-pdf . '((pdf-engine . "xelatex"))))
  :config
  ;; pandoc の呼び出し中だけ出力側を cp932 にする。
  ;; setq での退避/復元は非局所脱出で復元されないため let 束縛にした。
  (defun my:org-pandoc-run-with-cp932 (orig &rest args)
    (let ((default-process-coding-system '(utf-8 . cp932)))
      (apply orig args)))
  (advice-add 'org-pandoc-run :around #'my:org-pandoc-run-with-cp932))

(leaf ob-mermaid
  :straight t
  :commands org-babel-execute:mermaid)

(leaf org-bullets
  :straight t
  :if window-system
  :custom (org-bullets-bullet-list . '("" "" "" "" "" "" ""))
  :hook (org-mode-hook . org-bullets-mode))

(leaf org-download
  :straight t
  :custom
  (org-download-image-dir . "./img"))

;;; [3] markdown

(leaf markdown-mode
  :straight t
  :mode ("\\.\\(markdown\\|md\\|mkd\\)\\'" . gfm-mode)
  :preface
  (defun my:setup-markdown-mode ()
    (setq line-move-visual nil)
    (setq truncate-lines nil)
    (electric-indent-local-mode -1))
  :bind
  (:markdown-mode-map ("C-c ." . hydra-markdown/body))
  :hook
  (markdown-mode-hook . my:setup-markdown-mode)
  (gfm-mode-hook      . my:setup-markdown-mode)
  :custom
  `(markdown-command . ,(let ((pandoc-options `("-F pandoc-crossref"
                                                "--template=default.html"
                                                "--self-contained"
                                                "-s"
                                                "--from=gfm+footnotes"
                                                "--to=html"
                                                "--metadata"
                                                ,(my:pandoc-data-file "metadata.yml"))))
                          (concat "pandoc " (s-join " " pandoc-options))))
  ;; Typora は Windows のインストールパス直書きだったため、存在するときだけ設定する
  `(markdown-open-command
    . ,(seq-find #'file-executable-p
                 (list "c:/Program Files/Typora/Typora.exe"
                       "/Applications/Typora.app/Contents/MacOS/Typora"
                       "/usr/bin/typora")))
  (markdown-use-pandoc-style-yaml-metadata . t)
  (markdown-header-scaling . nil)
  :hydra
  (hydra-markdown (:hint nil :exit t)
                  "
^Format^      ^Insert^        ^Head.Foot^     ^Code.Link^      ^Move^           ^Pndoc
^^^^^^-----------------------------------------------------------------------------------
_s_torong     _b_lockquote    H1~H6:_a_uto    _c_ode block     _p_romote        _H_tml
italic:_/_    pre:_:_         _f_ootnote      code i_n_line    _d_emote         _P_DF
リスト:_._    _t_able         _r_eference     _l_ink           _j_:move-up      _D_ocx
取消線:_x_    hr:_-_          _i_mage         _u_ri            _k_:move-down    Pre_v_iew"
                  ("s" markdown-insert-bold)
                  ("/" markdown-insert-italic)
                  ("-" markdown-insert-hr)
                  ("x" markdown-insert-strike-through)
                  ("b" markdown-insert-blockquote)
                  (":" markdown-insert-pre)
                  ("t" markdown-insert-table)
                  ("c" markdown-insert-gfm-code-block)
                  ("n" markdown-insert-code)
                  ("K" markdown-insert-kbd)
                  ("a" markdown-insert-header-dwim)
                  ("1" markdown-insert-header-atx-1)
                  ("2" markdown-insert-header-atx-2)
                  ("3" markdown-insert-header-atx-3)
                  ("4" markdown-insert-header-atx-4)
                  ("5" markdown-insert-header-atx-5)
                  ("6" markdown-insert-header-atx-6)
                  ("." markdown-insert-list-item)
                  ("i" markdown-insert-image)
                  ("l" markdown-insert-link)
                  ("u" markdown-insert-uri)
                  ("f" markdown-insert-footnote)
                  ("r" markdown-insert-reference-link-dwim)
                  ("p" markdown-promote)
                  ("d" markdown-demote)
                  ("j" markdown-move-down)
                  ("k" markdown-move-up)
                  ;; Pandoc (TODO)
                  ("H" md2html :exit t)
                  ("P" md2pdf :exit t)
                  ("D" md2docx :exit t)
                  ("v" markdown-preview :exit t)))

;;; [3] ReST

(leaf rst
  :mode ("\\.\\(rst|rest\\)$" . rst-mode)
  :bind
  (:rst-mode-map
   ;; remove rst-deprecated-* bindings
   ("C-c C-b" . nil)
   ("C-c C-d" . nil)
   ("C-c C-e" . nil)
   ("C-c C-f" . nil)
   ("C-c TAB" . nil)
   ("C-c RET" . nil)
   ("C-c C-n" . nil)
   ("C-c C-p" . nil)
   ("C-c C-s" . nil)
   ("C-c C-u" . nil)
   ("C-c C-v" . nil)
   ("C-c C-w" . nil)
   ("C-c 1" . nil)
   ("C-c 2" . nil)
   ("C-c 3" . nil)
   ("C-c 4" . nil)
   ("C-c 5" . nil)
   ("C-c C-l <t>" . nil)
   ("C-c C-r <t>" . nil)
   ("C-c C-a <t>" . nil))
  :hook (rst-mode-hook . (lambda ()
                           (setq indent-tabs-mode nil)
                           (setq frame-background-mode 'dark))))

;;; [3] asciidoc

(leaf adoc-mode
  :straight t)

(provide 'my-text)
;;; my-text.el ends here
