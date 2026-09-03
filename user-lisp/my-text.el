;;; my-text.el --- テキストモード (org / markdown / rst)  -*- lexical-binding: t -*-
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

(use-package org
  ;; Emacs 31.1 同梱の org (9.8.7) を使う。
  ;; 以前は :straight t で org 9.5.1 (2021年) を入れていたが、
  ;; init.org のタングルで組み込み org が先にロードされるため版が混在していた。
  ;; :mode 指定も組み込みの auto-mode-alist で足りるので外した。
  :preface
  ;; クリップボードの画像を <buffer-file-name>_assets/ に保存する。
  ;; 保存先ディレクトリは org--image-yank-media-handler 側が make-directory
  ;; するので、ここでは名前を返すだけでよい。
  (defun my:org-image-save-directory ()
    "クリップボードから貼り付ける画像の保存先ディレクトリ名を返す。
訪問中のファイル名に \"_assets\" を付けたもの (例: note.org_assets)。"
    (let ((file (buffer-file-name (buffer-base-buffer))))
      (unless file
        (user-error "ファイルを訪問していないバッファには画像を保存できません"))
      (concat file "_assets")))
  (defun my:org-yank-image-filename ()
    "クリップボードから貼り付ける画像のファイル名 (拡張子なし) を返す。
既定の `org-yank-image-autogen-filename' は \"clipboard-...T...%6N\" と
マイクロ秒をドットで繋ぐが、org--image-yank-media-handler が呼ぶ
`file-name-with-extension' はそれを拡張子とみなして落とすため、実際には
秒単位の名前になり、同じ秒に 2 回貼ると 1 枚目が上書きされる。
ドットの代わりにハイフンで繋いでマイクロ秒を残す。"
    (format-time-string "clipboard-%Y%m%dT%H%M%S-%6N"))
  (defun my:org-yank-image (&optional noselect)
    "クリップボードの画像を保存し、リンクを挿入してその場でプレビューする。
保存先は `org-yank-image-save-method' 経由で `my:org-image-save-directory'、
ファイル名は `org-yank-image-file-name-function' が付ける
clipboard-YYYYMMDDTHHMMSS-NNNNNN.png。リンクは
`org-link-file-path-type' が \='adaptive なので相対パスになる。

NOSELECT (前置引数) を付けると MIME 型を選ばせる (`yank-media' と同じ)。"
    (interactive "P")
    (let ((beg (point)))
      (yank-media noselect)
      ;; 挿入されたときだけプレビューする。クリップボードに画像が無ければ
      ;; yank-media はエラーかメッセージだけで戻り、point は動かない。
      (when (> (point) beg)
        (org-link-preview-region nil t beg (point)))))

  ;; --- 保存時に <buffer-file-name>_assets/ との整合性を見る ---
  (defvar my:org-assets-inhibit-check nil
    "非 nil のあいだは `my:org-assets-check-on-save' を何もせずに戻す。
`org-save-all-org-buffers' は 1 時間ごとのタイマーからも呼ばれるので、
その最中に y-or-n-p でユーザーの手を止めないため。")
  (defun my:org-assets--key (file)
    "FILE を比較用に正規化した絶対パスにして返す。
大文字小文字を区別しないファイルシステム (Windows / macOS) では downcase する。"
    (let ((f (file-truename (expand-file-name file))))
      (if (file-name-case-insensitive-p (file-name-directory f))
          (downcase f)
        f)))
  (defun my:org-assets--linked-files ()
    "バッファ内の file: リンクが指す先を `my:org-assets--key' 化して返す。
ナローイングされていてもバッファ全体を見る。見えている範囲だけを見ると
範囲外からリンクされているファイルを消してしまう。"
    (org-with-wide-buffer
     (delete-dups
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (when (equal "file" (org-element-property :type link))
            (my:org-assets--key (org-element-property :path link))))))))
  (defun my:org-assets--mentioned-p (file)
    "FILE のファイル名がバッファ内に文字列として現れるか。
`org-element' はコメント行や例示ブロックの中のリンクを拾わない。
消してしまってからでは遅いので、削除の前にもう一段の保険として見る。"
    (org-with-wide-buffer
     (goto-char (point-min))
     (search-forward (file-name-nondirectory file) nil t)))
  (defun my:org-assets-check-on-save ()
    "保存後に _assets/ ディレクトリとバッファ内のリンクを突き合わせる。
どこからもリンクされていないファイルは 1 つずつ確認のうえごみ箱へ移し、
リンク先が存在しないものは警告するだけで保存自体は成功させる。"
    (unless (or my:org-assets-inhibit-check noninteractive)
      (let ((dir (ignore-errors (my:org-image-save-directory))))
        (when (and dir (file-directory-p dir))
          (let* ((linked (my:org-assets--linked-files))
                 (dirkey (file-name-as-directory (my:org-assets--key dir)))
                 ;; サブディレクトリとドットファイルは対象外。
                 (files (seq-filter
                         (lambda (f)
                           (and (file-regular-p f)
                                (not (string-prefix-p "." (file-name-nondirectory f)))))
                         (directory-files dir t)))
                 (keys (mapcar #'my:org-assets--key files))
                 (orphans (seq-remove
                           (lambda (f)
                             (or (member (my:org-assets--key f) linked)
                                 (my:org-assets--mentioned-p f)))
                           files))
                 (missing (seq-filter
                           (lambda (k)
                             (and (string-prefix-p dirkey k)
                                  (not (member k keys))))
                           linked)))
            (when orphans
              (map-y-or-n-p
               (lambda (f)
                 (format "%s はどこからもリンクされていません。ごみ箱へ移しますか? "
                         (file-name-nondirectory f)))
               (lambda (f)
                 ;; delete-file の TRASH を t にして戻せるようにしておく。
                 (condition-case err
                     (delete-file f t)
                   (error (message "%s を削除できませんでした: %s"
                                   (file-name-nondirectory f)
                                   (error-message-string err)))))
               orphans
               '("ファイル" "ファイル" "ごみ箱へ移す")))
            (when missing
              ;; after-save-hook は write-region の "Wrote ..." より後に走るので、
              ;; ここの message は上書きされずに残る。
              (message "警告: %s にリンク先がありません: %s"
                       (file-name-nondirectory (directory-file-name dir))
                       (mapconcat #'file-name-nondirectory missing ", "))))))))
  (defun my:org-assets-enable-check ()
    "このバッファの `after-save-hook' に `my:org-assets-check-on-save' を足す。"
    (add-hook 'after-save-hook #'my:org-assets-check-on-save nil t))
  :hook ((org-mode-hook . turn-on-font-lock)
         (org-mode-hook . my:org-assets-enable-check))
  ;; M-v (scroll-down-command) を org-mode でだけ潰す。
  ;; スクロールは my-keybind.el の C-z が使える。
  :bind (:map org-mode-map ("M-v" . my:org-yank-image))
  :custom
  ;; クリップボード画像 (と D&D した画像) の保存先。
  ;; 既定の attach (org-attach 管理下) ではなくバッファの隣に置く。
  (org-yank-image-save-method #'my:org-image-save-directory)
  (org-yank-image-file-name-function #'my:org-yank-image-filename)
  ;; org-mode内部のソースを色付けする
  (org-src-fontify-natively t)
  ;; org-modeの開始時に、行の折り返しを無効にする。
  (org-startup-truncated t)
  ;; follow-linkから戻ることを可能とする。
  (org-return-follows-link t)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-log-done t)
  ;; (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
  (org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                            ("STARTED" :foreground "cornflower blue" :weight bold)
                            ("DONE" :foreground "green" :weight bold)
                            ("WAITING" :foreground "orange" :weight bold)
                            ("HOLD" :foreground "magenta" :weight bold)
                            ("CANCELLED" :foreground "green" :weight bold)
                            ("MEETING" :foreground "green" :weight bold)))
  (org-indent-indentation-per-level 0)
  (org-adapt-indentation nil)
  (org-clock-clocked-in-display 'none)
  (org-clock-out-remove-zero-time-clocks t)
  :config
  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 #'org-save-all-org-buffers)
  ;; そのタイマー経由の保存では _assets/ の確認プロンプトを出さない。
  (defun my:org-assets-around-save-all (orig &rest args)
    (let ((my:org-assets-inhibit-check t))
      (apply orig args)))
  (advice-add 'org-save-all-org-buffers :around #'my:org-assets-around-save-all)
  ;; local functions
  ;; アーカイブ先の指定に含まれる #YM を YYYY-MM に置き換える。
  ;; 例: #+ARCHIVE: %s_#YM_archive::  ->  foo.org_2026-08_archive
  ;;
  ;; 旧実装は org-extract-archive-file への :filter-return advice だったが、
  ;; この関数は org 9.8 で削除された。後継の org-archive--compute-location は
  ;; 戻り値が (FILE . HEADING) の cons なので :filter-return は使えない。
  ;;
  ;; そこで :filter-args で入口を押さえる。引数は「::」で区切る前の生の
  ;; 指定文字列なので、戻り値の形に依存しない。org-archive-subtree は
  ;;   (or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location)
  ;; をこの関数に渡すので、#+ARCHIVE: や ARCHIVE プロパティ経由の指定も
  ;; まとめてカバーできる (org-archive-all-* からの呼び出しも同様)。
  ;;
  ;; プライベート関数 (--) を advise しているが、渡すのも受け取るのも
  ;; ただの文字列なので結合度は低い。
  (defun my:org-archive-expand-ym (args)
    "アーカイブ先指定 (ARGS の第 1 要素) の #YM を YYYY-MM に置き換える。"
    (cons (replace-regexp-in-string "#YM" (format-time-string "%Y-%m")
                                    (car args))
          (cdr args)))
  (with-eval-after-load 'org-archive
    (advice-add 'org-archive--compute-location
                :filter-args #'my:org-archive-expand-ym))
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

(use-package ox-pandoc
  ;; https://taipapamotohus.com/post/org-mode_paper_4/
  :straight t
  ;; 以前は org 側が :after ox-pandoc かつ :config で (org-pandoc-startup-check) を
  ;; 呼んで ox-pandoc を強制ロードしていたが、依存の向きが逆だった。
  ;; org のロード後に ox-pandoc を読む形に直す。
  :after org
  :demand t
  :commands org-pandoc-startup-check
  :custom
  ;; use-package の :custom は値の位置を式として評価する。
  ;; leaf のようにリスト全体をバッククォートする必要はない。
  ;; default options for all output formats
  (org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  ;;
  ;; 計画書の G-4。custom-reference.docx を無条件に reference-doc へ渡していた。
  ;; pandoc は指定されたファイルが無いと
  ;;   custom-reference.docx: withBinaryFile: does not exist
  ;; で exit 1 になり、docx エクスポートがまるごと失敗する。実際この環境には
  ;; pandoc のユーザーデータディレクトリ自体が無い。markdown-command と同じく、
  ;; 在るときだけ渡す。省略すれば pandoc 内蔵の既定スタイルで出力される。
  (org-pandoc-options-for-docx
   (let ((ref (my:pandoc-data-file "custom-reference.docx")))
     (append '((standalone . nil))
             (when (file-readable-p ref) `((reference-doc . ,ref))))))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  :config
  ;; pandoc の呼び出し中だけ出力側を cp932 にする。
  ;; setq での退避/復元は非局所脱出で復元されないため let 束縛にした。
  (defun my:org-pandoc-run-with-cp932 (orig &rest args)
    (let ((default-process-coding-system '(utf-8 . cp932)))
      (apply orig args)))
  (advice-add 'org-pandoc-run :around #'my:org-pandoc-run-with-cp932))

(use-package ob-mermaid
  :straight t
  :commands org-babel-execute:mermaid)

(use-package org-bullets
  :straight t
  :if window-system
  :custom (org-bullets-bullet-list '("" "" "" "" "" "" ""))
  :hook (org-mode-hook . org-bullets-mode))

(use-package org-download
  :straight t
  :defer t
  :custom
  (org-download-image-dir "./img"))

;;; [3] markdown

(use-package markdown-mode
  :straight t
  :mode ("\\.\\(markdown\\|md\\|mkd\\)\\'" . gfm-mode)
  :preface
  (defun my:setup-markdown-mode ()
    (setq line-move-visual nil)
    (setq truncate-lines nil)
    (electric-indent-local-mode -1))
  :bind
  (:map markdown-mode-map ("C-c ." . hydra-markdown/body))
  :hook
  (markdown-mode-hook . my:setup-markdown-mode)
  (gfm-mode-hook      . my:setup-markdown-mode)
  :custom
  ;; 計画書の G-3。
  ;;
  ;;   - --self-contained は pandoc 3 で非推奨。実行すると
  ;;     "[WARNING] Deprecated: --self-contained. Use --embed-resources
  ;;      --standalone instead." が出る。--embed-resources に置き換えた。
  ;;     --standalone (-s) は元から付いているので出力は同一。
  ;;   - --metadata はファイルを読ませるオプションではなく KEY[=VAL] を取る。
  ;;     パスを渡していたため metadata.yml は読まれず、代わりに無意味な
  ;;     メタデータキーが 1 つ作られるだけだった。--metadata-file に直した。
  ;;   - --template / --metadata-file / -F pandoc-crossref は、指すものが無い
  ;;     環境ではコマンド全体を失敗させる。実際この環境には pandoc の
  ;;     ユーザーデータディレクトリ自体が無く、
  ;;       Could not find data file 'templates\default.html'
  ;;     で exit 97 になっていた (markdown のプレビューが動いていなかった)。
  ;;     open-junk-file や Typora と同じく、在るときだけ付ける形にする。
  ;;     テンプレートを外しても -s で pandoc 内蔵の既定テンプレートが使われる。
  (markdown-command
   (let* ((template (my:pandoc-data-file "templates/default.html"))
          (metadata (my:pandoc-data-file "metadata.yml"))
          (pandoc-options
           (append
            (when (executable-find "pandoc-crossref") '("-F pandoc-crossref"))
            (when (file-readable-p template) (list (concat "--template=" template)))
            '("--embed-resources"
              "-s"
              "--from=gfm+footnotes"
              "--to=html")
            (when (file-readable-p metadata) (list (concat "--metadata-file=" metadata))))))
     (concat "pandoc " (s-join " " pandoc-options))))
  ;; C-c C-c o (markdown-open) が起動する外部エディタ。
  ;; markdown-open は「保存してから call-process でこのコマンドにファイル名を
  ;; 渡す」だけなので、pandoc も browse-url も通らない (markdown-preview とは
  ;; 別経路)。
  ;;
  ;; Typora は Windows のインストールパス直書きだったため、存在するときだけ
  ;; 設定していたが、このマシンには入っておらず結果は nil、つまり C-c C-c o は
  ;; "Variable `markdown-open-command' must be set" で常に失敗していた。
  ;; MarkText (~/.local/bin/marktext.cmd) を先頭に置く。
  ;;
  ;; marktext.cmd は start で起動して即座に戻るので call-process は待たない。
  ;; executable-find は Windows では exec-suffixes (.exe .com .bat .cmd ...) を
  ;; 補うので、拡張子なしの "marktext" で .cmd が見つかる。
  (markdown-open-command
   (or (executable-find "marktext")
       (seq-find #'file-executable-p
                 (list "c:/Program Files/Typora/Typora.exe"
                       "/Applications/Typora.app/Contents/MacOS/Typora"
                       "/usr/bin/typora"))))
  (markdown-use-pandoc-style-yaml-metadata t)
  (markdown-header-scaling nil)
  ;; leaf の :hydra は init 時にインライン展開されるので :init に置く。
  :init
  (defhydra hydra-markdown (:hint nil :exit t)
                  "
^Format^      ^Insert^        ^Head.Foot^     ^Code.Link^      ^Move^           ^Pndoc
^^^^^^-----------------------------------------------------------------------------------
_s_torong     _b_lockquote    H1~H6:_a_uto    _c_ode block     _p_romote        _H_tml
italic:_/_    pre:_:_         _f_ootnote      code i_n_line    _d_emote         _P_DF
リスト:_._    _t_able         _r_eference     _l_ink           _j_:move-up      _D_ocx
取消線:_x_    hr:_-_          _i_mage         _u_ri            _k_:move-down    Pre_v_iew
^^^^^^-----------------------------------------------------------------------------------
外部エディタ (MarkText) で開く:_O_"
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
    ("v" markdown-preview :exit t)
    ;; 外部エディタ (markdown-open-command = MarkText) で開く。C-c C-c o と同じ。
    ("O" markdown-open :exit t)))

;;; [3] ReST

(use-package rst
  :mode ("\\.\\(rst|rest\\)$" . rst-mode)
  :bind
  (:map rst-mode-map
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

;;; [3] asciidoc (adoc-mode は削除した)

;; ほとんど使わなくなったため外した。
;; adoc-mode は (require 'cl) を素で書いており (adoc-mode.el:177、コード中の
;; コメントも "I know, I should remove it, I will, eventually")、
;; ロードするだけで "Package cl is deprecated" が出ていた。
;; straight/build/ 配下なのでこちらでは直せない。
;; .adoc / .asciidoc は今後 fundamental-mode で開く。

(provide 'my-text)
;;; my-text.el ends here
