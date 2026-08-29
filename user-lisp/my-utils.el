;;; my-utils.el --- ユーティリティ  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; --------------------------------------------------
;;; ユーティリティ
;;; --------------------------------------------------

;;; [3] Calendar

(leaf calendar
  ;; 組み込みライブラリなのでインストール指定は不要
  :custom
  (mark-holidays-in-calendar . t) ; 祝日をカレンダーに表示
  (calendar-month-name-array . ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" ])
  (calendar-day-name-array   . ["日" "月" "火" "水" "木" "金" "土"])
  (calendar-day-header-array . ["日" "月" "火" "水" "木" "金" "土"])
  (calendar-week-start-day   . 0)) ;; 日曜開始

(leaf japanese-holidays
  :straight t
  :custom
  (japanese-holiday-weekend . '(0 6)) ; 土日を祝日として表示
  (japanese-holiday-weekend-marker . '(holiday nil nil nil nil nil japanese-holiday-saturday)) ; 土曜日を水色で表示
  ;;    `((calendar-holidays . ,(append japanese-holidays holiday-local-holidays holiday-other-holidays))) ; 他の国の祝日も表示させたい場合は適当に調整
  :hook
  (calendar-today-visible-hook . japanese-holiday-mark-weekend)
  (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
  (calendar-today-visible-hook . calendar-mark-today))

;;; [3] open-junk-file

(leaf open-junk-file
  :straight t
  :bind ("C-x j" . open-junk-file)
  :custom
  ;; macOS の Dropbox パス直書きだったため Windows / Linux で壊れていた。
  ;; 存在する Dropbox ディレクトリを探し、無ければ ~/junk/ にする。
  `(open-junk-file-format
    . ,(concat (or (seq-find #'file-directory-p
                             (list (expand-file-name "~/Library/CloudStorage/Dropbox-個人用")
                                   (expand-file-name "~/Dropbox")))
                   (expand-file-name "~"))
               "/junk/%Y-%m-%d-%H%M%S.")))

;;; [3] dashboard

(leaf dashboard
  ;; :when (version<= "25.1" emacs-version)
  :when nil
  :straight t
  :custom ((dashboard-items . '((recents . 15)
                                (projects . 5)
                                (bookmarks . 5)
                                ;; (agenda . 5)
                                )))
  :config
  (dashboard-setup-startup-hook))

;;; [3] Google検索

(leaf google-search
  :config
  ;;
  ;; Google Search via Browser
  ;;
  (require 'browse-url)
  (require 'thingatpt)

  ;; w3m-url-encode-string の rename 版 (w3m.el を入れてないから)
  (defun my-url-encode-string (str &optional coding)
    (apply (function concat)
           (mapcar
            (lambda (ch)
              (cond
               ((eq ch ?\n)               ; newline
                "%0D%0A")
               ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
                (char-to-string ch))      ; printable
               ((char-equal ch ?\x20)     ; space
                "+")
               (t
                (format "%%%02X" ch))))   ; escape
            ;; Coerce a string to a list of chars.
            (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                    nil))))

  ;; google で検索。引数無しだと mini-buffer で編集できる。
  (defun google (str &optional flag)
    "google で検索。引数無しだと mini-buffer で編集できる。"
    (interactive
     (list (cond ((or
                   ;; mouse drag の後で呼び出された場合
                   (eq last-command 'mouse-drag-region)
                   ;; region が活性
                   (and transient-mark-mode mark-active)
                   ;; point と mark を入れ替えた後
                   (eq last-command 'exchange-point-and-mark))
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))
                 (t (thing-at-point 'word)))
           current-prefix-arg))
    (unless flag
      (setq str (read-from-minibuffer "Search word: " str)))
    (browse-url
     (concat
      "http://www.google.com/search?q="
      (my-url-encode-string str 'shift_jis)
      "&hl=ja&ie=Shift_JIS&lr=lang_ja"))))

;;; [3] 再帰的に grep

(leaf grep-r
  :config
  ;; 再帰的にgrep
  (require 'grep)

  ;; `grep-command' は文字列でなければならない。
  ;; 以前はここに (COMMAND . POSITION) の cons を代入し、あわせて組み込みの
  ;; `grep-default-command' を (car grep-command) を返すように再定義していたが、
  ;; Emacs 31 の grep.el は `grep-command' を `string-match' に直接渡すため
  ;; `M-x grep' が壊れていた。
  ;; ミニバッファのカーソル位置指定は Emacs 30 以降の `grep-command-position'
  ;; を使う。prefix 引数でカーソル位置の語を埋める挙動は組み込みの
  ;; `grep-default-command' が元々備えているので、再定義は不要。
  ;;
  ;; コマンド名は絶対パスではなく基底名だけを埋める。
  ;; executable-find が返す Windows の絶対パス
  ;; ("c:/Program Files/Git/usr/bin/grep.exe") には空白とドライブレターが
  ;; 含まれ、shell-quote-argument で括っても実行するシェル (MSYS の bash か
  ;; cmdproxy か) によって解釈が変わって失敗していた。
  ;; コマンドはシェルが PATH から解決すればよいので絶対パスは不要。
  ;; executable-find は yagrep と grep のどちらを使うかの判定にだけ使う。
  (when-let* ((cmd (cond ((executable-find "yagrep") "yagrep")
                         ((executable-find "grep")   "grep")))
              (prefix (concat cmd " -nH -r -e ")))
    (setq grep-command (concat prefix " ."))
    (setq grep-command-position (1+ (length prefix))))

  ;; (defadvice grep (around grep-coding-system-setup compile)
  ;;   "When a prefix argument given, specify coding-system-for-read."
  ;;   (let ((coding-system-for-read
  ;;          (if current-prefix-arg
  ;;              (read-coding-system "coding system: ")
  ;;            coding-system-for-read)))
  ;;     ad-do-it))

  ;; (defadvice grep (around grep-coding-system-setup compile)
  ;;   "When a prefix argument given, specify coding-system-for-read."
  ;;   (let ((coding-system-for-read 'utf-8))
  ;;     ad-do-it))
  ;; grep 実行中だけ出力を cp932 として読み、null-device を Unix 形式にする。
  ;; 旧コードは null-device を復元しておらず (復元行がコメントアウトされていた)、
  ;; 一度 M-x grep するとセッション全体で null-device が "/dev/null" のままだった。
  ;; let 束縛にしたので両方とも確実に元へ戻る。
  (defun my:grep-with-cp932 (orig &rest args)
    (let ((default-process-coding-system '(utf-8 . cp932))
          ;; grep をどのシェル経由で動かすかで null デバイス名が変わる。
          ;; bash/sh 経由なら /dev/null、cmd (cmdproxy) なら NUL。
          ;; 以前は無条件で "/dev/null" にしていたため、cmd 側で動くと
          ;; コマンド末尾に /dev/null が付いて失敗していた。
          (null-device (if (string-match-p "\\(?:ba\\)?sh\\(?:\\.exe\\)?\\'"
                                           (or shell-file-name ""))
                           "/dev/null"
                         null-device)))
      (apply orig args)))
  (advice-add 'grep :around #'my:grep-with-cp932))

;;; [3] ripgrep

(leaf ripgrep*
  :init
  (defun my:ripgrep-regexp (regexp &optional args)
    "Run a ripgrep search with `REGEXP' rooted at `.'.
`ARGS' provides Ripgrep command line arguments."
    (interactive
     (list (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
    (let ((default-directory (dired-current-directory)))
      (compilation-start
       (mapconcat 'identity
                  (append (list ripgrep-executable)
                          ripgrep-arguments
                          args
                          ripgrep--base-arguments
                          (when ripgrep-highlight-search '("--color=always"))
                          (when (and case-fold-search
                                     (isearch-no-upper-case-p regexp t))
                            '("--ignore-case"))
                          '("--")
                          (list (shell-quote-argument regexp) ".")) " ")
       'ripgrep-search-mode))))

;;; [3] 自分の Blog 記述用に作成したもの（あまり使ってない）

(leaf myblog-hugo
  :config
  (defvar myblog-hugo/base-directory-format-string "~/blog/myblog-hugo/content/post/%Y-%m/%d/"
    "format string for post directory. use this with `format-time-string'")

  (defvar myblog-hugo/draft-directory "~/blog/drafts/"
    "draft directory for myblog-hugo. draft file for markdown, thumbnails")

  (defvar myblog-hugo/draft-template "+++
shortname = \"\"
title = \"\"
description = \"\"
date = \"%Y-%m-%dT%H:%M:%S+09:00\"
categories = [\"Programming\"]
tags = [\"\"]
archives = [\"%Y-%m\"]
url = \"post/%Y-%m/%d/{{shortname}}\"
thumbnail = \"/img/%Y-%m/%d/{{shortname}}.png\"
+++

<!--more-->
"
    "template string for post's default markdown text. use this with `format-time-string', and replace {{post-title}}.")

  (defun myblog-hugo/create-draft ()
    "create a hugo draft file with default template."
    (interactive)
    (let* ((draft-filename (format-time-string "%Y-%m-%d-%H%M%S.md" (current-time)))
           (filename (concat myblog-hugo/draft-directory draft-filename))
           (directory (file-name-directory filename))
           (draft-content myblog-hugo/draft-template)
           (buf (set-buffer (find-file-noselect filename t))))
      (with-current-buffer buf
        (goto-char (point-min))
        (insert draft-content)
        ;; (basic-save-buffer)
        (switch-to-buffer buf)
        (goto-char (point-max)))))

  (defun myblog-hugo/get-shortname ()
    "frontmatter にある shortname を取得する"
    (goto-char (point-min))
    (when (re-search-forward "shortname* = *\"\\(.*\\)\"" nil t)
      (let* ((matched (match-string-no-properties 1)))
        matched)))

  (defun myblog-hugo/apply-current-time (field-name end)
    "frontmatter にある keyword = format の format に現在時刻を適用する。"
    (let* ((left-part (concat "\\(" field-name " *= *\\[?"))
           (right-part (concat "\"" "\\)" "\\(.+\\)" "\\(\"\\]?\\)")))
      (when (re-search-forward (concat left-part right-part) end t)
        (let* ((matched (match-string-no-properties 2))
               (formatted (format-time-string matched (current-time))))
          (replace-match (concat "\\1" formatted "\\3"))
          (goto-char (point-min))))))

  (defun myblog-hugo/apply-shortname (shortname end)
    "frontmatter に含まれる {{shortname}} を置き換える"
    (goto-char (point-min))
    (while (re-search-forward "{{shortname}}" end t)
      (replace-match shortname)))

  (defun myblog-hugo/publish ()
    "publish current draft buffer to hugo post directory."
    (interactive)
    (let* ((end)
           (post-destdir (format-time-string myblog-hugo/base-directory-format-string (current-time)))
           (shortname (downcase (myblog-hugo/get-shortname)))
           (destfile (concat post-destdir "/" shortname ".md")))
      (goto-char (point-min))
      (re-search-forward "\\+\\+\\+")
      (forward-char)
      (re-search-forward "\\+\\+\\+")
      (forward-char -3)
      (setq end (point))
      ;;
      (goto-char (point-min))
      (myblog-hugo/apply-current-time "date" end)
      (myblog-hugo/apply-current-time "archives" end)
      (myblog-hugo/apply-current-time "url" end)
      (myblog-hugo/apply-current-time "thumbnail" end)
      (myblog-hugo/apply-shortname shortname end)
      ;;
      (unless (file-exists-p post-destdir)
        (make-directory post-destdir t))
      (set-visited-file-name destfile)
      (basic-save-buffer))))

(provide 'my-utils)
;;; my-utils.el ends here
