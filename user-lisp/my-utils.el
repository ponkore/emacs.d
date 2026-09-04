;;; my-utils.el --- ユーティリティ  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; ユーティリティ
;;; --------------------------------------------------

;;; [3] Calendar

(use-package calendar
  ;; 組み込みライブラリなのでインストール指定は不要。
  ;; leaf は (require) を出さなかったので :defer t で揃える。
  :defer t
  :custom
  (mark-holidays-in-calendar t) ; 祝日をカレンダーに表示
  (calendar-month-name-array ["01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" ])
  (calendar-day-name-array   ["日" "月" "火" "水" "木" "金" "土"])
  (calendar-day-header-array ["日" "月" "火" "水" "木" "金" "土"])
  (calendar-week-start-day   0)) ;; 日曜開始

(use-package japanese-holidays
  :straight t
  :custom
  (japanese-holiday-weekend '(0 6)) ; 土日を祝日として表示
  (japanese-holiday-weekend-marker '(holiday nil nil nil nil nil japanese-holiday-saturday)) ; 土曜日を水色で表示
  ;;    `((calendar-holidays . ,(append japanese-holidays holiday-local-holidays holiday-other-holidays))) ; 他の国の祝日も表示させたい場合は適当に調整
  :hook
  (calendar-today-visible-hook . japanese-holiday-mark-weekend)
  (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
  (calendar-today-visible-hook . calendar-mark-today))

;;; [3] open-junk-file

(use-package open-junk-file
  :straight t
  :bind ("C-x j" . open-junk-file)
  :custom
  ;; macOS の Dropbox パス直書きだったため Windows / Linux で壊れていた。
  ;; 存在する Dropbox ディレクトリを探し、無ければ ~/junk/ にする。
  (open-junk-file-format
   (concat (or (seq-find #'file-directory-p
                         (list (expand-file-name "~/Library/CloudStorage/Dropbox-個人用")
                               (expand-file-name "~/Dropbox")))
               (expand-file-name "~"))
           "/junk/%Y-%m-%d-%H%M%S.")))

;;; [3] dashboard

(use-package dashboard
  ;; :when (version<= "25.1" emacs-version)
  :when nil
  :straight t
  :custom ((dashboard-items '((recents . 15)
                              (projects . 5)
                              (bookmarks . 5)
                              ;; (agenda . 5)
                              )))
  :config
  (dashboard-setup-startup-hook))

;;; [3] 再帰的に grep

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
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
    (let (;; 【重要】コマンド行の書き込み側を ANSI コードページに固定する。
          ;;
          ;; grep は compilation-start 経由で shell-file-name (bash.exe) に
          ;; "-c コマンド行" を渡す。Emacs はプロセスを ANSI API で起動する
          ;; ので、コマンド行は cp932 でエンコードされていなければならない。
          ;; UTF-8 のまま渡すと受け取り側で cp932 として解釈され、日本語の
          ;; 検索語が化けて「一致なし」になる (エラーにはならない)。
          ;;
          ;; default-process-coding-system だけでは効かない。my-shell.el の
          ;;   (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
          ;; が process-coding-system-alist を通じて car/cdr とも utf-8 に
          ;; 固定し、そちらが優先されるため。coding-system-for-write は
          ;; それより強い。
          ;;
          ;; grep は標準入力を使わない (/dev/null を渡している) ので、
          ;; 書き込み側を変えても副作用は無い。M-! / M-| や M-x shell の
          ;; 標準入力は utf-8 のまま (alist を触っていないので不変)。
          ;; 非 Windows では locale-coding-system が utf-8 なので no-op。
          (coding-system-for-write locale-coding-system)
          (default-process-coding-system '(utf-8 . cp932))
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

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :init
  ;; 【重要】ripgrep も grep とまったく同じ理由でコマンド行を ANSI コード
  ;; ページで書かなければならない (my:grep-with-cp932 のコメント参照)。
  ;;
  ;; 以前は my:ripgrep-regexp (dired の G) が本家 ripgrep-regexp をコピーして
  ;; その中でだけ束縛していたため、本家を呼ぶ経路 —— projectile-ripgrep
  ;; (C-c p s の my:projectile-search-dwim) と M-x ripgrep-regexp —— が
  ;; 漏れていた。日本語の検索語がエラーも出さずに 0 件になる。
  ;; 入口である ripgrep-regexp に advice を張って全経路をまとめて押さえる。
  ;;
  ;; ripgrep-regexp は autoload なので、定義前にここで張っておけば
  ;; ripgrep.el がロードされた時点で引き継がれる。
  ;; ripgrep は標準入力を使わないので書き込み側を変えても副作用は無い。
  (defun my:ripgrep-with-cp932 (orig &rest args)
    "コマンド行を ANSI コードページで書いた上で ORIG を ARGS で呼ぶ。"
    (let ((coding-system-for-write locale-coding-system))
      (apply orig args)))
  (advice-add 'ripgrep-regexp :around #'my:ripgrep-with-cp932)

  (defun my:ripgrep-regexp (regexp &optional args)
    "Run a ripgrep search with `REGEXP' rooted at the current dired directory.
`ARGS' provides Ripgrep command line arguments."
    (interactive
     (list (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
    ;; 本家は検索ディレクトリも対話的に聞いてくる。dired ではそのバッファの
    ;; ディレクトリで検索したいので、それを渡すだけの薄いラッパにしてある。
    ;; cp932 化は上の advice が担当するので、ここで束縛する必要は無い。
    (ripgrep-regexp regexp (dired-current-directory) args)))

;;; [3] 自分の Blog (myblog-hugo) 用のものは削除した

;; 計画書の J-9。Hugo のドラフト作成・公開を行う myblog-hugo/* 一式
;; (create-draft / publish / get-shortname / apply-current-time /
;;  apply-shortname と、base-directory-format-string / draft-directory /
;;  draft-template の 3 変数) を削除した。元のコメントにも「あまり使ってない」と
;; あり、使わなくなったため。
;;
;; あわせて my-keybind.el の ("C-x n" . myblog-hugo/create-draft) も外した。
;; これが narrowing のプレフィックス (C-x n n / w / d) を丸ごと潰しており、
;; widen がどのキーからも呼べない状態になっていた。

(provide 'my-utils)
;;; my-utils.el ends here
