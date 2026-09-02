;;; my-gitd.el --- magit の git 実行を常駐プロセスに肩代わりさせる  -*- lexical-binding: t -*-

;;; Commentary:

;; magit のリフレッシュが遅い原因は git ではなく、**Emacs の Windows における
;; プロセス生成コスト**である。同じ `cmd.exe' を起動するのに PowerShell が
;; 20ms、Emacs の `call-process' は 60〜76ms かかる (2026-09 実測)。
;; magit の refresh 1 回は git を 29 回起動するので、1.7 秒のうち 7 割以上が
;; 「git.exe が起動するのを待っている時間」になっている。
;;
;; そこで `magit-process-file' を横取りし、常駐させた Rust プロセス
;; (gitd/、`magit-gitd.exe') に git の実行を肩代わりさせる。
;; Rust からの spawn は Emacs の約半分で済むため、refresh が 2.4 倍速くなる。
;;
;;   実測 (~/.emacs.d、refresh 相当の 29 コマンド):
;;     素の process-file  1602 ms
;;     デーモン経由        656 ms
;;     stdio の往復        0.13 ms/回 (無視できる)
;;
;; これは段階 2a (素通しプロキシ)。キャッシュも先読みもファイル監視もしない。
;; 設計と実測の詳細は tmp/magit-gitd-2a-design.md を参照。
;;
;; 安全側の作り:
;;   - バイナリが無い / デーモンが死んだ / 形態が未知 → 黙って素の
;;     `process-file' に戻る。magit から見て挙動は変わらない。
;;   - タイムアウトを設けない。素の `process-file' にも無いので、
;;     挙動を変えないことが最も安全。ハングは C-g で抜けられる。
;;   - 失敗してフォールバックするとき、git が既に走っていた可能性がある
;;     場合は再実行しない (`git add' の二重実行を防ぐ)。
;;
;; ビルド: M-x my:gitd-build (cargo build --release)

;;; Code:

(require 'jsonrpc)
(require 'seq)
(require 'cl-lib)

(declare-function magit-git-executable "magit-git")
(declare-function magit-process-environment "magit-process")
(declare-function magit--process-coding-system "magit-process")
(declare-function magit-process-git-arguments--split "magit-git")

(defvar my:gitd-mode)                   ; define-minor-mode で定義される

(defgroup my:gitd nil
  "magit の git 実行を常駐プロセスに肩代わりさせる。"
  :group 'magit)

(defcustom my:gitd-directory
  (expand-file-name "gitd/" user-emacs-directory)
  "Rust クレートのディレクトリ。"
  :type 'directory)

(defcustom my:gitd-executable
  (expand-file-name "gitd/target/release/magit-gitd.exe" user-emacs-directory)
  "常駐プロセスの実行ファイル。無ければ機能は単に無効になる。"
  :type 'file)

(defcustom my:gitd-verify nil
  "非 nil なら、デーモン経由と素の `process-file' の両方を実行して結果を比較する。

`my:gitd-read-only-p' が真のコマンドだけが対象 (書き込みを 2 回
走らせるわけにはいかないため)。差異は `my:gitd-verify-buffer' に記録する。
当然遅くなるので、導入直後の検証期間だけ有効にする使い方を想定している。"
  :type 'boolean)

(defconst my:gitd-protocol 1
  "プロトコル版。Rust 側の PROTOCOL と揃える。")

(defconst my:gitd-verify-buffer "*gitd verify*")

(defconst my:gitd-ansi-coding
  ;; Windows の環境変数ブロックと、magit が encode した args は
  ;; ANSI コードページで入っている。`locale-coding-system' はコンソールの
  ;; コードページ (PowerShell 7 だと cp65001) で別物なので使ってはいけない。
  (or (and (eq system-type 'windows-nt)
           (boundp 'w32-ansi-code-page)
           (let ((cs (intern (format "cp%d" w32-ansi-code-page))))
             (and (coding-system-p cs) cs)))
      locale-coding-system)
  "Emacs 内部のバイト列を復号するための coding system。")

;;; ---------------------------------------------------------------- 状態

(defvar my:gitd--conn nil "現在の `jsonrpc-process-connection'。")
(defvar my:gitd--envs nil "登録済み env id のハッシュ表。")
(defvar my:gitd--failures 0 "連続失敗回数。")
(defvar my:gitd--disabled nil "サーキットブレーカが落ちたら非 nil。")
(defvar my:gitd--in-fallback nil "再入防止。素通し実行中は非 nil。")

(defvar my:gitd--stats (list :routed 0 :fallback 0 :daemon-ms 0.0 :saved-ms 0.0)
  "統計。`my:gitd-stats' で表示する。")

(defconst my:gitd--native-spawn-ms 56.0
  "素の `process-file' 1 回あたりのプロセス生成コストの実測値 (ms)。
短縮時間の見積りにしか使わない。")

;;; ---------------------------------------------------------------- 接続

(defun my:gitd--available-p ()
  "デーモンを使える状態なら非 nil。"
  (and (not my:gitd--disabled)
       (not my:gitd--in-fallback)
       (file-executable-p my:gitd-executable)))

(defun my:gitd--live-p ()
  (and my:gitd--conn (jsonrpc-running-p my:gitd--conn)))

(defun my:gitd--connect ()
  "デーモンを起動して接続する。成功したら接続を返す。"
  (let* ((name "gitd")
         (proc (make-process
                :name name
                :command (list my:gitd-executable)
                :connection-type 'pipe
                :coding 'utf-8-emacs-unix
                :noquery t
                :stderr (get-buffer-create (format "*%s stderr*" name))))
         (conn (make-instance 'jsonrpc-process-connection
                              :name name :process proc)))
    (let ((r (jsonrpc-request conn 'initialize
                              (list :protocol my:gitd-protocol) :timeout nil)))
      (unless (equal (plist-get r :protocol) my:gitd-protocol)
        (jsonrpc-shutdown conn)
        (setq my:gitd--disabled t)
        (error "gitd: プロトコル不一致 (Emacs %s / バイナリ %s)。M-x my:gitd-build で作り直してください"
               my:gitd-protocol (plist-get r :protocol)))
      (setq my:gitd--envs (make-hash-table :test #'equal))
      (setq my:gitd--conn conn))))

(defun my:gitd--ensure ()
  "接続を用意して返す。用意できなければ nil。"
  (when (my:gitd--available-p)
    (or (and (my:gitd--live-p) my:gitd--conn)
        (condition-case err
            (my:gitd--connect)
          (error (my:gitd--note-failure err) nil)))))

(defun my:gitd--note-failure (err)
  (setq my:gitd--failures (1+ my:gitd--failures))
  (when (>= my:gitd--failures 3)
    (setq my:gitd--disabled t)
    (message "gitd: 3 回続けて失敗したのでこのセッションでは使いません (%s)。M-x my:gitd-restart で再開"
             (error-message-string err))))

;;;###autoload
(defun my:gitd-restart ()
  "デーモンを止めて状態を初期化する。次のアクセスで再接続する。"
  (interactive)
  (when (my:gitd--live-p)
    (ignore-errors (jsonrpc-shutdown my:gitd--conn)))
  (setq my:gitd--conn nil
        my:gitd--envs nil
        my:gitd--failures 0
        my:gitd--disabled nil)
  (message "gitd: 再起動しました"))

(defun my:gitd--shutdown-on-exit ()
  "デーモンを止める。
まず shutdown を送って自分から終了させる。そうしないと jsonrpc が
sentinel を待って \"still hasn't run\" の警告を出す。"
  (when (my:gitd--live-p)
    (ignore-errors (jsonrpc-request my:gitd--conn 'shutdown nil :timeout 1))
    (ignore-errors (jsonrpc-shutdown my:gitd--conn))))

(add-hook 'kill-emacs-hook #'my:gitd--shutdown-on-exit)

;;; ---------------------------------------------------------------- 文字列

(defun my:gitd--to-text (s)
  "Emacs 内部の文字列 S を JSON に載る Unicode 文字列にする。

Windows では 2 か所でバイト列が混ざってくる:

1. `magit-process-git-arguments' は args を `w32-ansi-code-page' で
   encode してから `process-file' に渡す (magit issue #3250)。
   Emacs の `call-process' が ANSI API を使うため。
2. `process-environment' には ANSI のまま復号されていない項目がある
   (例: OneDrive の \"ドキュメント\" を含む PSModulePath)。

どちらも `json-serialize' が受け付けない。同じ ANSI コードページで復号して
Unicode に戻す。Rust 側はワイド API で子プロセスを起動するので、
これで元の文字が復元される。"
  (cond
   ((not (stringp s)) s)
   ((not (multibyte-string-p s)) (decode-coding-string s my:gitd-ansi-coding))
   ((string-match-p "[\x3FFF80-\x3FFFFF]" s)
    (decode-coding-string (encode-coding-string s 'raw-text) my:gitd-ansi-coding))
   (t s)))

(defun my:gitd--env-id (conn)
  "現在の `magit-process-environment' をデーモンに登録して id を返す。"
  (let* ((env (magit-process-environment))
         (id (format "e%x" (sxhash-equal env))))
    (unless (gethash id my:gitd--envs)
      (jsonrpc-request conn 'env/register
                       (list :id id
                             :env (vconcat (mapcar #'my:gitd--to-text env)))
                       :timeout nil)
      (puthash id t my:gitd--envs))
    id))

;;; ---------------------------------------------------------------- 判定

(defconst my:gitd--read-only-subcommands
  '("rev-parse" "symbolic-ref" "describe" "status" "diff" "show" "log"
    "for-each-ref" "show-ref" "ls-files" "ls-tree" "cat-file" "merge-base"
    "rev-list" "var" "name-rev" "check-ignore" "check-attr" "count-objects"
    "branch" "tag" "remote" "config" "stash" "update-index")
  "失敗したときに素通しで再実行してよいサブコマンド。

ルーティングの可否ではなく **再実行してよいか** の判定に使う。
デーモンが応答前に死んだ場合、git が既に走ったかどうかは分からない。
読み取りなら再実行は無害だが、`git add' を 2 回走らせるわけにはいかない。

`config' と `branch' / `tag' / `remote' は書き込み形もあるので
`my:gitd-read-only-p' 側で引数を見て弾く。
2b でキャッシュ許可リストにするときはこのリストを土台にする。")

(defun my:gitd--local-args (program args)
  "ARGS から `magit-git-global-arguments' と \"-c X\" を読み飛ばした残りを返す。"
  (and (fboundp 'magit-process-git-arguments--split)
       (cadr (magit-process-git-arguments--split program args))))

(defun my:gitd--subcommand (program args)
  "サブコマンド名を返す。分からなければ nil (= 安全側に倒す)。"
  (car (my:gitd--local-args program args)))

(defun my:gitd-read-only-p (program args)
  "PROGRAM ARGS が読み取り専用で、再実行しても安全なら非 nil。"
  (let* ((local (my:gitd--local-args program args))
         (sub (car local))
         (rest (cdr local)))
    (and (member sub my:gitd--read-only-subcommands)
         (pcase sub
           ;; 書き込み形を持つものは引数で絞る
           ("config" (seq-some (lambda (a) (member a '("--list" "--get" "--get-all"
                                                       "--get-regexp")))
                               rest))
           ((or "branch" "tag" "remote") (and (member "--list" rest) t))
           ("stash" (equal rest '("list")))
           ;; .git/index の stat キャッシュを書き換えるが冪等なので
           ;; 二重実行は無害。2b ではキャッシュ対象から外すこと。
           ("update-index" (equal rest '("--refresh")))
           (_ t))
         t)))

(defun my:gitd--known-buffer-form-p (buffer)
  "BUFFER が再現できる形なら非 nil。

`magit-run-gitk' は BUFFER に整数 (0 = 非同期・出力破棄) を渡してくる。
同期実行すると gitk のウィンドウを閉じるまで Emacs が固まるので、
整数は必ず弾く。"
  (or (null buffer)
      (bufferp buffer)
      (and (consp buffer)
           (eq (car buffer) t)
           (or (null (cadr buffer)) (stringp (cadr buffer)))
           (null (cddr buffer)))))

(defun my:gitd--routable-p (program infile buffer display)
  (and my:gitd-mode
       (my:gitd--available-p)
       (null infile)                            ; 標準入力は扱わない
       (null display)
       (not (file-remote-p default-directory))
       (fboundp 'magit-git-executable)
       (equal program (magit-git-executable))   ; gitk / shell は除外
       (my:gitd--known-buffer-form-p buffer)))

;;; ---------------------------------------------------------------- 実行

(defun my:gitd--request (conn program args want-stderr)
  "デーモンに git を実行させて (EXIT STDOUT STDERR) を返す。
STDOUT / STDERR は生バイト (unibyte 文字列)。"
  (let* ((send (lambda (id)
                 (jsonrpc-request
                  conn 'git/run
                  (list :program (my:gitd--to-text program)
                        :cwd (my:gitd--to-text default-directory)
                        :args (vconcat (mapcar #'my:gitd--to-text args))
                        :env id
                        :want_stderr (if want-stderr t :json-false))
                  :timeout nil)))
         (id (my:gitd--env-id conn))
         (r (condition-case err
                (funcall send id)
              (jsonrpc-error
               ;; env が失われていたら 1 度だけ登録し直して再送する。
               ;; このエラーは git が起動する前に返るので再送は安全。
               (if (eq (alist-get 'jsonrpc-error-code (cdr err)) -32001)
                   (progn (remhash id my:gitd--envs)
                          (funcall send (my:gitd--env-id conn)))
                 (signal (car err) (cdr err)))))))
    (list (plist-get r :exit)
          (base64-decode-string (plist-get r :stdout))
          (and want-stderr (plist-get r :stderr)
               (base64-decode-string (plist-get r :stderr))))))

(defun my:gitd--emit (buffer stdout stderr)
  "素の `process-file' と同じように STDOUT / STDERR を BUFFER に出す。"
  (let ((text (and stdout (decode-coding-string
                           stdout (car (magit--process-coding-system))))))
    (pcase buffer
      ('nil nil)                                   ; 全部捨てる
      ((pred bufferp) (with-current-buffer buffer (insert text)))
      (`(t nil) (insert text))                     ; カレントバッファの point へ
      (`(t ,(and file (pred stringp)))
       (insert text)
       ;; stderr はデコードせず生バイトのまま書く (process-file と同じ)
       (let ((coding-system-for-write 'binary))
         (write-region (or stderr "") nil file nil 'silent))))))

;;; ---------------------------------------------------------------- 検証

(defun my:gitd--verify (program args exit stdout)
  "同じコマンドを素の `process-file' でも実行してバイト単位で比較する。"
  (let* ((native
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let* ((coding-system-for-read 'binary)
                   (coding-system-for-write 'binary)
                   (process-environment (magit-process-environment))
                   (my:gitd--in-fallback t)
                   (e (apply #'process-file program nil '(t nil) nil args)))
              (cons e (buffer-string)))))
         (ok (and (equal exit (car native)) (equal stdout (cdr native)))))
    (unless ok
      (with-current-buffer (get-buffer-create my:gitd-verify-buffer)
        (goto-char (point-max))
        (insert (format "\n=== 不一致 %s ===\ncwd  : %s\nargs : %s\nexit : daemon=%s native=%s\nlen  : daemon=%d native=%d\n"
                        (format-time-string "%F %T") default-directory
                        (mapconcat #'identity args " ")
                        exit (car native)
                        (length stdout) (length (cdr native))))))
    ok))

;;; ---------------------------------------------------------------- advice

(defun my:gitd--process-file (orig program &optional infile buffer display &rest args)
  "`magit-process-file' の :around。可能ならデーモンに肩代わりさせる。

`quit' (C-g) は捕まえない。`condition-case' の `error' は `quit' を
拾わないので、そのまま呼び出し元に抜ける。素の `process-file' と同じ挙動。"
  (let ((conn (and (my:gitd--routable-p program infile buffer display)
                   (my:gitd--ensure))))
    (if (not conn)
        (apply orig program infile buffer display args)
      (condition-case err
          (let* ((want-stderr (and (consp buffer) (stringp (cadr buffer))))
                 (t0 (float-time))
                 (r (my:gitd--request conn program args want-stderr))
                 (ms (* 1000 (- (float-time) t0))))
            (setq my:gitd--failures 0)
            (cl-incf (plist-get my:gitd--stats :routed))
            (cl-incf (plist-get my:gitd--stats :daemon-ms) ms)
            (cl-incf (plist-get my:gitd--stats :saved-ms)
                     (max 0 (- my:gitd--native-spawn-ms ms)))
            (when (and my:gitd-verify (my:gitd-read-only-p program args))
              (my:gitd--verify program args (nth 0 r) (nth 1 r)))
            (my:gitd--emit buffer (nth 1 r) (nth 2 r))
            (nth 0 r))
        (error
         (my:gitd--note-failure err)
         (cl-incf (plist-get my:gitd--stats :fallback))
         ;; デーモンが応答前に死んだ場合、git が既に走ったかは分からない。
         ;; 読み取り専用なら素通しで再実行してよい。それ以外は
         ;; 二重実行を避けるため、エラーをそのまま magit に返す。
         (if (my:gitd-read-only-p program args)
             (let ((my:gitd--in-fallback t))
               (apply orig program infile buffer display args))
           (message "gitd: %s の実行結果が不明です。二重実行を避けるため再実行しません"
                    (my:gitd--subcommand program args))
           (signal (car err) (cdr err))))))))

;;; ---------------------------------------------------------------- コマンド

;;;###autoload
(defun my:gitd-build ()
  "`gitd/' を cargo でビルドする。tree-sitter の文法と同じく各マシンで作る。"
  (interactive)
  (unless (executable-find "cargo")
    (user-error "cargo が見つかりません。rustup を入れてください"))
  (let ((default-directory my:gitd-directory))
    (compile "cargo build --release")))

;;;###autoload
(defun my:gitd-stats ()
  "ルーティング数・フォールバック数・短縮時間を表示する。"
  (interactive)
  (let ((routed (plist-get my:gitd--stats :routed)))
    (message "gitd: %d 回経由 / %d 回フォールバック / 平均 %.1f ms / 累計短縮 %.1f 秒%s"
             routed
             (plist-get my:gitd--stats :fallback)
             (if (zerop routed) 0.0 (/ (plist-get my:gitd--stats :daemon-ms) routed))
             (/ (plist-get my:gitd--stats :saved-ms) 1000.0)
             (cond (my:gitd--disabled "  [停止中]")
                   ((my:gitd--live-p) "")
                   (t "  [未接続]")))))

;;;###autoload
(define-minor-mode my:gitd-mode
  "magit の同期 git 実行を常駐プロセスに肩代わりさせる。"
  :global t
  :lighter nil
  (if my:gitd-mode
      (progn
        (setq my:gitd--failures 0 my:gitd--disabled nil)
        (advice-add 'magit-process-file :around #'my:gitd--process-file))
    (advice-remove 'magit-process-file #'my:gitd--process-file)
    (my:gitd--shutdown-on-exit)
    (setq my:gitd--conn nil my:gitd--envs nil)))

;; 対象は Windows のみ。macOS / Linux はプロセス生成が速いので入れる意味が薄い。
;; バイナリが無ければ `my:gitd-mode' は何もしない (`my:gitd--available-p' が
;; nil を返して素通しになる) ので、まだ M-x my:gitd-build していないマシンでは
;; 自動的に従来動作になる。
(when (eq system-type 'windows-nt)
  (my:gitd-mode 1))

(provide 'my-gitd)
;;; my-gitd.el ends here
