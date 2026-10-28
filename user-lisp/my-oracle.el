;;; my-oracle.el --- oracled 経由で Oracle を使う  -*- lexical-binding: t -*-

;;; Commentary:

;; Go の常駐プロセス `oracled' に Oracle との会話を任せ、Emacs は
;; 「文を送る」「結果を表で見る」「止める」だけをやる。
;;
;;   Emacs ──stdio (JSON-RPC 2.0 / Content-Length)──> oracled ──TCP──> Oracle
;;
;; → 設計: oracled/docs/oracle-client-design.md
;; → 実測: oracled/docs/phase0-findings.md
;;
;; これは Phase 1（最小）。`.sql' バッファからの送信・結果の表・割り込み・
;; トランザクションまで。対話バッファ（SQL> プロンプト）と @file は Phase 2。
;;
;; 触るときに知っておくこと。
;;
;; - 【重要】送信は必ず非同期（`jsonrpc-async-request'）。同期で待つと、
;;   待っている間に `$/cancel' を送れないので割り込みが成立しない
;; - 【重要】NUMBER は**文字列**で来る（精度が落ちないように daemon が
;;   そうしている）。右詰めにするかは列の `:numeric' で決める。値の型で
;;   判定してはいけない
;; - 【重要】DATE / TIMESTAMP はオフセットの付かない ISO-8601 で来る。
;;   go-ora が壁時計の時刻に UTC の札を付けて返すためで、Emacs 側で
;;   地方時に変換すると 9 時間ずれる。そのまま出す
;; - 【重要】文の切り分けは daemon の `sql/split' に任せる。`;' と `/' の
;;   規則を Emacs 側にも書くと lexer が 2 つになって必ず食い違う
;; - SQLcl 版（`my:sql-sqlcl'）は消していない。`ddl' やフルスペックの
;;   スクリプトはあちらを使う

;;; Code:

(require 'jsonrpc)
(require 'auth-source)
(require 'seq)
(require 'subr-x)

(defgroup my:oracle nil
  "oracled 経由の Oracle クライアント。"
  :group 'SQL
  :prefix "my:oracle-")

;;; [1] 設定

(defcustom my:oracle-executable
  (expand-file-name "oracled/oracled.exe" user-emacs-directory)
  "oracled の実行ファイル。無ければこの機能は何もしない。"
  :type 'file)

(defconst my:oracle-protocol 1
  "daemon と一致していないといけないプロトコル版。
不一致なら機能を無効化して `M-x my:oracle-build' を促す（gitd と同じ）。")

(defcustom my:oracle-connections
  '(("XEPDB1" :host "192.168.66.101" :port 1521 :service "XEPDB1"
     :user "TEST2TAX"))
  "接続定義。名前 → 接続先。

【重要】パスワードはここに書かない。`auth-source'（`~/.authinfo.gpg'）
から引き、無ければミニバッファで訊く。daemon へは stdio の RPC で渡すので
コマンドラインにも環境変数にも出ない。

`:confirm-write' が非 nil なら、書き込む文を送る前に確認する（本番 DB 用）。"
  :type '(alist :key-type string :value-type plist))

(defcustom my:oracle-autocommit nil
  "非 nil なら autocommit ON で繋ぐ。
既定は OFF。業務 DB を触るので明示コミット側に倒してある。

OFF のとき daemon は常にトランザクションの中で実行する（go-ora の
autoCommit は既定 true で、tx の外の DML は即コミットされるため）。"
  :type 'boolean)

(defcustom my:oracle-maxrows 1000
  "1 文で取る行数の上限。超えたぶんは `C-c q m' で取る。"
  :type 'integer)

(defcustom my:oracle-chunk 200
  "行を何行ずつ受け取るか（逐次表示の粒度）。"
  :type 'integer)

(defcustom my:oracle-max-column-width 40
  "表示するときの 1 列の最大幅（文字）。超えたら切り詰める。"
  :type 'integer)

(defcustom my:oracle-null-string "-"
  "NULL の表示。値が空のときと見分けるために出す。

【重要】`string-width' と**実際の描画幅**が一致する文字しか使えない。
一致しないと、その列から右が 1 桁ずれる（桁を揃えるのがこの表の目的なので、
ずれると読めなくなる）。判定は次で行う。

  (= (* (string-pixel-width \"a\") (string-width S)) (string-pixel-width S))

実測（HackGen 11.6pt の GUI。ASCII 1 文字 = 8px）。

  - `∅'(U+2205) / `⌀' / `␀'  幅 1 と数えるのに **16px で描かれる**。
    Unicode 上 Neutral なので eaw.el も幅 1 とする（eaw.el の表に
    #x2205 が無いのは正しい）。HackGen に字が無く、代替フォントが
    全角で描くためずれる。**既定にしていて実際にずれた**
  - `·' / `•'                幅 2 と数えるのに 8px。逆向きにずれる
  - `ø' / `×' / `¤'          幅 2 / 16px で揃うが、**eaw.el 依存**。
    `my-pty' の端末（ambiguous を幅 1 に倒す）では崩れる
  - `-' / `~' / `_' / `.'    幅 1 / 8px。**フォントにも端末にも依存しない**

曖昧さを嫌うなら \"(null)\" でもよい（6 桁ぶん列が広がる）。"
  :type 'string)

(defface my:oracle-null-face '((t :inherit shadow))
  "NULL の face。")

(defface my:oracle-header-face '((t :inherit font-lock-keyword-face :weight bold))
  "結果の表の見出しの face。")

;;; [2] daemon

(defvar my:oracle--conn nil
  "daemon への `jsonrpc-process-connection'。")

(defvar my:oracle--sessions nil
  "session id → plist（:name :info）の alist。")

(defvar my:oracle--last-session nil
  "最後に自分で選んだ session id。
【重要】送り先はこれで決める。`my-claude' で「取り直すとよその
セッションに書き始める」事故があったので、暗黙に探さない。")

(defun my:oracle--available-p ()
  "daemon のバイナリがあるか。"
  (and my:oracle-executable (file-executable-p my:oracle-executable)))

;;;###autoload
(defun my:oracle-build ()
  "oracled をビルドする。"
  (interactive)
  (let ((dir (expand-file-name "oracled" user-emacs-directory)))
    (unless (file-directory-p dir)
      (user-error "%s が無い" dir))
    (compile (format "cd %s && go build -o oracled.exe ./cmd/oracled"
                     (shell-quote-argument dir)))))

(defun my:oracle--notify (_conn method params)
  "daemon からの通知を捌く。"
  (pcase method
    ('rows/chunk (my:oracle--on-chunk params))
    ('stmt/result (my:oracle--on-result params))
    ('stmt/error (my:oracle--on-error params))
    (_ nil)))

(defun my:oracle--ensure-daemon ()
  "daemon を起こして接続を返す。"
  (unless (my:oracle--available-p)
    (user-error "oracled が無い。M-x my:oracle-build で作る"))
  (when (and my:oracle--conn (not (jsonrpc-running-p my:oracle--conn)))
    (setq my:oracle--conn nil
          my:oracle--sessions nil
          my:oracle--last-session nil))
  (unless my:oracle--conn
    (let* ((default-directory (expand-file-name "~/"))
           (process
            (make-process
             :name "oracled"
             :command (list my:oracle-executable)
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t
             :stderr (get-buffer-create "*oracled stderr*"))))
      (setq my:oracle--conn
            (make-instance 'jsonrpc-process-connection
                           :name "oracled"
                           :process process
                           :notification-dispatcher #'my:oracle--notify))
      (let* ((hello (jsonrpc-request my:oracle--conn 'initialize nil))
             (proto (plist-get hello :protocol)))
        (unless (equal proto my:oracle-protocol)
          (jsonrpc-shutdown my:oracle--conn)
          (setq my:oracle--conn nil)
          (user-error "oracled のプロトコルが %s（期待 %s）。M-x my:oracle-build で作り直す"
                      proto my:oracle-protocol))
        (message "oracled %s (protocol %s, pid %s)"
                 (plist-get hello :version) proto (plist-get hello :pid)))))
  my:oracle--conn)

(defun my:oracle--request (method params)
  "同期で呼ぶ（DB を叩かない軽いものだけ）。"
  (jsonrpc-request (my:oracle--ensure-daemon) method params))

;;; [3] 接続

(defun my:oracle--password (spec)
  "SPEC（接続定義の plist）のパスワードを取る。
`auth-source' を引き、無ければ訊く。**保存はしない。**"
  (let* ((host (plist-get spec :host))
         (user (plist-get spec :user))
         (port (number-to-string (or (plist-get spec :port) 1521)))
         (found (car (auth-source-search :host host :user user :port port :max 1))))
    (if-let* ((secret (plist-get found :secret)))
        (if (functionp secret) (funcall secret) secret)
      (read-passwd (format "%s@%s のパスワード: " user host)))))

;;;###autoload
(defun my:oracle-connect (name)
  "`my:oracle-connections' から選んで接続する。"
  (interactive
   (list (completing-read "接続: " (mapcar #'car my:oracle-connections) nil t
                          nil nil (caar my:oracle-connections))))
  (let* ((spec (cdr (assoc name my:oracle-connections)))
         (_ (unless spec (user-error "接続定義が無い: %s" name)))
         (info (my:oracle--request
                'session/open
                (list :host (plist-get spec :host)
                      :port (or (plist-get spec :port) 1521)
                      :service (or (plist-get spec :service) "")
                      :sid (or (plist-get spec :sid) "")
                      :user (plist-get spec :user)
                      :password (my:oracle--password spec)
                      :autocommit (if my:oracle-autocommit t :json-false))))
         (id (plist-get info :session)))
    (push (cons id (list :name name :info info)) my:oracle--sessions)
    (setq my:oracle--last-session id)
    (my:oracle--log (format "接続 %s: %s@%s %s charset=%s (%.1f ms)"
                            name (plist-get info :user) (plist-get info :db)
                            (plist-get info :version) (plist-get info :charset)
                            (or (plist-get info :connect_ms) 0)))
    (message "%s に接続 (%s@%s, %.1f ms, autocommit %s)"
             name (plist-get info :user) (plist-get info :db)
             (or (plist-get info :connect_ms) 0)
             (if my:oracle-autocommit "ON" "OFF"))
    id))

(defun my:oracle--session ()
  "送り先の session。無ければ接続する。"
  (or (and my:oracle--last-session
           (assoc my:oracle--last-session my:oracle--sessions)
           my:oracle--last-session)
      (my:oracle-connect
       (completing-read "接続: " (mapcar #'car my:oracle-connections) nil t))))

(defun my:oracle--info (&optional id)
  "session の状態を取り直す。"
  (let ((id (or id my:oracle--last-session)))
    (when id
      (let ((info (my:oracle--request 'session/info (list :session id))))
        (setf (plist-get (cdr (assoc id my:oracle--sessions)) :info) info)
        info))))

;;;###autoload
(defun my:oracle-disconnect (&optional id)
  "session を閉じる（daemon 側で明示 ROLLBACK してから）。"
  (interactive)
  (let ((id (or id my:oracle--last-session)))
    (unless id (user-error "繋がっていない"))
    (my:oracle--request 'session/close (list :session id))
    (setq my:oracle--sessions (assoc-delete-all id my:oracle--sessions))
    (when (equal id my:oracle--last-session)
      (setq my:oracle--last-session (caar my:oracle--sessions)))
    (message "%s を閉じた" id)))

;;; [4] バッファ

(defvar-local my:oracle--session-id nil
  "このバッファが見ている session。")

(defun my:oracle--buffer-name (id kind)
  (let ((name (or (plist-get (cdr (assoc id my:oracle--sessions)) :name) id)))
    (format "*oracle(%s)%s*" name (pcase kind ('rows ":rows") ('log ":log") (_ "")))))

(define-derived-mode my:oracle-rows-mode special-mode "Oracle-Rows"
  "結果の表を見るモード。"
  (setq buffer-read-only t
        truncate-lines t))

(defun my:oracle--rows-buffer (id)
  (let ((buf (get-buffer-create (my:oracle--buffer-name id 'rows))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my:oracle-rows-mode)
        (my:oracle-rows-mode))
      (setq my:oracle--session-id id))
    buf))

(defun my:oracle--log-buffer (id)
  (get-buffer-create (my:oracle--buffer-name id 'log)))

(defun my:oracle--log (text)
  "ログバッファに 1 行書く。"
  (let ((id (or my:oracle--last-session "?")))
    (with-current-buffer (my:oracle--log-buffer id)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "%H:%M:%S ") text "\n")))))

;;; [5] 送信

;; 走っている 1 文の状態。daemon は session ごとに 1 文ずつしか
;; 走らせないので、ここも 1 つで足りる。
(defvar my:oracle--pending nil
  "plist: :session :query :text :origin-buffer :origin-pos :columns :rows :start")

(defun my:oracle--busy-p ()
  (and my:oracle--pending (plist-get my:oracle--pending :session)))

(defun my:oracle-statement-at-point ()
  "点のある文を `sql/split' に決めさせて返す。
戻り値は plist（:text :kind :begin :end :complete）。

【重要】ここで正規表現を書かない。`;' と `/' と `q'[]'` と `@file' の
規則は daemon の lexer が 1 つだけ持つ。"
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (pos (1- (point)))            ; daemon は 0 始まりの文字位置
         (res (my:oracle--request 'sql/split (list :text text)))
         (statements (append (plist-get res :statements) nil)))
    (or (seq-find (lambda (st)
                    (and (<= (plist-get st :begin) pos)
                         (< pos (plist-get st :end))))
                  statements)
        ;; 点が文の外（末尾の空行など）なら、手前にある最後の文
        (car (last (seq-filter (lambda (st) (<= (plist-get st :end) (1+ pos)))
                               statements))))))

(defun my:oracle--send (text &optional origin-pos)
  "TEXT を送る。ORIGIN-POS はバッファ内の文の開始位置（エラー位置に使う）。"
  (when (my:oracle--busy-p)
    (user-error "まだ前の文が走っている（C-c C-k で止める）"))
  (let ((id (my:oracle--session)))
    (setq my:oracle--pending
          (list :session id :text text :columns nil :rows nil
                :origin-buffer (current-buffer) :origin-pos origin-pos
                :start (current-time)))
    (my:oracle--log (format "送信: %s" (my:oracle--one-line text)))
    ;; 【重要】非同期。同期で待つと $/cancel を送れない。
    (jsonrpc-async-request
     (my:oracle--ensure-daemon) 'sql/exec
     (list :session id :text text
           :maxrows my:oracle-maxrows :chunk my:oracle-chunk)
     :success-fn #'ignore              ; 結末は stmt/result 通知で受ける
     :error-fn (lambda (err)
                 ;; stmt/error 通知も来るが、通知より先にこちらが来ることも
                 ;; あるので両方で終わらせられるようにしておく
                 (my:oracle--finish-error
                  (or (plist-get err :data)
                      (list :message (plist-get err :message) :position -1))))
     :timeout nil)
    (message "送信中… (C-c C-k で止める)")))

;;;###autoload
(defun my:oracle-send-statement ()
  "点のある文を送る。"
  (interactive)
  (let ((st (my:oracle-statement-at-point)))
    (unless st (user-error "送る文が無い"))
    (when (eq (plist-get st :kind) 'client)
      (user-error "SQL*Plus のコマンドは Phase 2（%s）" (plist-get st :text)))
    (unless (eq (plist-get st :complete) t)
      (unless (y-or-n-p "文が閉じていない（終端子が無い）。それでも送る? ")
        (user-error "中止")))
    (my:oracle--send (plist-get st :text) (1+ (plist-get st :begin)))))

;;;###autoload
(defun my:oracle-send-region (begin end)
  "リージョンをそのまま 1 文として送る。"
  (interactive "r")
  (my:oracle--send (string-trim (buffer-substring-no-properties begin end)) begin))

;;;###autoload
(defun my:oracle-cancel ()
  "走っている文を止める（実測で 165〜260 ms。セッションは残る）。"
  (interactive)
  (let ((id (or (plist-get my:oracle--pending :session) my:oracle--last-session)))
    (unless id (user-error "繋がっていない"))
    (jsonrpc-notify (my:oracle--ensure-daemon) '$/cancel (list :session id))
    (message "割り込みを送った")))

;;;###autoload
(defun my:oracle-more ()
  "止まっているカーソルから続きを取る。"
  (interactive)
  (let ((id (my:oracle--session)))
    (setq my:oracle--pending
          (list :session id :text "(続き)" :columns nil :rows nil
                :append t :start (current-time)))
    (jsonrpc-async-request
     (my:oracle--ensure-daemon) 'rows/more
     (list :session id :maxrows my:oracle-maxrows :chunk my:oracle-chunk)
     :success-fn #'ignore :error-fn #'ignore :timeout nil)))

;;; [6] 通知の受け取り

(defun my:oracle--on-chunk (params)
  "行の束を溜める。表示は stmt/result でまとめて行う。"
  (when my:oracle--pending
    (let ((rows (append (plist-get params :rows) nil)))
      (setf (plist-get my:oracle--pending :rows)
            (append (plist-get my:oracle--pending :rows) rows)))))

(defun my:oracle--on-result (params)
  (let* ((pending my:oracle--pending)
         (id (or (plist-get params :session) (plist-get pending :session)))
         (columns (append (plist-get params :columns) nil))
         (rows (plist-get pending :rows))
         (elapsed (plist-get params :elapsed_ms)))
    (setq my:oracle--pending nil)
    (when (eq (plist-get params :implicit_commit) t)
      (message "この文は暗黙コミットした（DDL なので保留中の変更も確定）"))
    (if (equal (plist-get params :kind) "query")
        (my:oracle--render id columns rows params)
      (my:oracle--log
       (format "完了: %s 行 (%.1f ms)%s"
               (or (plist-get params :affected) 0) (or elapsed 0)
               (if (eq (plist-get params :implicit_commit) t) " ※暗黙コミット" "")))
      (message "%s 行 (%.1f ms)%s"
               (or (plist-get params :affected) 0) (or elapsed 0)
               (my:oracle--tx-suffix params)))
    (when (eq (plist-get params :implicit_commit) t)
      (my:oracle--log "※ DDL により暗黙コミットされた"))))

(defun my:oracle--tx-suffix (params)
  (if (eq (plist-get params :in_tx) t) "  [TX]" ""))

(defun my:oracle--on-error (params)
  (my:oracle--finish-error params))

(defun my:oracle--finish-error (err)
  "エラーを報告し、位置が分かればソースの該当位置へ飛ぶ。"
  (let* ((pending my:oracle--pending)
         (msg (or (plist-get err :message) "?"))
         (code (plist-get err :code))
         (pos (plist-get err :position))
         (buf (plist-get pending :origin-buffer))
         (origin (plist-get pending :origin-pos)))
    (setq my:oracle--pending nil)
    (my:oracle--log (format "エラー: %s" (my:oracle--one-line msg)))
    ;; 【重要】position は 0 始まりの**文字**オフセット（daemon が
    ;; `error occur at position: N' から取っている）
    (when (and pos (>= pos 0) origin (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (min (point-max) (+ origin pos)))))
    (message "%s%s" (if code (format "ORA-%05d " code) "") msg)))

;;; [7] 表示

(defun my:oracle--one-line (s)
  (string-join (split-string (string-trim s) "\n+" t) " ⏎ "))

(defun my:oracle--cell (v)
  "値を表示用の文字列にする。"
  (cond
   ((null v) (propertize my:oracle-null-string 'face 'my:oracle-null-face))
   ((eq v :json-false) "false")
   ((eq v t) "true")
   ((stringp v) (my:oracle--one-line v))
   ((numberp v) (number-to-string v))
   ;; RAW / BLOB は {"$b64": …, "len": N} で来る
   ((and (listp v) (plist-get v :$b64))
    (propertize (format "⟨%s bytes⟩" (plist-get v :len)) 'face 'shadow))
   (t (format "%s" v))))

(defun my:oracle--truncate (s width)
  (if (<= (string-width s) width)
      s
    (truncate-string-to-width s width nil nil "…")))

(defun my:oracle--pad (s width right)
  "S を WIDTH に合わせる。RIGHT が非 nil なら右詰め。
【重要】`length' ではなく `string-width' で数える（日本語は幅 2）。"
  (let ((gap (max 0 (- width (string-width s)))))
    (if right (concat (make-string gap ?\s) s) (concat s (make-string gap ?\s)))))

(defun my:oracle--render (id columns rows params)
  "結果の表を出す。
`vtable' は Phase 2 で検討する（CJK の桁は `string-width' で揃う。
テキストの整形器は書き出しと対話バッファでも要るので、まずこちらを作る）。"
  (let* ((buf (my:oracle--rows-buffer id))
         (names (mapcar (lambda (c) (or (plist-get c :name) "")) columns))
         (numeric (mapcar (lambda (c) (eq (plist-get c :numeric) t)) columns))
         (cells (mapcar (lambda (row)
                          (mapcar #'my:oracle--cell (append row nil)))
                        rows))
         (widths
          (seq-map-indexed
           (lambda (name i)
             (min my:oracle-max-column-width
                  (apply #'max (string-width name)
                         (mapcar (lambda (row)
                                   (string-width (or (nth i row) "")))
                                 cells))))
           names)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; 見出し
        (insert (string-join
                 (seq-map-indexed
                  (lambda (name i)
                    (propertize (my:oracle--pad (my:oracle--truncate name (nth i widths))
                                                (nth i widths) nil)
                                'face 'my:oracle-header-face))
                  names)
                 " │ ")
                "\n")
        (insert (string-join (mapcar (lambda (w) (make-string w ?─)) widths) "─┼─") "\n")
        (dolist (row cells)
          (insert (string-join
                   (seq-map-indexed
                    (lambda (w i)
                      (my:oracle--pad (my:oracle--truncate (or (nth i row) "") w)
                                      w (nth i numeric)))
                    widths)
                   " │ ")
                  "\n"))
        (when (eq (plist-get params :more) t)
          (insert (propertize
                   (format "\n--- %d 行で止めている（C-c q m で続き）---\n" (length rows))
                   'face 'shadow)))
        (goto-char (point-min))
        (setq header-line-format (my:oracle--header-line id params (length rows)))))
    (display-buffer buf)
    (my:oracle--log (format "完了: %d 行 (%.1f ms)%s" (length rows)
                            (or (plist-get params :elapsed_ms) 0)
                            (if (eq (plist-get params :more) t) " ＋続きあり" "")))
    (message "%d 行 (%.1f ms)%s" (length rows) (or (plist-get params :elapsed_ms) 0)
             (my:oracle--tx-suffix params))))

(defun my:oracle--header-line (id params nrows)
  (let* ((entry (cdr (assoc id my:oracle--sessions)))
         (info (plist-get entry :info)))
    (format " %s  %s@%s  AC:%s%s  %d 行%s  %.1f ms"
            (or (plist-get entry :name) id)
            (or (plist-get info :user) "?") (or (plist-get info :db) "?")
            (if (eq (plist-get info :autocommit) t) "on" "off")
            (if (eq (plist-get params :in_tx) t)
                (propertize "  TX" 'face 'warning) "")
            nrows
            (if (eq (plist-get params :more) t) "＋" "")
            (or (plist-get params :elapsed_ms) 0))))

;;; [8] トランザクション

(defun my:oracle--tx (method)
  (let* ((id (my:oracle--session))
         (info (my:oracle--request method (list :session id))))
    (setf (plist-get (cdr (assoc id my:oracle--sessions)) :info) info)
    (message "%s（autocommit %s, tx %s）"
             (if (eq method 'tx/commit) "commit した" "rollback した")
             (if (eq (plist-get info :autocommit) t) "ON" "OFF")
             (if (eq (plist-get info :in_tx) t) "あり" "なし"))))

;;;###autoload
(defun my:oracle-commit () (interactive) (my:oracle--tx 'tx/commit))

;;;###autoload
(defun my:oracle-rollback () (interactive) (my:oracle--tx 'tx/rollback))

;;;###autoload
(defun my:oracle-toggle-autocommit ()
  "autocommit を切り替える（トランザクション中は daemon が拒否する）。"
  (interactive)
  (let* ((id (my:oracle--session))
         (info (plist-get (cdr (assoc id my:oracle--sessions)) :info))
         (on (not (eq (plist-get info :autocommit) t)))
         (new (my:oracle--request 'tx/autocommit (list :session id :on (if on t :json-false)))))
    (setf (plist-get (cdr (assoc id my:oracle--sessions)) :info) new)
    (message "autocommit %s" (if (eq (plist-get new :autocommit) t) "ON" "OFF"))))

;;;###autoload
(defun my:oracle-info ()
  "繋がっている session の状態を出す。"
  (interactive)
  (let ((info (my:oracle--info)))
    (unless info (user-error "繋がっていない"))
    (message "%s@%s %s charset=%s sid=%s,%s autocommit=%s tx=%s cancel_sql=%s"
             (plist-get info :user) (plist-get info :db) (plist-get info :version)
             (plist-get info :charset) (plist-get info :sid) (plist-get info :serial)
             (if (eq (plist-get info :autocommit) t) "ON" "OFF")
             (if (eq (plist-get info :in_tx) t) "あり" "なし")
             (if (eq (plist-get info :can_cancel_sql) t) "可" "不可"))))

;;; [9] キーバインド

(defvar my:oracle-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'my:oracle-connect)
    (define-key map "c" #'my:oracle-commit)
    (define-key map "b" #'my:oracle-rollback)
    (define-key map "a" #'my:oracle-toggle-autocommit)
    (define-key map "m" #'my:oracle-more)
    (define-key map "i" #'my:oracle-info)
    (define-key map "q" #'my:oracle-disconnect)
    map)
  "`C-c q' に置くキーマップ（q = query）。")

(use-package sql
  :defer t
  :config
  ;; 【重要】`sql-mode-map' には SQLcl 版（sql.el の C-c C-c など）が
  ;; 既に乗っている。こちらは `C-c q' のプレフィクスに固め、
  ;; 送信だけ `C-c C-q' に置いて衝突させない。
  (define-key sql-mode-map (kbd "C-c q") my:oracle-prefix-map)
  (define-key sql-mode-map (kbd "C-c C-q") #'my:oracle-send-statement)
  (define-key sql-mode-map (kbd "C-c M-q") #'my:oracle-send-region)
  (define-key sql-mode-map (kbd "C-c C-k") #'my:oracle-cancel))

(provide 'my-oracle)
;;; my-oracle.el ends here
