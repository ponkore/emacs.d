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
(require 'comint)
(require 'cl-lib)
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

(defcustom my:oracle-serveroutput t
  "非 nil なら `DBMS_OUTPUT' を取る（`SET SERVEROUTPUT ON' 相当）。
文ごとに 1 往復ぶん余計にかかる（この回線で 110 ms 前後）。"
  :type 'boolean)

(defcustom my:oracle-repl-max-rows 200
  "対話バッファに出す行数の上限。続きは `C-c q m'。"
  :type 'integer)

(defcustom my:oracle-prompt "SQL> "
  "対話バッファのプロンプト。
【重要】変えるなら `my:oracle--prompt-regexp' も合わせる。継続プロンプトは
出さない（`comint-prompt-regexp' に 2 種類混ぜると、SQLcl 版で
`sql-interactive-remove-continuation-prompt' が本物のプロンプトを
食ったのと同種の事故になる）。"
  :type 'string)

(defcustom my:oracle-script-path nil
  "`@file' でスクリプトを探すディレクトリ。
`default-directory' の次に、ここを順に見る。"
  :type '(repeat directory))

(defcustom my:oracle-script-coding 'undecided
  "`@file' で読むスクリプトの文字コード。
【重要】daemon との間は UTF-8 固定なので、化けうるのはファイルを読む瞬間
だけ。BOM 無し cp932 の `.sql' があるなら `cp932' を指定する。"
  :type 'coding-system)

(defcustom my:oracle-script-on-error 'stop
  "`@file' の途中でエラーが出たときどうするか。
`stop' は止める（既定）、`continue' は続ける、`ask' は訊く。
【重要】どれでも**トランザクションはそのまま残す**（自動 commit も
自動 rollback もしない。どちらも「静かにデータが変わる」側に倒れる）。"
  :type '(choice (const stop) (const continue) (const ask)))

(defcustom my:oracle-lob-limit 4096
  "LOB / 長い文字列を切り詰める文字数。
`C-u' を付けて送ると `my:oracle-lob-limit-large' を使う。

【重要】go-ora は既定で LOB の中身を丸ごと転送するので、上限が無いと
巨大な CLOB で stdio が埋まる。"
  :type 'integer)

(defcustom my:oracle-lob-limit-large 1000000
  "`C-u' を付けて送ったときの LOB の上限。"
  :type 'integer)

(defcustom my:oracle-script-max-depth 8
  "`@file' の入れ子の深さの上限。"
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

(defvar my:oracle--pending nil
  "走っている 1 文の状態。
plist: :session :query :text :origin-buffer :origin-pos :dest
       :columns :rows :start
daemon を張り直すときに捨てるので、ここで宣言しておく。")

(defvar my:oracle--script nil
  "走っているスクリプトの状態（`@file' と、対話バッファの 1 入力）。
plist: :queue :name :dest :errors :ignored :done :total :depth :dir")

(defvar my:oracle--meta nil
  "session id → plist（:token :objects :columns :checked）の alist。
`:objects' は (名前 . 種別) の alist、`:columns' は 表名 → 列名のリスト。
daemon を張り直すときに捨てるので、ここで宣言しておく。")

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
  "daemon からの通知を捌く。

【重要】通知で来るのは**行の束だけ**。文の結末（結果・エラー）は
`sql/exec' の応答で受ける。両方で返すと Emacs 側が 2 回処理して、
対話バッファにプロンプトが 2 つ出る（実機で踏んだ）。"
  (pcase method
    ('rows/chunk (my:oracle--on-chunk params))
    (_ nil)))

(defun my:oracle--ensure-daemon ()
  "daemon を起こして接続を返す。"
  (unless (my:oracle--available-p)
    (user-error "oracled が無い。M-x my:oracle-build で作る"))
  (when (and my:oracle--conn (not (jsonrpc-running-p my:oracle--conn)))
    ;; daemon が死んだら、走っていた文とスクリプトの札も捨てる。
    ;; 残しておくと「まだ前の文が走っている」と言い続けて何も送れなくなる
    (setq my:oracle--conn nil
          my:oracle--sessions nil
          my:oracle--last-session nil
          my:oracle--pending nil
          my:oracle--script nil
          my:oracle--meta nil))
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
                      :autocommit (if my:oracle-autocommit t :json-false)
                      :serveroutput (if my:oracle-serveroutput t :json-false))))
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
    (format "*oracle(%s)%s*" name
            (pcase kind
              ('rows ":rows") ('log ":log") ('desc ":desc") ('plan ":plan")
              (_ "")))))

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
(defun my:oracle--busy-p ()
  (and my:oracle--pending (plist-get my:oracle--pending :session)))

(defun my:oracle--kind (st)
  "文の種類をシンボルで返す。

【重要】jsonrpc.el が渡してくる JSON の値は**文字列**で、シンボルでは
ない。`(eq (plist-get st :kind) \='client)' は**永久に偽**になり、
`@file' や `SET` が Oracle に送られて ORA-00900 / ORA-00922 になる
（実機で踏んだ）。"
  (let ((k (plist-get st :kind)))
    (cond ((symbolp k) k)
          ((stringp k) (intern k))
          (t 'sql))))

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

(defun my:oracle--send (text &optional origin-pos dest binds lob-limit)
  "TEXT を送る。
ORIGIN-POS はバッファ内の文の開始位置（エラー位置に飛ぶのに使う）。
DEST が非 nil なら、そのバッファにも結果をテキストで出す（対話バッファ）。
BINDS は `my:oracle--read-binds' が作ったバインド変数。
LOB-LIMIT は LOB を切り詰める文字数。"
  (when (my:oracle--busy-p)
    (user-error "まだ前の文が走っている（C-c C-k で止める）"))
  (let ((id (my:oracle--session)))
    (setq my:oracle--pending
          (list :session id :text text :columns nil :rows nil
                :origin-buffer (current-buffer) :origin-pos origin-pos
                :dest dest :start (current-time)))
    (my:oracle--log (format "送信: %s" (my:oracle--one-line text)))
    ;; 【重要】非同期。同期で待つと $/cancel を送れない。
    (jsonrpc-async-request
     (my:oracle--ensure-daemon) 'sql/exec
     (list :session id :text text
           :maxrows my:oracle-maxrows :chunk my:oracle-chunk
           ;; 【重要】JSON の配列は**ベクタ**で渡す。リストで渡すと
           ;; `json-serialize' が alist と解釈して
           ;; `Wrong type argument: symbolp, "jp"' で落ちる
           :binds (vconcat binds)
           :lob_limit (or lob-limit my:oracle-lob-limit))
     :success-fn #'my:oracle--on-result
     :error-fn (lambda (err)
                 (my:oracle--finish-error
                  (or (plist-get err :data)
                      (list :message (plist-get err :message) :position -1))))
     :timeout nil)
    (message "送信中… (C-c C-k で止める)")))

;;;###autoload
(defun my:oracle-send-statement (&optional big-lob)
  "点のある文を送る。
文にバインド変数（`:name'）があれば値を訊く。`C-u' を付けると LOB の
上限を `my:oracle-lob-limit-large' に上げる。"
  (interactive "P")
  (let ((st (my:oracle-statement-at-point)))
    (unless st (user-error "送る文が無い"))
    (when (eq (my:oracle--kind st) 'client)
      (user-error "SQL*Plus のコマンドは Phase 2（%s）" (plist-get st :text)))
    (unless (eq (plist-get st :complete) t)
      (unless (y-or-n-p "文が閉じていない（終端子が無い）。それでも送る? ")
        (user-error "中止")))
    (my:oracle--send (plist-get st :text) (1+ (plist-get st :begin)) nil
                     (my:oracle--read-binds
                      (append (plist-get st :binds) nil))
                     (and big-lob my:oracle-lob-limit-large))))

;;;###autoload
(defun my:oracle-send-region (begin end)
  "リージョンをそのまま 1 文として送る。"
  (interactive "r")
  (my:oracle--send (string-trim (buffer-substring-no-properties begin end)) begin))

;;;###autoload
(defun my:oracle-cancel (&optional force)
  "走っている文を止める（実測で 165〜260 ms。セッションは残る）。
`C-u' を付けると、応答が来ないまま残った札も降ろす（スクリプトも打ち切る）。"
  (interactive "P")
  (let ((id (or (plist-get my:oracle--pending :session) my:oracle--last-session)))
    (unless id (user-error "繋がっていない"))
    (jsonrpc-notify (my:oracle--ensure-daemon) '$/cancel (list :session id))
    (if force
        (progn (setq my:oracle--pending nil my:oracle--script nil)
               (message "割り込みを送り、札も降ろした"))
      (message "割り込みを送った"))))

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
     :success-fn #'my:oracle--on-result
     :error-fn (lambda (err)
                 (my:oracle--finish-error
                  (or (plist-get err :data)
                      (list :message (plist-get err :message) :position -1))))
     :timeout nil)))

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
         (dest (plist-get pending :dest))
         (elapsed (or (plist-get params :elapsed_ms) 0))
         (query-p (equal (plist-get params :kind) "query"))
         (implicit (eq (plist-get params :implicit_commit) t)))
    (setq my:oracle--pending nil)
    ;; プロンプトの tx 印に使うので、いまの tx 状態を覚えておく
    (when-let* ((entry (cdr (assoc id my:oracle--sessions))))
      (setf (plist-get entry :info)
            (plist-put (copy-sequence (plist-get entry :info))
                       :in_tx (plist-get params :in_tx))))
    ;; 【重要】DBMS_OUTPUT は結果より**前**に出す（SQL*Plus と同じ順）
    (my:oracle--emit-output (append (plist-get params :output) nil) dest)
    (when implicit
      (my:oracle--log "※ DDL により暗黙コミットされた")
      (my:oracle--emit dest
                       (propertize "※ この文は暗黙コミットした（DDL なので保留中の変更も確定）\n"
                                   'face 'warning)))
    (when query-p (my:oracle--remember-result id columns rows))
    (if query-p
        (progn
          (my:oracle--render id columns rows params)
          (when dest
            (my:oracle--emit
             dest (concat (my:oracle--format-table columns rows)
                          (my:oracle--feedback-line
                           (format "%d 行%s" (length rows)
                                   (if (eq (plist-get params :more) t)
                                       "（続きあり。C-c q m）" ""))
                           elapsed params)))))
      (my:oracle--log (format "完了: %s 行 (%.1f ms)%s"
                              (or (plist-get params :affected) 0) elapsed
                              (if implicit " ※暗黙コミット" "")))
      (when dest
        (my:oracle--emit
         dest (my:oracle--feedback-line (my:oracle--feedback-text params)
                                        elapsed params))))
    (unless dest
      (message "%s (%.1f ms)%s"
               (if query-p (format "%d 行" (length rows))
                 (my:oracle--feedback-text params))
               elapsed (my:oracle--tx-suffix params)))
    ;; 【重要】スクリプト（対話バッファの入力も 1 文のスクリプトとして
    ;; 流す）が走っているときは、プロンプトは最後に 1 回だけ出す。
    ;; ここでも出すと `SQL> SQL> ' と二重になる（実機で踏んだ）。
    (when (and dest (not my:oracle--script)) (my:oracle--repl-ready dest))
    (my:oracle--script-continue nil)))

(defun my:oracle--feedback-text (params)
  "文の種類に合わせた完了の文言。

【重要】種類は daemon が `stmt_kind' で教えてくる。Emacs 側で先頭語を
見て判定し直すと lexer が 2 つになる（§7 と同じ理由）。"
  (let ((affected (or (plist-get params :affected) 0)))
    (pcase (plist-get params :stmt_kind)
      ("plsql" "PL/SQL が完了しました")
      ("ddl" "完了しました")
      ("tx" "完了しました")
      (_ (format "%s 行が処理されました" affected)))))

(defun my:oracle--feedback-line (text elapsed params)
  "件数・所要・tx の 1 行を作る。"
  (propertize (format "\n%s  (%.1f ms)%s\n" text elapsed
                      (if (eq (plist-get params :in_tx) t) "  [TX]" ""))
              'face 'shadow))

(defun my:oracle--emit (dest text)
  "DEST（対話バッファ）に TEXT を出す。DEST が nil なら何もしない。"
  (when (and dest (buffer-live-p dest))
    (with-current-buffer dest
      (my:oracle--repl-insert text))))

(defun my:oracle--emit-output (lines dest)
  "DBMS_OUTPUT の LINES を出す。"
  (when lines
    (my:oracle--log (format "DBMS_OUTPUT %d 行" (length lines)))
    (if dest
        (my:oracle--emit dest (concat (string-join lines "\n") "\n"))
      ;; 対話バッファが無いときはログへ（埋もれないよう件数も出す）
      (dolist (line lines) (my:oracle--log (concat "| " line)))
      (message "DBMS_OUTPUT %d 行（:log を見る）" (length lines)))))

(defun my:oracle--tx-suffix (params)
  (if (eq (plist-get params :in_tx) t) "  [TX]" ""))

(defun my:oracle--finish-error (err)
  "エラーを報告し、位置が分かればソースの該当位置へ飛ぶ。"
  (let* ((pending my:oracle--pending)
         (msg (or (plist-get err :message) "?"))
         (code (plist-get err :code))
         (pos (plist-get err :position))
         (buf (plist-get pending :origin-buffer))
         (dest (plist-get pending :dest))
         (origin (plist-get pending :origin-pos)))
    (setq my:oracle--pending nil)
    (my:oracle--log (format "エラー: %s" (my:oracle--one-line msg)))
    ;; エラーの前に出た DBMS_OUTPUT も見せる（SQL*Plus と同じ）
    (my:oracle--emit-output (append (plist-get err :output) nil) dest)
    ;; 【重要】position は 0 始まりの**文字**オフセット（daemon が
    ;; `error occur at position: N' から取っている）
    (when (and pos (>= pos 0) origin (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (min (point-max) (+ origin pos)))))
    (if dest
        (progn
          (my:oracle--emit dest (propertize (concat msg "\n") 'face 'error))
          ;; 【重要】スクリプト（対話バッファの 1 入力も 1 文のスクリプト）
          ;; が走っているときは、プロンプトは最後に 1 回だけ出す。
          ;; ここでも出すと `SQL> SQL> ' と二重になる（実機で踏んだ）。
          (unless my:oracle--script (my:oracle--repl-ready dest)))
      (message "%s%s" (if code (format "ORA-%05d " code) "") msg))
    (my:oracle--script-continue (or msg "error"))))

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
   ;; 長すぎる文字列・CLOB は {"text": …, "len": N, "$trunc": t}
   ((and (listp v) (plist-get v :text))
    (concat (my:oracle--one-line (plist-get v :text))
            (propertize (format "…(全 %s 文字)" (plist-get v :len)) 'face 'shadow)))
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

(defun my:oracle--format-table (columns rows)
  "COLUMNS と ROWS を桁の揃ったテキストにして返す。
結果バッファと対話バッファと書き出しで共有する。

【重要】桁は `string-width' で数える（`length' ではない）。あわせて、
印に使う文字は**描画幅と `string-width' が一致**していないといけない
（`my:oracle-null-string' の説明を参照）。"
  (let* ((names (mapcar (lambda (c) (or (plist-get c :name) "")) columns))
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
    (concat
     (string-trim-right
      (string-join
       (seq-map-indexed
        (lambda (name i)
          (propertize (my:oracle--pad (my:oracle--truncate name (nth i widths))
                                      (nth i widths) nil)
                      'face 'my:oracle-header-face))
        names)
       " │ "))
     "\n"
     (string-join (mapcar (lambda (w) (make-string w ?─)) widths) "─┼─")
     "\n"
     ;; 行末の詰め物は落とす（コピーしたときに余分な空白が付かないように）
     (mapconcat
      (lambda (row)
        (string-trim-right
         (string-join
          (seq-map-indexed
           (lambda (w i)
             (my:oracle--pad (my:oracle--truncate (or (nth i row) "") w)
                             w (nth i numeric)))
           widths)
          " │ ")))
      cells "\n")
     (if cells "\n" ""))))

(defun my:oracle--render (id columns rows params)
  "結果の表を結果バッファに出す。"
  (let ((buf (my:oracle--rows-buffer id)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (my:oracle--format-table columns rows))
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

(defvar my:oracle-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'my:oracle-connect)
    (define-key map "r" #'my:oracle-repl)
    (define-key map "c" #'my:oracle-commit)
    (define-key map "b" #'my:oracle-rollback)
    (define-key map "a" #'my:oracle-toggle-autocommit)
    (define-key map "s" #'my:oracle-toggle-serveroutput)
    (define-key map "f" #'my:oracle-run-script)
    (define-key map "d" #'my:oracle-describe)
    (define-key map "l" #'my:oracle-objects)
    (define-key map "g" #'my:oracle-refresh-metadata)
    (define-key map "x" #'my:oracle-toggle-vertical)
    (define-key map "w" #'my:oracle-write-result)
    (define-key map "e" #'my:oracle-explain)
    (define-key map "m" #'my:oracle-more)
    (define-key map "i" #'my:oracle-info)
    (define-key map "q" #'my:oracle-disconnect)
    map)
  "`C-c q' に置くキーマップ（q = query）。
【重要】対話バッファのキーマップから参照するので、ここより後ろで
定義してはいけない（void-variable になる）。")

;;; [9] 対話バッファ（SQL> プロンプト）

;; `comint-mode' 派生。入力履歴（M-p / M-n）・プロンプトの read-only・
;; 入力行の kill / yank・履歴のファイル保存を持っているので、
;; `my-claude' のように確定領域とマーカーを自作するより安い。
;;
;; 【重要】comint は「本物のプロセス」を前提にしているが、daemon との
;; 通信は JSON-RPC で、そこに comint 用の stdin は無い。そこで
;; `make-pipe-process' で**書き込みが捨てられるだけのプロセス**を握らせる。
;; `ielm' は同じ目的で hexl を起こすが、Windows のプロセス生成は 59〜76 ms
;; かかるうえ無駄な子が残る。pipe プロセスなら子は無い。
;; 実際の送信は `comint-input-sender' を差し替えて横取りする。

(defvar my:oracle--prompt-regexp "^SQL\\*?> "
  "プロンプトの正規表現。
【重要】トランザクション中を `SQL*> ' で示すので、**最初から両方**を
含めてある（SQLcl 版で `:prompt-regexp' に ANSI escape を入れ忘れて
`sql-redirect' が固まったのと同じ種類の抜けを作らないため）。")

(defvar-local my:oracle--repl-queue nil
  "応答待ちの間に打たれた入力（逆順）。")

(defvar my:oracle-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my:oracle-repl-return)
    (define-key map (kbd "C-j") #'my:oracle-repl-newline)
    (define-key map (kbd "C-c C-k") #'my:oracle-cancel)
    (define-key map (kbd "C-c q") my:oracle-prefix-map)
    ;; 【重要】comint の「プロセスにシグナルを送る」キーを全部潰す。
    ;; ダミープロセスを殺すと**画面は死ぬのに Oracle 側の文は走り続ける**
    ;; という最悪の壊れ方をする。
    (define-key map (kbd "C-c C-c") #'my:oracle-repl-send-force)
    (define-key map (kbd "C-c C-\\") #'my:oracle-cancel)
    (define-key map (kbd "C-c C-d") #'ignore)
    (define-key map (kbd "C-c C-z") #'ignore)
    map)
  "対話バッファのキーマップ。")

(define-derived-mode my:oracle-repl-mode comint-mode "Oracle"
  "oracled と話す対話バッファ。"
  (setq comint-prompt-regexp my:oracle--prompt-regexp
        comint-prompt-read-only t
        comint-use-prompt-regexp nil
        comint-process-echoes nil      ; 入力はバッファに残るので自前のエコーは不要
        comint-input-sender #'my:oracle--repl-sender
        comint-input-ring-file-name
        (expand-file-name "oracle-history" user-emacs-directory))
  ;; 【重要】履歴にパスワードを残さない。
  (add-hook 'comint-input-filter-functions #'my:oracle--repl-input-filter nil t)
  (setq-local truncate-lines t)
  (my:oracle--setup-completion)
  (comint-read-input-ring t)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t))

(defun my:oracle--repl-input-filter (input)
  "履歴に入れてよい入力か。"
  (not (string-match-p "\\`[ \t]*\\(connect\\|conn\\)\\b" (downcase input))))

(defun my:oracle--repl-buffer (id)
  "session ID の対話バッファを作る（無ければ）。"
  (let ((buf (get-buffer-create (my:oracle--buffer-name id nil))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my:oracle-repl-mode)
        (my:oracle-repl-mode)
        ;; comint が要求するプロセス。書いたものは捨てられる
        (let ((proc (make-pipe-process
                     :name (format "oracle-repl-%s" id)
                     :buffer (current-buffer) :noquery t :coding 'utf-8-unix
                     :filter #'ignore :sentinel #'ignore)))
          (set-marker (process-mark proc) (point-max))))
      (setq my:oracle--session-id id)
      (when (= (buffer-size) 0)
        (my:oracle--repl-insert
         (propertize (format "-- oracled %s --\n" id) 'face 'shadow))
        (my:oracle--repl-ready (current-buffer))))
    buf))

(defun my:oracle--tx-prompt ()
  "トランザクション中のプロンプト（`SQL> ' → `SQL*> '）。
【重要】`my:oracle--prompt-regexp' に一致する形でないといけない。
一致しないと comint がプロンプトと認識せず、M-p の挿入位置が壊れる。"
  (let ((trimmed (string-trim-right my:oracle-prompt)))
    (concat (string-remove-suffix ">" trimmed) "*> ")))

(defun my:oracle--repl-insert (text)
  "【重要】`comint-output-filter' を通して書く。
`insert' を直書きすると `process-mark' と `comint-last-output-start' が
ずれ、プロンプトの read-only 境界と M-p の挿入位置が壊れる。"
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
        (comint-output-filter proc text)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun my:oracle--repl-ready (buffer)
  "応答が終わったのでプロンプトを出し、溜まっていた入力を流す。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (my:oracle--repl-insert
       (let ((info (plist-get (cdr (assoc my:oracle--session-id my:oracle--sessions))
                              :info)))
         ;; tx 中は SQL*> にする（prompt-regexp は両方を含めてある）。
         ;; 【重要】形を間違えると prompt-regexp に一致せず、comint が
         ;; プロンプトと認識しなくなる（M-p の挿入位置が壊れる）
         (if (eq (plist-get info :in_tx) t)
             (my:oracle--tx-prompt)
           my:oracle-prompt)))
      ;; 保留していた入力を 1 つだけ流す（残りはキューに戻してから呼ぶ。
      ;; 先に呼ぶと、sender が押し戻したものを上書きして失う）
      (let ((queued (nreverse my:oracle--repl-queue)))
        (when queued
          (setq my:oracle--repl-queue (nreverse (cdr queued)))
          (my:oracle--repl-sender (get-buffer-process (current-buffer))
                                  (car queued)))))))

(defun my:oracle--repl-sender (_proc input)
  "対話バッファの入力を捌く（`comint-input-sender'）。
daemon には書かない（ダミープロセスに書くと取りこぼす）。"
  (let ((text (string-trim input))
        (buffer (current-buffer)))
    (cond
     ((string-empty-p text) (my:oracle--repl-ready buffer))
     ;; 応答待ちなら保留する。SQLcl 版の「接続が済む前に送ると二度と
     ;; 応えない」への対処と同じ仕掛けだが、今回は回避策ではなく仕様
     ((my:oracle--busy-p)
      (push input my:oracle--repl-queue)
      (message "応答待ち。終わってから送ります（%d 件）"
               (length my:oracle--repl-queue)))
     (t
      (let* ((res (my:oracle--request 'sql/split (list :text text)))
             (statements (append (plist-get res :statements) nil)))
        (if (null statements)
            (my:oracle--repl-ready buffer)
          ;; 複数の文が 1 回の入力に入っていたらスクリプトとして順に流す
          (my:oracle--script-start statements "入力" buffer)))))))

;;;###autoload
(defun my:oracle-repl ()
  "対話バッファを開く。"
  (interactive)
  (let ((id (my:oracle--session)))
    (pop-to-buffer (my:oracle--repl-buffer id))))

(defun my:oracle-repl-newline ()
  "送らずに改行する。"
  (interactive)
  (insert "\n"))

(defun my:oracle-repl-send-force ()
  "文が閉じていなくても送る。"
  (interactive)
  (comint-send-input))

(defun my:oracle-repl-return ()
  "文が閉じていれば送り、閉じていなければ改行する。

【重要】判定は daemon の `sql/split' の `complete' に任せる。
「文字列リテラルの途中」「END; がまだ無い」を Emacs 側の正規表現で
書くと lexer が 2 つになる。"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (start (if proc (marker-position (process-mark proc)) (point-min)))
         (input (string-trim (buffer-substring-no-properties start (point-max)))))
    (cond
     ((string-empty-p input) (comint-send-input))
     ;; client コマンド（@file / SET ...）は 1 行で完結する
     ((string-prefix-p "@" input) (comint-send-input))
     (t
      (let* ((res (my:oracle--request 'sql/split (list :text input)))
             (statements (append (plist-get res :statements) nil))
             (last (car (last statements))))
        (if (and last (eq (plist-get last :complete) t))
            (comint-send-input)
          (my:oracle-repl-newline)))))))

;;; [10] @file — 最小のスクリプト実行

;; 対応するのは DML / DDL / PL/SQL ブロック / @ / @@ / EXEC / PROMPT /
;; EXIT / COMMIT / ROLLBACK まで。**フルスペックの SQL*Plus ではない。**
;;
;; 【重要】未対応のコマンドは Oracle に送らず、無視したことを必ず報告する。
;; 送ると ORA-00900 の山になって本当のエラーが埋もれ、黙って飛ばすと
;; 「スクリプトが半分だけ効いた」ことに気づけない。

(defconst my:oracle--script-ignorable
  '("SET" "COLUMN" "COL" "SPOOL" "DEFINE" "DEF" "UNDEFINE" "UNDEF"
    "ACCEPT" "TTITLE" "BTITLE" "WHENEVER" "VARIABLE" "VAR" "PRINT"
    "PAUSE" "CLEAR" "BREAK" "COMPUTE" "TIMING" "SHOW" "HOST" "EDIT"
    "SAVE" "GET" "STORE" "LIST" "L" "RUN" "R" "COPY" "ARCHIVE"
    "CONNECT" "DISCONNECT" "DESC" "DESCRIBE" "INFO")
  "受け取るが実行しないコマンド（報告して飛ばす）。")

(defun my:oracle--script-resolve (path dir)
  "PATH を探す。拡張子が無ければ `.sql' を補う。"
  (let* ((name (if (file-name-extension path) path (concat path ".sql")))
         (dirs (cons (or dir default-directory) my:oracle-script-path)))
    (or (seq-find #'file-readable-p
                  (mapcar (lambda (d) (expand-file-name name d)) dirs))
        (and (file-name-absolute-p name) (file-readable-p name) name))))

(defun my:oracle--script-read (file)
  "FILE を読んで文に切り分ける。"
  (let ((text (let ((coding-system-for-read my:oracle-script-coding))
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-substring-no-properties (point-min) (point-max))))))
    (append (plist-get (my:oracle--request 'sql/split (list :text text))
                       :statements)
            nil)))

(defun my:oracle--script-start (statements name dest &optional depth dir)
  "STATEMENTS を順に流す。"
  (setq my:oracle--script
        (list :queue statements :name name :dest dest :errors 0 :ignored 0
              :done 0 :total (length statements)
              :depth (or depth 0) :dir (or dir default-directory)))
  (my:oracle--script-step))

;;;###autoload
(defun my:oracle-run-script (file)
  "FILE の中身を順に実行する（`@file' と同じ）。"
  (interactive "fスクリプト: ")
  (when (my:oracle--busy-p)
    (user-error "まだ前の文が走っている"))
  (let ((id (my:oracle--session)))
    (my:oracle--script-start (my:oracle--script-read file)
                             (file-name-nondirectory file)
                             (get-buffer (my:oracle--buffer-name id nil))
                             0 (file-name-directory file))))

(defun my:oracle--script-step ()
  "キューの先頭を 1 つ実行する。"
  (let* ((st (car (plist-get my:oracle--script :queue)))
         (dest (plist-get my:oracle--script :dest)))
   (if (null st)
       (my:oracle--script-finish)
    (setf (plist-get my:oracle--script :queue)
          (cdr (plist-get my:oracle--script :queue)))
    (cl-incf (plist-get my:oracle--script :done))
    (let ((kind (my:oracle--kind st))
          (text (plist-get st :text)))
      ;; 送る文を echo する（SQL*Plus の SET ECHO ON 相当）
      (when (and dest (> (plist-get my:oracle--script :total) 1))
        (my:oracle--emit dest (propertize (concat text "\n") 'face 'shadow)))
      (pcase kind
        ('client (my:oracle--script-client st))
        (_ (my:oracle--send text nil dest)))))))

(defun my:oracle--script-client (st)
  "client コマンドを捌く。実行しないものは報告して次へ。"
  (let* ((cmd (plist-get st :command))
         (text (plist-get st :text))
         (dest (plist-get my:oracle--script :dest))
         (line (plist-get st :line))
         (arg (string-trim (substring text (min (length text)
                                                (length (or cmd "")))))))
    (pcase cmd
      ((or "@" "@@")
       (let* ((rel (string-trim (substring text (length cmd))))
              (dir (if (equal cmd "@@") (plist-get my:oracle--script :dir)
                     default-directory))
              (depth (1+ (plist-get my:oracle--script :depth)))
              (file (my:oracle--script-resolve rel dir)))
         (cond
          ((> depth my:oracle-script-max-depth)
           (my:oracle--script-note (format "%s: 入れ子が深すぎる" text) t))
          ((not file)
           (my:oracle--script-note (format "%s: ファイルが無い" text) t))
          (t
           ;; 読み込んだ文をキューの先頭に差し込む
           (setf (plist-get my:oracle--script :queue)
                 (append (my:oracle--script-read file)
                         (plist-get my:oracle--script :queue)))
           (setf (plist-get my:oracle--script :dir) (file-name-directory file))
           (setf (plist-get my:oracle--script :depth) depth)
           (setf (plist-get my:oracle--script :name) (file-name-nondirectory file))
           ;; 総数を数え直す。これをしないと「1 文のスクリプト」のままで、
           ;; 各文の echo も最後の要約も出ない
           (setf (plist-get my:oracle--script :total)
                 (+ (plist-get my:oracle--script :done)
                    (length (plist-get my:oracle--script :queue))))))
         (my:oracle--script-step)))
      ((or "EXEC" "EXECUTE")
       ;; BEGIN ... END; に包むだけ（これは安いので訳す）
       (my:oracle--send (format "begin %s; end;" (string-trim-right arg ";"))
                        nil dest))
      ("PROMPT"
       (my:oracle--emit dest (concat arg "\n"))
       (my:oracle--script-step))
      ((or "REM" "REMARK") (my:oracle--script-step))
      ((or "EXIT" "QUIT")
       (my:oracle--script-note "EXIT でスクリプトを終了" nil)
       (setf (plist-get my:oracle--script :queue) nil)
       (my:oracle--script-step))
      ("SET"
       ;; SET SERVEROUTPUT だけは読み替える
       (if (string-match-p "\\`serveroutput\\b" (downcase arg))
           (let ((on (not (string-match-p "\\boff\\b" (downcase arg)))))
             (my:oracle--request 'session/serveroutput
                                 (list :session (my:oracle--session)
                                       :on (if on t :json-false)))
             (my:oracle--script-note (format "SERVEROUTPUT %s" (if on "ON" "OFF")) nil))
         (my:oracle--script-ignore cmd line))
       (my:oracle--script-step))
      (_
       (if (member cmd my:oracle--script-ignorable)
           (my:oracle--script-ignore cmd line)
         (my:oracle--script-note (format "%s: 知らないコマンド（無視）" text) t))
       (my:oracle--script-step)))))

(defun my:oracle--script-ignore (cmd line)
  "未対応のコマンドを報告する（Oracle には送らない）。"
  (cl-incf (plist-get my:oracle--script :ignored))
  (my:oracle--script-note (format "%d 行目: %s は未対応（無視しました）"
                                  (or line 0) cmd)
                          t))

(defun my:oracle--script-note (text warn)
  (my:oracle--log text)
  (my:oracle--emit (plist-get my:oracle--script :dest)
                   (propertize (concat text "\n")
                               'face (if warn 'warning 'shadow))))

(defun my:oracle--script-continue (error-message)
  "1 文が終わったので次へ進む（結果・エラーの通知から呼ばれる）。"
  (when my:oracle--script
    (if (not error-message)
        (my:oracle--script-step)
      (cl-incf (plist-get my:oracle--script :errors))
      (pcase my:oracle-script-on-error
        ('continue (my:oracle--script-step))
        ('ask (if (y-or-n-p "エラーが出た。続ける? ")
                  (my:oracle--script-step)
                (my:oracle--script-abort "中止した")))
        (_ (my:oracle--script-abort "エラーで止めた"))))))

(defun my:oracle--script-abort (why)
  (let ((rest (length (plist-get my:oracle--script :queue))))
    (when (> rest 0)
      (my:oracle--script-note (format "%s（残り %d 文は実行していない）" why rest) t)))
  (setf (plist-get my:oracle--script :queue) nil)
  (my:oracle--script-finish))

(defun my:oracle--script-finish ()
  "【重要】要約を必ず出す。無視した件数が分からないと、
スクリプトが半分だけ効いたことに気づけない。"
  (let* ((s my:oracle--script)
         (dest (plist-get s :dest))
         (text (format "-- %s: %d 文を実行、エラー %d、無視 %d --"
                       (plist-get s :name) (plist-get s :done)
                       (plist-get s :errors) (plist-get s :ignored))))
    (setq my:oracle--script nil)
    (when (> (plist-get s :total) 1)
      (my:oracle--log text)
      (my:oracle--emit dest (propertize (concat text "\n") 'face 'shadow))
      (message "%s" text))
    (when dest (my:oracle--repl-ready dest))))

;;;###autoload
(defun my:oracle-toggle-serveroutput ()
  "`DBMS_OUTPUT' の取得を入り切りする。"
  (interactive)
  (let* ((id (my:oracle--session))
         (info (plist-get (cdr (assoc id my:oracle--sessions)) :info))
         (on (not (eq (plist-get info :serveroutput) t)))
         (new (my:oracle--request 'session/serveroutput
                                  (list :session id :on (if on t :json-false)))))
    (setf (plist-get (cdr (assoc id my:oracle--sessions)) :info) new)
    (message "SERVEROUTPUT %s" (if (eq (plist-get new :serveroutput) t) "ON" "OFF"))))

;;; [11] メタ情報（describe / 一覧 / 補完）

;; 【重要】キャッシュの無効化は「通知」ではなく**トークン**で表す
;; （gitd と同じ。通知を 1 つ落とすと永久に古いままになる壊れ方をしない）。
;; トークンは `MAX(last_ddl_time)/件数'。秒精度なので同じ秒の DDL は
;; 取りこぼす。だから**手で取り直す道（`C-u'）を必ず残す**。

(defcustom my:oracle-token-check-interval 60
  "キャッシュが古くないかを確かめる最短間隔（秒）。
0 にすると毎回確かめる（1 往復ぶん遅くなる）。"
  :type 'integer)

(defun my:oracle--meta-entry (id)
  (or (cdr (assoc id my:oracle--meta))
      (let ((new (list :token nil :objects nil :columns nil :checked nil)))
        (push (cons id new) my:oracle--meta)
        new)))

(defun my:oracle--meta-put (id key value)
  (let ((entry (my:oracle--meta-entry id)))
    (setf (plist-get entry key) value)
    (setcdr (assoc id my:oracle--meta) entry)
    value))

(defun my:oracle--fetch-objects (id &optional all)
  "オブジェクト一覧を取り直してキャッシュする。"
  (let* ((res (my:oracle--request 'meta/objects
                                  (list :session id :all (if all t :json-false))))
         (objects (mapcar (lambda (o)
                            (cons (plist-get o :name) (plist-get o :type)))
                          (append (plist-get res :objects) nil))))
    (my:oracle--meta-put id :objects objects)
    (my:oracle--meta-put id :token (plist-get res :token))
    (my:oracle--meta-put id :checked (current-time))
    (my:oracle--log (format "オブジェクト %d 件 (%.1f ms) token=%s"
                            (length objects) (or (plist-get res :elapsed_ms) 0)
                            (plist-get res :token)))
    objects))

(defun my:oracle--objects (id &optional force)
  "キャッシュしたオブジェクト名を返す。FORCE なら取り直す。"
  (let* ((entry (my:oracle--meta-entry id))
         (objects (plist-get entry :objects)))
    (cond
     ((or force (null objects)) (my:oracle--fetch-objects id))
     (t (my:oracle--check-token id) objects))))

(defun my:oracle--check-token (id)
  "キャッシュが古くないかを**非同期で**確かめる。
【重要】ここを同期にすると補完のたびに往復（100 ms 前後）が入る。
古かった場合の取り直しも非同期で、次の補完から新しくなる。"
  (let* ((entry (my:oracle--meta-entry id))
         (checked (plist-get entry :checked)))
    (when (or (null checked)
              (> (float-time (time-subtract (current-time) checked))
                 my:oracle-token-check-interval))
      (my:oracle--meta-put id :checked (current-time))
      (jsonrpc-async-request
       (my:oracle--ensure-daemon) 'meta/token (list :session id)
       :success-fn
       (lambda (res)
         (unless (equal (plist-get res :token) (plist-get entry :token))
           (my:oracle--log "DDL があったのでメタ情報を取り直す")
           (jsonrpc-async-request
            (my:oracle--ensure-daemon) 'meta/objects (list :session id)
            :success-fn (lambda (r)
                          (my:oracle--meta-put
                           id :objects
                           (mapcar (lambda (o) (cons (plist-get o :name)
                                                     (plist-get o :type)))
                                   (append (plist-get r :objects) nil)))
                          (my:oracle--meta-put id :token (plist-get r :token)))
            :error-fn #'ignore :timeout nil)))
       :error-fn #'ignore :timeout nil))))

;;;###autoload
(defun my:oracle-refresh-metadata (&optional all)
  "メタ情報を取り直す（`C-u' で全スキーマ）。"
  (interactive "P")
  (let ((id (my:oracle--session)))
    (message "%d 件" (length (my:oracle--fetch-objects id all)))))

;;;###autoload
(defun my:oracle-objects (&optional force)
  "オブジェクトを選んで describe する（`C-u' で一覧を取り直す）。"
  (interactive "P")
  (let* ((id (my:oracle--session))
         (objects (my:oracle--objects id force))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (category . my:oracle-object)
                                 (annotation-function
                                  . ,(lambda (cand)
                                       (let ((type (cdr (assoc cand objects))))
                                         (and type (concat "  " type))))))
                    (complete-with-action action objects string pred)))))
    (my:oracle-describe (completing-read "オブジェクト: " table nil t))))

;;;###autoload
(defun my:oracle-describe (name &optional full)
  "NAME の定義を出す。`C-u' を付けると制約・索引・統計まで引く。

既定で制約と索引を引かないのは**辞書ビューが遅い**から（実測で
`all_constraints' の件数を数えるだけで 0.8 秒）。SQLcl の `DESC' と
`INFO+' の切り分けと同じ。"
  (interactive
   (list (let* ((id (my:oracle--session))
                (objects (my:oracle--objects id))
                (default (thing-at-point 'symbol t)))
           (completing-read
            (format "describe%s%s: " (if current-prefix-arg "（制約・索引も）" "")
                    (if default (format " (既定 %s)" default) ""))
            objects nil nil nil nil default))
         current-prefix-arg))
  (let* ((id (my:oracle--session))
         (res (my:oracle--request 'meta/describe
                                  (list :session id :name name
                                        :full (if full t :json-false)))))
    ;; 補完のために列名も覚えておく
    (my:oracle--remember-columns id (plist-get res :name)
                                 (append (plist-get res :columns) nil))
    (my:oracle--render-describe id res)))

(defun my:oracle--remember-columns (id table columns)
  (let* ((entry (my:oracle--meta-entry id))
         (alist (plist-get entry :columns))
         (names (mapcar (lambda (c) (plist-get c :name)) columns)))
    (setf (alist-get table alist nil nil #'equal) names)
    (my:oracle--meta-put id :columns alist)))

(defun my:oracle--render-describe (id res)
  "describe の結果を `*oracle(接続名):desc*' に出す。"
  (let* ((buf (get-buffer-create (my:oracle--buffer-name id 'desc)))
         (columns (append (plist-get res :columns) nil))
         (grid-cols '((:name "列名" :numeric :json-false)
                      (:name "型" :numeric :json-false)
                      (:name "NULL" :numeric :json-false)
                      (:name "PK" :numeric t)
                      (:name "コメント" :numeric :json-false)))
         (grid-rows
          (mapcar (lambda (c)
                    (vector (plist-get c :name)
                            (plist-get c :type)
                            (if (eq (plist-get c :nullable) t) "" "NOT NULL")
                            ;; 【重要】ここは DB の NULL ではなく「無い」だけ
                            ;; なので、NULL の印（`my:oracle-null-string'）は
                            ;; 使わず空にする
                            (let ((pk (plist-get c :pk)))
                              (if (and pk (> pk 0)) (number-to-string pk) ""))
                            (or (plist-get c :comment) "")))
                  columns)))
    (with-current-buffer buf
      (unless (derived-mode-p 'my:oracle-rows-mode) (my:oracle-rows-mode))
      (setq my:oracle--session-id id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s.%s" (plist-get res :owner)
                                    (plist-get res :name))
                            'face 'my:oracle-header-face)
                (format "  %s" (plist-get res :object_type))
                (if (and (plist-get res :comment)
                         (not (string-empty-p (plist-get res :comment))))
                    (format "  — %s" (plist-get res :comment)) "")
                "\n")
        (when (plist-get res :num_rows)
          (insert (propertize
                   (format "行数 %s%s\n" (plist-get res :num_rows)
                           (if (plist-get res :last_analyzed)
                               (format "（統計 %s）" (plist-get res :last_analyzed)) ""))
                   'face 'shadow)))
        (insert "\n" (my:oracle--format-table grid-cols grid-rows))
        (when-let* ((ks (append (plist-get res :constraints) nil))
                    (_ (> (length ks) 0)))
          (insert (propertize "\n制約\n" 'face 'my:oracle-header-face))
          (dolist (k ks)
            (insert (format "  %-30s %s (%s)%s\n"
                            (plist-get k :name)
                            (pcase (plist-get k :type)
                              ("P" "主キー") ("U" "一意") ("R" "外部キー")
                              (other other))
                            (string-join (append (plist-get k :columns) nil) ", ")
                            (if (and (plist-get k :refers)
                                     (not (string-empty-p (plist-get k :refers))))
                                (format " → %s" (plist-get k :refers)) "")))))
        (when-let* ((is (append (plist-get res :indexes) nil))
                    (_ (> (length is) 0)))
          (insert (propertize "\n索引\n" 'face 'my:oracle-header-face))
          (dolist (i is)
            (insert (format "  %-30s %s(%s)\n"
                            (plist-get i :name)
                            (if (eq (plist-get i :unique) t) "UNIQUE " "")
                            (string-join (append (plist-get i :columns) nil) ", ")))))
        (goto-char (point-min))
        (setq header-line-format
              (format " %s  %d 列%s  %.1f ms"
                      (plist-get res :name) (length columns)
                      (if (> (+ (length (plist-get res :constraints))
                                (length (plist-get res :indexes))) 0)
                          (format "  制約 %d  索引 %d"
                                  (length (plist-get res :constraints))
                                  (length (plist-get res :indexes)))
                        "  （C-u で制約・索引も）")
                      (or (plist-get res :elapsed_ms) 0)))))
    (display-buffer buf)
    buf))

;;; [12] 補完（corfu がそのまま使う）

(defun my:oracle-completion-at-point ()
  "オブジェクト名と、describe した表の列名を補完する。

【重要】**往復しない。**キャッシュしたものだけを返し、古くないかの
確認は非同期で行う（補完のたびに 100 ms 待たされるのは論外）。"
  (when-let* ((id (and my:oracle--last-session
                       (assoc my:oracle--last-session my:oracle--sessions)
                       my:oracle--last-session))
              (bounds (bounds-of-thing-at-point 'symbol)))
    (let* ((entry (my:oracle--meta-entry id))
           (objects (plist-get entry :objects))
           (columns (apply #'append (mapcar #'cdr (plist-get entry :columns))))
           (cands (append (mapcar #'car objects) columns)))
      (when (null objects)
        ;; まだ持っていないときは、次回に間に合わせるため非同期で取りにいく
        (my:oracle--check-token id))
      (when cands
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic (lambda (_) cands))
              :annotation-function
              (lambda (cand)
                (cond ((cdr (assoc cand objects))
                       (concat "  " (cdr (assoc cand objects))))
                      ((member cand columns) "  列")))
              :exclusive 'no)))))

(defun my:oracle--setup-completion ()
  "このバッファの capf に足す。"
  (add-hook 'completion-at-point-functions
            #'my:oracle-completion-at-point nil t))

;;; [13] バインド変数

(defvar my:oracle--bind-history nil
  "バインド変数の名前 → 前回の値。")

(defun my:oracle--read-binds (names)
  "NAMES の値を訊いて、daemon に渡す形で返す。

【重要】`&var' の置換と違い、値は**本物のバインド**として渡るので
SQL の文字列として解釈されない（引用符を含む値でも壊れない）。
空のまま `RET' すると NULL になる。"
  (mapcar
   (lambda (name)
     (let* ((prev (cdr (assoc name my:oracle--bind-history)))
            (input (read-string (format ":%s = " name) prev)))
       (setf (alist-get name my:oracle--bind-history nil nil #'equal) input)
       (list :name name
             :value (cond
                     ((string-empty-p input) nil) ; 空は NULL
                     ((string-match-p "\\`-?[0-9]+\\'" input)
                      (string-to-number input))
                     (t input)))))
   names))

;;; [14] 縦表示・書き出し・実行計画

(defvar my:oracle--last-result nil
  "session id → plist（:columns :rows）。縦表示と書き出しに使う。")

(defun my:oracle--remember-result (id columns rows)
  (setf (alist-get id my:oracle--last-result nil nil #'equal)
        (list :columns columns :rows rows)))

(defvar-local my:oracle--vertical nil
  "このバッファを縦表示にしているか。")

(defun my:oracle--format-vertical (columns rows)
  "1 列 1 行の縦置き（`psql' の `\\x' 相当）にして返す。
列が多い 1 行を読むときに要る。**構造のまま持っているからできること。**"
  (let* ((names (mapcar (lambda (c) (or (plist-get c :name) "")) columns))
         (width (apply #'max 0 (mapcar #'string-width names))))
    (mapconcat
     (lambda (pair)
       ;; `seq-map-indexed' は (関数 要素 添字) で呼ぶので、
       ;; cons の car が行・cdr が添字になる
       (let ((row (car pair)) (i (cdr pair)))
         (concat
          (propertize (format "-- %d 行目 --\n" (1+ i)) 'face 'shadow)
          (mapconcat
           (lambda (nc)
             (format "%s │ %s\n"
                     (propertize (my:oracle--pad (car nc) width nil)
                                 'face 'my:oracle-header-face)
                     (my:oracle--cell (cdr nc))))
           (seq-mapn #'cons names (append row nil))
           ""))))
     (seq-map-indexed #'cons rows)
     "\n")))

;;;###autoload
(defun my:oracle-toggle-vertical ()
  "結果の表示を縦置きと表で切り替える。"
  (interactive)
  (let* ((id (or my:oracle--session-id my:oracle--last-session))
         (last (cdr (assoc id my:oracle--last-result))))
    (unless last (user-error "表示できる結果が無い"))
    (setq my:oracle--vertical (not my:oracle--vertical))
    (let ((inhibit-read-only t)
          (columns (plist-get last :columns))
          (rows (plist-get last :rows)))
      (erase-buffer)
      (insert (if my:oracle--vertical
                  (my:oracle--format-vertical columns rows)
                (my:oracle--format-table columns rows)))
      (goto-char (point-min)))
    (message "%s" (if my:oracle--vertical "縦置き" "表"))))

;;;###autoload
(defun my:oracle-write-result (file format)
  "直前の結果を FILE に書き出す。"
  (interactive
   (list (read-file-name "書き出し先: ")
         (intern (completing-read "形式: " '("csv" "tsv" "json") nil t nil nil "csv"))))
  (let* ((id (or my:oracle--session-id my:oracle--last-session))
         (last (cdr (assoc id my:oracle--last-result))))
    (unless last (user-error "書き出せる結果が無い"))
    (let* ((columns (plist-get last :columns))
           (rows (plist-get last :rows))
           (names (mapcar (lambda (c) (or (plist-get c :name) "")) columns))
           (coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (pcase format
          ('json
           (insert "[\n")
           (insert (mapconcat
                    (lambda (row)
                      (concat "  {"
                              (string-join
                               (seq-mapn (lambda (n v)
                                           (format "%s: %s" (json-serialize n)
                                                   (my:oracle--json-value v)))
                                         names (append row nil))
                               ", ")
                              "}"))
                    rows ",\n"))
           (insert "\n]\n"))
          (_
           (let ((sep (if (eq format 'tsv) "\t" ",")))
             (insert (string-join (mapcar (lambda (n) (my:oracle--csv n sep)) names) sep)
                     "\n")
             (dolist (row rows)
               (insert (string-join
                        (mapcar (lambda (v) (my:oracle--csv (my:oracle--raw-cell v) sep))
                                (append row nil))
                        sep)
                       "\n")))))))
    (message "%d 行を %s に書き出した" (length (plist-get last :rows)) file)))

(defun my:oracle--raw-cell (v)
  "書き出し用の文字列（NULL は空、切り詰めた LOB は中身だけ）。"
  (cond
   ((null v) "")
   ((eq v :json-false) "false")
   ((eq v t) "true")
   ((stringp v) v)
   ((numberp v) (number-to-string v))
   ((and (listp v) (plist-get v :text)) (plist-get v :text))
   ((and (listp v) (plist-get v :$b64)) (plist-get v :$b64))
   (t (format "%s" v))))

(defun my:oracle--json-value (v)
  (cond
   ((null v) "null")
   ((eq v :json-false) "false")
   ((eq v t) "true")
   ((numberp v) (number-to-string v))
   (t (json-serialize (my:oracle--raw-cell v)))))

(defun my:oracle--csv (s sep)
  "S を CSV / TSV の 1 項目にする。"
  (let ((text (my:oracle--one-line s)))
    (if (or (string-search sep text) (string-search "\"" text))
        (concat "\"" (string-replace "\"" "\"\"" text) "\"")
      text)))

;;;###autoload
(defun my:oracle-explain ()
  "点のある文の実行計画を出す（文は実行しない）。"
  (interactive)
  (let* ((id (my:oracle--session))
         (st (my:oracle-statement-at-point))
         (_ (unless st (user-error "文が無い")))
         (res (my:oracle--request 'plan/explain
                                  (list :session id :text (plist-get st :text))))
         (buf (get-buffer-create (my:oracle--buffer-name id 'plan))))
    (with-current-buffer buf
      (unless (derived-mode-p 'my:oracle-rows-mode) (my:oracle-rows-mode))
      (setq my:oracle--session-id id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join (append (plist-get res :lines) nil) "\n"))
        (goto-char (point-min))
        (setq header-line-format
              (format " 実行計画（実行はしていない）  %s"
                      (my:oracle--one-line (plist-get st :text))))))
    (display-buffer buf)))

;;; [15] キーバインド

(use-package sql
  :defer t
  :config
  ;; 【重要】`sql-mode-map' には SQLcl 版（sql.el の C-c C-c など）が
  ;; 既に乗っている。こちらは `C-c q' のプレフィクスに固め、
  ;; 送信だけ `C-c C-q' に置いて衝突させない。
  (define-key sql-mode-map (kbd "C-c q") my:oracle-prefix-map)
  (define-key sql-mode-map (kbd "C-c C-q") #'my:oracle-send-statement)
  (define-key sql-mode-map (kbd "C-c M-q") #'my:oracle-send-region)
  (define-key sql-mode-map (kbd "C-c C-k") #'my:oracle-cancel)
  (add-hook 'sql-mode-hook #'my:oracle--setup-completion))

(provide 'my-oracle)
;;; my-oracle.el ends here
