;;; my-claude.el --- Claude Code を stream-json で使う  -*- lexical-binding: t -*-
;;; Commentary:
;; Windows の Emacs には PTY が無いので、claude の対話 TUI はそのままでは動かない。
;; 代わりに claude が持っている双方向のストリーミング JSON 入出力
;; (`--input-format stream-json' / `--output-format stream-json') を素のパイプで
;; 駆動する。端末エミュレーションも常駐プロキシも要らない。
;;
;; 設計と実測は docs/claude/emacs-claude-stream-json-plan.md、
;; PTY プロキシ方式との比較は docs/claude/emacs-claude-pty-proxy-study.md を参照。
;;
;; 構成:
;; セッションは Emacs 全体で 1 つだけ。アカウント (Pro / Enterprise / Max) の
;; 切り替えは CLAUDE_CONFIG_DIR をプロセス起動時に渡すことでしか行えないので、
;; C-c a a で環境を選び、切り替えたくなったら C-c a e で立て直す。
;;
;;   *claude*        会話の記録 (読み取り専用、`my:claude-mode')
;;   *claude-input*  送信するテキストを書く (`my:claude-input-mode')
;;   *claude-log*    生の JSON Lines (`my:claude-log' が非 nil のとき)
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; --------------------------------------------------
;;; カスタマイズ
;;; --------------------------------------------------

(defgroup my:claude nil
  "Claude Code を stream-json 経由で使う。"
  :group 'tools
  :prefix "my:claude-")

(defcustom my:claude-environments
  '(("personal"  . nil)
    ("jighead"   . "~/.claude-config/jighead")
    ("ESC-Web"   . "~/.claude-config/ESC-Web"))
  "使い分ける claude の環境。(ラベル . CLAUDE_CONFIG_DIR) の alist。

CONFIG-DIR が nil なら CLAUDE_CONFIG_DIR を設定しない (claude の既定)。
**既定の環境に対しては必ず nil にすること。** `~/.claude' を明示的に
指定すると claude は `~/.claude/.claude.json' を探しに行くが、実体は
`~/.claude.json' にあるため見つからず、

  Claude configuration file not found at: ...\.claude\.claude.json

という警告を **標準出力に** 吐く。stream-json の途中に非 JSON の行が
混ざることになるうえ、`auth status' の email / orgName も null になる。

プラン名はここに書かない。`claude auth status --json' が実際の
subscriptionType を返すので、選択時にそちらを見せる。"
  :type '(alist :key-type string
                :value-type (choice (const :tag "既定 (~/.claude)" nil)
                                    directory)))

(defcustom my:claude-executable
  (or (executable-find "claude")
      (expand-file-name "~/.local/bin/claude.exe"))
  "claude の実行ファイル。"
  :type 'string)

(defcustom my:claude-model nil
  "使うモデル。nil なら claude の既定に任せる。
変更はプロセスの起動時にしか効かない。"
  :type '(choice (const :tag "既定" nil) string))

(defcustom my:claude-permission-mode nil
  "起動時に渡す `--permission-mode'。nil なら指定しない。"
  :type '(choice (const :tag "既定" nil)
                 (const "acceptEdits") (const "auto") (const "plan")
                 (const "manual") (const "dontAsk") (const "bypassPermissions")))

(defcustom my:claude-extra-args nil
  "起動時に追加で渡す引数のリスト。"
  :type '(repeat string))

(defcustom my:claude-log nil
  "非 nil なら受信した生の JSON Lines を *claude-log: ...* に残す。
上流のイベント種別が変わったときに気づける唯一の手掛かりなので、
様子がおかしいときは真にすること。"
  :type 'boolean)

(defcustom my:claude-auto-approve nil
  "ここに一致するツール名は許可を聞かずに通す。
正規表現の文字列、またはツール名を引数に取る述語。
nil なら毎回聞く。"
  :type '(choice (const :tag "毎回聞く" nil) regexp function))

(defcustom my:claude-stream t
  "非 nil なら応答を書かれる端から表示する。

`--include-partial-messages' を付けて `stream_event' を拾う。
受信する JSON の量は倍近くになるが、待たされている感じは相当減る。
nil にするとブロックが確定してから一度に出る (段階 3 までの挙動)。"
  :type 'boolean)

(defcustom my:claude-show-thinking nil
  "非 nil なら thinking ブロックの中身も薄く表示する。

モデルによっては `thinking_delta' の本文が空で届く (haiku で実測)。
その場合は非 nil にしても何も出ない。"
  :type 'boolean)

(defcustom my:claude-tool-result-max-lines 12
  "ツールの実行結果を畳まずに見せる行数。これを超えると折りたたむ。"
  :type 'integer)

;;; --------------------------------------------------
;;; face
;;; --------------------------------------------------

(defface my:claude-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "こちらの発言の見出し。")

(defface my:claude-assistant-face
  '((t :inherit default))
  "claude の本文。")

(defface my:claude-tool-face
  '((t :inherit font-lock-function-name-face))
  "ツール呼び出しの見出し。")

(defface my:claude-tool-result-face
  '((t :inherit shadow))
  "ツールの実行結果。")

(defface my:claude-error-face
  '((t :inherit error))
  "エラーと拒否。")

(defface my:claude-notice-face
  '((t :inherit warning))
  "claude が標準出力に吐いた平文の警告。")

(defface my:claude-code-face
  '((((background dark))  :background "#20242b" :extend t)
    (((background light)) :background "#f2f2f2" :extend t))
  "コードブロックの中身。")

(defface my:claude-code-fence-face
  '((t :inherit shadow))
  "コードブロックの ``` の行。")

(defface my:claude-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "見出し (# …)。")

(defface my:claude-inline-code-face
  '((t :inherit font-lock-constant-face))
  "行中の `コード`。")

(defface my:claude-meta-face
  '((t :inherit shadow :height 0.9))
  "コスト・所要時間などの補足。")

;;; --------------------------------------------------
;;; セッション
;;; --------------------------------------------------

(cl-defstruct (my:claude-session (:constructor my:claude--make-session)
                                 (:copier nil))
  process        ; プロセス
  buffer         ; 会話バッファ
  log-buffer     ; 生 JSON のバッファ (nil のことがある)
  directory      ; 起動した default-directory (展開済み)
  name           ; 環境のラベル
  config-dir     ; CLAUDE_CONFIG_DIR (nil なら既定)
  rate-limit     ; 直近の rate_limit_event の中身
  untrusted-key  ; claude が「信頼されていない」と言ってきた projects のキー
  stream-block   ; 逐次表示中のブロックの種別 (text / thinking / tool_use)
  text-start     ; いま流し込んでいる本文の開始位置 (マーカー)
  streamed-text  ; いま開いているブロックを delta で出したか
  terminal-only  ; 端末でしか使えないスラッシュコマンドの名前
  (pending "")   ; フィルタの未処理バイト
  session-id
  model
  (busy nil)     ; 応答待ちか
  (tool-names (make-hash-table :test 'equal)) ; tool_use_id -> ツール名
  (approved nil) ; このセッションで自動許可すると決めたツール名
  last-result)   ; 直近の result イベント (alist)

(defvar-local my:claude--session nil
  "そのバッファが属するセッション。会話バッファと入力バッファに入る。")

(defvar my:claude--the-session nil
  "唯一のセッション。複数持てるようにはしない。

環境 (アカウント) を切り替えるには CLAUDE_CONFIG_DIR を変えてプロセスを
起動し直すしかなく、同時に複数あるとどちらに送っているのか分からなくなる。")

(defun my:claude--project-directory ()
  "claude を動かすディレクトリ。プロジェクトのルート、無ければ現在地。"
  (expand-file-name
   (or (and (fboundp 'projectile-project-root)
            (ignore-errors (projectile-project-root)))
       (and (fboundp 'project-current)
            (let ((p (project-current)))
              (and p (project-root p))))
       default-directory)))

(defun my:claude--session-for-buffer ()
  "いま使うセッション。無ければ nil。"
  (or my:claude--session (my:claude--live-session)))

(defun my:claude--live-session ()
  "セッションが生きていれば返す。"
  (and my:claude--the-session
       (process-live-p (my:claude-session-process my:claude--the-session))
       my:claude--the-session))

;;; --------------------------------------------------
;;; 環境 (アカウント) の切り替え
;;; --------------------------------------------------

(defvar my:claude--auth-cache (make-hash-table :test 'equal)
  "CONFIG-DIR -> `claude auth status --json' の結果。")

(defvar my:claude--commands nil
  "claude が持っているスラッシュコマンド。((名前 説明 引数ヒント) …)。
`initialize' の control_response に入っている。")

(defvar my:claude--last-environment nil
  "前回選んだ環境のラベル。次回の既定にする。")

(defun my:claude--config-dir (env)
  "環境 ENV の CLAUDE_CONFIG_DIR。既定を使うなら nil。"
  (let ((dir (cdr (assoc env my:claude-environments))))
    (and dir (expand-file-name dir))))

(defun my:claude--process-environment (config-dir)
  "CLAUDE_CONFIG_DIR を CONFIG-DIR にした `process-environment' を返す。

CONFIG-DIR が nil のときは **設定しない** のではなく **消す**。
Emacs 自体が CLAUDE_CONFIG_DIR の設定された環境から起動されていると、
何もしなければそれを継承してしまい、「既定の環境」を選んだつもりで
別のアカウントに繋がる。実際に踏んだ。"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "CLAUDE_CONFIG_DIR" config-dir) ; nil なら削除される
    process-environment))

(defun my:claude--auth-status (env &optional force)
  "環境 ENV のアカウント情報を alist で返す。失敗したら nil。
`claude auth status --json' は実測 0.24 秒と速いが、選択のたびに
全環境ぶん呼ぶと体感に出るのでキャッシュする。FORCE で取り直す。"
  (let ((dir (my:claude--config-dir env)))
    (or (and (not force) (gethash env my:claude--auth-cache))
        (puthash
         env
         (ignore-errors
           (with-temp-buffer
             (let ((process-environment (my:claude--process-environment dir))
                   (coding-system-for-read 'utf-8-unix)
                   (default-process-coding-system '(utf-8-unix . utf-8-unix)))
               (when (zerop (call-process my:claude-executable nil t nil
                                          "auth" "status" "--json"))
                 (goto-char (point-min))
                 ;; 既定以外の設定ディレクトリを指定すると JSON の前に
                 ;; 警告が出ることがあるので、最初の { から読む。
                 (when (search-forward "{" nil t)
                   (goto-char (match-beginning 0))
                   (json-parse-buffer :object-type 'alist))))))
         my:claude--auth-cache))))

(defun my:claude--environment-line (env)
  "選択肢に出す 1 行。"
  (let ((auth (my:claude--auth-status env)))
    (format "%-10s %-11s %s"
            env
            (or (alist-get 'subscriptionType auth) "?")
            (or (alist-get 'orgName auth)
                (alist-get 'email auth)
                (if auth "(不明)" "(未ログイン?)")))))

(defun my:claude-refresh-auth ()
  "アカウント情報のキャッシュを捨てる。"
  (interactive)
  (clrhash my:claude--auth-cache)
  (message "claude のアカウント情報を取り直す"))

(defun my:claude--read-environment ()
  "使う環境をミニバッファで選ばせてラベルを返す。"
  (let* ((envs (mapcar #'car my:claude-environments))
         (lines (mapcar (lambda (e) (cons (my:claude--environment-line e) e)) envs))
         (default (car (rassoc (or my:claude--last-environment (car envs)) lines)))
         (choice (completing-read
                  (format "claude の環境 (既定 %s): "
                          (or my:claude--last-environment (car envs)))
                  (mapcar #'car lines) nil t nil nil default)))
    (setq my:claude--last-environment (cdr (assoc choice lines)))))

;;; --------------------------------------------------
;;; プロセスの起動
;;; --------------------------------------------------

(defun my:claude--command (&optional resume)
  "claude に渡す引数リスト。
`--verbose' と `--permission-prompt-tool stdio' は省略できない。
前者は無いと即エラー終了し、後者は無いと許可要求が黙って自動拒否される。

RESUME が t なら `--continue' (そのディレクトリの直近の会話を継ぐ)、
文字列ならその ID で `--resume' する。実測ではどちらも stream-json と
併用でき、`--continue' では前のターンの内容を憶えていた。"
  (append
   (list my:claude-executable
         "-p" "--verbose"
         "--input-format" "stream-json"
         "--output-format" "stream-json"
         "--permission-prompt-tool" "stdio")
   (cond ((stringp resume) (list "--resume" resume))
         (resume            (list "--continue")))
   (when my:claude-stream (list "--include-partial-messages"))
   (when my:claude-model (list "--model" my:claude-model))
   (when my:claude-permission-mode
     (list "--permission-mode" my:claude-permission-mode))
   my:claude-extra-args))

(defun my:claude--start (dir env &optional resume)
  "環境 ENV で DIR に claude を起動して session 構造体を返す。
RESUME は `my:claude--command' に渡す (t で --continue、文字列で --resume)。"
  (unless (file-executable-p my:claude-executable)
    (user-error "claude が見つからない: %s" my:claude-executable))
  (let* ((config-dir (my:claude--config-dir env))
         (conv (get-buffer-create "*claude*"))
         (log  (when my:claude-log (get-buffer-create "*claude-log*")))
         (session (my:claude--make-session
                   :buffer conv :log-buffer log
                   :directory dir :name env :config-dir config-dir))
         proc)
    (when (and config-dir (not (file-directory-p config-dir)))
      (user-error "CLAUDE_CONFIG_DIR が無い: %s" config-dir))
    ;; ヘッダにプラン名を出すため。0.24 秒で、以後はキャッシュに乗る。
    (my:claude--auth-status env)
    (setq proc
          ;; my-japanese.el が default-process-coding-system の cdr を cp932 に
          ;; しているので、束縛せずに起動すると標準入力の日本語が壊れる。
          ;; ここは引数ではなく標準入力で本文を渡す経路なので utf-8 でよい。
          (let ((default-process-coding-system '(utf-8-unix . utf-8-unix))
                ;; Rust/Node 側は `~' を展開しないので必ず絶対パスにする
                ;; (gitd で os error 267 を踏んでいる)。
                (default-directory dir)
                ;; アカウントの切り替えはこれだけ。claude はプロセス起動時に
                ;; しか読まないので、環境を変えるには立て直すしかない。
                (process-environment (my:claude--process-environment config-dir)))
            (make-process
             :name (format "claude-%s" env)
             :buffer nil                ; 出力は自前のフィルタで捌く
             :connection-type 'pipe
             :noquery t
             :command (my:claude--command resume)
             :filter (lambda (_p str) (my:claude--filter session str))
             :sentinel (lambda (_p e) (my:claude--sentinel session e)))))
    (setf (my:claude-session-process session) proc)
    (setq my:claude--the-session session)
    (with-current-buffer conv
      (my:claude-mode)
      (setq my:claude--session session
            default-directory dir
            header-line-format (my:claude--header session)))
    ;; SDK が送るハンドシェイク。返ってくる control_response に
    ;; スラッシュコマンドの一覧が入っている。
    (my:claude--send-json session
                          '((type . "control_request")
                            (request_id . "my-claude-init")
                            (request . ((subtype . "initialize")))))
    session))

(defun my:claude--sentinel (session event)
  (let ((e (string-trim event)))
    ;; result が is_error のときに EOF を送ると終了コードは 1 になる。
    ;; 異常ではないので騒がない。
    (my:claude--insert session
                       (format "\n[プロセス %s]\n" e)
                       'my:claude-meta-face)
    (setf (my:claude-session-busy session) nil)
    (when (eq session my:claude--the-session)
      (setq my:claude--the-session nil))))

;;; --------------------------------------------------
;;; 送受信
;;; --------------------------------------------------

(defun my:claude--send-json (session obj)
  "OBJ を 1 行の JSON にして SESSION に送る。"
  (let ((proc (my:claude-session-process session)))
    (unless (process-live-p proc)
      (user-error "claude のプロセスが生きていない"))
    (let ((line (concat (json-serialize obj) "\n")))
      (when-let* ((log (my:claude-session-log-buffer session)))
        (with-current-buffer log
          (goto-char (point-max))
          (insert ">>> " line)))
      (process-send-string proc line))))

(defun my:claude--filter (session str)
  "プロセスフィルタ。行の途中で呼ばれるので持ち越す。"
  (setf (my:claude-session-pending session)
        (concat (my:claude-session-pending session) str))
  (let (line)
    (while (string-match "\n" (my:claude-session-pending session))
      (setq line (substring (my:claude-session-pending session)
                            0 (match-beginning 0)))
      (setf (my:claude-session-pending session)
            (substring (my:claude-session-pending session) (match-end 0)))
      (unless (string-empty-p (string-trim line))
        (when-let* ((log (my:claude-session-log-buffer session)))
          (with-current-buffer log
            (goto-char (point-max))
            (insert line "\n")))
        (if (not (string-prefix-p "{" (string-trim-left line)))
            ;; claude は警告を stderr ではなく標準出力に吐くことがある。
            ;; 異常ではないので JSON の解釈失敗とは分けて見せる。
            (my:claude--handle-notice line session)
          (condition-case err
              (my:claude--handle (json-parse-string line :object-type 'alist)
                                 session)
            (error
             ;; JSON のはずなのに読めなかった行は捨てずに見せる。
             ;; 上流のフォーマット変更に気づける唯一の手掛かり。
             (my:claude--insert
              session
              (format "[解釈できない行: %S]\n%s\n"
                      err (truncate-string-to-width line 200))
              'my:claude-error-face))))))))

(defun my:claude--handle-notice (line session)
  "claude が標準出力に吐いた平文 LINE を見せる。"
  (my:claude--insert session (concat (string-trim line) "\n")
                     'my:claude-notice-face)
  ;; ワークスペースが信頼されていないという警告なら直し方まで出す。
  ;; Emacs から起動すると必ずこうなる (下の my:claude-trust-workspace 参照)。
  ;; 放っておくとプロジェクト側の permissions.allow がまるごと無視される。
  (when (string-match "projects\\[\"\\([^\"]+\\)\"\\]" line)
    (setf (my:claude-session-untrusted-key session) (match-string 1 line))
    (my:claude--insert
     session
     "  → M-x my:claude-trust-workspace で信頼済みにできます\n"
     'my:claude-meta-face)))

;;; --------------------------------------------------
;;; 描画
;;; --------------------------------------------------

(defun my:claude--insert (session text &optional face)
  "SESSION の会話バッファの末尾に TEXT を挿入する。"
  (let ((buf (my:claude-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-end (and (get-buffer-window buf)
                           (>= (point) (point-max)))))
          (save-excursion
            (goto-char (point-max))
            (insert (if face (propertize text 'font-lock-face face) text)))
          ;; 末尾を見ていたときだけ追従する。読み返している最中に
          ;; 飛ばされるのは鬱陶しいため。
          (when at-end
            (dolist (w (get-buffer-window-list buf nil t))
              (with-selected-window w (goto-char (point-max))))))))))

(defun my:claude--insert-block (session text face)
  "TEXT を字下げして挿入する。"
  (my:claude--insert
   session
   (mapconcat (lambda (l) (concat "  " l))
              (split-string (string-trim-right text) "\n")
              "\n")
   face)
  (my:claude--insert session "\n"))

(defun my:claude--fold (session text face)
  "長い TEXT は先頭だけ見せて残りを隠す。"
  (let* ((lines (split-string (string-trim-right text) "\n"))
         (n (length lines)))
    (if (<= n my:claude-tool-result-max-lines)
        (my:claude--insert-block session text face)
      (my:claude--insert-block
       session
       (string-join (seq-take lines my:claude-tool-result-max-lines) "\n")
       face)
      (my:claude--insert
       session
       (format "  … 残り %d 行 (TAB で全体を表示)\n"
               (- n my:claude-tool-result-max-lines))
       'my:claude-meta-face)
      ;; 全文はテキストプロパティに持たせておく。
      (with-current-buffer (my:claude-session-buffer session)
        (let ((inhibit-read-only t))
          (put-text-property (line-beginning-position 0) (point-max)
                             'my:claude-full text))))))

;;; --------------------------------------------------
;;; イベントの処理
;;; --------------------------------------------------

(defun my:claude--content-string (content)
  "tool_result の content を文字列にする。文字列とブロック配列の両方が来る。"
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat (lambda (c)
                 (or (alist-get 'text c)
                     (format "%S" c)))
               content "\n"))
   (t (format "%S" content))))

(defun my:claude--handle (obj session)
  "受信した 1 イベント OBJ を処理する。"
  (pcase (alist-get 'type obj)
    ("system"          (my:claude--handle-system obj session))
    ("assistant"       (my:claude--handle-assistant obj session))
    ("user"            (my:claude--handle-user obj session))
    ("result"          (my:claude--handle-result obj session))
    ("control_request" (my:claude--handle-control-request obj session))
    ;; control_response は initialize の応答。今のところ使い道が無い。
    ("control_response" (my:claude--handle-control-response obj))
    ("stream_event"    (my:claude--handle-stream obj session))
    ;; 残量はアカウントを切り替える判断材料そのものなので拾う。
    ("rate_limit_event"
     (setf (my:claude-session-rate-limit session)
           (alist-get 'rate_limit_info obj))
     (my:claude--update-header session))
    (_ nil)))

(defun my:claude--header (session)
  "会話バッファのヘッダ行。どのアカウントに繋いでいるかを常に見せる。"
  (let* ((auth (gethash (my:claude-session-name session) my:claude--auth-cache))
         (rl (my:claude-session-rate-limit session)))
    (concat
     (format "%s(%s)"
             (my:claude-session-name session)
             (or (alist-get 'subscriptionType auth) "?"))
     (when-let* ((m (my:claude-session-model session))) (format " | %s" m))
     (when rl
       (let ((w (alist-get 'unifiedWindows rl)))
         (format " | 5h %d%% 7d %d%%"
                 (round (* 100 (or (alist-get 'utilization (alist-get 'five_hour w)) 0)))
                 (round (* 100 (or (alist-get 'utilization (alist-get 'seven_day w)) 0))))))
     (format " | %s" (abbreviate-file-name
                      (directory-file-name (my:claude-session-directory session)))))))

(defun my:claude--update-header (session)
  (when (buffer-live-p (my:claude-session-buffer session))
    (with-current-buffer (my:claude-session-buffer session)
      (setq header-line-format (my:claude--header session)))))

(defun my:claude--handle-system (obj session)
  (pcase (alist-get 'subtype obj)
    ("init"
     ;; init はターンごとに来る。バッファに挿すと会話の途中に何度も
     ;; 見出しが混ざるので、ヘッダ行に出す。
     (setf (my:claude-session-session-id session) (alist-get 'session_id obj)
           (my:claude-session-model session) (alist-get 'model obj)
           ;; 端末が要るコマンド (doctor / color / reload-plugins)。
           ;; 補完の注釈で分かるようにする。
           (my:claude-session-terminal-only session)
           (append (alist-get 'terminal_slash_commands obj) nil))
     (my:claude--update-header session)
     ;; MCP の失敗は毎ターン出すとうるさいので 1 度だけ本文に出す。
     (let ((bad (seq-filter
                 (lambda (m) (not (equal (alist-get 'status m) "connected")))
                 (append (alist-get 'mcp_servers obj) nil))))
       (when (and bad (not (my:claude-session-session-id session)))
         (my:claude--insert
          session
          (format "MCP 未接続: %s
"
                  (mapconcat (lambda (m) (alist-get 'name m)) bad ", "))
          'my:claude-error-face))))
    ("permission_denied"
     (my:claude--insert
      session
      (format "拒否: %s\n" (or (alist-get 'message obj) ""))
      'my:claude-error-face))
    (_ nil)))

(defun my:claude--handle-stream (obj session)
  "`stream_event' を処理して、書かれる端から表示する。

イベントの並びは実測で次のとおり。**`assistant' は
`content_block_stop' より先に、ブロック 1 つぶんずつ届く。**

  content_block_start (thinking/text/tool_use)
  content_block_delta … (thinking_delta / signature_delta /
                         text_delta / input_json_delta)
  assistant                ← そのブロックの確定版
  content_block_stop

そのため text は delta で出しておき、`assistant' 側では出さない
(`my:claude--handle-assistant' が `my:claude-stream' を見て飛ばす)。
tool_use は逆に delta を捨てて `assistant' の確定版だけを使う。
`input_json_delta' は JSON の断片なので、揃うまで意味を持たない。"
  (let* ((ev (alist-get 'event obj))
         (delta (alist-get 'delta ev)))
    (pcase (alist-get 'type ev)
      ("content_block_start"
       (setf (my:claude-session-stream-block session)
             (alist-get 'type (alist-get 'content_block ev)))
       (when (equal (my:claude-session-stream-block session) "text")
         (my:claude--mark-text-start session)))
      ("content_block_delta"
       (pcase (alist-get 'type delta)
         ("text_delta"
          (setf (my:claude-session-streamed-text session) t)
          (my:claude--insert session (alist-get 'text delta)
                             'my:claude-assistant-face))
         ("thinking_delta"
          (when my:claude-show-thinking
            (let ((th (alist-get 'thinking delta)))
              (unless (or (null th) (string-empty-p th))
                (my:claude--insert session th 'my:claude-meta-face)))))
         ;; signature_delta は署名、input_json_delta は JSON の断片。
         (_ nil)))
      ("content_block_stop"
       (when (equal (my:claude-session-stream-block session) "text")
         (my:claude--fontify-markdown session (my:claude-session-text-start session))
         (my:claude--end-paragraph session))
       (setf (my:claude-session-stream-block session) nil
             (my:claude-session-streamed-text session) nil))
      (_ nil))))

(defun my:claude--fontify-markdown (session beg)
  "SESSION の会話バッファの BEG から末尾までを markdown として色づけする。

font-lock は使わない。このバッファは `special-mode' 派生で、挿入時に
`font-lock-face' を直に載せているため、font-lock を有効にすると
そちらに上書きされて競合する。ブロックが確定した時点で一度だけ塗る。"
  (let ((buf (my:claude-session-buffer session)))
    (when (and (buffer-live-p buf) (markerp beg) (marker-position beg))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (end (point-max)))
          (save-excursion
            ;; ``` で囲まれたブロック
            (goto-char beg)
            (while (re-search-forward "^[ \t]*```.*$" end t)
              (let ((fence1-beg (match-beginning 0))
                    (fence1-end (match-end 0))
                    body-end)
                (if (re-search-forward "^[ \t]*```[ \t]*$" end t)
                    (setq body-end (match-beginning 0))
                  ;; 閉じていない (中断されたなど) ときは末尾まで
                  (setq body-end end)
                  (goto-char end))
                (put-text-property fence1-beg fence1-end
                                   'font-lock-face 'my:claude-code-fence-face)
                (when (< fence1-end body-end)
                  (put-text-property fence1-end body-end
                                     'font-lock-face 'my:claude-code-face))
                (when (< body-end end)
                  (put-text-property body-end (min end (line-end-position))
                                     'font-lock-face 'my:claude-code-fence-face))))
            ;; 見出しと行中のコード。コードブロックの中は塗り直さない。
            (goto-char beg)
            (while (re-search-forward "^[ \t]*#\\{1,6\\} .*$" end t)
              (unless (eq (get-text-property (match-beginning 0) 'font-lock-face)
                          'my:claude-code-face)
                (put-text-property (match-beginning 0) (match-end 0)
                                   'font-lock-face 'my:claude-heading-face)))
            (goto-char beg)
            (while (re-search-forward "`[^`\n]+`" end t)
              (unless (memq (get-text-property (match-beginning 0) 'font-lock-face)
                            '(my:claude-code-face my:claude-code-fence-face
                              my:claude-heading-face))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'font-lock-face 'my:claude-inline-code-face)))))))))

(defun my:claude--mark-text-start (session)
  "いまの末尾に本文の開始位置を記録する。"
  (let ((buf (my:claude-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setf (my:claude-session-text-start session)
              (copy-marker (point-max) nil))))))

(defun my:claude--end-paragraph (session)
  "会話バッファの末尾を「空行 1 つ」に整える。
delta で流し込んだ本文は末尾の改行がまちまちなので、ここで揃える。"
  (let ((buf (my:claude-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (skip-chars-backward " \t\n")
            (delete-region (point) (point-max))
            (insert "\n\n")))))))

(defun my:claude--close-stream-block (session)
  "開いたままのブロックがあれば行を閉じる。中断されたときに使う。"
  (when (my:claude-session-stream-block session)
    (my:claude--insert session "\n")
    (setf (my:claude-session-stream-block session) nil)))

(defun my:claude--handle-assistant (obj session)
  (let ((content (alist-get 'content (alist-get 'message obj))))
    (seq-doseq (block content)
      (pcase (alist-get 'type block)
        ("text"
         ;; 【重要】`my:claude-stream' ではなく「このブロックを実際に
         ;; delta で出したか」で判断する。スラッシュコマンドは
         ;; assistant で本文を返すが stream_event を伴わない
         ;; (num_turns=0 で API を通らないため)。フラグを見ずに
         ;; my:claude-stream だけで飛ばすと /mcp や /context が
         ;; 何も表示されない。実際にそうなっていた。
         (unless (my:claude-session-streamed-text session)
           (my:claude--mark-text-start session)
           (my:claude--insert session
                              (concat (string-trim-right (alist-get 'text block)) "\n\n")
                              'my:claude-assistant-face)
           (my:claude--fontify-markdown session
                                        (my:claude-session-text-start session))))
        ("tool_use"
         (let ((name (alist-get 'name block))
               (id (alist-get 'id block)))
           (puthash id name (my:claude-session-tool-names session))
           (my:claude--insert
            session
            (format "▶ %s %s\n" name (my:claude--tool-summary block))
            'my:claude-tool-face)))
        (_ nil)))))

(defun my:claude--tool-summary (block)
  "tool_use の入力を 1 行にまとめる。"
  (let* ((input (alist-get 'input block))
         (s (or (alist-get 'command input)
                (alist-get 'file_path input)
                (alist-get 'pattern input)
                (alist-get 'description input)
                "")))
    (truncate-string-to-width (replace-regexp-in-string "\n" " " s) 100 nil nil "…")))

(defun my:claude--handle-user (obj session)
  "tool_result を表示する。"
  (let ((content (alist-get 'content (alist-get 'message obj))))
    (seq-doseq (block content)
      (when (equal (alist-get 'type block) "tool_result")
        (let* ((id (alist-get 'tool_use_id block))
               (name (gethash id (my:claude-session-tool-names session) "?"))
               (err (eq t (alist-get 'is_error block)))
               (text (my:claude--content-string (alist-get 'content block))))
          (my:claude--fold session
                           (if (string-empty-p (string-trim text))
                               (format "(%s: 出力なし)" name)
                             text)
                           (if err 'my:claude-error-face
                             'my:claude-tool-result-face)))))))

(defun my:claude--handle-result (obj session)
  ;; 中断されると content_block_stop が来ないことがある。
  (my:claude--close-stream-block session)
  (setf (my:claude-session-last-result session) obj
        (my:claude-session-busy session) nil)
  (let* ((usage (alist-get 'usage obj))
         (cost (alist-get 'total_cost_usd obj))
         (ms (alist-get 'duration_ms obj))
         (interrupted (equal (alist-get 'terminal_reason obj) "aborted_streaming")))
    (when interrupted
      (my:claude--insert session "[中断しました]\n" 'my:claude-error-face))
    (my:claude--insert
     session
     (format "── %s | in %s / out %s | $%.4f | %.1fs\n\n"
             (or (alist-get 'subtype obj) "?")
             (or (alist-get 'input_tokens usage) 0)
             (or (alist-get 'output_tokens usage) 0)
             (or cost 0.0)
             (/ (or ms 0) 1000.0))
     'my:claude-meta-face))
  (force-mode-line-update t))

;;; --------------------------------------------------
;;; 許可プロンプト
;;; --------------------------------------------------

(defun my:claude--auto-approve-p (session name)
  "NAME を聞かずに通してよいか。"
  (or (member name (my:claude-session-approved session))
      (cond
       ((stringp my:claude-auto-approve) (string-match-p my:claude-auto-approve name))
       ((functionp my:claude-auto-approve) (funcall my:claude-auto-approve name))
       (t nil))))

(defun my:claude--handle-control-request (obj session)
  "claude からの制御要求。今のところ can_use_tool だけ。"
  (let* ((rid (alist-get 'request_id obj))
         (req (alist-get 'request obj)))
    (if (equal (alist-get 'subtype req) "can_use_tool")
        (my:claude--ask-permission obj session rid req)
      ;; 知らない要求は成功として返しておく。無視すると claude が待ち続ける。
      (my:claude--send-json session
                            `((type . "control_response")
                              (response . ((subtype . "success")
                                           (request_id . ,rid)
                                           (response . ,(make-hash-table)))))))))

(defun my:claude--respond-permission (session rid body)
  "can_use_tool の要求 RID に BODY を返す。

【重要】許可と拒否で形が違う。claude が返してくるエラーによれば

  Expected {behavior: 'allow', updatedInput?: object}
        or {behavior: 'deny', message: string}

**拒否に `updatedInput' を付けてはいけない。** 付けると不正な応答と
判定され、claude には「拒否された」ではなく「許可フックでエラーが
起きた」と伝わる。ツールが実行されない点は同じなので気づきにくい。
`message' も必須で、省くと同じエラーになる (どちらも実測)。"
  (my:claude--send-json
   session
   `((type . "control_response")
     (response . ((subtype . "success")
                  (request_id . ,rid)
                  (response . ,body))))))

(defun my:claude--respond-allow (session rid input)
  (my:claude--respond-permission session rid
                                 `((behavior . "allow")
                                   (updatedInput . ,input))))

(defun my:claude--respond-deny (session rid message)
  (my:claude--respond-permission session rid
                                 `((behavior . "deny")
                                   (message . ,(if (string-empty-p (string-trim message))
                                                   "Denied by the user in Emacs."
                                                 message)))))

(defun my:claude--ask-permission (_obj session rid req)
  "ツール使用の可否を尋ねて control_response を返す。"
  (let* ((name (or (alist-get 'tool_name req) "?"))
         (desc (or (alist-get 'description req) ""))
         (input (alist-get 'input req)))
    (if (my:claude--auto-approve-p session name)
        (progn
          (my:claude--insert session (format "  (自動許可: %s)\n" name)
                             'my:claude-meta-face)
          (my:claude--respond-allow session rid input))
      (let (done)
        (while (not done)
          (pcase (car (read-multiple-choice
                       (format "%s %s を許可する?"
                               name (truncate-string-to-width desc 60 nil nil "…"))
                       '((?y "今回だけ許可")
                         (?n "拒否")
                         (?r "理由を書いて拒否")
                         (?a "以後このツールは聞かない")
                         (?v "入力を全部見る"))))
            (?y (my:claude--respond-allow session rid input)
                (setq done t))
            (?n (my:claude--insert session (format "  (拒否: %s)\n" name)
                                   'my:claude-error-face)
                (my:claude--respond-deny session rid "Denied by the user in Emacs.")
                (setq done t))
            ;; 理由を渡せると claude が別の手を考えられる。
            ;; 「そのファイルは触らないで、代わりに…」が効く。
            (?r (let ((why (read-string "拒否する理由: ")))
                  (my:claude--insert session (format "  (拒否: %s — %s)\n" name why)
                                     'my:claude-error-face)
                  (my:claude--respond-deny session rid why))
                (setq done t))
            (?a (push name (my:claude-session-approved session))
                (my:claude--respond-allow session rid input)
                (setq done t))
            (?v (my:claude--show-input name input))))))))

(defun my:claude--show-input (name input)
  "ツールの入力を別バッファに出す。"
  (let ((buf (get-buffer-create "*claude tool input*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" name))
        (dolist (kv input)
          (insert (format "%s:\n%s\n\n" (car kv) (cdr kv))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;;; --------------------------------------------------
;;; コマンド
;;; --------------------------------------------------

;;;###autoload
(defun my:claude (&optional arg)
  "claude セッションを開く。無ければ環境を選んで起動する。

セッションは Emacs 全体で 1 つだけ持つ。アカウントの切り替えは
CLAUDE_CONFIG_DIR をプロセス起動時に渡すことでしか行えないため、
複数あるとどちらに送っているのか分からなくなるので増やさない。

ARG (`C-u') を付けると、生きているセッションがあっても畳んで、
環境と作業ディレクトリを選び直す。Pro の残量が尽きたときに
その場で Max へ逃がすのがこの操作。"
  (interactive "P")
  (let ((session (my:claude--live-session)))
    (when (and session arg)
      (my:claude-quit-session session)
      (setq session nil))
    (unless session
      (let ((dir (my:claude--project-directory))
            (env (my:claude--read-environment)))
        (setq session (my:claude--start dir env))))
    ;; 起動済みのセッションを別プロジェクトから呼んだときは黙って
    ;; 使い回すが、cwd が違うことは知らせる (claude はそちらを見る)。
    (let ((here (my:claude--project-directory)))
      (unless (equal here (my:claude-session-directory session))
        (message "claude のセッションは %s のまま (C-u C-c a a で立て直す)"
                 (abbreviate-file-name
                  (directory-file-name (my:claude-session-directory session))))))
    (pop-to-buffer (my:claude-session-buffer session))
    session))

;;; スラッシュコマンド

(defun my:claude--handle-control-response (obj)
  "`initialize' の応答からスラッシュコマンドの一覧を覚える。"
  (when-let* ((resp (alist-get 'response obj))
              (inner (alist-get 'response resp))
              (cmds (alist-get 'commands inner)))
    (setq my:claude--commands
          (mapcar (lambda (c)
                    (list (alist-get 'name c)
                          (or (alist-get 'description c) "")
                          (or (alist-get 'argumentHint c) "")))
                  (append cmds nil)))))

(defun my:claude--capf ()
  "入力バッファで `/コマンド' を補完する。

**行頭の `/' だけを対象にする。** 文中のスラッシュまで拾うと
`src/foo' のようなパスを書くたびに候補が出て邪魔になる。
2 つめの `/' が来たらパスだと見なして手を引く (`cape-file' に譲る)。

【重要】補完領域には先頭の `/' を含め、候補も `/name' の形にすること。
`/' の **後ろ** から始めると接頭辞の長さが 0 になり、`corfu-auto-prefix'
(この設定では 1) に満たないという理由で corfu の自動補完に**捨てられる**。
その結果、次の capf である `cape-file' が `/' を絶対パスとして拾い、
C: 直下のディレクトリ一覧が出る。実際にそうなっていた。"
  (let* ((bol (line-beginning-position))
         (text (buffer-substring-no-properties bol (point))))
    (when (and my:claude--commands
               (string-match-p "\\`/[A-Za-z0-9_-]*\\'" text))
      (list bol (point)
            (mapcar (lambda (c) (concat "/" (car c))) my:claude--commands)
            :exclusive 'no
            :annotation-function
            (lambda (cand)
              (let* ((name (substring cand 1))
                     (e (assoc name my:claude--commands))
                     (s (my:claude--live-session))
                     (term (and s (member name (my:claude-session-terminal-only s)))))
                (concat (when term " [端末専用]")
                        (when e
                          (concat " " (truncate-string-to-width
                                       (replace-regexp-in-string "\n" " " (nth 1 e))
                                       70 nil nil "…"))))))))))

;;; ワークスペースの信頼

(defun my:claude--workspace-key (dir)
  "claude が `.claude.json' の projects に使うキーを DIR から作る。

Emacs から起動した claude は **必ずドライブレターが小文字**の
ワークスペースを見る。`expand-file-name' は大文字を保つのに、
`make-process' が子プロセスの作業ディレクトリを設定する経路で
小文字になる。実測 (Emacs 31.1 / Windows 11):

  default-directory      = C:/Projects/Foo/
  expand-file-name       = C:/Projects/Foo/
  子が見る cwd           = c:\\Projects\\Foo     ← 小文字

一方、端末で対話的に起動した claude は大文字のまま記録するので、
同じディレクトリに対して大小 2 つのエントリができる。Emacs 側は
必ず信頼されていない方を引くため、プロジェクトの
`.claude/settings.json' の permissions.allow が毎回まるごと無視される。
gopls が大文字のドライブレターを返して診断が出なかったのと同じ罠。"
  (let ((path (directory-file-name (expand-file-name dir))))
    (if (string-match "\\`\\([A-Za-z]\\):" path)
        (concat (downcase (match-string 1 path)) (substring path 1))
      path)))

(defun my:claude--config-json (session)
  "SESSION の設定ディレクトリにある `.claude.json' のパス。"
  (expand-file-name ".claude.json"
                    (or (my:claude-session-config-dir session)
                        (expand-file-name "~"))))

;;;###autoload
(defun my:claude-trust-workspace ()
  "いまのワークスペースを claude の設定で信頼済みにする。

`.claude.json' の projects[KEY].hasTrustDialogAccepted を t にする。
KEY は claude が警告で言ってきたものを優先し、無ければ
`my:claude--workspace-key' で組み立てる。

**claude が動いている間に実行しない。** claude はこのファイルを
自分でも書き戻すので、走っている最中に触ると上書きされる。
このコマンドはセッションを先に終了させ、書き換える前に
バックアップを取る。"
  (interactive)
  (let* ((session (or (my:claude--session-for-buffer)
                      (user-error "セッションが無い")))
         (key (or (my:claude-session-untrusted-key session)
                  (my:claude--workspace-key (my:claude-session-directory session))))
         (file (my:claude--config-json session)))
    (unless (file-exists-p file)
      (user-error "設定ファイルが無い: %s" file))
    (unless (yes-or-no-p
             (format "%s の projects[\"%s\"] を信頼済みにする (セッションは終了します)? "
                     (abbreviate-file-name file) key))
      (user-error "やめました"))
    (when (my:claude--live-session)
      (my:claude-quit-session session)
      ;; プロセスが落ちて設定を書き終えるのを待つ。
      (let ((d (+ (float-time) 10)))
        (while (and (process-live-p (my:claude-session-process session))
                    (< (float-time) d))
          (accept-process-output (my:claude-session-process session) 0.2)))
      (sleep-for 0.5))
    (let ((backup (concat file ".bak-my-claude-"
                          (format-time-string "%Y%m%d%H%M%S"))))
      (copy-file file backup)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (goto-char (point-min))
        (let* ((root (json-parse-buffer :object-type 'hash-table
                                        :array-type 'array))
               (projects (or (gethash "projects" root)
                             (puthash "projects" (make-hash-table :test 'equal)
                                      root)))
               (entry (or (gethash key projects)
                          (puthash key (make-hash-table :test 'equal) projects))))
          (puthash "hasTrustDialogAccepted" t entry)
          (erase-buffer)
          (insert (json-serialize root))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max) file nil 'quiet))))
      (message "信頼済みにしました: %s (バックアップ: %s)"
               key (file-name-nondirectory backup)))))

;;; セッションの再開とモデルの変更

(defun my:claude--restart (resume &optional env)
  "いまと同じディレクトリでセッションを立て直す。
RESUME は `my:claude--command' に渡す。ENV を省くと今の環境のまま。"
  (let* ((old (my:claude--session-for-buffer))
         (dir (if old (my:claude-session-directory old)
                (my:claude--project-directory)))
         (env (or env (and old (my:claude-session-name old))
                  (my:claude--read-environment))))
    (when (and old (process-live-p (my:claude-session-process old)))
      (my:claude-quit-session old)
      (let ((d (+ (float-time) 10)))
        (while (and (process-live-p (my:claude-session-process old))
                    (< (float-time) d))
          (accept-process-output (my:claude-session-process old) 0.2))))
    (let ((session (my:claude--start dir env resume)))
      (pop-to-buffer (my:claude-session-buffer session))
      session)))

;;;###autoload
(defun my:claude-continue ()
  "このディレクトリの直近の会話を継いでセッションを開く。

`--continue' を渡す。Emacs を再起動したあとでも、端末で続けていた会話でも、
そのディレクトリで最後に話していたものに繋がる (実測)。"
  (interactive)
  (let ((session (my:claude--live-session)))
    (if session
        (my:claude--restart t)
      (let ((dir (my:claude--project-directory))
            (env (my:claude--read-environment)))
        (pop-to-buffer
         (my:claude-session-buffer (my:claude--start dir env t)))))))

;;;###autoload
(defun my:claude-set-model (model)
  "モデルを変えてセッションを立て直す。会話は `--resume' で引き継ぐ。

claude はモデルを起動時にしか受け取らないので立て直すしかないが、
同じアカウントなら session-id で会話を継げる。
Opus と Haiku を行き来してもそれまでの話は消えない。"
  (interactive
   (list (completing-read "モデル: " '("opus" "sonnet" "haiku" "fable") nil nil
                          (or my:claude-model ""))))
  (let* ((old (my:claude--session-for-buffer))
         (id (and old (my:claude-session-session-id old))))
    (setq my:claude-model (if (string-empty-p model) nil model))
    (my:claude--restart (or id t))
    (message "モデルを %s にしました%s" model
             (if id " (会話は継続)" " (--continue で再開)"))))

;;;###autoload
(defun my:claude-switch-environment ()
  "環境 (アカウント) を選び直してセッションを立て直す。

会話の文脈は引き継がれない。アカウントが違えばセッションの保存先も
別なので、`--resume' でも繋がらない。"
  (interactive)
  (let* ((old (my:claude--live-session))
         (dir (if old (my:claude-session-directory old)
                (my:claude--project-directory)))
         (env (my:claude--read-environment)))
    (when old (my:claude-quit-session old))
    (let ((session (my:claude--start dir env)))
      (pop-to-buffer (my:claude-session-buffer session))
      session)))

(defun my:claude-send-string (text &optional session)
  "TEXT を claude に送る。"
  (let ((session (or session (my:claude--session-for-buffer)
                     (my:claude))))
    (unless (string-empty-p (string-trim text))
      (my:claude--insert session (format "\n> %s\n\n" (string-trim text))
                         'my:claude-user-face)
      (setf (my:claude-session-busy session) t)
      (my:claude--send-json
       session
       `((type . "user")
         (message . ((role . "user")
                     (content . [((type . "text") (text . ,text))])))))
      (force-mode-line-update t))
    session))

;;;###autoload
(defun my:claude-send-region (start end)
  "リージョンを claude に送る。"
  (interactive "r")
  (let ((session (my:claude-send-string (buffer-substring-no-properties start end))))
    (display-buffer (my:claude-session-buffer session))))

(defun my:claude-interrupt-session (session)
  "SESSION の応答を中断する。セッションは生き残り、次のターンも送れる。"
  (my:claude--send-json session
                        `((type . "control_request")
                          (request_id . ,(format "int-%s" (float-time)))
                          (request . ((subtype . "interrupt"))))))

(defun my:claude-quit-session (session)
  "SESSION を終了する。"
  (let ((proc (my:claude-session-process session)))
    (when (process-live-p proc)
      (process-send-eof proc))))

;;;###autoload
(defun my:claude-interrupt ()
  "応答中の claude を中断する。セッションは生き残る。"
  (interactive)
  (let ((session (my:claude--session-for-buffer)))
    (unless session (user-error "セッションが無い"))
    (my:claude-interrupt-session session)))

;;;###autoload
(defun my:claude-quit ()
  "セッションを終了する。"
  (interactive)
  (let ((session (my:claude--session-for-buffer)))
    (unless session (user-error "セッションが無い"))
    (my:claude-quit-session session)))

;;; 入力バッファ

(defvar my:claude--input-history nil
  "送信したプロンプトの履歴。新しいものが先頭。")

(defvar-local my:claude--input-index -1
  "入力バッファで履歴をたどっている位置。-1 は「たどっていない」。")

(defvar-local my:claude--input-draft nil
  "履歴をたどり始めたときに書きかけだった内容。")

(defun my:claude--input-replace (text)
  (erase-buffer)
  (insert (or text ""))
  (goto-char (point-max)))

(defun my:claude-input-previous ()
  "1 つ前に送ったプロンプトを呼び出す。"
  (interactive)
  (unless my:claude--input-history (user-error "履歴が無い"))
  (when (< my:claude--input-index 0)
    (setq my:claude--input-draft
          (buffer-substring-no-properties (point-min) (point-max))))
  (setq my:claude--input-index
        (min (1- (length my:claude--input-history)) (1+ my:claude--input-index)))
  (my:claude--input-replace (nth my:claude--input-index my:claude--input-history)))

(defun my:claude-input-next ()
  "1 つ後のプロンプトに戻る。先頭まで来たら書きかけの内容に戻す。"
  (interactive)
  (when (>= my:claude--input-index 0)
    (setq my:claude--input-index (1- my:claude--input-index))
    (my:claude--input-replace
     (if (< my:claude--input-index 0)
         my:claude--input-draft
       (nth my:claude--input-index my:claude--input-history)))))

(defun my:claude-input ()
  "送信するテキストを書くバッファを開く。"
  (interactive)
  (let* ((session (or (my:claude--session-for-buffer) (my:claude)))
         (buf (get-buffer-create "*claude-input*")))
    (with-current-buffer buf
      (my:claude-input-mode)
      (setq my:claude--session session))
    (pop-to-buffer buf)))

(defun my:claude-input-send ()
  "入力バッファの内容を送って空にする。"
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (session my:claude--session))
    (my:claude-send-string text session)
    (unless (string-empty-p (string-trim text))
      (setq my:claude--input-history
            (cons text (delete text my:claude--input-history))))
    (erase-buffer)
    (setq my:claude--input-index -1
          my:claude--input-draft nil)
    (when-let* ((buf (and session (my:claude-session-buffer session))))
      (display-buffer buf))))

(defun my:claude-toggle-fold ()
  "折りたたんだツール出力の全体を別バッファで見る。"
  (interactive)
  (if-let* ((full (get-text-property (point) 'my:claude-full)))
      (let ((buf (get-buffer-create "*claude tool output*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert full)
            (goto-char (point-min))
            (special-mode)))
        (display-buffer buf))
    (user-error "ここには折りたたまれた出力が無い")))

;;; --------------------------------------------------
;;; メジャーモード
;;; --------------------------------------------------

(defvar my:claude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'my:claude-input)
    (define-key map (kbd "C-c C-i") #'my:claude-input)
    (define-key map (kbd "TAB") #'my:claude-toggle-fold)
    (define-key map (kbd "C-c C-k") #'my:claude-interrupt)
    (define-key map (kbd "q") #'quit-window)
    map)
  "`my:claude-mode' のキーマップ。")

(define-derived-mode my:claude-mode special-mode "Claude"
  "claude との会話を表示するモード。"
  (setq-local truncate-lines nil)
  (setq-local mode-line-process '(:eval (my:claude--mode-line))))

(defun my:claude--mode-line ()
  (let ((s my:claude--session))
    (if (null s) ""
      (format " [%s%s]"
              (if (my:claude-session-busy s) "..." "-")
              (let ((r (my:claude-session-last-result s)))
                (if r (format " $%.2f" (or (alist-get 'total_cost_usd r) 0.0)) ""))))))

(defvar my:claude-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my:claude-input-send)
    (define-key map (kbd "C-c C-k") #'quit-window)
    (define-key map (kbd "M-p") #'my:claude-input-previous)
    (define-key map (kbd "M-n") #'my:claude-input-next)
    map)
  "`my:claude-input-mode' のキーマップ。")

(define-derived-mode my:claude-input-mode text-mode "Claude-Input"
  "claude に送るテキストを書くモード。"
  (setq-local header-line-format
              "C-c C-c 送信 / C-c C-k 閉じる / 行頭 / は TAB 補完 / M-p 履歴")
  ;; cape-file が深さ 90 にいる。念のため明示的に先頭へ置く。
  (add-hook 'completion-at-point-functions #'my:claude--capf -100 t))

;;; --------------------------------------------------
;;; グローバルキーバインド
;;; --------------------------------------------------

;; C-c a を prefix にする。c は compile、g は diff-hl、l は eglot、
;; p は projectile、! は flymake で埋まっている。
(use-package emacs
  :bind (("C-c a a" . my:claude)
         ("C-c a e" . my:claude-switch-environment)
         ("C-c a t" . my:claude-trust-workspace)
         ("C-c a c" . my:claude-continue)
         ("C-c a m" . my:claude-set-model)
         ("C-c a i" . my:claude-input)
         ("C-c a s" . my:claude-send-region)
         ("C-c a k" . my:claude-interrupt)
         ("C-c a q" . my:claude-quit)))

(provide 'my-claude)
;;; my-claude.el ends here
