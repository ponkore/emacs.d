;;; my-claude.el --- Claude Code を stream-json で使う  -*- lexical-binding: t -*-
;;; Commentary:
;; Windows の Emacs には PTY が無いので、claude の対話 TUI はそのままでは動かない。
;; 代わりに claude が持っている双方向のストリーミング JSON 入出力
;; (`--input-format stream-json' / `--output-format stream-json') を素のパイプで
;; 駆動する。端末エミュレーションも常駐プロキシも要らない。
;;
;; 設計と実測は docs/emacs-claude-stream-json-plan.md、
;; PTY プロキシ方式との比較は docs/emacs-claude-pty-proxy-study.md を参照。
;;
;; 構成:
;;   *claude: PROJECT*        会話の記録 (読み取り専用、`my:claude-mode')
;;   *claude-input: PROJECT*  送信するテキストを書く (`my:claude-input-mode')
;;   *claude-log: PROJECT*    生の JSON Lines (`my:claude-log' が非 nil のとき)
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
  name           ; プロジェクト名
  (pending "")   ; フィルタの未処理バイト
  session-id
  model
  (busy nil)     ; 応答待ちか
  (tool-names (make-hash-table :test 'equal)) ; tool_use_id -> ツール名
  (approved nil) ; このセッションで自動許可すると決めたツール名
  last-result)   ; 直近の result イベント (alist)

(defvar-local my:claude--session nil
  "そのバッファが属するセッション。会話バッファと入力バッファに入る。")

(defvar my:claude--sessions nil
  "生きているセッションのリスト。")

(defun my:claude--project-name ()
  "現在のプロジェクト名。無ければディレクトリ名。"
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  (and (fboundp 'project-current)
                       (let ((p (project-current)))
                         (and p (project-root p))))
                  default-directory)))
    (cons (expand-file-name root)
          (file-name-nondirectory (directory-file-name root)))))

(defun my:claude--session-for-buffer ()
  "このバッファに紐づくセッション、無ければ nil。"
  (or my:claude--session
      (car my:claude--sessions)))

(defun my:claude--live-session (name)
  "NAME のセッションが生きていれば返す。"
  (seq-find (lambda (s)
              (and (equal (my:claude-session-name s) name)
                   (process-live-p (my:claude-session-process s))))
            my:claude--sessions))

;;; --------------------------------------------------
;;; プロセスの起動
;;; --------------------------------------------------

(defun my:claude--command ()
  "claude に渡す引数リスト。
`--verbose' と `--permission-prompt-tool stdio' は省略できない。
前者は無いと即エラー終了し、後者は無いと許可要求が黙って自動拒否される。"
  (append
   (list my:claude-executable
         "-p" "--verbose"
         "--input-format" "stream-json"
         "--output-format" "stream-json"
         "--permission-prompt-tool" "stdio")
   (when my:claude-model (list "--model" my:claude-model))
   (when my:claude-permission-mode
     (list "--permission-mode" my:claude-permission-mode))
   my:claude-extra-args))

(defun my:claude--start (dir name)
  "DIR で claude を起動して session 構造体を返す。"
  (unless (file-executable-p my:claude-executable)
    (user-error "claude が見つからない: %s" my:claude-executable))
  (let* ((conv (get-buffer-create (format "*claude: %s*" name)))
         (log  (when my:claude-log
                 (get-buffer-create (format "*claude-log: %s*" name))))
         (session (my:claude--make-session
                   :buffer conv :log-buffer log
                   :directory dir :name name))
         proc)
    (setq proc
          ;; my-japanese.el が default-process-coding-system の cdr を cp932 に
          ;; しているので、束縛せずに起動すると標準入力の日本語が壊れる。
          ;; ここは引数ではなく標準入力で本文を渡す経路なので utf-8 でよい。
          (let ((default-process-coding-system '(utf-8-unix . utf-8-unix))
                ;; Rust/Node 側は `~' を展開しないので必ず絶対パスにする
                ;; (gitd で os error 267 を踏んでいる)。
                (default-directory dir))
            (make-process
             :name (format "claude-%s" name)
             :buffer nil                ; 出力は自前のフィルタで捌く
             :connection-type 'pipe
             :noquery t
             :command (my:claude--command)
             :filter (lambda (_p str) (my:claude--filter session str))
             :sentinel (lambda (_p e) (my:claude--sentinel session e)))))
    (setf (my:claude-session-process session) proc)
    (push session my:claude--sessions)
    (with-current-buffer conv
      (my:claude-mode)
      (setq my:claude--session session
            default-directory dir))
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
    (setq my:claude--sessions (delq session my:claude--sessions))))

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
        (condition-case err
            (my:claude--handle (json-parse-string line :object-type 'alist)
                               session)
          (error
           ;; パースにも処理にも失敗した行は捨てずに見せる。
           ;; 上流のフォーマット変更に気づける唯一の手掛かり。
           (my:claude--insert
            session
            (format "[解釈できない行: %S]\n%s\n"
                    err (truncate-string-to-width line 200))
            'my:claude-error-face)))))))

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
    ("control_response" nil)
    ;; 段階 4 で使う。今は捨てる。
    ("stream_event" nil)
    ("rate_limit_event" nil)
    (_ nil)))

(defun my:claude--handle-system (obj session)
  (pcase (alist-get 'subtype obj)
    ("init"
     ;; init はターンごとに来る。バッファに挿すと会話の途中に何度も
     ;; 見出しが混ざるので、ヘッダ行に出す。
     (setf (my:claude-session-session-id session) (alist-get 'session_id obj)
           (my:claude-session-model session) (alist-get 'model obj))
     (let ((header
            (format "claude %s | %s | %s%s"
                    (or (alist-get 'claude_code_version obj) "?")
                    (or (alist-get 'model obj) "?")
                    (or (alist-get 'permissionMode obj) "?")
                    (let ((bad (seq-filter
                                (lambda (m)
                                  (not (equal (alist-get 'status m) "connected")))
                                (append (alist-get 'mcp_servers obj) nil))))
                      (if bad
                          (format " | MCP 未接続: %s"
                                  (mapconcat (lambda (m) (alist-get 'name m))
                                             bad ", "))
                        "")))))
       (when (buffer-live-p (my:claude-session-buffer session))
         (with-current-buffer (my:claude-session-buffer session)
           (setq header-line-format header)))))
    ("permission_denied"
     (my:claude--insert
      session
      (format "拒否: %s\n" (or (alist-get 'message obj) ""))
      'my:claude-error-face))
    (_ nil)))

(defun my:claude--handle-assistant (obj session)
  (let ((content (alist-get 'content (alist-get 'message obj))))
    (seq-doseq (block content)
      (pcase (alist-get 'type block)
        ("text"
         (my:claude--insert session
                            (concat (string-trim-right (alist-get 'text block)) "\n\n")
                            'my:claude-assistant-face))
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

(defun my:claude--respond-permission (session rid behavior input)
  (my:claude--send-json
   session
   `((type . "control_response")
     (response . ((subtype . "success")
                  (request_id . ,rid)
                  (response . ((behavior . ,behavior)
                               (updatedInput . ,input))))))))

(defun my:claude--ask-permission (_obj session rid req)
  "ツール使用の可否を尋ねて control_response を返す。"
  (let* ((name (or (alist-get 'tool_name req) "?"))
         (desc (or (alist-get 'description req) ""))
         (input (alist-get 'input req)))
    (if (my:claude--auto-approve-p session name)
        (progn
          (my:claude--insert session (format "  (自動許可: %s)\n" name)
                             'my:claude-meta-face)
          (my:claude--respond-permission session rid "allow" input))
      (let (done)
        (while (not done)
          (pcase (car (read-multiple-choice
                       (format "%s %s を許可する?"
                               name (truncate-string-to-width desc 60 nil nil "…"))
                       '((?y "今回だけ許可")
                         (?n "拒否")
                         (?a "以後このツールは聞かない")
                         (?v "入力を全部見る"))))
            (?y (my:claude--respond-permission session rid "allow" input)
                (setq done t))
            (?n (my:claude--insert session (format "  (拒否: %s)\n" name)
                                   'my:claude-error-face)
                (my:claude--respond-permission session rid "deny" input)
                (setq done t))
            (?a (push name (my:claude-session-approved session))
                (my:claude--respond-permission session rid "allow" input)
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
(defun my:claude ()
  "このプロジェクトの claude セッションを開く。無ければ起動する。"
  (interactive)
  (let* ((pair (my:claude--project-name))
         (dir (car pair))
         (name (cdr pair))
         (session (or (my:claude--live-session name)
                      (my:claude--start dir name))))
    (pop-to-buffer (my:claude-session-buffer session))
    session))

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

(defun my:claude-input ()
  "送信するテキストを書くバッファを開く。"
  (interactive)
  (let* ((session (or (my:claude--session-for-buffer) (my:claude)))
         (buf (get-buffer-create
               (format "*claude-input: %s*" (my:claude-session-name session)))))
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
    (erase-buffer)
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
    map)
  "`my:claude-input-mode' のキーマップ。")

(define-derived-mode my:claude-input-mode text-mode "Claude-Input"
  "claude に送るテキストを書くモード。"
  (setq-local header-line-format "C-c C-c で送信 / C-c C-k で閉じる"))

;;; --------------------------------------------------
;;; グローバルキーバインド
;;; --------------------------------------------------

;; C-c a を prefix にする。c は compile、g は diff-hl、l は eglot、
;; p は projectile、! は flymake で埋まっている。
(use-package emacs
  :bind (("C-c a a" . my:claude)
         ("C-c a i" . my:claude-input)
         ("C-c a s" . my:claude-send-region)
         ("C-c a k" . my:claude-interrupt)
         ("C-c a q" . my:claude-quit)))

(provide 'my-claude)
;;; my-claude.el ends here
