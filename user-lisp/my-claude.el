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
;;   *claude(PROJ)*        会話の記録 (読み取り専用、`my:claude-mode')
;;   *claude-input(PROJ)*  送信するテキストを書く (`my:claude-input-mode')
;;   *claude-log(PROJ)*    生の JSON Lines (`my:claude-log' が非 nil のとき)
;;
;; PROJ は作業ディレクトリの名前 (`my:claude--buffer-name')。セッションは
;; 1 つに限っているので衝突避けではなく、どのプロジェクトに向かって話して
;; いるのかをバッファ一覧から見えるようにするためのもの。
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

(defcustom my:claude-effort nil
  "起動時に渡す `--effort'。nil なら settings.json の効き目に任せる。

**stream-json は effort level を返さない** (全イベントの全キーを
列挙して確認した)。そのためヘッダ行に出す値は、ここが nil のときは
`settings.json' を自分で読んで求める (`my:claude--effort')。
非 nil なら `--effort' で明示するので、その値がそのまま効く。"
  :type '(choice (const :tag "settings.json に任せる" nil)
                 (const "low") (const "medium") (const "high")
                 (const "xhigh") (const "max")))

(defcustom my:claude-permission-mode nil
  "起動時に渡す `--permission-mode'。nil なら指定しない。"
  :type '(choice (const :tag "既定" nil)
                 (const "acceptEdits") (const "auto") (const "plan")
                 (const "manual") (const "dontAsk") (const "bypassPermissions")))

(defcustom my:claude-extra-args nil
  "起動時に追加で渡す引数のリスト。"
  :type '(repeat string))

(defcustom my:claude-log nil
  "非 nil なら受信した生の JSON Lines を *claude-log(PROJ)* に残す。
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

(defcustom my:claude-forward-subagent-text t
  "非 nil なら `--forward-subagent-text' を付ける。

サブエージェントの発言は `parent_tool_use_id' 付きの assistant / user
イベントとして届く。**このフラグが無くても一部は届く** (実測) ので、
表示側は常に親 ID を見て字下げする。"
  :type 'boolean)

(defcustom my:claude-show-thinking nil
  "非 nil なら thinking ブロックの中身も薄く表示する。

モデルによっては `thinking_delta' の本文が空で届く (haiku で実測)。
その場合は非 nil にしても何も出ない。"
  :type 'boolean)

(defcustom my:claude-tool-result-max-lines 0
  "ツールの実行結果を畳まずに見せる行数。これを超えると折りたたむ。

**既定は 0 = 全部畳む。** 結果の中身は読み飛ばすことのほうが多く、
開いたままだと肝心の会話が流れてしまう。畳んだ行には 1 行要約
 (`● Read(foo.el) … 42 行') を出し、全体は TAB
 (`my:claude-toggle-fold') で別バッファに開く。"
  :type 'integer)

(defcustom my:claude-error-result-max-lines 30
  "エラーになったツール出力を畳まずに見せる行数。

エラーだけは既定で畳まない。畳むと「なぜ失敗したか」がその場から
消えてしまい、`my:claude-tool-result-max-lines' を 0 にした意味
 (雑音を減らす) とは逆に、いちばん見たいものが隠れる。
ただしビルドの失敗などで数百行来ることがあるので上限は設ける。"
  :type 'integer)

(defcustom my:claude-fontify-code t
  "非 nil なら ```lang のコードブロックをその言語として着色する。"
  :type 'boolean)

(defcustom my:claude-fontify-code-max-lines 300
  "コードブロックを言語として着色する上限の行数。
これを超えたら `my:claude-code-face' の単色のままにする。
長い出力が続いたときに描画が詰まらないようにするための保険。"
  :type 'integer)

(defcustom my:claude-render-tables t
  "非 nil なら markdown のパイプ表を罫線 (box-drawing) の表に組み直す。

【重要】桁は **Emacs バッファの規則** (`site-lisp/eaw.el'、ambiguous は
幅 2) で決める。claude 側の桁組みには合わせない。セルの中身だけを
取り出して `string-width' で組み直すので、元の桁は信用しない。

`my-pty' が端末の中で ambiguous を幅 1 に切り替えているのとは
**逆の話**。あちらは桁を数えているのが conhost なので合わせにいくが、
こちらは Emacs 自身が数えるので合わせる必要が無い。"
  :type 'boolean)

(defcustom my:claude-window-height-ratio 0.5
  "`my:claude-layout' で会話 + 入力に使うフレーム高さの割合。"
  :type 'number)

(defcustom my:claude-input-window-height 6
  "`my:claude-layout' で入力バッファに使う行数。"
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

(defface my:claude-subagent-face
  '((t :inherit font-lock-doc-face))
  "サブエージェントの発言。")

(defface my:claude-diff-removed-face
  '((((background dark))  :background "#3a1d1f" :extend t)
    (((background light)) :background "#ffe6e6" :extend t))
  "差分の削除行。")

(defface my:claude-diff-added-face
  '((((background dark))  :background "#1d3a24" :extend t)
    (((background light)) :background "#e6ffe6" :extend t))
  "差分の追加行。")

(defface my:claude-meta-face
  '((t :inherit shadow :height 0.9))
  "コスト・所要時間などの補足。")

;; ヘッダ行の 6 列。`~/.claude/statusline-command.sh' が端末の TUI で
;; 使っている ANSI 色に合わせてある (plan=マゼンタ / dir=シアン /
;; branch=グリーン / model=イエロー / ctx=グリーン / limit=シアン)。
;; `:foreground' だけを指定する。ヘッダ行では `header-line' face が
;; 下地になり、テキストプロパティの face はその上に重なるので、
;; 背景はテーマのものがそのまま残る。
(defface my:claude-header-plan-face
  '((((background dark))  :foreground "magenta")
    (((background light)) :foreground "dark magenta"))
  "ヘッダ行 1 列目 (アカウントとプラン)。")

(defface my:claude-header-dir-face
  '((((background dark))  :foreground "cyan")
    (((background light)) :foreground "dark cyan"))
  "ヘッダ行 2 列目 (プロジェクト名)。")

(defface my:claude-header-branch-face
  '((((background dark))  :foreground "green")
    (((background light)) :foreground "dark green"))
  "ヘッダ行 3 列目 (git ブランチ)。")

(defface my:claude-header-model-face
  '((((background dark))  :foreground "yellow")
    (((background light)) :foreground "dark goldenrod"))
  "ヘッダ行 4 列目 (モデルと effort)。")

(defface my:claude-header-context-face
  '((((background dark))  :foreground "green")
    (((background light)) :foreground "dark green"))
  "ヘッダ行 5 列目 (コンテキスト使用量)。")

(defface my:claude-header-limit-face
  '((((background dark))  :foreground "cyan")
    (((background light)) :foreground "dark cyan"))
  "ヘッダ行 6 列目 (レート上限とリセット時刻)。")

(defface my:claude-header-cost-face
  '((t :inherit shadow))
  "ヘッダ行 7 列目 (セッション累計コストと応答待ちの `...')。

ここだけ色名を直接書かず `shadow' を継ぐ。statusline スクリプトが
コストを C_DIM (ANSI の dim) で出しており、dim に対応する固定の色が
無いため。`shadow' は前景しか持たないので、背景がテーマのまま残る点は
他の列と同じ。")

(defface my:claude-input-header-face
  '((((background dark))  :foreground "cyan")
    (((background light)) :foreground "dark cyan"))
  "入力バッファ (`my:claude-input-mode') のヘッダ行 (キーの案内)。

色は会話バッファのヘッダ行と揃えてシアン。大きさは
`my:claude-header-size-face' が持つので、ここでは指定しない。")

(defface my:claude-header-size-face
  '((t :height 0.9))
  "ヘッダ行の大きさ。会話バッファと入力バッファで共通。

`my:claude--header-segment' が色の face と**並べて**載せる。
face をリストにすると先に書いたものが勝つので、色は列ごとの face から、
大きさはこちらから来る。

【重要】`:height' は相対値 (浮動小数) にすること。整数は絶対値なので、
フォントサイズを変えたときにヘッダ行だけ取り残される。

【重要】**区切りの \" | \" にも載せること。** 行の高さは行内でいちばん
高いグリフで決まるので、1 か所でも素のままだと行は縮まない。")

;;; --------------------------------------------------
;;; セッション
;;; --------------------------------------------------

(cl-defstruct (my:claude-session (:constructor my:claude--make-session)
                                 (:copier nil))
  process        ; プロセス
  buffer         ; 会話バッファ
  log-buffer     ; 生 JSON のバッファ (nil のことがある)
  directory      ; 起動した default-directory (展開済み)
  gitdir         ; directory を含むリポジトリの .git (nil なら git 管理外)
  branch-cache   ; (FINGERPRINT . BRANCH)。`my:claude--branch' が使う
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
  claude-version   ; system/init の claude_code_version
  effort           ; effort level (settings.json から求めたもの)
  context-tokens   ; 直近の assistant の message.usage の合計
  context-window   ; result の modelUsage.<model>.contextWindow
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

(defun my:claude--guess-directory ()
  "claude を動かすディレクトリを自動で決める。決められなければ nil。

**さかのぼりはしない。** Emacs らしく projectile の判定を最優先し、
外れたら cwd に `.claude/' があるかだけを見る。

  1. projectile のプロジェクトルート
  2. cwd に `.claude/' があれば cwd

`project.el' は見ない。projectile が既に同じ役目を負っているうえ、
両方を並べると「どちらが決めたのか」が説明できなくなる。
どちらも外れたときは `my:claude--project-directory' が確認を取る。

cwd が変わるとセッション記録の置き場
 (`<CLAUDE_CONFIG_DIR>/projects/<エンコードしたパス>/') も変わり、
`C-c a r' の一覧に出る対象も変わる。黙って決めてよい範囲を
この 2 つに絞ってあるのはそのため。"
  (let ((here (expand-file-name default-directory)))
    (cond
     ((and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (expand-file-name (projectile-project-root)))
     ((file-directory-p (expand-file-name ".claude" here)) here))))

(defun my:claude--project-directory ()
  "claude を動かすディレクトリ。自動で決まらなければ確認して決める。

`my:claude--guess-directory' が nil を返したときは `default-directory'
を候補にして `y/n' で聞く。拒否されたらディレクトリを読ませる
 (起動そのものは中止しない)。"
  (or (my:claude--guess-directory)
      (let ((here (expand-file-name default-directory)))
        (if (y-or-n-p (format "%s で claude を起動する? "
                              (abbreviate-file-name here)))
            here
          (expand-file-name
           (read-directory-name "claude を起動するディレクトリ: "
                                here nil t))))))

(defun my:claude--buffer-name (base dir)
  "BASE と作業ディレクトリ DIR からバッファ名を作る。

  (my:claude--buffer-name \"claude\" \"c:/Users/masao/.emacs.d/\")
  => \"*claude(.emacs.d)*\"

DIR が nil なら従来どおりプロジェクト名を付けない (`*claude*')。

セッションは Emacs 全体で 1 つに限っている (`my:claude--the-session')
ので名前が衝突することは無いが、**どのプロジェクトに向かって話して
いるのかはバッファ名から見えたほうがよい**。ヘッダ行の 2 列目に出して
いるものと同じ値 (`file-name-nondirectory' + `directory-file-name')。"
  (if dir
      (format "*%s(%s)*" base
              (file-name-nondirectory (directory-file-name dir)))
    (format "*%s*" base)))

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
   (when my:claude-forward-subagent-text (list "--forward-subagent-text"))
   (when my:claude-model (list "--model" my:claude-model))
   (when my:claude-effort (list "--effort" my:claude-effort))
   (when my:claude-permission-mode
     (list "--permission-mode" my:claude-permission-mode))
   my:claude-extra-args))

(defun my:claude--start (dir env &optional resume)
  "環境 ENV で DIR に claude を起動して session 構造体を返す。
RESUME は `my:claude--command' に渡す (t で --continue、文字列で --resume)。"
  (unless (file-executable-p my:claude-executable)
    (user-error "claude が見つからない: %s" my:claude-executable))
  (let* ((config-dir (my:claude--config-dir env))
         (conv (get-buffer-create (my:claude--buffer-name "claude" dir)))
         (log  (when my:claude-log
                 (get-buffer-create (my:claude--buffer-name "claude-log" dir))))
         (session (my:claude--make-session
                   :buffer conv :log-buffer log
                   :directory dir :name env :config-dir config-dir
                   ;; DIR は起動後に変わらないので、探索はここで 1 回だけ。
                   ;; ヘッダ行はこれが非 nil のときだけブランチの列を出す。
                   :gitdir (my:claude--git-dir dir)))
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

(defmacro my:claude--at-end (session &rest body)
  "SESSION の会話バッファの末尾で BODY を評価し、**追従を保つ**。

**末尾を見ているときだけ追従する。** 読み返している最中に飛ばされるのは
鬱陶しいため。判定は **ウィンドウごとに `window-point\' で行う**。
バッファの `point\' 1 つで決めていると、`my:claude-layout\' のように
会話バッファが複数のウィンドウに出たとき、片方が末尾にいるだけで
読み返している側まで飛ばされる (あるいはその逆)。

バッファ自身の `point\' も別に見る。ウィンドウに出ていない間に届いた
ぶんで追従が切れると、次に表示したときに古い位置から始まってしまう。

【重要】BODY のあとの `goto-char\' / `set-window-point\' を省いてはいけない。
`save-excursion\' が使うマーカーは insertion-type が nil なので、
**末尾での挿入では挿入したテキストの前に取り残される**。一度でも
末尾から外れたバッファは、以後どれだけ流れても二度と追従しない。

  (save-excursion (goto-char (point-max)) (insert \"    +new\\n\"))
  => point=5 point-max=14   ; 末尾にいたのに外れる (実測)

差分表示 (`my:claude--insert-diff\') と段落の整形
 (`my:claude--end-paragraph\') がこれを持たずに直接書いており、
**差分が 1 回出ると自動スクロールが止まっていた**。"
  (declare (indent 1) (debug (form body)))
  (let ((buf (gensym "buf")) (max (gensym "max"))
        (at-end (gensym "at-end")) (wins (gensym "wins")) (w (gensym "w")))
    `(let ((,buf (my:claude-session-buffer ,session)))
       (when (buffer-live-p ,buf)
         (with-current-buffer ,buf
           (let* ((inhibit-read-only t)
                  (,max (point-max))
                  (,at-end (>= (point) ,max))
                  (,wins (seq-filter (lambda (,w) (>= (window-point ,w) ,max))
                                     (get-buffer-window-list ,buf nil t))))
             (save-excursion
               (goto-char (point-max))
               ,@body)
             (when ,at-end (goto-char (point-max)))
             (dolist (,w ,wins)
               (set-window-point ,w (point-max)))))))))

(defun my:claude--insert (session text &optional face)
  "SESSION の会話バッファの末尾に TEXT を挿入する。
追従の作法は `my:claude--at-end\' を参照。"
  (my:claude--at-end session
    (insert (if face (propertize text 'font-lock-face face) text))))

(defun my:claude--insert-block (session text face)
  "TEXT を字下げして挿入する。"
  (my:claude--insert
   session
   (mapconcat (lambda (l) (concat "  " l))
              (split-string (string-trim-right text) "\n")
              "\n")
   face)
  (my:claude--insert session "\n"))

(defun my:claude--fold (session text face &optional label)
  "ツールの実行結果 TEXT を畳んで 1 行にまとめる。

LABEL は `Read(foo.el)\' のような呼び出しの要約 (`my:claude--tool-summary\')。

**既定では全部畳む** (`my:claude-tool-result-max-lines\' が 0)。
`my:claude-error-result-max-lines\' 行までのエラーだけは畳まない。
全文はテキストプロパティに持たせ、TAB (`my:claude-toggle-fold\') で
別バッファに開く。"
  (let* ((lines (split-string (string-trim-right text) "\n"))
         (n (length lines))
         (keep (if (eq face 'my:claude-error-face)
                   my:claude-error-result-max-lines
                 my:claude-tool-result-max-lines)))
    (if (<= n keep)
        (my:claude--insert-block session text face)
      (my:claude--insert
       session
       (format "  ● %s … %d 行\n" (or label "出力") n)
       (if (eq face 'my:claude-error-face) 'my:claude-error-face
         'my:claude-meta-face))
      ;; 全文はテキストプロパティに持たせておく。載せる範囲はいま書いた
      ;; 1 行だけ。point-max は挿入した改行の**次**の行頭にあるので、
      ;; そこから 1 行戻ったところが要約行の先頭になる。
      (with-current-buffer (my:claude-session-buffer session)
        (let* ((inhibit-read-only t)
               (beg (save-excursion (goto-char (point-max)) (forward-line -1) (point))))
          (put-text-property beg (point-max) 'my:claude-full text))))))

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

(defun my:claude--format-tokens (n)
  "トークン数 N を 103.2k のように短く書く。"
  (cond ((not (numberp n)) "?")
        ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
        (t (format "%d" n))))

;;; effort level

(defun my:claude--json-file (file)
  "FILE を JSON として読んで alist で返す。読めなければ nil。"
  (when (file-readable-p file)
    (ignore-errors
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist)))))

(defun my:claude--settings-files (session)
  "SESSION に効く settings.json を、優先順位の高い順に返す。

claude 自身の順序 (プロジェクトのローカル → プロジェクト共有 → ユーザ)
に合わせてある。ユーザ側は `.claude.json' と違って
**CLAUDE_CONFIG_DIR の下の `settings.json'** なので、
`my:claude--config-json' とは組み立て方が違う点に注意。"
  (let ((dir (my:claude-session-directory session)))
    (list (expand-file-name ".claude/settings.local.json" dir)
          (expand-file-name ".claude/settings.json" dir)
          (expand-file-name "settings.json"
                            (or (my:claude-session-config-dir session)
                                (expand-file-name "~/.claude"))))))

(defun my:claude--effort-in (settings model)
  "SETTINGS (alist) から MODEL の effortLevel を取り出す。

`modelSettings' のほうが上位。キーは `claude-opus-5' のように
日付の付かない名前で、`system/init' が返すのは
`claude-haiku-4-5-20251001' のような日付付きのことがあるので、
**完全一致ではなく前方一致**で突き合わせる。"
  (or (seq-some (lambda (kv)
                  (and (string-prefix-p (symbol-name (car kv)) (or model ""))
                       (alist-get 'effortLevel (cdr kv))))
                (alist-get 'modelSettings settings))
      (alist-get 'effortLevel settings)))

(defun my:claude--effort (session)
  "SESSION の effort level。分からなければ nil。

`my:claude-effort' が非 nil ならそれ (`--effort' で明示している)。
そうでなければ settings.json を読んで求める。**stream-json は
effort を返さない**ので、ここでしか分からない。"
  (or my:claude-effort
      (let ((model (my:claude-session-model session)))
        (seq-some (lambda (f)
                    (when-let* ((conf (my:claude--json-file f)))
                      (my:claude--effort-in conf model)))
                  (my:claude--settings-files session)))))

;;; ------------------------------------------------ git ブランチ (3 列目)

;; **git は呼ばない。`.git/HEAD' を読むだけで足りる。**
;; 実測 (batch、`~/.emacs.d'、1 回あたり):
;;
;;   file-attributes で stat       0.043 ms   キャッシュのヒット判定
;;   HEAD を読んでパース           0.061 ms   キャッシュのミス時
;;   call-process git rev-parse   55.6   ms   参考
;;
;; 1300 倍違うので `header-line-format' の `:eval' から毎回呼んでよい。
;; 「Emacs の `call-process' が Windows で遅い」(CLAUDE.md の既知の課題) を
;; 丸ごと迂回できる。ブランチの切り替えは Emacs の外 (端末や magit) でも
;; 起きるので、ターンごとの更新では古い表示が残る。`:eval' なら再描画の
;; たびに追随するので、監視もタイマーも要らない。

(defun my:claude--git-dir (dir)
  "DIR を含むリポジトリの .git ディレクトリを返す。git 管理外なら nil。

**git は呼ばない** (`locate-dominating-file' で上へ探すだけ)。
worktree と submodule では `.git' がディレクトリではなくファイルで、
中身が `gitdir: PATH' なのでそれを辿る。

セッションの作業ディレクトリは起動後に変わらないので、呼ぶのは
セッションを作るときの 1 回だけでよい (`my:claude-session-gitdir')。"
  (and-let* ((root (locate-dominating-file dir ".git"))
             (g (expand-file-name ".git" root)))
    (cond
     ((file-directory-p g) (file-name-as-directory g))
     ((file-regular-p g)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents g nil 0 1024))
        (goto-char (point-min))
        (when (looking-at "gitdir: *\\([^\n\r]+\\)")
          (file-name-as-directory
           (expand-file-name (string-trim (match-string 1)) root))))))))

(defun my:claude--head-branch (gitdir)
  "GITDIR の HEAD からブランチ名を返す。detached なら短縮 SHA。

**`literally' で読んで自分で decode する。** git はブランチ名を UTF-8 の
バイト列で書くので、`insert-file-contents-literally' が作る unibyte の
ままでは非 ASCII のブランチ名が化ける。"
  (let ((head (expand-file-name "HEAD" gitdir)))
    (when (file-readable-p head)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents-literally head nil 0 512)
          (goto-char (point-min))
          (cond
           ((looking-at "ref: refs/heads/\\([^\n\r]+\\)")
            (decode-coding-string (match-string 1) 'utf-8))
           ;; detached HEAD は生の SHA が入っている
           ((looking-at "\\([0-9a-f]\\{7\\}\\)") (match-string 1))))))))

(defun my:claude--branch (session)
  "SESSION のリポジトリの現在のブランチ。git 管理外なら nil。

`.git/HEAD' の (mtime . size) をフィンガープリントにしてキャッシュする
\(`my-magit-watch' の `my:magit-watch--fingerprint' と同じ手口)。
ヒットするかぎり stat 1 回で済む。"
  (and-let* ((gitdir (my:claude-session-gitdir session))
             (attr (file-attributes (expand-file-name "HEAD" gitdir))))
    (let ((fp (cons (file-attribute-modification-time attr)
                    (file-attribute-size attr)))
          (cache (my:claude-session-branch-cache session)))
      (if (equal (car cache) fp)
          (cdr cache)
        (let ((branch (my:claude--head-branch gitdir)))
          (setf (my:claude-session-branch-cache session) (cons fp branch))
          branch)))))

(defun my:claude--branch-segment ()
  "ヘッダ行 3 列目 (git ブランチ)。`header-line-format' の `:eval' から呼ぶ。

【重要】`:eval' の戻り値は mode-line 構文として**再解釈される**ので、
`%' の escape は依然として要る (`my:claude--header-segment' が済ませる)。
ブランチ名に `%' を入れることはできる。

読めなかったときは `?' を出す。空文字列を返すと区切りだけが残るため。
列そのものを出すかどうかは gitdir の有無で `my:claude--header' が
決めており、そちらは起動後に変わらない。"
  (when-let* ((session my:claude--session))
    (my:claude--header-segment (or (my:claude--branch session) "?")
                               'my:claude-header-branch-face)))

;;; ------------------------------------------------ ヘッダ行の組み立て

(defconst my:claude--header-separator
  (propertize " | " 'face 'my:claude-header-size-face)
  "ヘッダ行の列の区切り。

**大きさの face を載せること。** 行の高さは行内でいちばん高いグリフで
決まるので、1 か所でも素のままだとヘッダ行が縮まない。

`:eval' の列が自分で区切りを出す必要がある (末尾の
`my:claude--cost-segment') ので、定数にして共有している。")

(defun my:claude--header-segment (text face &rest props)
  "TEXT を FACE で色づけしてヘッダ行の 1 列にする。PROPS も載せる。

【重要】`%\' の escape は **列ごとに、色を付ける前に** 済ませる。
`header-line-format\' に素の文字列を渡しているので、`%\' は mode-line の
書式指定子として解釈され、`%\' と**直後の 1 文字**がまとめて消える
 (`5h 4%% 7d 8%%\' が `5h 47d 8\' になっていた)。

組み立てた全体に `replace-regexp-in-string\' を掛けると、**差し込まれる
`%%\' だけが face を持たない**素の文字列になり、その桁で色が切れる。
ディレクトリ名やモデル名に `%\' が入る場合もあるので、escape 自体は
やめられない。

face は `my:claude-header-size-face\' と**並べたリスト**にする。
リストは先に書いたものが勝つので、色は FACE から、大きさは
size face から来る。大きさを 1 か所で決めるためにこうしてある。"
  (apply #'propertize (replace-regexp-in-string "%" "%%" text)
         'face (list face 'my:claude-header-size-face) props))

(defun my:claude--cost-segment ()
  "ヘッダ行の最後の列。セッション累計コストと、応答待ちの `...'。

`header-line-format\' の `:eval\' から呼ぶ。**モードラインではなく
ここに出す。** かつては `mode-line-process\' に
`[プロジェクト名 ... $0.12]\' を出していたが、ヘッダ行と重複していた。

`:eval\' にするのは応答待ちの表示のため。`busy\' は送信した時点で立ち、
`result\' で降りる。ターンごとの `my:claude--update-header\' では
立ち上がりに間に合わない。

コストは直近の `result\' の `total_cost_usd\'。**1 往復ぶんではなく
セッション開始からの累計**が来る (`--resume\' で継いだ会話ぶんを含む)。

**区切りも自分で出す。** 起動直後は `result\' がまだ来ておらず応答待ちでも
ないので何も出さないが、そのとき区切りだけが末尾に残らないようにするため。"
  (and-let* ((session my:claude--session)
             (parts (delq nil
                          (list (and-let* ((r (my:claude-session-last-result session))
                                           (c (alist-get 'total_cost_usd r)))
                                  (format "$%.2f" c))
                                (and (my:claude-session-busy session) "...")))))
    (concat my:claude--header-separator
            (my:claude--header-segment (string-join parts " ")
                                       'my:claude-header-cost-face))))

(defun my:claude--header (session)
  "会話バッファのヘッダ行。

**表示はここに集約する。** 7 列に分けて色を付けてある
 (`~/.claude/statusline-command.sh\' が端末の TUI で使っている ANSI 色に
合わせた)。モードラインには何も出さない。

  1 アカウント (プラン) と claude のバージョン   マゼンタ
  2 プロジェクト名 (フルパスは help-echo)        シアン
  3 git ブランチ                                 グリーン
  4 モデルと effort                              イエロー
  5 コンテキスト使用量                           グリーン
  6 レート上限とリセット時刻                     シアン
  7 累計コストと応答待ちの `...\'                 dim

**戻り値は文字列ではなくリスト** (mode-line 構文)。3 列目と 7 列目は
`:eval\' で、再描画のたびに評価される。ブランチの切り替えは Emacs の
外でも起き、応答待ちは送信した時点で立つので、どちらもターンごとの
更新 (`my:claude--update-header\') では追随できないため。

**7 列目は区切りも自分で出す** (`my:claude--cost-segment\')。末尾なので、
出すものが無いときに区切りだけが残らないようにする必要がある。

出す項目は `~/.claude/statusline-command.sh\' が端末の TUI に出して
いるものに合わせてある。ただし **statusLine は端末 TUI の機能で、
`-p\' (stream-json) 経路では発火しない** (実測: 出力に一切現れない)。
スクリプトの出力をもらうのではなく、同じ情報を stream-json から
自前で組み立てている。

  claude のバージョン  system/init の claude_code_version
  コンテキスト使用量   assistant の message.usage の合計 /
                       result の modelUsage.<model>.contextWindow
  レート上限とリセット rate_limit_event の unifiedWindows

git ブランチは `.git/HEAD\' を直接読む (`my:claude--branch\')。**git は
呼ばない。** stat 0.043 ms / 読み込み 0.061 ms で、`call-process\' 経由の
`git rev-parse\' (55.6 ms) の 1300 分の 1。

effort level は **stream-json に出てこない**ので、`--effort' の指定か
settings.json から求める (`my:claude--effort')。"
  (let* ((auth (gethash (my:claude-session-name session) my:claude--auth-cache))
         (rl (my:claude-session-rate-limit session))
         (w (and rl (alist-get 'unifiedWindows rl)))
         (used (my:claude-session-context-tokens session))
         (limit (or (my:claude-session-context-window session) 200000))
         (dir (my:claude-session-directory session))
         (segs nil))
    ;; [1] アカウント (プラン) と claude のバージョン
    (push (my:claude--header-segment
           (format "%s(%s)%s"
                   (my:claude-session-name session)
                   (or (alist-get 'subscriptionType auth) "?")
                   (if-let* ((v (my:claude-session-claude-version session)))
                       (format " v%s" v) ""))
           'my:claude-header-plan-face)
          segs)
    ;; [2] プロジェクト名。フルパスは help-echo に入れる。
    (push (my:claude--header-segment
           (file-name-nondirectory (directory-file-name dir))
           'my:claude-header-dir-face
           'help-echo (abbreviate-file-name dir))
          segs)
    ;; [3] git ブランチ。**列を出すかどうかはここで決める。** gitdir は
    ;; 起動後に変わらないので、変わりうるのは中身だけ。それを `:eval' に
    ;; 委ねて再描画のたびに評価させる (`my:claude--branch-segment')。
    (when (my:claude-session-gitdir session)
      (push '(:eval (my:claude--branch-segment)) segs))
    ;; [4] モデルと effort
    (when-let* ((m (my:claude-session-model session)))
      (push (my:claude--header-segment
             (concat m (if-let* ((e (my:claude-session-effort session)))
                           (format " (%s)" e) ""))
             'my:claude-header-model-face)
            segs))
    ;; [5] コンテキスト使用量
    (when (numberp used)
      (push (my:claude--header-segment
             (format "ctx %s %d%%" (my:claude--format-tokens used)
                     (round (* 100.0 (/ (float used) (max 1 limit)))))
             'my:claude-header-context-face)
            segs))
    ;; [6] レート上限とリセット時刻
    (when w
      (let ((r (alist-get 'resetsAt (alist-get 'five_hour w))))
        (push (my:claude--header-segment
               (format "(5h %d%%)(7d %d%%)%s"
                       (round (* 100 (or (alist-get 'utilization
                                                    (alist-get 'five_hour w)) 0)))
                       (round (* 100 (or (alist-get 'utilization
                                                    (alist-get 'seven_day w)) 0)))
                       (if (numberp r) (format-time-string "(reset %m/%d %H:%M)" r) ""))
               'my:claude-header-limit-face)
              segs)))
    ;; **`mapconcat' で 1 つの文字列にはしない。** `(:eval ...)' の列を
    ;; 活かすため、mode-line 構文のリストのまま返す。
    ;;
    ;; [7] は区切り込みで自分を出すので、ここで挟む対象には入れない。
    (append (cdr (mapcan (lambda (seg) (list my:claude--header-separator seg))
                         (nreverse segs)))
            (list '(:eval (my:claude--cost-segment))))))

(defun my:claude--update-header (session)
  (when (buffer-live-p (my:claude-session-buffer session))
    (with-current-buffer (my:claude-session-buffer session)
      (setq header-line-format (my:claude--header session)))))

(defun my:claude--handle-system (obj session)
  (pcase (alist-get 'subtype obj)
    ("init"
     ;; init はターンごとに来る。バッファに挿すと会話の途中に何度も
     ;; 見出しが混ざるので、ヘッダ行に出す。
     ;;
     ;; effort は毎ターン求め直さない。settings.json を 3 つまで読むので
     ;; ファイル I/O が要るのに対し、モデルが変わらない限り結果は変わら
     ;; ない。**モデルの更新より先に**判定すること (更新してしまうと
     ;; 「変わったかどうか」が分からなくなる)。
     ;; どこにも settings.json が無くて nil のままのときは毎ターン試すが、
     ;; `file-readable-p' が 3 回走るだけなので放っておく。
     (unless (and (my:claude-session-effort session)
                  (equal (my:claude-session-model session) (alist-get 'model obj)))
       (setf (my:claude-session-model session) (alist-get 'model obj)
             (my:claude-session-effort session) (my:claude--effort session)))
     (setf (my:claude-session-session-id session) (alist-get 'session_id obj)
           (my:claude-session-model session) (alist-get 'model obj)
           ;; ヘッダ行に出す。`claude --version' を別に呼ぶ必要は無い。
           (my:claude-session-claude-version session)
           (alist-get 'claude_code_version obj)
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

(defvar my:claude-lang-mode-alist
  '(("elisp"         . emacs-lisp-mode)
    ("emacs-lisp"    . emacs-lisp-mode)
    ("el"            . emacs-lisp-mode)
    ("lisp"          . lisp-mode)
    ("sh"            . sh-mode)
    ("shell"         . sh-mode)
    ("bash"          . sh-mode)
    ("zsh"           . sh-mode)
    ("console"       . sh-mode)
    ("shell-session" . sh-mode)
    ("js"            . js-mode)
    ("javascript"    . js-mode)
    ("json"          . js-json-mode)
    ("jsonc"         . js-json-mode)
    ("md"            . markdown-mode)
    ("markdown"      . markdown-mode)
    ("diff"          . diff-mode)
    ("patch"         . diff-mode)
    ("text"          . fundamental-mode)
    ("txt"           . fundamental-mode)
    ("ps1"           . powershell-mode)
    ("powershell"    . powershell-mode))
  "言語名 → メジャーモード。`markdown-get-lang-mode' より先に引く。

`markdown-get-lang-mode' は `<lang>-mode' と `<lang>-ts-mode' を
自動で試すので、素直な名前 (rust / python / go / yaml …) はここに
書かなくてよい。ここに置くのは名前が一致しないものと、
このリポジトリの都合で別のモードに寄せたいものだけ。")

(defun my:claude--lang-mode (lang)
  "LANG に対応する、実際にロードできるメジャーモードを返す。無ければ nil。

対応表は `markdown-mode' のものを流用する (`markdown-get-lang-mode')。
あちらは `<lang>-mode' / `<lang>-ts-mode' の推測と `fboundp' の確認まで
やってくれるので、自前で持つのは名前が一致しないものだけで済む。
markdown-mode は autoload 済みなので、必要になった時点で読み込まれる。"
  (when (and (stringp lang) (not (string-empty-p lang)))
    (let* ((key (downcase lang))
           (mode (or (cdr (assoc key my:claude-lang-mode-alist))
                     (and (require 'markdown-mode nil t)
                          (fboundp 'markdown-get-lang-mode)
                          (markdown-get-lang-mode key)))))
      (and mode (fboundp mode) mode))))

(defun my:claude--face-list (f)
  "テキストプロパティの face の値 F を face のリストにする。
無名 face (`(:foreground \"red\")' のような plist) は 1 つとして扱う。"
  (cond ((null f) nil)
        ((and (consp f) (keywordp (car f))) (list f))
        ((consp f) (copy-sequence f))
        (t (list f))))

(defun my:claude--code-face-p (pos)
  "POS がコードブロック (フェンス行を含む) の中か。

C-1 で構文の face を重ねると `font-lock-face' が **リストになる** ので、
`eq' で単一の face と比べる書き方は使えない。"
  (let ((f (get-text-property pos 'font-lock-face)))
    (cond ((memq f '(my:claude-code-face my:claude-code-fence-face)) t)
          ((and (consp f) (not (keywordp (car f))))
           (or (memq 'my:claude-code-face f)
               (memq 'my:claude-code-fence-face f)))
          (t nil))))

(defun my:claude--fontify-code (beg end lang)
  "BEG..END を LANG のメジャーモードとして着色する。

一時バッファで該当モードを立てて `font-lock-ensure' し、付いた `face' を
`font-lock-face' としてコピーする。org の
`org-src-font-lock-fontify-block' と同じやり方。このバッファでは
font-lock を有効にできない (`my:claude--fontify-markdown' 参照) ため、
描画済みの結果だけを貼り付ける形になる。

**背景色を消さないこと。** `my:claude-code-face' は背景しか持たないので、
構文の face と **並べてリストで** 載せる。上書きするとブロックの
地の色が消える。

一時バッファではモードフックを走らせない (`delay-mode-hooks')。
他人の設定 (flycheck / lsp / 自動整形) がここで動く道理が無いうえ、
`funcall' がエラーになると会話の表示ごと止まってしまう。"
  (when-let* ((mode (and my:claude-fontify-code (my:claude--lang-mode lang))))
    (let ((text (buffer-substring-no-properties beg end))
          (spans nil))
      (condition-case nil
          (with-temp-buffer
            (insert text)
            (delay-mode-hooks (funcall mode))
            (font-lock-ensure)
            (let ((pos (point-min)))
              (while (< pos (point-max))
                (let ((next (next-single-property-change pos 'face nil (point-max)))
                      (f (get-text-property pos 'face)))
                  (when f (push (list (1- pos) (1- next) f) spans))
                  (setq pos next)))))
        ;; モードが壊れていても会話の表示は続ける。単色のままになるだけ。
        (error (setq spans nil)))
      (dolist (sp spans)
        (put-text-property (+ beg (nth 0 sp)) (+ beg (nth 1 sp))
                           'font-lock-face
                           (append (my:claude--face-list (nth 2 sp))
                                   (list 'my:claude-code-face)))))))

;;; markdown の表を罫線に組み直す

(defun my:claude--table-row-p ()
  "いまの行が `| a | b |' の形なら非 nil。"
  (string-match-p "\\`[ \t]*|.*|[ \t]*\\'"
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))

(defun my:claude--table-separator-p (line)
  "LINE が `|---|:---:|' のような区切り行なら非 nil。"
  (string-match-p "\\`[ \t]*|\\([ \t]*:?-+:?[ \t]*|\\)+[ \t]*\\'" line))

(defun my:claude--split-row (line)
  "`| a | b |' の LINE をセルのリストにする。
`\\|' でエスケープされた `|' はセルの区切りにしない。"
  (let* ((body (string-remove-suffix
                "|" (string-remove-prefix "|" (string-trim line))))
         (n (length body))
         (cells nil) (cur nil) (i 0))
    (while (< i n)
      (let ((c (aref body i)))
        (cond ((and (eq c ?\\) (< (1+ i) n) (eq (aref body (1+ i)) ?|))
               (push ?| cur) (setq i (+ i 2)))
              ((eq c ?|) (push (nreverse cur) cells) (setq cur nil) (setq i (1+ i)))
              (t (push c cur) (setq i (1+ i))))))
    (push (nreverse cur) cells)
    (mapcar (lambda (cs) (string-trim (apply #'string cs))) (nreverse cells))))

(defun my:claude--table-align (cells)
  "区切り行のセル CELLS から、列ごとの寄せ方 (left/right/center) を返す。"
  (mapcar (lambda (c)
            (let ((l (string-prefix-p ":" c))
                  (r (string-suffix-p ":" c)))
              (cond ((and l r) 'center) (r 'right) (t 'left))))
          cells))

(defun my:claude--pad (s width align)
  "S を WIDTH 桁に ALIGN で詰める。桁は `string-width' で数える。"
  (let ((d (max 0 (- width (string-width s)))))
    (pcase align
      ('right  (concat (make-string d ?\s) s))
      ('center (let ((l (/ d 2)))
                 (concat (make-string l ?\s) s (make-string (- d l) ?\s))))
      (_       (concat s (make-string d ?\s))))))

(defun my:claude--table-string (rows aligns header indent)
  "ROWS を罫線の表にした文字列を返す。

ALIGNS は列ごとの寄せ方、HEADER は見出し行の数、INDENT は行頭に付ける空白。
桁は `string-width' で数えるので、East Asian Ambiguous は
`site-lisp/eaw.el' が与える幅 2 になる。**元の桁は使わない**
 (claude は幅 1 で組んでいることがある)。"
  (let* ((ncol (apply #'max 1 (mapcar #'length rows)))
         (rows (mapcar (lambda (r)
                         (append r (make-list (- ncol (length r)) "")))
                       rows))
         (aligns (append aligns
                         (make-list (max 0 (- ncol (length aligns))) 'left)))
         ;; 【重要】罫線素片は 1 文字で 2 桁ある。
         ;; `─' は JIS X 0208 の罫線素片なので `site-lisp/eaw.el' が
         ;; 幅 2 を与え、HackGen も全角 (16px) で描く。つまり
         ;; `(make-string (+ w 2) ?─)' は **w+2 桁ではなく 2(w+2) 桁**
         ;; になり、罫線の行だけが倍の長さになる。実際にそうなっていた。
         ;;
         ;;   幅= 44 |┌─────┬────────┬─────┐|
         ;;   幅= 26 |│ 列  │ 説明   │  値 │|
         ;;
         ;; セルの中身の詰め物は半角空白 (1 桁) なので列幅はどんな値でも
         ;; 組めるが、罫線側は 2 桁単位でしか刻めない。そこで
         ;; **列幅を「w+2 が罫線 1 文字の桁数の倍数」になるまで広げる**。
         ;; eaw を外した Emacs では `─' が幅 1 になるので、その場合は
         ;; 何も広げない (`rw' を実測しているのはそのため)。
         (rw (max 1 (char-width ?─)))
         (widths (mapcar (lambda (i)
                           (let ((w (apply #'max 1
                                           (mapcar (lambda (r)
                                                     (string-width (nth i r)))
                                                   rows))))
                             (+ w (mod (- rw (mod (+ w 2) rw)) rw))))
                         (number-sequence 0 (1- ncol))))
         (rule (lambda (l m r)
                 (concat indent l
                         (mapconcat (lambda (w) (make-string (/ (+ w 2) rw) ?─))
                                    widths m)
                         r "\n")))
         (row (lambda (r)
                (concat indent "│"
                        (mapconcat
                         (lambda (i)
                           (concat " " (my:claude--pad (nth i r) (nth i widths)
                                                       (nth i aligns))
                                   " "))
                         (number-sequence 0 (1- ncol)) "│")
                        "│\n"))))
    (concat (funcall rule "┌" "┬" "┐")
            (mapconcat row (seq-take rows header) "")
            (if (> header 0) (funcall rule "├" "┼" "┤") "")
            (mapconcat row (seq-drop rows header) "")
            (funcall rule "└" "┴" "┘"))))

(defun my:claude--render-table-at-point (end)
  "point の行から続くパイプ表を罫線の表に置き換える。END を越えない。"
  (let* ((start (line-beginning-position))
         (indent (progn (goto-char start) (looking-at "[ \t]*") (match-string 0)))
         (lines nil))
    (while (and (< (point) end) (my:claude--table-row-p))
      (push (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))
            lines)
      (forward-line 1))
    (setq lines (nreverse lines))
    (let* ((sep (seq-position lines nil
                              (lambda (l _) (my:claude--table-separator-p l))))
           (rows (mapcar #'my:claude--split-row lines)))
      (when sep
        (let* ((aligns (my:claude--table-align (nth sep rows)))
               (body (append (seq-take rows sep) (seq-drop rows (1+ sep))))
               (text (my:claude--table-string body aligns sep indent))
               (finish (point)))
          (delete-region start finish)
          (goto-char start)
          (insert text))))))

(defun my:claude--render-tables (beg end)
  "BEG..END にある markdown のパイプ表を罫線の表に組み直す。

【重要】**元の桁は信用しない。** claude は East Asian Ambiguous を
幅 1 として桁を組んでいることがあるが、この Emacs では
`site-lisp/eaw.el' が幅 2 にする。セルの中身だけを取り出して
`string-width' で組み直せば、論理幅と実描画幅が一致する
 (罫線素片は JIS X 0208 にあり `my-appearance.el' が HackGen に
割り当てるので全角 = 2 桁で描かれる)。

区切り行 (`|---|---|') が続く場合だけ表とみなす。これが無いと
`a | b' のような何気ない行まで拾ってしまう。コードブロックの中は
触らない。"
  (when my:claude-render-tables
    (goto-char beg)
    (forward-line 0)
    (while (< (point) end)
      (if (and (not (my:claude--code-face-p (line-beginning-position)))
               (my:claude--table-row-p)
               (save-excursion
                 (forward-line 1)
                 (and (< (point) end)
                      (my:claude--table-separator-p
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))))
          (my:claude--render-table-at-point end)
        (forward-line 1)))))

(defun my:claude--fontify-markdown (session beg)
  "SESSION の会話バッファの BEG から末尾までを markdown として整える。

font-lock は使わない。このバッファは `special-mode' 派生で、挿入時に
`font-lock-face' を直に載せているため、font-lock を有効にすると
そちらに上書きされて競合する。ブロックが確定した時点で一度だけ塗る。

やることは 3 つ。**この順でなければならない。**

  1. ``` のブロックを塗る (言語指定があればその言語として着色する)
  2. `|' の表を罫線に組み直す。1 の結果を見てコードブロックの中を避ける
  3. 見出しと行中のコード

呼ばれる場所は 2 か所ある (逐次表示の `content_block_stop' と、
delta が来ないスラッシュコマンドの `assistant')。**片方だけだと
`/context' の見出しが素のままになる。**"
  (let ((buf (my:claude-session-buffer session)))
    (when (and (buffer-live-p buf) (markerp beg) (marker-position beg))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              ;; 表の組み直しで長さが変わるのでマーカーで持つ。
              (end (copy-marker (point-max) t)))
          (save-excursion
            ;; [1] ``` で囲まれたブロック
            (goto-char beg)
            (while (re-search-forward "^[ \t]*```[ \t]*\\([^ \t\n]*\\).*$" end t)
              (let ((fence1-beg (match-beginning 0))
                    (fence1-end (match-end 0))
                    (lang (match-string 1))
                    body-end)
                (if (re-search-forward "^[ \t]*```[ \t]*$" end t)
                    (setq body-end (match-beginning 0))
                  ;; 閉じていない (中断されたなど) ときは末尾まで
                  (setq body-end (marker-position end))
                  (goto-char end))
                (put-text-property fence1-beg fence1-end
                                   'font-lock-face 'my:claude-code-fence-face)
                (when (< fence1-end body-end)
                  (put-text-property fence1-end body-end
                                     'font-lock-face 'my:claude-code-face)
                  (when (<= (count-lines fence1-end body-end)
                            my:claude-fontify-code-max-lines)
                    (my:claude--fontify-code fence1-end body-end lang)))
                (when (< body-end (marker-position end))
                  (put-text-property body-end (min (marker-position end)
                                                   (line-end-position))
                                     'font-lock-face 'my:claude-code-fence-face))))
            ;; [2] パイプ表を罫線に
            (my:claude--render-tables beg end)
            ;; [3] 見出しと行中のコード。コードブロックの中は塗り直さない。
            (goto-char beg)
            (while (re-search-forward "^[ \t]*#\\{1,6\\} .*$" end t)
              (unless (my:claude--code-face-p (match-beginning 0))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'font-lock-face 'my:claude-heading-face)))
            (goto-char beg)
            (while (re-search-forward "`[^`\n]+`" end t)
              (unless (or (my:claude--code-face-p (match-beginning 0))
                          (eq (get-text-property (match-beginning 0) 'font-lock-face)
                              'my:claude-heading-face))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'font-lock-face 'my:claude-inline-code-face))))
          (set-marker end nil))))))

(defun my:claude--mark-text-start (session)
  "いまの末尾に本文の開始位置を記録する。"
  (let ((buf (my:claude-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setf (my:claude-session-text-start session)
              (copy-marker (point-max) nil))))))

(defun my:claude--end-paragraph (session)
  "会話バッファの末尾を「空行 1 つ」に整える。
delta で流し込んだ本文は末尾の改行がまちまちなので、ここで揃える。

末尾の削り直しでも point は取り残される (削除でマーカーが手前に
引かれ、そこへ挿入しても前に置かれたままになる) ので、
`my:claude--at-end\' を通す。"
  (my:claude--at-end session
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (insert "\n\n")))

(defun my:claude--close-stream-block (session)
  "開いたままのブロックがあれば行を閉じる。中断されたときに使う。"
  (when (my:claude-session-stream-block session)
    (my:claude--insert session "\n")
    (setf (my:claude-session-stream-block session) nil)))

(defun my:claude--parent-id (obj)
  "OBJ が サブエージェント由来なら親の tool_use_id、そうでなければ nil。
JSON の null は `:null' で来るので `alist-get' の結果をそのまま使えない。"
  (let ((p (alist-get 'parent_tool_use_id obj)))
    (and (stringp p) p)))

(defun my:claude--handle-assistant (obj session)
  (let ((content (alist-get 'content (alist-get 'message obj)))
        (sub (my:claude--parent-id obj)))
    ;; コンテキスト使用量。statusline-command.sh が transcript から
    ;; 集計しているのと同じ 3 つの和。サブエージェントの usage は
    ;; 別の文脈なので混ぜない。
    (unless sub
      (when-let* ((usage (alist-get 'usage (alist-get 'message obj))))
        (setf (my:claude-session-context-tokens session)
              (+ (or (alist-get 'input_tokens usage) 0)
                 (or (alist-get 'cache_read_input_tokens usage) 0)
                 (or (alist-get 'cache_creation_input_tokens usage) 0)))
        (my:claude--update-header session)))
    (seq-doseq (block content)
      (pcase (alist-get 'type block)
        ("text"
         (cond
          ;; サブエージェントの本文。**delta では来ない** (実測で
          ;; stream_event に parent_tool_use_id が付くことは無かった) ので、
          ;; streamed-text を見ずに必ず出す。見ると本体のブロックが
          ;; 開いている間は捨てられてしまう。字下げして本体と区別する。
          (sub
           (my:claude--insert-block session
                                    (string-trim-right (alist-get 'text block))
                                    'my:claude-subagent-face))
          ;; 【重要】`my:claude-stream' ではなく「このブロックを実際に
          ;; delta で出したか」で判断する。スラッシュコマンドは
          ;; assistant で本文を返すが stream_event を伴わない
          ;; (num_turns=0 で API を通らないため)。フラグを見ずに
          ;; my:claude-stream だけで飛ばすと /mcp や /context が
          ;; 何も表示されない。実際にそうなっていた。
          ((not (my:claude-session-streamed-text session))
           (my:claude--mark-text-start session)
           (my:claude--insert session
                              (concat (string-trim-right (alist-get 'text block)) "\n\n")
                              'my:claude-assistant-face)
           (my:claude--fontify-markdown session
                                        (my:claude-session-text-start session)))))
        ("tool_use"
         (let* ((name (alist-get 'name block))
                (id (alist-get 'id block))
                (summary (my:claude--tool-summary block)))
           ;; 折りたたんだ結果に出す 1 行要約でも使うので、名前と一緒に
           ;; 引数の要約も覚えておく。tool_result には入力が入っていない。
           (puthash id (cons name summary) (my:claude-session-tool-names session))
           (my:claude--insert
            session
            (format "%s▶ %s %s\n" (if sub "  " "") name summary)
            (if sub 'my:claude-subagent-face 'my:claude-tool-face))
           ;; Edit / Write は入力そのものが差分なので、その場で見せる。
           (my:claude--show-edit session name (alist-get 'input block))))
        (_ nil)))))

(defun my:claude--insert-diff (session removed added)
  "REMOVED / ADDED を diff 風に見せる。

外部の diff は呼ばない。Edit の入力は old_string と new_string が
そのまま来るので、行単位で `-' と `+' を付けて並べれば足りる。
Windows に diff が入っている保証も無い。

`my:claude--at-end\' を通すこと。直接書いていたため、**差分が 1 回出ると
point が末尾から外れて自動スクロールが止まっていた**。"
  (my:claude--at-end session
    (dolist (pair (list (cons removed 'my:claude-diff-removed-face)
                        (cons added 'my:claude-diff-added-face)))
      (when (and (stringp (car pair))
                 (not (string-empty-p (car pair))))
        (let ((mark (if (eq (cdr pair) 'my:claude-diff-removed-face) "-" "+")))
          (dolist (l (split-string (string-trim-right (car pair)) "\n"))
            (insert (propertize (concat "    " mark l "\n")
                                'font-lock-face (cdr pair)))))))))

(defcustom my:claude-diff-max-lines 30
  "Edit / Write の差分をそのまま見せる行数の上限。
これを超えたら行数だけ知らせる。"
  :type 'integer)

(defun my:claude--show-edit (session name input)
  "Edit / Write の入力を差分として見せる。"
  (let* ((old (alist-get 'old_string input))
         (new (or (alist-get 'new_string input) (alist-get 'content input)))
         (lines (+ (if (stringp old) (length (split-string old "\n")) 0)
                   (if (stringp new) (length (split-string new "\n")) 0))))
    (when (and (member name '("Edit" "Write" "NotebookEdit"))
               (or (stringp old) (stringp new)))
      (if (> lines my:claude-diff-max-lines)
          ;; 【重要】TAB は案内しない。この関数は `my:claude-full' を
          ;; 設定しないので、押しても「ここには折りたたまれた出力が無い」
          ;; になるだけだった。全体が見たければ git diff を使う。
          (my:claude--insert session
                             (format "    (差分 %d 行。git diff で確認)\n" lines)
                             'my:claude-meta-face)
        (my:claude--insert-diff session old new)))))

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
  (let ((content (alist-get 'content (alist-get 'message obj)))
        (sub (my:claude--parent-id obj)))
    (seq-doseq (block content)
      (when (equal (alist-get 'type block) "tool_result")
        (let* ((id (alist-get 'tool_use_id block))
               (entry (gethash id (my:claude-session-tool-names session)))
               (name (or (car-safe entry) "?"))
               (summary (or (cdr-safe entry) ""))
               (label (if (string-empty-p summary) name
                        (format "%s(%s)" name summary)))
               (err (eq t (alist-get 'is_error block)))
               (text (my:claude--content-string (alist-get 'content block))))
          (if (string-empty-p (string-trim text))
              ;; 出力が無いものを畳んでも意味が無い。
              (my:claude--insert session (format "  ● %s … 出力なし\n" label)
                                 'my:claude-meta-face)
            (my:claude--fold session text
                             (cond (err 'my:claude-error-face)
                                   (sub 'my:claude-subagent-face)
                                   (t 'my:claude-tool-result-face))
                             label)))))))

(defun my:claude--handle-result (obj session)
  ;; 中断されると content_block_stop が来ないことがある。
  (my:claude--close-stream-block session)
  (setf (my:claude-session-last-result session) obj
        (my:claude-session-busy session) nil)
  ;; コンテキストの上限。モデルごとに入っている (1M 版なら 1000000)。
  ;; ここでしか来ないので、来たときに覚えておく。
  (dolist (e (alist-get 'modelUsage obj))
    (let ((cw (alist-get 'contextWindow (cdr e))))
      (when (numberp cw)
        (setf (my:claude-session-context-window session) cw))))
  (my:claude--update-header session)
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

(defun my:claude--suggestion-label (suggestions)
  "SUGGESTIONS を人間向けの短い説明にする。"
  (mapconcat
   (lambda (s)
     (pcase (alist-get 'type s)
       ("setMode" (format "%s に切り替える" (alist-get 'mode s)))
       ("addRules" "ルールを追加する")
       (other (format "%s" other))))
   (append suggestions nil) " / "))

(defun my:claude--ask-permission (_obj session rid req)
  "ツール使用の可否を尋ねて control_response を返す。"
  (let* ((name (or (alist-get 'tool_name req) "?"))
         (desc (or (alist-get 'description req) ""))
         (input (alist-get 'input req))
         (sugg (alist-get 'permission_suggestions req)))
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
                       `((?y "今回だけ許可")
                         (?n "拒否")
                         (?r "理由を書いて拒否")
                         (?a ,(if (and sugg (> (length sugg) 0))
                                  (format "以後聞かない (%s)"
                                          (my:claude--suggestion-label sugg))
                                "以後このツールは聞かない"))
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
            ;; 要求には permission_suggestions が付いてくる (例: acceptEdits に
            ;; 切り替える)。これを updatedPermissions に載せて返すと
            ;; **claude 側が以後聞いてこなくなる** (実測で 2 回目の Write が
            ;; 聞かれなくなった)。付いていないときは Emacs 側で覚えるだけの
            ;; 従来動作に落とす。
            (?a (if (and sugg (> (length sugg) 0))
                    (progn
                      (my:claude--insert
                       session
                       (format "  (以後許可: %s — %s)\n"
                               name (my:claude--suggestion-label sugg))
                       'my:claude-meta-face)
                      (my:claude--respond-permission
                       session rid
                       `((behavior . "allow")
                         (updatedInput . ,input)
                         (updatedPermissions . ,sugg))))
                  (push name (my:claude-session-approved session))
                  (my:claude--respond-allow session rid input))
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
(defun my:claude--ensure-session (&optional arg)
  "セッションを返す。無ければ環境を選んで起動する。ウィンドウは触らない。

セッションは Emacs 全体で 1 つだけ持つ。アカウントの切り替えは
CLAUDE_CONFIG_DIR をプロセス起動時に渡すことでしか行えないため、
複数あるとどちらに送っているのか分からなくなるので増やさない。

ARG が非 nil なら、生きているセッションがあっても畳んで環境と
作業ディレクトリを選び直す。"
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
    ;; **ここでは `my:claude--guess-directory' を使う。**
    ;; `my:claude--project-directory' だと y/n を聞いてしまい、
    ;; 使い回すだけの場面で確認が出る。
    (let ((here (my:claude--guess-directory)))
      (when (and here (not (equal here (my:claude-session-directory session))))
        (message "claude のセッションは %s のまま (C-u C-c a a で立て直す)"
                 (abbreviate-file-name
                  (directory-file-name (my:claude-session-directory session))))))
    session))

;;;###autoload
(defun my:claude (&optional arg)
  "claude セッションを開き、`my:claude-layout' の形に画面を整える。

上半分は編集中のバッファ、下半分が会話と入力。**カーソルは入力
バッファに入る。** 開いてすぐ書き始められるのが `C-c a a' の役目。

ARG (`C-u') を付けると、生きているセッションがあっても畳んで、
環境と作業ディレクトリを選び直す。Pro の残量が尽きたときに
その場で Max へ逃がすのがこの操作。"
  (interactive "P")
  (let ((session (my:claude--ensure-session arg)))
    (my:claude-layout)
    session))

;;; ウィンドウのレイアウト

(defun my:claude--buffer-p (buf)
  "BUF が claude の会話 / 入力バッファなら非 nil。

**名前では見ない。** バッファ名にはプロジェクト名が入る
 (`my:claude--buffer-name') ので、メジャーモードで判定する。"
  (and (bufferp buf)
       (memq (buffer-local-value 'major-mode buf)
             '(my:claude-mode my:claude-input-mode))
       t))

(defun my:claude--keep-buffer ()
  "レイアウトの上半分に残すバッファ。claude 系でないものを選ぶ。

いま見ているバッファが claude 系でなければそれ。claude 系なら、
表示中の他のウィンドウ、それも無ければ直近のバッファ。"
  (or (and (not (my:claude--buffer-p (current-buffer))) (current-buffer))
      (seq-some (lambda (w)
                  (let ((b (window-buffer w)))
                    (and (not (my:claude--buffer-p b)) b)))
                (window-list nil 'no-mini))
      (seq-find (lambda (b)
                  (and (not (my:claude--buffer-p b))
                       (not (string-prefix-p " " (buffer-name b)))))
                (buffer-list))
      (get-buffer-create "*scratch*")))

;;;###autoload
(defun my:claude-layout ()
  "画面を上下 2 分割し、下半分に会話バッファと入力バッファを出す。

  上半分  編集中のバッファ
  下半分  上が会話バッファ (出力)、下が入力バッファ
          それぞれ *claude(PROJ)* と *claude-input(PROJ)*

下半分の高さはフレームの `my:claude-window-height-ratio' 倍、
入力バッファは `my:claude-input-window-height' 行。最後にカーソルを
入力バッファへ置く。

いつでもこの形に戻せるように `C-c a l' に割り当ててある。
`my:claude-toggle-maximize' の復帰先でもある
 (トグル前の `window-configuration' は退避しない)。"
  (interactive)
  (let* ((session (my:claude--session-for-buffer))
         ;; バッファ名に入れるプロジェクト名の元。セッションがまだ無いときは
         ;; **確認を出さない** `my:claude--guess-directory' で推測する。
         ;; `my:claude--project-directory' を使うと、画面を整えるだけの
         ;; `C-c a l' でも y-or-n-p が出てしまう。
         (dir (if session (my:claude-session-directory session)
                (my:claude--guess-directory)))
         ;; **conv / input を作るより先に決める。** あとに回すと、まだ
         ;; メジャーモードが立っていない新品のバッファを
         ;; `my:claude--buffer-p' が claude 系と見なせず、上半分に
         ;; 残すバッファとして選んでしまう。
         (keep (my:claude--keep-buffer))
         (conv (if session (my:claude-session-buffer session)
                 (get-buffer-create (my:claude--buffer-name "claude" dir))))
         (input (get-buffer-create (my:claude--buffer-name "claude-input" dir)))
         (total (window-total-height (frame-root-window)))
         (bottom (max 8 (round (* total my:claude-window-height-ratio))))
         (ih (max 3 my:claude-input-window-height)))
    ;; セッションより先に `C-c a l' を押したときは、ここで作った会話
    ;; バッファにまだモードが立っていない。次に `my:claude--buffer-p' が
    ;; 呼ばれたときのために立てておく (`my:claude-mode' は special-mode
    ;; 派生で、空バッファに立てても読み取り専用になるだけ)。
    (with-current-buffer conv
      (unless (derived-mode-p 'my:claude-mode) (my:claude-mode)))
    (with-current-buffer input
      (unless (derived-mode-p 'my:claude-input-mode) (my:claude-input-mode))
      (when session (setq my:claude--session session)))
    (if (< total (+ bottom ih 4))
        ;; フレームが低すぎて 3 分割できない。壊すより諦める。
        (pop-to-buffer input)
      (delete-other-windows)
      (switch-to-buffer keep nil t)
      (let ((cw (split-window-below (- total bottom))))
        (set-window-buffer cw conv)
        (let ((iw (with-selected-window cw (split-window-below (- bottom ih)))))
          (set-window-buffer iw input)
          (select-window iw)
          (goto-char (point-max)))))))

;;;###autoload
(defun my:claude-toggle-maximize ()
  "claude のウィンドウを最大化する。もう一度押すと元に戻す。

**戻り先はトグル前の状態ではなく `my:claude-layout' の正規レイアウト。**
`window-configuration' を退避しないので、どこから何度押しても同じ形に
落ち着く。出力を読み込みたいとき、長い入力を書きたいときに使う。"
  (interactive)
  (cond
   ((one-window-p 'no-mini) (my:claude-layout))
   ((my:claude--buffer-p (current-buffer)) (delete-other-windows))
   (t (user-error "claude のバッファではない"))))

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
      (my:claude-layout)
      session)))

(defun my:claude--projects-directory (env)
  "環境 ENV がセッションを貯めているディレクトリ。"
  (expand-file-name "projects"
                    (or (my:claude--config-dir env) (expand-file-name "~/.claude"))))

(defun my:claude--session-directory (env dir)
  "環境 ENV で DIR のセッションが入っているディレクトリ。

claude はワークスペースのパスの **英数字以外をすべて `-' に置き換えた**
名前を使う。`C:/Users/masao/.emacs.d' なら `C--Users-masao--emacs-d'。
手元の 10 個で突き合わせて確かめた (合わなかった 1 つはドライブレターの
大小違いだけで、Windows のファイルシステムでは同じ場所を指す)。"
  (let ((name (replace-regexp-in-string
               "[^A-Za-z0-9]" "-"
               (directory-file-name (expand-file-name dir)))))
    (expand-file-name name (my:claude--projects-directory env))))

(defun my:claude--session-preview (file)
  "セッションの記録 FILE から、最初のプロンプトを 1 行で返す。

先頭から順に読むが、`<local-command-…>' のような差し込みは飛ばす。
1 MB を超えるファイルもあるので、見つかるか 400 行で打ち切る。"
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (found nil) (n 0))
      (insert-file-contents file nil 0 200000)
      (goto-char (point-min))
      (while (and (not found) (< n 400) (not (eobp)))
        (setq n (1+ n))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (o (and (string-prefix-p "{" line)
                       (ignore-errors (json-parse-string line :object-type 'alist)))))
          (when (and o (equal (alist-get 'type o) "user")
                     (not (eq t (alist-get 'isSidechain o))))
            ;; content は文字列のこともブロックの配列のこともある。
            ;; **Emacs から送ったものは必ず配列**なので、文字列だけを
            ;; 見ていると自分で作ったセッションが全部「プロンプトなし」に
            ;; なる。実際にそうなっていた。
            (let ((c (my:claude--content-string
                      (alist-get 'content (alist-get 'message o)))))
              (when (and (stringp c)
                         (not (string-empty-p (string-trim c)))
                         (not (string-prefix-p "<local-command" c))
                         (not (string-prefix-p "<command-name" c)))
                (setq found c)))))
        (forward-line 1))
      (if found
          (truncate-string-to-width
           (replace-regexp-in-string "[ \t\n]+" " " (string-trim found))
           70 nil nil "…")
        "(プロンプトなし)"))))

(defun my:claude--past-sessions (env dir)
  "環境 ENV / ディレクトリ DIR の過去セッションを新しい順に返す。
要素は (表示用文字列 . session-id)。"
  (let ((d (my:claude--session-directory env dir)))
    (when (file-directory-p d)
      (let ((files (sort (directory-files d t "\\.jsonl\\'")
                         (lambda (a b)
                           (time-less-p (file-attribute-modification-time
                                         (file-attributes b))
                                        (file-attribute-modification-time
                                         (file-attributes a)))))))
        (mapcar
         (lambda (f)
           (cons (format "%s  %s"
                         (format-time-string
                          "%m-%d %H:%M"
                          (file-attribute-modification-time (file-attributes f)))
                         (my:claude--session-preview f))
                 (file-name-base f)))
         (seq-take files 30))))))

;;;###autoload
(defun my:claude-resume ()
  "過去のセッションを一覧から選んで再開する。

`--continue' は「そのディレクトリの直近の 1 つ」しか選べない。
こちらは記録ファイル (`<CLAUDE_CONFIG_DIR>/projects/…/*.jsonl') を
新しい順に並べて選ばせる。アカウントが違うと保存先も別なので、
いまの環境のものだけが出る。"
  (interactive)
  (let* ((old (my:claude--session-for-buffer))
         (env (or (and old (my:claude-session-name old))
                  (my:claude--read-environment)))
         (dir (if old (my:claude-session-directory old)
                (my:claude--project-directory)))
         (rows (my:claude--past-sessions env dir)))
    (unless rows
      (user-error "%s に %s のセッションの記録が無い"
                  env (abbreviate-file-name (directory-file-name dir))))
    (let* ((choice (completing-read "再開するセッション: "
                                    (mapcar #'car rows) nil t))
           (id (cdr (assoc choice rows))))
      (my:claude--restart id env))))

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
        (my:claude--start dir env t)
        (my:claude-layout)))))

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
      (my:claude-layout)
      session)))

(defun my:claude-send-string (text &optional session)
  "TEXT を claude に送る。"
  ;; リージョン送信など、他所から呼ばれることがある。ここで
  ;; `my:claude' を呼ぶとウィンドウを組み替えてしまうので使わない。
  (let ((session (or session (my:claude--session-for-buffer)
                     (my:claude--ensure-session))))
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
  "送信するテキストを書くバッファを開く。画面は `my:claude-layout' にする。"
  (interactive)
  (my:claude--ensure-session)
  (my:claude-layout))

(defun my:claude--conversation-buffer ()
  "いま使う会話バッファ。無ければ nil。"
  (let ((session (my:claude--session-for-buffer)))
    (if session
        (my:claude-session-buffer session)
      ;; セッションが無いときは名前で引けない (プロジェクト名が入る)。
      ;; `my:claude--buffer-p' と同じくメジャーモードで探す。
      (seq-find (lambda (b)
                  (eq (buffer-local-value 'major-mode b) 'my:claude-mode))
                (buffer-list)))))

(defun my:claude-input-quit ()
  "入力バッファを閉じ、空いた領域を会話バッファに渡す。

`quit-window\' だと**ウィンドウはそのまま残り**、下半分が別のバッファで
埋まるだけになる。ここではウィンドウごと畳むので、下半分は *claude* だけに
なる。`C-c C-i\' (`my:claude-input\') を押せば `my:claude-layout\' が
元の 3 分割に組み直す。"
  (interactive)
  (let ((buf (current-buffer))
        (conv (my:claude--conversation-buffer)))
    (if (one-window-p 'no-mini)
        ;; 畳む先が無い。ウィンドウを消すと何も残らないので中身を差し替える。
        (if conv (switch-to-buffer conv) (bury-buffer))
      (delete-window)
      (bury-buffer buf)
      ;; 空いた領域を受け取ったのは会話バッファのはず。そこへ移る。
      (when-let* ((cw (and conv (get-buffer-window conv))))
        (select-window cw)))))

(defun my:claude-input-send ()
  "入力バッファの内容を送って空にし、入力ウィンドウを畳む。

送ったあとに書くことはもう無いので、`C-c C-k\' (`my:claude-input-quit\')
と同じ形にして下半分を *claude* だけにする。応答は読む一方なので、
入力用の数行を残しておく理由が無い。

書き足すときは会話バッファで `i\' (`my:claude-input\') を押せば
`my:claude-layout\' が元の 3 分割に組み直す。"
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
      (display-buffer buf)
      ;; 送信のたびに会話バッファを末尾へ戻し、**追従を張り直す**。
      ;; 読み返している間に届いたぶんで point が末尾から外れていると、
      ;; `my:claude--at-end\' の判定が偽になって以後の応答が流れても
      ;; 追いかけない。自分で送った直後だけは必ず末尾に付ける。
      ;; ウィンドウごとに `window-point\' を持つので両方動かすこと。
      ;; 【重要】`point-max\' は会話バッファの中で評価する。外に出すと
      ;; 直前に `erase-buffer\' した入力バッファの 1 を渡すことになる。
      (with-current-buffer buf
        (goto-char (point-max))
        (dolist (w (get-buffer-window-list buf nil t))
          (set-window-point w (point-max)))))
    ;; 畳むのはいちばん最後。`my:claude-input-quit' が消すのは入力
    ;; ウィンドウなので、上の `display-buffer' で会話バッファを出して
    ;; からでないと、空いた領域を受け取る窓が無い。
    (my:claude-input-quit)))

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
    (define-key map (kbd "C-c C-z") #'my:claude-toggle-maximize)
    (define-key map (kbd "z") #'my:claude-toggle-maximize)
    (define-key map (kbd "q") #'quit-window)
    map)
  "`my:claude-mode' のキーマップ。")

(define-derived-mode my:claude-mode special-mode "Claude"
  "claude との会話を表示するモード。

i / C-c C-i で入力バッファを開く (`C-c a i' と同じ)。送信すると入力
ウィンドウは畳まれてこのバッファだけになるので、次を書くときはここから
`i' で戻る。
TAB で折りたたんだツール出力の全体を別バッファに出す。
z / C-c C-z でこのウィンドウを最大化 (もう一度で元のレイアウト)。"
  (setq-local truncate-lines nil))

(defvar my:claude-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my:claude-input-send)
    (define-key map (kbd "C-c C-k") #'my:claude-input-quit)
    (define-key map (kbd "C-c C-z") #'my:claude-toggle-maximize)
    (define-key map (kbd "M-p") #'my:claude-input-previous)
    (define-key map (kbd "M-n") #'my:claude-input-next)
    map)
  "`my:claude-input-mode' のキーマップ。

`markdown-mode-map' が親になるが、ここに書いたものが優先される。
とくに `C-c C-c' は markdown 側では prefix (`markdown-mode-command-map')
なので、この束縛が無いと送信できなくなる。")

(define-derived-mode my:claude-input-mode markdown-mode "Claude-Input"
  "claude に送るテキストを書くモード。

markdown として書くので `markdown-mode' から派生させる。会話バッファ
 (`my:claude-mode') と違って **font-lock をそのまま使える** ので、
C-1 のようにテキストプロパティを貼る仕掛けは要らない。コードブロックの
言語判別は `markdown-fontify-code-blocks-natively' に任せる。

【重要】`markdown-mode-hook' は走らせない。`my-text.el' の
`my:setup-markdown-mode' は「.md ファイルを編集する」前提の設定
 (`electric-indent-local-mode' を切るなど) で、送信用の一時バッファに
持ち込む理由が無い。将来 `my-text.el' を触ったときにこちらの挙動が
黙って変わるのも避けたい。`delay-mode-hooks' で溜められたフックは
`run-mode-hooks' が `run-hooks' で回すので、**バッファローカルに nil に
すれば走らない** (ローカル値に t が無ければグローバル値も見ない)。

【重要】`completion-at-point-functions' の `my:claude--capf' を落とさない
こと。落とすと行頭の `/' が `cape-file' に食われて C: 直下の
ディレクトリ一覧が出る。"
  (setq-local markdown-mode-hook nil)
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; 案内は `my:claude--header-segment' を通す。いまの文言に `%' は無いが、
  ;; 素の文字列を `header-line-format' に渡すと `%' と直後の 1 文字が
  ;; まとめて消えるので、文言を書き換えたときに黙って壊れないようにしておく。
  (setq-local header-line-format
              (my:claude--header-segment
               ;; `C-c a k' だけプレフィックスが違うのは、中断がこのバッファ
               ;; ではなく**セッション**への操作だから。C-c C-* はこの
               ;; バッファの操作 (送信・閉じる・最大化)、C-c a * はセッション
               ;; の操作 (中断・終了・環境切り替え) という区別が付いている。
               ;;
               ;; `C-c C-k' を中断にはしない。org-capture / git-commit /
               ;; message-mode と同じく、下書きバッファの C-c C-k は「書き
               ;; かけをやめる」が慣習で、変えると入力バッファを畳む手段が
               ;; 無くなる。会話バッファ側の C-c C-k が中断なのは、あちらが
               ;; compilation の kill-compilation と同じ性格だから。
               "C-c C-c 送信 / C-c C-k 閉じる / C-c a k 中断 / C-c C-z 最大化 / 行頭 / は TAB 補完 / M-p 履歴"
               'my:claude-input-header-face))
  ;; cape-file が深さ 90 にいる。念のため明示的に先頭へ置く。
  (add-hook 'completion-at-point-functions #'my:claude--capf -100 t))

(defun my:claude-input--disable-line-numbers ()
  "入力バッファでは行番号を出さない。

数行しか書かないバッファなので、桁を食うだけで得が無い。

【重要】モード本体ではなくフックで切ること。`markdown-mode' は
`text-mode' 派生なので `my-editor.el' が
`text-mode-hook' に載せた `display-line-numbers-mode' が走る。
`delay-mode-hooks' で溜められた親のフックは `run-mode-hooks' が
このモード自身のフックより **先に** 回すので、モード本体で切っても
そのあと有効にされてしまう。

`markdown-mode-hook' のようにバッファローカルに nil にする手も
あるが、`text-mode-hook' は他の用途にも使う場所なので潰さない。"
  (display-line-numbers-mode -1))

(add-hook 'my:claude-input-mode-hook #'my:claude-input--disable-line-numbers)

;;; --------------------------------------------------
;;; グローバルキーバインド
;;; --------------------------------------------------

;; C-c a を prefix にする。c は compile、g は diff-hl、l は eglot、
;; p は projectile、! は flymake で埋まっている。
(use-package emacs
  :bind (("C-c a a" . my:claude)
         ("C-c a l" . my:claude-layout)
         ("C-c a e" . my:claude-switch-environment)
         ("C-c a t" . my:claude-trust-workspace)
         ("C-c a c" . my:claude-continue)
         ("C-c a r" . my:claude-resume)
         ("C-c a m" . my:claude-set-model)
         ("C-c a i" . my:claude-input)
         ("C-c a s" . my:claude-send-region)
         ("C-c a k" . my:claude-interrupt)
         ("C-c a q" . my:claude-quit)))

(provide 'my-claude)
;;; my-claude.el ends here
