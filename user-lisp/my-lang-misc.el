;;; my-lang-misc.el --- その他の言語 (SQL / bat / Swift / Lua / VB / PowerShell)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; [3] SQL

(use-package sql
  ;; パッケージ名は "sql-mode" だったが、それは feature 名ではない
  ;; (ライブラリは sql.el)。leaf / use-package は :bind / :config を
  ;; (eval-after-load '<パッケージ名>) で遅延させるため、存在しない feature を
  ;; 待ち続けて :config も :bind も永久に適用されていなかった。
  ;; その結果 :config の関数定義が行われず、C-c " / C-c , も未バインドだった。
  ;; 実在する feature 名 sql に直すことで正しく遅延適用される。
  :mode ("\\.ddl$" . sql-mode)
  :custom
  (sql-product 'postgres)
  :bind
  (:map sql-mode-map
   ("C-c \"" . wrap-double-quote-thing-at-symbol)
   ("C-c ," . move-trailing-comma-to-line-start))
  :hook
  (sql-mode-hook . (lambda ()
                     (yas-minor-mode-on)
                     (setq indent-tabs-mode nil)))
  :config
  ;; SQL*Plus 関連は一式削除した (計画書の G-6)。
  ;;
  ;; M-x oracle-settings という手動実行用の関数があり、その中で
  ;;   - sql-oracle-program を "c:/Apps/Oracle/sqlplus.exe" /
  ;;     "~/Applications/Oracle/instantclient_10_2/sqlplus" に決め打ち
  ;;   - NLS_LANG / DYLD_LIBRARY_PATH / LD_LIBRARY_PATH の setenv
  ;;   - sql-product を oracle に変更
  ;;   - sql-interactive-mode-hook で SQL*Plus に ALTER SESSION や set linesize
  ;;     などを流し込む
  ;;   - config-sqlplus.el (この環境には存在しない) の load
  ;;   - 勤務先スキーマ向けの font-lock-add-keywords
  ;; をまとめて行っていた。SQL*Plus を使わなくなったため丸ごと落とす。
  ;; どこからも呼ばれていない関数だったので、他への影響はない。
  ;; sql-product は :custom の postgres のままになる。

  (defun wrap-double-quote-thing-at-symbol ()
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (str (thing-at-point 'symbol))
           (wrapped (format "\"%s\"" str)))
      (delete-region start end)
      (insert wrapped)
      (goto-char (+ 2 end))))

  (defun move-trailing-comma-to-line-start ()
    (interactive)
    (let* ((eol (save-excursion (end-of-line) (point)))
           (pt (re-search-forward ",[ \t]*$" eol t)))
      (when pt
        (goto-char (- pt 1))
        (delete-char 1)
        (forward-line)
        (let* ((eol (save-excursion (end-of-line) (point)))
               (pt (re-search-forward "^[ \t]*--" eol t)))
          (when pt (forward-line)))
        (let* ((eol (save-excursion (end-of-line) (point))))
          (when (= eol pt) (forward-line)))
        (insert "  ,")
        (just-one-space)))))

;;; [3] SQL — SQLcl (Oracle)

;; sql.el の oracle 製品は SQL*Plus を前提にしているが、SQLcl でも REPL は
;; そのまま成立する (プロンプト・エコー・フラッシュを実測)。
;; → docs/languages/sqlcl.md
;;
;; oracle を書き換えるのではなく sqlcl という別製品として足す。oracle から
;; 流用するのは font-lock・文の開始語・終端子・プレースホルダ (&var) の
;; 問い合わせで、SHOW を通る 3 つ (:list-all / :list-table /
;; :completion-object) だけ差し替える。SQLcl は複数オプションの SHOW を
;; 解釈せず SP2-0158 を並べて返すので、`sql-oracle-save-settings' が通らない。

(defcustom my:sql-sqlcl-program
  (or (and (eq system-type 'windows-nt)
           (let ((wrapper (expand-file-name "~/bin/sql.cmd")))
             (and (file-exists-p wrapper) wrapper)))
      "sql")
  "SQLcl を起動するコマンド。
Windows では JLine / JVM の警告を抑えるラッパ `~/bin/sql.cmd' があれば
それを使い、無ければ PATH 上の sql を探す。"
  :type 'string
  :group 'SQL)

(defcustom my:sql-sqlcl-options '("-L")
  "`my:sql-sqlcl-program' に渡す追加オプション。
`-S' を付けてはいけない。バナーと一緒に `SQL> ' プロンプトまで消え、
`sql-redirect' が完了を待てなくなる。"
  :type '(repeat string)
  :group 'SQL)

(defcustom my:sql-sqlcl-java-options
  '("-Dstdin.encoding=UTF-8" "-Dstdout.encoding=UTF-8" "-Dstderr.encoding=UTF-8")
  "SQLcl の JVM に `JDK_JAVA_OPTIONS' 経由で渡すプロパティ。
これが無いと stdin は JVM の native encoding (Windows では cp932) で
読まれ、送った日本語リテラルが化ける。出力側は `-Dfile.encoding=UTF-8'
で既に UTF-8 なので、入力だけが食い違う。

java のランチャは拾ったことを stderr に 1 行書く
 (\"NOTE: Picked up JDK_JAVA_OPTIONS: ...\")。消す手段は無いので、
会話バッファの先頭にそれが出るのは正常。

nil にすると渡さない。そのときは入力を cp932 で送る必要があるので
`my:sql-sqlcl-coding-system' も合わせて変える。"
  :type '(repeat string)
  :group 'SQL)

(defcustom my:sql-sqlcl-coding-system 'utf-8-unix
  "SQLcl との入出力に使う coding system。
`my:sql-sqlcl-java-options' で stdin を UTF-8 にしてあるので既定は
`utf-8-unix'。オプションを渡さない場合は入力が cp932 になるため
`(utf-8-unix . cp932-unix)' のように cons で与える (car が復号、
cdr が符号化)。"
  :type '(choice coding-system (cons coding-system coding-system))
  :group 'SQL)

(defcustom my:sql-sqlcl-startup-commands
  '("SET LINESIZE 32767 PAGESIZE 50000 SERVEROUTPUT ON")
  "最初のプロンプトが出たら流す SQLcl のコマンド。
既定のまま (LINESIZE 80) だと少し横に広い結果で桁が折り返されるので
広げてある。SQLcl 側で列幅は中身に合わせて調整されるため、広げても
詰まらない。

SQLcl の SET は SQL*Plus の全部を持っているわけではない。知らない
オプションは `SP2-0158' を返すだけで、同じ行の他のオプションは効く
 (`RECSEP' が無いのは実測済み)。足すときは 1 つずつ確かめること。

SET は何も出力しないが、コマンド 1 つにつきプロンプトが 1 回出るので、
起動直後の `SQL> ' が 1 つ余計に見えるのは正常。

一覧表示や補完 (`my:sql-sqlcl-list-all' など) はこの値を流し直して
元の状態に戻すので、SHOW で現在値を読む必要がない。"
  :type '(repeat string)
  :group 'SQL)

(defcustom my:sql-sqlcl-connections-directory
  (if (eq system-type 'windows-nt)
      (and (getenv "APPDATA")
           (expand-file-name "DBTools/connections" (getenv "APPDATA")))
    (expand-file-name "~/.dbtools/connections"))
  "SQLcl が名前付き接続を保存しているディレクトリ。
接続 1 つが `<ハッシュ>/dbtools.properties' の `name=' 行に入っている。"
  :type '(choice directory (const nil))
  :group 'SQL)

(defcustom my:sql-sqlcl-connection "WebCoreDB_main"
  "既定の接続先 (SQLcl の connmgr に保存した名前)。
`my:sql-sqlcl' は `sql-database' が空のときだけこれを入れる。
以後はミニバッファの初期値が前回の接続先になる。"
  :type 'string
  :group 'SQL)

(defcustom my:sql-sqlcl-login-params
  '((database :completion my:sql-sqlcl--connection-completion-table
              :must-match confirm))
  "SQLcl のログインに要るパラメータ。
`sql-database' には connmgr の接続名を入れる。認証情報は SQLcl 側に
預けてあるので user / password は訊かない。保存済みでない文字列を
入れた場合は接続文字列 (`USER/PASS@HOST:PORT/SERVICE') として渡す。"
  :type 'sql-login-params
  :group 'SQL)

(defun my:sql-sqlcl--connection-names ()
  "SQLcl に保存されている名前付き接続の名前を集める。
`connmgr list' は JVM の起動に数秒かかるので、保存先のファイルを読む。"
  (let (names)
    (when (and my:sql-sqlcl-connections-directory
               (file-directory-p my:sql-sqlcl-connections-directory))
      (dolist (file (directory-files-recursively
                     my:sql-sqlcl-connections-directory
                     "\\`dbtools\\.properties\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward "^name=\\(.+\\)$" nil t)
            (push (string-trim (match-string 1)) names)))))
    (sort names #'string<)))

(defun my:sql-sqlcl--connection-completion-table (string pred action)
  "保存済みの接続名を `completing-read' の COLLECTION として返す。"
  (complete-with-action action (my:sql-sqlcl--connection-names) string pred))

(defun my:sql-comint-sqlcl (product options &optional buf-name)
  "SQLcl の comint バッファを作る。
接続は `sql-database' に入れた名前付き接続 (`-name') で行う。保存済みの
名前でなければ接続文字列としてそのまま渡し、空なら `/nolog' で起こす。"
  (let* ((database (string-trim (or sql-database "")))
         (params
          (append options
                  (cond
                   ((string-empty-p database) (list "/nolog"))
                   ((member database (my:sql-sqlcl--connection-names))
                    (list "-name" database))
                   (t (list database)))))
         (process-environment
          (if my:sql-sqlcl-java-options
              (cons (concat "JDK_JAVA_OPTIONS="
                            (string-join my:sql-sqlcl-java-options " "))
                    process-environment)
            process-environment)))
    (sql-comint product params buf-name)
    (let ((proc (get-buffer-process (current-buffer)))
          (coding my:sql-sqlcl-coding-system))
      (when proc
        (set-process-coding-system proc
                                   (if (consp coding) (car coding) coding)
                                   (if (consp coding) (cdr coding) coding))))))

;; 【重要】最初のプロンプトより前に stdin へ書くと、SQLcl はそれ以降
;; プロンプトも応答も一切返さなくなる (JVM の起動と接続で 8 秒前後かかる
;; ので、その間に 1 行打つだけで当たる)。エラーも出ず、見た目は「繋がって
;; いるのに無言」になるだけなので、セッションを作り直すしかなくなる。
;;   - 起動時の設定 (`my:sql-sqlcl-startup-commands') は
;;     `comint-output-filter-functions' でプロンプトを見てから送る
;;   - ユーザーの入力はそれまで保留して、見えてから順に流す

(defvar-local my:sql-sqlcl--ready nil
  "接続が済んで最初の `SQL> ' を見たか。")

(defvar-local my:sql-sqlcl--pending nil
  "プロンプトが出る前に送ろうとした入力 (逆順)。")

(defun my:sql-sqlcl--on-output (output)
  "最初のプロンプトを見たら、起動時の設定と保留した入力を流す。
`comint-output-filter-functions' に (バッファローカルで) 入れる。"
  (when (and (not my:sql-sqlcl--ready)
             (string-match-p "SQL> " output))
    (setq my:sql-sqlcl--ready t)
    (remove-hook 'comint-output-filter-functions #'my:sql-sqlcl--on-output t)
    (let ((proc (get-buffer-process (current-buffer))))
      (when (process-live-p proc)
        (dolist (command my:sql-sqlcl-startup-commands)
          (process-send-string proc (concat command "\n")))
        (let ((pending (nreverse my:sql-sqlcl--pending)))
          (setq my:sql-sqlcl--pending nil)
          (dolist (string pending)
            (sql-input-sender proc string)))))))

(defun my:sql-sqlcl--magic-terminator (orig buf str terminator)
  "SQLcl に自動で足す終端子を `;' に限る (`sql-send-magic-terminator' の advice)。

【重要】`sql-send-terminator' を t にすると製品の終端子が送られるが、
SQLcl の**裸の `/' は直前の文をもう一度実行する**(実測。
`show linesize' を送っただけで直前の DELETE が走りうる)。裸の `;' は
バッファを listing するだけなので無害。

sql.el は `sql-send-terminator' が nil でも終端子を送る
 (`sql-send-magic-terminator' が nil を `;' に読み替える)。そのため
`;' でも `/' でも終わっていない文字列 — SET や SHOW の行など — を送ると、
直前の文の listing が返る。これは無害な見た目の問題。"
  (if (with-current-buffer buf (eq sql-product 'sqlcl))
      (unless (string-match-p "\\(;\\|^/\\)[ \t\r\n]*\\'" str)
        (with-current-buffer buf
          (sql-input-sender (get-buffer-process buf) ";")))
    (funcall orig buf str terminator)))

(defun my:sql-sqlcl--hold-input (orig proc string)
  "接続が済むまで SQLcl への送信を保留する。
`sql-input-sender' への :around advice。comint の入力も
`sql-send-region' などもここを通るので、1 箇所で押さえられる。"
  (if (and (eq sql-product 'sqlcl) (not my:sql-sqlcl--ready))
      (progn
        (push string my:sql-sqlcl--pending)
        (message "SQLcl の接続待ち。プロンプトが出てから送ります"))
    (funcall orig proc string)))

(defconst my:sql-sqlcl--list-settings
  "SET LINESIZE 32767 PAGESIZE 50000 FEEDBACK OFF TIMING OFF TRIMOUT ON"
  "一覧を取る間だけ使う設定。
SHOW で現在値を読まないので、戻すときは `my:sql-sqlcl-startup-commands'
を流し直す (`my:sql-sqlcl--restore-settings')。")

(defun my:sql-sqlcl--restore-settings (sqlbuf)
  "SQLBUF の設定を `my:sql-sqlcl-startup-commands' の状態に戻す。"
  (sql-redirect sqlbuf my:sql-sqlcl-startup-commands))

(defun my:sql-sqlcl--ansi-color (outbuf)
  "OUTBUF に残った ANSI の escape を face に変える。
SQLcl は見出しを必ず太字の escape 付きで出す (`SET COLOR OFF' は
受け付けるが効かない。DESCRIBE で 6 個、INFO+ で 24 個を実測)。
会話バッファでは `ansi-color-process-output' が処理するが、
`sql-redirect' の出力先は comint ではないので生のまま残る。"
  (when (buffer-live-p (get-buffer outbuf))
    (require 'ansi-color)
    (with-current-buffer outbuf
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun my:sql-sqlcl-list-all (sqlbuf outbuf enhanced _table-name)
  "Implements :list-all under SQLcl.
列幅は SQLcl が中身に合わせるので、`COLUMN ... FORMAT' は使わない。"
  (sql-redirect sqlbuf my:sql-sqlcl--list-settings)
  (sql-redirect
   sqlbuf
   (concat "SELECT INITCAP(x.object_type) AS \"Type\", "
           (if enhanced
               "x.owner ||'.'|| x.object_name AS \"Name\" FROM all_objects x "
             "x.object_name AS \"Name\" FROM user_objects x ")
           "WHERE x.object_type NOT LIKE '% BODY' "
           (if enhanced "AND x.owner <> 'SYS' " "")
           "ORDER BY 2, 1;")
   outbuf)
  (my:sql-sqlcl--ansi-color outbuf)
  (my:sql-sqlcl--restore-settings sqlbuf))

(defun my:sql-sqlcl-list-table (sqlbuf outbuf enhanced table-name)
  "Implements :list-table under SQLcl.
ENHANCED (`C-u') のときは SQLcl の `INFO+' を使う (制約・索引・統計も出る)。"
  (sql-redirect sqlbuf
                (format "SET LINESIZE %d PAGESIZE 50000 FEEDBACK OFF TIMING OFF"
                        (max 65 (min 120 (window-width)))))
  (sql-redirect sqlbuf
                (format (if enhanced "INFO+ %s" "DESCRIBE %s") table-name)
                outbuf)
  (my:sql-sqlcl--ansi-color outbuf)
  (my:sql-sqlcl--restore-settings sqlbuf))

(defun my:sql-sqlcl-completion-object (sqlbuf schema)
  "Implements :completion-object under SQLcl.
問い合わせ自体は oracle のものを使い (`sql-oracle-completion-types' も
そのまま効く)、見出しと件数が混ざらない状態を自分で作ってから渡す。

SQLcl は列幅に合わせて右側を空白で埋める (`SET TRIMOUT ON' でも
埋まる)。oracle 側の正規表現は行末までを 1 つの名前として取るので、
ここで落とさないと候補が \"MT_BRAND   \" のようになる。"
  (sql-redirect sqlbuf "SET LINESIZE 32767 PAGESIZE 0 FEEDBACK OFF TRIMOUT ON")
  (prog1 (mapcar #'string-trim-right (sql-oracle-completion-object sqlbuf schema))
    (my:sql-sqlcl--restore-settings sqlbuf)))

(defun my:sql-sqlcl--interactive-setup ()
  "SQLcl のバッファの後始末と見張りを入れる。

継続プロンプト除去フィルタを外す。SQLcl は継続プロンプト (`  2  ') を
出さず、複数行の文は完成するまで何も返さない。フィルタが残っていると、
送った行数ぶんの `SQL> ' を代わりに食ってプロンプトが画面から消える。

ここは `sql-interactive-mode' のフックなので、`comint-mode' の
`kill-all-local-variables' より後に走る (comint バッファを作る
`my:sql-comint-sqlcl' の側でバッファローカルな設定をしても消える)。"
  (when (eq sql-product 'sqlcl)
    (remove-hook 'comint-preoutput-filter-functions
                 #'sql-interactive-remove-continuation-prompt t)
    (setq my:sql-sqlcl--ready nil
          my:sql-sqlcl--pending nil)
    (add-hook 'comint-output-filter-functions #'my:sql-sqlcl--on-output nil t)))

;;;###autoload
(defun my:sql-sqlcl (&optional buffer)
  "SQLcl を子プロセスとして起こし、SQL interactive モードで繋ぐ。
接続先は `sql-database' (SQLcl の connmgr に保存した名前)。空のときは
`my:sql-sqlcl-connection' を初期値にして訊く。

`C-u' を付けるとバッファ名も訊く (`sql-product-interactive' と同じ)。"
  (interactive "P")
  (require 'sql)
  (when (and (string-empty-p (string-trim (or (default-value 'sql-database) "")))
             my:sql-sqlcl-connection)
    (setq-default sql-database my:sql-sqlcl-connection))
  (sql-product-interactive 'sqlcl buffer))

;; 製品の登録は sql.el がロードされてから。再評価で user-error にならない
;; よう、既にあれば足さない (`sql-add-product' は二重登録を拒否する)。
(use-package sql
  :defer t
  :config
  (unless (assq 'sqlcl sql-product-alist)
    (sql-add-product
     'sqlcl "SQLcl"
     :font-lock 'sql-mode-oracle-font-lock-keywords
     :sqli-program 'my:sql-sqlcl-program
     :sqli-options 'my:sql-sqlcl-options
     :sqli-login 'my:sql-sqlcl-login-params
     :sqli-comint-func #'my:sql-comint-sqlcl
     :list-all #'my:sql-sqlcl-list-all
     :list-table #'my:sql-sqlcl-list-table
     :completion-object #'my:sql-sqlcl-completion-object
     ;; 【重要】プロンプトの手前に ANSI の reset が入ることがある。
     ;; `INFO+' の出力は最後の行が "\e[mSQL> " で終わるので、素の
     ;; "^SQL> " では一致せず、`sql-redirect' が完了を待ち続けて
     ;; **Emacs が固まる** (あのループにはタイムアウトが無い)。
     ;; 会話バッファ側は ansi-color が escape を消すので気づけない。
     :prompt-regexp "^\\(?:\e\\[[0-9;]*m\\)*SQL> "
     :prompt-length 5
     ;; 継続プロンプトは出ない。:prompt-cont-regexp を足すと
     ;; comint-prompt-regexp に混ぜられるので、書かないのが正しい。
     :statement 'sql-oracle-statement-starters
     :syntax-alist '((?$ . "_") (?# . "_"))
     :terminator '("\\(^/\\|;\\)" . "/")
     :input-filter #'sql-placeholders-filter))
  (add-hook 'sql-interactive-mode-hook #'my:sql-sqlcl--interactive-setup)
  (advice-add 'sql-input-sender :around #'my:sql-sqlcl--hold-input)
  (advice-add 'sql-send-magic-terminator :around #'my:sql-sqlcl--magic-terminator))

;;; [3] bat

;; leaf は (require) を出さないので bat-mode は起動時にロードされない。
;; use-package で同じにするため :defer t を付け、設定は :init に置く。
;; bat-font-lock-keywords には bat-mode.el の defvar より先に値が入るため、
;; defvar は上書きせずこちらの値が残る (leaf のときと同じ)。
(use-package bat-mode
  :if (eq system-type 'windows-nt)
  :defer t
  :init
  (setq bat-font-lock-keywords
        (eval-when-compile
          (let ((COMMANDS
                 '("assoc" "at" "attrib" "cd" "cls" "color" "copy" "date" "del" "dir"
                   "doskey" "echo" "endlocal" "erase" "fc" "find" "findstr" "format"
                   "ftype" "label" "md" "mkdir" "more" "move" "net" "path" "pause"
                   "popd" "prompt" "pushd" "rd" "ren" "rename" "replace" "rmdir" "set"
                   "setlocal" "shift" "sort" "subst" "time" "title" "tree" "type"
                   "ver" "vol" "xcopy"))
                (CONTROLFLOW
                 '("call" "cmd" "defined" "do" "else" "equ" "exist" "exit" "for" "geq"
                   "goto" "gtr" "if" "in" "leq" "lss" "neq" "not" "start"))
                (UNIX
                 '("bash" "cat" "cp" "fgrep" "grep" "ls" "sed" "sh" "mv" "rm")))
            `(("\\_<\\(call\\|goto\\)\\_>[ \t]+%?\\([A-Za-z0-9-_\\:.]+\\)%?"
               (2 font-lock-constant-face t))
              ("^:[^:].*"
               . 'bat-label-face)
              ("\\_<\\(defined\\|set\\)\\_>[ \t]*\\(\\w+\\)"
               (2 font-lock-variable-name-face))
              ("%\\([A-Za-z0-9_]+\\)%?"
               (1 font-lock-variable-name-face))
              ("!\\([A-Za-z0-9_]+\\)!?"        ; delayed-expansion !variable!
               (1 font-lock-variable-name-face))
              ("[ =][-/]+\\([A-Za-z0-9_]\\)"
               (1 font-lock-type-face append))
              (,(concat "\\_<" (regexp-opt COMMANDS) "\\_>") . font-lock-builtin-face)
              (,(concat "\\_<" (regexp-opt CONTROLFLOW) "\\_>") . font-lock-keyword-face)
              (,(concat "\\_<" (regexp-opt UNIX) "\\_>") . font-lock-warning-face))))))

;;; [3] swift

(use-package swift-mode
  :straight t
  :hook (swift-mode-hook . eglot-ensure))

;; lsp-sourcekit は lsp-mode 用のパッケージなので削除した。
;; sourcekit-lsp の登録は my-lsp.el の eglot-server-programs で行っている。

;;; [3] lua

(use-package lua-mode
  :straight t
  :mode (".nyagos" . lua-mode))

;;; [3] VisualBasic

(use-package visual-basic-mode
  ;; in site-lisp
  :mode ("\\.\\(frm\\|bas\\|cls\\|vbs\\|vb\\)$" . visual-basic-mode)
  :hook (visual-basic-mode-hook . (lambda () (setq mode-name "vb")))
  :config
  (setq visual-basic-mode-indent 4))

;;; [3] PowerShell

(use-package powershell
  ;; Emacs に組み込みの PowerShell モードは無く、tree-sitter 版も MELPA に
  ;; 出ていないので jschaf/powershell.el を使う。font-lock とインデント目当て。
  :straight t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(provide 'my-lang-misc)
;;; my-lang-misc.el ends here
