;;; my-ideco.el --- iDeCo の日次サマリを psql で見る  -*- lexical-binding: t -*-
;;; Commentary:
;; 毎朝スクレイピングして PostgreSQL に溜めている ideco_daily_summary を、
;; 直近から数行だけ罫線の表にして見る。`M-x check-ideco'。
;;
;;   ┌────────────┬────────────┬────────────┬────────────┬────────────┐
;;   │ 日付       │   資産残高 │ 拠出金累計 │       損益 │     前日比 │
;;   ├────────────┼────────────┼────────────┼────────────┼────────────┤
;;   │ 2026-11-10 │ 12,914,906 │ 10,265,130 │  2,649,776 │ ↑   83,556 │
;;   │ 2026-11-09 │ 12,831,350 │ 10,265,130 │  2,566,220 │ →        0 │
;;
;; 前日比は損益の「1 つ前の行」との差で、↑ は赤・↓ は緑・→ (変化なし) は灰色
;; (国内の証券会社の慣習)。資産残高で比べないのは、拠出日に相場が下がっても
;; 拠出の分だけ増えて ↑ になるため (実測で 2026-09-15 は資産残高 +7,255 /
;; 損益 -15,745)。
;;
;; `psql' を 1 回起こすだけ (同期)。Windows では WSL の psql を使う。
;; 見終わったら `q' でバッファごと消える。`g' で取り直す。
;;
;; パスワードは `auth-source' (`~/.authinfo.gpg') から引き、無ければ訊く。
;; 書いておくなら次の 1 行。
;;
;;   machine 192.168.1.5 port 5432 login postgres password ********
;;
;;; Code:

(require 'auth-source)
(require 'seq)
(require 'subr-x)

(defgroup my:ideco nil
  "iDeCo の日次サマリを見る。"
  :group 'tools
  :prefix "my:ideco-")

;;; [1] 設定

(defcustom my:ideco-psql-command
  (if (eq system-type 'windows-nt)
      '("wsl.exe" "-e" "psql")
    '("psql"))
  "psql をどう起こすか。先頭が実行ファイルで、残りは前置きの引数。

Windows には PostgreSQL のクライアントを入れていないので WSL の psql を
使う (`wsl.exe -e psql')。実測で起動は 0.16 秒。"
  :type '(repeat string))

(defcustom my:ideco-host "192.168.1.5"
  "PostgreSQL のホスト。"
  :type 'string)

(defcustom my:ideco-port 5432
  "PostgreSQL のポート。"
  :type 'integer)

(defcustom my:ideco-database "ideco_db"
  "データベース名。"
  :type 'string)

(defcustom my:ideco-user "postgres"
  "接続するユーザー。"
  :type 'string)

(defcustom my:ideco-table "ideco_daily_summary"
  "日次サマリのテーブル。"
  :type 'string)

(defcustom my:ideco-rows 10
  "既定で何行出すか。1 日 1 行なので、既定は直近 10 日ぶん。
前置引数で変えられる (`C-u 30 M-x check-ideco')。"
  :type 'integer)

(defcustom my:ideco-buffer-name "*ideco*"
  "表を出すバッファの名前。"
  :type 'string)

(defconst my:ideco--columns
  '(("summary_date"       "日付"       left  my:ideco--format-date)
    ("shisan_zandaka"     "資産残高"   right my:ideco--format-number)
    ("kyoshutukin_ruikei" "拠出金累計" right my:ideco--format-number)
    ("soneki"             "損益"       right my:ideco--format-number))
  "(列名 見出し 寄せ 整形関数) の並び。

**クエリも表もここから作る**ので、見る列を変えるならここ 1 か所でよい。
`created_at' / `updated_at' と `ideco_summary_id' は見ないので入れていない。")

(defconst my:ideco--null "-"
  "NULL と空欄に出す文字。")

(defconst my:ideco--diff-column "soneki"
  "前日比を出す列 (`my:ideco--columns' の列名)。
表ではこの列の直後に「前日比」の列を挟む。

資産残高ではなく損益で比べる。資産残高は拠出金が入った日に、相場が
下がっていても拠出の分だけ増えて `↑' になる。損益 (= 資産残高 −
拠出金累計) なら運用だけの増減が出る。")

;; 色は国内の証券会社の慣習に合わせて「上昇＝赤、下落＝緑」。
(defface my:ideco-up '((t :inherit error :weight normal))
  "前日より増えた行の前日比 (赤)。")

(defface my:ideco-down '((t :inherit success :weight normal))
  "前日より減った行の前日比 (緑)。")

(defface my:ideco-flat '((t :inherit shadow))
  "前日と同じ行の前日比 (土日など)。")

;;; [2] 値の整形

(defun my:ideco--format-date (s)
  "YYYYMMDD を YYYY-MM-DD にする。想定外の形なら素通し。

`summary_date' は文字列の列 (psql の出力が左詰めなので判る) なので、
ここで見た目だけ整える。並び順は YYYYMMDD の辞書順 = 日付順。"
  (if (string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" s)
      (format "%s-%s-%s" (match-string 1 s) (match-string 2 s) (match-string 3 s))
    s))

(defun my:ideco--format-number (s)
  "整数に 3 桁ごとのカンマを入れる。整数でなければ素通し。
生の桁 (12831350) は目で追えないため。"
  (if (string-match-p "\\`-?[0-9]+\\'" s)
      (let* ((neg (string-prefix-p "-" s))
             (digits (if neg (substring s 1) s))
             (out ""))
        (while (> (length digits) 3)
          (setq out (concat "," (substring digits -3) out)
                digits (substring digits 0 -3)))
        (concat (if neg "-" "") digits out))
    s))

(defun my:ideco--cell (value spec)
  "VALUE を列 SPEC に従って整形する。空なら `my:ideco--null'。"
  (let ((s (string-trim (or value ""))))
    (if (string-empty-p s)
        my:ideco--null
      (funcall (nth 3 spec) s))))

;;; [2.5] 前日比

(defun my:ideco--int (s)
  "S が整数なら数にして返す。そうでなければ nil。"
  (let ((s (string-trim (or s ""))))
    (and (string-match-p "\\`-?[0-9]+\\'" s) (string-to-number s))))

(defun my:ideco--diff-index ()
  "`my:ideco--diff-column' が `my:ideco--columns' の何番目か。"
  (seq-position (mapcar #'car my:ideco--columns) my:ideco--diff-column))

(defun my:ideco--with-diff (list value)
  "LIST の前日比を挟む位置に VALUE を差し込んだ新しいリストを返す。
見出し・寄せ・各行のセルのどれにも同じ位置で差し込むために使う。"
  (let ((i (1+ (my:ideco--diff-index))))
    (append (seq-take list i) (list value) (seq-drop list i))))

(defun my:ideco--diff-cells (data)
  "DATA (新しい順に並んだ生のセル) の各行について前日比のセルを返す。

比べる相手は**次の行** (= 1 つ前の日)。だから呼ぶ側は表に出す行数より
1 行多く取ること。最後の行は相手が無いので `my:ideco--null'。

矢印はセルの左端に揃え、数字はその右で右詰めにする。セル全体を右詰めに
すると、数字の桁数しだいで矢印の位置が行ごとにずれて縦に追えない。
符号は付けない (矢印と色で足りる)。"
  (let* ((i (my:ideco--diff-index))
         (vals (mapcar (lambda (r) (my:ideco--int (nth i r))) data))
         (deltas (seq-map-indexed
                  (lambda (v k)
                    (let ((p (nth (1+ k) vals)))
                      (and v p (- v p))))
                  vals))
         (texts (mapcar (lambda (d)
                          (and d (my:ideco--format-number
                                  (number-to-string (abs d)))))
                        deltas))
         (w (apply #'max 0 (mapcar (lambda (s) (if s (string-width s) 0))
                                   texts))))
    (seq-mapn
     (lambda (d s)
       (if (null d)
           my:ideco--null
         (pcase-let ((`(,arrow . ,face)
                      (cond ((> d 0) '("↑" . my:ideco-up))
                            ((< d 0) '("↓" . my:ideco-down))
                            (t       '("→" . my:ideco-flat)))))
           (propertize (concat arrow " " (my:ideco--pad s w 'right))
                       'face face))))
     deltas texts)))

;;; [3] 罫線の表

(defun my:ideco--pad (s width align)
  "S を WIDTH 桁に詰める。ALIGN が `right' なら右詰め。
【重要】桁は `string-width' で数える (`length' ではない)。"
  (let ((gap (max 0 (- width (string-width s)))))
    (if (eq align 'right)
        (concat (make-string gap ?\s) s)
      (concat s (make-string gap ?\s)))))

(defun my:ideco--table (header rows aligns)
  "HEADER と ROWS を罫線の表にした文字列にする。ALIGNS は列ごとの寄せ方。

【重要】罫線素片は 1 文字で 2 桁ある。`─' `│' は East Asian Ambiguous で
`site-lisp/eaw.el' が幅 2 を与え、HackGen も全角 (16px) で描く。つまり
`(make-string (+ w 2) ?─)' は w+2 桁ではなく **2(w+2) 桁**になり、罫線の
行だけが倍の長さになる。セルの詰め物は半角空白 (1 桁) なので列幅は
どんな値でも組めるが、罫線側は 2 桁単位でしか刻めない。そこで**列幅を
「w+2 が罫線 1 文字の桁数の倍数」になるまで広げる**。eaw を外した Emacs や
ambiguous を幅 1 に倒す端末では `rw' が 1 になるので何も広げない。

同じ事情を `my:claude--table-string' も踏んでいる。経緯はそちらのコメント。
折り返しはしない (この表は 4 列で桁が決まっているため)。"
  (let* ((rw (max 1 (char-width ?─)))
         (ncol (length header))
         (idx (number-sequence 0 (1- ncol)))
         (widths
          (mapcar (lambda (i)
                    (let ((w (apply #'max (string-width (nth i header))
                                    (mapcar (lambda (r)
                                              (string-width (or (nth i r) "")))
                                            rows))))
                      (+ w (mod (- rw (mod (+ w 2) rw)) rw))))
                  idx))
         (rule (lambda (l m r)
                 (concat l (mapconcat (lambda (w)
                                        (make-string (/ (+ w 2) rw) ?─))
                                      widths m)
                         r "\n")))
         (line (lambda (cells face)
                 (concat "│"
                         (mapconcat
                          (lambda (i)
                            (let ((s (concat " " (my:ideco--pad
                                                  (or (nth i cells) "")
                                                  (nth i widths) (nth i aligns))
                                             " ")))
                              (if face (propertize s 'face face) s)))
                          idx "│")
                         "│\n"))))
    (concat (funcall rule "┌" "┬" "┐")
            (funcall line header 'font-lock-keyword-face)
            (funcall rule "├" "┼" "┤")
            (mapconcat (lambda (r) (funcall line r nil)) rows "")
            (funcall rule "└" "┴" "┘"))))

;;; [4] psql

(defun my:ideco--wsl-p ()
  "psql を WSL 経由で起こすなら非 nil。"
  (string-match-p "\\`wsl\\(\\.exe\\)?\\'"
                  (downcase (file-name-nondirectory
                             (or (car my:ideco-psql-command) "")))))

(defun my:ideco--password ()
  "`auth-source' を引き、無ければ訊く。**保存はしない** (my-oracle と同じ)。

【重要】引けなかったときは**プロンプトにそう書く**。黙って `read-passwd' に
落ちると、`~/.authinfo.gpg' を書いたのに訊かれたときに「書式が悪いのか・
復号に失敗したのか・そもそも見ていないのか」が区別できない (実際に詰まった)。
`auth-sources' のファイルが 1 つも無いときは普通のプロンプトのままにする
 (auth-source を使っていないなら、毎回そう書かれても邪魔なだけ)。"
  (let* ((port (number-to-string my:ideco-port))
         (found (car (auth-source-search :host my:ideco-host
                                         :user my:ideco-user
                                         :port port :max 1)))
         (secret (plist-get found :secret)))
    (if secret
        (if (functionp secret) (funcall secret) secret)
      (let ((files (seq-filter #'file-exists-p
                               (mapcar #'expand-file-name
                                       (seq-filter #'stringp auth-sources)))))
        (when files
          (message "auth-source は %s@%s:%s を見つけられなかった (見たのは %s)"
                   my:ideco-user my:ideco-host port (string-join files " ")))
        (read-passwd (format "%s@%s のパスワード%s: "
                             my:ideco-user my:ideco-host
                             (if files " [auth-source では見つからず]" "")))))))

(defun my:ideco--environment (password)
  "PASSWORD を `PGPASSWORD' で渡すための `process-environment' を返す。

コマンドラインには載せない (`ps' から見えるため)。

【重要】WSL 経由のときは `WSLENV' に名前を書かないと Linux 側のプロセスに
渡らない (実測で確認した)。既存の `WSLENV' は壊さず後ろに残す。
フラグ (`/u' など) は付けない —— 素の名前で渡ることを実測した形にしてある。"
  (let ((env (cons (concat "PGPASSWORD=" password) process-environment)))
    (if (my:ideco--wsl-p)
        (let ((old (getenv "WSLENV")))
          (cons (concat "WSLENV=PGPASSWORD"
                        (if (and old (not (string-empty-p old)))
                            (concat ":" old)
                          ""))
                env))
      env)))

(defun my:ideco--query (rows)
  "直近 ROWS 行を取る SELECT 文。"
  (format "select %s from %s order by summary_date desc limit %d"
          (mapconcat #'car my:ideco--columns ", ")
          my:ideco-table rows))

(defun my:ideco--psql (sql)
  "SQL を psql に流し、セルのリストのリストを返す。

オプションには理由がある。**どれも外してはいけない。**

- `-w' 【重要】これが無いと psql は**パスワードを stdin から読もうとして
  待ち続ける**。`call-process' は stdin を与えないので、Emacs ごと
  固まったように見える (実測)。`-w' なら 0.18 秒で認証エラーを返す
- `-X' `~/.psqlrc' を読まない (出力形式を変えられると壊れる)
- `-t --csv' 見出しと行数のフッタを出さない機械可読の形
- `-v ON_ERROR_STOP=1' 失敗を終了コードで返させる

【重要】stderr は**一時ファイルに分ける**。psql 16 からサーバ 18 に繋ぐと
`WARNING: psql major version 16, server major version 18' が stderr に出る
ので、`call-process' の出力先を 1 つにすると**この警告が CSV に混ざって
パースが壊れる**。終了コードは 0 なので気づけない。"
  (let* ((password (my:ideco--password))
         (program (car my:ideco-psql-command))
         (args (append (cdr my:ideco-psql-command)
                       (list "-w" "-X" "-q" "-t" "--csv"
                             "-v" "ON_ERROR_STOP=1"
                             "-h" my:ideco-host
                             "-p" (number-to-string my:ideco-port)
                             "-U" my:ideco-user
                             "-d" my:ideco-database
                             "-c" sql)))
         (process-environment (my:ideco--environment password))
         ;; 出力は UTF-8。引数は ASCII だけなので書き込み側は問わない
         ;; (CLAUDE.md「文字コード」。日本語を渡すようになったら要検証)。
         (coding-system-for-read 'utf-8-unix)
         (errfile (make-temp-file "ideco-psql-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process program nil (list t errfile)
                               nil args)))
            (unless (eq status 0)
              (user-error "psql が失敗した (%s): %s" status
                          (my:ideco--first-line errfile)))
            (my:ideco--parse (buffer-string))))
      (ignore-errors (delete-file errfile)))))

(defun my:ideco--first-line (file)
  "FILE の 1 行目を返す (エラーメッセージ用)。

**全部は出さない。** psql は接続の試行ごとに同じ内容を繰り返すことがあり、
認証に失敗したときは 2 行出て 2 行目は 1 行目の末尾と同じだった (実測)。
1 行目にいちばん情報があるので、それだけ出す。"
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (ignore-errors (insert-file-contents file)))
    (or (car (split-string (string-trim (buffer-string)) "\n" t))
        "(stderr は空)")))

(defun my:ideco--parse (text)
  "psql の `-t --csv' の出力をセルのリストのリストにする。

クォートは解かない。**この表に出る値 (数値と YYYYMMDD) には `,' も `\"' も
現れない**ので要らない。文字列の列を足すなら、ここを直すこと。"
  (let ((ncol (length my:ideco--columns)))
    (mapcar (lambda (line)
              (let ((cells (split-string line ",")))
                (unless (= (length cells) ncol)
                  (user-error "psql の出力が %d 列ではない: %s" ncol line))
                cells))
            (split-string (string-trim text) "\n" t "[ \t\r]+"))))

;;; [5] バッファ

(defvar-local my:ideco--rows nil
  "そのバッファを作ったときの行数。`g' で取り直すときに使う。")

(define-derived-mode my:ideco-mode special-mode "iDeCo"
  "iDeCo の日次サマリを見るモード。
`q' でバッファごと閉じ、`g' で取り直す。"
  ;; 【重要】バッファローカルにしたいものは `setq-local' で書く
  ;; (CLAUDE.md §1)。`truncate-lines' は自動バッファローカルだが、
  ;; 書き分けを揃えておく。
  (setq-local truncate-lines t))

;; `special-mode-map' の `q' は `quit-window' で、バッファは残る。
;; 見終わったら消したいので kill する側に倒す。`g' は
;; `revert-buffer-function' を立てれば `special-mode' のものがそのまま効く。
(keymap-set my:ideco-mode-map "q" #'my:ideco-quit)

(defun my:ideco-quit ()
  "表を閉じ、バッファも消す。"
  (interactive)
  (quit-window t))

(defun my:ideco--revert (&rest _)
  "`revert-buffer' から呼ばれて取り直す。"
  (my:ideco-check (or my:ideco--rows my:ideco-rows)))

;;;###autoload
(defun my:ideco-check (&optional rows)
  "iDeCo の日次サマリを直近 ROWS 行ぶん、罫線の表にして出す。

ROWS の既定は `my:ideco-rows'。前置引数で行数を指定できる
 (`C-u 30 M-x check-ideco')。`M-x check-ideco' はこれの別名。"
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       my:ideco-rows)))
  (let* ((rows (max 1 (or rows my:ideco-rows)))
         ;; 1 行多く取る。いちばん古い行の前日比を出す相手 (表には出さない)。
         (data (my:ideco--psql (my:ideco--query (1+ rows))))
         (diffs (seq-take (my:ideco--diff-cells data) rows))
         (cells (seq-mapn (lambda (row diff)
                            (my:ideco--with-diff
                             (seq-map-indexed
                              (lambda (spec i) (my:ideco--cell (nth i row) spec))
                              my:ideco--columns)
                             diff))
                          (seq-take data rows) diffs))
         (buf (get-buffer-create my:ideco-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'my:ideco-mode)
          (my:ideco-mode))
        (erase-buffer)
        (if (null cells)
            (insert "該当する行が無い。\n")
          (insert (my:ideco--table
                   (my:ideco--with-diff (mapcar #'cadr my:ideco--columns)
                                        "前日比")
                   cells
                   (my:ideco--with-diff (mapcar #'caddr my:ideco--columns)
                                        'right))))
        (insert (propertize
                 (format "%s / %s@%s / %d 行 (%s 取得)\n"
                         my:ideco-table my:ideco-database my:ideco-host
                         (length cells)
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 'face 'shadow))
        (goto-char (point-min)))
      ;; 【重要】`define-derived-mode' は `kill-all-local-variables' を通る
      ;; ので、バッファローカルの設定はモードを立てた**後**に置く。
      (setq-local my:ideco--rows rows)
      (setq-local revert-buffer-function #'my:ideco--revert))
    (pop-to-buffer buf)
    (message "%s: %d 行" my:ideco-table (length cells))))

;;;###autoload
(defalias 'check-ideco #'my:ideco-check
  "`my:ideco-check' の別名。`M-x check-ideco' で呼ぶため。")

(provide 'my-ideco)
;;; my-ideco.el ends here
