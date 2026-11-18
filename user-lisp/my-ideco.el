;;; my-ideco.el --- iDeCo の資産状況を ideco_server の API で見る  -*- lexical-binding: t -*-
;;; Commentary:
;; 毎朝スクレイピングして PostgreSQL に溜めている iDeCo の資産状況を
;; 罫線の表にして見る。`M-x check-ideco'。表は 2 つ。
;;
;; - 直近 10 日分の推移
;; - 最新の日の銘柄別状況
;;
;; **DB は直接引かない。** ideco_server (FastAPI) の
;; `GET /api/v1/ideco/status?days=N' が返す JSON を表にする。つなぎ先は
;; `my:ideco-api-base-url'。以前は psql を起こして SQL を 3 本流していたが、
;; 2026-11-17 に API 経由にした。おかげでパスワード (auth-source) も
;; WSL の psql も CSV のパースも要らなくなっている。
;;
;; 見出しの時刻は最新の行を取り込んだ時刻。**JST への変換は API 側**
;; (DB には UTC で入っている)。
;;
;;   ┌────────────┬────────────┬────────────┬────────────┬────────────┐
;;   │ 日付       │   資産残高 │ 拠出金累計 │       損益 │     前日比 │
;;   ├────────────┼────────────┼────────────┼────────────┼────────────┤
;;   │ 2026-11-10 │ 12,914,906 │ 10,265,130 │  2,649,776 │ ↑   83,556 │
;;   │ 2026-11-09 │ 12,831,350 │ 10,265,130 │  2,566,220 │ →        0 │
;;
;; 前日比は損益の「1 つ前の日」との差で、↑ は赤・↓ は緑・→ (変化なし) は灰色
;; (国内の証券会社の慣習)。資産残高で比べないのは、拠出日に相場が下がっても
;; 拠出の分だけ増えて ↑ になるため (実測で 2026-09-15 は資産残高 +7,255 /
;; 損益 -15,745)。**差を出すのは API の仕事** (`soneki_diff')。
;;
;; 同期で 1 回 GET するだけ (`my:ideco-api-timeout' 秒で諦める)。
;; 見終わったら `q' でバッファごと消える。`g' で取り直す。
;;
;; **API キーが要る** (`my:ideco-api-key')。既定では auth-source から引くので、
;; 鍵はこのリポジトリの外 (`~/.authinfo' か `~/.authinfo.gpg') に置く。
;; つなぎ先 (`my:ideco-api-base-url') も鍵もマシン固有なので `local.el' に書く
;; (このリポジトリは公開しているため)。
;;
;;; Code:

(require 'seq)
(require 'subr-x)

;; url は `my:ideco--fetch' でだけ要るので、トップレベルでは読まない
;; (my-htnblog / my-translate と同じ。CLAUDE.md §1「`eval-when-compile' は
;; 読み込み時に走らないことの保証にならない」。宣言だけ置く)。
(defvar url-http-response-status)
;; `let' で縛るので、url-vars が読まれる前でも special である必要がある
;; (lexical-binding では、束縛の種類は評価時に決まる)。
(defvar url-request-extra-headers)
(defvar url-debug)
(declare-function url-retrieve-synchronously "url"
                  (url &optional silent inhibit-cookies timeout))
(declare-function url-generic-parse-url "url-parse" (url))
(declare-function url-host "url-parse" (url))
(declare-function auth-source-search "auth-source" (&rest spec))

(defgroup my:ideco nil
  "iDeCo の資産状況を見る。"
  :group 'tools
  :prefix "my:ideco-")

;;; [1] 設定

(defcustom my:ideco-api-base-url nil
  "ideco_server の URL (スキームとホストまで)。

**設定しないと使えない。** `local.el' に書く (git 管理外)。

ideco_server のコンテナは taigei の `127.0.0.1:9001' にしか bind して
いないので、LAN から平文で直接は届かない。tailscale serve 経由の HTTPS
 (`https://HOST.tailXXXXXX.ts.net:8443') を書く。以前の mDNS 名
 (`http://taigei.local:9001') はもう通らない。

**tailnet のホスト名を既定値に書き戻さない。** このリポジトリは公開して
いる (§1「マシン固有の値は `custom.el' ではなく `local.el' に書く」)。"
  :type '(choice (const :tag "未設定" nil) (string :tag "URL")))

(defcustom my:ideco-api-key nil
  "ideco_server の `X-Api-Key' に送る鍵。

nil なら auth-source から引く (既定)。`auth-sources' の順に
`~/.authinfo' / `~/.authinfo.gpg' を見て、`my:ideco-api-base-url' の
ホストと `my:ideco-api-key-user' の login が一致する行の password を使う。

  machine HOST.tailXXXXXX.ts.net login ideco password <鍵>

**鍵をこのリポジトリの中に置かない。** 文字列で直接書くなら `local.el'
 (git 管理外)。`custom.el' は git 管理下なので書かない。

関数を渡すと、呼んだ戻り値を鍵として使う
 (例: (lambda () (getenv \"IDECO_API_KEY\")))。"
  :type '(choice (const :tag "auth-source から引く" nil)
                 (string :tag "鍵そのもの")
                 (function :tag "鍵を返す関数")))

(defcustom my:ideco-api-key-user "ideco"
  "auth-source で鍵を引くときの login (authinfo の `login')。"
  :type 'string)

(defcustom my:ideco-api-timeout 5
  "API の応答を何秒待つか。

同期で取るので、ここを長くすると**待っている間 Emacs が止まる**。
サーバが落ちていたら待たずに諦めてほしいので短くしてある。"
  :type 'number)

(defcustom my:ideco-rows 10
  "既定で何行出すか。1 日 1 行なので、既定は直近 10 日ぶん。
前置引数で変えられる (`C-u 30 M-x check-ideco')。

API 側の上限は 365 (それを超えると 422 が返る)。"
  :type 'integer)

(defcustom my:ideco-buffer-name "*ideco*"
  "表を出すバッファの名前。"
  :type 'string)

(defconst my:ideco--status-path "/api/v1/ideco/status"
  "資産状況を返す API のパス。")

(defconst my:ideco--columns
  '((summary_date       "日付"       left  my:ideco--format-date)
    (shisan_zandaka     "資産残高"   right my:ideco--format-number)
    (kyoshutukin_ruikei "拠出金累計" right my:ideco--format-number)
    (soneki             "損益"       right my:ideco--format-number))
  "(JSON のキー 見出し 寄せ 整形関数) の並び。

**表はここから作る**ので、見る列を変えるならここ 1 か所でよい
 (キーは API の `summaries' の要素のキー)。`created_at' は見出しに出すので
表には入れない。")

(defconst my:ideco--null "-"
  "値が無いときに出す文字 (JSON の null と空文字)。")

(defconst my:ideco--diff-column 'soneki
  "前日比を出す列 (`my:ideco--columns' のキー)。
表ではこの列の直後に「前日比」の列を挟む。

値そのものは API が `soneki_diff' で返す。資産残高ではなく損益で
比べる理由は Commentary に書いてある。")

;; 色は国内の証券会社の慣習に合わせて「上昇＝赤、下落＝緑」。
(defface my:ideco-up '((t :inherit error :weight normal))
  "前日より増えた行の前日比 (赤)。")

(defface my:ideco-down '((t :inherit success :weight normal))
  "前日より減った行の前日比 (緑)。")

(defface my:ideco-flat '((t :inherit shadow))
  "前日と同じ行の前日比 (土日など)。")

(defface my:ideco-title
  '((t :inherit (bold font-lock-function-name-face) :height 1.4))
  "バッファ先頭の見出し (「iDeCo 資産状況」)。")

(defface my:ideco-heading
  '((t :inherit (bold font-lock-type-face) :height 1.15))
  "表ごとの見出し (「直近 10 日分の推移」「銘柄別状況」)。")

(defcustom my:ideco-normalize-item-names t
  "非 nil なら銘柄名の全角英数と全角空白を半角にする (NFKC + NFC)。
DB には `ｅＭＡＸＩＳ　Ｓｌｉｍ　国内株式（ＴＯＰＩＸ）' と全角で入っていて、
そのままだと銘柄名の列だけで 46 桁ある。NFKC で
`eMAXIS Slim 国内株式(TOPIX)' (28 桁) になる。カタカナは変わらない。"
  :type 'boolean)

;;; [2] 値の整形

(defun my:ideco--string (value)
  "JSON の VALUE を表に出す文字列にする。

整形関数 (`my:ideco--columns' の 4 番目) は文字列を受けるので、数も
nil もここで文字列に揃える。JSON の null は nil で来るので空文字にする
 (`my:ideco--cell' が `my:ideco--null' に直す)。

小数は、小数部が無ければ整数として書く。API は表に出す列を整数で
返すのでここは保険 (DB の `soneki' は Double)。"
  (cond ((null value) "")
        ((integerp value) (number-to-string value))
        ((floatp value)
         (let ((i (round value)))
           (if (= i value) (number-to-string i) (number-to-string value))))
        ((stringp value) value)
        (t (format "%s" value))))

(defun my:ideco--format-date (s)
  "YYYYMMDD を YYYY-MM-DD にする。想定外の形なら素通し。

`summary_date' は API でも文字列 (DB の列が `character(8)')。
並び順は YYYYMMDD の辞書順 = 日付順。"
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

(defun my:ideco--format-timestamp (s)
  "API の `created_at' (ISO 8601) を `YYYY/MM/DD HH:MM:SS' にする。
読めない形なら素通し。nil なら nil。

**時刻の変換はしない。並べ替えるだけ。** API が JST (`+09:00') で返す。
`date-to-time' + `format-time-string' にゾーン `Asia/Tokyo' を渡す手も
あるが、**Windows には tz database が無い**のでマシンによって狂う。"
  (if (and (stringp s)
           (string-match
            "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
            s))
      (format "%s/%s/%s %s"
              (match-string 1 s) (match-string 2 s) (match-string 3 s)
              (match-string 4 s))
    s))

(defun my:ideco--cell (row spec)
  "ROW (1 行ぶんの alist) から列 SPEC のセルを作る。
値が無ければ `my:ideco--null'。"
  (let ((s (string-trim (my:ideco--string (alist-get (nth 0 spec) row)))))
    (if (string-empty-p s)
        my:ideco--null
      (funcall (nth 3 spec) s))))

;;; [2.5] 前日比

(defun my:ideco--diff-index ()
  "`my:ideco--diff-column' が `my:ideco--columns' の何番目か。"
  (seq-position (mapcar #'car my:ideco--columns) my:ideco--diff-column))

(defun my:ideco--with-diff (list value)
  "LIST の前日比を挟む位置に VALUE を差し込んだ新しいリストを返す。
見出し・寄せ・各行のセルのどれにも同じ位置で差し込むために使う。"
  (let ((i (1+ (my:ideco--diff-index))))
    (append (seq-take list i) (list value) (seq-drop list i))))

(defun my:ideco--delta-cells (deltas)
  "DELTAS (整数か nil) を前日比のセルにする。nil は `my:ideco--null'。

矢印はセルの左端に揃え、数字はその右で右詰めにする。セル全体を右詰めに
すると、数字の桁数しだいで矢印の位置が行ごとにずれて縦に追えない。
数字の幅は DELTAS 全体で揃える。符号は付けない (矢印と色で足りる)。
推移の表と銘柄別の表で同じ表記にするため、両方ともここを通す。"
  (let* ((texts (mapcar (lambda (d)
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

(defun my:ideco--diffs (rows)
  "ROWS (alist の並び) の `soneki_diff' を取り出してセルにする。

**差は API が計算して返す。** psql を引いていたころは「表に出す行数より
1 行多く取って自分で引く」をやっていたが、API は `lag()' で出すので
いちばん古い行にも本当の前日比が入っている。"
  (my:ideco--delta-cells
   (mapcar (lambda (r) (alist-get 'soneki_diff r)) rows)))

;;; [3] 罫線の表

(defun my:ideco--pad (s width align)
  "S を WIDTH 桁に詰める。ALIGN が `right' なら右詰め。
【重要】桁は `string-width' で数える (`length' ではない)。"
  (let ((gap (max 0 (- width (string-width s)))))
    (if (eq align 'right)
        (concat (make-string gap ?\s) s)
      (concat s (make-string gap ?\s)))))

(defun my:ideco--table (header rows aligns &optional footer)
  "HEADER と ROWS を罫線の表にした文字列にする。ALIGNS は列ごとの寄せ方。
FOOTER (合計行など) を渡すと、横罫で区切って最後に太字で置く。

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
                                            (if footer (cons footer rows) rows)))))
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
            (when footer
              (concat (funcall rule "├" "┼" "┤")
                      (funcall line footer 'bold)))
            (funcall rule "└" "┴" "┘"))))

;;; [4] API

(defun my:ideco--base-url ()
  "つなぎ先の URL (末尾の / を落としたもの)。未設定なら教えて止まる。"
  (let ((url (and (stringp my:ideco-api-base-url)
                  (string-trim my:ideco-api-base-url))))
    (if (and url (not (string-empty-p url)))
        (string-trim-right url "/+")
      (user-error "`my:ideco-api-base-url' が未設定。\
local.el に tailscale serve の URL を書く (雛形は local.el.sample)"))))

(defun my:ideco--host ()
  "つなぎ先のホスト名。auth-source の `machine' に使う。"
  (require 'url-parse)
  (url-host (url-generic-parse-url (my:ideco--base-url))))

(defun my:ideco--auth-source-key ()
  "auth-source から API キーを引く。見つからなければ nil。

`:secret' は関数で返ることがある (暗号化された `~/.authinfo.gpg' など)
ので、関数なら呼んでから返す。"
  (require 'auth-source)
  (when-let* ((entry (car (auth-source-search :host (my:ideco--host)
                                              :user my:ideco-api-key-user
                                              :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun my:ideco--api-key ()
  "`X-Api-Key' に送る鍵を返す。見つからなければ教えて止まる。

`my:ideco-api-key' が文字列ならそれ、関数なら呼んだ戻り値、nil なら
auth-source から引く。"
  (let ((key (cond ((functionp my:ideco-api-key) (funcall my:ideco-api-key))
                   ((stringp my:ideco-api-key) my:ideco-api-key)
                   (t (my:ideco--auth-source-key)))))
    (if (and (stringp key) (not (string-empty-p key)))
        key
      (user-error "ideco_server の API キーが無い。\
~/.authinfo に `machine %s login %s password <鍵>' を書くか、\
local.el で `my:ideco-api-key' を設定する"
                  (my:ideco--host) my:ideco-api-key-user))))

(defun my:ideco--status-url (days)
  "資産状況の API の URL。DAYS 日分を頼む。"
  (format "%s%s?days=%d" (my:ideco--base-url) my:ideco--status-path days))

(defun my:ideco--response-body ()
  "応答バッファの本文を UTF-8 で復号して返す (my-translate と同じ)。

ヘッダと本文の境界は `\\r\\n\\r\\n' だが、`\\n\\n' のサーバもあるので両方見る。"
  (save-excursion
    (goto-char (point-min))
    (let ((start (or (save-excursion
                       (and (search-forward "\r\n\r\n" nil t) (point)))
                     (save-excursion
                       (and (search-forward "\n\n" nil t) (point))))))
      (if start
          (decode-coding-string
           (buffer-substring-no-properties start (point-max)) 'utf-8)
        ""))))

(defun my:ideco--fetch (days)
  "API から資産状況を取って alist で返す。DAYS 日分の推移を頼む。

同期で 1 回 GET する (`url-retrieve-synchronously' の TIMEOUT を渡すので、
サーバが黙っていても `my:ideco-api-timeout' 秒で諦める。渡さないと
**応答が来るまで Emacs が止まる**)。

【重要】HTTP のステータスを先に見る。ideco_server は入力が不正なとき
**空の本文で 422 を返す** (`main.py' の例外ハンドラ) ので、本文だけ見ると
「JSON が読めない」としか言えない。

JSON は alist / リストで受ける。`:array-type' を既定 (vector) のままに
すると、表を組む `my:ideco--table' の `(cons footer rows)' が improper list
になって `mapconcat' が通らない。"
  (require 'url)
  (let* ((url (my:ideco--status-url days))
         ;; 【重要】鍵を url の内部ログに残さない (my-htnblog と同じ)。
         (url-debug nil)
         (url-request-extra-headers
          (list (cons "X-Api-Key" (my:ideco--api-key))))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t nil my:ideco-api-timeout)
                   (error
                    (user-error "ideco_server に繋がらない (%s): %s"
                                url (error-message-string err))))))
    (unless buffer
      (user-error "ideco_server が %s 秒で応答しない (%s)"
                  my:ideco-api-timeout url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (and (boundp 'url-http-response-status)
                             url-http-response-status))
                (body (my:ideco--response-body)))
            (unless (integerp status)
              ;; 接続を断られたときは `url-retrieve-synchronously' がエラーを
              ;; 出さずに、ステータスの無いバッファを返してくる (実測。
              ;; サーバが起動していないときがこれ)。
              (user-error "ideco_server に繋がらない (%s)" url))
            (unless (and (<= 200 status) (< status 300))
              ;; 認証まわりは「繋がらない」と区別がつかないと原因を探せないので
              ;; 名指しで出す。どちらも取り直しては直らない。
              (cond
               ;; サーバは 403 を返す。401 だと `url' が
               ;; `url-http-handle-authentication' でユーザー名を聞きに行き、
               ;; 「応答が無い」と区別がつかなくなる (実測でプロンプトに入る)。
               ((memq status '(401 403))
                (user-error "ideco_server に API キーを拒否された (%s)。\
~/.authinfo の machine %s login %s の password を確かめる"
                            status (my:ideco--host) my:ideco-api-key-user))
               ((= status 503)
                (user-error "ideco_server 側で API キーが設定されていない (503)。\
サーバの .env と compose.yml を見る (%s)" url))
               (t
                (user-error "ideco_server が %s を返した (%s)%s"
                            status url
                            (if (string-empty-p (string-trim body))
                                ""
                              (format ": %s" (string-trim body)))))))
            (condition-case err
                (json-parse-string body
                                   :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)
              (error (user-error "ideco_server の応答を読めない (%s): %s"
                                 url (error-message-string err))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; [4.5] 銘柄別

(defun my:ideco--item-name (s)
  "銘柄名 S を表に出す形にする (`my:ideco-normalize-item-names')。

【重要】NFKC のあとに NFC をかける。DB の銘柄名には**半角の濁点**
 (U+FF9E) が入っていて、`ucs-normalize-NFKC-string' はこれを結合文字の
濁点 (U+3099) にするだけで `テ' と合成してくれない (実測。50 文字のまま
U+3099 が残る)。結合文字が残ると**桁の数え方が 2 通りになる**:
`char-width' の総和は 73 なのに、合成を見る `string-width' は 71 を返す
 (しかもバッファによって違う。display table のあるバッファでは 73)。
表は `string-width' で桁を数えるので、濁点を含む行だけ 2 桁短くなって
罫線が揃わなくなる (実測で 122 桁の表の 3 行が 120 桁)。NFC を通すと
`デ' (U+30C7) に合成され、どの数え方でも 71 で一致する。"
  (let ((s (string-trim (or s ""))))
    (if my:ideco-normalize-item-names
        (progn (require 'ucs-normalize)
               (ucs-normalize-NFC-string (ucs-normalize-NFKC-string s)))
      s)))

(defun my:ideco--items-table (items)
  "ITEMS (API の `items') を表の文字列にする。

前日比は推移の表と同じく**損益**の差で、表記も同じ
 (`my:ideco--delta-cells')。前の日に無かった銘柄は API が null を返すので
空欄になる。

【重要】合計行には資産残高しか出さない。銘柄の損益は「資産残高 − 購入
金額」で、日次サマリの損益 (「資産残高 − 拠出金累計」) とは基準が違う。
足しても推移の表の損益と合わず (2026-11-10 で 2,704,054 対 2,649,776)、
同じ画面に 2 つの「損益の合計」が並ぶことになる。資産残高は一致する。
運営管理機関の画面も合計は資産残高だけ。"
  (let* ((fmt (lambda (v)
                (let ((s (my:ideco--string v)))
                  (if (string-empty-p s)
                      my:ideco--null
                    (my:ideco--format-number s)))))
         (zan (mapcar (lambda (r) (alist-get 'shisan_zandaka r)) items))
         (diffs (my:ideco--diffs items)))
    (my:ideco--table
     '("運用商品名" "資産残高" "損益" "前日比")
     (seq-mapn (lambda (r z d)
                 (list (my:ideco--item-name (alist-get 'unyo_shohin_name r))
                       (funcall fmt z)
                       (funcall fmt (alist-get 'soneki r))
                       d))
               items zan diffs)
     '(left right right right)
     (list "合計"
           (funcall fmt (and (seq-every-p #'numberp zan) (apply #'+ zan)))
           "" ""))))

;;; [5] バッファ

(defun my:ideco--insert-heading (text face)
  "TEXT を見出しとして FACE で挿入する (前後の空行は呼ぶ側)。"
  (insert (propertize text 'face face) "\n"))

(defvar-local my:ideco--rows nil
  "そのバッファを作ったときの行数。`g' で取り直すときに使う。")

(define-derived-mode my:ideco-mode special-mode "iDeCo"
  "iDeCo の資産状況を見るモード。
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
  "iDeCo の資産状況を出す。直近 ROWS 日の推移と、最新の日の銘柄別。

ROWS の既定は `my:ideco-rows'。前置引数で行数を指定できる
 (`C-u 30 M-x check-ideco')。`M-x check-ideco' はこれの別名。

値は ideco_server の API から取る (`my:ideco-api-base-url')。"
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       my:ideco-rows)))
  (let* ((rows (max 1 (or rows my:ideco-rows)))
         (status (my:ideco--fetch rows))
         (stamp (my:ideco--format-timestamp (alist-get 'created_at status)))
         (latest (alist-get 'summary_date status))
         (data (alist-get 'summaries status))
         (items (alist-get 'items status))
         (diffs (my:ideco--diffs data))
         (cells (seq-mapn (lambda (row diff)
                            (my:ideco--with-diff
                             (mapcar (lambda (spec) (my:ideco--cell row spec))
                                     my:ideco--columns)
                             diff))
                          data diffs))
         (buf (get-buffer-create my:ideco-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'my:ideco-mode)
          (my:ideco-mode))
        (erase-buffer)
        (my:ideco--insert-heading
         (concat "iDeCo 資産状況"
                 (if stamp (format " (%s 時点)" stamp) ""))
         'my:ideco-title)
        (insert "\n")
        (my:ideco--insert-heading (format "直近 %d 日分の推移" rows)
                                  'my:ideco-heading)
        (if (null cells)
            (insert "該当する行が無い。\n")
          (insert (my:ideco--table
                   (my:ideco--with-diff (mapcar #'cadr my:ideco--columns)
                                        "前日比")
                   cells
                   (my:ideco--with-diff (mapcar #'caddr my:ideco--columns)
                                        'right))))
        (insert "\n")
        (my:ideco--insert-heading
         (concat "銘柄別状況"
                 (if latest
                     (format " (%s)" (my:ideco--format-date latest))
                   ""))
         'my:ideco-heading)
        (insert (if items
                    (my:ideco--items-table items)
                  "該当する行が無い。\n"))
        (insert "\n")
        (insert (propertize
                 (format "%s / %d 行 (%s 取得)\n"
                         (my:ideco--status-url rows)
                         (length cells)
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 'face 'shadow))
        (goto-char (point-min)))
      ;; 【重要】`define-derived-mode' は `kill-all-local-variables' を通る
      ;; ので、バッファローカルの設定はモードを立てた**後**に置く。
      (setq-local my:ideco--rows rows)
      (setq-local revert-buffer-function #'my:ideco--revert))
    (pop-to-buffer buf)
    (message "iDeCo: %d 行" (length cells))))

;;;###autoload
(defalias 'check-ideco #'my:ideco-check
  "`my:ideco-check' の別名。`M-x check-ideco' で呼ぶため。")

(provide 'my-ideco)
;;; my-ideco.el ends here
