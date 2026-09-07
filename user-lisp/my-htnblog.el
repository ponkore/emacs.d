;;; my-htnblog.el --- はてなブログへ投稿する  -*- lexical-binding: t -*-
;;; Commentary:
;; はてなブログ AtomPub API へ直接投稿する。外部コマンドは要らない。
;;
;; 毎日 1 記事、カテゴリー・タイトル・本文 1 行目が決まっているので、
;; M-x htnblog でそれをプリセットしたバッファを開き、本文を書いて C-c C-c
;; すると公開される。
;;
;;   ```header
;;   Category: 体調管理
;;   Title: 9月7日(日)の記録
;;   ```
;;
;;   * 9月7日(日)
;;   ここに本文を書く
;;
;; ヘッダの書式は htnblog コマンド (https://github.com/hymkor/htnblog-go) の
;; new と同じにしてある。あちらで書いた下書きをそのまま貼っても通る。
;; ヘッダは編集してよい。Category 行は複数書ける。`Draft: yes' を足すと
;; 公開せず下書きとして保存する。
;;
;; 認証は ~/.htnblog (htnblog コマンドと同じファイル) の userid / apikey に
;; よる Basic 認証。WSSE も OAuth も要らない。投稿は endpointurl + "/entry" へ
;; Atom の entry を POST するだけで、app:draft を "no" にすれば最初から公開
;; 状態で投稿できる (htnblog コマンドの new → publish が 2 手なのは、あちらの
;; new が下書き固定だからで、API の制約ではない)。
;;
;; HTTP は組み込みの url.el で行う。子プロセスを起こさないので、この設定の
;; 持病である「Windows の call-process が遅い」(CLAUDE.md) とは無関係。
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'xml)

;; url-http.el の (defvar url-http-response-status) は値を伴わないため、
;; special 宣言がそのファイルの中でしか効かない。こちらでも宣言しておく。
(defvar url-http-response-status)

;;; --------------------------------------------------
;;; カスタマイズ
;;; --------------------------------------------------

(defgroup my:htnblog nil
  "はてなブログ AtomPub API への投稿."
  :group 'tools
  :prefix "my:htnblog-")

(defcustom my:htnblog-config-file "~/.htnblog"
  "認証情報の JSON ファイル。
htnblog コマンドが使うものと同じ。userid / endpointurl / apikey を読む。"
  :type 'file
  :group 'my:htnblog)

(defcustom my:htnblog-category "体調管理"
  "新しい記事に付けるカテゴリー。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-title-format "%sの記録"
  "タイトルの書式。%s に `my:htnblog--date-string' の日付が入る。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-body-format "* %s"
  "本文 1 行目の書式。%s に `my:htnblog--date-string' の日付が入る。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-header-start "```header"
  "ヘッダの開始行。htnblog コマンドの -header-start に対応する。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-header-end "```"
  "ヘッダの終了行。htnblog コマンドの -header-end に対応する。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-buffer-name "*htnblog*"
  "記事を書くバッファの名前。"
  :type 'string
  :group 'my:htnblog)

(defcustom my:htnblog-timeout 30
  "サーバの応答を待つ秒数。"
  :type 'integer
  :group 'my:htnblog)

(defcustom my:htnblog-draft nil
  "非 nil なら既定で下書きとして投稿する。
ヘッダに `Draft: yes' / `Draft: no' と書けば記事ごとに上書きできる。"
  :type 'boolean
  :group 'my:htnblog)

(defcustom my:htnblog-show-previous t
  "非 nil なら、ひな形の後ろに最新の公開記事を参考として貼る。
貼った部分は読み取り専用で、投稿には含まれない。
記事一覧を GET する (実測 0.3 秒) ので、その待ちが惜しければ nil にする。"
  :type 'boolean
  :group 'my:htnblog)

(defcustom my:htnblog-previous-separator "--- 前日分 ---"
  "最新の公開記事を貼るときの区切り行。
`my:htnblog--parse' はこの行から後ろを本文に含めない。行頭で一致を見る。"
  :type 'string
  :group 'my:htnblog)

(defface my:htnblog-previous-face
  '((t :inherit shadow))
  "貼り付けた最新の公開記事に使う face。
本文と見分けが付くよう薄く出す。"
  :group 'my:htnblog)

;;; --------------------------------------------------
;;; 設定ファイル
;;; --------------------------------------------------

(defun my:htnblog--config ()
  "`my:htnblog-config-file' を読んで alist で返す。
必須のキーが欠けていればこの時点で `user-error' にする。"
  (let ((file (expand-file-name my:htnblog-config-file)))
    (unless (file-readable-p file)
      (user-error "htnblog: 設定ファイルが読めない: %s" file))
    (let ((config
           (condition-case err
               (json-parse-string
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8))
                    (insert-file-contents file))
                  (buffer-string))
                :object-type 'alist :null-object nil :false-object nil)
             (error
              (user-error "htnblog: %s を解析できない: %s"
                          file (error-message-string err))))))
      (dolist (key '(userid endpointurl apikey))
        (let ((value (alist-get key config)))
          (when (or (not (stringp value)) (string-empty-p value))
            (user-error "htnblog: %s に %s が無い" file key))))
      config)))

;;; --------------------------------------------------
;;; ひな形
;;; --------------------------------------------------

(defconst my:htnblog--day-names ["日" "月" "火" "水" "木" "金" "土"]
  "曜日の名前。
`format-time-string' の %a は `system-time-locale' 次第で英語になるので、
ロケールに依存しないよう自前で持つ。")

(defun my:htnblog--date-string (&optional time)
  "TIME (既定は今日) を \"9月7日(日)\" の形で返す。"
  (let ((decoded (decode-time (or time (current-time)))))
    (format "%d月%d日(%s)"
            (decoded-time-month decoded)
            (decoded-time-day decoded)
            (aref my:htnblog--day-names (decoded-time-weekday decoded)))))

(defun my:htnblog--template (&optional time)
  "TIME の日付でひな形の文字列を作る。"
  (let ((date (my:htnblog--date-string time)))
    (concat my:htnblog-header-start "\n"
            "Category: " my:htnblog-category "\n"
            "Title: " (format my:htnblog-title-format date) "\n"
            my:htnblog-header-end "\n"
            "\n"
            (format my:htnblog-body-format date) "\n"
            "\n")))

;;; --------------------------------------------------
;;; バッファの解析
;;; --------------------------------------------------

(defun my:htnblog--body-end (body-start)
  "本文の終わりの位置を返す。
参考として貼った最新記事 (`my:htnblog--insert-previous') を投稿に含めない
ための境目。貼った部分にはテキストプロパティが付いているので、区切りの
文字列に頼るより確実。プロパティが無ければ (手で区切り行を書いた場合など)
`my:htnblog-previous-separator' の行を探す。"
  (or (let ((pos (next-single-property-change body-start 'my:htnblog-previous)))
        (and pos (get-text-property pos 'my:htnblog-previous) pos))
      (save-excursion
        (goto-char body-start)
        (if (re-search-forward
             (concat "^" (regexp-quote my:htnblog-previous-separator)) nil t)
            (match-beginning 0)
          (point-max)))))

(defun my:htnblog--parse ()
  "現在のバッファをヘッダと本文に分けて plist で返す。
キーは :title :categories :body :draft。ヘッダが無ければ全体を本文とみなす。"
  (save-excursion
    (goto-char (point-min))
    (let ((fields nil)
          (body-start (point-min)))
      (when (looking-at-p (concat "[ \t]*" (regexp-quote my:htnblog-header-start)
                                  "[ \t]*$"))
        (forward-line 1)
        (let ((closed nil))
          (while (and (not closed) (not (eobp)))
            (cond
             ((looking-at-p (concat "[ \t]*" (regexp-quote my:htnblog-header-end)
                                    "[ \t]*$"))
              (forward-line 1)
              (setq closed t))
             ;; "Key: value" の行だけを拾う。htnblog コマンドが出す
             ;; "Rem: EndPointUrl: ..." もここに入るが、使わないので害はない。
             ((looking-at "[ \t]*\\([A-Za-z]+\\)[ \t]*:[ \t]*\\(.*?\\)[ \t]*$")
              (push (cons (downcase (match-string-no-properties 1))
                          (match-string-no-properties 2))
                    fields)
              (forward-line 1))
             (t (forward-line 1))))
          (unless closed
            (user-error "htnblog: ヘッダが %s で閉じていない" my:htnblog-header-end)))
        (setq body-start (point)))
      (setq fields (nreverse fields))
      (let ((draft-field (cdr (assoc "draft" fields))))
        (list :title (string-trim (or (cdr (assoc "title" fields)) ""))
              :categories (seq-remove #'string-empty-p
                                      (mapcar (lambda (field) (string-trim (cdr field)))
                                              (seq-filter (lambda (field)
                                                            (equal (car field) "category"))
                                                          fields)))
              :body (string-trim (buffer-substring-no-properties
                                  body-start (my:htnblog--body-end body-start)))
              :draft (if draft-field
                         (and (member (downcase (string-trim draft-field))
                                      '("yes" "true" "t" "1"))
                              t)
                       my:htnblog-draft))))))

;;; --------------------------------------------------
;;; Atom entry の組み立て
;;; --------------------------------------------------

(defun my:htnblog--cdata (string)
  "STRING を CDATA セクションに入れられる形にする。
中に \"]]>\" があるとそこでセクションが閉じてしまうので、分割して繋ぎ直す。"
  (replace-regexp-in-string "]]>" "]]]]><![CDATA[>" string t t))

(defun my:htnblog--entry-xml (title body categories draft)
  "投稿する Atom entry の XML を文字列で返す。"
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   "<entry xmlns=\"http://www.w3.org/2005/Atom\"\n"
   "       xmlns:app=\"http://www.w3.org/2007/app\">\n"
   "  <title>" (xml-escape-string title) "</title>\n"
   "  <content type=\"text/plain\"><![CDATA[" (my:htnblog--cdata body) "]]></content>\n"
   (mapconcat (lambda (category)
                (concat "  <category term=\"" (xml-escape-string category) "\" />\n"))
              categories "")
   "  <app:control><app:draft>" (if draft "yes" "no") "</app:draft></app:control>\n"
   "</entry>\n"))

(defun my:htnblog--node-text (node)
  "XML の NODE が持つテキストを連結して返す。
CDATA は `xml-parse-region' が複数のノードに分けて返すことがあるので、
文字列の子だけを集めて繋ぐ。"
  (and node (apply #'concat (seq-filter #'stringp (xml-node-children node)))))

(defun my:htnblog--entry-url (xml)
  "応答の XML から記事の URL (link rel=\"alternate\") を取り出す。
取れなければ nil。表示に使うだけなので、失敗しても投稿の成否には関わらない。"
  (condition-case nil
      (let ((entry (with-temp-buffer
                     (insert xml)
                     (car (xml-parse-region (point-min) (point-max))))))
        (seq-some (lambda (link)
                    (let ((attrs (xml-node-attributes link)))
                      (and (equal (cdr (assq 'rel attrs)) "alternate")
                           (cdr (assq 'href attrs)))))
                  (xml-get-children entry 'link)))
    (error nil)))

;;; --------------------------------------------------
;;; HTTP
;;; --------------------------------------------------

(defun my:htnblog--response-body ()
  "応答バッファの本文を UTF-8 で復号して返す。"
  (save-excursion
    (goto-char (point-min))
    (let ((body-start
           (or (save-excursion (and (search-forward "\r\n\r\n" nil t) (point)))
               (save-excursion (and (search-forward "\n\n" nil t) (point))))))
      (if body-start
          (decode-coding-string
           (buffer-substring-no-properties body-start (point-max))
           'utf-8)
        ""))))

(defun my:htnblog--show-error (status body)
  "失敗した応答を *htnblog-error* に出す。"
  (let ((buffer (get-buffer-create "*htnblog-error*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "HTTP %s\n\n%s" (or status "?") body))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buffer)))

(defun my:htnblog--auth-headers (config)
  "CONFIG から `url-request-extra-headers' に渡すヘッダを作る。"
  (list (cons "Authorization"
              (concat "Basic "
                      ;; 第 2 引数 NO-LINE-BREAK は必須。改行が入ると
                      ;; ヘッダが壊れる。
                      (base64-encode-string
                       (encode-coding-string
                        (concat (alist-get 'userid config) ":"
                                (alist-get 'apikey config))
                        'utf-8)
                       t)))
        (cons "Content-Type"
              "application/atom+xml; type=entry; charset=utf-8")))

(defun my:htnblog--request (config method url body)
  "CONFIG の認証情報で URL に METHOD で BODY を送り、応答の本文を返す。"
  (let* ((url-debug nil)                ; apikey をログに残さない
         (url-request-method method)
         (url-request-extra-headers (my:htnblog--auth-headers config))
         ;; url-request-data は unibyte でなければならない。ここを通さないと
         ;; 日本語がそのまま化ける。
         (url-request-data (and body (encode-coding-string body 'utf-8)))
         (buffer (url-retrieve-synchronously url t t my:htnblog-timeout)))
    (unless buffer
      (user-error "htnblog: 応答がない (%s)" url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status url-http-response-status)
                (text (my:htnblog--response-body)))
            (unless (and (integerp status) (<= 200 status) (< status 300))
              (my:htnblog--show-error status text)
              (user-error "htnblog: 失敗した (HTTP %s)。詳細は *htnblog-error*"
                          (or status "?")))
            text))
      (kill-buffer buffer))))

(defun my:htnblog--post-url (config)
  "記事を POST する URL を返す。"
  (concat (string-trim-right (alist-get 'endpointurl config) "/+") "/entry"))

;;; --------------------------------------------------
;;; 最新の公開記事
;;; --------------------------------------------------

(defun my:htnblog--entry-draft-p (entry)
  "ENTRY (XML ノード) が下書きなら非 nil。"
  (equal "yes"
         (my:htnblog--node-text
          (car (xml-get-children
                (car (xml-get-children entry 'app:control)) 'app:draft)))))

(defun my:htnblog--feed-latest-entry (response)
  "feed の XML RESPONSE から最新の公開記事を plist (:title :date :body) で返す。
下書きは飛ばす。取れなければ nil。"
  (condition-case nil
      (let* ((feed (with-temp-buffer
                     (insert response)
                     (car (xml-parse-region (point-min) (point-max)))))
             (entry (seq-find (lambda (e) (not (my:htnblog--entry-draft-p e)))
                              (xml-get-children feed 'entry))))
        (when entry
          (let ((published (my:htnblog--node-text
                            (car (xml-get-children entry 'published)))))
            (list :title (my:htnblog--node-text (car (xml-get-children entry 'title)))
                  ;; "2026-09-06T22:34:35+09:00" の日付の部分だけ
                  :date (and published (>= (length published) 10)
                             (substring published 0 10))
                  :body (my:htnblog--node-text
                         (car (xml-get-children entry 'content)))))))
    (error nil)))

(defvar-local my:htnblog--previous-token 0
  "参考記事の取得の世代。
`C-u' でひな形を入れ直すと進む。飛んで来た応答はこれが一致するときだけ貼る。
古いリクエストの結果が、入れ直した後のバッファに紛れ込まないようにするため。")

;; permanent-local が要る。`define-derived-mode' は kill-all-local-variables を
;; 通るので、これが無いと入れ直すたびにグローバル値の 0 に戻り、世代が常に 1 に
;; なる。古い応答まで一致してしまい、二重に貼って read-only に当たる (実測)。
(put 'my:htnblog--previous-token 'permanent-local t)

(defun my:htnblog--fetch-previous (config buffer token)
  "最新の公開記事を非同期に取って BUFFER の末尾に貼る。

同期にしない理由: サーバの応答は普段 0.1 秒だが、散発的に十数秒かかる
(GUI 実測で 15.4 秒。TCP 0.02 / TLS 0.06 / DNS 0.01 秒なので Emacs 側では
なくサーバ側の揺らぎ)。参考として貼るだけのものに `M-x htnblog' を
待たせる価値はない。"
  (let ((url-debug nil)                 ; apikey をログに残さない
        (url-request-method "GET")
        (url-request-extra-headers (my:htnblog--auth-headers config))
        (url-request-data nil))
    (url-retrieve
     (my:htnblog--post-url config)
     (lambda (status)
       (let ((response (and (not (plist-get status :error))
                            (my:htnblog--response-body))))
         (kill-buffer (current-buffer))
         (if (not response)
             (message "htnblog: 最新記事を取れなかった")
           (let ((entry (my:htnblog--feed-latest-entry response)))
             (when (and entry (buffer-live-p buffer))
               (with-current-buffer buffer
                 (when (eq my:htnblog--previous-token token)
                   ;; 書いている最中に届くので、undo と変更フラグを汚さない。
                   (let ((modified (buffer-modified-p))
                         (buffer-undo-list t))
                     (my:htnblog--insert-previous entry)
                     (set-buffer-modified-p modified)))))))))
     nil t t)))

(defun my:htnblog--insert-previous (entry)
  "ENTRY (`my:htnblog--feed-latest-entry' の戻り値) をバッファ末尾に貼る。
区切り行から後ろは読み取り専用にする。`my:htnblog--parse' がその境目を見て
本文から落とすので、投稿には含まれない。既に貼ってあれば何もしない。"
  ;; 二重に貼らない。貼った後にもう一度貼ろうとすると、point-max が
  ;; 読み取り専用の中なので挿入そのものが失敗する。
  (unless (text-property-any (point-min) (point-max) 'my:htnblog-previous t)
    (save-excursion
      ;; 自分で貼るぶんには読み取り専用を無視してよい。防ぎたいのは手入力。
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        ;; この改行は読み取り専用にしない。カーソルの初期位置 (本文の末尾) が
        ;; そのまま領域の先頭になると、front-sticky のせいで本文が書けなくなる。
        (insert "\n")
        (let ((start (point)))
          (insert my:htnblog-previous-separator
                  (let ((date (plist-get entry :date)))
                    (if date (concat " " date) ""))
                  (let ((title (plist-get entry :title)))
                    (if title (concat " " title) ""))
                  "\n\n"
                  (or (plist-get entry :body) "")
                  "\n")
          (add-text-properties
           start (point-max)
           ;; front-sticky が要る。これが無いと区切り行の行頭に 1 文字
           ;; 入れられてしまい、^区切り の一致が外れて前日分が丸ごと本文に
           ;; 混ざる (実測)。
           ;; rear-nonsticky は付けない。付けると read-only の文字の直後への
           ;; 挿入が継承されず素通りする。本文が書けるのは、上の改行を領域の
           ;; 外に置いてあるおかげ (領域の直前に read-only でない文字が
           ;; あるので、そこへの挿入は継承しない)。
           (list 'read-only t
                 'front-sticky '(read-only)
                 'my:htnblog-previous t
                 'font-lock-face 'my:htnblog-previous-face
                 'face 'my:htnblog-previous-face)))))))

;;; --------------------------------------------------
;;; コマンド
;;; --------------------------------------------------

(defun my:htnblog-post ()
  "バッファの内容をはてなブログへ投稿する。
送信前に一度だけ確認する。成功したらバッファを閉じ、記事の URL を
kill-ring に入れて echo area に出す。"
  (interactive)
  (unless (derived-mode-p 'my:htnblog-mode)
    (user-error "htnblog: %s のバッファではない" 'my:htnblog-mode))
  (let* ((config (my:htnblog--config))
         (parsed (my:htnblog--parse))
         (title (plist-get parsed :title))
         (body (plist-get parsed :body))
         (categories (plist-get parsed :categories))
         (draft (plist-get parsed :draft))
         (action (if draft "下書き保存" "公開")))
    (when (string-empty-p title)
      (user-error "htnblog: Title が空"))
    (when (string-empty-p body)
      (user-error "htnblog: 本文が空"))
    (unless (y-or-n-p (format "%s「%s」を%sします。よろしいですか? "
                              (if categories
                                  (format "[%s] " (string-join categories " "))
                                "")
                              title action))
      (user-error "htnblog: 中止した"))
    (let* ((response (my:htnblog--request
                      config "POST" (my:htnblog--post-url config)
                      (my:htnblog--entry-xml title body categories draft)))
           (entry-url (my:htnblog--entry-url response))
           (buffer (current-buffer)))
      (when entry-url
        (kill-new entry-url))
      (set-buffer-modified-p nil)
      (kill-buffer buffer)
      (message "htnblog: %sしました%s" action
               (if entry-url (concat ": " entry-url) "")))))

(defun my:htnblog-abort ()
  "書きかけを捨ててバッファを閉じる。"
  (interactive)
  (when (y-or-n-p "書きかけを捨てますか? ")
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defvar my:htnblog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my:htnblog-post)
    (define-key map (kbd "C-c C-k") #'my:htnblog-abort)
    map)
  "`my:htnblog-mode' のキーマップ。")

(define-derived-mode my:htnblog-mode text-mode "Htnblog"
  "はてなブログの記事を書くためのモード。

\\{my:htnblog-mode-map}"
  ;; 送信時に string-trim するので、末尾の改行を足す意味がない。
  (setq-local require-final-newline nil))

(defun my:htnblog (&optional arg)
  "はてなブログの記事を書くバッファを開く。
カテゴリー・タイトル・本文 1 行目は今日の日付でプリセットされる。
書き終えたら \\<my:htnblog-mode-map>\\[my:htnblog-post] で投稿する。

`my:htnblog-show-previous' が非 nil なら、後ろに最新の公開記事を参考として
貼る。区切り (`my:htnblog-previous-separator') から後ろは読み取り専用で、
投稿には含まれない。取得は非同期なので、バッファが先に開き、記事は少し
遅れて現れる (書き始めるのを待たせない)。

書きかけのバッファがあればそれに切り替える。ARG (\\[universal-argument]) を
付けると、中身を捨てて今日の日付でひな形を入れ直す。"
  (interactive "P")
  ;; 書き終えてから設定の不備に気づくと最悪なので、開く時点で検証しておく。
  (let ((config (my:htnblog--config))
        (buffer (get-buffer my:htnblog-buffer-name)))
    (if (and buffer (not arg))
        (pop-to-buffer buffer)
      (setq buffer (get-buffer-create my:htnblog-buffer-name))
      (with-current-buffer buffer
        (when (and (> (buffer-size) 0)
                   (not (y-or-n-p "書きかけの内容を捨てますか? ")))
          (user-error "htnblog: 中止した"))
        ;; 前回貼った最新記事は読み取り専用なので、束縛しないと消せない。
        (let ((inhibit-read-only t))
          (erase-buffer))
        (my:htnblog-mode)
        (insert (my:htnblog--template))
        (goto-char (point-max))
        (set-buffer-modified-p nil)
        ;; 参考の最新記事は非同期に取って後ろに貼る。モードを立てた後で
        ;; 世代を進めること (define-derived-mode が kill-all-local-variables
        ;; を通るので、その前に設定しても消える)。
        (setq my:htnblog--previous-token (1+ my:htnblog--previous-token))
        (when my:htnblog-show-previous
          (my:htnblog--fetch-previous config buffer my:htnblog--previous-token)))
      (pop-to-buffer buffer))))

(defalias 'htnblog #'my:htnblog
  "`my:htnblog' の別名。M-x htnblog で呼べるようにする。")

(provide 'my-htnblog)
;;; my-htnblog.el ends here
