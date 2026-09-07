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
              :body (string-trim (buffer-substring-no-properties body-start (point-max)))
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

(defun my:htnblog--request (config method url body)
  "CONFIG の認証情報で URL に METHOD で BODY を送り、応答の本文を返す。"
  (let* ((url-debug nil)                ; apikey をログに残さない
         (url-request-method method)
         (url-request-extra-headers
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

書きかけのバッファがあればそれに切り替える。ARG (\\[universal-argument]) を
付けると、中身を捨てて今日の日付でひな形を入れ直す。"
  (interactive "P")
  ;; 書き終えてから設定の不備に気づくと最悪なので、開く時点で検証しておく。
  (my:htnblog--config)
  (let ((buffer (get-buffer my:htnblog-buffer-name)))
    (if (and buffer (not arg))
        (pop-to-buffer buffer)
      (setq buffer (get-buffer-create my:htnblog-buffer-name))
      (with-current-buffer buffer
        (when (and (> (buffer-size) 0)
                   (not (y-or-n-p "書きかけの内容を捨てますか? ")))
          (user-error "htnblog: 中止した"))
        (erase-buffer)
        (my:htnblog-mode)
        (insert (my:htnblog--template))
        (goto-char (point-max))
        (set-buffer-modified-p nil))
      (pop-to-buffer buffer))))

(defalias 'htnblog #'my:htnblog
  "`my:htnblog' の別名。M-x htnblog で呼べるようにする。")

(provide 'my-htnblog)
;;; my-htnblog.el ends here
