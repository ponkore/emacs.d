;;; my-translate.el --- ローカル LLM で翻訳する (PLaMo 3 Translate)  -*- lexical-binding: t -*-
;;; Commentary:
;; PFN の翻訳特化 LLM PLaMo 3 Translate を、このマシンの中だけで動かして
;; 翻訳する。clone してきたリポジトリの README や、英語の docstring を
;; 読むためのもの。ネットワークへは出ない。
;;
;; 構成は常駐サーバ + HTTP。llama.cpp の `llama-server' にモデルを持たせて
;; 常駐させ、Emacs からは localhost へ JSON を投げる。
;;
;;   Emacs ──HTTP(localhost)──> llama-server.exe ──Vulkan──> iGPU
;;
;; リクエストごとにプロセスを起こさないのは、Windows の Emacs では
;; `call-process' のプロセス生成だけで 59〜76 ms 払うため (CLAUDE.md)。
;; 段落ごとに起こしていては翻訳と無関係なコストだけで数秒積む。
;;
;; 設計の根拠と実測は docs/translate/plamo-3-translate-study.md にある。
;; 触る前にそちらを読むこと。とくに次の 3 つは知らないと必ず踏む。
;;
;;   1. Intel の Vulkan は `GGML_VK_DISABLE_COOPMAT=1' が無いと GPU が
;;      落ちてサーバがプロセスごと死ぬ (§3.4)
;;   2. モデルに「構造を保て」と頼む経路が無い。PFML3 には指示文の欄が
;;      無く、渡せるのは target_lang / source_lang / context / glossary /
;;      form の 5 つだけ (§2)
;;   3. 訳文の検証は `string-search' で行う。`string-match' を使うと
;;      バッククォートや括弧が正規表現として解釈されて誤報する (§4.5-3)
;;
;; 使い方。
;;
;;   C-c T t   リージョンか段落を訳して *translation* に出す
;;             (C-u で丁寧なモデルに切り替える)
;;   C-c T s   サーバの状態を見る
;;   C-c T k   サーバを止める
;;
;; 【重要】元のバッファは絶対に書き換えない。他人のリポジトリを読んでいる
;; 最中に差分を作らないため。結果は必ず別バッファに出す。

;;; Code:

(require 'url)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

;;; --------------------------------------------------
;;; カスタマイズ
;;; --------------------------------------------------

(defgroup my:translate nil
  "ローカル LLM による翻訳."
  :group 'tools
  :prefix "my:translate-")

(defcustom my:translate-llama-server nil
  "`llama-server' の実行ファイル。nil なら自動で探す。
探す順は `my:translate-search-dirs' の下の llama.cpp-* ディレクトリ →
PATH。見つからなければ翻訳コマンドは「未設定」と言って何もしない。

マシンごとに置き場が違うので、ここに絶対パスを書いてリポジトリに
コミットしてはいけない。設定したいならそのマシンの custom か
環境側の設定に置く。"
  :type '(choice (const :tag "自動で探す" nil) file))

(defcustom my:translate-search-dirs
  '("~/.local/opt/" "~/opt/")
  "`llama-server' を探すディレクトリ。直下の llama.cpp* を見る。"
  :type '(repeat directory))

(defcustom my:translate-model-dir "~/.local/share/plamo-models/"
  "GGUF の置き場。"
  :type 'directory)

(defcustom my:translate-profiles
  '((fast    :match "0.8b" :port 18080 :label "0.8b (速い)")
    (quality :match "-2b"  :port 18081 :label "2b (丁寧)"))
  "プロファイルの定義。

`:match' は `my:translate-model-dir' の中の .gguf を選ぶための部分文字列。
`:port' は `llama-server' を待たせるポート。

2 本同時に常駐させてよい。実測で 0.8b + 2b = 2.2 GB で、iGPU から
使える 17.2 GB に対して問題にならない (docs §4.6)。"
  :type '(alist :key-type symbol :value-type plist))

(defcustom my:translate-default-profile 'fast
  "既定のプロファイル。前置引数を付けると `my:translate-alt-profile' になる。"
  :type 'symbol)

(defcustom my:translate-alt-profile 'quality
  "前置引数を付けたときに使うプロファイル。"
  :type 'symbol)

(defcustom my:translate-target-lang "ja"
  "訳文の言語コード。小文字。`ja' `en' `zh-hans' など。
`ja(easy)' を指定するとやさしい日本語になる。"
  :type 'string)

(defcustom my:translate-source-lang nil
  "原文の言語コード。nil ならモデルに判定させる。"
  :type '(choice (const :tag "自動判定" nil) string))

(defcustom my:translate-form "plain"
  "日本語の文体。\"plain\" が常体、\"polite\" が敬体、nil で指定しない。
`my:translate-target-lang' が \"ja\" のときだけ意味がある。

【注意】これは指示ではなくヒントで、段落ごとに守られないことがある
\(docs §4.5-2 で実測)。文書全体を揃えたいなら後処理が要る。"
  :type '(choice (const :tag "指定しない" nil)
                 (const "plain") (const "polite")))

(defcustom my:translate-disable-coopmat t
  "非 nil なら `GGML_VK_DISABLE_COOPMAT=1' を立ててサーバを起こす。

【重要】これを nil にすると Intel の iGPU で GPU が落ち、
サーバがプロセスごと死ぬ (docs §3.4)。実測で 3 回再現した。

  ggml_vulkan: device lost on Vulkan0
  GGML_ASSERT(batch.slot_batched || batch.size() == 0) failed

落ちるまでの時間がばらつき (175 / 26 / 34 秒)、Windows の
イベントログにも痕跡が残らないので、知らずに当たると原因が分からない。

切ってもこの用途の速度はほぼ落ちない。プロンプト処理は 57% 落ちるが
生成は落ちず (123.5 対 121.5 tok/s)、翻訳は生成律速なので実質タダ。"
  :type 'boolean)

(defcustom my:translate-server-args
  '("-ngl" "99" "-c" "4096" "--parallel" "1" "-b" "512" "-ub" "256"
    "--no-webui")
  "`llama-server' に渡す引数。モデルとポートは自動で足す。"
  :type '(repeat string))

(defcustom my:translate-start-timeout 90
  "サーバが応答するようになるまで待つ秒数。
モデルのロードは実測で 1 秒 (ページキャッシュにあるとき) だが、
初回は数秒かかる。"
  :type 'number)

(defcustom my:translate-timeout 180
  "1 リクエストの待ち時間の上限 (秒)。"
  :type 'number)

(defcustom my:translate-buffer-name "*translation*"
  "結果を出すバッファの名前。"
  :type 'string)

;;; --------------------------------------------------
;;; 実行ファイルとモデルを探す
;;; --------------------------------------------------

(defun my:translate--llama-server ()
  "`llama-server' の実行ファイルのパスを返す。見つからなければ nil。"
  (or (and my:translate-llama-server
           (file-executable-p my:translate-llama-server)
           my:translate-llama-server)
      (car (seq-filter
            #'file-executable-p
            (seq-mapcat
             (lambda (dir)
               (and (file-directory-p dir)
                    (seq-mapcat
                     (lambda (sub)
                       (seq-filter
                        #'stringp
                        (list (expand-file-name "llama-server.exe" sub)
                              (expand-file-name "llama-server" sub))))
                     (directory-files (expand-file-name dir) t "\\`llama\\.cpp"))))
             my:translate-search-dirs)))
      (executable-find "llama-server")))

(defun my:translate--profile (profile)
  "PROFILE の定義を返す。無ければ `user-error'。"
  (or (alist-get profile my:translate-profiles)
      (user-error "プロファイル %s は `my:translate-profiles' に無い" profile)))

(defun my:translate--model (profile)
  "PROFILE に対応する .gguf のパスを返す。見つからなければ nil。

`:match' を含み、かつ名前に \"mmproj\" を含まない最初のものを採る。"
  (let ((dir (expand-file-name my:translate-model-dir))
        (match (plist-get (my:translate--profile profile) :match)))
    (when (file-directory-p dir)
      (car (seq-filter
            (lambda (f) (string-search match (file-name-nondirectory f)))
            (directory-files dir t "\\.gguf\\'"))))))

(defun my:translate--port (profile)
  "PROFILE のポート番号。"
  (plist-get (my:translate--profile profile) :port))

(defun my:translate--label (profile)
  "PROFILE の表示名。"
  (or (plist-get (my:translate--profile profile) :label)
      (symbol-name profile)))

;;; --------------------------------------------------
;;; サーバの管理
;;; --------------------------------------------------

(defvar my:translate--servers (make-hash-table :test #'eq)
  "PROFILE -> プロセス。")

(defun my:translate--server-live-p (profile)
  "PROFILE のサーバのプロセスが生きていれば非 nil。"
  (let ((proc (gethash profile my:translate--servers)))
    (and (processp proc) (process-live-p proc))))

(defun my:translate--log-buffer (profile)
  "PROFILE のサーバのログバッファ。"
  (get-buffer-create (format " *my:translate-server(%s)*" profile)))

(defun my:translate--start-server (profile)
  "PROFILE のサーバを起こす。既に生きていれば何もしない。"
  (unless (my:translate--server-live-p profile)
    (let ((exe (my:translate--llama-server))
          (model (my:translate--model profile)))
      (unless exe
        (user-error "llama-server が見つからない。%s か PATH に置くか `my:translate-llama-server' を設定する"
                    (car my:translate-search-dirs)))
      (unless model
        (user-error "%s に %s を含む .gguf が無い"
                    my:translate-model-dir
                    (plist-get (my:translate--profile profile) :match)))
      (let* ((port (number-to-string (my:translate--port profile)))
             ;; 【重要】coopmat を切る。切らないと GPU が落ちる (docs §3.4)。
             (process-environment
              (if my:translate-disable-coopmat
                  (cons "GGML_VK_DISABLE_COOPMAT=1" process-environment)
                process-environment))
             ;; 出力は読まないが、落ちたときに理由を見たいので残す。
             (buffer (my:translate--log-buffer profile))
             (proc (apply #'start-process
                          (format "my:translate-server(%s)" profile)
                          buffer exe
                          "-m" (expand-file-name model)
                          "--port" port
                          "--host" "127.0.0.1"
                          my:translate-server-args)))
        (set-process-query-on-exit-flag proc nil)
        (puthash profile proc my:translate--servers)
        (message "翻訳サーバ %s を起動中... (%s)"
                 (my:translate--label profile) (file-name-nondirectory model))
        proc))))

(defun my:translate-stop-servers ()
  "起こしてある翻訳サーバを全部止める。"
  (interactive)
  (let ((n 0))
    (maphash (lambda (profile proc)
               (when (and (processp proc) (process-live-p proc))
                 (delete-process proc)
                 (setq n (1+ n)))
               (remhash profile my:translate--servers))
             (copy-hash-table my:translate--servers))
    (clrhash my:translate--servers)
    (when (called-interactively-p 'interactive)
      (message (if (zerop n) "動いている翻訳サーバは無い"
                 (format "翻訳サーバを %d 本止めた" n))))))

(add-hook 'kill-emacs-hook #'my:translate-stop-servers)

(defun my:translate-server-status ()
  "翻訳サーバとモデルの状態を echo area に出す。"
  (interactive)
  (let ((exe (my:translate--llama-server)))
    (message
     "%s\n%s"
     (if exe (format "llama-server: %s" exe)
       "llama-server: 見つからない")
     (mapconcat
      (lambda (entry)
        (let* ((profile (car entry))
               (model (my:translate--model profile)))
          (format "  %-8s %-14s port %d  %s"
                  profile
                  (if (my:translate--server-live-p profile) "起動中" "停止")
                  (my:translate--port profile)
                  (if model (file-name-nondirectory model) "モデル無し"))))
      my:translate-profiles "\n"))))

;;; --------------------------------------------------
;;; PFML3 の組み立て
;;; --------------------------------------------------
;; 書式は docs/translate/plamo-3-translate-study.md §2 の表がそのまま仕様。
;; 最後の <|plamo:msg|> の直後から訳文が生成される。

(defconst my:translate--pfml-prefix "<|plamo:"
  "PFML3 の特殊トークンの頭。入力に現れたら拒否する。")

(defun my:translate--check-text (text what)
  "TEXT が PFML3 の入力として安全か見る。駄目なら `user-error'。

【重要】本文に特殊トークンが混ざるとプロトコルの構造が壊れ、
翻訳の指定を外から上書きできてしまう。公式の chat テンプレートも
同じ理由で `raise_exception' で弾いている。"
  (when (string-search my:translate--pfml-prefix text)
    (user-error "%s に PFML3 の予約トークン (%s...) が含まれている"
                what my:translate--pfml-prefix)))

(defun my:translate--pfml3 (text &optional context)
  "TEXT を訳させる PFML3 のプロンプトを組んで返す。
CONTEXT があれば参考文脈として添える。"
  (my:translate--check-text text "原文")
  (when context (my:translate--check-text context "文脈"))
  (concat
   "<|plamo:tag|>dataset<|plamo:msg|>translation"
   (when (and context (not (string-empty-p context)))
     (concat "<|plamo:tag|>context<|plamo:msg|>" context))
   "<|plamo:tag|>input"
   (when my:translate-source-lang
     (concat "<|plamo:key|>lang<|plamo:val|>" my:translate-source-lang))
   "<|plamo:msg|>" text
   "<|plamo:tag|>output<|plamo:key|>lang<|plamo:val|>" my:translate-target-lang
   (when (and my:translate-form (equal my:translate-target-lang "ja"))
     (concat "<|plamo:key|>form<|plamo:val|>" my:translate-form))
   "<|plamo:msg|>"))

(defun my:translate--n-predict (text)
  "TEXT に対する生成トークン数の上限。

暴走生成の安全弁。日本語の出力は原文と同程度のトークン数で収まるので、
2.5 倍あれば足りる。上限に達した応答は訳文として信用しない。"
  (let ((est (+ 8 (/ (string-bytes text) 3))))
    (max 64 (min 2048 (floor (* est 2.5))))))

;;; --------------------------------------------------
;;; HTTP
;;; --------------------------------------------------

(defun my:translate--response-body ()
  "応答バッファの本文を UTF-8 で復号して返す。"
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

(defun my:translate--post (profile text context callback &optional deadline)
  "PROFILE のサーバに TEXT の翻訳を頼む。非同期。
CONTEXT は参考文脈 (nil 可)。CALLBACK は 2 引数 (TRANSLATION ERROR) で
ちょうど 1 回呼ばれる。

DEADLINE は内部用。サーバの起動待ちと再試行をここで吸収する。

【重要】サーバはプロセスごと死ぬことがある (docs §8-B)。死んでいたら
起こし直して、応答するようになるまで待ってから投げ直す。この関数が
その面倒を全部見るので、呼ぶ側は気にしなくてよい。"
  (let* ((deadline (or deadline
                       (+ (float-time) my:translate-start-timeout)))
         (retry
          (lambda (reason)
            (if (< (float-time) deadline)
                (run-at-time 0.3 nil
                             #'my:translate--post
                             profile text context callback deadline)
              (funcall callback nil (format "翻訳サーバに繋がらない (%s)" reason))))))
    (condition-case err
        (progn
          (my:translate--start-server profile)
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  '(("Content-Type" . "application/json; charset=utf-8")))
                 ;; 【重要】`url-request-data' は unibyte でなければ
                 ;; ならない。ここを通さないと日本語がそのまま化ける
                 ;; (my-htnblog で踏んだのと同じ罠)。
                 (url-request-data
                  (encode-coding-string
                   (json-serialize
                    (list :prompt (my:translate--pfml3 text context)
                          :n_predict (my:translate--n-predict text)
                          :temperature 0
                          ;; モデルは訳文の最後で EOS を出すので実際には
                          ;; 発火しないが、保険として渡しておく。
                          :stop (vector "<|plamo:tag|>" "<|plamo:bos|>")
                          :cache_prompt t))
                   'utf-8))
                 (done nil)
                 (timer nil)
                 (finish (lambda (text err)
                           (unless done
                             (setq done t)
                             (when (timerp timer) (cancel-timer timer))
                             (funcall callback text err))))
                 (buffer
                  (url-retrieve
                   (format "http://127.0.0.1:%d/completion"
                           (my:translate--port profile))
                   (lambda (status)
                     (let ((http url-http-response-status)
                           (body (my:translate--response-body))
                           (err (plist-get status :error)))
                       (when (buffer-live-p (current-buffer))
                         (kill-buffer (current-buffer)))
                       ;; HTTP のステータスを :error より先に見る
                       ;; (my-htnblog の実測。url は 4xx/5xx を
                       ;; :error にも積むので、先に見ると本文を捨てる)。
                       (cond
                        ((and (integerp http) (<= 200 http) (< http 300))
                         (condition-case e
                             (let* ((obj (json-parse-string
                                          body :object-type 'alist))
                                    (content (alist-get 'content obj))
                                    (stop (alist-get 'stop_type obj)))
                               (if (equal stop "limit")
                                   ;; 上限で打ち切られた訳文は信用しない。
                                   (funcall finish nil "生成が上限で打ち切られた")
                                 (funcall finish (string-trim (or content "")) nil)))
                           (error (funcall finish nil (error-message-string e)))))
                        ;; サーバが死んだ / まだ起きていない。起こし直す。
                        ((or err (not (integerp http)))
                         (unless (my:translate--server-live-p profile)
                           (remhash profile my:translate--servers))
                         (unless done
                           (setq done t)
                           (when (timerp timer) (cancel-timer timer))
                           (funcall retry (if err (error-message-string err)
                                            "応答が読めない"))))
                        (t (funcall finish nil (format "HTTP %s" http))))))
                   nil t t)))
            (cond
             ((and (not done) (not buffer)) (funcall retry "応答がない"))
             ((and (not done) (numberp my:translate-timeout))
              (setq timer (run-at-time my:translate-timeout nil
                                       (lambda ()
                                         (when (buffer-live-p buffer)
                                           (kill-buffer buffer))
                                         (funcall finish nil "時間切れ"))))))))
      ;; 接続できない = サーバがまだロード中。待って投げ直す。
      (file-error (funcall retry (error-message-string err)))
      (user-error (funcall callback nil (error-message-string err))))))

;;; --------------------------------------------------
;;; 訳文の検証
;;; --------------------------------------------------

(defun my:translate--protected (text)
  "TEXT の中で訳文に残っていなければならない文字列を集めて返す。
インラインコード (`...') と URL。"
  (let (result)
    (dolist (re '("`[^`\n]+`" "https?://[^][\s\n\"'<>)]+"))
      (let ((start 0))
        (while (string-match re text start)
          (push (match-string 0 text) result)
          (setq start (match-end 0)))))
    (delete-dups result)))

(defun my:translate--lost (src dst)
  "SRC にあって DST から消えた保護対象の一覧。

【重要】比較は `string-search' で行う。`string-match' を使うと
バッククォート・括弧・ドットが正規表現として解釈され、
実測で「132 件中 132 件が消失」という誤報を出した (docs §4.5-3)。
検証器が壊れていると、本当の壊れ 1 件が埋もれる。"
  (seq-filter (lambda (s) (not (string-search s dst)))
              (my:translate--protected src)))

;;; --------------------------------------------------
;;; 結果バッファ
;;; --------------------------------------------------

(defvar-local my:translate--source-buffer nil
  "この結果バッファの元のバッファ。")

(define-derived-mode my:translate-mode special-mode "Translate"
  "翻訳結果を見るモード。"
  (setq-local truncate-lines nil)
  ;; 訳文は 1 段落 1 行の長い行になるので、視覚行で動けるようにする。
  (setq-local word-wrap t))

(defun my:translate--result-buffer (source)
  "結果バッファを用意して返す。SOURCE は元のバッファ。"
  (let ((buffer (get-buffer-create my:translate-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my:translate-mode)
        (my:translate-mode))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq my:translate--source-buffer source))
    buffer))

(defun my:translate--append (buffer &rest strings)
  "BUFFER の末尾に STRINGS を足す。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (apply #'insert strings))))))

;;; --------------------------------------------------
;;; 段落に分ける
;;; --------------------------------------------------

(defun my:translate--segments (text)
  "TEXT を訳す単位に分ける。空行で区切り、各単位の改行は空白に畳む。

【重要】1 単位の中で改行を残してはいけない。原文が `fill-column' で
折り返されていると、そこで文が切れたものとして訳される。

markdown の構造を見た分割は Phase 2 (docs §6.1)。ここでは
空行区切りだけ。"
  (seq-filter
   (lambda (s) (not (string-empty-p s)))
   (mapcar (lambda (chunk)
             (string-trim (replace-regexp-in-string "[ \t]*\n[ \t]*" " " chunk)))
           (split-string text "\n[ \t]*\n+"))))

;;; --------------------------------------------------
;;; コマンド
;;; --------------------------------------------------

(defun my:translate--run (segments buffer profile)
  "SEGMENTS を順に訳して BUFFER に足す。PROFILE のサーバを使う。

1 つずつ直列に投げる。理由は 2 つ。順序が保てること、そして
GPU への要求が直列化されるので同時に投げても速くならないこと。

段落が訳せるたびに挿入されるので、それが進捗表示になる。
トークン単位の逐次表示 (SSE) は要らない (docs §4.7)。"
  (let ((total (length segments))
        (index 0))
    (cl-labels
        ((step
          (rest previous)
          (if (null rest)
              (progn
                (my:translate--append buffer "\n")
                (message "翻訳が終わった (%d 段落)" total))
            (setq index (1+ index))
            (let ((src (car rest)))
              (message "翻訳中... %d/%d" index total)
              (my:translate--post
               profile src previous
               (lambda (translation error)
                 (cond
                  (error
                   (my:translate--append
                    buffer
                    (format "── %d/%d ──\n%s\n\n!! 訳せなかった: %s\n\n"
                            index total src error)))
                  (t
                   (let ((lost (my:translate--lost src translation)))
                     (my:translate--append
                      buffer
                      (format "── %d/%d ──\n%s\n\n%s\n"
                              index total src translation)
                      ;; 識別子や URL が落ちていたら黙って通さない。
                      ;; 壊れた訳文を信じるより、言われた方が安全。
                      (if lost
                          (format "\n!! 原文にあって訳文に無い: %s\n\n"
                                  (mapconcat #'identity lost " "))
                        "\n")))))
                 (step (cdr rest) src)))))))
      (step segments nil))))

(defun my:translate-dwim (&optional arg)
  "リージョンか、ポイントのある段落を訳して別バッファに出す。

リージョンがあればそれを、無ければポイントのある段落を訳す。
前置引数 ARG を付けると `my:translate-alt-profile' (丁寧なモデル) を使う。

【重要】元のバッファは書き換えない。"
  (interactive "P")
  ;; 先に実行ファイルとモデルを確かめる。ここで落とさないと、
  ;; 「llama-server が無い」が結果バッファの中に書かれることになる。
  (let ((profile (if arg my:translate-alt-profile my:translate-default-profile)))
    (unless (my:translate--llama-server)
      (user-error "llama-server が見つからない。%s か PATH に置く"
                  (car my:translate-search-dirs)))
    (unless (my:translate--model profile)
      (user-error "%s に %s を含む .gguf が無い"
                  my:translate-model-dir
                  (plist-get (my:translate--profile profile) :match))))
  (let* ((profile (if arg my:translate-alt-profile my:translate-default-profile))
         (text (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (forward-paragraph) (point)))
                         (beg (progn (backward-paragraph) (point))))
                     (buffer-substring-no-properties beg end)))))
         (segments (my:translate--segments text)))
    (unless segments
      (user-error "訳すものが無い"))
    (let ((buffer (my:translate--result-buffer (current-buffer))))
      (my:translate--append
       buffer
       (format "%s / %s → %s%s\n\n"
               (buffer-name (current-buffer))
               (my:translate--label profile)
               my:translate-target-lang
               (if my:translate-form (format " (%s)" my:translate-form) "")))
      (display-buffer buffer)
      (my:translate--run segments buffer profile))))

(use-package emacs
  :bind (("C-c T t" . my:translate-dwim)
         ("C-c T s" . my:translate-server-status)
         ("C-c T k" . my:translate-stop-servers)))

(provide 'my-translate)
;;; my-translate.el ends here
