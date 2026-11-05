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
;; 使い方。どれも C-u で丁寧なモデル (2b) に切り替わる。
;;
;;   C-c T t   リージョンか段落を訳して *translation* に出す
;;   C-c T c   リージョンかバッファのコメントを訳し、各コメントの下に
;;             overlay で出す (ソースを読むときはこれ)
;;   C-c T m   markdown を構造ごと訳して別バッファに出す (README はこれ)
;;   C-c T h   *Help* の散文を訳して別バッファに出す
;;   C-c T x   貼ったコメントの訳を消す
;;   C-c T s   サーバの状態を見る
;;   C-c T k   サーバを止める
;;
;; コメントの検出は tree-sitter のパーサがあればそれを使い、無ければ
;; `syntax-ppss' に落ちる。どちらも文字列リテラルの中の `//' を
;; 誤認しない。**ノードの型名は決め打ちしない** (Rust は `comment' ではなく
;; `line_comment' / `block_comment')。
;;
;; 【重要】元のバッファは絶対に書き換えない。他人のリポジトリを読んでいる
;; 最中に差分を作らないため。結果は必ず別バッファに出す。

;;; Code:

(require 'url)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'rx)
;; tree-sitter 無しでビルドされた Emacs でも読めるようにする。
;; コメントの検出は `fboundp' で見て `syntax-ppss' に落ちる。
(require 'treesit nil t)

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

(defcustom my:translate-comment-prefix "▸ "
  "コメントの訳文の頭に付ける印。"
  :type 'string)

(defcustom my:translate-comment-min-letters 10
  "コメントを訳す下限。英字がこれ未満なら訳さない。
`ok' `TODO' `nolint' のような断片を訳しても読めるものにならない。"
  :type 'integer)

(defcustom my:translate-ask-threshold 40
  "訳す単位がこの数を超えたら、訳す前に確認する（コメントと help）。

`C-c T m'（markdown）には掛けない。文書を丸ごと訳すのは明示的な操作で、
単位が 100 を超えるのが普通なため。"
  :type 'integer)

(defcustom my:translate-max-cjk 1
  "日本語の文字がこの数を超える単位は訳さない（コメント・markdown 共通）。

**既に日本語で書かれているものを訳し返さないための判定。**
この設定を自分のリポジトリで使うと、日本語のコメントや文書が大半になる。

【重要】非 ASCII の「比率」で見てはいけない。識別子・URL・コード片が
多いコメントでは比率が下がってすり抜ける。実測では
`gitd/src/main.rs' の
「(`Content-Length: N...`)。Emacs 側は同梱の jsonrpc.el を使う。」が
比率 0.19 で通ってしまい、日本語を日本語に訳しに行った。
**英語のコメントに日本語はまず現れないので、数で見るほうが確実。**"
  :type 'integer)

(defcustom my:translate-comment-skip-regexp
  (rx (or "-*-"                         ; ファイルローカル変数
          "SPDX-License-Identifier"
          "DO NOT EDIT" "Code generated by"
          "nolint" "noqa" "eslint-disable" "prettier-ignore"
          "@ts-ignore" "@ts-expect-error"
          "pylint:" "mypy:" "type:" "pragma:"
          "checkstyle:" "NOSONAR"
          ;; Emacs Lisp のライブラリヘッダ。散文ではない
          (seq bos (* space)
               (or "Author" "Maintainer" "URL" "Homepage" "Keywords"
                   "Version" "Package-Requires" "Package-Version"
                   "Created" "Copyright" "Filename" "Commentary" "Code")
               ":")
          ;; ライセンスの定型文。訳しても読む価値が無い
          "GNU General Public License" "Free Software Foundation"
          "WITHOUT ANY WARRANTY" "is free software" "is part of GNU Emacs"
          "Licensed under the Apache License"
          "THE SOFTWARE IS PROVIDED"))
  "これに一致するコメントは訳さない。機械向けの指示や定型句。"
  :type 'regexp)

(defface my:translate-comment-face
  '((t :inherit (shadow italic)))
  "コメントの訳文を表示する face。")

(defcustom my:translate-help-min-letters 20
  "help バッファで訳す単位の下限。英字がこれ未満なら訳さない。
`Its value is 70' のような短い行を落とす。"
  :type 'integer)

(defcustom my:translate-markdown-keep-heading-original t
  "非 nil なら、訳した見出しに原文を括弧で併記する。

【重要】切ると目次のリンクが全部外れる。markdown の目次は
`* [Contribute!](#contribute)' の形で **原文の見出しから生成された
アンカー**を指すので、見出しだけ訳すと指す先が無くなる（実測）。
併記しておけば GFM のアンカーは原文側からも作られるのでリンクが生きる。
目次の行そのものは「リンクだけの行」として訳さず素通しする。"
  :type 'boolean)

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

(defconst my:translate--protect-common
  '("`[^`\n]+`" "https?://[^][\s\n\"'<>)]+")
  "どの用途でも訳文に残っていなければならないもの。")

(defconst my:translate--protect-help
  '(("‘\\([^’\n]+\\)’" . 1)            ; help が描く引用符つきシンボル
    ("`\\([^'\n]+\\)'" . 1)            ; 生の docstring の引用符
    "\\_<[A-Z][A-Z0-9]+\\(?:-[A-Z0-9]+\\)*\\_>")  ; 大文字の引数名
  "help バッファで追加で守るもの。

【重要】docstring の大文字の引数名 (BUFFER / NOERROR) と
`‘replace-match’' のようなシンボルは、訳文に同じ形で残っていなければ
意味が通らない。help でだけ見るのは、markdown や README では
API / JSON / README のような語が普通の単語として訳されうるため。

【重要】引用符つきシンボルは**中身だけを守る**（グループ 1）。
モデルは `‘replace-match’' を「replace-match」と書くことがあり、
**シンボル名は保たれているのに引用符の体裁だけが変わる**。丸ごとで
照合すると毎回「消えた」と報告してしまう（実測）。
markdown のインラインコードは体裁そのものが構造なので、
あちらは丸ごと（グループ 0）で見る。")

(defun my:translate--protected (text &optional extra)
  "TEXT の中で訳文に残っていなければならない文字列を集めて返す。
EXTRA は追加で見る正規表現のリスト。

【重要】`case-fold-search' を nil に束縛すること。既定は t なので、
**`[A-Z]' が小文字にも一致して普通の英単語まで保護対象になる**。
実測では `Replace all matches for REGEXP with REP in STRING.' に対して
Replace / all / matches / for / in / with まで拾い、訳文から消えたと
報告した（訳文は正しかったのに「壊れた」と言う）。"
  (let ((case-fold-search nil)
        (result nil))
    ;; SPEC は正規表現、または (正規表現 . 取り出すグループ番号)。
    (dolist (spec (append my:translate--protect-common extra))
      (let ((re (if (consp spec) (car spec) spec))
            (group (if (consp spec) (cdr spec) 0))
            (start 0))
        (while (string-match re text start)
          (push (match-string group text) result)
          (setq start (max (match-end 0) (1+ start))))))
    (delete-dups result)))

(defun my:translate--lost (src dst &optional extra)
  "SRC にあって DST から消えた保護対象の一覧。EXTRA は追加の正規表現。

【重要】比較は `string-search' で行う。`string-match' を使うと
バッククォート・括弧・ドットが正規表現として解釈され、
実測で「132 件中 132 件が消失」という誤報を出した (docs §4.5-3)。
検証器が壊れていると、本当の壊れ 1 件が埋もれる。"
  (seq-filter (lambda (s) (not (string-search s dst)))
              (my:translate--protected src extra)))

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
;;; コメントを取り出す
;;; --------------------------------------------------

(defun my:translate--comment-ranges-treesit (beg end)
  "BEG..END のコメントノードを (START . END) のリストで返す。

【重要】ノードの型名を決め打ちしてはいけない。文法ごとに違い、
Go / JS / Python は `comment' だが **Rust は `line_comment' と
`block_comment'**。`(comment)' のクエリを決め打ちすると、
その文法では 1 件も取れない (しかも `treesit-query-error' になる)。
名前に \"comment\" を含むかで見る。"
  (let (acc)
    (dolist (parser (treesit-parser-list))
      (cl-labels
          ((walk (node)
             (let ((ns (treesit-node-start node))
                   (ne (treesit-node-end node)))
               ;; 範囲に掛からない枝には降りない
               (when (and (< ns end) (> ne beg))
                 (if (string-search "comment" (treesit-node-type node))
                     (push (cons ns ne) acc)
                   (dolist (child (treesit-node-children node))
                     (walk child)))))))
        (walk (treesit-parser-root-node parser))))
    (sort acc (lambda (a b) (< (car a) (car b))))))

(defun my:translate--trim-range-end (from to)
  "TO を後ろの空白と改行のぶん戻す (FROM より手前には戻らない)。

【重要】`forward-comment' は行コメントの**終端の改行まで食う**ので、
そのままだと隣接判定の隙間に改行が入らず、**連続した行コメントが
1 単位にまとまらない**。実測で `diff-hl.el' の 148 件が 137 件にしか
減らず、ライセンスと Commentary が分断された。
tree-sitter のノードは改行を含まないので、こちらを揃える。"
  (save-excursion
    (goto-char to)
    (skip-chars-backward " \t\n" from)
    (max from (point))))

(defun my:translate--comment-ranges-syntax (beg end)
  "BEG..END のコメントを `syntax-ppss' で拾う。

tree-sitter のパーサが無いモード用 (この設定が文法を入れているのは 17 言語
だけなので、**Emacs Lisp / PHP / C++ / sh はこちらを通る**)。

正規表現で「`//' で始まる行」を探す方式を採らないのは、
**文字列リテラルの中の `//' を誤認しない**ため。`syntax-ppss' は
本物の構文状態を見る。"
  (let (acc)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((state (syntax-ppss))
               (start (nth 8 state)))
          (if (nth 4 state)
              ;; コメントの中にいる。頭に戻って末尾まで食う。
              ;; `forward-comment' は終端の改行まで食うが、落とすのは
              ;; `my:translate--comment-ranges' の側でまとめてやる。
              (let ((from (or start (point))))
                (goto-char from)
                (if (forward-comment 1)
                    (push (cons from (min (point) end)) acc)
                  (goto-char end)))
            ;; 次のコメントの開始を探す
            (if (and comment-start-skip
                     (re-search-forward comment-start-skip end t))
                (progn
                  (goto-char (match-beginning 0))
                  ;; 文字列の中に当たっただけなら 1 文字進めて続ける
                  (unless (nth 4 (syntax-ppss))
                    (forward-char 1)))
              (goto-char end))))))
    (nreverse acc)))

(defun my:translate--comment-ranges (beg end)
  "BEG..END のコメントの範囲。tree-sitter があればそちら、無ければ構文解析。

【重要】**どちらの経路でも、末尾の改行と空白をここで必ず落とす。**
改行が範囲に残っていると 2 つが同時に壊れ、しかも**エラーは出ない**。

- 隣接判定 (`my:translate--comment-adjacent-p') の隙間に改行が入らず、
  **連続した行コメントが 1 単位にまとまらない**。1 行ずつ訳されるので
  文が途中で切れた訳文が並ぶ
- `after-string' が改行の後ろに付くので、**訳文が 1 行ずれて次の
  コメント行と同じ行に出る**

範囲に改行が入るかは**経路でも文法でも変わる**ので、個別に対処しない。

- `forward-comment' (構文解析) は行コメントの終端の改行まで食う
- tree-sitter は文法しだい。**Python の `comment' は含まないが、
  Rust の `line_comment' は含む**（実測。Python だけで試すと
  この問題は表面化しない）"
  (seq-filter
   (lambda (r) (< (car r) (cdr r)))
   (mapcar (lambda (r)
             (cons (car r) (my:translate--trim-range-end (car r) (cdr r))))
           (if (and (fboundp 'treesit-parser-list) (treesit-parser-list))
               (my:translate--comment-ranges-treesit beg end)
             (my:translate--comment-ranges-syntax beg end)))))

(defun my:translate--comment-adjacent-p (a b)
  "範囲 A の直後の行が範囲 B で、字下げも同じなら非 nil。"
  (and (<= (cdr a) (car b))
       (string-match-p "\\`[ \t]*\n[ \t]*\\'"
                       (buffer-substring-no-properties (cdr a) (car b)))
       (= (save-excursion (goto-char (car a)) (current-column))
          (save-excursion (goto-char (car b)) (current-column)))))

;; 連続した行コメントは 1 単位にまとめる (`my:translate--comment-units')。
;; 行ごとに切って訳すと 1 つの文が分断されて訳文の品質が落ちる。

(defconst my:translate--comment-delimiters
  '("<!--" "-->" "///" "//!" "//" "/**" "/*" "*/" "#|" "|#" "#!"
    "###" "##" "#" ";;;" ";;" ";" "--" "%%" "%" "\"" "*")
  "行頭から剥がすコメント記号。**長いものを先に並べる**
\(`//' を先に置くと `///' が 1 本残る)。")

(defconst my:translate--comment-strip-bol
  (concat "\\`[ \t]*\\(?:"
          (mapconcat #'regexp-quote my:translate--comment-delimiters "\\|")
          "\\)?[ \t]*")
  "行頭の字下げとコメント記号に一致する。")

(defconst my:translate--comment-strip-eol
  (concat "[ \t]*\\(?:"
          (mapconcat #'regexp-quote '("*/" "-->" "|#") "\\|")
          "\\)?[ \t]*\\'")
  "行末のコメント終端に一致する。")

(defun my:translate--strip-comment (text)
  "コメント TEXT から記号と字下げを剥がし、1 行に畳んで返す。

【重要】改行は空白にする。1 単位の中に改行を残すと、そこで文が
切れたものとして訳される (`my:translate--segments' と同じ理由)。"
  (string-trim
   (mapconcat
    #'identity
    (seq-remove
     #'string-empty-p
     (mapcar (lambda (line)
               (string-trim
                (replace-regexp-in-string
                 my:translate--comment-strip-eol ""
                 (replace-regexp-in-string
                  my:translate--comment-strip-bol "" line))))
             (split-string text "\n")))
    " ")))

(defun my:translate--letters (text)
  "TEXT に含まれる英字の数。"
  (seq-count (lambda (c) (or (<= ?a c ?z) (<= ?A c ?Z))) text))

(defun my:translate--cjk-count (text)
  "TEXT に含まれる日本語の文字 (かな・漢字・全角記号) の数。"
  (seq-count (lambda (c)
               (or (<= #x3041 c #x309F)     ; ひらがな
                   (<= #x30A0 c #x30FF)     ; カタカナ
                   (<= #x4E00 c #x9FFF)     ; CJK 統合漢字
                   (<= #x3000 c #x303F)     ; 、。「」などの記号
                   (<= #xFF01 c #xFF60)))   ; 全角英数・記号
             text))

(defun my:translate--comment-prose-p (text)
  "剥がしたあとの TEXT を訳す価値があれば非 nil。

落とすのは 3 種類。

- 機械向けの指示と定型句 (`my:translate-comment-skip-regexp')
- 英字が足りないもの。`TODO' `ok' `=====' のような断片や、
  コメントアウトされた記号だけの行
- **既に日本語で書かれているもの**
  (`my:translate-max-cjk')。この設定を自分のリポジトリで
  使うと大半がこれになるので、入れておかないと訳し返してしまう。"
  (and (not (string-empty-p text))
       (not (string-match-p my:translate-comment-skip-regexp text))
       (>= (my:translate--letters text) my:translate-comment-min-letters)
       (<= (my:translate--cjk-count text) my:translate-max-cjk)))

(defun my:translate--shebang-p (range)
  "RANGE がファイル先頭の shebang 行なら非 nil。

`#!/usr/bin/env python3' は剥がすと `/usr/bin/env python3' になり、
英字の数だけなら閾値を超えてしまう (実測で訳しに行った)。"
  (and (= (car range) (point-min))
       (string-prefix-p "#!" (buffer-substring-no-properties
                              (car range)
                              (min (cdr range) (+ (car range) 2))))))

(defun my:translate--comment-units (beg end)
  "BEG..END から訳す単位を ((START . END) . TEXT) のリストで返す。

隣接していて字下げも同じ行コメントは 1 単位にまとめる。
ただし **中身が空のコメント行（`;;' だけの行）は段落の区切りとして扱う。**

【重要】区切りにしないと、Emacs Lisp のファイルヘッダのように
`;;' が延々と続く塊が 1 単位になり、**ライセンスの定型文と
Commentary の本文が一緒になる**（定型文の側が
`my:translate-comment-skip-regexp' に当たるので、読みたい
Commentary まで落ちる）。"
  (let ((items
         ;; ((START . END) . TEXT)。TEXT は空のこともある
         (mapcar (lambda (r)
                   (cons r (if (my:translate--shebang-p r)
                               ""
                             (my:translate--strip-comment
                              (buffer-substring-no-properties (car r) (cdr r))))))
                 (my:translate--comment-ranges beg end)))
        (units nil)
        (cur nil))
    (dolist (item items)
      (let ((range (car item))
            (text (cdr item)))
        (cond
         ;; 中身が空 = 段落の区切り
         ((string-empty-p text)
          (when cur (push cur units) (setq cur nil)))
         ;; 直前の単位の続き
         ((and cur (my:translate--comment-adjacent-p (car cur) range))
          (setcdr (car cur) (cdr range))
          (setcdr cur (concat (cdr cur) " " text)))
         (t
          (when cur (push cur units))
          (setq cur (cons (cons (car range) (cdr range)) text))))))
    (when cur (push cur units))
    (seq-filter (lambda (u) (my:translate--comment-prose-p (cdr u)))
                (nreverse units))))

;;; --------------------------------------------------
;;; コメント訳の overlay
;;; --------------------------------------------------

(defvar-local my:translate--overlays nil
  "このバッファに貼ったコメント訳の overlay。")

(defun my:translate--comment-overlay (buffer beg end text)
  "BUFFER の BEG..END のコメントの下に TEXT を overlay で出す。

【重要】BUFFER を必ず明示的に受け取ること。**`make-overlay' の
BUFFER を省略すると「呼ばれた時点の current buffer」に貼られる**が、
この関数は非同期の応答コールバックから呼ばれるので、そのとき
current なのは対象のバッファではない。実測では、ユーザーが開いていた
**dired バッファに訳文が 2 件貼られた**（エラーは出ず、対象の
バッファには 1 件も出ないので、原因が分かりにくい）。
`my:translate--overlays' も `defvar-local' なので同じ罠がある。

【重要】バッファのテキストは変更しない。`after-string' は表示だけで、
ファイルの内容も `point' も動かない。用途が「clone してきた他人の
リポジトリを読む」なので、読んでいる最中に差分を作らない。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; 訳している間に編集されていることがあるので範囲を詰める。
      (let* ((beg (min beg (point-max)))
             (end (min end (point-max))))
        (when (< beg end)
          (let ((indent (save-excursion (goto-char beg) (current-column)))
                (ov (make-overlay beg end buffer t nil)))
            (overlay-put ov 'my:translate t)
            ;; コメントを編集したら訳も消える。空でない overlay なので効く
            ;; (空の overlay に `evaporate' を付けると即座に消えてしまう)。
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'after-string
                         (concat "\n" (make-string indent ?\s)
                                 (propertize
                                  (concat my:translate-comment-prefix text)
                                  'face 'my:translate-comment-face)))
            (push ov my:translate--overlays)
            ov))))))

(defun my:translate-clear-overlays ()
  "このバッファに貼ったコメントの訳を消す。"
  (interactive)
  (let ((n (seq-count #'overlay-buffer my:translate--overlays)))
    (remove-overlays (point-min) (point-max) 'my:translate t)
    (setq my:translate--overlays nil)
    (when (called-interactively-p 'interactive)
      (message (if (zerop n) "コメントの訳は貼っていない"
                 (format "コメントの訳を %d 件消した" n))))))

;;; --------------------------------------------------
;;; markdown を分割する
;;; --------------------------------------------------
;; 【重要】構造の保持はモデルに頼めない。PFML3 に「markdown を壊すな」と
;; 書く欄が無い (docs §6)。**記法・URL・コードブロックはモデルに渡さない**
;; ことで、構造を確率ではなく構成的に保証する。
;;
;; 1 行は (TYPE . 部品のリスト) で表す。TYPE は `table' か `other'。
;; 部品は
;;   (lit . STRING)  そのまま出す（記法・URL・コード）
;;   (tr  . STRING)  訳す
;; 段落なら (other (lit . "- ") (tr . "本文"))、表の行ならセルが交互に並ぶ。
;;
;; 【重要】TYPE を持たせているのは、**どこが表かをパーサだけが決める**ため。
;; 出力を `^|' で探し直して整形すると、コードブロックの中のシェルの
;; パイプ（`    | xargs ...'）を表と誤認して書き換えてしまう（実測で踏んだ）。

(defun my:translate--md-separator-cell-p (cell)
  "CELL が表の区切り（`---' や `:--:'）なら非 nil。"
  (string-match-p "\\`:?-+:?\\'" (string-trim cell)))

(defun my:translate--md-separator-row-p (line)
  "LINE が表の区切り行なら非 nil。"
  (string-match-p "\\`[ \t]*|[ \t:|-]+|?[ \t]*\\'" line))

(defun my:translate--md-prose-remainder (text)
  "TEXT から記法（リンク・画像・インラインコード・URL）を除いた残りを返す。

**バッジだけの行を見分けるために使う。** バッジは
`[![Build Status](img)](link)' の形で、リンクを除くと何も残らない。
`[dash.el](https://...)' のように文の一部なら、周りの散文が残る。"
  (let ((s text))
    (setq s (replace-regexp-in-string "!?\\[[^]]*\\]([^)]*)" " " s))
    (setq s (replace-regexp-in-string "`[^`]*`" " " s))
    (setq s (replace-regexp-in-string "https?://[^ \t]+" " " s))
    s))

(defun my:translate--md-translatable-p (text)
  "TEXT を訳す価値があれば非 nil。

落とすのは、英単語が無いもの、**リンクや画像だけの行（バッジ）**、
HTML だけ、インラインコードだけ、そして**既に日本語のもの**。"
  (let ((s (string-trim text)))
    (and (not (string-empty-p s))
         ;; 記法を除いた残りに英単語があること。これでバッジだけの行と
         ;; 目次（リンクだけの行）がまとめて落ちる。
         (string-match-p "[A-Za-z]\\{2\\}" (my:translate--md-prose-remainder s))
         (not (string-match-p "\\`<.*>\\'" s))
         (<= (my:translate--cjk-count s) my:translate-max-cjk))))

(defun my:translate--md-block-start-p (line)
  "LINE が新しいブロックの始まりなら非 nil（段落の継続行ではない）。"
  (or (string-match-p "\\`[ \t]*\\'" line)
      (string-match-p "\\`[ \t]*\\(```\\|~~~\\)" line)
      (string-match-p "\\`[ \t]*#\\{1,6\\}[ \t]" line)
      (string-match-p "\\`[ \t]*|" line)
      (string-match-p "\\`[ \t]*\\([-*+]\\|[0-9]+[.)]\\)[ \t]" line)
      (string-match-p "\\`[ \t]*>" line)
      (string-match-p "\\`\\(    \\|\t\\)" line)
      (string-match-p "\\`[ \t]*<" line)
      (string-match-p "\\`[ \t]*\\[[^]]+\\]:[ \t]" line)))

(defun my:translate--md-parse (lines)
  "LINES（文字列のリスト）を 1 行ずつ「部品のリスト」にして返す。

表の行だけは `(table . 部品のリスト)' の形になる。どこが表かを
**パーサだけが決める**ための印（冒頭の解説を参照）。

素通しにするのは、フェンスドコードブロック・YAML フロントマター・
字下げコードブロック・HTML・リンクの参照定義・空行・表の区切り行。

【重要】字下げ 4 文字の行は無条件に素通しにする。GFM ではリストの
継続行と字下げコードブロックが紛らわしく、**訳して壊すより
訳さずに残すほうが害が小さい**。"
  (let ((out nil)
        (i 0)
        (n (length lines))
        (fence nil)
        (vec (vconcat lines)))
    ;; YAML フロントマター
    (when (and (> n 0) (string-match-p "\\`---[ \t]*\\'" (aref vec 0)))
      (push (list (cons 'lit (aref vec 0))) out)
      (setq i 1)
      (while (and (< i n) (not (string-match-p "\\`\\(---\\|\\.\\.\\.\\)[ \t]*\\'" (aref vec i))))
        (push (list (cons 'lit (aref vec i))) out)
        (setq i (1+ i)))
      (when (< i n)
        (push (list (cons 'lit (aref vec i))) out)
        (setq i (1+ i))))
    (while (< i n)
      (let ((line (aref vec i)))
        (cond
         ;; フェンスの開閉と中身
         (fence
          (push (list (cons 'lit line)) out)
          (when (string-match-p (concat "\\`[ \t]*" (regexp-quote fence)) line)
            (setq fence nil))
          (setq i (1+ i)))
         ((string-match "\\`[ \t]*\\(```\\|~~~\\)" line)
          (setq fence (match-string 1 line))
          (push (list (cons 'lit line)) out)
          (setq i (1+ i)))
         ;; そのまま出すもの
         ((or (string-match-p "\\`[ \t]*\\'" line)
              (string-match-p "\\`\\(    \\|\t\\)" line)
              (string-match-p "\\`[ \t]*<" line)
              (string-match-p "\\`[ \t]*\\[[^]]+\\]:[ \t]" line))
          (push (list (cons 'lit line)) out)
          (setq i (1+ i)))
         ;; 表。**ここだけ `table' の印を付ける**（他は部品リストのまま。
         ;; 部品の car は `lit' / `tr' なので `table' と混ざらない）。
         ((string-match-p "\\`[ \t]*|" line)
          (push (cons 'table
                      (if (my:translate--md-separator-row-p line)
                          (list (cons 'lit line))
                        (my:translate--md-row-parts line)))
                out)
          (setq i (1+ i)))
         ;; 見出し
         ((string-match "\\`\\([ \t]*#\\{1,6\\}[ \t]+\\)\\(.*\\)\\'" line)
          (let ((pre (match-string 1 line))
                (body (match-string 2 line)))
            (push (if (my:translate--md-translatable-p body)
                      ;; 【重要】原文を併記する。見出しだけ訳すと、目次が
                      ;; 指している原文由来のアンカーが全部外れる (docs §4.5-1)。
                      ;; ただし**リンクや画像を含む見出しには併記しない。**
                      ;; バッジの並んだタイトル行で、同じバッジ群が 2 回
                      ;; 並ぶことになる（実測で踏んだ）。
                      (if (and my:translate-markdown-keep-heading-original
                               (not (string-match-p "](" body)))
                          (list (cons 'lit pre) (cons 'tr body)
                                (cons 'lit (format " (%s)" (string-trim body))))
                        (list (cons 'lit pre) (cons 'tr body)))
                    (list (cons 'lit line)))
                  out))
          (setq i (1+ i)))
         ;; リスト項目 / 引用 / 段落（継続行をまとめて 1 単位にする）
         (t
          (let (pre body)
            (cond
             ((string-match "\\`\\([ \t]*\\([-*+]\\|[0-9]+[.)]\\)[ \t]+\\)\\(.*\\)\\'" line)
              (setq pre (match-string 1 line) body (match-string 3 line)))
             ((string-match "\\`\\([ \t]*>[ \t]*\\)\\(.*\\)\\'" line)
              (setq pre (match-string 1 line) body (match-string 2 line)))
             (t (setq pre "" body line)))
            (setq i (1+ i))
            ;; 【重要】継続行は空白で繋ぐ。改行を残すと、そこで文が
            ;; 切れたものとして訳される。
            (while (and (< i n)
                        (not (my:translate--md-block-start-p (aref vec i))))
              (setq body (concat body " " (string-trim (aref vec i))))
              (setq i (1+ i)))
            (push (if (my:translate--md-translatable-p body)
                      (list (cons 'lit pre) (cons 'tr body))
                    (list (cons 'lit (concat pre body))))
                  out))))))
    (nreverse out)))

(defun my:translate--md-row-parts (line)
  "表の行 LINE をセルごとの部品に分ける。"
  (let* ((trimmed (string-trim line))
         (inner (string-trim trimmed "|" "|"))
         (cells (split-string inner "|"))
         (parts (list (cons 'lit "|"))))
    (dolist (cell cells)
      (let ((c (string-trim cell)))
        (push (cons (if (my:translate--md-translatable-p c) 'tr 'lit)
                    (if (string-empty-p c) " " (concat " " c " ")))
              parts)
        (push (cons 'lit "|") parts)))
    (nreverse parts)))

;;; --------------------------------------------------
;;; 表の桁を取り直す
;;; --------------------------------------------------

(defun my:translate--md-align-table (beg end)
  "BEG..END の表の桁を揃える。

【重要】桁は `length' ではなく `string-width' で測る。訳文は日本語に
なって表示幅が原文とまったく変わるので、取り直さないと崩れる
\(この設定は East Asian Width に手を入れている。docs/japanese/)。"
  (let* ((lines (split-string (buffer-substring-no-properties beg end) "\n" t))
         (rows (mapcar (lambda (l)
                         (mapcar #'string-trim
                                 (split-string (string-trim (string-trim l) "|" "|")
                                               "|")))
                       lines))
         (ncol (apply #'max (mapcar #'length rows)))
         (widths (make-list ncol 3)))
    (dolist (row rows)
      (let ((i 0))
        (dolist (cell row)
          (when (< i ncol)
            (unless (my:translate--md-separator-cell-p cell)
              (setf (nth i widths) (max (nth i widths) (string-width cell)))))
          (setq i (1+ i)))))
    (let ((text
           (mapconcat
            (lambda (row)
              (concat
               "|"
               (let ((i -1))
                 (mapconcat
                  (lambda (cell)
                    (setq i (1+ i))
                    (let ((w (or (nth i widths) 3)))
                      (if (my:translate--md-separator-cell-p cell)
                          (concat (if (string-prefix-p ":" cell) ":" "-")
                                  (make-string (max 1 w) ?-)
                                  (if (string-suffix-p ":" cell) ":" "-"))
                        (concat " " cell
                                (make-string (max 0 (- w (string-width cell))) ?\s)
                                " "))))
                  row "|"))
               "|"))
            rows "\n")))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(defun my:translate--md-align-tables (buffer runs)
  "BUFFER の RUNS（(START-MARKER . END-MARKER) のリスト）の桁を取り直す。

【重要】RUNS はパーサが「表だ」と判定した範囲だけ。**出力を `^|' で
探し直してはいけない。** コードブロックの中のシェルのパイプ
\(`    | xargs -I {} -0 ls -l {}'）を表と誤認して書き換えてしまう
\(実測で踏んだ。原文に表が 1 つも無いのに出力に表の行が 1 つ増えた)。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (run runs)
        (let ((beg (car run)) (end (cdr run)))
          (when (and (marker-position beg) (marker-position end)
                     (< beg end))
            (my:translate--md-align-table beg end)))))))

;;; --------------------------------------------------
;;; help バッファを分割する
;;; --------------------------------------------------
;; 【重要】`*Help*' の本文は絶対に書き換えない。help バッファは
;; `help-xref' ボタン・`help-argument-name' face・`help-mode-revert-buffer'
;; が前提にする元テキストの集合体で、本文を訳文に差し替えると
;; **リンクが全滅する** (docs §6.2)。結果は別バッファに原文と訳文の対で出す。
;;
;; 実際の出力を見て決めた (docs §6.2)。訳す価値があるのは散文だけで、
;; 大半は署名・型・キーバインド表・定型句。

(defconst my:translate--help-skip-regexp
  (rx (or (seq bos "Inferred type:")
          (seq bos "Probably introduced at")
          (seq bos "Its value is")
          (seq bos "[back]")
          (seq bos "Major mode ")
          (seq bos "Key" (+ " ") "Binding" eos)
          " is a " (seq " is a variable defined in")
          "(this binding is currently shadowed)"))
  "これに一致する単位は訳さない。help の定型句。")

(defun my:translate--help-table-line-p (line)
  "LINE がキーバインド表の行なら非 nil。

**タブか、行頭以外の 3 連続以上の空白**で見る。2 連続にすると、
docstring の「文末のピリオドのあと空白 2 つ」(`match.  If START')
を表と誤認する（実測の出力にその形がある）。"
  (string-match-p "\t\\|[^ ]   +" line))

(defun my:translate--help-prose-remainder (text)
  "TEXT から引用符つきシンボルと大文字の引数名を除いた残り。

`‘checkdoc’' がただ並んでいるだけの段落（`describe-mode' の
「キーに割り当てられていないコマンド」）を落とすために使う。

【重要】`case-fold-search' を nil にする。既定の t のままだと
`[A-Z]' が小文字にも一致して**普通の英単語まで消え**、残りが空になって
散文の段落まで落ちる。"
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "‘[^’]*’\\|`[^']*'\\|\\_<[A-Z][A-Z0-9-]+\\_>" " " text)))

(defun my:translate--help-units (beg end)
  "BEG..END（help バッファ）から訳す単位を文字列のリストで返す。

空行と**字下げの変化**で切る。字下げで切るのは、docstring の中の
例示コードが地の文と同じ段落に入っているため（実測）。

  To replace only the first match (if any), make REGEXP match up to \\='
  and replace a sub-expression, e.g.
    (replace-regexp-in-string ...)      ← ここで切りたい"
  (let ((lines (split-string (buffer-substring-no-properties beg end) "\n"))
        (units nil)
        (cur nil)
        (indent nil))
    (cl-labels
        ((flush ()
           (when cur
             (push (nreverse cur) units)
             (setq cur nil indent nil))))
      (dolist (line lines)
        (if (string-match-p "\\`[ \t]*\\'" line)
            (flush)
          (let ((this (- (length line) (length (string-trim-left line)))))
            (when (and indent (/= this indent)) (flush))
            (setq indent this)
            (push line cur))))
      (flush))
    (seq-filter
     #'identity
     (mapcar
      (lambda (group)
        (let ((text (string-trim (mapconcat #'string-trim group " "))))
          (and (my:translate--help-prose-p text group) text)))
      (nreverse units)))))

(defun my:translate--help-prose-p (text group)
  "TEXT（GROUP を 1 行に畳んだもの）を訳す価値があれば非 nil。"
  (and (not (string-empty-p text))
       ;; 署名と例示コード
       (not (string-prefix-p "(" text))
       (not (string-prefix-p "=>" text))
       (not (string-match-p my:translate--help-skip-regexp text))
       ;; キーバインド表（過半数の行が表の形）
       (< (seq-count #'my:translate--help-table-line-p group)
          (max 1 (/ (length group) 2.0)))
       ;; シンボルが並んでいるだけ
       (string-match-p "[A-Za-z]\\{2\\}"
                       (my:translate--help-prose-remainder text))
       (>= (my:translate--letters text) my:translate-help-min-letters)
       (<= (my:translate--cjk-count text) my:translate-max-cjk)))

;;; --------------------------------------------------
;;; 直列に訳す
;;; --------------------------------------------------

(defun my:translate--sequential (units profile on-each &optional on-done chain)
  "UNITS を順に訳す。UNITS の各要素は (DATA . TEXT) のコンス。

ON-EACH は 1 単位ごとに
 (DATA TEXT TRANSLATION ERROR INDEX TOTAL) の 6 引数で呼ばれる。
TRANSLATION と ERROR はどちらか一方が nil。
全部終わったら ON-DONE を (TOTAL ERRORS) で呼ぶ。

CHAIN が非 nil なら、直前の単位の原文を `context' として渡す。
地の文が続く散文では訳語が安定するが、互いに無関係な単位
 (ばらばらのコメントなど) では混ぜない方がよい。

1 つずつ直列に投げる。理由は 2 つ。順序が保てること、そして
GPU への要求は llama-server の側で直列化されるので、同時に投げても
速くならないこと。

1 単位が訳せるたびに ON-EACH が呼ばれるので、それが進捗表示になる。
トークン単位の逐次表示 (SSE) は要らない (docs §4.7)。"
  (let ((total (length units))
        (index 0)
        (errors 0))
    (cl-labels
        ((step
           (rest previous)
           (if (null rest)
               (when on-done (funcall on-done total errors))
             (setq index (1+ index))
             (let ((data (caar rest))
                   (src (cdar rest))
                   (i index))
               (my:translate--post
                profile src (and chain previous)
                (lambda (translation error)
                  (when error (setq errors (1+ errors)))
                  (funcall on-each data src translation error i total)
                  (step (cdr rest) src)))))))
      (step units nil))))

;;; --------------------------------------------------
;;; コマンド: リージョン / 段落
;;; --------------------------------------------------

(defun my:translate--run (segments buffer profile &optional extra chain)
  "SEGMENTS を順に訳して BUFFER に足す。PROFILE のサーバを使う。
EXTRA は追加で守る正規表現、CHAIN が非 nil なら直前の原文を文脈に渡す。"
  (my:translate--sequential
   (mapcar (lambda (s) (cons nil s)) segments)
   profile
   (lambda (_data src translation error index total)
     (message "翻訳中... %d/%d" index total)
     (if error
         (my:translate--append
          buffer
          (format "── %d/%d ──\n%s\n\n!! 訳せなかった: %s\n\n"
                  index total src error))
       (let ((lost (my:translate--lost src translation extra)))
         (my:translate--append
          buffer
          (format "── %d/%d ──\n%s\n\n%s\n" index total src translation)
          ;; 識別子や URL が落ちていたら黙って通さない。
          ;; 壊れた訳文を信じるより、言われた方が安全。
          (if lost
              (format "\n!! 原文にあって訳文に無い: %s\n\n"
                      (mapconcat #'identity lost " "))
            "\n")))))
   (lambda (total errors)
     (my:translate--append buffer "\n")
     (message "翻訳が終わった (%d 段落%s)" total
              (if (zerop errors) "" (format "、%d 件失敗" errors))))
   chain))

(defun my:translate--pick-profile (arg)
  "前置引数 ARG からプロファイルを決める。"
  (if arg my:translate-alt-profile my:translate-default-profile))

(defun my:translate--check-ready (profile)
  "PROFILE で翻訳できる状態か確かめる。駄目なら `user-error'。

**コマンドの入口で呼ぶ。** ここで落とさないと「llama-server が無い」が
結果バッファや overlay の中に書かれることになる。"
  (unless (my:translate--llama-server)
    (user-error "llama-server が見つからない。%s か PATH に置く"
                (car my:translate-search-dirs)))
  (unless (my:translate--model profile)
    (user-error "%s に %s を含む .gguf が無い"
                my:translate-model-dir
                (plist-get (my:translate--profile profile) :match))))

(defun my:translate-dwim (&optional arg)
  "リージョンか、ポイントのある段落を訳して別バッファに出す。

リージョンがあればそれを、無ければポイントのある段落を訳す。
前置引数 ARG を付けると `my:translate-alt-profile' (丁寧なモデル) を使う。

【重要】元のバッファは書き換えない。"
  (interactive "P")
  (my:translate--check-ready (my:translate--pick-profile arg))
  (let* ((profile (my:translate--pick-profile arg))
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
      (my:translate--run segments buffer profile nil t))))

(defun my:translate-help (&optional arg)
  "help バッファの散文を訳して、別バッファに原文と訳文の対で出す。

`*Help*' を見ている状態で呼ぶ。`describe-function' /
`describe-variable' / `describe-mode' のどれでも使える。
**リージョンがあればその範囲だけ**訳す（`describe-mode' は長いので、
知りたいところだけ選ぶとよい）。

【重要】`*Help*' の本文には一切触れない。本文を訳文に差し替えると
`help-xref' ボタンが全滅するため（docs §6.2）。

訳すのは散文だけ。署名・型・キーバインド表・定型句・例示コードと、
**大文字の引数名やシンボルだけが並んだ段落**は飛ばす。
訳文から引数名やシンボルが消えていたら、その単位は印を付ける。"
  (interactive "P")
  (let* ((profile (my:translate--pick-profile arg))
         (help (cond
                ((derived-mode-p 'help-mode) (current-buffer))
                ((get-buffer (help-buffer)) (get-buffer (help-buffer)))
                (t (user-error "help バッファが無い")))))
    (my:translate--check-ready profile)
    (let ((segments
           (with-current-buffer help
             ;; 【重要】同じ段落を何度も訳さない。`describe-mode' は
             ;; 有効なマイナーモードぶんの docstring を全部並べるので、
             ;; 「The mode's hook is called both when the mode is enabled
             ;; and when it is disabled.」が実測で約 50 回現れる。
             (delete-dups
              (if (use-region-p)
                  (my:translate--help-units (region-beginning) (region-end))
                (my:translate--help-units (point-min) (point-max)))))))
      (unless segments
        (user-error "訳す散文が無い（署名と表だけ、あるいは既に日本語）"))
      (when (and (> (length segments) my:translate-ask-threshold)
                 (not (yes-or-no-p
                       (format "%d 段落を訳す (%s で約 %d 秒)? "
                               (length segments) (my:translate--label profile)
                               (round (* (length segments)
                                         (if (eq profile my:translate-default-profile)
                                             0.5 1.6)))))))
        (user-error "やめた（リージョンを選べばその範囲だけ訳せる）"))
      (let ((buffer (my:translate--result-buffer help)))
        (my:translate--append
         buffer
         (format "%s / %s → %s%s\n\n"
                 (buffer-name help) (my:translate--label profile)
                 my:translate-target-lang
                 (if my:translate-form (format " (%s)" my:translate-form) "")))
        (display-buffer buffer)
        ;; 文脈は繋がない。help の段落は互いに独立していることが多い。
        (my:translate--run segments buffer profile
                           my:translate--protect-help nil)))))

(defun my:translate-comments (&optional arg)
  "リージョンかバッファのコメントを訳し、各コメントの下に overlay で出す。

リージョンがあればその範囲、無ければバッファ全体。前置引数 ARG を
付けると `my:translate-alt-profile' (丁寧なモデル) を使う。

消すのは `my:translate-clear-overlays' (`C-c T x')。

【重要】バッファのテキストは変更しない。表示だけを足す。

訳さないものがある (`my:translate--comment-prose-p')。機械向けの指示、
英字が足りない断片、**既に日本語で書かれているコメント**は飛ばす。"
  (interactive "P")
  (let ((profile (my:translate--pick-profile arg))
        ;; 【重要】対象のバッファをここで捕まえる。応答のコールバックは
        ;; 別のバッファが current の状態で走る (§`my:translate--comment-overlay')。
        (target (current-buffer)))
    (my:translate--check-ready profile)
    (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
           (end (if (use-region-p) (region-end) (point-max)))
           (units (my:translate--comment-units beg end)))
      (unless units
        (user-error "訳すコメントが無い (既に日本語か、英字が足りない)"))
      ;; 貼り直しなので、同じ範囲の古い訳は先に消す。
      (remove-overlays beg end 'my:translate t)
      (setq my:translate--overlays
            (seq-filter #'overlay-buffer my:translate--overlays))
      (let ((n (length units)))
        (when (and (> n my:translate-ask-threshold)
                   (not (yes-or-no-p
                         (format "コメント %d 件を訳す (%s で約 %d 秒)? "
                                 n (my:translate--label profile)
                                 (round (* n (if (eq profile
                                                     my:translate-default-profile)
                                                 0.4 1.3)))))))
          (user-error "やめた"))
        (my:translate--sequential
         units profile
         (lambda (range src translation error index total)
           (message "コメントを翻訳中... %d/%d" index total)
           (if error
               ;; 訳せなかった単位は印だけ出す。黙って飛ばすと
               ;; 「ここは訳す価値が無いと判定された」と区別が付かない。
               (my:translate--comment-overlay
                target (car range) (cdr range)
                (format "(訳せなかった: %s)" error))
             (let ((lost (my:translate--lost src translation)))
               (my:translate--comment-overlay
                target (car range) (cdr range)
                (if lost
                    (format "%s  [原文にあって訳文に無い: %s]"
                            translation (mapconcat #'identity lost " "))
                  translation)))))
         (lambda (total errors)
           (message "コメントの翻訳が終わった (%d 件%s)" total
                    (if (zerop errors) "" (format "、%d 件失敗" errors))))
         ;; 互いに無関係なコメントなので context は繋がない。
         nil)))))

(defun my:translate-markdown (&optional arg)
  "markdown のバッファ（かリージョン）を訳して、別バッファに出す。

**記法・コードブロック・URL はモデルに渡さず、散文だけを訳す**ので、
構造は確率ではなく構成的に保たれる（docs §6.1）。
見出しには原文を併記する（`my:translate-markdown-keep-heading-original'）。
表はセルごとに訳し、最後に桁を取り直す。

前置引数 ARG を付けると `my:translate-alt-profile'（丁寧なモデル）を使う。

【重要】元のバッファもファイルも書き換えない。結果は新しいバッファに出る。
訳し終わるまでは原文が入っていて、訳せた単位から置き換わる。"
  (interactive "P")
  (let ((profile (my:translate--pick-profile arg))
        (src (current-buffer)))
    (my:translate--check-ready profile)
    (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
           (end (if (use-region-p) (region-end) (point-max)))
           (parsed (my:translate--md-parse
                    (split-string (buffer-substring-no-properties beg end) "\n")))
           (out (get-buffer-create (format "*translation: %s*" (buffer-name src))))
           (units nil)
           (runs nil)                   ; 表の範囲 (START . END) のリスト
           (run-start nil))
      ;; まず原文のまま流し込み、訳す部分にマーカーを打つ。
      ;; 訳せた単位から置き換わるので、それが進捗表示になる。
      (with-current-buffer out
        (when (fboundp 'markdown-mode) (markdown-mode))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (entry parsed)
            ;; 表の行は (table . 部品) の形。連続する表の範囲を覚えておき、
            ;; 訳し終わってから桁を取り直す。
            (let* ((table (eq (car entry) 'table))
                   (parts (if table (cdr entry) entry)))
              (cond ((and table (null run-start))
                     (setq run-start (point-marker)))
                    ((and (not table) run-start)
                     (push (cons run-start (copy-marker (max (point-min) (1- (point)))))
                           runs)
                     (setq run-start nil)))
              (dolist (part parts)
                (if (eq (car part) 'lit)
                    (insert (cdr part))
                (let ((s (point-marker)))
                  (insert (cdr part))
                  ;; 【重要】`insertion-type' は両方とも nil のままにする
                  ;; (`point-marker' の既定)。E に t を立ててはいけない。
                  ;; 流し込みは末尾への追記の繰り返しなので、**t を立てた
                  ;; マーカーは以後の追記のたびに前へ送られ、最後には
                  ;; `point-max' まで引きずられる**。すると置換のときの
                  ;; `delete-region' が「その単位から文末まで」を消す。
                  ;; 実測では 26805 文字の出力が 837 文字まで溶けた。
                  ;; 置換が終わった E を読み直すことは無いので、t は要らない。
                  (push (cons (cons s (point-marker)) (cdr part)) units))))
              (insert "\n")))
          ;; 末尾が表で終わっていた場合
          (when run-start
            (push (cons run-start (copy-marker (max (point-min) (1- (point))))) runs)
            (setq run-start nil)))
        (setq units (nreverse units)
              runs (nreverse runs))
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (unless units
        (kill-buffer out)
        (user-error "訳すものが無い（既に日本語か、記法だけ）"))
      (display-buffer out)
      (let ((kept 0))
        (my:translate--sequential
         units profile
         (lambda (markers source translation error index total)
           (message "markdown を翻訳中... %d/%d" index total)
           (let ((text
                  (cond
                   (error nil)
                   ;; 識別子や URL が落ちた訳文は採らない。壊れた訳文を
                   ;; 入れるより、その単位だけ英語のまま残すほうが安全。
                   ((my:translate--lost source translation) nil)
                   (t translation))))
             (if (not text)
                 (setq kept (1+ kept))
               (when (buffer-live-p out)
                 (with-current-buffer out
                   (let ((inhibit-read-only t)
                         (s (car markers))
                         (e (cdr markers)))
                     (save-excursion
                       (delete-region s e)
                       (goto-char s)
                       ;; 【重要】訳文は折り返さない。GFM は段落内の
                       ;; 単独改行を空白 1 個として描画するので、
                       ;; 日本語の文の途中で改行すると字間に空白が入る。
                       (insert text))))))))
         (lambda (total errors)
           (my:translate--md-align-tables out runs)
           (when (buffer-live-p out)
             (with-current-buffer out (set-buffer-modified-p nil)))
           (message "markdown の翻訳が終わった (%d 単位%s%s)"
                    total
                    (if (zerop errors) "" (format "、%d 件失敗" errors))
                    (if (zerop kept) ""
                      (format "、%d 件は原文のまま残した" kept))))
         t)))))

(use-package emacs
  :bind (("C-c T t" . my:translate-dwim)
         ("C-c T c" . my:translate-comments)
         ("C-c T m" . my:translate-markdown)
         ("C-c T h" . my:translate-help)
         ("C-c T x" . my:translate-clear-overlays)
         ("C-c T s" . my:translate-server-status)
         ("C-c T k" . my:translate-stop-servers)))

(provide 'my-translate)
;;; my-translate.el ends here
