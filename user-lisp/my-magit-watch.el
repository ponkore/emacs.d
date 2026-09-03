;;; my-magit-watch.el --- ワークツリーの変化で magit を自動更新する  -*- lexical-binding: t -*-

;;; Commentary:

;; ワークツリー / インデックス / HEAD の変化を検知して、表示中の magit
;; バッファを自動で最新化する。`g' を押さなくてよくする。
;;
;; 段階 2a (my-gitd.el) でリフレッシュが 1.7 秒 → 0.6 秒になったので実用に
;; 耐えるようになった。逆に言えば 2a 無しではこれは入れられなかった。
;;
;; Windows のみ。`w32notify-add-watch' に `subtree' を渡すと 1 個の watch で
;; 配下を再帰的に監視できる (追加コスト 0.2 ms)。**`filenotify.el' の
;; `file-notify-add-watch' は `subtree' を渡さないので非再帰**であり、
;; 汎用 API 経由では使えない。
;;
;; 設計と実測は tmp/magit-autorefresh-stage1-design.md を参照。
;;
;;; gitd との協調 (段階 2b)
;;
;; `my-gitd' のデーモン側キャッシュは (リポジトリ, トークン, コマンド) で引く。
;; そのトークンをここが持っている (`my:magit-watch-repo-serial')。
;; **進め忘れると gitd が古い答えを返し続ける**ので、判断に迷ったら進める。
;;
;; タイマーは 2 本ある。どちらもイベントごとに張り直す。
;;
;;   0.1 秒 (`my:magit-watch-prewarm-delay')  先読みを頼む
;;   0.4 秒 (`my:magit-watch-debounce')       リフレッシュする
;;
;; 差の 0.3 秒が先読みの持ち時間で、その間にデーモンが 28 コマンドを 8 並列で
;; 走らせてキャッシュを埋める。おかげでリフレッシュが 0.6 秒 → 50〜70 ms。
;; 0.1 秒待つのは、1 ファイルの保存で w32notify が約 10 件のイベントを出すため
;; (最初の 1 件で頼むと残り 9 件でトークンが進んで無駄になる)。
;;
;;; 【重要】読み取りだけの git もイベントを出す
;;
;; 実測 (2026-09): `status --porcelain' で 4 件 (`.git' ×2 と
;; `.git\index.lock' ×2)、`update-index --refresh' は何もしない場合でも 3 件。
;; つまり **リフレッシュも先読みも、必ず自分でイベントを作る**。
;;
;; 分類上これらはすべて `suspect' なので、`suspect' だけを狙って 2 つ手当てする。
;;
;;   1. `suspect' ではトークンを進めない。進めると自分のリフレッシュや
;;      先読みが自分のキャッシュを壊す
;;   2. `suspect' では既に張ってあるタイマーを延長しない。「何かが起きたかも」
;;      以上のことを言わないので、評価が予約済みなら足す情報が無い。
;;      **延長すると先読みが自分自身を呼び続け、リフレッシュが永久に来ない**
;;      (実際にそうなった。先読み 63 回・リフレッシュ 0 回)
;;
;; 1 の代わりに、`my:magit-watch--fire' がフィンガープリントの不一致を
;; 見つけたときにトークンを進める。イベントが落ちても取り返せるという
;; 段階 1 の性質はこれで保たれる。
;;
;;; 自励振動について (重要)
;;
;; `magit-refresh-buffer' を 1 回走らせるだけで毎回 7 件のイベントが出る
;; (`.git/index.lock' が 4 件、`.git' ディレクトリ自身が 3 件)。素直に繋ぐと
;; 「イベント → リフレッシュ → イベント」で回り続ける。二重に止めている:
;;
;;   1. パスによる分類 (`my:magit-watch--classify') で「確定」と「合図だけ」を
;;      分ける。リフレッシュが出す 7 件はすべて「合図だけ」になる
;;   2. 合図だけのイベントは、`.git/index' と `.git/HEAD' の (mtime . size) を
;;      前回のリフレッシュ直後に取ったスナップショットと比べ、
;;      変わっていなければ無視する
;;
;; 2 が magit 自身の stage / unstage による二重リフレッシュも同時に消す。
;; magit は書き込んでから自分でリフレッシュするので、そのとき
;; `magit-refresh-buffer-hook' でスナップショットを取り直しており、
;; あとから届くイベントは必ず一致する。外部の `git add' なら一致しない。
;; **時刻ではなく内容で判定しているので、イベントの配送順に依存しない。**
;;
;;; .gitignore の扱い
;;
;; 監視は `.gitignore' を知らないので、`build/' に 200 ファイル作ると
;; 分類後でも 1001 件のイベントが残る。git は無視するのに監視は拾うため、
;; そのままだとビルド中に無関係なリフレッシュが延々と走る。
;;
;; パスだけのフィルタでは落とせない (どのパターンが書いてあるか分からない)
;; ので git に聞くしかないが、**イベントごとに聞いてはいけない**。
;; 3 段構えで濃縮している:
;;
;;   1. コールバックでは「変化したディレクトリ」をハッシュに入れるだけ。
;;      ビルドは数千ファイルを出すが **ディレクトリは数個**なので濃縮できる
;;   2. デバウンス後に、未知のディレクトリだけを `git check-ignore' へ
;;      **まとめて 1 回**渡す
;;   3. 結果はリポジトリごとにキャッシュする。ビルド中は同じディレクトリが
;;      延々と来るので、**定常状態では git を 1 回も呼ばない**
;;
;; キャッシュは `.gitignore' / `.git/info/exclude' / `.git/config' の変更で捨てる。
;;
;; 判定はディレクトリ単位なので、**リポジトリ直下の無視されるファイル**
;; (ルートの `*.log' など) はパス単位で見る。ディレクトリ数が
;; `my:magit-watch--wt-dirs-limit' を超えたら判断を諦めてリフレッシュする。

;;; Code:

(require 'cl-lib)

(declare-function magit-toplevel "magit-git")
(declare-function magit-gitdir "magit-git")
(declare-function magit-refresh-buffer "magit-mode")
(declare-function magit-process-git "magit-process")
(defvar magit-git-global-arguments)
(defvar magit-pre-refresh-hook)

(defvar my:magit-watch-mode)            ; define-minor-mode で定義される

(defgroup my:magit-watch nil
  "ワークツリーの変化で magit を自動更新する。"
  :group 'magit)

(defcustom my:magit-watch-debounce 0.4
  "最後のイベントからこの秒数だけ待ってからリフレッシュする。"
  :type 'number)

(defcustom my:magit-watch-prewarm-delay 0.1
  "最後のイベントからこの秒数で `my-gitd' に先読みを頼む。

`my:magit-watch-debounce' より **短く**すること。差が先読みの持ち時間になる。
既定では 0.4 - 0.1 = 0.3 秒あり、29 コマンドを 8 並列で走らせるには足りる。

0.1 秒待つのは、**1 ファイルの保存で w32notify が約 10 件のイベントを出す**
ため。イベント 1 件ごとにトークンが上がるので、最初の 1 件で先読みを頼むと
残り 9 件で無効化されてしまう。

nil にすると先読みを頼まない (リフレッシュ直前には必ず頼むので、
遅くなるだけで壊れはしない)。"
  :type '(choice number (const nil)))

(defcustom my:magit-watch-min-interval 2.0
  "自動リフレッシュの最短間隔 (秒)。

`.gitignore' の判定 (Commentary 参照) で無関係な変化はほぼ落ちるが、
追跡対象のファイルが本当に大量に書き換わる場合の上限としてここで抑える。"
  :type 'number)

(defcustom my:magit-watch-visible-only t
  "非 nil なら、ウィンドウに表示されている magit バッファだけ更新する。

表示していないバッファは次に表示されたときに更新すればよい。
ビルド中の暴走を抑える一番効く手でもある。"
  :type 'boolean)

;;; ---------------------------------------------------------------- 状態

(cl-defstruct (my:magit-watch-repo (:constructor my:magit-watch--make-repo))
  root          ; toplevel (末尾 "/")
  gitdir        ; .git ディレクトリ (絶対、末尾 "/")
  desc          ; w32notify のディスクリプタ
  fp            ; .git/index と .git/HEAD の (mtime . size)。判定の要
  serial        ; 状態の通し番号。gitd のキャッシュのトークンになる
  pending       ; 未処理イベントの分類 (シンボルのリスト)
  wt-dirs       ; この窓で変化したワークツリーのディレクトリ (ハッシュ)
  wt-overflow   ; wt-dirs が上限を超えたら t (判断を諦める)
  ign-cache     ; 相対ディレクトリ -> ignored かどうか (ハッシュ)
  timer         ; リフレッシュ用のデバウンスタイマー
  prewarm-timer ; 先読み用の短いタイマー
  prewarmed     ; この窓で先読みを頼んだか (1 窓 1 回に絞る)
  last-refresh) ; 最後にリフレッシュした時刻 (レート制限用)

(defvar my:magit-watch--repos (make-hash-table :test #'equal)
  "root -> `my:magit-watch-repo'。")

(defvar my:magit-watch--stats
  (list :events 0 :classified 0 :refreshed 0 :skipped 0 :deferred 0
        :throttled 0 :prewarmed 0 :ign-error 0)
  "統計。`my:magit-watch-stats' で表示する。")

(defvar my:magit-watch--ign-warned nil
  "check-ignore の失敗を 1 度だけ知らせるためのフラグ。")

;;; ------------------------------------------------ トークン (gitd との接点)

;; `my-gitd' のデーモン側キャッシュは (repo, token, コマンド) で引く。
;; token を進めるのはここだけで、進め忘れると **古い答えが返り続ける**。
;; 逆に進めすぎても遅くなるだけなので、迷ったら進める。

(declare-function my:gitd-prewarm "my-gitd")
(declare-function my:gitd-forget "my-gitd")

(defun my:magit-watch-scope (&optional dir)
  "DIR を含む監視中リポジトリの (ROOT . TOKEN) を返す。無ければ nil。

`my-gitd' が `git/run' のたびに呼ぶ。**git を呼んではいけない**
\(`magit-process-file' に再入する)。監視表を引くだけで済ませている。"
  (and my:magit-watch-mode
       (when-let* ((repo (my:magit-watch--repo-of (or dir default-directory))))
         (cons (my:magit-watch-repo-root repo)
               (or (my:magit-watch-repo-serial repo) 0)))))

(defun my:magit-watch-bump (&optional dir)
  "DIR のリポジトリの状態が変わったことにする (トークンを進める)。"
  (when-let* ((repo (my:magit-watch--repo-of (or dir default-directory))))
    (cl-incf (my:magit-watch-repo-serial repo))))

(defun my:magit-watch--ask-prewarm (repo)
  "REPO の現在のトークンで先読みを頼む。**純粋な最適化**。

送らなくても遅くなるだけで壊れない。逆に送りすぎると、ビルド中に
無駄な git 起動が増える。"
  (when (fboundp 'my:gitd-prewarm)
    (my:gitd-prewarm (my:magit-watch-repo-root repo)
                     (or (my:magit-watch-repo-serial repo) 0))))

;;; ---------------------------------------------------------------- 分類

(defun my:magit-watch--classify (rel)
  "監視ルートからの相対パス REL を分類する。
w32notify の FILE は相対パスで区切りはバックスラッシュ。

戻り値:

  nil        無視してよい
  `worktree' ワークツリーのファイル。無条件にリフレッシュ
  `meta'     .git 配下の HEAD / refs / logs など。無条件にリフレッシュ
  `index'    .git/index。フィンガープリントを見て判断する
  `suspect'  それ自体は何も証明しないが、**何かが起きた合図**。
             フィンガープリントを見て判断する

`suspect' が要る理由は **w32notify がイベントを取りこぼすから**。
実測で 1000 ファイル作成に対しイベントは 4095 件しか届かなかった
\(1 ファイルあたり 10 件出るので 1 万件が期待値)。
`ReadDirectoryChangesW' のバッファ溢れで、これは避けられない。

つまり `.git/HEAD' のような決め手のイベントが落ちることがありうる。
一方で `.git' ディレクトリ自身の mtime 更新は粗いぶん残りやすい。
そこで「何かは起きた」という合図として拾い、`.git/index' と `.git/HEAD' の
mtime を見て判断する。ブランチ切替では両方とも必ず変わることを確認済み。

なお `.git/refs/tags/*' だけが変わるような操作 (`git tag') で、その meta
イベントが落ちてしまうと取りこぼす。フィンガープリントに refs 全体を
入れるのはコストが見合わないので、そこは次のイベントで回復するのに任せる。"
  (let ((f (subst-char-in-string ?\\ ?/ rel)))
    (cond
     ;; --- 無視 ---
     ;; gc / fetch で大量に出る (実測: gc 1 回で 178 件)。
     ;; オブジェクトの増減は index や refs の変化で必ず伴われる
     ((string= f ".git/objects") nil)
     ((string-prefix-p ".git/objects/" f) nil)
     ;; Emacs のロックファイル・自動保存・バックアップ
     ((string-match-p "\\(?:\\`\\|/\\)\\.#" f) nil)
     ((string-match-p "\\(?:\\`\\|/\\)#.*#\\'" f) nil)
     ((string-suffix-p "~" f) nil)
     ;; --- 合図だけ (フィンガープリントで判定) ---
     ;; .git ディレクトリ自身の mtime。リフレッシュのたびに出る
     ((string= f ".git") 'suspect)
     ;; index.lock / HEAD.lock / packed-refs.lock / AUTO_MERGE.lock など。
     ;; **.git/ の下に限ること。** ワークツリーには Cargo.lock や
     ;; flake.lock といった追跡対象のファイルがある
     ((and (string-prefix-p ".git/" f) (string-suffix-p ".lock" f)) 'suspect)
     ((string= f ".git/gc.pid") 'suspect)
     ;; --- 確定 ---
     ((string= f ".git/index") 'index)
     ((string-prefix-p ".git/" f) 'meta)      ; HEAD / refs / logs / packed-refs
     (t 'worktree))))

;;; ---------------------------------------------------------------- 判定

(defun my:magit-watch--fingerprint (repo)
  "REPO の状態フィンガープリントを返す。git は呼ばない (stat 2 回だけ)。

`.git/index' と `.git/HEAD' の (mtime . size) を見る。

- index: stage / unstage / commit で変わる
- HEAD:  ブランチの切り替えで**必ず**変わる（実測で確認）"
  (and-let* ((gitdir (my:magit-watch-repo-gitdir repo)))
    (mapcar (lambda (name)
              (and-let* ((a (file-attributes (expand-file-name name gitdir))))
                (cons (file-attribute-modification-time a)
                      (file-attribute-size a))))
            '("index" "HEAD"))))

;;; ------------------------------------------------ .gitignore (課題 3)

(defconst my:magit-watch--wt-dirs-limit 64
  "1 つのデバウンス窓で覚えるワークツリーのディレクトリ数の上限。

超えたら `.gitignore' の判断を諦めてリフレッシュする（安全側）。
実運用ではビルド出力は数個のディレクトリに集中するので、まず超えない。")

(defun my:magit-watch--ign-key (rel)
  "REL (相対パス、区切りは /) から `.gitignore' 判定のキーを作る。

ディレクトリ単位にまとめるのが肝。ビルドは数千ファイルを出すが
**ディレクトリは数個**なので、これで濃縮できる。
ルート直下のファイルはまとめようがないのでパスそのものを使う。"
  (if-let* ((dir (file-name-directory rel)))
      (directory-file-name dir)
    rel))

(defun my:magit-watch--ignored-p (repo keys)
  "KEYS (相対パスのリスト) が全部 git に無視されるなら非 nil。

判定はキャッシュする。ビルド中は同じディレクトリが延々と来るので、
**定常状態では git を 1 回も呼ばない**。未知のキーがあるときだけ
`git check-ignore' をまとめて 1 回呼ぶ。"
  (let* ((cache (or (my:magit-watch-repo-ign-cache repo)
                    (setf (my:magit-watch-repo-ign-cache repo)
                          (make-hash-table :test #'equal))))
         (unknown (seq-filter (lambda (k) (eq (gethash k cache 'unset) 'unset)) keys)))
    (when unknown
      (let* ((default-directory (my:magit-watch-repo-root repo))
             ;; `magit-git-global-arguments' をそのまま使ってはいけない。
             ;;   - `--literal-pathspecs' を check-ignore は受け付けず
             ;;     "pathspec magic not supported by this command" で落ちる
             ;;   - `-z' は `--stdin' とセットでないと
             ;;     "-z only makes sense with --stdin" で落ちる
             ;; そこで最小限に絞る。`core.quotePath=false' は日本語パスが
             ;; C 形式でクォートされて突き合わせに失敗するのを防ぐため。
             (magit-git-global-arguments '("--no-pager" "-c" "core.quotePath=false"))
             (exit nil)
             (out (magit--with-temp-process-buffer
                    (setq exit (magit-process-git t (list "check-ignore" "--" unknown)))
                    (buffer-string)))
             ;; 該当なしの終了コードは 1。128 以上は本物のエラー
             (ignored (and (memq exit '(0 1))
                           (split-string out "\n" t "[ \t\r]+"))))
        (when (and (integerp exit) (>= exit 128))
          (cl-incf (plist-get my:magit-watch--stats :ign-error))
          (unless my:magit-watch--ign-warned
            (setq my:magit-watch--ign-warned t)
            (message "magit-watch: git check-ignore が失敗しました (exit %s)。%s"
                     exit "gitignore の判定を諦めて毎回リフレッシュします")))
        ;; check-ignore は無視されるものだけを返す。
        ;; 返らなかったものは「無視されない」として覚える。
        ;; 失敗したときも「無視されない」= 安全側 (リフレッシュする) に倒れる。
        (dolist (k unknown)
          (puthash k (and (member k ignored) t) cache))))
    (seq-every-p (lambda (k) (gethash k cache)) keys)))

(defun my:magit-watch--worktree-relevant-p (repo)
  "この窓のワークツリー変化が git から見て意味があるなら非 nil。

`.gitignore' で無視されるディレクトリの変化しか無ければ nil を返す。
これが無いと、ビルド中に無関係なリフレッシュが延々と走る
（実測: build/ に 200 ファイルで分類後 1001 件のイベント）。"
  (or (my:magit-watch-repo-wt-overflow repo)
      (let ((h (my:magit-watch-repo-wt-dirs repo)))
        (or (null h)
            (zerop (hash-table-count h))
            (not (my:magit-watch--ignored-p repo (hash-table-keys h)))))))

(defun my:magit-watch--stale-p (repo)
  "REPO をリフレッシュする必要があるなら、その理由のシンボルを返す。

  `meta'        .git のメタ情報が動いた
  `worktree'    ワークツリーが動いた (gitignore されていないもの)
  `fingerprint' 決め手のイベントは無いが、内容を見たら変わっていた
  nil           更新は要らない

`index' と `suspect' しか無いときは、前回のリフレッシュ直後に取った
フィンガープリントと比べて本当に変わったかを見る。ここで

  - `magit-refresh-buffer' 自身が出すイベント (自励振動)
  - magit の stage / unstage 直後のイベント (二重リフレッシュ)

の両方が落ちる。どちらも magit が既にリフレッシュ済みで、その時点の
スナップショットと一致するため。**時刻ではなく内容で見ているので、
イベントの配送順に依存しない。**

**副作用を持たせないこと。** 先読みタイマーとリフレッシュタイマーの
両方から、同じ窓で何度も呼ばれる。"
  (let ((pending (my:magit-watch-repo-pending repo)))
    (cond
     ((null pending) nil)
     ((memq 'meta pending) 'meta)
     ;; ワークツリーの変化は、gitignore されているものだけなら無視する
     ((and (memq 'worktree pending)
           (my:magit-watch--worktree-relevant-p repo))
      'worktree)
     ((not (equal (my:magit-watch-repo-fp repo)
                  (my:magit-watch--fingerprint repo)))
      'fingerprint))))

(defun my:magit-watch--allowed-p ()
  "今リフレッシュしてよいなら非 nil。

**ここに置いてよいのは「ユーザの操作を邪魔しないため」の条件だけ。**
いずれもユーザが操作をやめれば自然に解消するので、待ち直せば必ず進む。

`frame-focus-state' をここに入れてはいけない。フォーカスが外れている間は
永久に偽のままなので、待ち直しが終わらず **フォーカスを失った時点から
二度と更新されなくなる**（実測で 0.3 秒ごとに再アームし続けた）。
背景での CPU 消費は `my:magit-watch-visible-only' と
`my:magit-watch-min-interval' で抑える。"
  (and (not (minibufferp))
       (not (and (boundp 'transient--window) transient--window))
       (not (bound-and-true-p isearch-mode))
       (not defining-kbd-macro)
       (not executing-kbd-macro)
       (not (region-active-p))
       (not (input-pending-p))))

;;; ---------------------------------------------------------------- 更新

(defun my:magit-watch--buffers (repo)
  "REPO に属する magit バッファのうち、更新対象のものを返す。"
  (let ((root (my:magit-watch-repo-root repo)))
    (seq-filter
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'magit-mode)
                   ;; リフレッシュ関数を持たないバッファ (magit-process-mode
                   ;; など) は magit-refresh-buffer が no-op になる。
                   ;; 呼ぶだけ無駄なので外す
                   (or (not (fboundp 'magit--refresh-buffer-function))
                       (magit--refresh-buffer-function))
                   (my:magit-watch--under-p default-directory root)))
            (or (not my:magit-watch-visible-only) (get-buffer-window buf t))))
     (buffer-list))))

(defun my:magit-watch--refresh (repo)
  (dolist (buf (my:magit-watch--buffers repo))
    (with-current-buffer buf
      ;; magit-refresh (全バッファ + post-refresh-hook の diff-hl) ではなく
      ;; そのバッファだけにする。自動更新のたびに全部を取り直すのは重い。
      (magit-refresh-buffer)))
  (setf (my:magit-watch-repo-last-refresh repo) (float-time)))

(defun my:magit-watch--fire (root)
  "デバウンスタイマーから呼ばれる。"
  (when-let* ((repo (gethash root my:magit-watch--repos)))
    (setf (my:magit-watch-repo-timer repo) nil)
    (cond
     ;; 操作中なら捨てずに待ち直す。あとで必ず更新する。
     ;; 待ち直しの間隔はデバウンス値ではなくレート制限の値にする。
     ;; 短い間隔で回すとタイマーが空回りするだけで、応答は良くならない
     ((not (my:magit-watch--allowed-p))
      (cl-incf (plist-get my:magit-watch--stats :deferred))
      (my:magit-watch--arm repo my:magit-watch-min-interval))
     ;; レート制限。ビルド中の暴走を止める
     ((< (- (float-time) (or (my:magit-watch-repo-last-refresh repo) 0))
         my:magit-watch-min-interval)
      (cl-incf (plist-get my:magit-watch--stats :throttled))
      (my:magit-watch--arm repo my:magit-watch-min-interval))
     (t
      (let ((reason (my:magit-watch--stale-p repo)))
        ;; 決め手のイベントが落ちていて内容で気づいた場合だけ、ここで
        ;; トークンを進める。`suspect' ではコールバックで進めていないので、
        ;; これが無いと gitd が古い答えを返し続ける
        (when (eq reason 'fingerprint)
          (cl-incf (my:magit-watch-repo-serial repo)))
        (setf (my:magit-watch-repo-pending repo) nil)
        (setf (my:magit-watch-repo-wt-dirs repo) nil)
        (setf (my:magit-watch-repo-wt-overflow repo) nil)
        (setf (my:magit-watch-repo-prewarmed repo) nil)
        (if reason
            (progn
              ;; 先読みタイマーが間に合っていなくても、ここで頼めば
              ;; デーモン側が並列に走らせ、magit の要求は single-flight で
              ;; それに合流する。直列 29 回ぶんの待ちが 1 回ぶんになる
              (my:magit-watch--ask-prewarm repo)
              (cl-incf (plist-get my:magit-watch--stats :refreshed))
              (my:magit-watch--refresh repo))
          (cl-incf (plist-get my:magit-watch--stats :skipped))))))))

(defun my:magit-watch--fire-prewarm (root)
  "先読みタイマーから呼ばれる。リフレッシュはしない。

`my:magit-watch--stale-p' をここでも通すのが肝で、`.gitignore' で
無視されるだけの変化 (ビルド出力など) では先読みも走らない。
**`pending' を消さないこと。** リフレッシュ側の判定がまだ要る。

**1 つの窓では 1 回しか頼まない。** 先読みは 28 個の git を走らせ、その
どれもが `.git' と `.git/index.lock' のイベントを出す (実測: 読み取り
10 種で 4 件、`update-index --refresh' だけでも 3 件)。頼むたびに
タイマーが張り直されるので、絞らないと先読みが自分自身を呼び続ける。"
  (when-let* ((repo (gethash root my:magit-watch--repos)))
    (setf (my:magit-watch-repo-prewarm-timer repo) nil)
    (when (and (not (my:magit-watch-repo-prewarmed repo))
               (my:magit-watch--stale-p repo))
      (setf (my:magit-watch-repo-prewarmed repo) t)
      (cl-incf (plist-get my:magit-watch--stats :prewarmed))
      (my:magit-watch--ask-prewarm repo))))

(defun my:magit-watch--arm (repo delay &optional only-if-idle)
  "REPO のリフレッシュを DELAY 秒後に予約する。

ONLY-IF-IDLE が非 nil なら、既に予約があるときは**延長しない**。
`suspect' のイベントに使う。自分の git 実行が出すイベントで
デバウンスの窓が延び続け、いつまでも発火しなくなるのを防ぐ。"
  (unless (and only-if-idle (my:magit-watch-repo-timer repo))
    (when (my:magit-watch-repo-timer repo)
      (cancel-timer (my:magit-watch-repo-timer repo)))
    (setf (my:magit-watch-repo-timer repo)
          (run-with-timer delay nil #'my:magit-watch--fire
                          (my:magit-watch-repo-root repo)))))

(defun my:magit-watch--arm-prewarm (repo &optional only-if-idle)
  (when (and my:magit-watch-prewarm-delay
             (< my:magit-watch-prewarm-delay my:magit-watch-debounce)
             (not (and only-if-idle (my:magit-watch-repo-prewarm-timer repo))))
    (when (my:magit-watch-repo-prewarm-timer repo)
      (cancel-timer (my:magit-watch-repo-prewarm-timer repo)))
    (setf (my:magit-watch-repo-prewarm-timer repo)
          (run-with-timer my:magit-watch-prewarm-delay nil
                          #'my:magit-watch--fire-prewarm
                          (my:magit-watch-repo-root repo)))))

;;; ---------------------------------------------------------------- 監視

(defun my:magit-watch--callback (root ev)
  "w32notify のコールバック。**軽く保つこと。**

ビルド中は 1 秒に数千件来る (実測: 200 ファイル作成で 2001 件)。
**ここで git を呼ぶような作りにしてはいけない。** `.gitignore' の判定も
ここではせず、ディレクトリ名をハッシュに入れて濃縮するだけにして、
実際の判定はデバウンス後に 1 回だけ行う。"
  (cl-incf (plist-get my:magit-watch--stats :events))
  (when-let* ((repo (gethash root my:magit-watch--repos))
              (rel (subst-char-in-string ?\\ ?/ (or (nth 2 ev) "")))
              (kind (my:magit-watch--classify rel)))
    (cl-incf (plist-get my:magit-watch--stats :classified))
    (cl-pushnew kind (my:magit-watch-repo-pending repo))
    ;; **状態が変わった証拠があるときだけトークンを進める。**
    ;; `suspect' (.git 自身と .git/**/*.lock) を含めてはいけない。
    ;; **読み取りだけの git もこれらのイベントを出す**ため (実測: `status'
    ;; ですら index.lock を作る)、含めると自分のリフレッシュや先読みが
    ;; 自分のキャッシュを壊してしまう。
    ;; 決め手のイベントが落ちた場合は `my:magit-watch--fire' が
    ;; フィンガープリントの不一致を見て進める
    (unless (eq kind 'suspect)
      (cl-incf (my:magit-watch-repo-serial repo)))
    (cond
     ((eq kind 'worktree)
      ;; 除外ルールが変わったらキャッシュを捨てる
      (when (equal (file-name-nondirectory rel) ".gitignore")
        (setf (my:magit-watch-repo-ign-cache repo) nil))
      (let ((h (or (my:magit-watch-repo-wt-dirs repo)
                   (setf (my:magit-watch-repo-wt-dirs repo)
                         (make-hash-table :test #'equal)))))
        (if (>= (hash-table-count h) my:magit-watch--wt-dirs-limit)
            (setf (my:magit-watch-repo-wt-overflow repo) t)
          (puthash (my:magit-watch--ign-key rel) t h))))
     ((eq kind 'meta)
      ;; .git/info/exclude や core.excludesFile の変更でも除外ルールは変わる
      (when (member rel '(".git/info/exclude" ".git/config"))
        (setf (my:magit-watch-repo-ign-cache repo) nil))))
    ;; `suspect' は「何かが起きたかもしれない」以上のことを言わない。
    ;; 既に評価が予約してあるなら窓を延ばさない (延ばすと、自分の git 実行が
    ;; 出すイベントで永久に発火しなくなる。実際にそうなった)
    (let ((idle-only (eq kind 'suspect)))
      (my:magit-watch--arm repo my:magit-watch-debounce idle-only)
      (my:magit-watch--arm-prewarm repo idle-only))))

(defun my:magit-watch--under-p (dir root)
  (let ((d (expand-file-name dir))
        (r root))
    (when (file-name-case-insensitive-p r)
      (setq d (downcase d) r (downcase r)))
    (string-prefix-p r (file-name-as-directory d))))

(defun my:magit-watch--repo-of (dir)
  "DIR を含む監視中のリポジトリを返す。git は呼ばない。"
  (let (found)
    (maphash (lambda (root repo)
               (when (my:magit-watch--under-p dir root) (setq found repo)))
             my:magit-watch--repos)
    found))

(defun my:magit-watch-add (&optional directory)
  "DIRECTORY のリポジトリを監視対象にする。既にしていれば何もしない。"
  (when (and my:magit-watch-mode (eq system-type 'windows-nt))
    (let ((default-directory (or directory default-directory)))
      (when-let* ((root (magit-toplevel))
                  (root (file-name-as-directory (expand-file-name root))))
        (unless (gethash root my:magit-watch--repos)
          (when-let* ((gitdir (magit-gitdir)))
            (let ((repo (my:magit-watch--make-repo
                         :root root :gitdir gitdir :serial 1)))
              ;; watch を張る前にフィンガープリントを取る (取りこぼし防止)
              (setf (my:magit-watch-repo-fp repo)
                    (my:magit-watch--fingerprint repo))
              (setf (my:magit-watch-repo-desc repo)
                    (w32notify-add-watch
                     (directory-file-name root)
                     '(file-name directory-name size last-write-time subtree)
                     (lambda (ev) (my:magit-watch--callback root ev))))
              (puthash root repo my:magit-watch--repos)
              repo)))))))

(defun my:magit-watch-remove (root)
  (when-let* ((repo (gethash root my:magit-watch--repos)))
    (dolist (tm (list (my:magit-watch-repo-timer repo)
                      (my:magit-watch-repo-prewarm-timer repo)))
      (when tm (cancel-timer tm)))
    (ignore-errors (w32notify-rm-watch (my:magit-watch-repo-desc repo)))
    (remhash root my:magit-watch--repos)
    ;; 監視をやめたらキャッシュも捨てさせる。以後トークンを進める者が
    ;; いなくなるので、残しておくと古い答えを返し続けることになる
    (when (fboundp 'my:gitd-forget)
      (my:gitd-forget root))))

(defun my:magit-watch--remove-all ()
  (dolist (root (hash-table-keys my:magit-watch--repos))
    (my:magit-watch-remove root)))

;;; ---------------------------------------------------------------- フック

(defun my:magit-watch--after-refresh ()
  "`magit-refresh-buffer-hook'。フィンガープリントを取り直す。

**この 1 行が二重リフレッシュ対策の核。** magit 自身の stage / unstage の
あとにもこのフックは走るので、そのとき取ったスナップショットと、
あとから届く `.git/index' のイベントが一致し、`my:magit-watch--stale-p'
で落ちる。"
  (when-let* ((repo (my:magit-watch--repo-of default-directory)))
    (setf (my:magit-watch-repo-fp repo)
          (my:magit-watch--fingerprint repo))
    (setf (my:magit-watch-repo-last-refresh repo) (float-time))))

(defun my:magit-watch--pre-refresh ()
  "`magit-pre-refresh-hook'。トークンを進めて先読みを頼む。

**このフックは `magit-refresh' でしか走らない。** つまり

  - ユーザが `g' を押したとき
  - magit のコマンド (stage / commit / checkout / ...) の直後

の 2 つで、自動更新が使う `magit-refresh-buffer' では走らない。

トークンを進めるのが重要で、`magit-run-git-with-input'
\(`call-process-region') や `magit-start-process' (非同期) はデーモンを
通らないため、書き込みをデーモン側から観測できない。magit は必ず
`magit-refresh' で締めるので、ここで捕まえれば取りこぼさない。
**`g' が必ず本当のことを言う**のもこれで保証される。

先読みを頼むのは速度のため。頼んでおけばデーモンが 29 コマンドを
並列に走らせ、直後に来る magit の要求はそれに合流する。"
  (when-let* ((repo (my:magit-watch--repo-of default-directory)))
    (cl-incf (my:magit-watch-repo-serial repo))
    (my:magit-watch--ask-prewarm repo)))

(defun my:magit-watch--after-mode ()
  "`magit-mode-hook'。バッファができたら監視を始める。"
  (my:magit-watch-add default-directory))

;;; ---------------------------------------------------------------- コマンド

;;;###autoload
(defun my:magit-watch-stats ()
  "イベント数・リフレッシュ数・抑止数を表示する。"
  (interactive)
  (message "magit-watch: %d 監視中 / イベント %d (分類を通った %d) / リフレッシュ %d / 先読み %d / 変化なしで抑止 %d / 操作中で待ち直し %d / レート制限で待ち直し %d%s"
           (hash-table-count my:magit-watch--repos)
           (plist-get my:magit-watch--stats :events)
           (plist-get my:magit-watch--stats :classified)
           (plist-get my:magit-watch--stats :refreshed)
           (plist-get my:magit-watch--stats :prewarmed)
           (plist-get my:magit-watch--stats :skipped)
           (plist-get my:magit-watch--stats :deferred)
           (plist-get my:magit-watch--stats :throttled)
           (let ((e (plist-get my:magit-watch--stats :ign-error)))
             (if (zerop e) "" (format " / check-ignore 失敗 %d" e)))))

(declare-function magit--refresh-buffer-function "magit-mode")

;;;###autoload
(define-minor-mode my:magit-watch-mode
  "ワークツリーの変化で magit バッファを自動更新する。"
  :global t
  :lighter nil
  (if my:magit-watch-mode
      (progn
        (add-hook 'magit-mode-hook #'my:magit-watch--after-mode)
        (add-hook 'magit-refresh-buffer-hook #'my:magit-watch--after-refresh)
        (add-hook 'magit-pre-refresh-hook #'my:magit-watch--pre-refresh)
        ;; 既にある magit バッファを拾う
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'magit-mode) (my:magit-watch-add)))))
    (remove-hook 'magit-mode-hook #'my:magit-watch--after-mode)
    (remove-hook 'magit-refresh-buffer-hook #'my:magit-watch--after-refresh)
    (remove-hook 'magit-pre-refresh-hook #'my:magit-watch--pre-refresh)
    (my:magit-watch--remove-all)))

;; 対象は Windows のみ。`w32notify-add-watch' の `subtree' に相当するものが
;; 他のバックエンドには無く (inotify も kqueue も非再帰)、そちらは段階 2b で
;; 常駐プロセス側に監視を移すときにまとめて考える。
(when (eq system-type 'windows-nt)
  (my:magit-watch-mode 1))

(provide 'my-magit-watch)
;;; my-magit-watch.el ends here
