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
;;; 既知の制限
;;
;; `.gitignore' を見ていない。`build/' に 200 ファイル作ると分類後でも
;; 1001 件残る。パスだけのフィルタでは落とせず、git に聞くしかない。
;; 「表示中のバッファだけ」と「レート制限」で上限は決めているが、
;; 根本的な解決は段階 2b (監視を常駐プロセスに移し、Rust の `ignore' crate で
;; イベントを Emacs に送る前に落とす) で行う。

;;; Code:

(require 'cl-lib)

(declare-function magit-toplevel "magit-git")
(declare-function magit-gitdir "magit-git")
(declare-function magit-refresh-buffer "magit-mode")

(defgroup my:magit-watch nil
  "ワークツリーの変化で magit を自動更新する。"
  :group 'magit)

(defcustom my:magit-watch-debounce 0.4
  "最後のイベントからこの秒数だけ待ってからリフレッシュする。"
  :type 'number)

(defcustom my:magit-watch-min-interval 2.0
  "自動リフレッシュの最短間隔 (秒)。

`.gitignore' を見られないため、ビルド中は無関係なイベントが大量に来る
\(実測: build/ に 200 ファイルで分類後 1001 件)。ここで上限を決める。"
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
  pending       ; 未処理イベントの分類 (シンボルのリスト)
  timer
  last-refresh) ; 最後にリフレッシュした時刻 (レート制限用)

(defvar my:magit-watch--repos (make-hash-table :test #'equal)
  "root -> `my:magit-watch-repo'。")

(defvar my:magit-watch--stats
  (list :events 0 :classified 0 :refreshed 0 :skipped 0 :deferred 0)
  "統計。`my:magit-watch-stats' で表示する。")

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

(defun my:magit-watch--stale-p (repo)
  "REPO をリフレッシュする必要があるなら非 nil。

ワークツリーか .git のメタ情報が動いていれば無条件に要る。
`index' と `suspect' しか無いときは、前回のリフレッシュ直後に取った
フィンガープリントと比べて本当に変わったかを見る。ここで

  - `magit-refresh-buffer' 自身が出すイベント (自励振動)
  - magit の stage / unstage 直後のイベント (二重リフレッシュ)

の両方が落ちる。どちらも magit が既にリフレッシュ済みで、その時点の
スナップショットと一致するため。**時刻ではなく内容で見ているので、
イベントの配送順に依存しない。**"
  (let ((pending (my:magit-watch-repo-pending repo)))
    (cond
     ((null pending) nil)
     ((or (memq 'worktree pending) (memq 'meta pending)) t)
     (t (not (equal (my:magit-watch-repo-fp repo)
                    (my:magit-watch--fingerprint repo)))))))

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
      (my:magit-watch--arm repo my:magit-watch-min-interval))
     (t
      (let ((stale (my:magit-watch--stale-p repo)))
        (setf (my:magit-watch-repo-pending repo) nil)
        (if stale
            (progn (cl-incf (plist-get my:magit-watch--stats :refreshed))
                   (my:magit-watch--refresh repo))
          (cl-incf (plist-get my:magit-watch--stats :skipped))))))))

(defun my:magit-watch--arm (repo delay)
  (when (my:magit-watch-repo-timer repo)
    (cancel-timer (my:magit-watch-repo-timer repo)))
  (setf (my:magit-watch-repo-timer repo)
        (run-with-timer delay nil #'my:magit-watch--fire
                        (my:magit-watch-repo-root repo))))

;;; ---------------------------------------------------------------- 監視

(defun my:magit-watch--callback (root ev)
  "w32notify のコールバック。**軽く保つこと。**

ビルド中は 1 秒に数千件来る (実測: 200 ファイル作成で 2001 件)。
ここで git を呼ぶような作りにしてはいけない。"
  (cl-incf (plist-get my:magit-watch--stats :events))
  (when-let* ((repo (gethash root my:magit-watch--repos))
              (kind (my:magit-watch--classify (or (nth 2 ev) ""))))
    (cl-incf (plist-get my:magit-watch--stats :classified))
    (cl-pushnew kind (my:magit-watch-repo-pending repo))
    (my:magit-watch--arm repo my:magit-watch-debounce)))

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
                         :root root :gitdir gitdir)))
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
    (when (my:magit-watch-repo-timer repo)
      (cancel-timer (my:magit-watch-repo-timer repo)))
    (ignore-errors (w32notify-rm-watch (my:magit-watch-repo-desc repo)))
    (remhash root my:magit-watch--repos)))

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

(defun my:magit-watch--after-mode ()
  "`magit-mode-hook'。バッファができたら監視を始める。"
  (my:magit-watch-add default-directory))

;;; ---------------------------------------------------------------- コマンド

;;;###autoload
(defun my:magit-watch-stats ()
  "イベント数・リフレッシュ数・抑止数を表示する。"
  (interactive)
  (message "magit-watch: %d 監視中 / イベント %d (分類を通った %d) / リフレッシュ %d / 変化なしで抑止 %d / 操作中で待ち直し %d"
           (hash-table-count my:magit-watch--repos)
           (plist-get my:magit-watch--stats :events)
           (plist-get my:magit-watch--stats :classified)
           (plist-get my:magit-watch--stats :refreshed)
           (plist-get my:magit-watch--stats :skipped)
           (plist-get my:magit-watch--stats :deferred)))

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
        ;; 既にある magit バッファを拾う
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'magit-mode) (my:magit-watch-add)))))
    (remove-hook 'magit-mode-hook #'my:magit-watch--after-mode)
    (remove-hook 'magit-refresh-buffer-hook #'my:magit-watch--after-refresh)
    (my:magit-watch--remove-all)))

(provide 'my-magit-watch)
;;; my-magit-watch.el ends here
