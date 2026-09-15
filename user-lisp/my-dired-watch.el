;;; my-dired-watch.el --- ファイルの中身の変化で dired を追従させる  -*- lexical-binding: t -*-

;;; Commentary:

;; dired バッファを、**既にある行の中身が変わったとき**にも追従させる。
;; サイズ・更新日時・属性の変化が対象で、`g' を押さなくてよくする。
;;
;; Windows のみ。設計と実測は tmp/dired-auto-update-design.md を参照。
;;
;;; なぜ素の autorevert では足りないのか
;;
;; 追従しないのは通知が来ないからではない。**イベントは今も届いている**
;; (実測)。門が 2 つあり、どちらも独立に閉じている。
;;
;;   1. `auto-revert-notify-handler' は `buffer-file-name' を持たない
;;      バッファでは `created' / `renamed' / `deleted' しか通さない
;;      (autorevert.el)。`changed' はそこに書かれてすらいない。
;;      そもそも watch を張るときのフラグが
;;      `(if buffer-file-name (quote (change attribute-change)) (quote (change)))'
;;      なので、**dired では属性の変化を OS に問い合わせてすらいない**
;;
;;   2. `dired-buffer-stale-p' は `dired-directory-changed-p'、つまり
;;      **ディレクトリ自身の mtime** しか見ない (dired.el)。中のファイルに
;;      追記しても親ディレクトリの mtime は 1 ビットも動かない (実測)。
;;      仮に 1 を通しても、ここで nil を返されて revert に至らない
;;
;; したがってここでは autorevert に手を入れず、**`modified' のイベントだけを
;; 自分で拾う**。行が増減する変化 (作成 / 削除 / 改名) は門 1 を通るので
;; 今までどおり `my-dired.el' の `auto-revert-mode' に任せる。
;; **既存の挙動には触らない。**
;;
;;; 【重要】全体 revert ではなく 1 行だけ貼り替える
;;
;; サイズ・日時・属性が変わっても**行は増減しない**。そこで
;; `dired-relist-entry' でその行だけ貼り替える。実測 (4862 エントリ):
;;
;;   revert-buffer        648 ms  (dired-after-readin-hook あり)
;;   revert-buffer        237 ms  (同 nil)
;;   dired-relist-entry   454 ms  (dired-after-readin-hook あり)
;;   dired-relist-entry   0.71 ms (同 nil)          <- これを使う
;;
;; 支配的なのは `nerd-icons-dired--refresh' で、**1 行しか変えていなくても
;; バッファ全体を舐め直す**。アイコンはファイル名と種別だけで決まり、
;; サイズや日時では変わらないので、貼り替えの間だけフックを外す。
;;
;; マークと point が保たれ、スクロール位置が飛ばないのも全体 revert との違い。
;;
;;; 【重要】アイコンは overlay なので自分で付け直す
;;
;; `nerd-icons-dired' のアイコンは行の上に張った overlay (`evaporate' が t)。
;; `dired-relist-entry' は行を `delete-region' するので **アイコンだけが
;; 消える**。フックを外している以上、付け直すのはこちらの仕事になる
;; (`my:dired-watch--annotate')。
;;
;;; magit-watch より簡単な点
;;
;; `my-magit-watch.el' で要った仕掛けは、ここではどれも要らない。
;;
;;   - 自励振動対策: **要らない。** dired の更新はファイルを書かない
;;   - `.gitignore' の判定: **要らない。** dired は無視しない。見えている
;;     ものが変わったなら反映するのが正しい
;;   - フィンガープリント: **要らない。** イベントがファイル名を持っている
;;   - gitd のトークン: 無関係
;;
;; 例外は `.git/' を dired で開いているときで、magit や `diff-hl-dired' が
;; 作る `index.lock' で発火する。レート制限で足りる。
;;
;;; 【重要】サブディレクトリの行は追従しない (仕様)
;;
;; `sub/' の中にファイルを作ると `sub' 自身の mtime は変わるが、
;; **親の watch にはイベントが 1 件も来ない**。実測:
;;
;;   sub/ の中にファイルを作る  -> イベント 0 件 (行は古いまま)
;;   直下のファイルを書き換える -> (modified "f07.txt") が 2 件
;;
;; 非再帰の `ReadDirectoryChangesW' が配下の変化を親に報告しないため。
;; 拾うには `subtree' を足すしかなく、それはビルド出力のディレクトリを
;; 開いているときに毎秒数千件を呼び込む。**得るもの (ディレクトリ行の
;; mtime) に対して代償が大きすぎる**ので取らない。`g' を押せば直る。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'dired-aux)

;; nerd-icons-dired の内部に触る (`my:dired-watch--annotate' の説明を参照)。
;; 無くても動くように、参照はすべて `bound-and-true-p' / `fboundp' で守る。
(declare-function nerd-icons-dired--add-overlay "nerd-icons-dired")
(defvar nerd-icons-dired-icon-size)
(defvar nerd-icons-dired-infix-string)
(defvar nerd-icons-dired-dir-icon-function)
(defvar nerd-icons-dired-file-icon-function)

(defvar my:dired-watch-mode)            ; define-minor-mode で定義される

(defgroup my:dired-watch nil
  "ファイルの中身の変化で dired を追従させる。"
  :group 'dired)

(defcustom my:dired-watch-debounce 0.3
  "最後のイベントからこの秒数だけ待ってから貼り替える。"
  :type 'number)

(defcustom my:dired-watch-min-interval 1.0
  "同じバッファを更新する最短間隔 (秒)。

dired でビルド出力のディレクトリを開いていると毎秒数千件のイベントが来る。
1 行の貼り替えは 0.71 ms と安いが、件数が増えれば効いてくるのでここで抑える。"
  :type 'number)

(defcustom my:dired-watch-relist-limit 32
  "1 回の窓でこの件数を超えたら、1 行ずつではなく全体を revert する。

多数の行が変わったなら、貼り替えを繰り返すより 1 回読み直すほうが安い。"
  :type 'integer)

;;; ---------------------------------------------------------------- 状態

(defvar-local my:dired-watch--watches nil
  "このバッファが張っている watch の alist。(DIR . DESC)。

dired はバッファ 1 つにディレクトリが複数ありうる (`i' で挿入した
サブディレクトリ、dired-sidebar の subtree)。`dired-subdir-alist' を
追いかけて張り直す。")

(defvar-local my:dired-watch--pending nil
  "この窓で変化した絶対ファイル名のハッシュ。")

(defvar-local my:dired-watch--overflow nil
  "件数が `my:dired-watch-relist-limit' を超えたら t。全体 revert に倒す。")

(defvar-local my:dired-watch--timer nil)
(defvar-local my:dired-watch--last 0.0
  "最後に更新した時刻 (レート制限用)。")

(defvar my:dired-watch--stats
  (list :events 0 :relisted 0 :reverted 0 :deferred 0 :throttled 0 :missing 0)
  "統計。`my:dired-watch-stats' で表示する。")

;;; ---------------------------------------------------------------- 監視

(defconst my:dired-watch--flags '(size last-write-time attributes)
  "w32notify に渡すフラグ。

**`file-name' と `directory-name' は入れない。** 行の増減 (作成 / 削除 /
改名) は autorevert が拾う担当で、ここが二重に revert しないようにする。
`subtree' も入れない。dired は 1 階層しか表示しないので、配下の変化で
発火しても貼り替える行が無い (ディレクトリ自身の mtime は親の watch で
`modified' として届く)。")

(defun my:dired-watch--dirs ()
  "このバッファが表示しているディレクトリを返す (絶対、末尾スラッシュ無し)。"
  (let (dirs)
    (dolist (e dired-subdir-alist)
      (let ((d (car e)))
        (when (and (stringp d) (not (file-remote-p d)) (file-directory-p d))
          (cl-pushnew (directory-file-name (expand-file-name d)) dirs
                      :test #'equal))))
    dirs))

(defun my:dired-watch--callback (buffer dir ev)
  "w32notify のコールバック。**軽く保つこと。**

ビルド中は 1 秒に数千件来る。ここでは名前をハッシュに入れて窓を張り直す
だけにして、判断はデバウンス後に 1 回だけ行う。"
  (cl-incf (plist-get my:dired-watch--stats :events))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; フラグ上 `modified' しか来ないはずだが、念のため絞る。
      (when (eq (nth 1 ev) 'modified)
        (let ((name (nth 2 ev)))
          (when (and (stringp name) (not (string-empty-p name)))
            (let ((h (or my:dired-watch--pending
                         (setq my:dired-watch--pending
                               (make-hash-table :test #'equal)))))
              (if (>= (hash-table-count h) my:dired-watch-relist-limit)
                  (setq my:dired-watch--overflow t)
                (puthash (expand-file-name (subst-char-in-string ?\\ ?/ name)
                                           (file-name-as-directory dir))
                         t h)))
            (my:dired-watch--arm buffer my:dired-watch-debounce)))))))

(defun my:dired-watch--sync ()
  "`dired-after-readin-hook'。表示中のディレクトリに watch を合わせる。

revert とサブディレクトリの挿入の両方でここに来る。**貼り替えの間は
`dired-after-readin-hook' を nil に束縛しているので再入しない。**"
  (when (and my:dired-watch-mode (derived-mode-p 'dired-mode))
    (let ((want (my:dired-watch--dirs))
          (buffer (current-buffer)))
      ;; 消えたものを外す
      (let (keep)
        (dolist (w my:dired-watch--watches)
          (if (member (car w) want)
              (push w keep)
            (ignore-errors (w32notify-rm-watch (cdr w)))))
        (setq my:dired-watch--watches keep))
      ;; 増えたものを張る
      (dolist (dir want)
        (unless (assoc dir my:dired-watch--watches)
          (when-let* ((desc (ignore-errors
                              (w32notify-add-watch
                               dir my:dired-watch--flags
                               (lambda (ev)
                                 (my:dired-watch--callback buffer dir ev))))))
            (push (cons dir desc) my:dired-watch--watches)
            (add-hook 'kill-buffer-hook #'my:dired-watch--unwatch nil t)))))))

(defun my:dired-watch--unwatch ()
  "このバッファの watch を全部外す。"
  (dolist (w my:dired-watch--watches)
    (ignore-errors (w32notify-rm-watch (cdr w))))
  (setq my:dired-watch--watches nil)
  (when my:dired-watch--timer
    (cancel-timer my:dired-watch--timer)
    (setq my:dired-watch--timer nil)))

;;; ---------------------------------------------------------------- 更新

(defun my:dired-watch--allowed-p ()
  "いま貼り替えてよいなら非 nil。カレントバッファは対象の dired バッファ。

**ここに置いてよいのは「ユーザの操作を邪魔しないため」の条件だけ。**
いずれもユーザが操作をやめれば解消するので、待ち直せば必ず進む。

`frame-focus-state' を入れてはいけない (CLAUDE.md)。フォーカスが外れて
いる間は永久に偽なので、その時点から二度と更新されなくなる。"
  (and
   ;; 【重要】wdired 中は絶対に触らない。編集中のバッファを書き換えることに
   ;; なる。`dired-relist-entry' は自分で `buffer-read-only' を nil に束縛
   ;; してしまうので、**呼ぶ前にこちらで見るしかない**
   buffer-read-only
   (not (active-minibuffer-window))
   (not (bound-and-true-p isearch-mode))
   (not defining-kbd-macro)
   (not executing-kbd-macro)
   (not (region-active-p))
   (not (input-pending-p))))

(defun my:dired-watch--resort-needed-p ()
  "並び順が変わりうるなら非 nil。

`dired-add-entry' は行を元の位置に戻すだけで**並べ直さない**。`-t' で
時刻順に並べているときは、日時が変われば順序も変わるはずなので、1 行の
貼り替えでは嘘になる。そのときは全体 revert に倒す。

長い名前に `time' ではなく `sort=time' を渡すこと。`dired-check-switches'
は LONG を `--LONG\\>' で見るので、`time' だと `--time-style=...' にも
当たってしまう (`-' は単語境界になる)。"
  (and (stringp dired-actual-switches)
       (or (dired-check-switches dired-actual-switches "t" "sort=time")
           (dired-check-switches dired-actual-switches "S" "sort=size"))))

(defun my:dired-watch--annotate (file)
  "貼り替えた FILE の行にアイコンを付け直す。

`nerd-icons-dired' のアイコンは行の上の overlay (`evaporate' が t) なので、
`dired-relist-entry' の `delete-region' で消える。`dired-after-readin-hook'
を外している以上、ここで付け直す。

**`nerd-icons-dired' が無ければ何もしない。** あちらの内部関数に触るのは
承知の上で、代わりにフックを走らせると 4862 件で 453 ms かかる。"
  (when (and (bound-and-true-p nerd-icons-dired-mode)
             (fboundp 'nerd-icons-dired--add-overlay))
    (save-excursion
      (when (dired-goto-file file)
        (let ((rel (dired-get-filename 'relative 'noerror))
              (pos (dired-move-to-filename)))
          (when (and rel pos (not (member rel '("." ".."))))
            (let* ((func (if (file-directory-p file)
                             nerd-icons-dired-dir-icon-function
                           nerd-icons-dired-file-icon-function))
                   (icon (funcall func rel :height nerd-icons-dired-icon-size))
                   (inhibit-read-only t))
              (nerd-icons-dired--add-overlay
               pos (concat icon nerd-icons-dired-infix-string)))))))))

(defun my:dired-watch--update-line (file)
  "FILE の行だけを貼り替える。貼り替えたら非 nil。

**行が無ければ何もしない。** `dired-relist-entry' は行が無ければ
`dired-add-entry' で作ってしまうが、それは新規ファイルの追加であって
ここの担当ではない (autorevert が拾う)。しかもフックを外しているので、
そうして作られた行にはアイコンが付かない。

`file-exists-p' を先に見るのは、イベントとタイマーの間に消された
ファイルで、行を消したあと `insert-directory' が失敗するのを防ぐため。"
  (and (file-exists-p file)
       (save-excursion
         (and (dired-goto-file file)
              (progn
                (let ((dired-after-readin-hook nil))
                  (dired-relist-entry file))
                (my:dired-watch--annotate file)
                t)))))

(defun my:dired-watch--fire (buffer)
  "デバウンスタイマーから呼ばれる。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my:dired-watch--timer nil)
      (cond
       ((not (derived-mode-p 'dired-mode)) nil)
       ;; 操作中なら捨てずに待ち直す。あとで必ず更新する
       ((not (my:dired-watch--allowed-p))
        (cl-incf (plist-get my:dired-watch--stats :deferred))
        (my:dired-watch--arm buffer my:dired-watch-min-interval))
       ((< (- (float-time) my:dired-watch--last) my:dired-watch-min-interval)
        (cl-incf (plist-get my:dired-watch--stats :throttled))
        (my:dired-watch--arm buffer my:dired-watch-min-interval))
       (t
        (let ((files (and my:dired-watch--pending
                          (hash-table-keys my:dired-watch--pending)))
              (overflow my:dired-watch--overflow))
          (setq my:dired-watch--pending nil
                my:dired-watch--overflow nil)
          (when (or files overflow)
            (setq my:dired-watch--last (float-time))
            (if (or overflow (my:dired-watch--resort-needed-p))
                (progn
                  (cl-incf (plist-get my:dired-watch--stats :reverted))
                  ;; ここはアイコンが要るのでフックを外さない
                  (revert-buffer))
              (dolist (f files)
                (if (my:dired-watch--update-line f)
                    (cl-incf (plist-get my:dired-watch--stats :relisted))
                  (cl-incf (plist-get my:dired-watch--stats :missing))))))))))))

(defun my:dired-watch--arm (buffer delay)
  (with-current-buffer buffer
    (when my:dired-watch--timer (cancel-timer my:dired-watch--timer))
    (setq my:dired-watch--timer
          (run-with-timer delay nil #'my:dired-watch--fire buffer))))

;;; ---------------------------------------------------------------- コマンド

;;;###autoload
(defun my:dired-watch-stats ()
  "監視中のバッファ数とイベント数を表示する。"
  (interactive)
  (let ((bufs 0) (watches 0))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when my:dired-watch--watches
          (cl-incf bufs)
          (cl-incf watches (length my:dired-watch--watches)))))
    (message "dired-watch: %d バッファ / watch %d / イベント %d / 貼り替え %d / 全体 revert %d / 行が無くて見送り %d / 操作中で待ち直し %d / レート制限で待ち直し %d"
             bufs watches
             (plist-get my:dired-watch--stats :events)
             (plist-get my:dired-watch--stats :relisted)
             (plist-get my:dired-watch--stats :reverted)
             (plist-get my:dired-watch--stats :missing)
             (plist-get my:dired-watch--stats :deferred)
             (plist-get my:dired-watch--stats :throttled))))

;;;###autoload
(define-minor-mode my:dired-watch-mode
  "ファイルの中身の変化で dired バッファを追従させる。"
  :global t
  :lighter nil
  (if my:dired-watch-mode
      (progn
        (add-hook 'dired-after-readin-hook #'my:dired-watch--sync)
        ;; 既にある dired バッファを拾う
        (dolist (b (buffer-list))
          (with-current-buffer b
            (when (derived-mode-p 'dired-mode) (my:dired-watch--sync)))))
    (remove-hook 'dired-after-readin-hook #'my:dired-watch--sync)
    (dolist (b (buffer-list))
      (with-current-buffer b (my:dired-watch--unwatch)))))

;; 対象は Windows のみ。他のバックエンドへの対応は、必要になってから
;; `file-notify-add-watch' 経由に一般化する (dired の watch は非再帰なので
;; inotify / kqueue でもそのまま張れる。`subtree' が要る magit とは違う)。
(when (and (eq system-type 'windows-nt) (featurep 'w32notify))
  (my:dired-watch-mode 1))

(provide 'my-dired-watch)
;;; my-dired-watch.el ends here
