;;; my-dired-watch.el --- ファイルの中身の変化で dired を追従させる  -*- lexical-binding: t -*-

;;; Commentary:

;; dired バッファを、**既にある行の中身が変わったとき**にも追従させる。
;; サイズ・更新日時・属性の変化が対象で、`g' を押さなくてよくする。
;;
;; 変化の見つけ方だけが OS で違い、**貼り替えの経路は 1 本に合流する**。
;;
;;   Windows  w32notify の `modified' イベントを拾う
;;   それ以外 アイドル中に stat を取って前回と比べる (下記)
;;
;; 設計と実測は docs/dired/dired-extensions.md を参照。
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
;;; 【重要】mac では「中身が変わった」が通知では分からない (2026-11-15 に実測)
;;
;; macOS の Emacs のバックエンドは kqueue (`(featurep (quote kqueue))' が t)。
;; ディレクトリに watch を張ると**行の増減は届くのに、既存のファイルの
;; サイズ・属性の変化は 1 件も届かない**。実測:
;;
;;   ファイルを作る    -> (created "new.txt") (changed "new.txt")  即座
;;   既存ファイルに追記 -> **0 件**
;;   chmod             -> **0 件**
;;   そのあと別の操作   -> ここで初めて (changed "exist.txt")
;;                         (attribute-changed "exist.txt") が遅れて届く
;;
;; kqueue はディレクトリの `NOTE_WRITE' しか見ず、エントリの一覧を
;; Emacs 側で持って差分を合成している。**中身を書いてもディレクトリは
;; 書き換わらない**ので、次に何かが起きるまで気づけない。
;; 個々のファイルに watch を張れば届くが、kqueue は 1 watch = 1 fd なので
;; 数千件のディレクトリで破綻する (eglot が 6751 本張って固まったのと同じ轍)。
;;
;; そこで mac では**通知をあきらめて stat を比べる**。`modified' の
;; イベントの代わりに差分を作るだけで、その先は Windows と同じ経路。
;; コストは実測で足りる (`directory-files-and-attributes' が 44 件 0.097 ms /
;; 936 件 5.97 ms)。アイドル中にしか走らせないので入力は邪魔しない。
;;
;; **ポーリングは可視のバッファだけを見る。** 隠れている dired は見えない
;; ものを貼り替えても意味がなく、可視になった次の窓で差分がまとめて出る。
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
;;
;; **mac では追従する。** ポーリングは表示している行を端から stat するので、
;; サブディレクトリ自身の mtime も差分に出る。わざわざ配下を覗きに
;; いかずに済むのはポーリング側の利点で、Windows に合わせて落とす理由も無い。

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

(defcustom my:dired-watch-poll-interval 1.0
  "stat を取り直す間隔 (秒)。`poll' のバックエンドでだけ使う。

**アイドルタイマーなので、この秒数は「アイドルが続いている間の間隔」。**
入力中は 1 度も走らない。応答は最悪このあと `my:dired-watch-debounce' で、
mac では 1.3 秒。通知で即座に拾える Windows (0.3 秒) との差はここ。"
  :type 'number)

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

(defvar-local my:dired-watch--snapshot nil
  "`poll' のバックエンドで前回 stat した結果のハッシュ。

絶対ファイル名 -> (SIZE MTIME MODES)。**nil なら「まだ基準が無い」**で、
その回は取るだけで差分を出さない (dired を開いた直後に全行が変化したことに
なるのを防ぐ)。")

(defvar-local my:dired-watch--timer nil)
(defvar-local my:dired-watch--last 0.0
  "最後に更新した時刻 (レート制限用)。")

(defvar my:dired-watch--poll-timer nil
  "`poll' のバックエンドのアイドルタイマー (グローバルに 1 本)。")

(defvar my:dired-watch--stats
  (list :events 0 :relisted 0 :reverted 0 :deferred 0 :throttled 0 :missing 0
        :polls 0)
  "統計。`my:dired-watch-stats' で表示する。")

;;; ---------------------------------------------------------------- 監視

(defconst my:dired-watch--backend
  (if (and (eq system-type 'windows-nt) (fboundp 'w32notify-add-watch))
      'w32notify
    'poll)
  "変化の見つけ方。`w32notify' (通知) か `poll' (stat の比較)。

**`file-notify' は使えない。** mac の kqueue も Linux の inotify も、
ディレクトリの watch では既存のファイルの中身の変化を報告しない
(Commentary の実測)。autorevert がそれで足りているのは、あちらが
行の増減しか見ていないから。")

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

(defun my:dired-watch--note-change (file)
  "FILE (絶対パス) が変わったことを控える。カレントバッファが対象。

通知とポーリングの合流点。件数が `my:dired-watch-relist-limit' を超えたら
1 行ずつ貼り替えるのをやめ、全体 revert に倒す印を立てる。"
  (let ((h (or my:dired-watch--pending
               (setq my:dired-watch--pending
                     (make-hash-table :test #'equal)))))
    (if (>= (hash-table-count h) my:dired-watch-relist-limit)
        (setq my:dired-watch--overflow t)
      (puthash file t h))))

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
            (my:dired-watch--note-change
             (expand-file-name (subst-char-in-string ?\\ ?/ name)
                               (file-name-as-directory dir)))
            (my:dired-watch--arm buffer my:dired-watch-debounce)))))))

(defun my:dired-watch--stat-dirs ()
  "表示しているディレクトリを stat して、絶対名 -> (SIZE MTIME MODES) を返す。

ID-FORMAT に `integer' を渡すのは uid / gid の名前解決を省くため。
ここで見るのはサイズ・更新日時・属性の 3 つだけで、持ち主は使わない。

`.' と `..' は入れない。前者に対応する行は無く (`total' の行になる)、
後者を貼り替えても意味が無い。w32notify 側もこの 2 つのイベントは出さない。"
  (let ((h (make-hash-table :test #'equal)))
    (dolist (dir (my:dired-watch--dirs))
      (dolist (e (ignore-errors
                   (directory-files-and-attributes dir t nil t 'integer)))
        (let ((f (car e)))
          (unless (member (file-name-nondirectory f) '("." ".."))
            (let ((a (cdr e)))
              (puthash f (list (file-attribute-size a)
                               (file-attribute-modification-time a)
                               (file-attribute-modes a))
                       h))))))
    h))

(defun my:dired-watch--poll-buffer ()
  "カレントバッファを stat し直し、変わった行を控える。控えたら非 nil。

**行の増減は見ない。** 現れた / 消えたファイルは autorevert の担当で、
こちらが触ると同じ変化で二重に revert することになる。両方の
スナップショットに在って値が違うものだけを拾う。"
  (let ((new (my:dired-watch--stat-dirs))
        (old my:dired-watch--snapshot)
        (found nil))
    (setq my:dired-watch--snapshot new)
    (when old
      (maphash (lambda (f v)
                 (let ((o (gethash f old)))
                   (when (and o (not (equal o v)))
                     (setq found t)
                     (my:dired-watch--note-change f))))
               new))
    found))

(defun my:dired-watch--arm-poll ()
  "走査のタイマーを張る。**定期タイマーであること。**

【重要】アイドルタイマーは使えない。2026-11-15 に 2 通り試して両方外れた。

  (1) REPEAT 付きの `run-with-idle-timer'
      あれは「**アイドル期間ごとに 1 回**」で、アイドルが続いている間は
      再実行されない (`timer-event-handler' が `timer-activate-when-idle'
      を DONT-WAIT nil で呼び直す)。しかも **emacsclient 経由の評価は
      アイドル時間をリセットしない**ので、実測では 1956 秒アイドルのまま
      **1 回も走らなかった**。dired を開いて放置し、外部の変更を待つ
      という**いちばん普通の使い方がそれに当たる**。

  (2) 走り終わるたびに `run-with-idle-timer' を張り直す
      **Emacs が CPU 100% で固まった** (実測。emacsclient も届かなくなり、
      SIGUSR2 でしか抜けられなかった)。`run-with-idle-timer' は
      DONT-WAIT を t で登録するので、**既にアイドルなら待たずに発火条件を
      満たす**。`timer-event-handler' は期限の来たタイマーを続けて処理
      するので、張り直したものが同じループでまた走り、**入力の処理まで
      戻ってこない**。

そこで定期タイマーにして、「入力中は走らせない」は `my:dired-watch--poll'
の側で `input-pending-p' を見て実現する。"
  (setq my:dired-watch--poll-timer
        (run-with-timer my:dired-watch-poll-interval
                        my:dired-watch-poll-interval
                        #'my:dired-watch--poll)))

(defun my:dired-watch--poll ()
  "アイドルタイマーから呼ばれる。**表示中の dired だけを見る。**

隠れているバッファまで stat すると、見えないものに毎秒払うことになる。
可視になれば次の窓で差分がまとめて出るので取りこぼしは無い (件数が多ければ
`my:dired-watch--overflow' で全体 revert に倒れる)。

同じバッファが 2 つのウィンドウに出ていることがあるので、見たバッファは
覚えて 1 回に絞る。リモート (tramp) は stat が高いので見ない。

**入力中は何もしない。** 定期タイマーなのでタイプしている間も起きるが、
stat は 936 件で 5.97 ms 払う。見送っても次の窓で拾える
\(アイドルタイマーで同じことをしようとして 2 度失敗した経緯は
`my:dired-watch--arm-poll' を参照)。"
  (unless (input-pending-p)
    (let ((seen nil))
      (dolist (frame (frame-list))
        (dolist (win (window-list frame 'no-minibuf))
          (let ((buf (window-buffer win)))
            (unless (memq buf seen)
              (push buf seen)
              (with-current-buffer buf
                (when (and (derived-mode-p 'dired-mode)
                           (not (file-remote-p default-directory)))
                  (cl-incf (plist-get my:dired-watch--stats :polls))
                  (when (my:dired-watch--poll-buffer)
                    (cl-incf (plist-get my:dired-watch--stats :events))
                    (my:dired-watch--arm buf my:dired-watch-debounce)))))))))))

(defun my:dired-watch--sync ()
  "`dired-after-readin-hook'。このバッファの監視を表示に合わせる。

revert とサブディレクトリの挿入の両方でここに来る。**貼り替えの間は
`dired-after-readin-hook' を nil に束縛しているので再入しない。**

`poll' のバックエンドでは watch を張る代わりに**基準の stat を取り直す**。
読み直した直後が基準になるので、いま反映した行が次の窓でもう一度
「変化した」と見えることがない。"
  (when (and my:dired-watch-mode (derived-mode-p 'dired-mode))
    (if (eq my:dired-watch--backend 'poll)
        (unless (file-remote-p default-directory)
          (setq my:dired-watch--snapshot (my:dired-watch--stat-dirs))
          (add-hook 'kill-buffer-hook #'my:dired-watch--unwatch nil t))
      (my:dired-watch--sync-w32notify))))

(defun my:dired-watch--sync-w32notify ()
  "表示中のディレクトリに w32notify の watch を合わせる。"
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
          (add-hook 'kill-buffer-hook #'my:dired-watch--unwatch nil t))))))

(defun my:dired-watch--unwatch ()
  "このバッファの監視をやめる。

`w32notify-rm-watch' は Windows にしか無い関数だが、`poll' のバックエンド
では `my:dired-watch--watches' が空なので dolist が空振りして呼ばれない。"
  (dolist (w my:dired-watch--watches)
    (ignore-errors (w32notify-rm-watch (cdr w))))
  (setq my:dired-watch--watches nil)
  (setq my:dired-watch--snapshot nil)
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

(defun my:dired-watch--aligned-p (file)
  "FILE の行が周りと桁が揃っていれば非 nil。

dired の行は必ず**マーク 1 桁 + 空白 1 桁**で始まる
(`dired-insert-directory' の `indent-rigidly')。3 桁になっているのは
`dired-align-file' が「空白を足すだけでは揃えられない」と諦めた印で、
そのとき**列の幅そのものが変わっている** (サイズが桁上がりしたなど)。
1 行の貼り替えでは直せないので、呼び出し側は全体 revert に倒す。

幅がずれる原因のうち、ls-lisp のグローバルな桁幅が他ディレクトリの
値のまま使われる件は `my-dired.el' の
`my:ls-lisp-narrow-single-entry' で潰してある。ここに残るのは
「そのファイル自身が列に収まらなくなった」場合だけ。"
  (save-excursion
    (and (dired-goto-file file)
         (progn (beginning-of-line) (looking-at-p "^. [^ ]")))))

(defun my:dired-watch--update-line (file)
  "FILE の行だけを貼り替える。
`relisted' (貼り替えた) / `misaligned' (桁が揃わなかった) / nil を返す。

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
                (if (my:dired-watch--aligned-p file)
                    (progn (my:dired-watch--annotate file) 'relisted)
                  ;; アイコンは付け直さない。どうせ revert で引き直される
                  'misaligned))))))

(declare-function diff-hl-dired-update "diff-hl-dired")

(defun my:dired-watch--update-vc-marks ()
  "貼り替えた行の VC マーク (diff-hl) を取り直す。

`diff-hl-dired-update' が載っているのは `dired-after-readin-hook' だけで、
1 行の貼り替えはそのフックを外して走る。**そのままではワークツリーを
書き換えてもマークが古いまま残る。** 全体 revert のときはフック経由で
取り直されるので、呼ぶのは貼り替えたときだけでよい。

**Windows では何もしない。** あちらは `my-magit-watch' がワークツリーを
subtree watch で見ていて、`my:magit-watch--refresh-dired' がリポジトリ単位で
取り直す。mac の kqueue はワークツリーの中身の変化を報告しないので
\(Commentary)、**ワークツリーの変更を VC マークに届けているのはここだけ**。
`.git/' 側の変化 (commit / stage / checkout) は mac でも `my-magit-watch' が
拾うので、そちらと二重にはならない。

1 回 47 ms (mac で実測)。1 行の貼り替え (0.71 ms) より桁違いに高いので、
`my:dired-watch-min-interval' のレート制限が効くこの位置に置く。
再入の抑制は `my-vc.el' の `my:diff-hl-dired-update-guard' が引き受ける。"
  (when (and (eq my:dired-watch--backend 'poll)
             (bound-and-true-p diff-hl-dired-mode)
             (fboundp 'diff-hl-dired-update)
             ;; 隠れたバッファには払わない (`my-vc.el' が可視になった時点で
             ;; 取り直す)。ポーリングは可視のものしか見ないが、デバウンスの
             ;; 間に隠れることはある
             (get-buffer-window (current-buffer) t))
    (diff-hl-dired-update)))

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
              ;; 桁が揃わなかった行が 1 つでもあれば、最後にまとめて
              ;; 読み直す。列の幅が変わったのだから、周りの行も含めて
              ;; 出し直すしかない。
              (let (widened relisted)
                (dolist (f files)
                  (pcase (my:dired-watch--update-line f)
                    ('relisted
                     (setq relisted t)
                     (cl-incf (plist-get my:dired-watch--stats :relisted)))
                    ('misaligned (setq widened t))
                    (_ (cl-incf (plist-get my:dired-watch--stats :missing)))))
                (if widened
                    (progn
                      (cl-incf (plist-get my:dired-watch--stats :reverted))
                      (revert-buffer))
                  ;; revert したときはフック経由で取り直されるので、
                  ;; 貼り替えだけで済んだときに呼ぶ
                  (when relisted (my:dired-watch--update-vc-marks))))))))))))

(defun my:dired-watch--arm (buffer delay)
  (with-current-buffer buffer
    (when my:dired-watch--timer (cancel-timer my:dired-watch--timer))
    (setq my:dired-watch--timer
          (run-with-timer delay nil #'my:dired-watch--fire buffer))))

;;; ---------------------------------------------------------------- コマンド

;;;###autoload
(defun my:dired-watch-stats ()
  "監視中のバッファ数と、これまでの更新の内訳を表示する。"
  (interactive)
  (let ((bufs 0) (watches 0))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (or my:dired-watch--watches my:dired-watch--snapshot)
          (cl-incf bufs)
          (cl-incf watches (length my:dired-watch--watches)))))
    (message "dired-watch[%s]: %d バッファ / watch %d / 走査 %d / 変化 %d / 貼り替え %d / 全体 revert %d / 行が無くて見送り %d / 操作中で待ち直し %d / レート制限で待ち直し %d"
             my:dired-watch--backend
             bufs watches
             (plist-get my:dired-watch--stats :polls)
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
        ;; `poll' のバックエンドではタイマーが監視そのもの。
        ;; 入力中に走らせない工夫は `my:dired-watch--poll' 側 (input-pending-p)。
        ;; **アイドルタイマーにしてはいけない** (暴走した。詳細は
        ;; `my:dired-watch--arm-poll' の docstring)。

        (when (eq my:dired-watch--backend 'poll)
          (unless my:dired-watch--poll-timer
            (my:dired-watch--arm-poll)))
        ;; 既にある dired バッファを拾う
        (dolist (b (buffer-list))
          (with-current-buffer b
            (when (derived-mode-p 'dired-mode) (my:dired-watch--sync)))))
    (remove-hook 'dired-after-readin-hook #'my:dired-watch--sync)
    (when my:dired-watch--poll-timer
      (cancel-timer my:dired-watch--poll-timer)
      (setq my:dired-watch--poll-timer nil))
    (dolist (b (buffer-list))
      (with-current-buffer b (my:dired-watch--unwatch)))))

;; Windows は通知で、それ以外は stat の比較で拾う (`my:dired-watch--backend')。
;; どちらも無い環境は無いので、条件を付けずに有効にする。
(my:dired-watch-mode 1)

(provide 'my-dired-watch)
;;; my-dired-watch.el ends here
