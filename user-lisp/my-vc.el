;;; my-vc.el --- 構成管理 (magit / diff-hl / SVN)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; 構成管理
;;; --------------------------------------------------

;;; [3] magit

(use-package magit
  :straight t
  :hook (magit-mode-hook . my:magit-setup-diff)
  ;; magit-status-internal は magit 4.x で削除済み。
  ;; git-commit-mode-hook はコマンドではなく変数なので :commands から外す。
  :commands magit-status-setup-buffer git-commit-mode
  ;; leaf の :advice は init 時にインライン展開される (eval-after-load に
  ;; 包まれない) ので :init に置く。advice-add は対象が未定義でも登録でき、
  ;; magit のロード時に有効になる。
  :init
  (advice-add 'magit-expand-git-file-name :filter-args
              #'magit-expand-git-file-name--msys)
  :config
  ;; --------------------------------------------------
  ;; 【重要】読み取りで .git/index.lock を取らせない (2026-10-16)
  ;;
  ;; magit は `--no-optional-locks' を付けない。そのため `status' や `diff'
  ;; のような **読み取りだけのコマンドでも index.lock を作る**
  ;; (実測: `status --porcelain' 1 回で `.git\index.lock' のイベントが 2 件。
  ;; `my-magit-watch.el' の Commentary を参照)。
  ;;
  ;; 素の magit なら「ユーザが操作した瞬間だけ」なので実害は無かったが、
  ;; この設定では事情が違う:
  ;;
  ;;   - `my-magit-watch' が外部の変更を検知して自動でリフレッシュする
  ;;   - `my-gitd' がその 0.1 秒後に **28 コマンドを 8 並列で先読み**する
  ;;
  ;; つまり Fork や Claude Code が `add' -> `commit' と続けている最中に、
  ;; Emacs 側の読み取りが index.lock を取りにいく。相手は
  ;; "Unable to create '.../index.lock': File exists" で落ちる。
  ;;
  ;; `--no-optional-locks' は「書かなくても答えが出せるときは index を
  ;; 書かない」という指示 (git 2.19 以降)。**書き込みコマンドの本物の
  ;; ロックには影響しない**ので commit や stage は従来どおり動く。
  ;;
  ;; 代償は「次のコマンドが stat をやり直すぶん遅くなる」ことだけだが、
  ;; magit の status リフレッシュは先頭で `update-index --refresh' を呼ぶ
  ;; (magit-status.el) ので、stat キャッシュはそこで更新される。
  ;;
  ;; `magit-process-git-arguments--split' は
  ;; `(length magit-git-global-arguments)' で位置的に切り分けるので、
  ;; **この変数に足す限り** `my-gitd' の役割判定 (`my:gitd--role') は
  ;; ずれない。コマンド側に直接足してはいけない。
  (unless (member "--no-optional-locks" magit-git-global-arguments)
    (setq magit-git-global-arguments
          (cons "--no-optional-locks" magit-git-global-arguments)))

  (defun magit-expand-git-file-name--msys (args)
    "Handle Msys directory names such as /c/* by changing them to C:/*"
    (let ((filename (car args)))
      (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
        (setq filename (concat (match-string 1 filename) ":/"
                               (match-string 2 filename))))
      (list filename)))
  ;; diff関連の設定
  (defun my:magit-setup-diff ()
    ;; diffを表示しているときに文字単位での変更箇所も強調表示する
    ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
    (setq magit-diff-refine-hunk 'all)
    ;; diff用のfaceを設定する
    (my:diff-mode-setup-faces)))

;;; [3] diff-hl (差分表示)

;; git-gutter と dired-k を diff-hl に統一した。
;;   git-gutter -> バッファの fringe に差分マーカー
;;   dired-k    -> dired の VC 状態マーカー
;; どちらも diff-hl が担う。diff-hl は vc 経由なので git / svn / hg を
;; 同じ仕組みで扱える (git-gutter の git-gutter:handled-backends は既定 '(git)
;; で、このリポジトリでは SVN 対応も入れているのに効いていなかった)。
;;
;; なお dired-k が持っていたファイルサイズ・更新日時の色分けは diff-hl には
;; 無い。ここは VC 状態の表示だけで、色分けは `my-dired-k.el' が担う。
;;
;; git-gutter は行頭に "~ + -" の文字を出していたが、diff-hl は fringe に
;; ビットマップを描く。色は face の foreground で決まるので、
;; git-gutter で背景色に使っていた色をそのまま foreground に移した。

(use-package diff-hl
  :straight t
  ;; :bind / :hook があると use-package が :config を eval-after-load で包む。
  ;; diff-hl を読み込む他のパッケージが無いので明示的にロードする
  ;; (leaf では :require t だったもの)。
  :demand t
  :bind
  ;; hydra 起動のキーバインド (git-gutter 時代と同じ C-c g)
  ("C-c g" . hydra-diff-hl/body)
  :custom
  ;; 保存前のバッファでも差分を追う (git-gutter と同じ感覚にする)
  (diff-hl-flydiff-delay 0.5)
  ;; use-package の :custom-face は face-spec-set (defface spec) を使うため
  ;; modus-vivendi の theme-face に負けて色が反映されない。leaf の
  ;; :custom-face は custom-set-faces (user テーマ) で、こちらはテーマに勝つ。
  ;; 同じ挙動にするため custom-set-faces を直接呼ぶ。
  :init
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "#50fa7b"))))
   '(diff-hl-change ((t (:foreground "#f1fa8c"))))
   '(diff-hl-delete ((t (:foreground "#ff79c6")))))
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  ;; magit の操作後にマーカーを更新する
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ;; 上の diff-hl-magit-post-refresh は「ファイルを訪問しているバッファ」しか
  ;; 見ないので、dired のマークは別に取り直す (:init の長いコメントを参照)。
  (magit-post-refresh-hook . my:diff-hl-dired-magit-post-refresh)
  ;; C-x v v (vc-checkin) から commit した場合。上流の diff-hl-after-checkin も
  ;; 同じフックにいるが、あちらも file バッファだけが対象。
  (vc-checkin-hook . my:diff-hl-dired-update-repo)
  ;; 隠れている間に古くなった dired を、可視になった時点で取り直す (段階 2)。
  (window-buffer-change-functions . my:diff-hl-dired--update-on-display)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  :init
  ;; --------------------------------------------------
  ;; diff-hl-dired の再入対策 (2026-09-05)
  ;;
  ;; 症状: dired を開いていると
  ;;   Buffer " *diff-hl-dired* tmp status" has a running process; kill it?
  ;; が頻繁に出る。
  ;;
  ;; 出所は `process-kill-buffer-query-function' (subr.el)。`kill-buffer' した
  ;; バッファに status が run のプロセスがぶら下がっていると聞いてくる。
  ;; そのバッファを作って kill しているのは diff-hl-dired だけ:
  ;;
  ;;   diff-hl-dired.el:101  前回のチェーンが生きていれば kill-process して
  ;;                         一時バッファ (1 個だけ) を使い回す
  ;;   diff-hl-dired.el:143  チェーンが終わったら kill-buffer する
  ;;
  ;; vc-git の dir-status-files は update-index -> diff-index ->
  ;; ls-files-missing -> ... -> ls-files-ignored とプロセスを 6 回前後
  ;; リレーする (vc-git.el の `vc-git-after-dir-status-stage')。Windows は
  ;; spawn 1 回が 55 ms 前後なので、1 チェーンで 0.5〜1 秒かかる。
  ;;
  ;; `diff-hl-dired-update' は `dired-after-readin-hook' に載っているので、
  ;; my-dired.el で dired を auto-revert させて以降 (2026-09-04)、この 1 秒の
  ;; 間に次の update が来るようになった。
  ;;
  ;; 再入したとき kill-process で前のチェーンを止められるのは「そのプロセスが
  ;; まだ生きている」ときだけ。プロセスは終了済みで sentinel がまだ走って
  ;; いない瞬間に再入すると kill-process は何もせず、旧チェーンが後から再開
  ;; して新チェーンと同じバッファで交錯する (タイマーと sentinel はどちらも
  ;; コマンドループの同じ場所で回るので、どちらが先かは保証されない)。
  ;; 先に終わった側が kill-buffer を呼び、そこには相手のプロセスが走って
  ;; いる、というのがあのプロンプト。
  ;;
  ;; 困るのはプロンプトだけではない。両チェーンが同じバッファを erase-buffer
  ;; し合うので、dired のマーカーが欠けたり古いままになったりする。
  ;;
  ;; 対策は 2 つ。
  ;;
  ;;   (1) 一時バッファで `kill-buffer-query-functions' を nil にする。
  ;;       中身は読み取り専用の git なので、途中で殺して困るものは無い。
  ;;   (2) 前のチェーンが走っている間は新しいチェーンを始めない。
  ;;       交錯そのものを避け、連続する revert を 1 回に畳む。
  ;;
  ;; (2) は「遅らせる」のではなく「走っていなければ即実行、走っていれば
  ;; 終わるのを待って 1 回だけ」にしてある。dired を開いた直後や g を押した
  ;; ときは従来どおり即座に走る。

  (defvar my:diff-hl-dired-poll-interval 0.5
    "前のチェーンが終わったかを見に行く間隔 (秒)。")

  (defvar my:diff-hl-dired-max-wait 5.0
    "チェーンが「死んでいる」と判断するまでの無プロセス時間 (秒)。

**プロセスが走っている間は、何秒経っても待つ。** ここで測るのは
「一時バッファは生きているのに git が 1 つも走っていない」状態が
続いた時間で、これを超えたら一時バッファが居座っているとみなして
新しいチェーンを始める。そのときプロセスはいないので
`kill-process' は走らない。

チェーンは 6 段のリレーなので段の切れ目では一瞬プロセスが途切れる。
その一瞬を「死んでいる」と誤認しないための猶予でもある。")

  (defvar my:diff-hl-dired-hard-max-wait 60.0
    "1 回の待ちの総上限 (秒)。これを超えたら**殺さずに**諦める。

諦めたときは `my:diff-hl-dired--stale' を立てるので、次に可視になった
ときか次の readin で取り直される。マークが 1 回古いまま残るだけで、
`.git/index.lock' を残すよりはるかに安い。")

  (defvar-local my:diff-hl-dired--timer nil
    "待ち直し用のタイマー。dired バッファごとに 1 本。")

  (defvar-local my:diff-hl-dired--deadline nil
    "待ちを諦める時刻 (`float-time')。待っていないときは nil。
`my:diff-hl-dired-hard-max-wait' で決まる総上限のほう。")

  (defvar-local my:diff-hl-dired--last-alive nil
    "最後にチェーンのプロセスが生きているのを見た時刻 (`float-time')。

段の切れ目と「一時バッファだけが居座っている」状態を区別するために要る。
`my:diff-hl-dired-max-wait' はこの時刻からの経過で測る。")

  (defvar-local my:diff-hl-dired--stale nil
    "表示していない間に VC 状態が変わったら t。可視になった時点で取り直す。

立てるのは `my:diff-hl-dired-update-repo' (下の段階 2 のブロック)。
下ろすのは `my:diff-hl-dired--start'、つまり**取り直しが実際に走ったとき**。
`dired-after-readin-hook' 経由の更新でもそこを通るので、隠れている間に
autorevert が revert したバッファに無駄な取り直しが残らない。")

  (defun my:diff-hl-dired--in-flight-p ()
    "この dired バッファの status チェーンがまだ走っているなら非 nil。

一時バッファはチェーンの最後に kill されるので、生きていることが
そのまま「走っている」ことの印になる。"
    (buffer-live-p (bound-and-true-p diff-hl-dired-process-buffer)))

  (defun my:diff-hl-dired--process ()
    "チェーンの一時バッファにぶら下がっているプロセスを返す。無ければ nil。

終了済みのプロセスは sentinel が走った時点で外れるので、非 nil なら
ほぼ「走っている」。**ただし「終了したが sentinel がまだ」の一瞬だけは
終了済みのものが返る。** そこで `kill-process' を呼ぶと
\"Process ... is not active\" で落ちるので、`my:diff-hl-dired--start' は
`process-live-p' ではなくこの関数で見る (いるなら触らない)。"
    (and (my:diff-hl-dired--in-flight-p)
         (get-buffer-process diff-hl-dired-process-buffer)))

  (defun my:diff-hl-dired--live-process-p ()
    "チェーンの git が **いま実際に走っている**なら非 nil。

`my:diff-hl-dired--in-flight-p' との違いが肝。あちらは一時バッファの
生死しか見ないので、「バッファは残っているがプロセスはいない」
\(段の切れ目 / チェーンが途中で死んだ) と区別が付かない。"
    (let ((p (my:diff-hl-dired--process)))
      (and p (process-live-p p))))

  ;; --------------------------------------------------
  ;; 【重要】走っている git を殺さない (2026-10-16)
  ;;
  ;; `diff-hl-dired-update' は、前のチェーンが生きていると
  ;; **問答無用で `kill-process' する** (diff-hl-dired.el:101)。
  ;;
  ;;   (when proc (kill-process proc))
  ;;
  ;; Windows の `kill-process' は `TerminateProcess' なので、git は
  ;; atexit の後始末を走らせずに死ぬ。**チェーンの第 1 段は
  ;; `git update-index --refresh'** (vc-git.el の
  ;; `vc-git-dir-status-goto-stage') で、これは起動直後に
  ;; `.git/index.lock' を作りっぱなしのまま消える。
  ;;
  ;; こうなると Fork でも Claude Code でも、以後その リポジトリの git は
  ;; **何度やっても失敗する** (手で index.lock を消すまで)。実際に起きていた。
  ;;
  ;; そこで `my:diff-hl-dired--start' は、生きたプロセスがあるときは
  ;; **ORIG を呼ばずに nil を返す**。上流の kill-process に到達する道を
  ;; 構造的に塞ぐのが目的で、タイムアウトの値を延ばすのでは駄目
  ;; (値がいくつでも「超えたら殺す」経路は残る)。
  ;;
  ;; 待ちが長引いたときは殺さずに諦める (`my:diff-hl-dired-hard-max-wait')。
  ;; マークが 1 回古いまま残るだけで、印 (`--stale') を立てておけば
  ;; 次に可視になった時点で取り直される。

  (defun my:diff-hl-dired--start (orig)
    "ORIG (`diff-hl-dired-update') を実際に呼ぶ。実行したら t を返す。

**プロセスがぶら下がっているときは呼ばない** (nil を返す)。呼ぶと ORIG が
`kill-process' して `.git/index.lock' を残す。最後の安全網なので、
呼び出し側の判断 (`--run' / `--update-guard') とは独立に置いてある。"
    (if (my:diff-hl-dired--process)
        nil
      ;; 待ちは終わり。**張ってあるタイマーを必ず外す。** 残すと、これから
      ;; 始めるチェーンの最中に前の待ちのタイマーが起きて、状態を見失う
      ;; (`--update-guard' から入ったときは `--run' が外してくれない)
      (when (timerp my:diff-hl-dired--timer)
        (cancel-timer my:diff-hl-dired--timer))
      (setq my:diff-hl-dired--timer nil)
      (setq my:diff-hl-dired--deadline nil)
      (setq my:diff-hl-dired--last-alive nil)
      ;; ここまで来れば取り直される。**入口がどれであっても**印は下ろしてよい。
      (setq my:diff-hl-dired--stale nil)
      (funcall orig)
      ;; 一時バッファはここで (再) 生成されている。バッファは使い回されるうえ
      ;; チェーンの終わりに kill されるので、毎回張り直す。
      (let ((buffer (bound-and-true-p diff-hl-dired-process-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil))))
      t))

  (defun my:diff-hl-dired--rearm (buffer orig)
    "BUFFER で ORIG を呼び直すタイマーを張り直す。"
    (when (timerp my:diff-hl-dired--timer)
      (cancel-timer my:diff-hl-dired--timer))
    (setq my:diff-hl-dired--timer
          (run-at-time my:diff-hl-dired-poll-interval nil
                       #'my:diff-hl-dired--run buffer orig)))

  (defun my:diff-hl-dired--give-up ()
    "待ちを諦める。**殺さない。** 印を立てて次の機会に回す。"
    (setq my:diff-hl-dired--deadline nil)
    (setq my:diff-hl-dired--last-alive nil)
    (setq my:diff-hl-dired--stale t))

  (defun my:diff-hl-dired--run (buffer orig)
    "待ちから復帰したときの入口。BUFFER が生きていれば判断し直す。"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq my:diff-hl-dired--timer nil)
        (let ((now (float-time)))
          ;; 入口がどこであっても基準時刻は必ず持たせる
          (unless my:diff-hl-dired--last-alive
            (setq my:diff-hl-dired--last-alive now))
          (cond
           ;; 終わった。素直に走らせる
           ((not (my:diff-hl-dired--in-flight-p))
            (or (my:diff-hl-dired--start orig)
                (my:diff-hl-dired--rearm buffer orig)))
           ;; 総上限。**殺さずに**諦める
           ((and my:diff-hl-dired--deadline
                 (>= now my:diff-hl-dired--deadline))
            (my:diff-hl-dired--give-up))
           ;; git が走っている。何秒かかっていても待つ
           ((my:diff-hl-dired--live-process-p)
            (setq my:diff-hl-dired--last-alive now)
            (my:diff-hl-dired--rearm buffer orig))
           ;; プロセスはいない。段の切れ目かもしれないので少し待つ
           ((< (- now my:diff-hl-dired--last-alive) my:diff-hl-dired-max-wait)
            (my:diff-hl-dired--rearm buffer orig))
           ;; 無プロセスのまま居座っている = チェーンが途中で死んでいる。
           ;; ここで ORIG を呼んでも殺す相手がいないので安全
           (t (or (my:diff-hl-dired--start orig)
                  (my:diff-hl-dired--rearm buffer orig))))))))

  (defun my:diff-hl-dired-update-guard (orig &rest _args)
    "`diff-hl-dired-update' の再入を防ぐ。

走っているチェーンが無ければそのまま呼ぶ。走っていれば呼ばず、
終わるのを待って 1 回だけ呼ぶ (待っている間に来た分は畳まれる)。"
    (let ((buffer (current-buffer)))
      ;; 【重要】判断は `--in-flight-p' (一時バッファの生死) で行う。
      ;; `--live-process-p' にしてはいけない。段の切れ目はプロセスが
      ;; いないので、そこで新しいチェーンを始めてしまい、**2 本が同じ
      ;; 一時バッファを erase-buffer し合う** (2026-09-05 に直した再入そのもの)。
      (if (and (not (my:diff-hl-dired--in-flight-p))
               (my:diff-hl-dired--start orig))
          t
        (let ((now (float-time)))
          (unless my:diff-hl-dired--deadline
            (setq my:diff-hl-dired--deadline
                  (+ now my:diff-hl-dired-hard-max-wait))
            (setq my:diff-hl-dired--last-alive now)))
        (my:diff-hl-dired--rearm buffer orig)
        nil)))

  (advice-add 'diff-hl-dired-update :around #'my:diff-hl-dired-update-guard)

  ;; --------------------------------------------------
  ;; 【重要】チェーン全体で optional lock を取らない (2026-10-16)
  ;;
  ;; `vc-git-dir-status-files' のリレー 6 段は、どれも index を書きうる。
  ;; Emacs 31.1 の vc-git には逃げ道が用意されているが、
  ;;
  ;;   (and (or revert-buffer-in-progress (equal subcommand "status"))
  ;;        '("--no-optional-locks"))            ; vc-git.el:2904
  ;;
  ;; **このチェーンに "status" は 1 つも無い** (update-index / diff-index /
  ;; ls-files だけ) ので、1 つも効いていない。
  ;;
  ;; `GIT_OPTIONAL_LOCKS=0' を環境変数で渡して全段に効かせる。
  ;; `revert-buffer-in-progress' を t に束縛しても同じ結果になるが、
  ;; あれはグローバルな動的変数で「revert 中である」という嘘を
  ;; 他のコードにも見せてしまうので採らない。
  ;;
  ;; **第 1 段の `update-index --refresh' だけはこれでも消えない。**
  ;; あのコマンドは起動直後に `LOCK_DIE_ON_ERROR' で index をロックするのが
  ;; 仕事そのもので、optional lock ではない。そちらは
  ;; `my-magit-watch.el' の `my:magit-watch--locked-p' (外部の git が
  ;; ロックを持っている間は走らせない) で避ける。
  ;;
  ;;; 2 段目以降をどうやって捕まえるか
  ;;
  ;; `vc-git-dir-status-goto-stage' は、2 段目以降は **sentinel から**
  ;; 呼ばれる。`diff-hl-dired-update' を let で包んでも第 1 段にしか
  ;; 効かないので、入口そのものに advice を張る。
  ;;
  ;;   第 1 段   `my:diff-hl-dired--in-chain' (動的束縛) で見分ける
  ;;   2 段目以降 一時バッファに立てたバッファローカルの印で見分ける
  ;;
  ;; 印を立てるのは advice の中。第 1 段の時点で current-buffer は
  ;; 既に一時バッファになっている (diff-hl が `with-current-buffer' で
  ;; 包んでから `dir-status-files' を呼ぶ) ので、ここで立てておけば
  ;; sentinel から来る 2 段目以降が拾える。

  (defvar my:diff-hl-dired--in-chain nil
    "`diff-hl-dired-update' の呼び出し中なら非 nil (動的束縛)。")

  (defvar-local my:diff-hl-dired--chain-buffer nil
    "この一時バッファが diff-hl のチェーン用なら t。")

  (defun my:diff-hl-dired--no-optional-locks (orig &rest args)
    "diff-hl のチェーンに `GIT_OPTIONAL_LOCKS=0' を効かせる。"
    (if (not (or my:diff-hl-dired--in-chain my:diff-hl-dired--chain-buffer))
        (apply orig args)
      (setq-local my:diff-hl-dired--chain-buffer t)
      (let ((process-environment (cons "GIT_OPTIONAL_LOCKS=0"
                                       process-environment)))
        (apply orig args))))

  (advice-add 'vc-git-dir-status-goto-stage :around
              #'my:diff-hl-dired--no-optional-locks)

  (defun my:diff-hl-dired--mark-chain (orig &rest args)
    "`diff-hl-dired-status-files' の呼び出し中を印で囲む。"
    (let ((my:diff-hl-dired--in-chain t))
      (apply orig args)))

  (advice-add 'diff-hl-dired-status-files :around
              #'my:diff-hl-dired--mark-chain)

  ;; --------------------------------------------------
  ;; 【重要】ファイルが多いと extra indicators の段が必ず落ちる (2026-10-29)
  ;;
  ;; 症状: ファイル数の多いディレクトリを dired で開くと、revert のたびに
  ;;
  ;;   error in process sentinel: vc-git-command: Spawning child process: Exec format error
  ;;   error in process sentinel: Spawning child process: Exec format error
  ;;
  ;; が出続ける。**「実行形式が壊れている」という意味ではない。** ENOEXEC で、
  ;; Windows の `CreateProcess' のコマンドライン上限 (32768 文字) を超えた
  ;; ときの表示。
  ;;
  ;; 出所は diff-hl の extra indicators (unregistered / ignored の色分け)。
  ;;
  ;;   diff-hl-dired-update                `dired-after-readin-hook' から
  ;;   -> diff-hl-dired-nondirectory-files **一覧の全ファイル名**を集める
  ;;                                       (git ではなく dired の表示から作る)
  ;;   -> diff-hl-dir-status-ignored-files
  ;;   -> vc-git-dir-status-goto-stage の `ls-files-ignored' 段
  ;;   -> git ls-files -z -o -i --directory --no-empty-directory
  ;;        --exclude-standard -- <全ファイル名>
  ;;
  ;; 実測 (emacs -Q --batch、使い捨てリポジトリで段を直接叩いた):
  ;;
  ;;    700 ファイル  引数  28669 文字  OK
  ;;    800 ファイル  引数  32769 文字  **Exec format error**
  ;;   4375 ファイル  引数 179344 文字  同上
  ;;
  ;; 境界はちょうど 32768。生きている Emacs で `call-process "git"' の引数を
  ;; 二分探索すると、成功する最大は 32719 文字だった。
  ;;
  ;; **チェーンの 1〜5 段目はファイル名を渡さないので通り、最後のこの段だけが
  ;; 必ず落ちる。** センチネルから起動される段なので "error in process
  ;; sentinel" になり、revert のたびに繰り返す。
  ;;
  ;; **`.gitignore' に入れても直らない。** 渡す一覧は git の状態ではなく
  ;; dired の表示から作られる。git リポジトリ配下なら、どのディレクトリでも
  ;; 700〜800 件を超えた時点で起きる。
  ;;
  ;;; 対処: 長すぎるときはこの段だけ飛ばす
  ;;
  ;; update-function を「ignored は 0 件」として呼べば、呼び出し側
  ;; (`diff-hl-dired-status-files' の内側のラムダ) が本来の entries で
  ;; チェーンを締める。**変更・追加のマークは通常どおり出る**ので、失うのは
  ;; そのディレクトリの unregistered / ignored の色分けだけ。
  ;;
  ;; 一覧を分割して複数回に分ける手もあるが、あの段は結果をまとめてから
  ;; update-function を呼ぶ作りなので、リレーを自前で回すことになる。
  ;; 上流 (diff-hl) 側のバグなので、そちらに投げるのが筋。

  (defvar my:diff-hl-dired-max-ignored-args 30000
    "`ls-files-ignored' の段に渡せる引数の総文字数。超えたらその段を飛ばす。

Windows の `CreateProcess' はコマンドライン全体で 32768 文字まで。
git 自身のパスと固定の引数、`vc-do-command' が空白を含む名前に付ける
引用の分を引いた安全側の値。")

  (defvar my:diff-hl-dired--huge-reported (make-hash-table :test #'equal)
    "飛ばしたことを既に報告したディレクトリ。revert のたびに言わないため。")

  (defun my:diff-hl-dired--args-length (files)
    "FILES を引数に並べたときのおおよその文字数。
空白を含む名前は引用されるので、1 件につき 3 文字 (区切り + 引用符 2) 見る。"
    (seq-reduce (lambda (acc f) (+ acc (length f) 3)) files 0))

  (defun my:diff-hl-dired--report-huge (dir n len)
    "DIR で段を飛ばしたことを 1 度だけ報告する。"
    (let ((key (let ((d (file-name-as-directory (expand-file-name dir))))
                 (if (file-name-case-insensitive-p d) (downcase d) d))))
      (unless (gethash key my:diff-hl-dired--huge-reported)
        (puthash key t my:diff-hl-dired--huge-reported)
        (message "diff-hl-dired: %s は %d ファイル (引数 %d 文字) なので ignored の判定を省きます"
                 (abbreviate-file-name dir) n len))))

  (defun my:diff-hl-dired--skip-huge-ignored (orig backend dir files update-function)
    "引数が長すぎるときは `ls-files-ignored' の段を飛ばす。"
    (let ((len (my:diff-hl-dired--args-length files)))
      (if (<= len my:diff-hl-dired-max-ignored-args)
          (funcall orig backend dir files update-function)
        (my:diff-hl-dired--report-huge dir (length files) len)
        ;; 「ignored は 0 件、続きは無い」。呼び出し側がここで本来の entries を
        ;; 流してチェーンを締めるので、一時バッファも畳まれる。
        (funcall update-function nil nil))))

  (advice-add 'diff-hl-dir-status-ignored-files :around
              #'my:diff-hl-dired--skip-huge-ignored)

  ;; --------------------------------------------------
  ;; commit のあとに dired のマークを取り直す (2026-09-17)
  ;;
  ;; `diff-hl-dired' がマークを更新する経路は **1 つしかない**。
  ;;
  ;;   (add-hook 'dired-after-readin-hook 'diff-hl-dired-update 10 t)
  ;;
  ;; つまり **dired が一覧を読み直したときだけ**で、行が増減しない変化は
  ;; すべて取り残される。
  ;;
  ;;   作成 / 削除 / 改名  -> ディレクトリの mtime が変わる -> autorevert が
  ;;                         revert -> readin -> **更新される**
  ;;   commit / stage /    -> 一覧は変わらない -> revert されない ->
  ;;   checkout / stash      **マークが古いまま残る**
  ;;
  ;; `my-dired-watch' が行を貼り替える経路でも更新されない。あちらは
  ;; `dired-after-readin-hook' を nil に束縛する (アイコンの再描画に 453 ms
  ;; かかるため)。**行だけ最新になってマークが置いていかれる。**
  ;;
  ;; 上流の magit 連携 (`diff-hl-magit-post-refresh') は埋めてくれない。
  ;; あれは `diff-hl--buffer-file-name' が非 nil のバッファ、つまり
  ;; **ファイルを訪問しているバッファしか見ない**ので、dired は 1 件も通らない。
  ;;
  ;; 実測 (2026-09-17、使い捨てリポジトリで外部から commit):
  ;;
  ;;   commit 前  a.txt=change / untracked.txt=unknown
  ;;   commit 後  **同じまま** (ワークツリーは clean)
  ;;   ここで (diff-hl-dired-update) を 1 回呼ぶ -> マークが消えた
  ;;
  ;;; 【重要】これはテキストを触らないので全体でよい
  ;;
  ;; `my-dired-watch' が「全体 revert ではなく 1 行だけ貼り替える」ことにした
  ;; のは、テキストの貼り替えが高価だったから。**ここは事情が違う。**
  ;; `diff-hl-dired-highlight-items' は overlay を消して貼り直すだけで、
  ;; 実測でも `buffer-chars-modified-tick' は動かず point も保たれた。
  ;;
  ;; そもそも行単位にはできない。`diff-hl-dired-clear' がバッファ全体の
  ;; overlay を消すところから始まるし、データ取得 (`dir-status-files') が
  ;; ディレクトリ単位なので 1 ファイルだけ知りたくてもコストは同じ。
  ;; commit では **マークが付いていた全ファイルが同時に変わる**ので、
  ;; 全体でなければ嘘が残る。
  ;;
  ;;; 表示中のバッファだけにする理由
  ;;
  ;; 1 バッファあたり `vc-do-command' が 6 回 = 6 プロセスで 0.51 秒 (実測、
  ;; 非同期)。うち 0.36 秒は Windows のプロセス生成コスト。**gitd は効かない**
  ;; \(あれが横取りするのは `magit-process-file' で、ここは vc-git 経由)。
  ;; dired バッファを 10 個開いていると commit 1 回で 60 プロセスになる。
  ;;
  ;; 表示していないバッファには `my:diff-hl-dired--stale' を立てるだけにして、
  ;; **可視になった時点で** `window-buffer-change-functions' から取り直す。
  ;; 印を下ろすのは `my:diff-hl-dired--start'、つまり入口によらず
  ;; 「実際に取り直しが走ったとき」なので、隠れている間に autorevert が
  ;; revert したバッファに無駄な取り直しが残らない。
  ;;
  ;; 重複の抑制は上の `my:diff-hl-dired-update-guard' がそのまま引き受ける
  ;; \(走っていれば待って 1 回だけ)。
  ;;
  ;;; 入口は 3 つ
  ;;
  ;;   `magit-post-refresh-hook'  magit のコマンド直後と `g' (段階 1)
  ;;   `vc-checkin-hook'          C-x v v での commit      (段階 1)
  ;;   `my:magit-watch--refresh'  外部の git と自動更新     (段階 2)
  ;;
  ;; 段階 2 は `my-magit-watch.el' 側から `my:diff-hl-dired-update-repo' を
  ;; 呼ぶ形。**切り戻せるようにしてある** (`my:magit-watch-update-dired' を
  ;; nil にすると段階 1 だけに戻る)。

  (defun my:diff-hl-dired--under-p (dir root)
    "DIR が ROOT 配下なら非 nil。

【重要】`file-equal-p' は使えない。`my-platform.el' が
`w32-get-true-file-attributes' を nil にしているため inode が常に 0 で返り、
**同じドライブのディレクトリがすべて「同じ」と判定される** (CLAUDE.md)。
文字列で比べる。stat を打たないので速くもある。"
    (let ((d (file-name-as-directory (expand-file-name dir)))
          (r (file-name-as-directory (expand-file-name root))))
      (if (file-name-case-insensitive-p r)
          ;; Windows は子プロセスの作業ディレクトリのドライブレターを
          ;; 小文字にするので、大小が食い違ったまま届く (CLAUDE.md)。
          (string-prefix-p (downcase r) (downcase d))
        (string-prefix-p r d))))

  (defun my:diff-hl-dired-update-repo (&optional root)
    "ROOT 配下の dired バッファでマークを取り直す。取り直した数を返す。

**実際に走るのは表示中のバッファだけ。** 表示していないものには
`my:diff-hl-dired--stale' を立て、可視になった時点で
`my:diff-hl-dired--update-on-display' が拾う。1 バッファあたり 6 プロセス
0.51 秒かかるので、見ていないバッファのために払うには高い。

ROOT を省略すると `vc-root-dir' で求める (`vc-checkin-hook' 用)。
求まらなければ何もしない。"
    (let ((n 0))
      (when-let* ((root (or root (ignore-errors (vc-root-dir)))))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and (derived-mode-p 'dired-mode)
                       (bound-and-true-p diff-hl-dired-mode)
                       ;; wdired 中は触らない。編集中のバッファに overlay を
                       ;; 張り直すことになる (`my-dired-watch' と同じ判定)
                       buffer-read-only
                       (my:diff-hl-dired--under-p default-directory root))
              (if (get-buffer-window buf t)
                  (progn (diff-hl-dired-update)
                         (setq n (1+ n)))
                (setq my:diff-hl-dired--stale t))))))
      n))

  (defvar my:diff-hl-dired--display-timer nil
    "`my:diff-hl-dired--update-stale' を呼ぶアイドルタイマー。")

  (defun my:diff-hl-dired--update-stale ()
    "表示中の dired のうち、印が立っているものを取り直す。"
    (setq my:diff-hl-dired--display-timer nil)
    (dolist (frame (frame-list))
      (dolist (win (window-list frame 'no-minibuf))
        (with-current-buffer (window-buffer win)
          (when (and my:diff-hl-dired--stale
                     (derived-mode-p 'dired-mode)
                     (bound-and-true-p diff-hl-dired-mode)
                     buffer-read-only)
            ;; 印は my:diff-hl-dired--start が下ろす (待たされた場合も含めて
            ;; 実際に走った時点で下りる)
            (diff-hl-dired-update))))))

  (defun my:diff-hl-dired--update-on-display (&optional frame)
    "`window-buffer-change-functions'。可視になった dired を取り直す。

【重要】**ここで `diff-hl-dired-update' を直接呼ばない。** この種のフックは
redisplay から走るので、6 プロセスを起こすのはアイドルタイマーへ逃がす。
表示が落ち着くまで待てば、連続した切り替えが 1 回にまとまる利点もある。"
    (unless my:diff-hl-dired--display-timer
      (when (seq-some (lambda (win)
                        (buffer-local-value 'my:diff-hl-dired--stale
                                            (window-buffer win)))
                      (window-list frame 'no-minibuf))
        (setq my:diff-hl-dired--display-timer
              (run-with-idle-timer 0.2 nil #'my:diff-hl-dired--update-stale)))))

  (defun my:diff-hl-dired-magit-post-refresh ()
    "`magit-post-refresh-hook'。magit の操作後に dired のマークを取り直す。

このフックは `magit-refresh' でしか走らない。つまり **ユーザが `g' を
押したときと magit のコマンド (commit / stage / checkout ...) の直後**の
2 つで、`my-magit-watch' の自動更新 (`magit-refresh-buffer') では走らない。
外部の git に追従させるのは段階 2。

ルートは `magit-toplevel' で取る。このフックは `magit-refresh' の中、
つまり `magit--refresh-cache' が束縛された状態で走るので git は起きない。"
    (my:diff-hl-dired-update-repo (ignore-errors (magit-toplevel))))

  ;; leaf の :hydra 相当 (init 時にインライン展開される)。
  :init
  (defhydra hydra-diff-hl nil
                 "diff hunk"
    ("p" diff-hl-previous-hunk "previous")
    ("n" diff-hl-next-hunk "next")
    ("s" diff-hl-stage-dwim "stage")
    ("r" diff-hl-revert-hunk "revert")
    ("SPC" diff-hl-show-hunk "show diff")))

;;; [3] Windows 環境でのSVN support

;; 疑似パッケージなので use-package の名前は emacs にする。
(use-package emacs
  :if (eq system-type 'windows-nt)
  :hook
  ;; svn log の出力は cp932
  (vc-svn-log-view-mode-hook . (lambda () (set-process-coding-system 'cp932 'cp932)))
  :config
  ;; Windows 上の SVN で日本語ファイル名がうまく扱えない問題への対応
  ;; (一時的に default-process-coding-system を '(utf-8 . cp932) に変更する)
  (defun my:vc-svn-command-with-cp932 (orig &rest args)
    (let ((default-process-coding-system '(utf-8 . cp932)))
      (apply orig args)))
  (advice-add 'vc-svn-command :around #'my:vc-svn-command-with-cp932))

(provide 'my-vc)
;;; my-vc.el ends here
