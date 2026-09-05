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
;; 無いので失われる。VC 状態の表示だけになる。
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
    "前のチェーンの終了を待つ上限 (秒)。

これを超えたら待たずに走らせる (= diff-hl 本来の kill-process する動作)。
一時バッファが何かの理由で残ったままになったときに、更新が永久に
止まってしまうのを防ぐための保険。")

  (defvar-local my:diff-hl-dired--timer nil
    "待ち直し用のタイマー。dired バッファごとに 1 本。")

  (defvar-local my:diff-hl-dired--deadline nil
    "待ちを諦める時刻 (`float-time')。待っていないときは nil。")

  (defun my:diff-hl-dired--in-flight-p ()
    "この dired バッファの status チェーンがまだ走っているなら非 nil。

一時バッファはチェーンの最後に kill されるので、生きていることが
そのまま「走っている」ことの印になる。"
    (buffer-live-p (bound-and-true-p diff-hl-dired-process-buffer)))

  (defun my:diff-hl-dired--start (orig)
    "ORIG (`diff-hl-dired-update') を実際に呼び、一時バッファを黙らせる。"
    (setq my:diff-hl-dired--deadline nil)
    (funcall orig)
    ;; 一時バッファはここで (再) 生成されている。バッファは使い回されるうえ
    ;; チェーンの終わりに kill されるので、毎回張り直す。
    (let ((buffer (bound-and-true-p diff-hl-dired-process-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions nil)))))

  (defun my:diff-hl-dired--rearm (buffer orig)
    "BUFFER で ORIG を呼び直すタイマーを張り直す。"
    (when (timerp my:diff-hl-dired--timer)
      (cancel-timer my:diff-hl-dired--timer))
    (setq my:diff-hl-dired--timer
          (run-at-time my:diff-hl-dired-poll-interval nil
                       #'my:diff-hl-dired--run buffer orig)))

  (defun my:diff-hl-dired--run (buffer orig)
    "待ちから復帰したときの入口。BUFFER が生きていれば ORIG を呼ぶ。"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq my:diff-hl-dired--timer nil)
        (if (and (my:diff-hl-dired--in-flight-p)
                 (< (float-time) (or my:diff-hl-dired--deadline 0)))
            (my:diff-hl-dired--rearm buffer orig)
          (my:diff-hl-dired--start orig)))))

  (defun my:diff-hl-dired-update-guard (orig &rest _args)
    "`diff-hl-dired-update' の再入を防ぐ。

走っているチェーンが無ければそのまま呼ぶ。走っていれば呼ばず、
終わるのを待って 1 回だけ呼ぶ (待っている間に来た分は畳まれる)。"
    (let ((buffer (current-buffer)))
      (if (not (my:diff-hl-dired--in-flight-p))
          (my:diff-hl-dired--start orig)
        (unless my:diff-hl-dired--deadline
          (setq my:diff-hl-dired--deadline
                (+ (float-time) my:diff-hl-dired-max-wait)))
        (my:diff-hl-dired--rearm buffer orig))))

  (advice-add 'diff-hl-dired-update :around #'my:diff-hl-dired-update-guard)

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
