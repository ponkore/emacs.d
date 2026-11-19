;;; my-platform.el --- OS 固有設定 (Windows / macOS)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; Windows環境固有
;;; --------------------------------------------------

;;; [3] 環境設定

(when (eq system-type 'windows-nt)
  ;; (setenv "HOME" (getenv "USERPROFILE")) は削除した (計画書の J-2)。
  ;;
  ;; 狙いは、HOME が未設定のときに Emacs が HOME を %APPDATA% にしてしまうのを
  ;; 避けることだったと思われる。emacs -Q で実測すると:
  ;;   HOME 未設定 -> HOME=C:\Users\masao\AppData\Roaming
  ;;                  ~ -> c:/Users/masao/AppData/Roaming
  ;;   HOME 設定済 -> ~ -> c:/Users/masao
  ;;
  ;; しかしこの行では狙いを達成できない。init.el が読まれている時点で .emacs.d
  ;; の探索は終わっている。HOME が %APPDATA% を指す環境なら Emacs は
  ;; %APPDATA%\.emacs.d を見に行くので、この設定自体が読み込まれない。
  ;;
  ;; そのうえ有害になりうる。user-emacs-directory は展開前の "~/.emacs.d/" と
  ;; いう文字列のままで、Windows の Emacs は expand-file-name のたびに HOME を
  ;; 読み直す。つまり途中で HOME を差し替えると、init.el を読み込んだ場所と
  ;; user-emacs-directory の指す先がずれ、recentf / custom.el / straight の
  ;; 保存先が実際に動いている設定とは別のディレクトリになる。
  ;;
  ;; Windows で ~ を C:\Users\<user> にしたい場合は、設定側ではなく OS の
  ;; ユーザー環境変数 HOME を設定する (このマシンでは設定済み)。
  (setq w32-get-true-file-attributes nil)

  ;; ~/Projects は C:\Projects へのジャンクション。これを入れておくと
  ;; c:/Projects/... の側から入ってきたファイルも ~/Projects/... として
  ;; 記録され、bookmarks を mac / Linux と共有できる。
  ;;
  ;; 【一方向にしか効かない】abbreviate-file-name (短くする側) だけが
  ;; この変数を見る。expand-file-name は見ない。**戻せる省略形は ~ だけ**
  ;; なので、読む側はジャンクション / symlink が担う。片方だけでは成立しない。
  ;;
  ;; TO に ~ を書いてはいけない (files.el:59) が、~ を含む結果は得られる。
  ;; abbreviate-file-name は「alist を適用してから ~ 置換」の順
  ;; (files.el:2316 -> 2329) なので、TO には home 配下の絶対パスを書けばよい。
  ;;
  ;; 【重要】FROM の末尾のスラッシュを省かないこと。directory-abbrev-apply
  ;; (files.el:87) は FROM を素の正規表現として使い、マッチ部分を TO で
  ;; 置き換えるだけなので、"\\`c:/Projects" と書くと
  ;; c:/ProjectsOld/foo/ が ~/ProjectsOld/foo/ になる (実測)。**存在しない
  ;; パスができるのにエラーは出ない。** abbreviated-home-dir のほうは
  ;; directory-abbrev-make-regexp (files.el:71) が \(/\|\'\) の境界を付けて
  ;; くれるが、手書きのエントリには付かない。
  ;;
  ;; 大文字のドライブレターは FROM が小文字でも当たる。abbreviate-file-name
  ;; が case-fold-search を (file-name-case-insensitive-p filename) に束縛
  ;; しており (files.el:2313)、Windows では t になるため。
  (add-to-list 'directory-abbrev-alist
               (cons "\\`c:/Projects/" (expand-file-name "Projects/" "~")))

  ;; `find-program' を GNU find に固定する。
  ;;
  ;; 既定値は "find" (grep.el:559 の defvar。loaddefs 経由で起動時から
  ;; bound なので、ここでの setq だけで足りる)。これをシェル経由で呼ぶのが
  ;; `project--files-in-directory' (project.el:398) と find-dired / grep-find。
  ;; ところが Windows の PATH は C:\WINDOWS\system32 が前にあるため、
  ;; **System32 の FIND.EXE** (GNU find とは無関係な文字列検索コマンド) に
  ;; 解決される。GNU find の引数を渡すと
  ;;   FIND: パラメーターの書式が違います     (exit 2)
  ;; になり、project.el:434 が `error "File listing failed: ..."' を投げる。
  ;;
  ;; 実測 (2026-11-19、このマシン):
  ;;   (executable-find "find") -> "c:/WINDOWS/system32/find.exe"
  ;;   PATH の並びは system32 が 2 番目、C:\Program Files\Git\usr\bin が 37 番目
  ;;   System32 FIND.EXE に "-H . -type f -print0" -> exit 2 / 上記メッセージ
  ;;   Git の GNU find に同じ引数                 -> exit 0 / 正しく列挙
  ;;
  ;; 表に出たのは eglot 経由だった。VC でもなく projectile のマーカーも無い
  ;; ディレクトリで php を開くと `eglot--current-project' (eglot.el:1654) が
  ;; `(transient . <dir>)' にフォールバックする。transient には
  ;; `project-files' の専用メソッドが無いので汎用メソッド
  ;; = `project--files-in-directory' が走り、`eglot--watch-globs'
  ;; (eglot.el:4860) の中から上のエラーが飛ぶ。eglot の非同期ディスパッチの
  ;; 中なので `Error running timer: (error "File listing failed: FIND: ...")'
  ;; という形で *Messages* に繰り返し出ていた (実測 138 件)。
  ;;
  ;; なお projectile は `project-files' を自前で持っている
  ;; (projectile.el:17690) ので、.projectile / .git があるディレクトリなら
  ;; この経路に落ちない。**マーカーの無いディレクトリを開いた瞬間だけ**
  ;; 踏むため、原因が分かりにくかった。
  ;;
  ;; 【PATH の並べ替えでは直さない】System32 には find 以外にも sort / more /
  ;; timeout など GNU 版と名前が衝突するものがあり、Git\usr\bin を前に出すと
  ;; 影響範囲が読めない。`find-program' だけを明示する。
  ;;
  ;; git の場所から辿るのを第 1 候補にする (Git for Windows は cmd/git.exe と
  ;; usr/bin/find.exe が同じツリーに入っている)。git が scoop の shim だと
  ;; 相対では辿れないので、既知のパスも候補に並べる。
  ;;
  ;; 【重要】スペースを含むパスをそのまま入れてはいけない。
  ;; `project--files-in-directory' は (format "%s -H . ..." find-program) と
  ;; **クォートせずに**コマンド文字列を組み立て、それを `shell-file-name'
  ;; (このマシンでは Git の bash) に渡す。実測:
  ;;   find-program = "c:/Program Files/Git/usr/bin/find.exe"
  ;;   -> File listing failed: /usr/bin/bash: line 1: c:/Program: No such file or directory
  ;; `shell-quote-argument' を通す手もあるが、`eglot--watch-globs' の
  ;; subdirs-using-find (eglot.el:4863) は `call-process' に直に渡すので、
  ;; クォート済みの文字列では今度はそちらが壊れる。**シェル経由と
  ;; call-process の両方で使える形は 8.3 短縮名** (c:/PROGRA~1/...) だけ。
  ;; `w32-short-file-name' で実行時に変換する (実測で前方スラッシュで返る)。
  (let* ((git (executable-find "git"))
         (from-git (and git (expand-file-name
                             "../usr/bin/find.exe" (file-name-directory git))))
         (gnu (seq-find #'file-executable-p
                        (delq nil (list from-git
                                        "C:/Program Files/Git/usr/bin/find.exe"
                                        "C:/Program Files/Git/mingw64/bin/find.exe"
                                        "C:/msys64/usr/bin/find.exe"))))
         ;; 8dot3name が無効化されたボリュームでは長い名前がそのまま返る。
         (short (and gnu (w32-short-file-name gnu))))
    ;; 直らないまま既定値で進むと project.el が error を投げるだけで原因が
    ;; 分からない。どちらの失敗も警告で見えるようにする。
    (cond
     ((null gnu)
      (display-warning
       'my-platform
       "GNU find が見つからないため find-program を既定値のままにした。\
project-files / find-dired が System32 の FIND.EXE を呼んで失敗する"
       :warning))
     ((or (null short) (string-match-p " " short))
      (display-warning
       'my-platform
       (format "GNU find (%s) の 8.3 短縮名が取れないため find-program を\
既定値のままにした。ボリュームの 8dot3name が無効になっている可能性がある" gnu)
       :warning))
     (t
      (setq find-program short)))))

;;; [3] w32-symlinks (削除済み)

;; site-lisp/w32-symlinks.el (874 行, 2002〜2005 年, EmacsWiki 由来) と
;; その設定ブロックを削除した。Windows の .lnk を magic file name handler で
;; シンボリックリンクのように扱うもので、ヘッダには「NTEmacs 21 と一緒に
;; 使うことを意図している」と書かれていた。
;;
;; 削除の理由:
;;   - :if が 'windoows-nt というタイポで 6 年間一度も実行されていなかった
;;   - 前提が失われている。当時「Emacs に無い」とされていた dired-do-symlink は
;;     Emacs 31 に組み込みで存在し、Windows にも本物の NTFS symlink がある
;;   - 実測すると 2005 年のパーサは現代の .lnk を取りこぼす。
;;     実在の 8 件で試して 1 件 (Brother Utilities.lnk) が
;;     c:/Users/Program Files (x86)/... という存在しないパスを返した
;;   - 動かすには insert-file-contents-literally へのグローバル advice が必須
;;     だった。一時バッファが multibyte だと署名判定に失敗するためだが、
;;     この advice は呼ばれるたびにカレントバッファを unibyte にしてしまう
;;   - minibuffer-complete へのグローバル advice も持っており、vertico を
;;     使っている現在では冗長かつ干渉の恐れがある
;;   - このマシンの .lnk は Downloads/old-desktop に 23 件あるだけで、
;;     21 件が exe、2 件がフォルダ。Emacs から辿りたいものではない

;;; [3] cygwin (削除済み)

;; cygwin ブロックは完全に死んでいたので削除した:
;;   - CYGWIN_DIR が未設定で、cygwin-mount-cygwin-bin-directory は
;;     バックスラッシュ + bin という壊れた値になっていた
;;   - cygwin-mount パッケージは導入されていない (関数も未定義)
;;   - (require 'setup-cygwin) と load はコメントアウト済みで、
;;     setup-cygwin というファイル自体が存在しない
;;   - Cygwin 自体が入っていない (シェルは Git 付属の bash)
;;
;; 唯一生きていたのが (file-name-shadow-mode -1) で、これが
;; my-completion.el の vertico-directory 側の (file-name-shadow-mode +1) を
;; 打ち消していた (my-platform は init.el の最後に読まれるため後勝ち)。
;; 削除により、ミニバッファでファイル名の重複部分が隠れるようになる。

;;; --------------------------------------------------
;;; Mac環境固有
;;; --------------------------------------------------

;;; [3] modifier

;; leaf では *modifier / *modifier-macos の二重の疑似パッケージだったが、
;; 外側は :config を持つだけで何もしていなかったので 1 つにまとめた。

;; どちらも macOS の Emacs が C 側で定義する変数で、他のプラットフォームには
;; 存在しない。値を持たない defvar は「この変数は他所で定義されている」という
;; 宣言だけを行い、束縛は作らない。これが無いと Windows / Linux での
;; バイトコンパイルで "assignment to free variable" になる (計画書の K-1)。
(defvar mac-option-modifier)
(defvar mac-command-modifier)

(use-package emacs
  :if (eq system-type 'darwin)
  :config
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(provide 'my-platform)
;;; my-platform.el ends here
