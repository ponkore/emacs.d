<!-- -*- gfm -*- -->

# dired の拡張

外部アプリ起動 (Excel)、exceldiff / MarkText、短くリネームできない問題、自動更新と diff-hl-dired の再入、中身の変化への追従 (my-dired-watch)、消えた行が残るレース。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## dired で外部アプリを起動する

Excel ブック（`.xls` / `.xlsx` / `.xlsm`）は Emacs で読んでも意味が無いので、
バッファに読み込まず OS のファイル関連付けに渡す。

| | |
|---|---|
| 判定 | `my:dired-external-open-regexp` / `my:dired-external-open-p`（`my-dired.el`） |
| 起動 | `my:open-file-externally`（`my-core.el`）。Windows は `w32-shell-execute`、macOS は `open(1)`、他は `xdg-open` |

拡張子を足したいときは `my:dired-external-open-regexp` に加える。dired と
サイドバーで同じ述語を共有しているので両方に効く。

dired 側は `RET` / `f` / `e` を差し替える（3 つとも同じ `dired-find-file`）。
`o`（other-window）と `v`（view）は素のままにしてある。

### サイドバーは `dired-find-file` を通らない

`dired-sidebar` の `RET` は `dired-sidebar-find-file` なので、`dired-mode-map`
の差し替えでは効かない。しかも入口が 3 つある。

| 入口 | コマンド |
|---|---|
| `RET` / `C-m` | `dired-sidebar-find-file` |
| `C-o` | `dired-sidebar-find-file-alt` → `call-interactively` で上を呼ぶ |
| `mouse-2` | `dired-sidebar-mouse-subtree-cycle-or-find-file` → DIR 引数付きで上を呼ぶ |

3 つまとめて押さえるため、キーではなく `dired-sidebar-find-file` への
`:around` advice にしてある。

**`orig` を呼ぶ前に判定すること。** `dired-sidebar-find-file` はファイルに
対して `get-mru-window` / `next-window` で表示先を選び、空いていなければ
`split-window` までする。外部に投げるだけのファイルでウィンドウ分割を
起こしてはいけない。

これは「別の開き方」ではなくウィンドウ管理のラッパで、サイドバーが
dedicated window であることに由来する。ディレクトリならサイドバーの中で
ルートを差し替え（`dired-sidebar-with-no-dedication` + `find-alternate-file`）、
ファイルなら隣のウィンドウを選んでから `find-file` する。

### 【重要】`dired-x` は `dired-mode-map` を無条件で書き換える

`dired-x.el` はロードされた瞬間に、トップレベルの裸の `define-key` で
`dired-mode-map` を書き換える。`defcustom` による切り替えは無い。

```elisp
(define-key dired-mode-map "F" 'dired-do-find-marked-files)
(define-key dired-mode-map "V" 'dired-do-run-mail)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "\C-x\M-o" 'dired-omit-mode)
```

`use-package dired` の `:bind` は dired のロード時に張られるので、あとから
`dired-x` が読まれると **`V` の `dired-vc-status` が奪われる**。

`dired-x` は明示的に require したつもりが無くても読まれる。入口は 2 つあり、
**どちらも `F8` を通る**。

- `dired-sidebar` の `:config` の `(require 'dired-x)`（`dired-omit-mode` のため）
- サイドバーの `a`（`dired-omit-mode` は `dired-x` で唯一の autoload）

つまり「**`F8` を一度でも押すと、そのセッションでは以後 `V` が効かなくなる**」
という壊れ方をしていた。2026-08-30 に neotree を dired-sidebar に置き換えた
ときからの回帰で、2026-09 に気づいた。

`:defer t` + `:config` の `use-package dired-x` で張り直している。
`eval-after-load` はファイルのロード完了後に走るので、`dired-x` 自身の
`define-key` に必ず勝つ。**`:bind` では駄目**で、`dired-x` のロードとは
無関係に張られてしまい上書きを取り返せない。

`dired-mode-map` に置いたキーが効かないときは、**まず `dired-x` を疑う**こと。
奪われるのは上の 5 つと `*(` / `*O` / `*.`。

## dired から exceldiff / MarkText を起動する（2026-09-06）

yazi に入れてある操作を dired からもできるようにしたもの。1 文字キーは
dired と dired-x が使い切っているので、`C-c w`（git 相対パス）と同じく
`C-c` 側に置く。yazi の `X` プレフィクスに合わせて exceldiff は `x` で束ねた。

| キー | hydra（`.`） | |
|---|---|---|
| `C-c x v` | `x` | point の Excel をコミット済みリビジョンと比較（`C-u` でリビジョン指定） |
| `C-c x d` | `X` | マークした 2 つの Excel を比較（`C-u` で A/B 入れ替え） |
| `C-c m` | `O` | point の markdown を MarkText で開く |

サイドバー（`F8`）のキーマップは `dired-mode-map` を親に持つのでそのまま効く。

### exceldiff は必ず非同期で起動する

[exceldiff](https://github.com/ponkore/exceldiff) は `-o` を省略すると差分
ブックを一時ファイルに書いて **Excel で開き、閉じられるまで戻らない**
（`cmd/common.go` の `diffFiles` → `viewer.OpenAndWait`）。`call-process` に
すると Excel を閉じるまで Emacs が固まる。yazi のプラグインが
`block = true` を避けているのと同じ理由で、`start-process` を使う。

- 出力バッファは実行ごとに作る。前の差分ブックを開いたままだと前のプロセスが
  生きているので、1 つのバッファを使い回せない。**成功したら sentinel が
  捨てる**ので溜まらない。失敗したときだけ `display-buffer` で見せる
- `set-process-query-on-exit-flag` は nil。差分ブックは既に Excel が握っていて
  exceldiff とは独立なので、Emacs 終了時に殺しても失われるのは `%TEMP%` の
  後始末だけ
- 対象は `.xlsx` / `.xlsm` のみ。excelize が旧形式を読めないため
  **`.xls` は入れない**（`my:dired-external-open-regexp` が `.xls` を含むのとは別物）
- git / svn の判別は `exceldiff vcs` 側がやるが、あちらは非同期でエラーが出力
  バッファ越しにしか見えないので、`locate-dominating-file` で先に弾く
  （git は起動しない。`my:dired-copy-git-relative-filename-as-kill` と同じ方針）
- 実行ファイルは `executable-find` で見つける。無ければ `user-error`

引数のエンコーディングは何も束縛しない。`default-process-coding-system` の
cdr が Windows では既に cp932 なので、`start-process` の引数もそのまま正しい
（`call-process` と同じ経路。CLAUDE.md の「`call-process` の引数は cp932 で
エンコードすること」）。GUI 実測で `見積書_①テスト.xlsx` を渡すと、
exceldiff のエラーメッセージに**同じ綴りで出てくる**ことを確認した。

### markdown は `markdown-open` と同じ経路を使う

`my:markdown-open-external`（`my-text.el`）に `&optional FILE` を足しただけで、
`C-c C-c o`（`markdown-open`）とまったく同じ関数を通る。`markdown-open` は
`markdown-open-command` を**引数無しで funcall** するので `&optional` で足りる。

FILE を渡した場合は `save-buffer` しない（呼び出し側がそのバッファを持っている
とは限らない）。代わりに dired 側で、そのファイルを開いてあるバッファに未保存の
変更があれば保存するか聞く。**外部エディタはディスク上の中身を読むので、
聞かないと古い内容が表示されるのに dired からは気づけない。**

## 【重要】dired で名前を短くリネームできない（2026-09-09 に対処）

`R`（`dired-do-rename`）で `2026-09-04-進捗報告.org` を
`2026-09-進捗報告.org` に縮めようとしても、**元の名前に戻される**。

`vertico-preselect` の既定は `directory` で、ファイル名の部分を打っている間は
先頭の候補が選択状態になる。`RET`（`vertico-directory-enter` →
`vertico-exit`）は**確定の前にその候補を挿入する**。縮めた入力は元の名前の
接頭辞なので候補は元の名前 1 件だけが残り、それが挿入されて「同じ名前への
リネーム」になる。

**エラーにならないので気づきにくい。** 同名のリネームは黙って通り、
dired が更新されてファイル名だけが変わらない（実測）。ディレクトリを
またぐときなど、経路によっては `file-already-exists` になる。

`M-RET`（`vertico-exit-input`）と `C-u RET` は元から通る。毎回押さずに
済むよう、`my-dired.el` が `dired-do-create-files` の読み取り中だけ `RET` を
差し替える（`my:dired-vertico-enter-or-input`）。**候補がディレクトリなら
従来どおり潜り、それ以外は入力をそのまま確定する。**

`vertico-preselect` を `prompt` にするだけでは駄目。GUI 実測
（`R` に続けてキーを送り、実際にできたファイルを見る）:

| 入力 / キー | 修正前 | `vertico-preselect` = prompt | 差し替え（現状） |
|---|---|---|---|
| `2026-09-進捗報告.org` / `RET` | **変わらない** | 縮まる | **縮まる** |
| `s` / `RET RET` | `sub/` へ移動 | **`s` という名前になる** | **`sub/` へ移動** |
| `su` / `TAB RET` | `sub/` へ移動 | — | `sub/` へ移動 |

- 差し込みは `dired-do-create-files` の 1 箇所でよい
  （`R` / `C` / `S` / `H` の 4 つとも通る）
- **`minibuffer-with-setup-hook` には `:append` で足すこと。** vertico は
  `minibuffer-setup-hook` で `vertico-map` を composed keymap の先頭に置くので、
  先に走らせるとこちらが後ろに回って `RET` を奪えない
- `vertico-preselect` は `prompt` にすると `TAB`（`vertico-insert`）も効かなく
  なる（`vertico--index` が -1 のとき何もしない）。差し替えなら候補は
  選択されたままなので `TAB` で採れる
- 検証は `execute-kbd-macro` で `R RET` を送る。ミニバッファの読み取りが要るので
  `ec.sh -n`（`inhibit-interaction` を外す）が要る。テスト用ディレクトリは
  **毎回ユニークな名前で作ること**。dired バッファを kill した直後は
  w32notify の watch が握っていて `Permission denied` で消せないことがある

## dired の自動更新（2026-09-04）

外部でファイルが増減したら dired の一覧も追随する。`dired-mode-hook` から
`auto-revert-mode` を**バッファローカルに**有効にしている
（`my:dired-auto-revert-setup`、`my-dired.el`）。

### 【重要】`global-auto-revert-non-file-buffers` は使わない

あれは `buffer-stale-function` を持つ非ファイルバッファを**一律に**対象に
するので、効き先が dired の外へ広がる。magit の更新は `my-magit-watch` と
`my-gitd` で自前に組んであり、そこに autorevert を並走させたくない。

実測では magit のバッファは `buffer-stale-function` が既定
（`buffer-stale--default-function`）のままで、`auto-revert--global-add-current-buffer`
は独自の stale 関数を要求する（`autorevert.el:561`）ため、あの変数を t に
しても magit は採用されない。**それでも範囲は広げない**（`buffer-menu` は
`auto-revert-interval` = 1 秒ごとに revert されるし、将来 magit 側が
`buffer-stale-function` を持てば黙って挙動が変わる）。

### ポーリングではない

dired 側は受け入れ準備が済んでいる（`dired.el:2906`）。

```elisp
(setq-local buffer-stale-function #'dired-buffer-stale-p)
(setq-local buffer-auto-revert-by-notification t)
```

`auto-revert-handler` は watch がある間、通知で `auto-revert-notify-modified-p`
が立たない限り `dired-buffer-stale-p` すら呼ばない（`autorevert.el:830`）。
`auto-revert-interval` が 1 でも毎秒 `ls` が走るわけではない。

### 拾えるもの・拾えないもの

**メインディレクトリの `created` / `renamed` / `deleted` だけ**が対象
（`autorevert.el:758`）。

| | |
|---|---|
| ファイルの追加・削除・改名 | **拾う** |
| ファイルの中身・サイズ・更新日時・属性の変化 | autorevert は拾わない。**`my-dired-watch` が別経路で拾う**（後述） |
| `i` で挿入したサブディレクトリの中の変化 | 拾わない |
| w32notify がバッファ溢れで落としたイベント | 拾えない |

保険として `dired-auto-revert-buffer` を `dired-directory-changed-p` にして
ある。これは auto-revert とは別物で、**既に開いてある dired バッファを訪ね
直したとき**に、変わっていれば revert する。通知に依存しない経路。

### 壊れないことの根拠

- `dired-revert` は**マーク・隠しサブディレクトリ・point とウィンドウ位置を
  復元する**（`dired.el:2232`）
- wdired 中は `buffer-read-only` が nil になり `dired-buffer-stale-p` が nil を
  返すので、編集中に潰されない
- `auto-revert-verbose` は dired だけ `setq-local` で nil にしてある。
  ファイルバッファ側のメッセージは残る
- `dired-sidebar` も dired バッファなので一緒に効くが、**あちらは
  auto-revert 前提の作り**になっている。`revert-buffer-function` をラップして
  窓の位置を保ち、`auto-revert-verbose` を自分で nil にし、
  `dired-sidebar-delay-auto-revert-updates`（既定 t）で 1.5 秒のアイドル待ちに
  間引く
- `diff-hl-dired-mode` が `dired-after-readin-hook` に載っている（`my-vc.el`）
  ので、自動更新のたびに vc 経由の git 呼び出しが増える。watch は非再帰で
  `.git/` の中の変化は届かないため、git の書き込みで更新が誘発される
  ループにはならない（この再入が別の問題を起こす。後述）

GUI プローブでの実測:

| | |
|---|---|
| 外部で作ったファイルが出る / 消したファイルが消える | **両方 t** |
| マークの維持 | **t**（`*` 1 個が残った） |
| magit バッファの `auto-revert-mode` / `auto-revert--global-mode` | **どちらも nil** |
| `magit-refresh-buffer` | 従来どおり成功 |

### 【重要】diff-hl-dired が再入する（2026-09-05 に対処）

自動更新を入れてから

```
Buffer " *diff-hl-dired* tmp status" has a running process; kill it? (yes or no)
```

が頻繁に出るようになった。出所は `process-kill-buffer-query-function`
（`subr.el`）で、`kill-buffer` した先に status が `run` のプロセスが
ぶら下がっていると聞いてくる。そのバッファを作って kill しているのは
diff-hl-dired だけ。

| | |
|---|---|
| `diff-hl-dired.el:101` | 前のチェーンが生きていれば `kill-process` して**一時バッファ 1 個を使い回す** |
| `diff-hl-dired.el:143` | チェーンが終わったら `kill-buffer` する |

vc-git の `dir-status-files` は `update-index` → `diff-index` →
`ls-files-missing` → … → `ls-files-ignored` と**プロセスを 6 回前後リレー**する
（`vc-git.el` の `vc-git-after-dir-status-stage`）。Windows は spawn 1 回が
55 ms 前後なので 1 チェーンで 0.5〜1 秒かかり、**auto-revert で
`dired-after-readin-hook` が再び走るには十分な長さ**になる。

再入したとき `kill-process` で前のチェーンを止められるのは「そのプロセスが
まだ生きている」ときだけ。**プロセスは終了済みで sentinel がまだ走っていない
瞬間**に再入すると `kill-process` は何もせず、旧チェーンが後から再開して
新チェーンと同じバッファで交錯する（タイマーと sentinel はどちらも
コマンドループの同じ場所で回るので、どちらが先かは保証されない）。
先に終わった側が `kill-buffer` を呼び、そこには相手のプロセスが走っている、
というのがあのプロンプト。**両チェーンが同じバッファを `erase-buffer` し合う
ので、プロンプトを別にしても dired のマーカーが欠けたり古いままになる。**

`my-vc.el` で 2 つ入れてある。

1. 一時バッファで `kill-buffer-query-functions` を nil にする。中身は
   読み取り専用の git なので、途中で殺して困るものは無い
2. `diff-hl-dired-update` に `:around`（`my:diff-hl-dired-update-guard`）。
   **走っているチェーンが無ければ従来どおり即実行**、走っていれば呼ばずに
   0.5 秒間隔で終了を待って 1 回だけ呼ぶ。待っている間に来た分は畳まれる。
   5 秒（`my:diff-hl-dired-max-wait`）で待ちを打ち切る保険付き
   （一時バッファが残ったときに更新が永久に止まらないように）

「走っているか」の判定は**一時バッファの生死**で足りる（チェーンの最後に
kill されるので、生きていること自体が印になる）。

batch プローブでの実測（`user-lisp/` を `dired-noselect` し、チェーンが
走っている最中に `diff-hl-dired-update` を呼ぶ）:

| | 対処後 | 対処前 |
|---|---|---|
| 一時バッファの `kill-buffer-query-functions` | **nil** | `(process-kill-buffer-query-function)` |
| 再入後にプロセスが同一か | **t**（新チェーンを始めない） | **nil**（殺して差し替え） |
| 待ちタイマー | t | nil |
| `run` のまま `kill-buffer` | **t**（黙って通る） | **`inhibited-interaction`** |

畳んだ更新が失われないことも確認した。開いた直後に 3 連続で呼ぶと
タイマー 1 本にまとまり、8 秒後にはチェーン完了・一時バッファ無し・
マーカーは変更済みの 2 ファイルに付いている。

**`inhibit-interaction` を立てずに測ってはいけない。** batch の `yes-or-no-p`
は stdin を待つので、対処前の `kill-buffer` でプローブが固まる
（最初の 1 回で実際に固まった。それはそれで「本当に聞いてくる」ことの
証明にはなる）。


---

## 中身の変化への追従（`my-dired-watch`、2026-09-15）

サイズ・更新日時・属性が変わったときにも行を最新にする。上の autorevert には
手を入れず、**`modified` のイベントだけを自分で拾う別経路**として足してある。

### 追従しなかった原因は「検知」ではない

**イベントは前から Emacs のプロセスの中まで届いていた。** 門が 2 つあり、
どちらも独立に閉じていた。

| | |
|---|---|
| 門 1 | `auto-revert-notify-handler` は `buffer-file-name` を持たないバッファでは `created` / `renamed` / `deleted` しか通さない（`autorevert.el:757-759`）。**`changed` はそこに書かれてすらいない**。watch のフラグも `(if buffer-file-name '(change attribute-change) '(change))` なので、dired では属性を OS に問い合わせてすらいない（`autorevert.el:678-681`） |
| 門 2 | `dired-buffer-stale-p` は `dired-directory-changed-p`、つまり**ディレクトリ自身の mtime** しか見ない（`dired.el:1324-1348`）。中のファイルに追記しても親の mtime は 1 ビットも動かない |

GUI プローブでの実測（200 ファイルのディレクトリで `f005.txt` に追記）:

```
:events                    ((changed "f005.txt"))   ← 届いている
:dir-mtime-before/after    完全に同一
:dired-buffer-stale-p      nil
:size-in-buffer            "... masao 1 ... f005.txt"   ← 実体は 21 バイト
```

門 1 も単独で確認した。`auto-revert-notify-modified-p` を nil に戻してから
もう一度追記しても、autorevert の watch は生きている（descriptor 非 nil）のに
フラグは**立たない**。

### 【重要】全体 revert ではなく 1 行だけ貼り替える

サイズ・日時・属性が変わっても**行は増減しない**。`dired-relist-entry` で
その行だけ貼り替える。実測（`C:/Windows/System32/` = 4862 エントリ）:

| | `dired-after-readin-hook` あり | 同 nil |
|---|---|---|
| `revert-buffer`（全体） | 648 ms | 237 ms |
| `dired-relist-entry`（1 行） | 454 ms | **0.71 ms** |

支配的なのは `nerd-icons-dired--refresh` で、**1 行しか変えていなくても
バッファ全体を舐め直す**。アイコンはファイル名と種別だけで決まりサイズや
日時では変わらないので、貼り替えの間だけフックを外す。

マークと point が保たれ、スクロールが飛ばないのも全体 revert との違い
（実測で別行の `*` と point の位置が残ることを確認）。

### 【重要】アイコンは overlay なので自分で付け直す

`nerd-icons-dired` のアイコンは行の上に張った overlay（`evaporate` が t、
`nerd-icons-dired.el:71-80`）。`dired-relist-entry` は行を `delete-region`
するので、**アイコンだけが消える**。フックを外している以上、付け直すのは
`my:dired-watch--annotate` の仕事になる。あちらの内部関数
（`nerd-icons-dired--add-overlay`）に触るのは承知の上で、代わりにフックを
走らせると 453 ms 払うことになる。

### 【重要】サブディレクトリの行は追従しない（仕様）

`sub/` の中にファイルを作ると `sub` 自身の mtime は変わるが、**親の watch には
イベントが 1 件も来ない**。実測:

| 操作 | 親の watch に届いたイベント |
|---|---|
| `sub/` の中にファイルを作る | **0 件**（行は古いまま） |
| 直下のファイルを書き換える | `(modified "f07.txt")` が 2 件 |

非再帰の `ReadDirectoryChangesW` が配下の変化を親に報告しないため。拾うには
`subtree` を足すしかなく、それはビルド出力のディレクトリを開いていると毎秒
数千件を呼び込む。得るもの（ディレクトリ行の mtime）に対して代償が大きすぎる。
`g` を押せば直る。

### magit-watch より簡単になった点

`my-magit-watch.el` で要った仕掛けは、どれも要らない。

| magit-watch | dired |
|---|---|
| 自励振動対策（refresh が 7 件のイベントを出す） | **要らない。** dired の更新はファイルを書かない |
| `.gitignore` の判定 | **要らない。** dired は無視しない。見えているものが変わったなら反映するのが正しい |
| フィンガープリント | **要らない。** イベントがファイル名を持っている |
| gitd のトークン | 無関係 |

### 全体 revert に倒す条件

- `-t`（時刻順）/ `-S`（サイズ順）で並べているとき。**`dired-add-entry` は
  行を元の位置に戻すだけで並べ直さない**ので、1 行の貼り替えでは嘘になる。
  判定の `dired-check-switches` には長い名前として `sort=time` を渡すこと。
  `time` だと `--time-style=...` にも当たる（`-` が単語境界になるため）
- 1 つの窓で `my:dired-watch-relist-limit`（既定 32）を超えたとき。多数の行が
  変わったなら、貼り替えを繰り返すより 1 回読み直すほうが安い

### 常駐プロセス（gitd 方式）にしなかった理由

コストは**全部 Emacs の中**にある。648 ms のうちデーモンが肩代わりできるのは
`directory-files-and-attributes` の 98 ms だけで、残りは ls-lisp の整形と挿入と
アイコン。検知そのものも既に Emacs まで届いている。**CLAUDE.md §2 の
「段階 2c（監視を常駐プロセスへ）は見送り」を覆す材料は出なかった。**

### 実装上の注意

- **wdired 中は絶対に触らない。** `dired-relist-entry` は自分で
  `buffer-read-only` を nil に束縛してしまうので、**呼ぶ前にこちらで
  `buffer-read-only` を見るしかない**（`dired-buffer-stale-p` と同じ判定）
- **行が無ければ何もしない。** `dired-relist-entry` は行が無ければ
  `dired-add-entry` で作ってしまうが、それは新規ファイルの追加であって
  autorevert の担当。しかもフックを外しているのでアイコンが付かない
- **`file-exists-p` を先に見る。** イベントとタイマーの間に消されたファイルで、
  行を消したあと `insert-directory` が失敗する
- `frame-focus-state` を抑止条件に入れない（CLAUDE.md。フォーカスを失った
  時点から二度と更新されなくなる）

---

## 【重要】消えたファイルの行が永久に残る（`dired-readin` のレース、2026-09-15 に対処）

別端末で `touch a.txt` → `vim a.txt` で 2 回書いたら、dired に **`a.txt~` と
`a.txt` の 2 行**が出た。`ls -l` には `a.txt` しか無い。

`a.txt~` は vim のバックアップ（`writebackup`、既定でオン）。2 回目の `:w` で
旧版を `a.txt~` に退避 → 新しい `a.txt` を書く → `a.txt~` を消す、という順に
動くので**数ミリ秒だけ存在して消える**。消えているのに行だけが残っていた。

### 原因

`dired-readin`（`dired.el:1568`）は一覧を読み終えた**後**にディレクトリの
mtime を取って記録する。

```elisp
(erase-buffer)
(dired-readin-insert)                       ; ← ここで一覧を読む
...
(let ((attributes (file-attributes dirname)))
  (set-visited-file-modtime (file-attribute-modification-time attributes)))
```

**この 2 つの間にファイルが消えると、バッファには古い一覧が入ったまま
「消えた後」の mtime が記録される。** `dired-buffer-stale-p` は
`dired-directory-changed-p`（= この mtime の比較）しか見ないので、
**そのバッファは以後永久に「変わっていない」と判定される**。`g` を押すまで
行が残り続ける。

vim で踏みやすいのは、3 段の書き込みと dired の一覧読み込みが重なるため。
`a.txt~` の作成が通知で auto-revert を起こし、その読み込みの最中に vim が
`a.txt~` を消す。

### 決定的な再現

`dired-readin-insert` の直後（= `set-visited-file-modtime` の直前）に
ファイルを消す advice を当てて `revert-buffer` する。

```
:line-remains              t      ← 行は残る
:file-exists               nil    ← 実体は無い
:dired-directory-changed-p nil    ← 変わっていないことになる
:recorded == :actual              ← 記録した mtime が最新
```

### 対処

`my:dired-readin-modtime-fix`（`my-dired.el`）。`dired-readin` に `:around` で、
記録する mtime を**一覧を読む前**の値に差し替える。読んでいる間に変化が
あれば「まだ古い」側に倒れるので、**余分な revert が 1 回走るだけで
取りこぼしが無くなる**。

| 同じ再現プローブ | 対処前 | 対処後 |
|---|---|---|
| `dired-directory-changed-p` | nil | **t** |
| `dired-buffer-stale-p` | nil | **t** |
| そのまま置いたときの行 | **残り続ける** | **autorevert が消す**（実測） |

### この件と `my-dired-watch` は無関係

`my-dired-watch` は行の貼り替えしかせず `visited-file-modtime` に触れない
（`dired-add-entry` は `set-visited-file-modtime` を呼ばない。`dired.el` で
呼んでいるのは `dired-readin` の 1 箇所だけ）。**以前からある Emacs 側の
レース**で、たまたま今回見つかった。

### 副産物: ディレクトリの mtime は遅れて更新される

調べる過程で、連続して測ると作成が mtime に反映されないことがあった。
0.4 秒空けると作成も削除も必ず反映される（`w32-get-true-file-attributes` の
値では変わらない）。

| | 作成 | 中身の変更 | 削除 |
|---|---|---|---|
| 0.4 秒空けて測る | **反映される** | 反映されない（当然） | **反映される** |
| 連続して測る | **落ちることがある** | — | 反映される |

**「ディレクトリの mtime を見れば増減が分かる」は、間隔を空けて測ったときの
話。** 通知の直後に測ると取りこぼす。
