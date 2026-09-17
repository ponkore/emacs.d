<!-- -*- gfm -*- -->

# magit の高速化と自動更新 (`gitd/` + `my-gitd.el` + `my-magit-watch.el`)

遅いのは git ではなく Emacs のプロセス生成。常駐プロセスへの肩代わりとトークン方式のキャッシュで 1.7 秒 → 50～70 ms。あわせてワークツリーを監視して magit バッファを自動更新する。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## magit の高速化 (`gitd/` + `my-gitd.el`)

magit のリフレッシュが遅い原因は **git ではなく Emacs のプロセス生成コスト**。
`user-lisp/my-gitd.el` が `magit-process-file` に `:around` を張り、
Rust の常駐プロセス（`gitd/`）に git の実行を肩代わりさせる。

計画と実測は `docs/magit/magit-auto-refresh-plan.md` と `docs/magit/magit-gitd-2a-design.md`。

### 遅さの原因（2026-09 実測）

**同じ `cmd.exe` を起動するのに PowerShell が約 20 ms、Emacs の
`call-process` は 59〜76 ms**（3 回反復して再現を確認）。約 40 ms が
Emacs 側のプロセス生成経路のコストで、git にも Defender にも由来しない。

| | |
|---|---|
| `magit-refresh-buffer` 1 回 | 1669 ms / **git 呼び出し 29 回** |
| → 1 回あたり | 56〜58 ms |

**時間は呼び出し回数に完全に線形。** リポジトリの規模にほぼ依存しない固定コスト。

効かなかった対策（試して確認済み。もう一度試さないこと）:

- Defender 除外 — git 固有のコストではないので効かない
- `core.fsmonitor` — 走査時間は減るがプロセス生成コストは変わらない
- `cmd/git.exe` ラッパの回避 — **magit は既に回避済み**。
  `magit-git-executable` の defcustom が Windows では cygpath 経由で
  `mingw64/libexec/git-core/git.exe` を解決する。PowerShell では 47→39 ms と
  効くが、Emacs の 55 ms に埋もれて有意差なし
- `magit-status-sections-hook` の削減 — 16→6 で 1669→1001 ms。表示を
  犠牲にする割に効かない

### 効果

`magit-refresh-buffer` 1 回（`~/.emacs.d`、28 コマンド）:

| | 時間 | Emacs からの git 起動 | デーモンでの git 起動 |
|---|---|---|---|
| デーモン無効 | 1503 / 1544 ms | 28 | — |
| 段階 2a（素通し） | 683 / 672 ms | 0 | 28 |
| **段階 2b（キャッシュ）** | **56 / 54 ms** | 0 | **0** |
| 段階 2b（0.3 秒前に先読み） | 72 / 54 ms | 0 | 0 |
| 段階 2b（直前に先読み = `g`） | 208 ms | 0 | 28（並列） |

**1.7 秒が 50〜70 ms になった。** GUI の実地計測（外部でファイルを変更してから
自動更新が終わるまで）でも 51〜81 ms。

Rust の `Command` からの spawn は 28.9 ms（`git status -z --porcelain`）で
Emacs の約半分。stdio の往復は **0.13 ms**、28 回でも 4 ms なので、
残る 50 ms はほぼ magit 自身の Elisp（セクションの構築と描画）。
**ここから先はデーモンでは縮まない。**

### ビルド

`tree-sitter/` の文法と同じ扱い。**ソースは git 管理下、`gitd/target/` は
`.gitignore`** して各マシンで作る。

```
M-x my:gitd-build     ; cargo build --release
M-x my:gitd-stats     ; 経由回数 / フォールバック数 / 累計短縮時間
M-x my:gitd-restart   ; サーキットブレーカが落ちたときの復帰
(setq my:gitd-verify t)  ; シャドウモード (下記)
```

**バイナリが無ければ `my:gitd-mode` は何もしない**ので、まだビルドしていない
マシンでは自動的に従来動作になる。対象は Windows のみ。

### 文字コードの地雷（3 つとも実際に踏んだ）

`my:gitd--to-text` が処理する。**`args` / `cwd` / `program` / `env` の
全部に適用すること。**

1. **`process-environment` に JSON に載らない項目がある。**
   `PSModulePath` が OneDrive の「ドキュメント」を ANSI の生バイトのまま
   含んでおり `json-serialize` が `wrong-type-argument json-value-p` で落ちる。
   **PowerShell から Emacs を起動したときだけ再現する**（bash 経由では出ない）
2. **復号に `locale-coding-system` を使うと直らない。**
   あれは**コンソールの**コードページで、PowerShell 7 では `cp65001`（UTF-8）。
   環境変数ブロックは **ANSI コードページ**（`w32-ansi-code-page` = 932）で別物。
   UTF-8 として復号すると生バイトが eight-bit 文字のまま残り、やはり載らない
3. **引数も ANSI に encode されている。**
   `magit-process-git-arguments` が意図的にやっている（Emacs の `call-process`
   が ANSI API を使うため。magit issue #3250）。デーモン境界で復号し直す。
   Rust はワイド API で起動するので、cp932 に無い文字ではむしろ改善になる

### `magit-process-file` を横取りするときの注意

同期読み取りは全部この関数を通るので差し込みは 1 箇所で足りる。ただし:

- **`BUFFER` に整数（`0`）が来る。** `magit-run-gitk` が使う
  「非同期・出力破棄」の意味。同期実行すると **gitk のウィンドウを閉じるまで
  Emacs が固まる**。必ず弾くこと
- `magit-run-gitk*` は `magit-gitk-executable`、`magit-patch-id` は
  `shell-file-name` を渡してくる。`(equal program (magit-git-executable))` で弾く
- 実際に来る `BUFFER` は `nil` / `(t nil)` / `(t "FILE")` / バッファ の 4 形態。
  それ以外は素通し（default deny）
- デコードは `(car (magit--process-coding-system))`（実測で `utf-8-unix`）。
  **値を決め打ちせず必ずこの関数から取る**
- `magit-run-git-with-input` は `call-process-region` を使うので通らない。
  `magit-start-process`（非同期）も無関係

### 安全側の作り

- **タイムアウトを設けない。** 素の `process-file` にも無いので、挙動を
  変えないことが最も安全。`jsonrpc-request` は `:timeout nil` でタイマーが
  完全に無効になり、待ちは `accept-process-output` なので `C-g` で抜けられる
- **フォールバック。** バイナリが無い / デーモンが死んだ / 形態が未知なら
  黙って素の `process-file` に戻る。3 回続けて失敗したらそのセッションでは使わない
- **二重実行の防止。** デーモンが応答前に死ぬと git が既に走ったかは分からない。
  読み取り専用なら再実行してよいが、それ以外は再実行せずエラーを返す
  （`git add` を 2 回走らせない）

### 検証はシャドウモードで

`(setq my:gitd-verify t)` にすると、読み取り専用コマンドを**デーモン経由と
素の `process-file` の両方で実行してバイト単位で比較**する。差異は
`*gitd verify*` に記録される。この設計で唯一こわいのは「静かに壊れる」ことなので、
**壊れていないことを実使用で証明する**のがこの機能の役目。遅くなるので常用はしない。

## デーモン側のキャッシュ（段階 2b）

設計と実測は `docs/magit/magit-gitd-2b-design.md`。

### 無効化を「通知」ではなく「トークン」でやる

**古い答えを返すキャッシュは静かに壊れる。** magit が事実と違う内容を表示し、
ユーザはそれに気づけない。そこで無効化通知は**作らなかった**。

`git/run` には毎回 `repo`（監視中のリポジトリのルート）と `token`
（そのリポジトリ状態の通し番号）を載せる。デーモンは
`(repo, token, コマンド)` でキャッシュし、token が違えば問答無用でミスにする。

こうすると正しさの条件が「Emacs が変化を漏れなく通知すること」から
**「トークンが古い状態を指し続けないこと」**に変わる。前者は通知を 1 つ
落とすとそのリポジトリが**永久に**古いままになるが、後者は Emacs 側だけで
閉じており、`my-magit-watch` が既に持っている情報で満たせる。

トークンを進めるのは 3 か所（`my-magit-watch.el`）:

| いつ | 何のため |
|---|---|
| 分類を通ったイベント（`suspect` を除く） | 外部からの変更 |
| `magit-pre-refresh-hook` | magit 自身の書き込みと、ユーザの `g` |
| デーモン経由で書き込みコマンドが走ったとき | 上を待たずに進める |

2 番目が要なのは、`magit-run-git-with-input`（`call-process-region`）と
`magit-start-process`（非同期）が**デーモンを通らない**ため。magit は
コマンドの後に必ず `magit-refresh` を呼ぶのでここで捕まる。
**`g` が必ず本当のことを言う**のもこれで保証される。

**監視が動いていなければ `repo` も `token` も付かず、キャッシュも先読みも
行われない。** `M-x my:magit-watch-mode` で切れば段階 2a と同じ動作に戻る。
キャッシュの寿命が監視の寿命に従属しているのが最大の安全弁。

### 先読みは 2 本目のタイマーで頼む

`my-magit-watch` のタイマーは 2 本ある。どちらもイベントごとに張り直す。

```
       イベント群 ......|
                        |--0.1s--> repo/prewarm を送る
                        |------------0.4s------> magit-refresh-buffer
```

差の 0.3 秒が先読みの持ち時間。デーモンは**直前のリフレッシュで実際に来た
コマンド列を覚えていて**（magit の内部を知る必要がない）、それを 8 並列で
走らせる。0.1 秒待つのは、1 ファイルの保存で w32notify が約 10 件の
イベントを出すため（最初の 1 件で頼むと残り 9 件でトークンが進んで無駄になる）。

同じコマンドが二重に起動しないよう single-flight にしてあるので、先読みが
間に合わなくても損はしない。`g` を押した瞬間に頼んでも、magit の要求は
走っている先読みに**合流**するので、直列 28 回ではなく並列 1 回ぶんで済む。

### `update-index --refresh` は先読みの先頭で走らせる（prelude）

`magit-status-refresh-buffer` は**先頭で** `update-index --refresh` を呼ぶ。
これを飛ばして先読みすると、`diff-files` が「stat が古いだけ」のファイルを
変更ありと報告し、その答えがキャッシュに残る。実測:

```
内容を変えずに書き直したあと
  update-index --refresh 無し → diff-files が 3 ファイルを M と報告
  update-index --refresh 後   → diff-files は何も報告しない
```

`git diff`（磁器）は自分で内容を比較するので影響を受けないが、
magit の `magit-unstaged-files` は `diff-files` を使う。

そこで `role: "prelude"` を作り、Emacs が明示したコマンドだけを先読みの
先頭で直列に走らせる。対象は `update-index --refresh` **ただ 1 つ**。
デーモンは相変わらず git の意味を知らない。

### 【重要】読み取りだけの git もファイル変更イベントを出す

これを見落として、最初の GUI 検証で**自動リフレッシュが 1 回も走らなかった**
（先読み 63 回・リフレッシュ 0 回）。実測:

| コマンド | イベント | 内訳 |
|---|---|---|
| `update-index --refresh`（何もしない場合でも） | 3 | `.git` ×1 / `.git\index.lock` ×2 |
| `status --porcelain` | 4 | `.git` ×2 / `.git\index.lock` ×2 |
| `diff-files -z --name-only` | 1 | `.git` ×1 |
| `rev-parse` / `for-each-ref` | 0 | — |

`status` ですら index.lock を作る。だから先読み（28 コマンド）は必ず
イベントを出し、それがデバウンスを張り直し、0.1 秒後にまた先読みが走る。
**0.4 秒の静けさは永久に来ない。**

分類上これらはすべて `suspect` なので、そこを狙い撃ちして 3 つ直した。

1. **`suspect` ではトークンを進めない**（進めると自分のリフレッシュや
   先読みが自分のキャッシュを壊す）。代わりに `my:magit-watch--fire` が
   フィンガープリントの不一致を見つけたときに進める
2. **`suspect` では既に張ってあるタイマーを延長しない**
3. 1 つの窓では先読みを 1 回だけ頼む。さらに**デーモン側でも、レシピが
   全部キャッシュ済みなら先読みごと打ち切る**（無いと prelude だけが
   毎回走ってイベントを出し続ける）

### `magit-process-record-invocations` は素通しにする

magit の呼び出しログは `magit-process-file` の**本体**にあるので、
`:around` で `orig` を呼ばずに済ませると記録されない。有効なときは
ルーティングしないようにしてある。

同じ理由で **`magit-process-file` に後から足した advice も呼ばれない**。
テストを書くときは `my:gitd-mode` を有効にした**後**に足すこと。

### `default-directory` は必ず `expand-file-name` する

Emacs は file バッファの `default-directory` を `~/...` に略記することがある。
`call-process` は内部で展開するが **Rust の `current_dir` は `~` を展開しない**。
そのまま渡すと `ディレクトリ名が無効です (os error 267)` になる。
段階 2a から入っていたバグで、magit のバッファからしか呼ばれていなかったので
表に出ていなかった。

### 次の段階

**段階 2c で監視を常駐プロセスに移す**（今回は見送った）。速度の目標は
キャッシュと並列化だけで達成できており、`check-ignore` は段階 1 で
キャッシュ済みで定常状態では 0 回しか呼ばれないので、`ignore` crate に
置き換えても速くはならない。移す価値があるのは別の 2 点:

- macOS / Linux 対応（`subtree` 相当が inotify / kqueue に無い）
- `ReadDirectoryChangesW` のバッファ溢れを**検知**できるようになる。
  Win32 API は溢れを通知するが Emacs の `w32notify` はそれを渡さない。
  検知できればトークンを強制的に進められ、「イベントが落ちるとキャッシュが
  古いまま」という唯一の穴が塞がる

## magit の自動更新 (`my-magit-watch.el`)

ワークツリー / インデックス / HEAD の変化を検知して、表示中の magit バッファを
`magit-refresh-buffer` する。Windows のみ、**既定で有効**。
切るときは `M-x my:magit-watch-mode`、様子を見るときは `M-x my:magit-watch-stats`。

段階 2a でリフレッシュが 0.6 秒になったので実用に耐えるようになった。
**2a 無しではこれは入れられなかった**（1.7 秒の固まりが頻発する）。
段階 2b のキャッシュで 50〜70 ms になっている。

設計と実測は `docs/magit/magit-autorefresh-stage1-design.md`。
gitd のキャッシュにトークンを供給する役目も負っている（前節）。

### `w32notify-add-watch` を直接呼ぶこと

`subtree` フラグを渡すと **1 個の watch で配下を再帰的に監視できる**
（追加コスト 0.2 ms、watch 後に作ったディレクトリも届く）。

**`filenotify.el` の `file-notify-add-watch` は `subtree` を渡さない**
（`file-notify--add-watch-w32notify` が `file-name` / `directory-name` /
`size` / `last-write-time` しか組み立てない）ので、汎用 API 経由では非再帰。

### 【重要】batch では検証できない

**w32notify のイベントはコマンドループ経由で配送されるため、`--batch` では
1 件も届かない。** `accept-process-output` や `sit-for` を回しても駄目。
最初 batch で測って全部 0 件になった。

テストは GUI で書く。`emacs -Q -l probe.el` で結果をファイルに書いて
`kill-emacs` する形にしてある。

### 自励振動と二重リフレッシュ

`magit-refresh-buffer` を 1 回走らせるだけで **毎回きっちり 7 件**の
イベントが出る（`.git/index.lock` が 4 件、`.git` ディレクトリ自身が 3 件）。
素直に繋ぐと「イベント → リフレッシュ → イベント」で回り続ける。
さらに magit で stage すると `.git/index` が書かれるが、magit は自分で
リフレッシュ済みなので監視側がもう 1 回走る。

**時刻では区別できない。** イベントは遅れて届くので、magit 自身の書き込みか
外部の変更かを到着時刻から判断することはできない。**内容で見る。**

`.git/index` と `.git/HEAD` の `(mtime . size)` をフィンガープリントとし、
**`magit-refresh-buffer-hook` で毎回取り直す**。このフックは
**自分のリフレッシュでも magit 自身のリフレッシュでも走る**のが肝。

| | |
|---|---|
| magit の stage | git が index を書く → magit がリフレッシュ → そこでスナップショット → あとから届くイベントは必ず一致 → **抑止** |
| 外部の `git add` | スナップショットは前のまま → 一致しない → **リフレッシュ** |

`stat` を 2 回するだけで git は呼ばない。

### イベントは欠落する

1000 ファイル作成に対しイベントは **4095 件**しか届かなかった
（1 ファイル 10 件出るので 1 万件が期待値）。`ReadDirectoryChangesW` の
バッファ溢れで避けられない。**イベントの完全性に依存した設計にはできない。**
差分更新（「このファイルだけ再描画」）のような最適化はやらないこと。

対策として分類に `suspect` を設けた。`.git` ディレクトリ自身や
`.git/**/*.lock` は**それ自体は何も証明しないが「何かは起きた」合図**なので、
拾ってフィンガープリントで判断する。決め手のイベントが落ちても
粗い `.git` の mtime 更新は残りやすい。

### `.gitignore` の判定

監視は `.gitignore` を知らないので、`build/` に 200 ファイル作ると分類後でも
1001 件残る。パスだけのフィルタでは落とせないので git に聞くしかないが、
**イベントごとに聞いてはいけない**。3 段構えで濃縮している。

1. コールバックでは**変化したディレクトリ**をハッシュに入れるだけ
   （ビルドは数千ファイルを出すが**ディレクトリは数個**）
2. デバウンス後に、未知のディレクトリだけを `check-ignore` へ**まとめて 1 回**
3. 結果をキャッシュ。**定常状態では git を 1 回も呼ばない**

実測: `build/out` に 100 ファイルを 3 回書いて、リフレッシュ 0 回、
`check-ignore` は 1 回目だけ。合計 4126 イベントに対しリフレッシュは 7 回。

#### `check-ignore` の呼び方（2 回はまった）

**`magit-git-global-arguments` をそのまま使ってはいけない。**

| 書き方 | 何が起きるか |
|---|---|
| `check-ignore -z -- PATH` | `fatal: -z only makes sense with --stdin` |
| `--literal-pathspecs` 付き | `fatal: pathspec magic not supported by this command: 'literal'` |

どちらも `ignore-errors` で握り潰すと **「何も無視されない」= 安全側に倒れる**ため、
**動いているように見えて 1 件も効いていない**という形で表面化する。

```elisp
(let ((magit-git-global-arguments '("--no-pager" "-c" "core.quotePath=false")))
  (magit-process-git t (list "check-ignore" "--" paths)))
```

`core.quotePath=false` は日本語パスが C 形式でクォートされて突き合わせに
失敗するのを防ぐため。終了コードは 0（該当あり）/ 1（該当なし）/
128 以上（エラー）で、128 以上は 1 度だけ `message` で知らせる。

#### 【重要】ここで magit の関数を無条件に呼んではいけない（2026-09-17）

上のコードは **magit がロードされていることを前提にしていた**。段階 2 で
**dired を開いただけのリポジトリも監視対象になった**ため、magit を一度も
開いていないセッションでこの経路に入るようになり、こうなった。

```
Error running timer ‘my:magit-watch--fire-prewarm’: (void-function magit--with-temp-process-buffer)
Error running timer ‘my:magit-watch--fire’: (void-function magit--with-temp-process-buffer)
```

再現は「GUI 起動直後に `C-x C-v` で git 管理下のディレクトリを開く」だけ。
`dired-mode-hook` → `my:magit-watch-add-for-dired`（**git を起こさない**のが
売りだった入口）で watch が張られ、最初のワークツリー変化で
`my:magit-watch--stale-p` → `--worktree-relevant-p` → `--ignored-p` と降りてくる。

**バイトコンパイルしない方針なので、マクロの未定義はロード時に出ない。**
`magit--with-temp-process-buffer` はマクロなので、コンパイルしていれば
展開時に分かる。インタプリタでは**実際にその行が走るまで**展開されず、
`void-function` という**関数呼び出しに見える形**で出てくる。
`declare-function` も嘘をつく（宣言はするがロードはしない）。

タイマーの中で落ちるので `pending` が消えず、**そのセッションでは
ワークツリー変化での dired の VC マーク更新が一切走らない**。
`.git` のメタ変化（`meta`）は `--ignored-p` を通らないので動いてしまい、
「たまに効く」という分かりにくい形になる。

`my:magit-watch--check-ignore` に切り出して 2 経路にした。

| magit | 使うもの |
|---|---|
| ロード済み | `magit-process-git`（`my-gitd` の advice に載るので速い） |
| **未ロード** | `call-process`（`my:magit-watch--git-program`） |

`process-environment` の引き継ぎは `magit--with-temp-process-buffer` が
やっていたことをそのまま書く（呼び出し元でバッファローカルだと
`with-temp-buffer` では伝わらない）。両経路で同じ結果になることは実測した。

**段階 2 以降、イベント処理の経路には magit 依存を持ち込めない。**
`magit-refresh-buffer` のように magit バッファに対してしか呼ばないものは
別（そこに magit バッファがあるなら magit はロード済み）。

### 抑止条件に入れてよいもの・いけないもの

**ユーザが操作をやめれば自然に解消するものだけ**を入れる
（ミニバッファ・transient・isearch・キーボードマクロ・リージョン・
`input-pending-p`）。

**`frame-focus-state` を入れてはいけない。** フォーカスが外れている間は
永久に偽のままなので待ち直しが終わらず、**フォーカスを失った時点から
二度と更新されなくなる**（実測で 0.3 秒ごとに再アームし続けた）。
背景の CPU は `my:magit-watch-visible-only` とレート制限で抑える。

### `.lock` の除外は `.git/` 配下に限ること

`index.lock` を落とすために `.lock` で除外したくなるが、ワークツリーには
`Cargo.lock` や `flake.lock` といった**追跡対象のファイル**がある。

### テストを書くときの注意

このマシンは `init.defaultBranch = main`。テスト用リポジトリで
`git checkout master` は失敗する。`-q` で握り潰すと「イベントが来ない」と
誤診する（実際に 1 度誤診した）。`git init -b main` と明示すること。

