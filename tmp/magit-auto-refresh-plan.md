# magit 操作性改善 — 自動リフレッシュ + 常駐 git プロセス

作成: 2026-09-02 / 状態: **検討中（要件整理・基本設計・段階 0 の計測まで完了）**

対象: Windows のみ（macOS / Linux は従来動作にフォールバック）
想定リポジトリ規模: 中規模（1〜3 万ファイル）

---

## 1. 動機（何が不満か）

- `M-x magit` の初回表示が遅い。これは我慢できる。
- **ワークスペース側の変更が magit バッファに自動で反映されない。**
  `g` を押すまで古い状態のまま。
- **その `g`（リフレッシュ）自体のレスポンスが悪い。** 押してから体感で 1〜2 秒止まる。

理想は「ワークスペースの状態を常時監視し、変化があれば magit バッファが勝手に最新化される」。
そのために Rust 等で書いた高速な常駐プロセスと Emacs が stdio で会話する、というイメージ。

---

## 2. 計測（2026-09-02、このマシン / `~/.emacs.d` リポジトリ）

環境: Windows 11 Pro / Emacs 31.1 / git 2.55.0.windows.5 / magit v4.7.0-20-g137f137d
Windows Defender リアルタイム保護: 有効（除外設定は管理者権限が無く確認できず）

### 2.1 結論から

**遅さの原因は git ではなく、Emacs の Windows におけるプロセス生成そのもの。**
そして **常駐プロセスへの stdio 往復は 0.36 ms** で、プロセス生成の **約 1/150**。

| 起動元 | 対象 | 1 回あたり |
|---|---|---|
| Emacs → 常駐プロセスへ stdio 往復 | `git cat-file --batch-check` に 1 行投げて 1 行受ける | **0.36 ms** |
| PowerShell | `cmd.exe /c exit`（最小の exe） | 約 20 ms |
| PowerShell | `git.exe status --porcelain`（`mingw64\bin` 実体） | 38.8 ms |
| PowerShell | `git.exe status --porcelain`（`cmd\git.exe` ラッパ） | 47.4 ms |
| PowerShell | `git.exe status --porcelain`（`bin\git.exe`） | 121.0 ms |
| **Emacs `call-process`** | **`cmd.exe /c exit`（最小の exe）** | **59〜76 ms** |
| Emacs `call-process` | `git.exe --version`（実体） | 61 ms |
| Emacs `call-process` | `git.exe status --porcelain` | 55〜89 ms |

注目すべきは **同じ `cmd.exe` を起動するのに PowerShell が 20 ms、Emacs が 60〜76 ms** という点
（3 回反復して再現を確認）。**約 40 ms は Emacs 側のプロセス生成経路のコスト**であり、
git にも Defender にも由来しない。

**→ したがって以下はどれも効かない:**

- Defender の除外設定（そもそも git 固有のコストではない）
- `core.fsmonitor = true`（git の走査時間を減らすが、生成コストは変わらない）
- `cmd\git.exe` ラッパを避けて `mingw64\bin\git.exe` を直接指定する
  （PowerShell では 47→39 ms と効くが、Emacs の 55 ms の中に埋もれて有意差なし。2.3 で確認）

**→ 効くのは「プロセスを起動しないこと」だけ。** 常駐プロセス方式は正しい方向。

### 2.2 magit の refresh の内訳

`magit-process-file` に advice を張って計測（`~/.emacs.d` は小さくクリーンなリポジトリ）:

| 構成 | 時間 | git 呼び出し | 1 回あたり |
|---|---|---|---|
| 既定（16 セクション） | **1669 ms** | **29 回** | 58 ms |
| セクションを 6 個まで削る | 1001 ms | 18 回 | 56 ms |

**リフレッシュ時間 ≒ git 呼び出し回数 × 56 ms。完全に線形。**
セクションを削れば呼び出し回数は減るが、単価は変わらないので焼け石に水
（1.67 秒 → 1.00 秒。しかも表示内容を犠牲にして）。

呼び出し回数はリポジトリ規模にほぼ依存しない **固定コスト**である点も重要。
1〜3 万ファイルのリポジトリでは、これに加えて `git status` の走査時間が乗る。

### 2.3 セクション別コスト（`magit-refresh-verbose`）

```
  magit-insert-status-headers                        0.603 s
    ├ magit-insert-diff-filter-header      0.098
    ├ magit-insert-head-branch-header      0.098
    ├ magit-insert-upstream-branch-header  0.106
    ├ magit-insert-push-branch-header      0.051
    └ magit-insert-tags-header             0.110
  magit-insert-unpushed-to-upstream-or-recent        0.566 s   ← 単独で最大
  magit-insert-untracked-files                       0.196 s
  magit-insert-staged-changes                        0.107 s
  magit-insert-unstaged-changes                      0.052 s
  magit-insert-merge-log                             0.051 s
  magit-insert-unpulled-from-upstream                0.051 s
  magit-insert-stashes                               0.050 s
  （残り 9 セクションは合計 0.001 s 未満 — git を起動しないため）
  ────────────────────────────────────────────────────────────
  合計                                               1.731 s
```

コストは「そのセクションが git を何回起動するか」でほぼ決まっている。

### 2.4 呼び出しの中身

refresh 1 回で走る 29 コマンド（抜粋）:

```
rev-parse --show-toplevel        rev-parse --git-dir        rev-parse --is-bare-repository
update-index --refresh           rev-parse --verify HEAD    config --list -z
log --no-walk --format=%h %s     symbolic-ref --short HEAD  describe --long --tags
rev-parse --verify --abbrev-ref master@{upstream}           describe --contains HEAD
config --local -z --get-all --include status.showUntrackedFiles
status -z --porcelain --untracked-files=normal
diff --ita-visible-in-index --no-ext-diff ...  (unstaged)
diff --ita-visible-in-index --cached ...       (staged)
rev-parse --verify refs/stash    merge-base --is-ancestor HEAD origin/master
log --format=... -n10            log --format=... -n256 ..@{upstream}
rev-parse --short HEAD / HEAD~   rev-parse --verify refs/tags/...   (×3)
symbolic-ref refs/remotes/origin/HEAD
```

半分以上は **リポジトリ構成のメタ情報**（toplevel / git-dir / config / upstream / tags）で、
ワークツリーが変わっても結果は変わらない。キャッシュが非常に効きやすい性質。

### 2.5 既存の magit のキャッシュは足りているか

`magit--with-refresh-cache` は既にある。ただし `magit-refresh` が
`magit--refresh-cache` を **let 束縛する間だけ**有効で、リフレッシュを跨いで生き残らない。
上の 29 回は「重複を除いた後」の数字なので、単純な重複排除では減らせない。

### 2.6 ファイル監視は Emacs 単体でどこまでできるか

- Windows のバックエンドは `w32notify`。**`w32notify-add-watch` は `subtree` フラグを
  持っており、1 個の watch で配下を再帰的に監視できる。**
- ただし **`filenotify.el` の `file-notify-add-watch` は `subtree` を渡していない**
  （`file-notify--add-watch-w32notify` が `file-name` / `directory-name` / `size` /
  `last-write-time` しか組み立てない）。汎用 API 経由では非再帰になる。
- したがって Windows では `w32notify-add-watch` を **直接呼べば、外部プロセス無しで
  リポジトリ全体を 1 watch で再帰監視できる。**

**→ 「勝手に更新される」だけなら Rust プロセスは要らない。**
外部プロセスが要るのは高速化のほう。ただし 1〜3 万ファイルの再帰監視を
Emacs のメインスレッドのコールバックで捌くのは負荷が読めないので、
最終的には監視も常駐プロセスに寄せるのが素直（4.4 参照）。

---

## 3. 要件

### 3.1 必須 (MUST)

1. ワークツリー / インデックス / HEAD の変化を検知し、表示中の magit バッファを自動更新する。
2. 更新は **ユーザの操作を妨げない**。入力中・transient 表示中・選択中に画面が飛ばない。
3. リフレッシュの体感を現在の 1.7 秒から **200 ms 以下**にする。
4. **いつでも従来動作にフォールバックできる**。常駐プロセスが死んでも、居なくても、
   magit が普通に動く。壊れたら黙って素の `process-file` に戻る。
5. magit の出力解釈を壊さない。magit は git の出力を厳密にパースするので、
   返す内容は **本物の git の出力とバイト単位で同一**であること。

### 3.2 望ましい (SHOULD)

6. magit 側の書き込み操作（stage / unstage / discard / delete / mv / commit …）も
   常駐プロセス経由にして、操作直後のキャッシュ整合性を保つ。
7. 複数リポジトリを同時に扱える。
8. 監視は `.gitignore` を尊重する（build 生成物でイベントを溢れさせない）。

### 3.3 対象外 (WON'T)

- magit の UI・キーバインドの変更。
- git そのものの再実装。**出力互換の再現コストが破滅的に高い**（後述 6.1）。
- TRAMP 越しのリモートリポジトリ。素通しにする。
- macOS / Linux での最適化。フォールバック経路で従来どおり動けばよい。

---

## 4. 基本設計

### 4.1 全体像 — 3 つの層に分ける

問題が 2 つ（「更新されない」と「更新が遅い」）あり、**それぞれ独立に解ける**。
混ぜると設計が膨らむので分離する。

```
┌─ 層 A: 自動リフレッシュ ──────────────────────────┐
│  ファイル監視 → デバウンス → magit-refresh         │  監視は常駐側に置く
│  (Windows: ReadDirectoryChangesW 再帰)             │  (Elisp 単体でも可能だが
└────────────────────────────────────────────────────┘   1〜3万ファイルでは重い)
┌─ 層 B: リフレッシュの高速化 ──────────────────────┐
│  magit-process-file を常駐プロセスへ迂回させる     │  ここが本題
│  B-1 素通しプロキシ → B-3 キャッシュ + 先読み      │
└────────────────────────────────────────────────────┘
┌─ 層 C: 書き込み操作の常駐化 ──────────────────────┐
│  stage/unstage/commit などを常駐プロセス経由に     │  速度目的ではなく
│                                                    │  キャッシュ整合性目的
└────────────────────────────────────────────────────┘
```

層 A だけでも「勝手に更新される」は達成できる。ただし **層 B が無いと
自動更新のたびに 1.7 秒固まる**ので、実用上は A と B はセット。

### 4.2 層 B の方式

常駐プロセスの中身には 3 案ある。2.1 の実測を踏まえた評価:

| 案 | 中身 | refresh 予測 | 出力互換のリスク | 実装量 |
|---|---|---|---|---|
| B-1 | **素通しプロキシ**（常駐プロセスが git.exe を spawn して結果を返すだけ） | 1670 → **約 670 ms** | 無し | 小 |
| B-2 | **gitoxide / libgit2 でネイティブ実装** | 数十 ms | **極大** | 特大 |
| B-3 | **キャッシュ + 先読み**（本物の git を裏で先に叩いて溜める） | **10〜50 ms** | 無し | 中 |

**B-1 でも約 2.5 倍速くなる（2026-09-03 に Rust で実測）。**
Emacs の `call-process` が 55〜58 ms なのに対し **Rust の `Command` は 28.9 ms**
（`git status -z --porcelain`）で、往復コストは 0.36 ms しか足されないため。
refresh 相当の 29 コマンドを通しで実行すると **Emacs 1669 ms / Rust 直列 659 ms**。
Rust で 8 並列にすると **180 ms** まで落ちるが、magit の呼び出しは同期・直列なので
この並列性は **先読み（B-3）でしか使えない**。

**B-2 は採らない。** magit は `status -z --porcelain`、
`diff --ita-visible-in-index --no-prefix`、`log --format=%h%x0c%D%x0c...` といった出力を
**バイト列として**パースする。gitoxide の出力を git とバイト単位で一致させるのは非現実的で、
しかも「一致しない」ことが静かなバグとして出る（セクションが空になる、hunk がずれる）。

**最終形は B-3。** 出力は本物の git のものなので互換性は定義上保証される。
速いのは「答えを先に用意してある」から。
そして **B-1 は B-3 への踏み台になる** — 同じプロトコル・同じ advice の上に
キャッシュと先読みを足すだけなので、作業が無駄にならない。

### 4.3 層 B（B-3）の動作

```
  [ファイル変更]
       │
       ▼
  ┌──────────────┐  ①変更検知（notify crate / ReadDirectoryChangesW 再帰）
  │  常駐プロセス │  ②epoch++ してキャッシュを破棄
  │   (Rust)     │  ③先読み: refresh 1 回分の 29 コマンドを
  │              │     バックグラウンドで **並列に** 実行してキャッシュに詰める
  │  キャッシュ  │     (29×30ms を直列でなく 8 並列 → 実時間 100〜150 ms)
  │  epoch=N     │  ④Emacs に "changed" を通知
  └──────────────┘
       │ stdio (往復 0.36 ms)
       ▼
  ┌──────────────┐  ⑤Emacs がデバウンス後に magit-refresh
  │    Emacs     │  ⑥magit-process-file → 常駐プロセスに問い合わせ
  │              │  ⑦全部キャッシュヒット → 29 × 0.36 ms ≒ **10 ms**
  └──────────────┘
```

**遅さを「消す」のではなく「ユーザが待たない時間帯に移す」設計。**
これがこの方式の肝。ユーザが `g` を押す頃には答えが出来上がっている。
並列先読みができるのは常駐プロセス側だけ（magit の呼び出しは同期・直列なので、
Emacs 側からは並列化できない）という点も、この配置が正しい理由。

キャッシュのキー: `(repo_root, args[], epoch)`。

- 先読みリストに無いコマンドは初回だけミス（= 素通しで約 30 ms）、以降ヒット。
- 判断できないコマンドは **キャッシュせず素通し**。安全側に倒す。

先読みリストは勘で作らず実測から作る。magit には
`magit-process-record-invocations` があり、全呼び出しをログに残せるので、
そこから頻出コマンドを抽出して先読みリストにする。

### 4.4 Emacs / 外部プロセスの分担

| | Emacs 側 | 常駐プロセス側 |
|---|---|---|
| UI・描画 | ○ 全部 | — |
| git 出力のパース | ○ 全部（magit のまま） | — |
| ファイル監視 | ✗ | ○（1〜3万ファイルを Emacs で捌かせない） |
| `.gitignore` 判定 | — | ○ (`ignore` crate) |
| デバウンス | △ | ○（両方に置く。6.4） |
| git コマンドの実行 | フォールバック時のみ | ○ 通常はこちら |
| **並列実行 / 先読み** | ✗ 不可能 | ○ ここが存在意義 |
| キャッシュ | ✗ 持たない（真実の源を 1 箇所に） | ○ |
| リポジトリ状態の判断 | ✗ | ✗ どちらもしない（git に聞く） |

**設計原則: 常駐プロセスに「git の意味」を持たせない。**
常駐プロセスは「git を代わりに叩いて結果を覚えておく箱」であって、git ではない。
これを守る限り、magit の挙動が変わることはない。

### 4.5 プロトコル

**トランスポート**: stdio（`make-process` の pipe）。ソケットは Windows で面倒なので使わない。
実測で往復 0.36 ms が出ているので、これで十分。

**フレーミング**: LSP 方式（`Content-Length:` ヘッダ + JSON 本体）。
Emacs 同梱の **`jsonrpc.el`**（eglot が使っているもの）がそのまま使える。
自前でフレーミングを書かない。

**バイナリの扱い（重要）**:
git の出力は NUL 区切りやマルチバイトを含む **生のバイト列**で、JSON 文字列に直接入らない。
→ `stdout` / `stderr` は **base64** で運ぶ。Emacs 側は `base64-decode-string`（C 実装）で戻す。
+33% のサイズ増だが、大きな diff でも実測上は問題にならない見込み（要検証、6.3）。

**メソッド（案）**:

```jsonc
// Emacs → プロセス : git を実行（同期読み取りの置き換え）
{"method":"git/run", "params":{
   "repo":"c:/Users/masao/.emacs.d/",
   "args":["status","-z","--porcelain","--untracked-files=normal","--"],
   "env":{"LC_ALL":"C"}            // magit-process-environment 相当
 }}
// ← 応答
{"result":{"exit":0, "stdout":"<base64>", "stderr":"<base64>",
           "cached":true, "epoch":42, "elapsed_ms":0}}

// Emacs → プロセス : 監視開始 / 終了
{"method":"watch/add",    "params":{"repo":"..."}}
{"method":"watch/remove", "params":{"repo":"..."}}

// プロセス → Emacs : 変化通知（通知のみ、応答不要）
{"method":"repo/changed", "params":{
   "repo":"...", "epoch":43,
   "kind":["worktree","index","head"],   // 何が変わったか
   "prewarmed": true                      // 先読み完了済みか
 }}

// Emacs → プロセス : ヘルスチェック / プロトコル版確認
{"method":"ping"}
```

`kind` を返すのは、**変化の種類でリフレッシュの粒度を変えられるようにする**ため
（`.git/HEAD` だけ変わったなら status セクションは再取得不要、など）。初版では使わなくてよい。

### 4.6 magit への差し込み（hook / advice のイメージ）

#### (a) 読み取りの迂回 — `magit-process-file` に `:around`

計測で確認したとおり、**同期読み取りは全部 `magit-process-file` を通る**。
ここ 1 点だけ押さえればよい。

```elisp
(defun my:magit-process-file--via-daemon (orig process infile buffer display &rest args)
  (if (or (not (my:gitd-live-p))            ; デーモンが居ない
          infile                            ; 標準入力を使う呼び出しは素通し
          (file-remote-p default-directory) ; TRAMP は素通し
          (not (equal process (magit-git-executable))))
      (apply orig process infile buffer display args)
    (condition-case err
        (my:gitd-run args buffer)           ; ← 常駐プロセスへ
      (error                                ; 何かあれば黙って従来経路
       (my:gitd-disable-temporarily err)
       (apply orig process infile buffer display args)))))

(advice-add 'magit-process-file :around #'my:magit-process-file--via-daemon)
```

**注意点**（実装時に必ず踏む）:

- `buffer` の形が多様。`nil` / `t` / バッファ / `(t "stderrファイル")` の 4 形態がある
  （計測時のトレースに `magit-stderrRaque8` が出ているのがそれ）。全部再現が要る。
- `magit--process-coding-system` によるデコードを再現しないと、
  日本語ファイル名が化ける。Windows の cp932 が絡むので特に注意。
- 終了コードを返すこと（`process-file` の戻り値）。

#### (b) 書き込みの通知 — 層 C

書き込みは `magit-call-process` / `magit-start-process` を通る。
**速度目的ではなく、書き込み直後に epoch を上げて先読みを開始させる**のが目的。
最小構成では advice すら不要で、ファイル監視が `.git/index` の変化を拾えば足りる。
まずは監視任せにして、遅延が気になったら advice を足す。

#### (c) 自動リフレッシュ — 層 A

```elisp
;; デーモンからの repo/changed 通知 → デバウンス → リフレッシュ
(defun my:gitd-on-changed (repo _epoch)
  (my:gitd--debounce
   repo 0.3
   (lambda ()
     (when (my:magit-refresh-allowed-p)
       (dolist (buf (my:magit-buffers-for repo))
         (when (get-buffer-window buf t)      ; 表示中のものだけ
           (with-current-buffer buf (magit-refresh-buffer))))))))

(defun my:magit-refresh-allowed-p ()
  (and (not (minibufferp))                    ; ミニバッファ入力中でない
       (not transient--window)                ; transient を開いていない
       (not (region-active-p))
       (not defining-kbd-macro)
       (sit-for 0.05)))                       ; 入力が続いていない
```

`magit-refresh-buffer` は point とセクションの展開状態を保存/復元するので、
そこは magit に任せてよい。

---

## 5. 段階計画

段階 0 は **実施済み**。結論は「安いチューニングでは解決しない」。

| 段階 | 内容 | 実装量 | 期待効果 |
|---|---|---|---|
| ~~0~~ | ~~Defender 除外 / セクション削減 / git 実体の直接指定~~ | **実施済** | **効果なし。2.1 参照** |
| **1** | 層 A のみ（Elisp）。`w32notify-add-watch` + `subtree` + デバウンス + 抑止条件 | 半日 | 「勝手に更新される」を達成。1 回 1.7 秒は残る |
| **2a** | 層 B の **B-1**（素通しプロキシ）。Rust の骨組み + jsonrpc + advice + フォールバック | 1〜2 日 | 1.7 秒 → 約 0.67 秒。**互換性の検証がここで終わる** |
| **2b** | 層 B の **B-3**（キャッシュ + 並列先読み + 監視の常駐側移管） | 2〜3 日 | 0.67 秒 → **数十 ms** |
| **3** | 層 C。書き込み経路の統合、`kind` による部分リフレッシュ | 数日 | 体感の詰め |

**着手順は 2a → 1 → 2b に決定**（2026-09-03）。段階 2a の詳細設計は
[magit-gitd-2a-design.md](magit-gitd-2a-design.md) に分離した。

**段階 2a を独立させる意図**: この段階では性能ではなく
「`buffer` の 4 形態」「文字コード」「終了コード」「フォールバック」という
**壊れやすい部分だけを、キャッシュという別の壊れやすさ抜きで検証できる**。
ここが通れば、2b でキャッシュを足しても疑う場所が 1 つに絞れる。

段階 1 と 2a は独立しているので、どちらを先にやってもよい。
「不満の大きさ」で言えば自動更新（段階 1）が先だが、
自動更新が入ると 1.7 秒の固まりが**今より頻繁に起きる**ので、
実用上は 2a まで進めてから段階 1 を有効化するほうが快適かもしれない。

---

## 6. 課題・リスク

### 6.1 出力互換

git の出力をネイティブ実装で再現するのは非現実的。**4.2 で B-2 を捨てることで回避する。**
この判断を後から覆さないこと。

### 6.2 キャッシュの正しさ

- 「ワークツリーの状態の純関数」でないコマンドをキャッシュすると壊れる
  （`git log` は純、`git stash list` は純、`git fetch` は副作用あり）。
- 対策: **許可リスト方式**。既知の読み取り専用コマンドだけキャッシュし、
  それ以外は素通し。迷ったらキャッシュしない。
- epoch の取りこぼしが致命的。`.git/index`、`.git/HEAD`、`.git/refs/**`、
  `.git/packed-refs`、ワークツリーの全部を漏れなく見る必要がある。
- 逃げ道として **強制的にキャッシュを捨てるリフレッシュ**（`C-u g` 等）を用意する。

### 6.3 base64 のオーバーヘッド → **解決（2026-09-03）**

実測: 100 KB で 0.48 ms、1 MB で 2.03 ms、**2 MB を `json-parse-string` 込みで 8.99 ms**。
問題にならない。**base64 + `jsonrpc.el` で確定**、独自フレーミングは不要。

なお base64 は「速いから」ではなく**必須**である点に注意。JSON 文字列は正当な UTF-8
でなければならないが、git の diff 出力には任意のバイト列（Shift_JIS のファイル、
バイナリ）が混ざりうる。

### 6.4 イベントの嵐とデバウンス

- `.git/index.lock` の生成/削除、`git gc`、ビルド出力で大量のイベントが出る。
  1〜3 万ファイルのリポジトリでは特に。
- 常駐側で `.gitignore` を尊重して間引き、さらに Emacs 側でもデバウンスする（二重）。
- **`.git/` 内は監視対象から外さない**（index / HEAD / refs が要る）が、
  `.git/objects/` は外す（gc で数万イベント出る）。

### 6.5 デッドロックとハング

Emacs が同期的に常駐プロセスの応答を待つ = `accept-process-output` でブロックする。
プロセスが応答しなければ Emacs が固まる。

- **必ずタイムアウト（例 500 ms）を置き、超えたら即フォールバック**して素の git を叩く。
- 一度失敗したら一定時間デーモンを使わない（サーキットブレーカ）。
- 再入（advice の中から advice が呼ばれる）に注意。

### 6.6 文字コード → **リスク低下（2026-09-03）**

実測すると `(magit--process-coding-system)` は `(utf-8-unix . undecided-unix)` で、
**cp932 は絡まなかった**（magit が `i18n.logOutputEncoding=UTF-8` を強制しているため）。
生バイトを受け取って `decode-coding-string` に `(car (magit--process-coding-system))` を
渡せばよい。**値を決め打ちせず必ずこの関数から取ること。**
それでも日本語ファイル名を含むテストリポジトリでの確認は行う。

### 6.7 自動リフレッシュの鬱陶しさ

勝手に画面が変わるのは操作中だと邪魔。抑止条件（4.6 (c)）を最初から入れる。
不安なら「フレームにフォーカスがあるときだけ」「表示中のバッファだけ」から始める。

### 6.8 プロセスのライフサイクル

- いつ起動するか（初回の magit 起動時 / Emacs 起動時）
- Emacs が死んだとき / 複数の Emacs があるとき（親 pid 監視して自死する、が簡単）
- バージョン不一致（Emacs 側 Lisp と Rust バイナリ）→ `ping` でプロトコル版を確認
- バイナリの配布。このリポジトリは git 管理下なので、`.exe` を置くのか
  各マシンで `cargo build` するのかを決める必要がある

### 6.9 Emacs のプロセス生成が遅い件そのもの

2.1 で見つかった「Emacs の `call-process` が PowerShell の 3 倍遅い」は
magit 以外にも影響している（`vc`、`grep`、`projectile`、`exec-path-from-shell` など）。
原因は未調査。**この設計とは独立に、別途調べる価値がある。**
原因が分かって解消すれば、本件の必要性自体が下がる可能性もある。

### 6.10 macOS / Linux

このリポジトリは 3 プラットフォーム対応を維持している。
今回は Windows のみを対象とし、他は **デーモン無しのフォールバック経路**で
従来どおり動かす。層 A（自動リフレッシュ）だけは `file-notify` 経由で
移植できる可能性があるが、今回は対象外。

---

## 7. 次に決めること

1. 段階 1（自動更新）と段階 2a（プロキシ）のどちらを先に着手するか
2. base64 で行くか、独自フレーミングにするか（6.3 の実測後でよい）
3. Rust バイナリの配布方法（リポジトリに `.exe` を置く / 各マシンで `cargo build`）
4. 6.9（Emacs のプロセス生成が遅い件）を先に調べるか
