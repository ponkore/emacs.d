# 段階 1: 自動リフレッシュ — 検討（実装前）

作成: 2026-09-03 / 状態: **検討中。未決事項は 7 章**
前提: [magit-auto-refresh-plan.md](magit-auto-refresh-plan.md) /
[magit-gitd-2a-design.md](magit-gitd-2a-design.md)（段階 2a は実装・コミット済み）

---

## 1. やりたいこと

ワークツリー / インデックス / HEAD の変化を検知して、表示中の magit バッファを
自動で最新化する。`g` を押さなくてよくする。

段階 2a でリフレッシュが 1.7 秒 → 0.6 秒になったので、自動更新が入っても
実用に耐える。逆に言えば **2a が無いと自動更新は入れられなかった**。

---

## 2. 計測（2026-09-03、GUI Emacs で実施）

### 2.0 batch では一切測れない

**w32notify のイベントはコマンドループ経由で配送されるため、`--batch` では
1 件も届かない。** 最初に batch で測って全部 0 件になった。
`accept-process-output` や `sit-for` を回しても駄目。
**この節の検証は全部 GUI（`emacs -Q -l probe.el`）で行うこと。**

### 2.1 `w32notify-add-watch` の `subtree` は使える

| | |
|---|---|
| watch の追加コスト | **0.15〜0.25 ms**（`~/.emacs.d` でも同じ） |
| 再帰性 | 3 階層下も届く。**watch 後に作ったディレクトリも届く** |
| 静穏時のイベント | 3 秒待って 0 件 |
| イベントの形 | `(DESCRIPTOR ACTION FILE)`。**FILE は監視対象からの相対パスで区切りはバックスラッシュ**（`.git\index.lock`, `build\out\o199.o`） |
| ACTION | `added` / `modified` / `removed` / `renamed-from` / `renamed-to` |

`filenotify.el` の `file-notify-add-watch` は `subtree` を渡さないので非再帰。
**`w32notify-add-watch` を直接呼ぶ必要がある。**

### 2.2 イベントは大量に出るし、欠落もする

| 操作 | 生イベント数 |
|---|---|
| ファイル 1 つ保存 | 7 |
| `git status` | 10 |
| `git add -A` | 8 |
| `git reset` | 32 |
| `git add` + `commit` | 49〜57 |
| `git checkout -b` | 35 |
| **`git gc`** | **126〜178** |
| **`build/` に 200 ファイル作成** | **2001**（1 ファイルあたり 10） |
| **`build/` に 1000 ファイル作成** | **4095**（1 万件出るはずなので**欠落**） |

- 1000 ファイルで 4095 件は `ReadDirectoryChangesW` のバッファ溢れ。
  **イベントの完全性に依存した設計にはできない。**
  「何か変わった」ことさえ分かればよいので実害は無い。
- コールバックのコストは**無視できる**（2001 件で累計 0.7 ms）。
  ただしそれは「push するだけ」の場合。**イベントごとに git を呼ぶような
  作りにしてはいけない。**

### 2.3 【最重要】リフレッシュ自体がイベントを生む

`magit-refresh-buffer` を 1 回走らせるだけで **毎回きっちり 7 件**出る。

```
  4 件  .git/index.lock     (update-index --refresh がロックを作って消す)
  3 件  .git                (ディレクトリ自身の mtime)
```

そのまま繋ぐと **「イベント → リフレッシュ → イベント → …」で自励振動する。**

**ただし `.git/index` は書かれない。** 外部の `git add` は `.git/index` を
書くので、ここで区別できる。

### 2.4 フィルタ案を実測で検証した

```elisp
;; 除外するもの
".git/index.lock"      ; リフレッシュ自身が作る
".git"                 ; ディレクトリ自身の mtime
".git/objects/..."     ; gc / fetch で大量に出る。index や refs で検知できる
".#*"                  ; Emacs のロックファイル
"#*#" / "*~"           ; 自動保存 / バックアップ
```

| | 生 | フィルタ後 |
|---|---|---|
| **`magit-refresh-buffer`** | 7 | **0** ← 自励振動しない |
| 外部でファイル更新 | 7 | 2 |
| 外部で `git add` | 14 | 2 |
| 外部で `git commit` | 49 | 24 |
| 外部で `git checkout -b` | 35 | 25 |
| 外部で `git gc` | 126 | 57 |
| **`build/` に 200 ファイル（gitignore 済）** | 2001 | **1001** ← 落ちない |

**実際にループさせたら 1 周で収束した**（変更 → 2 件 → リフレッシュ 1714 ms →
新たに 0 件 → 終了）。フィルタの有効性は確認できた。

フィルタの取りこぼし（実装時に足す）: `.git/**/*.lock`（`packed-refs.lock`、
`AUTO_MERGE.lock`）、`.git/gc.pid`、`.git/objects` というディレクトリ自身。

### 2.5 抑止条件に使える変数は全部ある

`transient--window` / `transient--prefix` / `isearch-mode` /
`defining-kbd-macro` / `executing-kbd-macro` / `magit-this-process` /
`magit-process-buffer` / `minibufferp` — すべて存在する。

---

## 3. 設計

### 3.1 構成

```
w32notify (subtree, 1 リポジトリ 1 watch)
   │  コールバックは push するだけ (2001 件で 0.7 ms)
   ▼
パスフィルタ (2.4)          ← 自励振動を止める
   ▼
デバウンス 400 ms
   ▼
抑止条件のチェック (3.3)
   ▼
レート制限 (最短 2 秒間隔)   ← ビルド中の暴走を止める
   ▼
表示中の magit バッファだけ magit-refresh-buffer
```

### 3.2 どのバッファを更新するか

- **ウィンドウに表示されているものだけ**（`get-buffer-window` が非 nil）。
  表示されていないバッファは次に表示されたときに更新すればよい。
- `magit-refresh`（全バッファ + `magit-pre/post-refresh-hook`）ではなく
  `magit-refresh-buffer`（そのバッファだけ）を使う。
  `magit-post-refresh-hook` には diff-hl がぶら下がっており、
  自動更新のたびに全バッファの差分を取り直すのは重い。
- point とセクションの展開状態は `magit-refresh-buffer` が保存・復元する。

### 3.3 抑止条件

```elisp
(and (not (minibufferp))
     (not transient--window)            ; transient を開いている
     (not isearch-mode)
     (not defining-kbd-macro) (not executing-kbd-macro)
     (not (region-active-p))
     (not (input-pending-p))            ; 入力が続いている
     (frame-focus-state))               ; フレームにフォーカスがある
```

満たさないときは**捨てずに再度デバウンスし直す**（後で必ず更新する）。

---

## 4. 見つかった課題

### 4.1 【解決済】自励振動

2.3 の問題。2.4 のフィルタで解決。実測で refresh → フィルタ後 0 件、
ループも 1 周で収束することを確認した。

### 4.2 【解決済 2026-09-03】magit 自身の操作で二重リフレッシュになる

magit で stage すると `.git/index` が書かれ、フィルタを通るイベントが出る。
**magit は自分でリフレッシュ済み**なので、そのあと監視側がもう 1 回走る。

**当初「Elisp だけではきれいに直せない、2b で消える」と判断したが、これは誤り。**
時刻で区別しようとしたのが間違いで、**内容で見れば決定的に判定できる。**

#### 解き方: 状態フィンガープリント

`.git/index` と `.git/HEAD` の `(mtime . size)` をフィンガープリントとし、
**`magit-refresh-buffer-hook` で毎回取り直す。**

このフックは **自分のリフレッシュでも magit 自身のリフレッシュでも走る**ので、

- magit の stage → git が index を書く → magit がリフレッシュ →
  **そこでスナップショット** → あとから届くイベントは必ず一致 → **抑止**
- 外部の `git add` → index が書かれる → スナップショットは前のまま →
  **一致しない** → **リフレッシュ**

**イベントの配送順に依存しない**のが利点。w32notify のイベントは
コマンドループ経由で遅れて届くので、時刻ベースの判定は成立しない。

`stat` を 2 回するだけで git は呼ばない。

#### 実測（GUI、`emacs -Q`）

| | リフレッシュ | 分類を通ったイベント | 抑止 |
|---|---|---|---|
| 手で `magit-refresh-buffer` 1 回 | 1（自分で呼んだぶん） | 7 | 1 |
| **magit で stage** | **1（magit 自身のみ）** | 14 | **1** |
| **magit で unstage** | **1（magit 自身のみ）** | 14 | **1** |
| 外部でファイル更新 | 1 | 9 | 1 |
| 外部で `git add` | 1 | 14 | 1 |
| 外部で `git commit` | 1 | 35 | 1 |
| 外部で `git checkout -b` | 1 | 44 | 1 |
| 外部で `git checkout`（戻る） | 1 | 34 | 1 |
| 何もせず 5 秒待つ | 0 | 0 | 0 |

`git checkout` では **status バッファのヘッダが実際に書き換わる**ことも確認した
（`Head: main init` → `Head: topic init` → `Head: feature init` → `Head: main init`）。

### 4.3 【未解決・これが一番大きい】`.gitignore` を見られない

`build/` に 200 ファイル作ると、フィルタ後でも **1001 件**残る。
git は無視するのに監視は拾う。パスだけのフィルタでは落とせない。

**ビルド中は「デバウンス窓ごとに 0.6 秒のリフレッシュ」が延々と走る**ことになる。
これは実用上かなり困る。

段階 1（Elisp のみ）でできる緩和:

| 緩和策 | 効果 | 副作用 |
|---|---|---|
| **表示中のバッファだけ更新** | 大きい。ビルド中に magit を見ていることは少ない | 表示していれば結局走る |
| **レート制限（最短 2 秒間隔）** | 暴走の上限を決められる | 反応が最大 2 秒遅れる |
| 除外パターンを手で設定（`target/` `node_modules/` `build/`） | 効くが管理が面倒 | `.gitignore` と二重管理 |
| `git check-ignore` を呼ぶ | 正確 | **イベントごとに git を呼ぶのは論外**。バッチでも遅い |

**根本的な解決は 2b。** Rust の `ignore` crate で `.gitignore` を正しく解釈し、
Emacs にイベントを送る前に落とす。

### 4.3.1 【実装中に見つけた】`frame-focus-state` を抑止条件に入れてはいけない

「フレームにフォーカスが無いなら急がない」つもりで `frame-focus-state` を
抑止条件に入れたところ、**フォーカスを失った時点から二度と更新されなくなった**
（実測で 0.3 秒ごとに再アームし続けるログが取れた）。

抑止条件に置いてよいのは **ユーザが操作をやめれば自然に解消するもの**だけ。
ミニバッファ・transient・isearch・キーボードマクロ・リージョン・
`input-pending-p` はすべてそう。フォーカスはそうではない。

背景での CPU 消費は `my:magit-watch-visible-only` と
`my:magit-watch-min-interval` で抑える。

あわせて、抑止されたときの待ち直しは**デバウンス値ではなくレート制限の値**に
した。0.3 秒で回してもタイマーが空回りするだけで応答は良くならない。

### 4.3.2 【実装中に見つけた】`.lock` の除外は `.git/` 配下に限ること

`index.lock` / `HEAD.lock` / `packed-refs.lock` を落とすために `.lock` で
除外したくなるが、**ワークツリーには `Cargo.lock` や `flake.lock` といった
追跡対象のファイルがある。** 条件を `.git/` 配下に限らないと、
それらの変更を取りこぼす。

### 4.4 イベントは欠落する

1000 ファイルで 4095 件（約 1 万件出るはず）。`ReadDirectoryChangesW` の
バッファ溢れ。**「何か変わった」ことさえ分かればよいので実害は無いが、
イベントの完全性に依存した設計にはできない。**

差分更新（「このファイルだけ再描画」）のような最適化はやらないこと。

対策として分類に `suspect` を設けた。`.git` ディレクトリ自身や
`.git/**/*.lock` は**それ自体は何も証明しないが「何かは起きた」合図**なので、
拾ったうえでフィンガープリントで判断する。決め手のイベント
（`.git/HEAD` など）が落ちても、粗い `.git` の mtime 更新は残りやすい。

残る穴: `.git/refs/tags/*` だけが変わる操作（`git tag`）で、その `meta`
イベントが落ちると取りこぼす。フィンガープリントに refs 全体を入れるのは
コストが見合わないので、次のイベントでの回復に任せる。

### 4.5 検証が batch でできない

2.0 のとおり、w32notify のイベントは batch では 1 件も届かない。
**段階 1 のテストは全部 GUI で書く必要がある**（`emacs -Q -l probe.el` で
結果をファイルに書いて `kill-emacs` する形）。
CI 的な自動テストにはしにくい。

### 4.6 watch のライフサイクル

- いつ張るか: `magit-status-mode` のバッファを作ったとき
- いつ外すか: そのリポジトリの magit バッファが全部消えたとき（`kill-buffer-hook`）
- リポジトリを跨ぐとき、worktree、サブモジュールの扱い
- **`.git` がファイル（worktree / submodule の場合）だと、実体の
  `.git/worktrees/...` は監視対象の外にある。** HEAD の変化を拾えない可能性がある
  → 実装時に要確認

### 4.7 その他

- **`magit-auto-revert-mode` との相互作用。** 自動リフレッシュが増えると
  ファイルバッファの revert も増える。たぶん望ましいが要観察
- **フレームが非フォーカスのときも監視は動く。** バックグラウンドで
  リフレッシュが走ると CPU を食う。`frame-focus-state` で抑止する
- 大量イベントは Emacs の入力キューに積まれる。コールバックが軽くても、
  **1 万件のイベントが入力処理を圧迫する可能性**がある（未計測）

---

## 5. 段階 1 でどこまでやるか

**やる**

- `w32notify-add-watch` + `subtree` による監視（Windows のみ）
- パスフィルタ（2.4）
- デバウンス + 抑止条件 + レート制限
- 表示中の magit バッファだけ `magit-refresh-buffer`
- `M-x my:magit-autorefresh-mode` で入切、統計の表示

**やらない（2b でやる）**

- `.gitignore` の尊重（Rust の `ignore` crate）
- magit 自身の書き込み由来のイベントの握り潰し
- 変化の種類（worktree / index / HEAD）による部分リフレッシュ

---

## 6. 実装したときに再利用できるもの

段階 2b で監視をデーモンに移しても、**Elisp 側で捨てるのは
「イベントの入り口」だけ**。以下はそのまま残る。

- デバウンス
- 抑止条件の判定
- どのバッファを更新するか
- レート制限
- モードと統計

入り口が `w32notify` のコールバックから `repo/changed` 通知に変わるだけなので、
段階 1 の作業はほぼ無駄にならない。

---

## 7. 実装状況（2026-09-03）

`user-lisp/my-magit-watch.el` を実装した。**課題 2 は解決済み**（4.2）。

| | 状態 |
|---|---|
| 監視（`w32notify` + `subtree`） | 実装済み |
| パス分類（`nil` / `suspect` / `index` / `meta` / `worktree`） | 実装済み |
| フィンガープリントによる判定（課題 1・2 の解） | **実装済み・検証済み** |
| デバウンス / 抑止条件 / レート制限 / 表示中のみ | 実装済み |
| `M-x my:magit-watch-mode` / `my:magit-watch-stats` | 実装済み |
| **`.gitignore` の尊重（課題 3）** | **未実装** |

**既定では有効にしていない。** 課題 3 が残っているため、`init.el` では
`require` するだけで `my:magit-watch-mode` は呼んでいない。
試すときは `M-x my:magit-watch-mode`。

### 検証方法

GUI でしか検証できない（2.0）。`emacs -Q -l probe.el` で結果をファイルに
書いて `kill-emacs` する形にしてある。

**テストを書くときの注意**: このマシンは `init.defaultBranch = main` なので、
テスト用リポジトリで `git checkout master` は失敗する。`-q` で握り潰すと
「イベントが来ない」と誤診する（実際に 1 度誤診した）。
`git init -b main` と明示すること。

## 8. 未決事項（要相談）

1. **課題 3（`.gitignore`）をどう片付けるか。** 段階 2b で監視ごと
   デーモンに移して `ignore` crate で落とすのが本筋。
   それまで既定で有効にするかどうか。
2. レート制限の間隔（既定 2 秒）とデバウンス（既定 400 ms）の値。
3. `magit-refresh-buffer`（そのバッファだけ）で十分か、`magit-refresh`
   （`magit-post-refresh-hook` 経由で diff-hl も更新される）が要るか。
   今は前者。diff-hl は自動更新されない。
