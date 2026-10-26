<!-- -*- gfm -*- -->

# キーバインドを一覧表で見る（`my-keyhelp`）

`describe-bindings` をモード別のテキストから 1 枚の表に置き換える。Context 列を作るには、C 実装の出力をパースするのではなく自前でキーマップを走査するしかない。絞り込みで不要な行が大量に出ていた原因は orderless の空白分割。

最終更新: 2026-10-27 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## きっかけ

`describe-bindings`（`<f1> b`）が読みにくい。zed の Keymap Editor と比べて、
不満は次の 3 つに分解できた。

| | `describe-bindings` | zed の Keymap Editor |
|---|---|---|
| 構造 | モードごとのセクションに分かれる | **1 つの表** |
| 桁 | セクションごとに揃っていない | 列で揃う |
| Context | セクション見出しを遡らないと分からない | **列になっている** |
| 絞り込み | `*Help*` の中で `C-s` | 上部でインクリメンタル |

上流も「後処理で読みやすくする」路線を取っていて、`C-h b` 専用の defcustom が
3 つ生えている（`describe-bindings-outline` / `-show-prefix-commands` /
`-outline-rules`）。ただし**全体を整形し直すものは、サードパーティにも見当たらない**
（helm-descbinds / embark-bindings / counsel は「一覧をやめて補完で絞る」、
which-key は「そもそも引かせない」）。bug#70530 の「キーではなくコマンド名で
ソートしたい」は未解決のまま。

`describe-bindings-outline-rules` を nil にして全セクションを畳む案は、
2026-10 に実際に設定して試した上で**戻した**（不満は「量」ではなく構造のほうだった）。

## なぜ出力をパースしないのか

`describe-bindings` は C の `describe-buffer-bindings` を呼んでバッファに
テキストを吐き、その上に `outline-minor-mode` を被せているだけ。Lisp から
触れるのは出来上がったテキストなので、整形し直すだけなら後処理でもできる。

**できないのは Context 列。** 「この行がどのキーマップ由来か」はセクション
見出しの形でしか残っておらず、親キーマップ（`special-mode-map`）に至っては
区別すらされない。だから `map-keymap` を自前で回す。

実測（2026-10-27、dired バッファ / Emacs 31.1 / Windows 11）。

| | |
|---|---|
| 有効マップの走査 | **3 ms** / 1189 件（global 1014 + `dired-mode` 159 + minor 16） |
| `key-binding` によるシャドウ判定 | **1 ms** / 159 件 |
| `help-fns-find-keymap-name` | 1〜5 ms（`mapatoms` なのでキャッシュする） |
| 表の生成（収集 + 整形 + 描画、1081 行） | **53 ms** |

docstring の 1 行目を全部引くのは 694 コマンドで 177 ms。4 列目を Doc 列に
する案はここで検討したが、**シャドウ状態のほうを採った**（下記）。

## キーマップに名前を付ける

`current-active-maps` は有効なキーマップを**優先度順**で返すが、名前は付かない。
名前の辞書を先に作って `eq` で引く。

```elisp
(push (cons (current-global-map) 'global) tbl)
(push (cons (current-local-map) major-mode) tbl)
(dolist (e minor-mode-map-alist) (push (cons (cdr e) (car e)) tbl))
;; minor-mode-overriding-map-alist / emulation-mode-map-alists / overriding-*
```

実測で dired バッファの 12 マップ**全てに名前が付いた**（取りこぼし 0）。

```
yas--direct-fundamental-mode / cua-mode / override-global-mode /
doom-modeline-mode / projectile-mode / server-mode / which-key-mode /
yas-minor-mode / recentf-mode / tab-bar-mode / dired-mode / global
```

`help-fns-find-keymap-name` でも名前は引けるが、そちらは `dired-mode-map` の
ような**変数名**になる。Context 列には `dired-mode` / `yas-minor-mode` という
**モード名**のほうが分かりやすいので、トップレベルは上の辞書、親キーマップだけ
`help-fns-find-keymap-name` にしてある。

## 【重要】`map-keymap` は親キーマップの束縛も返す

ここを分離しないと、同じ Context の中に矛盾した行が並ぶ。

```
SPC    dired-next-line      dired-mode      ← 実際に効く
SPC    scroll-up-command    dired-mode      ← special-mode-map のもの
```

`dired-mode-map` の親は `special-mode-map`（`keymap-parent` で確認）。
`map-keymap` は親まで辿るので、両方が「dired-mode」として出てくる。
dired バッファでは **21 件**がこれに当たる。

分離は「親を別に走査して差し引く」形にした。`set-keymap-parent` で一時的に
親を外す手もあるが、**ユーザーが使っているキーマップを書き換えることになる**ので
採らない。

比較はキーの文字列と束縛の `eq` で行う。`equal` のハッシュを張ると、DEF に
ラムダや `menu-item` の入れ子が来たときに深い比較になる。

## シャドウ判定は `key-binding` で行う

4 列目に何を置くかは Doc 列と競合したが、**シャドウ状態**を採った。
CLAUDE.md に記録のある「`yas-minor-mode-map` が `<C-tab>` を奪っていて
`tab-next` が片道だけ効かなくなっていた」類の事故が、ひと目で分かるため。

判定を `current-active-maps` の順序ではなく `key-binding` で行うのは、
**プレフィクス自体が他のマップで潰されている場合**（その配下が丸ごと到達不能に
なる）まで正しく拾えるから。コストは 1189 件で 10 ms 程度。

dired バッファの実測では **1081 件中 63 件**がシャドウされていた。内訳は
`special-mode-map` の `SPC` / `S-SPC` / `DEL` / `?` / `>` / `<` など。

### 束縛が nil の行は出さない

`yas-minor-mode-map` の `TAB` のように、DEF が nil / `undefined` の束縛がある。
これは「親の定義を打ち消して下のマップに落とす」ためのもので、キーバインド
ではない。出すと次のような誤解を招く行になる。

```
TAB    nil    yas-minor-mode    shadowed
```

dired バッファで 3 件あった（1084 → 1081）。

### `menu-bar` 配下は出さない

メニュー定義（`menu-item` の入れ子）なので、キーバインドの一覧としては雑音。
`<menu-bar> <subdir>` 1 件だけで 2 KB を超える。`tool-bar` / `mode-line` /
各種 fringe・margin・divider も同様に除く（`my:keyhelp-exclude-events`）。

## 【重要】`tabulated-list` は溢れた値を切り詰めない

列幅を超える値があると、その行だけ**次の列が右へ押される**。

```
[remap set-mark-command] cua-set-mark          cua-mode
[remap exchange-point-and-mark] cua-exchange-point-and-mark   cua-mode   ← ずれる
```

`[remap exchange-point-and-mark]` は 31 文字あり、Key 列 24 文字を 7 文字
はみ出す。**桁が揃わないのを直すのがこのモードの目的**なので、エントリを作る
段階で `truncate-string-to-width` に通して詰める。

検算は「全行で Command 列が始まる桁が同じか」を数える。

```
行 1081 / Command 開始桁の分布 ((25 . 1079) (1 . 2))
例 ((1 "<undo>" "undo") (1 "<compose-last-chars>" "compose-last-chars"))
```

桁 1 の 2 件は測り方の偽陽性（`<undo>` という Key の中に `undo` という
Command 文字列が含まれるので `string-search` が Key 列でヒットする）。
**実際には 1081 行すべてが桁 25 から始まる。**

### 列幅は絞り込み前の全行から 1 回だけ決める

絞り込むたびに計算し直すと、1 文字打つごとに列が動いて目で追えなくなる。

**幅の測定は表示後に行う。** まだウィンドウに出ていないバッファで
`window-body-width` を呼ぶと、**呼び出し元のウィンドウの幅**が返る。
`my:keyhelp` が `pop-to-buffer` のあとに `my:keyhelp--refresh` をもう一度
呼ぶのはこのため。

## 【重要】絞り込みで空白を AND 区切りにしない

これが「`*Help*` で `C-s` すると不要な行が大量に出る」の真因だった。

`C-s` は `my:consult-line`（`my-completion.el`）で、候補の絞り込みは
orderless が行う。**orderless は空白を AND 区切りとして扱う**ので、
`C-c e` は「`C-c` を含む」かつ「`e` を含む」になり、
`C-c & C-n ... yas-n`**`e`**`w-snippet` が両方を満たしてしまう。
`consult-line` が候補の先頭に付ける行番号も、キーの一覧では邪魔になる。

表の上では素の部分一致にする。`regexp-quote` を通すので `C-c` の `-` も
`[remap ...]` の `[` もリテラルとして効く。先頭の `^` と末尾の `$` だけは
アンカーとして解釈する（画像で `^C-c e` と打たれていたので、前方一致の
意図は既にあった）。

実測（dired バッファ、1081 行）。

| クエリ | 対象列 | 一致 |
|---|---|---|
| `C-c e` | Key | **1** |
| `C-c` | Key | 140 |
| `^C-c` | Key | 136（うち 120 は `C-c p` = projectile） |
| `e` | Key | 275 |
| `^e` | Key | 21 |
| `e$` | Key | 23 |
| `[remap` | Key | 15 |
| `dired` | Command | 141 |
| `yas` | Context | 9 |

大文字を含むクエリのときだけ大小を区別する。キーの表記では `C-c` と `c-c` が
別物だが、区別を既定にすると打ちにくい。

### 入力はミニバッファ、結果はバッファ

`read-from-minibuffer` で読みつつ、`after-change-functions` から 1 文字ごとに
表を絞る。`C-g` で元のクエリと対象列に戻す。

- **`after-change-functions` の中でエラーを出さない。** 打った 1 文字が入ったのか
  どうか分からなくなるので、`condition-case` で囲んで `message` に落とす
- **`point` を動かすだけでは表示が追従しない。** 選択されているのはミニバッファ
  なので、表のウィンドウは window-point を別に持っている。
  `set-window-point` / `set-window-start` を明示的に呼ぶ

### 対象列は 4 列ぜんぶ。既定は全体

TAB で 全体 → `Key` → `Command` → `Context` → `State` の順に回る
（`my:keyhelp-default-field` で既定を変えられる）。

**空白を AND 区切りにしないので、全体に当てても絞り込みは鋭いまま。**
実測で `C-c e` は Key 列でも全体でも **1 件**。列を絞るのが効くのは、
同じ語が複数の列に現れるときだけ。

| クエリ | 対象列 | 一致 |
|---|---|---|
| `dired` | Key | 0 |
| `dired` | Command | 141 |
| `dired` | Context | 132 |
| `dired` | 全体 | 143 |
| `shadowed` | State | 61 |
| `C-c e` | 全体 | **1** |

`State` 列が対象に入っているので、`shadowed` と打てば隠された束縛だけが残る
（`t` のトグルと同じ結果を、検索語の側からも引ける）。

### 【重要】絞り込みの状態を列見出しに埋めない

当初は対象列の見出しにクエリを埋めていた（`Key [^C-c a]`）。
**`tabulated-list-sort-key` は列名の文字列で `assoc` して列を引くので、
その列でソート中だと見出しを書き換えた瞬間にソートが外れる。**

状態の置き場所は次のとおり。`header-line` には余白が無い（Command 列が
ウィンドウ幅の残りを全部取るので、列の合計 106 に対してウィンドウ幅 108）。

| | |
|---|---|
| 対象列 | `mode-name`（`KeyHelp:Command`） |
| クエリと件数 | `mode-line-process`（`[C-c a 13/1079]`） |
| 切り替えた瞬間 | `minibuffer-message`（ミニバッファ使用中は `message' が echo area に出ない） |

doom-modeline は `mode-line-process` を表示する（実測）。

#### `mode-line-process` に入れる文字列は `%` をエスケープする

`format-mode-line` は `%` を escape として解釈する。そして **`%` はこの設定の
キーバインドの 1 つ**（`my:match-paren`）なので、検索語にそのまま現れる。
`(string-replace "%" "%%" query)` を通す。

## キーバインドの入口

`C-h` は `delete-backward-char` に割り当ててあるので（`my-keybind.el`）、
ヘルプのプレフィクスは `<f1>` と help キー（どちらも `help-map`）。
`<f1> b` を差し替え、元の `describe-bindings` は `<f1> C-b` に退避した。

| キー | |
|---|---|
| `<f1> b` | 一覧表（`C-u` でグローバルマップを除く） |
| `<f1> C-b` | 元の `describe-bindings` |

表の中では次のとおり。

| キー | |
|---|---|
| `/` / `C-s` | インクリメンタルに絞り込む（既定は 4 列の全体） |
| `TAB` | 対象列を 全体 → Key → Command → Context → State と切り替える |
| `DEL` | 絞り込みを解除 |
| `RET` | その行のコマンドの説明（`describe-function`） |
| `S` | 列でソート（`tabulated-list-mode` 組み込み。見出しのクリックでも） |
| `t` | シャドウされた束縛の表示を切り替える |
| `L` | グローバルマップを含めるかを切り替える |
| `w` | その行のキーを kill-ring へ |
| `g` | 取り直す |

`S` で Command 列を選べば bug#70530 の「コマンド名順」になる。
`tabulated-list-mode` に載せたので、ここは何も書かずに手に入った。
