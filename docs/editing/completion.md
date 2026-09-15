<!-- -*- gfm -*- -->

# 補完（vertico / consult / corfu）

`C-x C-r` が recentf とブックマークの両方から選ぶ仕掛け、text-mode の ispell 補完を切った理由、capf のトラブルの切り分け方。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## `C-x C-r` は recentf とブックマークの両方から選ぶ（2026-09-09）

`consult-recent-file` の代わりに `my:consult-recent-file-or-bookmark`
（`my-completion.el`）を割り当ててある。`consult--multi` で 2 つのソースを
束ねているので、`consult-narrow-key`（`<`）に続けて `f` でファイル、
`m` でブックマークだけに絞れる。バッファも混ぜたいなら `C-x b`
（`consult-buffer`）にどちらも既に入っている。

### 【重要】`consult-source-recent-file` は開いているファイルを落とす

組み込みのソースは `:items` の中で `consult--buffer-file-hash` を引き、
**既にバッファで開いているファイルを一覧から除外する**
（`consult.el:5076`）。`consult-buffer` ではそれらが Buffer ソースの側に
出るので正しいが、**ファイルを開く入口でこれをやると、開いているものだけ
選べなくなる**。

そのため recentf 側は組み込みを使わず、`recentf-list` をそのまま出す
`my:consult--source-recent-file` を定義してある（`consult-recent-file` と
同じ中身）。ブックマーク側は除外ロジックが無いので
`consult-source-bookmark` をそのまま使う。

実測（同じ瞬間に両方の `:items` を呼んで数えた）:

| | 件数 |
|---|---|
| `recentf-list` をそのまま | **195** |
| `consult-source-recent-file`（組み込み） | **194** |
| そのとき開いていたファイル | 1 |

**差が「いま開いている数」なので、開いていなければ気づけない。**

`:state` は `consult--file-state` / `consult--bookmark-state`
（`consult--define-state` が作る）で、プレビューと確定時の
`consult--*-action` を兼ねる。`:action` は要らない。
**`consult--multi` は autoload されていない**ので、コマンドの側で
`(require 'consult)` すること。

## text-mode の ispell 補完は切ってある（2026-09-07）

`text-mode` は `text-mode-ispell-word-completion`（既定 `completion-at-point`、
Emacs 30 で新設）を見て `ispell-completion-at-point` を capf に足す
（`text-mode.el:155`）。ところが `ispell-alternate-dictionary` の既定値は
`/usr/dict/words` などを `file-readable-p` で探す `cond` なので、**Windows では
必ず nil** になり、`ispell-lookup-words` が問答無用で `error` を投げる
（`ispell.el:2620`）。

corfu は capf を `corfu--protect`（`handler-bind` + `ignore-errors`）で包んで
いるため、`corfu--debug` がそれを拾って `*Messages*` に backtrace を流す。

```
Corfu detected an error:
  ...
  ispell-lookup-words("調査")
  ispell-completion-at-point()
  corfu--capf-wrapper(ispell-completion-at-point 1)
  corfu-auto--complete-deferred((#<window 66 on *claude-input(...)*> ...))
```

**発火するのは日本語を打っているとき。** capf は前から試され、`cape-dabbrev`
などが候補を出せなかったときだけ最後尾の ispell に到達する。バッファ内に既出で
ない語を打つたびに出るので際限なく溜まる。実測（`*claude-input*` で日本語を
書いていたセッション。61 件が全部これ）:

| | |
|---|---|
| 補完候補が失われるか | **失われない** |
| `*Messages*` の占有 | 1000 行中 **約 915 行（91%）** |
| エラー 1 回のコスト | **38.4 ms**（`corfu--protect` 経由） |
| エコーエリア | 赤字で `Corfu detected an error: Press C-h e to see the stack trace` |

候補が失われないのは、ローカル値が `(t ispell-completion-at-point)` で
**グローバル値を表す `t` が先、ispell が最後尾**だから（`add-hook` の depth 10
でもグローバル値の位置より後ろになる）。他の capf が全滅したときにしか到達
しないので、そこでエラーが飛んでも失われる候補は無い。壊れるのはログと
エコーエリアと 38 ms だけだが、`message-log-max` が 1000 なので**他の
メッセージがほぼ全部押し流される**のが実害。

`my-text.el` で `text-mode-ispell-word-completion` を nil にして capf ごと
外してある。`C-M-i` は変わらない（`:set` 関数が `ispell-complete-word` を
`text-mode-map` に張るのは「非 nil かつ `completion-at-point` 以外」のときだけ。
実測でも `complete-symbol` のまま）。効き先は `text-mode` 派生の全部
（`markdown-mode` / `gfm-mode` / `my:claude-mode` / `org-mode`）。

**変数を変えても、既に text-mode 派生になっているバッファには効かない。**
`add-hook` はモードを立てた時点で済んでいるため。その場で直すなら

```elisp
(remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)
```

## capf のトラブルは「どこで打ち切られたか」を見る

corfu は `run-hook-wrapped` で capf を前から回し、最初に候補を返したところで
止まる。したがって:

- **後ろの capf でエラーが出ても、前で候補が出ていれば表示は正常**。
  「補完は効いているのにエラーが出る」ときはこれを疑う
- 逆に**前の capf がエラーを投げると、後ろの候補ごと失われる**。
  `corfu--protect` は `ignore-errors` で丸ごと握るため、その回の補完が消える

再現と切り分けは corfu と同じ経路を通すのが確実（`completion-at-point` を
対話的に呼ぶと `corfu--capf-wrapper` を経由しないので条件が変わる）。

```elisp
(corfu--protect
 (lambda ()
   (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper 1)))
```

戻り値の `car` が採用された capf の関数名。候補は
`(alist-get 'corfu--candidates (plist-get (cddddr result) :corfu--state))`。

