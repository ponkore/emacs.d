<!-- -*- gfm -*- -->

# 視覚行の移動（`C-n` / `C-p` / `C-e`）

`line-move-visual` の既定は t。`C-e` をウィンドウの右端で止める `my:end-of-visual-line` が踏んだ 3 つの罠と、その GUI 実測。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## 折り返しを考慮した `C-n` / `C-p` は `line-move-visual`（既定 t）

既定が `t` なので、上の 1 行は **Emacs の既定を全体で打ち消すだけ**のもの
だった。削除して既定に戻してある。

GUI 実測（ウィンドウ幅 144、折り返した長い行の途中「固」の上で `C-n`。
実際にウィンドウへ出して測ること）:

| `line-move-visual` | 移動前 | 移動後 |
|---|---|---|
| `nil` | 3 行目 69 桁 | **4 行目**（= 次の論理行） |
| **`t`** | 3 行目 69 桁 | **3 行目 208 桁**（= 折り返した下の段） |

- 桁はピクセル単位（`temporary-goal-column`）で保持されるので、全角混じりでも
  真下の文字に落ちる
- `C-a` / `C-e` / `C-k` まで視覚行単位にしたいときは `visual-line-mode`
- 論理行で動きたいときは `next-logical-line` / `previous-logical-line`
- **検証は GUI で、バッファをウィンドウに出して行う。** `next-line` は
  `line-move-visual` が非 nil のとき `vertical-motion` を使うので、
  表示されていないバッファでは折り返しが再現できない

## `C-e` はウィンドウの右端で止まる（`my:end-of-visual-line`、2026-09-14）

`C-n` / `C-p` が視覚行単位なのに `C-e` だけ論理行の末尾（折り返した何段も下）へ
飛ぶのが噛み合わないので、`my-editor.el` で `C-a` の隣に置いた。
`visual-line-mode` は入れない（`word-wrap` や `C-k` まで変わる）。

**`truncate-lines` が t でも右端で止まる。** `end-of-visual-line` は
`(vertical-motion (cons (window-width) 0))` だけなので、折り返さないバッファでも
x = ウィンドウ幅で止まる。「折り返していなければ `move-end-of-line` と同じ」は
**誤り**。GUI 実測（ウィンドウ幅 144、603 桁の行の先頭から `C-e`）:

| 行 | `truncate-lines` | `move-end-of-line` | **`my:end-of-visual-line`** |
|---|---|---|---|
| 603 桁 | nil | 5 段目 603 桁 | **1 段目 143 桁** |
| 603 桁 | **t** | 603 桁 | **143 桁** |
| `abcEND` | nil / t | 6 桁 | **6 桁**（`eolp` が t） |

つまり**幅に収まる行では従来どおり**で、違いが出るのは幅を超える行だけ。
その代わり**長い行の論理的な末尾へは `C-e` では行けなくなる**（右端に着いた
状態でもう一度押しても同じ x なので動かない）。`M-x end-of-line` か、
折り返していれば `C-n` `C-e` で行く。行末まで消すのは `C-k`
（`kill-line` は `line-move-visual` を見ないので論理行の末尾まで消す。
右端から `C-k` を押すとその行の残り全部が消えることを実測）。

### 【重要】素の `end-of-visual-line` は折り返し位置で次の行の先頭に着く

`word-wrap` が nil（桁で折り返す）のバッファでは、`end-of-visual-line` の
着地点が**次の視覚行の先頭**になる。point としては行末なのに、**ブロック
カーソルは次の行の 1 桁目に描かれる**ので「右端に行っていない」ように見える。
`*claude(PROJ)*` も `word-wrap` は nil なのでこれを踏む。

GUI 実測（幅 144、空白区切りの長い 1 行の先頭から `C-e`。位置は
`posn-at-point` = 実際に描かれる桁）:

| `word-wrap` | point | `char-after` | 描画位置 |
|---|---|---|---|
| **nil**（素） | 145 | `2` | **row 1 col 0**（次の行の頭） |
| **nil**（`my:end-of-visual-line`） | 144 | `d` | **row 0 col 143**（右端の文字の上） |
| t（素・修正後とも） | 140 | 空白 | row 0 col 139（折り返しの空白の上） |

`word-wrap` が t なら折り返しに使った空白が row 0 の末尾にあり、素のままでも
同じ行に着くので**引き戻してはいけない**。判定は空白の有無ではなく
**「視覚行の先頭に着いたか」**（`beginning-of-visual-line` が動かない）で行う。

**`truncate-lines` が t のときは次の視覚行が無いので、この判定にかからない。**
素のままだと画面の 1 つ外側（col 144）に着く。そちらは「行末でなければ
1 文字戻す」で別に扱う。ただし**横スクロールは避けられない**。右端の文字に
載せても `hscroll-margin`（既定 5）と `hscroll-step`（既定 0 = 中央寄せ）で
`window-hscroll` が 0 → 71 になる（実測）。論理行末まで飛ぶよりはまし、という程度。

**桁は自分で数えないこと。** `display-line-numbers-mode` が有効だと行番号の
ぶん実際に使える幅が減る（実測: `current-column` は 139 なのに描画は col 143）。
`window-width` から引き算するのではなく、`end-of-visual-line` が着いた場所で
判断する。

修正後の実測（7 通り。いずれも 1 段目のまま、2 回続けて押しても動かない）:

| | 着地 | 描画桁 |
|---|---|---|
| 折り返し（`word-wrap` nil） | `x` の上 | 143 |
| 折り返し（`word-wrap` t） | 空白の上 | 139 |
| `truncate-lines` t | `x` の上 | 143（hscroll 71） |
| `display-line-numbers` | `x` の上 | 143（`current-column` は 139） |
| 全角（`日本語` の繰り返し） | `語` の上 | 142 |
| 幅に収まる行 | 行末（`eolp` が t） | 6 |
| 畳んだ org の見出し | 1 行目 14 桁 | — |

### 【重要】素の `end-of-visual-line` は畳んだ領域を飛び越える

org の畳んだ見出しでは改行ごと不可視になり、見出しと配下が **1 視覚行**になる。
そのため見出しの末尾ではなく**サブツリーの末尾**に着き、そこで打つと畳まれた
中身に紛れ込む。GUI 実測（`* 見出し :tag:` + 本文 2 行を `org-cycle` で畳んだ）:

| | 着地 |
|---|---|
| `org-end-of-line`（org の既定） | 1 行目 14 桁 `* 見出し :tag:` |
| **素の `end-of-visual-line`** | **3 行目 13 桁 `本文の 2 行目`** |
| **`my:end-of-visual-line`** | **1 行目 14 桁** |

**org の remap は当てにできない。** org は `move-end-of-line` を
`org-end-of-line` に remap して避けているが（`C-e` を直接は束縛していない）、
`C-e` を別のコマンドに張り替えると remap は経由しない。`C-a` を
`my:goto-line-beginning-or-indent` にしている時点で同じことが起きている
（あちらは後ろへ行かないので害が無いだけ）。

不可視テキストが無ければ視覚行が論理行を越えることはないので、
**論理行の末尾を越えたときだけ引き戻す**のが `my:end-of-visual-line`。
outline / hs-minor-mode / `my:org-fold-region` にも同じ理屈で効く。

