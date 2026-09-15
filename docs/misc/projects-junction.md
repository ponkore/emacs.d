<!-- -*- gfm -*- -->

# `~/Projects` のジャンクションと `directory-abbrev-alist`

`bookmarks` を git 管理下に置いて mac / Linux と共有するための仕込み。ジャンクションと alist は役割が違い、片方だけでは成立しない。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## `~/Projects` のジャンクションと `directory-abbrev-alist`（2026-09-09）

`bookmarks` を git 管理下に置いて mac / Linux と共有するための仕込み。
`C:\Users\masao\Projects` を `C:\Projects` へのジャンクションにしてあり、
`my-platform.el` が `directory-abbrev-alist` に 1 件足している。

**移植可能な省略形は `~` ただ 1 つ。** `bookmark-buffer-file-name`
（`bookmark.el:1212`）は保存時に必ず `abbreviate-file-name` を通しており、
コメントも「home はマシンごとに違うが `~/` なら届く」と書いている。
逆に言うと、home の外にある `c:/Projects/...` は**そのままでは共有できない**。

役割は 2 つに分かれる。**片方だけでは成立しない。**

| | 担当 |
|---|---|
| ジャンクション / symlink | `~/Projects` を実在させる = **読む側**（展開） |
| `directory-abbrev-alist` | `c:/Projects/...` と書かせない = **書く側**（省略） |

`abbreviate-file-name` だけがこの変数を見る。**`expand-file-name` は見ない**
ので、alist は一方向にしか効かない。

TO に `~` を書いてはいけない（`files.el:59`）が、`~` を含む結果は得られる。
`abbreviate-file-name` が **alist を適用してから `~` 置換する**
（`files.el:2316` → 2329）ので、TO には home 配下の絶対パスを書けばよい。

### 【重要】FROM の末尾のスラッシュを省かない

`directory-abbrev-apply`（`files.el:87`）は FROM を素の正規表現として使い、
マッチ部分を TO で置き換えるだけ。境界を見ないので、実測でこうなる。

| FROM | `c:/Projects/ESC-Web/` | `c:/ProjectsOld/foo/` |
|---|---|---|
| `\`c:/Projects` | `~/Projects/ESC-Web/` | **`~/ProjectsOld/foo/`** |
| **`\`c:/Projects/`** | `~/Projects/ESC-Web/` | **`c:/ProjectsOld/foo/`** |

**存在しないパスができるのにエラーは出ない。** `abbreviated-home-dir` は
`directory-abbrev-make-regexp`（`files.el:71`）が `\(/\|\'\)` の境界を
付けるが、**手書きのエントリには付かない**。

末尾スラッシュ形の唯一の取りこぼしは、末尾スラッシュ無しの `c:/Projects`
そのもの（変換されない）。

**大文字のドライブレターは FROM が小文字でも当たる。**
`abbreviate-file-name` が `case-fold-search` を
`(file-name-case-insensitive-p filename)` に束縛しており（`files.el:2313`）、
Windows では `t` になるため。ドライブレターの大小に悩まされる他の箇所
（gopls の診断、Emacs 起動時の cwd）とは違い、ここだけは自動で吸収される。

### 効果範囲は bookmark より広い

docstring は「新しく訪れたファイルバッファの `default-directory` を設定する
ときに置換が行われる」と書いており、`abbreviate-file-name` を通る経路すべて
（bookmark / recentf / 表示）に効く。パスを文字列で突き合わせている箇所
（`my:claude--same-directory-p`、gitd が `expand-file-name` を要求する件）は
どれも展開してから比べているので壊れないはずだが、**未検証**。

### 他マシンでは alist は要らない

必要なのは `~/Projects` を実在させること（実ディレクトリか symlink）だけ。
**`~/Projects` が無いマシンでは、共有したブックマークは開けない。**

