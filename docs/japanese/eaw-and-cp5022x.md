<!-- -*- gfm -*- -->

# East Asian Ambiguous 幅と機種依存文字の coding system（`site-lisp/`）

`eaw.el` と `cp5022x.el` をどちらも残す根拠。組み込みでは足りないことの実測と、MELPA 版に乗り換えない理由。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## East Asian Ambiguous 幅 (site-lisp/eaw.el)

`site-lisp/eaw.el` は残す。Emacs 31 の組み込み処理では足りないため。

Emacs 31 は `ambiguous-width-chars` を持ち、`cjk-ambiguous-chars-are-wide`
が t なら `use-cjk-char-width-table` がそれを幅 2 にする。日本語環境に
すると自動で適用されるので、組み込みだけでもある程度は効く。

HackGen で実測した結果（GUI）:

| | 文字数 |
|---|---|
| eaw が挙げる ambiguous 文字 | 3666 |
| 組み込みだけで幅 2 になるもの | 2170 |
| **eaw が追加で幅 2 にするもの** | **1496** |

その 1496 文字を実際に描画して測ると:

| 実測幅 | 文字数 | |
|---|---|---|
| 16px（全角） | 335 | eaw が正しい |
| 8px（半角） | 63 | 組み込みが正しい |
| それ以外 | 1098 | 絵文字・麻雀牌など。プロポーショナルなフォールバックで描かれ、`char-width` をどちらにしても桁は揃わない |

桁揃えが成立する 398 文字のうち **84% で eaw のほうが実描画と一致する**。
`○△□★※①→≒` のような日常的な記号は組み込みでも幅 2 になるので、
差が出るのは記号類が中心。

**計測は必ず GUI で行うこと。** Windows の batch では `initial-window-system`
が nil のため `use-cjk-char-width-table` が ambiguous を幅 1 に倒す分岐に入り、
組み込みのカバー範囲を過小評価する（2170 ではなく 1424 に見える）。

## 機種依存文字の coding system (site-lisp/cp5022x.el)

`site-lisp/cp5022x.el` も残す。eaw と同じく、**組み込みでは足りない**ため。

Emacs 31.1 は `lisp/international/cp51932.el` を同梱しているが、あれは
**翻訳テーブルだけ**で coding system の定義は無い（`lisp/` 全体を grep して
`define-coding-system 'cp51932` が 1 件も無いことを確認済み）。

```elisp
(define-translation-table 'cp51932-decode map)
(define-translation-table 'cp51932-encode map)
(provide 'cp51932)
```

`cp5022x.el` は**そのテーブルを使って** `cp50220` / `cp50221` / `cp50222` /
`cp51932` を `define-coding-system` する側なので、組み込みでは置き換えられない。

`my-japanese.el` が `(define-coding-system-alias 'euc-jp 'cp51932)` と
`set-coding-system-priority` でこれらを使っている。

### 【重要】translation-table を `utf-8` に貼ると UTF-8 が非可逆になる（2026-10-25）

`CLAUDE.md` を Emacs で保存したら、`1〜3 行` / `59〜76 ms` など **5 箇所の
`〜`（U+301C WAVE DASH）が `～`（U+FF5E FULLWIDTH TILDE）に書き換わっていた**。
文字数は前後とも 46,503 で、変わったのはこの 1 種類だけ。

原因は `my-japanese.el` が `japanese-ucs-jis-to-cp932-map` と自前の
1 組テーブル（`#x301c → #xff5e`）を **`utf-8` にも貼っていた**こと。
稼働中の Emacs で実測。

| | 結果 |
|---|---|
| UTF-8 の `E3 80 9C`（`〜`）を decode | **`#xff5e`** |
| `#x301c` を utf-8 で encode | **`ef bd 9e`** |

**decode 側でも変換されるので、バッファ上は最初から `～` に見える。**
ファイルを開いて保存するだけで中身が変わり、気づけるのは `git diff` だけ。
リポジトリの実情もこれと一致していて、`〜` が残っているのは Claude Code が
直接バイトで書いたファイル（`docs/` 以下に 100 箇所超）、`～` になっているのは
Emacs で保存したファイル（`docs/README.md` / `gitd/README.md`）だった。

#### 対応表は 8 組ある（`utf-8` を外すだけでは直らない）

`japanese-ucs-jis-to-cp932-map` を `map-char-table` で展開した実測値。

```
#xa2->#xffe0  ¢→￠    #xa3->#xffe1  £→￡    #xa6->#xffe4  ¦→￤
#xac->#xffe2  ¬→￢    #x2014->#x2015 —→―    #x2016->#x2225 ‖→∥
#x2212->#xff0d −→－    #x301c->#xff5e 〜→～
```

`utf-8` には**この 8 組と 1 組テーブルが二重に貼られていた**。後から貼った
1 組が勝っていたので、実際に効いていたのは `〜`→`～` だけ（`−` と `‖` は
素通しだと実測で確認）。したがって**1 組テーブルの `mapc` から `utf-8` を
外すだけでは、8 組の方が復活して悪化する**。`〜`→`～` は直らないまま、
`CLAUDE.md` の `—`（12 箇所）が `―` に化ける。**decode 側の
`coding-system-put` も消すこと。**

#### `coding-system-put` は後から貼った方が勝つ（消す手段が無い）

同じ coding system に 2 回貼ると前の指定が黙って潰れる。これで
**`cp932` 側にも副作用が出ていた**（1 組テーブルが 8 組を潰していた）。

| | 修正前 | 修正後 |
|---|---|---|
| `U+301C` を cp932 で encode | `81 60` | `81 60` |
| `U+2212`（`−`）を cp932 で encode | **`20`（空白に落ちる）** | `81 7c` |

また **モジュールを再評価しても外れない**。再起動するか、明示的に nil を貼る。

```elisp
(coding-system-put 'utf-8 :decode-translation-table nil)
(coding-system-put 'utf-8 :encode-translation-table nil)
```

#### 貼る先（修正後）

| coding system | decode | encode |
|---|---|---|
| `euc-jp`（= `cp51932`） / `iso-2022-jp` | jis→cp932 の 8 組 | cp932→jis の 8 組 |
| `cp932` | （貼らない。cp932 は `0x8160` を元から `U+FF5E` に decode する） | jis→cp932 の 8 組 |
| `utf-16le`（Windows のクリップボード） | `〜`→`～` の 1 組 | 同じ |
| **`utf-8`** | **貼らない** | **貼らない** |

クリップボードを素通しにするかは別の判断。`utf-16le` を残してあるので、
Emacs からコピーした `〜` は他アプリへ `～` として渡る。

#### 背景: Shift_JIS ⇔ Unicode の対応表が 2 系統ある

JIS X 0208 の 1 区 41 点（Shift_JIS `0x8160`）を Unicode のどこに当てるかで
流儀が分かれた。この食い違いが上の 8 組（`¦` と `—` を除く 6 組が
Shift_JIS 由来）の正体で、Emacs が両方向の対応表を同梱しているのもそのため。

| | `0x8160` | 他の組 |
|---|---|---|
| JIS 規格票 / Unicode コンソーシアムの対応表（Apple の変換もこちら） | `U+301C` `〜` | `U+2212` `−` / `U+2016` `‖` / `U+00A2` `¢` / `U+00A3` `£` / `U+00AC` `¬` |
| **Microsoft CP932** | `U+FF5E` `～` | `U+FF0D` `－` / `U+2225` `∥` / `U+FFE0` `￠` / `U+FFE1` `￡` / `U+FFE2` `￢` |

Windows で作った UTF-8 には `FF5E`、JIS 準拠の変換を通ったものには `301C` が
入るので、片方しか知らない相手を通すと「`〜` が化ける / `?` になる /
検索に引っかからない」が起きる。メール・CSV・DB（Oracle の `JA16SJIS`）・
ファイル名で有名になった件。U+301C のグリフが Unicode 規格票では波の向きが
JIS と逆に描かれていた（後に修正）ことも、見た目で区別できない一因。

**Windows 式に寄せる方針自体は妥当**だが、寄せるのは cp932 に出入りする
ときだけで足りる。UTF-8 同士の往復にまで張ると、保存するたびに中身が
変わるものになる。

### public repo はあるが、乗り換える利点が無い（2026-09-09 に調査）

MELPA の `cp5022x`（[awasira/cp5022x.el](https://github.com/awasira/cp5022x.el)）が
それで、straight のレシピキャッシュにも既に入っている。

```
straight/repos/melpa/recipes/cp5022x
  (cp5022x :repo "awasira/cp5022x.el" :fetcher github)
```

**中身はバイト単位で同一。** raw を落として diff を取ると、差分は
`;;; cp5022x.el --- …  -*- lexical-binding: nil -*-` の cookie 1 行だけで、
これは `693f6d1` でこちらが足したもの。156 行とも一致する。

| | |
|---|---|
| upstream の最新コミット | **2012-03-23**（コミット総数 1） |
| MELPA のバージョン | `20120323.2335` |
| fork 3 つ | `emacsmirror` / `hrs-allbsd` / `yasuhirokimura`（FreeBSD ports 用）。**master の中身は 3 つとも同一** |

つまり「より新しい / 保守されている代替」は無く、どこから取っても同じファイル。

乗り換えると **straight がバイトコンパイルするので警告が増える**（実測）。

```
cp5022x.el:1:1: Warning: file has no `lexical-binding' directive on its first line
```

ファイルはトップレベルの `define-translation-table` / `define-coding-system`
だけでクロージャを作らないため dynamic binding でも動作は変わらないが、
**得るのは site-lisp の管理対象が 1 つ減ることだけで、失うのは cookie**。
現状維持とした。**再提案しないこと。**

なお **Emacs 本体に取り込まれたかどうかは `(featurep 'cp5022x)` では分からない**
（site-lisp 側が必ず先に provide する）。`emacs -Q` で見ること。

```sh
emacs -Q --batch --eval '(message "%S %S" (coding-system-p (quote cp51932)) (coding-system-p (quote cp50220)))'
# 31.1 では nil nil
```

