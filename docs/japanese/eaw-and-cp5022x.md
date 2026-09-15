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

