<!-- -*- gfm -*- -->

# PDF をバッファ内でプレビューする

組み込みの `doc-view-mode` で見る。設定は 1 行も要らないが mupdf が要る（poppler は入っていても使われない）。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

組み込みの `doc-view-mode` で見る。**設定は 1 行も要らない**が、外部の変換
ツールが 1 つ要る（tree-sitter の文法や LSP サーバと同じ、マシンごとの手動
セットアップ）。

```
scoop install mupdf     # extras / 1.28.0。mutool.exe が入る
```

`.pdf` は既定の `auto-mode-alist` が `doc-view-mode-maybe` に送るので、
dired の `RET` でそのまま開く（`my:dired-external-open-regexp` は
`xls` 系だけなので邪魔しない）。

## 【重要】poppler は入っていても使われない

**`doc-view` が PDF → 画像に使えるのは Ghostscript か MuPDF だけ**
（`doc-view.el:203` の `doc-view-pdf->png-converter-function`）。
このマシンには scoop で poppler が入っていて `pdftoppm` / `pdftocairo` が
あるが、**doc-view はそれらを画像変換には使わない**。poppler で使われるのは
`pdftotext`（`C-c C-t` のテキスト表示）だけ。

導入前の状態（実測）:

| | |
|---|---|
| `(doc-view-mode-p 'pdf)` | **nil** |
| `doc-view-ghostscript-program` | **nil**（`gswin64c` も `gswin32c` も無い） |
| `doc-view-pdfdraw-program` | `"mudraw"`（見つからないときの既定） |

この状態で PDF を開くと変換されず、`doc-view-fallback-mode` に落ちる。
**「PDF が読めない」ときは、まず変換ツールの有無を疑うこと。**

## mupdf を選んだ理由

`doc-view-pdfdraw-program` の既定は `mutool` → `pdfdraw` → `mudraw` の順に
探す（`doc-view.el:188`）ので、`mutool` を置くだけで
`doc-view-pdf->png-converter-function` も自動で mupdf 側になる。
**Ghostscript でも動くが doc-view は mupdf を優先する。**

さらに Emacs 31.1 は **mupdf のときだけ PNG ではなく SVG で出す**
（`doc-view.el:2262`、`doc-view-mupdf-use-svg`）。拡大しても崩れない。

## 実測（サンプルは日本語ファイル名の PDF）

| | |
|---|---|
| `(doc-view-mode-p 'pdf)` | `c:/Users/masao/scoop/shims/mutool.exe` |
| `major-mode` | `doc-view-mode` |
| `doc-view-mupdf-use-svg` | **t** |
| 生成物 | `page-1.svg`（`doc-view--image-file-pattern` = `page-%s.svg`） |

**日本語ファイル名でも通る。** `default-process-coding-system` の cdr が
cp932 なので、`call-process` の引数が正しく渡る（別節）。

## 入れた直後は再起動が要る

`doc-view-pdfdraw-program` と `doc-view-pdf->png-converter-function` は
**`doc-view.el` のロード時に `executable-find` で決まる defcustom** なので、
doc-view を既に読んでいるセッションでは古い値のまま。再起動するか、
その場で 2 つを `setq` する。

## 他のマシンでは

`mutool` か `gs` を入れるだけ。既定のロジックが拾うので設定は要らない。

