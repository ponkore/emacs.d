<!-- -*- gfm -*- -->

# `user-lisp/` をバイトコンパイルしない根拠

2026-09-05 に測り直した。壊れたのは straight のブートストラップ前だったからで、コンパイルしたからではない。それでもしないのは速くならないから。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

**壊れたのは「straight のブートストラップ前だから」であって、
「コンパイルしたから」ではなかった。** バイトコンパイラは autoload された
マクロなら自分でロードして展開するので、straight が autoload を生成した後に
コンパイルすれば `defhydra` も `doom-modeline-def-segment` も
`define-clojure-indent` も正しく展開される。

全モジュールを `require` し終えた状態（= `init.el` の末尾と同じ）で
`user-lisp/*.el` をコンパイルすると、残る警告は 22 件でその内訳は:

```
my-appearance.el   7  eglot-current-server, eglot--project-nickname, ...
my-dired.el        5  dired-view-file-other-window, svn-status, ...
my-lang-native.el  4  eglot-format-buffer, eglot-execute, ...
my-text.el         4  markdown-insert-reference-link-dwim, md2html, ...
my-magit-watch.el  1  magit--with-temp-process-buffer
my-shell.el        1  exec-path-from-shell-initialize
```

**すべて素の関数**（実行時に解決されるので無害）で、マクロは 1 件も無い。

それでもコンパイルしないのは、**速くならないから**。`.elc` を作って
3 回ずつ起動した実測:

| | 1 | 2 | 3 | 平均 |
|---|---|---|---|---|
| `.elc` あり | 0.993 | 1.017 | 0.969 | **0.993 s** |
| `.elc` なし | 1.008 | 0.996 | 0.980 | **0.995 s** |

差はノイズ以下。**このビルドは native-comp が使えない**
（`native-comp-available-p` が nil なので `prepare-user-lisp` の
`native-compile-async` の分岐は死んでいる）ため、コンパイルしても
byte-code 止まりで、起動の 1 秒はパッケージ本体のロードが大半を占める。

得るものが 0 ms である一方、失うものは具体的にある。

- `prepare-user-lisp` は `init.el:140`、`require` 列より**前**に走る。
  組み込みの経路でコンパイルすると結局マクロが壊れる。
  正しい `.elc` を得るには「全部ロードし終えてからコンパイル」が要るが、
  それは**そのセッションでたまたまロードされていたパッケージ次第で
  `.elc` の中身が変わる**ということでもある
- **`load-prefer-newer` は既定 nil。** 古い `.elc` があれば新しい `.el` より
  優先される。`prepare-user-lisp` は `byte-recompile-file file force 0` で
  タイムスタンプを見るので起動時には追随するが、`C-M-x` で評価しながら
  書く運用とは噛み合わない

`site-lisp/` も同じ。唯一測れる差がある `eaw.el`（253 KB）でも
ロードが 14.9 ms → 0.9 ms、起動全体の 1.4% でしかない。しかも
`site-lisp/` は `user-lisp-directory` の外なので `prepare-user-lisp` の
対象外で、`gitd/` / `ptyd/` と同じくマシンごとの手動ビルドが増える。

コンパイル時チェックが欲しいだけなら `.elc` を残す必要は無い。
lexical-binding の検証（後述）と同じく、一時ディレクトリにコピーして
コンパイルし `*Compile-Log*` を読めばよい。

