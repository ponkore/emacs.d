<!-- -*- gfm -*- -->

# editorconfig

Emacs 30 で本体入り。効き先は `.editorconfig` の置き場所で決まる。`csharp-ts-mode` に `indent_size` が届かない件と、BOM 付きファイルで `end_of_line` が効かない件。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

Emacs 30 で本体に入ったので **`:straight` は付けない**（`org` / `transient` と
同じ扱い）。`my-editor.el` で `editorconfig-mode` を有効にしてある。

## 効くのは `.editorconfig` があるディレクトリだけ

**上へ辿って探す**ので、`.editorconfig` の置き場所がそのまま効き先になる。
`root = true` があればそこで打ち切る。実測（`editorconfig-core-get-properties-hash`）:

| ファイル | プロパティ |
|---|---|
| `RINSETSU/AdjacentAreaTool/AdjacentAreaTool/Controls/ControlBase.cs` | **180** |
| `RINSETSU/AdjacentAreaTool/AdjacentAreaTool.Tests/**/*.cs` | **180** |
| `RINSETSU/AdjacentAreaTool/AdjacentAreaTool.sln` | 0（`[*]` に該当しても core が無い） |
| `~/.emacs.d/user-lisp/*.el` | 0 |

**置き場所がプロジェクト直下だと兄弟プロジェクトに届かない。** 当初
`AdjacentAreaTool/AdjacentAreaTool/` にあり、`.cs` 140 個のうち 77 個しか
対象になっていなかった。2026-09-14 にソリューション直下
（`AdjacentAreaTool/`）へ移して 3 プロジェクトとも対象になっている。

手元にある他の `.editorconfig`（有効にすると一緒に効く）は
`straight/repos/php-mode` に 3 個、LSP の `node_modules` に 4 個、
`~/Projects` 側は `node_modules` 多数と `gh/marktext` / `pmap-web`。
**他所のリポジトリのものを尊重する形になる**ので、それでよいかは方針の問題。

## Emacs 30 以降はフックを 2 つ足すだけ

| フック | 役割 |
|---|---|
| `hack-dir-local-get-variables-functions` | **ディレクトリローカル変数として**適用する。`add-hook` の末尾に足すので **`.dir-locals.el` のほうが優先** |
| `auto-coding-functions` | `end_of_line` / `charset` |

29 以前の `find-file-noselect` への advice はこの経路では使わない。
dir-local はメジャーモードのフックより**後**に適用されるので、
`my:*-mode-setup` が `setq` したものには勝つ。

読むのは**コアプロパティだけ**。`.NET` の `dotnet_*` / `csharp_*`（Roslyn 用。
あのファイルでは 276 行中ほとんど）は読み飛ばされる。`unset` の値は捨てられる。

変数は普通の dir-local として通るので `safe-local-variable` の対象になるが、
効くもの（`tab-width` `indent-tabs-mode` `csharp-ts-indent-offset`
`require-final-newline`）はすべて安全と宣言済みで、確認は出ない（実測）。

## 【重要】`indent_size` は csharp-ts-mode には届かない（対処済み）

Emacs 31 では **`csharp-ts-mode` が `csharp-mode` の派生**
（`derived-mode-all-parents` は `(csharp-ts-mode csharp-mode prog-mode)`）なので、
`editorconfig-indentation-alist` の `(csharp-mode c-basic-offset)` に当たり、
**ts 版が見ない `c-basic-offset`** に入って何も変わらない。

Emacs 31 は `editorconfig-indent-size-vars` を**モード側がバッファローカルに
設定する**前提で、`js.el`（`js-indent-level`）と `c-ts-mode.el`
（`c-ts-indent-offset`）はやっているが `csharp-mode.el` はまだ。
`my:csharp-ts-mode-setup` で 1 行足してある。

| `csharp-ts-mode` で | 設定される変数 |
|---|---|
| そのまま | `c-basic-offset` = 4 |
| `(setq-local editorconfig-indent-size-vars '(csharp-ts-indent-offset))` | **`csharp-ts-indent-offset` = 4** |

GUI で実ファイルを開いた実測:

| | `ControlBase.cs`（対象） | `CsvOutputPathTests.cs`（対象外） |
|---|---|---|
| `tab-width` | **4**（バッファローカル） | 8（既定） |
| `csharp-ts-indent-offset` | **4**（バッファローカル） | 4（グローバル値のまま） |
| `c-basic-offset` | `set-from-style`（**触られない**） | 同左 |
| `require-final-newline` | nil | nil |
| 文字コード | `utf-8-unix` | `utf-8-unix` |

**「設定されたか」はバッファローカルかどうかで見ること。** 値だけ見ると
どちらも 4 で区別が付かない。

## 【重要】`end_of_line` は BOM 付きファイルには効かない

`editorconfig--get-coding-system` は `auto-coding-functions` に載るが、
`find-auto-coding` は**先に `auto-coding-regexp-alist`（BOM の判定）を見て
そこで決まればそちらを採る**。VS が書くファイルは BOM 付きなので、
`end_of_line = lf` と書いてあっても改行はディスクの実態のまま。

逆に **BOM 無しで CRLF のファイルは `^M` がバッファに残る。**
`undecided-unix` で復号するため CR が文字として見えるようになる。
壊れてはいない（`buffer-modified-p` は nil、保存しても同じバイト列に戻る）が、
`[*] end_of_line = lf` と実ファイルが食い違っていることが目に見える形になる。

GUI 実測（`RINSETSU/AdjacentAreaTool` の `.editorconfig` は `[*]` に
`end_of_line = lf`）:

| ファイル | ディスク | BOM | バッファの coding | `^M` |
|---|---|---|---|---|
| `AdjacentAreaTool.sln` | CRLF | あり | `utf-8-with-signature-dos` | 0 |
| `AdjacentAreaTool/AdjacentAreaTool.csproj` | LF | あり | `utf-8-with-signature-unix` | 0 |
| **`AdjacentAreaTool.Tests/…Tests.csproj`** | **CRLF** | **無し** | **`utf-8-unix`** | **37** |
| `Controls/ControlBase.cs` | LF | 無し | `utf-8-unix` | 0 |

## 範囲を絞る defcustom は同梱版には無い

MELPA 版にあった `editorconfig-exclude-regexps` / `-exclude-modes` は本体に
入るときに落ちた。残る defcustom は `editorconfig-indentation-alist` /
`editorconfig-trim-whitespaces-mode` / `editorconfig-mode-hook` の 3 つだけ。
パスで絞りたいなら `editorconfig-mode` を使わず、
`hack-dir-local-get-variables-functions` に自前のラッパを載せることになる。

## コスト

| | |
|---|---|
| ロード（`:demand t`） | **9〜25 ms**（ファイルキャッシュが冷えていると 80 ms） |
| `.editorconfig` を引く（該当あり / 無し） | **0.13 ms / 0.087 ms** |

ハンドルのキャッシュがあるので、ファイルを開くたびのコストは無視できる。

**`:custom` に `editorconfig-mode` を書いてはいけない。**
`customize-set-variable` はパッケージ未ロードだと変数に `t` を入れるだけで
モード関数を呼ばない（corfu で踏んだのと同じ罠）。`:demand t` + `:config`。

