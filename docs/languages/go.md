<!-- -*- gfm -*- -->

# Go

Emacs 31.1 は Go に必要なものをほぼ同梱している。外部から入れるのは gopls だけ。外部の `go-mode` を入れてはいけない理由。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

Emacs 31.1 は Go に必要なものをほぼ同梱している。**外部から入れるのは gopls だけ**。
設定は `my-lang-native.el`、gopls への設定は `my-lsp.el` の
`eglot-workspace-configuration` にある。

| 役割 | 使うもの | 備考 |
|---|---|---|
| メジャーモード | 組み込み `go-ts-mode` / `go-mod-ts-mode` / `go-work-ts-mode` | 外部 `go-mode` は入れない（後述） |
| LSP | gopls | eglot に既定エントリがある（設定不要で繋がる） |
| 整形 | gopls 内蔵の gofumpt（`:gofumpt t`） | gofumpt のバイナリは要らない |
| import 整理 | gopls の `source.organizeImports` | goimports のバイナリは要らない |
| 静的解析 | gopls 内蔵の staticcheck（`:staticcheck t`） | flymake（`C-c !`）に出る |
| 追加 lint | golangci-lint 2.13.2（scoop） | `C-c C-l` で `compile` |
| テスト実行 | `go-ts-mode` 組み込みの `C-c C-t t` / `f` / `p` | gotest.el 等は要らない |
| docstring 雛形 | `C-c C-d`（`go-ts-mode-docstring`） | 組み込み |

`go install` したものは `~/go/bin`（PATH 済み）に入る。
**nvm の npm グローバルと違い、Go のバージョンを変えても消えない**が、
`go install` はビルドし直しなので Go を上げたら入れ直しておくのが無難。

## 【重要】外部の `go-mode` を入れてはいけない

Rust / C# は「従来モードを残して `my:treesit-remap` で差し替える」形にしてあるが、
**Go でそれをやると tree-sitter 版に一生切り替わらない**。

`go-ts-mode.el` は autoload で自分の登録を済ませている。

```elisp
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode-maybe))
(add-to-list 'treesit-major-mode-remap-alist '(go-mode . go-ts-mode))
```

- `go-ts-mode-maybe` は文法があれば `go-ts-mode`、無ければ **`fundamental-mode`**
- `treesit-major-mode-remap-alist` が `major-mode-remap-alist` に反映されるのは
  **`treesit-enabled-modes` が非 nil のときだけ**（既定は `nil`）

つまり `go-mode` を `:mode "\\.go\\'"` で足すと、`auto-mode-alist` の先頭に
積まれて必ず `go-mode` が勝ち、remap も起きない。`.tsx` を web-mode より後に
置かねばならないのと同じ罠。

文法が無い環境への保険が要るなら、`go-mode` ではなく
**`treesit-enabled-modes` に `go-ts-mode` を入れる**。こうすると文法が無いときに
`treesit-ensure-installed` が導入を提案する（`treesit-auto-install-grammar` の既定は `ask`）。
このリポジトリでは `my:install-treesit-grammars` で入れる運用にしている。

なお `go-ts-mode.el` は `treesit-language-source-alist` に go / gomod / gowork を
`add-to-list` するが、`my-core.el` はその変数を `setq` で丸ごと上書きするので、
**`my-core.el` 側にも同じ内容を書いておかないと `my:install-treesit-grammars` から
見えない**。commit ハッシュまで一致させること。1 文字でも違うと `add-to-list` の
`equal` 判定をすり抜けて二重登録になり、2 回ビルドされる。

## 保存時は「import 整理 → 整形」の順で呼ぶ

`my:go-before-save`（`before-save-hook`）が 2 つを順に呼ぶ。
逆にすると、あとから足された import 行が整形されないまま残る。

**`eglot-code-actions` を対話的に呼んではいけない。**
INTERACTIVE 非 nil で呼ぶと `eglot--read-execute-code-action` に入り、該当が
0 件のとき `eglot--error` が飛ぶ。`before-save-hook` の中で飛ぶので
**import を整理する必要が無いファイルは保存できなくなる**。
`my:go-organize-imports` は非対話（INTERACTIVE nil）で候補リストを受け取り、
あるときだけ `eglot-execute` する形にしてある。

## インデントはタブ

gofmt がタブなので `go-ts-mode` は `indent-tabs-mode` を `t` にする。
`go-ts-indent-offset` は「タブ何個ぶんか」ではなく桁数なので、`tab-width` と
揃えないと継続行がずれる。既定の 8 は広いので両方 4 にしてある
（ファイルの中身はタブのままなので他のツールとは衝突しない）。

`whitespace-global-modes` に go 系は入っていないので、タブが強調されることはない。

## golangci-lint は flymake に載せない

モジュール全体を型検査するため 1 回が重く、`flymake-no-changes-timeout`（1.0 秒）で
回す用途には向かない。日常の指摘は gopls 内蔵の staticcheck で足りるので、
golangci-lint は `C-c C-l`（`my:go-golangci-lint`）で `go.mod` のあるディレクトリから
`golangci-lint run ./...` を `compile` する形にした。出力は
`main.go:10:5: S1002: ...` の形式なので、`compilation-error-regexp-alist` の
既定（gnu）でそのまま辿れる。

## 検証は GUI で、かつコマンドループを回すこと

`eglot-ensure` は **`post-command-hook` で接続する**。プローブ用の elisp を
`-l` で読ませて一気に実行すると、`find-file` しても永久に繋がらない
（実際に 90 秒待って TIMEOUT した）。`(run-hooks 'post-command-hook)` を
手で 1 回呼ぶ。flymake の診断も同様に `(flymake-start)` を明示的に呼ぶ。

