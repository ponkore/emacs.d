<!-- -*- gfm -*- -->

# markdown のプレビューと外部エディタ

よく似ているが**別経路**の 2 つ。`.md` そのものを渡したい相手（MarkText / Typora）は `markdown-open` のほう。文字コードの話は [japanese/encoding.md](../japanese/encoding.md) に分けてある。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

似ているが**別経路**の 2 つがある。

| | コマンド | 経路 |
|---|---|---|
| ブラウザで HTML を見る | `markdown-preview`（`C-c C-c p`、hydra の `v`） | `markdown-command`（pandoc）で HTML に変換 → `*markdown-output*` → `browse-url-of-buffer` が一時ファイルに書き出して OS 既定ブラウザで開く |
| 外部エディタで開く | `markdown-open`（`C-c C-c o`、hydra の `O`） | `save-buffer` してから `call-process` で `markdown-open-command` に**元の `.md` のパスを渡す**だけ。pandoc も browse-url も通らない |

`.md` そのものを渡したい相手（MarkText、Typora）は後者。**新しいコマンドを
作る必要は無い。**

`markdown-open-command` は MarkText を先頭に置いてある。

```elisp
(or (executable-find "marktext") ...Typora のパス候補...)
;; => "c:/Users/masao/.local/bin/marktext.cmd"
```

- `executable-find` は Windows では `exec-suffixes`（`.exe` `.com` `.bat`
  `.cmd` `.btm`）を補うので、拡張子なしの `"marktext"` で `.cmd` が見つかる。
  `~/.local/bin` は `exec-path` に入っている
- `marktext.cmd` は `start` で起動して即座に戻るため、`markdown-open` の
  同期 `call-process` でも Emacs は固まらない。**戻らないラッパを
  `markdown-open-command` にすると固まる**
- 以前は Typora のインストールパスを `seq-find` で探すだけだったが、この
  マシンに Typora は無いので結果は `nil` で、`markdown-open` は
  `Variable markdown-open-command must be set` で常に失敗していた

