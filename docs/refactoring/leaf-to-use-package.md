<!-- -*- gfm -*- -->

# leaf → use-package の対応表

2026-08 に leaf から Emacs 同梱の use-package へ移行したときの対応表。移行は完了しているので、いま設定を書くのに要るのは CLAUDE.md の「use-package と straight」だけ。過去の設定（`docs/_archived/archive-init.org`）を読むときのために残してある。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

| leaf | use-package |
|---|---|
| `:straight t` | 同じ |
| `:custom (var . val)` | `:custom (var val)`。値の位置は式として評価されるのでバッククォートは不要 |
| `:custom-face (face . '(...))` | 使わない。`:init (custom-set-faces '(face (...)))` |
| `:bind (:foo-map ...)` | `:bind (:map foo-map ...)`。グローバル束縛は `:map` より前に置き、全体を 1 つのリストにまとめる |
| `:require t` | `:demand t` |
| `:require OTHER-FEATURE` | `:demand t` + `:config (require 'OTHER-FEATURE)` |
| `:leaf-defer nil` | 不要（名前を `emacs` にする） |
| `:hydra (name () ...)` | `:init (defhydra name () ...)`。leaf の `:hydra` は init 時にインライン展開されるので `:config` に置くと意味が変わる |
| `:advice (:around f fn)` | `:init (advice-add 'f :around #'fn)`。これも init 時インライン |
| `:global-minor-mode M` | `:config (M 1)` |
| `:diminish t` | `:diminish`（引数なしで `<name>-mode` が対象） |
| `:after a b` | `:after (a b)`。ただし use-package は条件が満たされると `require` するので、leaf と同じく読み込みたくないときは `:defer t` + `:init` |
| `:doc` / `:tag` / `:includes` | 無い。コメントに落とす |
| `:disabled t` | 同じ（両者とも完全な no-op） |

