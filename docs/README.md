<!-- -*- gfm -*- -->

# docs/ — 設計メモと実測の記録

`README.md` が「何があるか・どう入れるか」、`CLAUDE.md` が「触るときの規約」、
ここが **「なぜそうしたか」と「その根拠の数字」**。

CLAUDE.md の各節から、詳細はここへリンクしてある。逆にここを読むときは、
結論だけなら CLAUDE.md の該当節で足りる。

最終更新: 2026-09-15

## 機能ごとの詳細

| | |
|---|---|
| [claude/my-claude.md](claude/my-claude.md) | Claude Code を stream-json で駆動する。会話バッファ・ヘッダ行・許可プロンプト・画像添付・逐次表示 |
| [magit/gitd-and-autorefresh.md](magit/gitd-and-autorefresh.md) | magit の高速化（`gitd/`）と自動更新（`my-magit-watch`）。1.7 秒 → 50～70 ms |
| [pty/my-pty.md](pty/my-pty.md) | ConPTY（`ptyd/`）で対話 TUI を動かす。文字幅とフォントの桁揃え |
| [dired/dired-extensions.md](dired/dired-extensions.md) | 外部アプリ起動・exceldiff / MarkText・リネーム・自動更新・中身の変化への追従 |
| [text/org-extensions.md](text/org-extensions.md) | `#YM` アーカイブ・`#+FOLD_REGION:`・クリップボード画像と `_assets/` |
| [text/markdown.md](text/markdown.md) | `markdown-preview` と `markdown-open` は別経路 |
| [htnblog/my-htnblog.md](htnblog/my-htnblog.md) | はてなブログへ AtomPub で投稿する |
| [languages/go.md](languages/go.md) | Go。gopls 以外は同梱で足りる。外部 `go-mode` を入れない理由 |
| [languages/tree-sitter.md](languages/tree-sitter.md) | 文法があるときだけ `*-ts-mode` に差し替える仕掛け |
| [lsp/eglot-and-flymake.md](lsp/eglot-and-flymake.md) | eglot と flymake の罠（didOpen・ドライブレター・`trusted-content`） |
| [japanese/encoding.md](japanese/encoding.md) | Windows のプロセス起動と文字コード（cp932 / UTF-8） |
| [japanese/eaw-and-cp5022x.md](japanese/eaw-and-cp5022x.md) | `site-lisp/` の 2 つを残す根拠。組み込みでは足りない |
| [appearance/fonts-theme-modeline.md](appearance/fonts-theme-modeline.md) | Nerd Fonts の世代、全角/半角ピッチ、modus-themes 5.x、doom-modeline |
| [editing/completion.md](editing/completion.md) | vertico / consult / corfu。`C-x C-r` と capf の切り分け |
| [editing/editorconfig.md](editing/editorconfig.md) | 効き先は `.editorconfig` の置き場所で決まる |
| [editing/line-movement.md](editing/line-movement.md) | 視覚行の `C-n` / `C-p` / `C-e` |
| [packages/straight-maintenance.md](packages/straight-maintenance.md) | 棚卸し・掃除・更新の手順 |
| [misc/calendar-holidays.md](misc/calendar-holidays.md) | 日本の祝日（本体には入っていない） |
| [misc/pdf-preview.md](misc/pdf-preview.md) | `doc-view-mode` + mupdf |
| [misc/projects-junction.md](misc/projects-junction.md) | `~/Projects` と `directory-abbrev-alist` |

## 設計と計画（着手前・着手中の検討）

| | |
|---|---|
| [claude/emacs-claude-pty-proxy-study.md](claude/emacs-claude-pty-proxy-study.md) | PTY プロキシ方式の実現可能性検討（不採用の経緯） |
| [claude/emacs-claude-stream-json-plan.md](claude/emacs-claude-stream-json-plan.md) | 案 A（stream-json）の実装メモ |
| [claude/emacs-claude-improve-01.md](claude/emacs-claude-improve-01.md) | my-claude 改善 第 1 弾（作業ディレクトリ・レイアウト・整形・ステータス） |
| [magit/magit-auto-refresh-plan.md](magit/magit-auto-refresh-plan.md) | 全体計画（自動リフレッシュ + 常駐 git プロセス） |
| [magit/magit-autorefresh-stage1-design.md](magit/magit-autorefresh-stage1-design.md) | 段階 1: 自動リフレッシュ |
| [magit/magit-gitd-2a-design.md](magit/magit-gitd-2a-design.md) | 段階 2a: 素通しプロキシ |
| [magit/magit-gitd-2b-design.md](magit/magit-gitd-2b-design.md) | 段階 2b: キャッシュと並列先読み |

## リファクタリングの記録

| | |
|---|---|
| [refactoring/emacs-config-refactoring-plan.md](refactoring/emacs-config-refactoring-plan.md) | Org リテラルプログラミングから素の Emacs Lisp への移行計画 |
| [refactoring/phase0-baseline.md](refactoring/phase0-baseline.md) | 移行前のベースライン計測 |
| [refactoring/no-bytecompile.md](refactoring/no-bytecompile.md) | `user-lisp/` をバイトコンパイルしない根拠（2026-09-05 に測り直した） |
| [refactoring/leaf-to-use-package.md](refactoring/leaf-to-use-package.md) | leaf → use-package の対応表（移行は完了済み） |
| [hydra/hydra-memo.md](hydra/hydra-memo.md) | hydra 起動キーの棚卸し |

## 役目を終えたもの

`_archived/` は履歴として保存してあるだけで、現在の設定からは参照していない。

| | |
|---|---|
| `_archived/archive-init.org` | Org 方式だった頃の設定 |
| `_archived/extract.el` / `verify.el` / `split.py` / `verify-split.el` | Org からの抽出・分割に使った検証スクリプト |
| `_archived/snapshot.el` | 設定を読み込んだ Emacs の観測可能な状態をダンプする（等価性検証用） |

## 書き方の約束

- 冒頭に **1 行サマリ**と**最終更新日**、`README.md` / `CLAUDE.md` への戻りリンクを置く
- 実測は「**何を・どの環境で・いつ**測ったか」→ 表 → 結論、の順に書く。
  数字は時間が経つと嘘になるので、いつのものかを必ず添える
- ここに書いた結論のうち、**知らずに書くと壊れる**ものだけを CLAUDE.md へ上げる
