<!-- -*- gfm -*- -->

# tree-sitter

メジャーモードは `*-ts-mode` を使う方針。文法が実際に使えるときだけ差し替える仕掛けと、`my:treesit-remap` の置き場所の罠。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

メジャーモードは tree-sitter 版（`*-ts-mode`）を使う方針。ただし文法は
共有ライブラリで別途ビルドが必要（**C コンパイラと git が要る**）。
文法が無い環境で `*-ts-mode` に切り替えると何も動かなくなるため、
**従来のモードを残したうえで、文法が実際に使えるときだけ差し替える**形にしてある。

- `my:treesit-remap MODE TS-MODE LANGUAGE`（`my-core.el`）が
  `major-mode-remap-alist` に登録する。文法が無ければ何もしない
- `my:install-treesit-grammars`（`M-x`）で `treesit-language-source-alist` の
  文法をまとめてビルドする。反映には再起動が必要
- **フォントロックやインデントの設定はモードごとに別物**。`csharp-mode` は
  cc-mode 派生（`c-set-offset`）、`csharp-ts-mode` は tree-sitter 派生
  （`csharp-ts-mode-indent-offset`）なので、セットアップ関数を分けてある
- `*-ts-mode` は従来モードのフックを継承しない。`:hook` は
  `((foo-mode-hook foo-ts-mode-hook) . func)` の形で両方に張ること

- **`my:treesit-remap` は必ずトップレベルで呼ぶこと**。`:config` は
  `(eval-after-load '<パッケージ名>)` に包まれるので、そこで差し替えても
  「その回に開いたバッファ」には間に合わない。さらに差し替えが効くと
  従来のモードはもうロードされないため、`:config` は二度と実行されない
- `.tsx` の `auto-mode-alist` 登録は **web-mode のブロックより後**に置くこと。
  `:mode` が先頭に積むので、前に置くと web-mode に負ける

導入済みの文法（`tree-sitter/`、git 管理外）:
bash / c-sharp / css / dockerfile / go / gomod / gowork / html /
javascript / jsdoc / json / python / rust / toml / tsx / typescript / yaml
の 17 個。
`jsdoc` は `js-ts-mode` がコメント解析に `treesit-ensure-installed` するので必要。

コンパイラは scoop の `gcc`（mingw-w64 15.2.0、`~/scoop/apps/gcc/current/bin`）。
Emacs は `cc` → `gcc` → `c99` の順に探すので `gcc` があれば足りる。

