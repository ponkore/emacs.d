# フェーズ0 ベースライン記録

- 取得日: 2026-08-29
- ブランチ: `refactor/de-org-tangle`
- Emacs: GNU Emacs 31.1
- 取得方法: `emacs --batch --debug-init -l init.el`（GUI 依存ブロックは未実行）
- 生ログ: `tmp/baseline-batch.txt`

> 注: batch 実行は `history` / `recentf` を書き換えるため、事前バックアップ→事後リストアを実施済み。
> 以後の検証でも必ず同じ手順を踏むこと。

## 1. 起動結果

**init.el のロードはエラーなく完走する。** 致命的な起動失敗は現状なし。

## 2. 実測で確認できた不具合（runtime 値の probe 結果）

| 変数                      | 実測値                                                           | 期待値                    | 判定                                                          |
| ----------------------- | ------------------------------------------------------------- | ---------------------- | ----------------------------------------------------------- |
| `grep-command`          | `("c:/Program Files/Git/usr/bin/grep.exe -nH -r -e  ." . 49)` | 文字列                    | **バグ確定**（cons が入っている。`M-x grep` が壊れる）                       |
| `split-width-threshold` | `(global-configuraions-mode 150)`                             | `150`（数値 or nil）       | **バグ確定**（`:hook` に変数を書いたため、モード関数がリストに push された）             |
| `bavckup-inhibited`     | `t`                                                           | —                      | **タイポ確定**（`backup-inhibited` は `nil` のまま）                   |
| `inferior-lisp-program` | `" run"`                                                      | `"ros run"` 等          | **バグ確定**（`ros` 未インストールのため `(concat nil " run")` → `" run"`） |
| `before-save-hook`      | `(py-isort-before-save ...)`                                  | —                      | **グローバル汚染確定**                                               |
| `after-save-hook`       | `(delete-file-if-no-contents ...)`                            | —                      | **グローバル汚染確定**                                               |
| `completion-styles`     | `(orderless)`                                                 | `(orderless basic)` 推奨 | 要改善                                                         |

## 3. バイトコンパイル/ロード時の警告

| 警告                                                   | 件数                   | 対応                                                                                                   |
| ---------------------------------------------------- | -------------------- | ---------------------------------------------------------------------------------------------------- |
| `` `defadvice' is an obsolete macro (as of 30.1) ``  | **7**                | C-1                                                                                                  |
| `` `when-let' is an obsolete macro (as of 31.1) ``   | **1**（`grep-r` ブロック） | 新規発見 → C-12 として追加                                                                                    |
| `Missing 'lexical-binding' cookie`                   | **21 ファイル**          | 自前ファイル（`init.el`, `my-config/init.el`, `custom.el`, `site-lisp/eaw.el`, `site-lisp/cp5022x.el`）は対応必須 |
| `Package cl is deprecated`                           | 1                    | `site-lisp/ntcmd.el`, `site-lisp/smartchr.el`, `site-lisp/visual-basic-mode.el` が `(require 'cl)`    |
| `Package autoload is deprecated`                     | 1                    | 発生元未特定（要調査）                                                                                          |
| `Failed to enable theme(s): sanityinc-tomorrow-blue` | 1                    | `custom.el` の `custom-enabled-themes` が未ロードのテーマを参照                                                   |

## 4. パッケージマネージャの実態（実測）

- `leaf` の実体 → `straight/build/leaf/leaf.elc` （**4.5.5**、straight 側が勝っている）
- `elpa/leaf-20200706.2213/` （**4.2.7**、2020年）が残存し load-path 上に同居
- `leaf-keywords` は **elpa の 2020-04-28 版のみ** → leaf 4.5.5 との版ズレ
- `hydra` も `straight/build/hydra` と `elpa/hydra-20201115.1055` に二重存在
- `elpa/` に 2020〜2021 年で止まった残骸が多数（`init-loader`, `pastels-on-dark-theme`, `page-break-lines`, `ppp`, `el-get`, `dashboard`, `imenu-list`, `blackout`, `gnu-elpa-keyring-update` 等）

## 5. 環境条件（このマシン）

| 項目              | 状態                                      |
| --------------- | --------------------------------------- |
| `ros` (roswell) | **未インストール**                             |
| `elpy/rpc-venv` | 存在する                                    |
| `grep`          | `c:/Program Files/Git/usr/bin/grep.exe` |

## 6. 計画書からの訂正

`tmp/emacs-config-refactoring-plan.md` の記載を以下の通り訂正する。

| 項目                          | 訂正前             | 訂正後                                                                                                                                                   |
| --------------------------- | --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| A-1 (slime)                 | 「致命 / 起動が壊れる」   | **「高 / 起動は通るが値が壊れる」**。`(concat nil " run")` はエラーにならず `" run"` を返す（`nil` は空シーケンスとして受理される）ため、slime の起動が失敗するだけ                                           |
| A-2 (pyvenv)                | 「致命」            | **「高」**。このマシンには `elpy/rpc-venv` があるため未発現。新規マシン/他 OS でのみ顕在化                                                                                            |
| B-8 (whitespace)            | 「シンボルを代入していてバグ」 | **誤り。実測 `whitespace-style` は正しいリストになっている。** leaf の `:custom` は直前に定義した `whitespace-style-with-tab` の値を評価している。バグではないので対象外とする（ただし `defvar` されていない点は整理対象） |
| A-3 (split-width-threshold) | 「バグ」            | **確定**。実測値 `(global-configuraions-mode 150)`                                                                                                          |
| A-7 (grep-command)          | 「バグ」            | **確定**。実測値が cons                                                                                                                                      |
| —                           | —               | **C-12 を新規追加**: `when-let` が Emacs 31.1 で obsolete（`grep-r` ブロック）→ `when-let*`                                                                        |

## 7. 意思決定 B の調査結果（elpaca 採用可否）

ユーザー希望は B-3（elpaca）だが、以下の理由で **B-1（straight に統一）を推奨**する。

| #   | 事実                                                                                                                                                          | 出典                                                                |
| --- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| 1   | `leaf-keywords` がサポートするパッケージマネージャ用キーワードは `:straight` / `:el-get` / `:feather` のみ。**`:elpaca` は存在しない**                                                       | ローカルの `elpa/leaf-keywords-20200428.1803/leaf-keywords.el:110-111` |
| 2   | elpaca 公式が提供する統合は **use-package 向けのみ**。leaf.el への言及なし                                                                                                       | elpaca README                                                     |
| 3   | elpaca は**パッケージキューを init.el の読み込み後に非同期処理する**。leaf の `:custom` / `:config` / `:bind` は init 実行中に走るため、全 133 ブロックを `(elpaca pkg (leaf pkg ...))` の形に書き換える必要がある | elpaca README                                                     |
| 4   | Windows では**シンボリックリンク作成権限が必要**（または `elpaca-no-symlink-mode`）。主環境が Windows 11 なので追加の前提条件になる                                                                  | elpaca README                                                     |
| 5   | 意思決定 A-1（bootstrap 後に `prepare-user-lisp` を手動呼び出し）と組み合わせると、elpaca ではパッケージ未インストールの時点で `prepare-user-lisp` が走るため、さらに `elpaca-after-init-hook` へ追い出す必要がある      | 本調査                                                               |

**結論**: elpaca への移行は実質的に「leaf を捨てて use-package にする」決断とセットになる。
これは C-1（バグ修正と非推奨 API 置換のみ）の範囲を大きく超えるため、
**今回は B-1（straight に統一）で進め、elpaca は 2.9「今風への移行」フェーズで
「leaf → use-package + elpaca」として再検討する**のが合理的。

## 8. 動作確認チェックリストのベースライン

batch 実行で確認できた範囲:

- [x] `emacs --batch -l init.el` がエラーなく終了する
- [x] straight / leaf のブートストラップ成功
- [x] vertico / company / yasnippet / whitespace / recentf / savehist がロードされる
- [x] custom.el がロードされる（ただしテーマ有効化に失敗）
- [x] GUI 起動時の確認は未実施（フォント / doom-modeline / IME / org-bullets 等は `:if window-system` のため batch では未評価）

**GUI 側のチェックは、フェーズ1完了後にユーザーに実施いただく。**

→ 実施済
