<!-- -*- gfm -*- -->

# ponkore's Emacs settings

Emacs 31.1 向けの個人設定。**Windows 11 が主**で、macOS と Linux でも動く。
自分用なので、他人がそのまま使うことは想定していない。

| | |
|---|---|
| 触るときの規約・罠 | [CLAUDE.md](CLAUDE.md) |
| 設計の経緯と実測の記録 | [docs/](docs/README.md) |

以前は Org-mode のリテラルプログラミング（`my-config/init.org` を
`org-babel-load-file`）だったが、Org の恩恵が薄い割にコストが大きかったため
素の Emacs Lisp に戻し、Emacs 31.1 で新設された `user-lisp/` に機能分割してある。

---

## 新しいマシンでやること

### 1. これだけは要る

```sh
git clone git@github.com:ponkore/emacs.d.git ~/.emacs.d
```

1. **Windows のみ: `HOME` をユーザー環境変数として `C:\Users\<user>` に設定する。**
   未設定だと Windows の Emacs は `%APPDATA%` を `~` とみなし、Explorer や
   スタートメニューから起動したときに `init.el` が見つからない。
   **設定側で `(setenv "HOME" ...)` してはいけない**（`init.el` が読まれる時点で
   `.emacs.d` の探索は終わっており手遅れ。詳細は CLAUDE.md）

2. **起動する。** straight.el が自動でブートストラップし、全パッケージを
   clone / build する。初回は数分かかる

3. **`fonts/NFM.ttf`（Symbols Nerd Font Mono）を OS にインストールする。**
   リポジトリに置いてあるだけでは効かない

   | OS | |
   |---|---|
   | Windows | `%LOCALAPPDATA%\Microsoft\Windows\Fonts` へコピーし、`HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts` にレジストリ登録 |
   | macOS | `cp fonts/NFM.ttf ~/Library/Fonts/` だけ |

   入れないと dired のディレクトリアイコン（U+E6AD）が豆腐になる。
   インストール後に **Emacs の再起動が要る**

4. **本文フォント `HackGen` を入れる**（Windows 11.6pt / macOS 16pt）

5. **tree-sitter の文法をビルドする。** `M-x my:install-treesit-grammars` →
   再起動。**C コンパイラと git が要る**（Windows は scoop の `gcc`）。
   文法が無い環境では従来のメジャーモードにフォールバックするので、
   飛ばしても壊れはしない

### 2. 要る機能だけ

| 入れるもの | 何のため | 入れ方 |
|---|---|---|
| **LSP サーバ** | eglot | 下の[外部依存](#外部依存)の表 |
| **mupdf** | PDF をバッファで見る | `scoop install mupdf`（poppler では代用できない） |
| **Rust ツールチェイン** | magit の高速化 | `M-x my:gitd-build`（Windows のみ）→ [gitd/README.md](gitd/README.md) |
| **Go ツールチェイン** | 対話 TUI を動かす | `M-x my:pty-build`（Windows のみ） |
| **`~/Projects` の実体** | bookmark をマシン間で共有 | Windows はジャンクション（`C:\Projects` へ）、他は symlink か実ディレクトリ |
| **`~/.htnblog`** | はてなブログ投稿 | `userid` / `endpointurl` / `apikey` を書く |
| **cmigemo** | ローマ字で日本語 isearch | バイナリと辞書（`~/.emacs.d/migemo/utf-8/migemo-dict`）。無ければ黙って無効 |
| **exceldiff / MarkText** | dired からの起動 | PATH に置くだけ |

`gitd` / `ptyd` / tree-sitter 文法 / `straight/` は**すべて git 管理外**で、
マシンごとにビルドする。**バイナリが無ければその機能が無効になるだけ**で、
他の設定には影響しない。

---

## ディレクトリ

| | |
|---|---|
| `early-init.el` | `init.el` では手遅れになる設定だけ（GC 抑制、`package.el` 無効化、`user-lisp-auto-scrape` を nil、ちらつき回避） |
| `init.el` | ブートストラップと読み込み順の宣言のみ |
| `user-lisp/` | **設定本体。26 モジュール**（[下表](#user-lisp-のモジュール)） |
| `custom.el` | `customize` が自動生成する。手で書かない（[設計方針](#設計方針)） |
| `site-lisp/` | パッケージマネージャで入手できないローカルベンダの Emacs Lisp |
| `etc/` | 文字を目で選ぶための一覧（emoji / Nerd Font）と、その生成スクリプト |
| `fonts/` | `NFM.ttf` のみ。OS 側へのインストールが別途要る |
| `snippets/` | yasnippet |
| `gitd/` | magit の git 実行を肩代わりする常駐プロセス（Rust）。`target/` は git 管理外 |
| `ptyd/` | ConPTY を持って対話 TUI を動かすプロセス（Go）。`ptyd.exe` は git 管理外 |
| `docs/` | 設計メモ・実測の記録（[索引](docs/README.md)） |
| `tmp/` | 作業用の捨て場。`.gitkeep` 以外は git 管理外 |

## 読み込み順

`early-init.el` → `init.el` の順で、`init.el` の中は次の 7 段。

1. straight.el のブートストラップ
2. 組み込みを使うパッケージの宣言（`org` / `transient` を `:type built-in`）
3. use-package の初期化（**キーワード順と挙動を 3 点調整している**。CLAUDE.md 参照）
4. `site-lisp/` を `load-path` に追加
5. `(prepare-user-lisp t)` — **バイトコンパイルはしない**（[根拠](docs/refactoring/no-bytecompile.md)）
6. `custom.el` の読み込み
7. `user-lisp/` 各モジュールの `require`

**`custom.el` より `user-lisp/` のほうが後**なので、同じ変数を両方で設定すると
`user-lisp/` 側が勝つ。モジュールを追加したら `init.el` の `require` 列に加える。

## `user-lisp/` のモジュール

| モジュール | 内容 |
|---|---|
| `my-core` | 汎用ヘルパ（`my:pandoc-data-file`、`my:open-file-externally`、`my:treesit-remap`）、`s` |
| `my-japanese` | 文字コード、cp932/UTF-8 変換テーブル、Windows IME（tr-ime）、migemo |
| `my-appearance` | フォント、フレーム、modus-vivendi テーマ、doom-modeline、nerd-icons |
| `my-completion` | vertico、consult、marginalia、orderless、corfu、cape、embark |
| `my-keybind` | グローバルキーバインド |
| `my-editor` | hydra、symbol-overlay、smartparens、whitespace、yasnippet、recentf、editorconfig、tab-bar ほか |
| `my-dired` | dired、hydra-dired、dired-sidebar（`F8`）、dired-x の上書き対策、exceldiff / MarkText の起動 |
| `my-dired-watch` | ファイルのサイズ・日時・属性の変化で dired の行を貼り替える。Windows のみ |
| `my-text` | text-mode、org-mode、ox-pandoc、markdown、rst、adoc |
| `my-lang-lisp` | Emacs Lisp、Clojure（cider）、Common Lisp（slime） |
| `my-lang-python` | Python（python-ts-mode、pyvenv、py-isort、blacken） |
| `my-lang-web` | PHP、JavaScript / TypeScript、web-mode、scss |
| `my-lang-native` | Rust、C++、C#、Go |
| `my-lang-misc` | SQL、bat、Swift、Lua、VisualBasic |
| `my-lsp` | eglot（組み込み、`C-c l`）、flymake（`C-c !`） |
| `my-fileformat` | yaml、diff、log4j、Dockerfile、vimrc、XML（`.csproj`） |
| `my-project` | projectile（`C-c p`） |
| `my-vc` | magit、diff-hl（`C-c g`）、Windows の SVN 対応 |
| `my-gitd` | magit の git 実行を常駐プロセスに肩代わりさせる。Windows のみ |
| `my-magit-watch` | ワークツリーを監視して magit バッファと dired の VC マークを自動更新。Windows のみ |
| `my-shell` | exec-path-from-shell、Windows 用 shell 設定 |
| `my-utils` | calendar（日本の祝日）、open-junk-file、grep/ripgrep、server |
| `my-claude` | Claude Code を stream-json で使う（`C-c a`） |
| `my-htnblog` | はてなブログ AtomPub API へ投稿する（`M-x htnblog`） |
| `my-pty` | ConPTY 経由で対話 TUI を動かす。Windows のみ |
| `my-platform` | Windows / macOS 固有設定 |

## 設計方針

| | |
|---|---|
| **パッケージ管理は straight.el に一本化** | `package.el` は `early-init.el` で無効化。組み込みを使うものは `:type built-in` を `init.el` で宣言する |
| **設定の記述は Emacs 同梱の use-package** | 2026-08 に leaf から移行。Emacs 本体に入っているので腐らない |
| **`user-lisp/` はバイトコンパイルしない** | 速くならず、失うものだけがある（[実測](docs/refactoring/no-bytecompile.md)） |
| **`custom.el` は 4 変数だけ** | 設定は `user-lisp/` に置く。`custom.el` に書いても負ける |
| **メジャーモードは tree-sitter 版** | ただし文法が実際に使えるときだけ `my:treesit-remap` で差し替える |
| **lexical-binding は全ファイル `t`** | |
| **Windows で困ることは自前で迂回する** | プロセス生成が遅い → `gitd/`、PTY が無い → `ptyd/`、文字幅 → `site-lisp/eaw.el` |

## 機能

### 日本語環境

cp932 / UTF-8 の混在、Windows IME（tr-ime + w32-ime）、migemo。
`site-lisp/eaw.el` が East Asian Ambiguous を幅 2 にし（組み込みでは 1496 文字
足りない）、`site-lisp/cp5022x.el` が `cp51932` などの coding system を定義する
（Emacs 同梱の `cp51932.el` は翻訳テーブルだけで coding system が無い）。

Windows のプロセス起動と文字コードは踏みやすい。→ [docs/japanese/encoding.md](docs/japanese/encoding.md)

### 補完

vertico + consult + marginalia + orderless（ミニバッファ）、corfu + cape（バッファ内）、
embark。`C-x C-r` は recentf とブックマークの両方から選ぶ。

### 見た目

modus-vivendi（Emacs 31.1 同梱の 5.2.0）+ doom-modeline + nerd-icons。
本文は HackGen、アイコンは `Symbols Nerd Font Mono`。

### 言語サポート

eglot（組み込み）+ flymake。メジャーモードは tree-sitter 版。
言語ごとのサーバは[外部依存](#外部依存)の表。
→ [docs/lsp/eglot-and-flymake.md](docs/lsp/eglot-and-flymake.md) / [docs/languages/go.md](docs/languages/go.md)

### magit の高速化と自動更新

Windows では `magit-refresh-buffer` 1 回に **1.7 秒**かかっていた。遅いのは git
ではなく **Emacs のプロセス生成**（同じ `cmd.exe` を起こすのに PowerShell 20 ms
に対し `call-process` 59〜76 ms）。Rust の常駐プロセス（`gitd/`）に肩代わりさせ、
トークン方式のキャッシュと並列先読みを足して **50〜70 ms** になった。

あわせて `w32notify` でワークツリーを監視し、表示中の magit バッファを自動更新する。
→ [docs/magit/gitd-and-autorefresh.md](docs/magit/gitd-and-autorefresh.md)

### Claude Code を Emacs から使う

Windows の Emacs には PTY が無いので、双方向のストリーミング JSON を素のパイプで
駆動する。端末エミュレーションも常駐プロキシも要らない。セッションは
プロジェクトごとに持て、アカウント（`CLAUDE_CONFIG_DIR`）もセッションごとに選べる。
逐次表示、画像添付（`M-v`）、許可プロンプト、AskUserQuestion への応答、
markdown 装飾、ヘッダ行のステータス表示。
→ [docs/claude/my-claude.md](docs/claude/my-claude.md)

### 対話 TUI（`M-x my:pty-run` / `M-x my:claude-term`）

Go の `ptyd` が ConPTY を持って子プロセスを動かし、VT を stdio で Emacs に流す。
表示は `eat`。Windows のみ。
→ [docs/pty/my-pty.md](docs/pty/my-pty.md)

### dired の拡張

Excel ブックは OS の関連付けに渡す、`C-c x` で exceldiff、`C-c m` で MarkText、
サイドバー（`F8`）、外部の増減に追随する自動更新。**サイズ・日時・属性の変化**にも
追従する（`my-dired-watch`。行が増減しないので、その行だけを貼り替える）。
**VC マーク（diff-hl）も commit / stage に追従する**（ターミナルや Claude Code から
コミットした場合も含む）。
→ [docs/dired/dired-extensions.md](docs/dired/dired-extensions.md)

### org / markdown

アーカイブ先に `#YM` と書くと `YYYY-MM` に展開される。`#+FOLD_REGION:` で
バッファ内の任意の範囲を畳める。`M-v` でクリップボードの画像を `_assets/` に
保存して貼り、保存時に整合性を検査する。
→ [docs/text/org-extensions.md](docs/text/org-extensions.md)

### はてなブログへ投稿（`M-x htnblog`）

カテゴリー・タイトル・本文 1 行目をプリセットしたバッファを開き、`C-c C-c` で公開。
外部コマンドは要らない（AtomPub API を Basic 認証で叩くだけ）。
→ [docs/htnblog/my-htnblog.md](docs/htnblog/my-htnblog.md)

### その他

- **カレンダーに日本の祝日**（`japanese-holidays`。Emacs 本体には入っていない）
- **PDF をバッファでプレビュー**（`doc-view-mode` + mupdf。SVG で出るので拡大しても崩れない）
- **editorconfig**（Emacs 30 で本体入り）

---

## キーバインド

### グローバル

| キー | |
|---|---|
| `C-h` | `delete-backward-char` |
| `C-z` | `scroll-down-command`（画面を上へ） |
| `C-a` | 行頭 ⇄ インデント位置をトグル |
| `C-e` | **視覚行の末尾**（ウィンドウの右端）へ |
| `C-=` | `er/expand-region` |
| `%` | 対応する括弧へ（括弧の上でないときは `%` を挿入） |
| `C-c c` | `compile` |
| `C-x C-n` | `next-error` |
| `C-x l` | `consult-goto-line` |
| `C-x b` | `consult-buffer` |
| `C-x C-r` | recentf + ブックマークから開く |
| `C-x g` | `grep` |
| `C-x T` | `toggle-truncate-lines` |
| `C-x =` | バッファの行数 |
| `C-x !` / `C-x \|` | `shell-command` / `shell-command-on-region` |
| `C-x C-;` | 日時を挿入（`2026/09/14 12:34:56`） |
| `C-;` | 日付を挿入（`2026-09-14 (月)`） |
| `C-S-a` | `embark-act` |
| `C-c t` | ディレクトリを選んで**新しいタブ**で開く（タブ名に色付きの印） |
| `C-TAB` / `C-S-TAB` | タブ移動（`tab-next` / `tab-previous`。組み込み） |
| `ESC ?` | `apropos` |
| `<f2>` | フォントサイズの hydra |
| `<f8>` | dired サイドバーの開閉 |
| `<f12>` | 実装 ⇄ テストの切り替え（projectile） |

### プレフィクス

| | |
|---|---|
| `C-c a` | Claude Code（下記） |
| `C-c g` | diff-hl の hydra |
| `C-c l` | eglot（`r` rename / `a` code-actions / `f` format / `d` doc / `h` inlay hints / `R` reconnect / `q` shutdown） |
| `C-c !` | flymake（`n` / `p` 移動、`l` / `P` 一覧、`h` hydra） |
| `C-c p` | projectile（`s` は検索 dwim） |
| `C-x i` | yasnippet（`TAB` expand / `i` insert / `n` new / `v` visit / `l` tables / `g` reload） |
| `C-x t` | タブ（`0` 閉じる / `1` 他を閉じる / `2` 新規 / `RET` 選択 / `u` 戻す / `r` 改名。組み込み） |

### Claude Code（`C-c a`）

| キー | |
|---|---|
| `a` | セッションを開き、画面をレイアウトする（`C-u` で立て直す） |
| `l` | いつでも同じレイアウトに戻す |
| `e` | 環境（アカウント）を切り替える |
| `t` | ワークスペースを信頼済みにする |
| `c` / `r` | 直近の会話を継ぐ / 過去のセッションを一覧から選んで再開 |
| `m` | モデルを変える（会話は継続） |
| `i` / `s` | 入力エリアへ / リージョンを送る |
| `k` / `q` | 中断 / セッション終了 |
| `M` | MCP サーバを名前と状態で一覧する（`/mcp` は要約しか返さない） |

会話バッファ `*claude(PROJ)*` の中では `C-c C-c` 送信、`C-c C-k` 書きかけを捨てる、
`M-p` / `M-n` 入力履歴、`M-v` クリップボードの画像を添付、
`C-c C-p` / `C-c C-n` 前後の自分の発言へ移動（確定した会話の側では `p` / `n`）。

自分の発言は各行の行頭にオレンジの帯が付く。長い行は折り返した先にも付くので、
遡って探すときの目印になる。

### dired

| キー | |
|---|---|
| `.` | hydra |
| `RET` / `f` / `e` | 開く（Excel なら OS の関連付けへ） |
| `TAB` / `S-TAB` | サブツリーの開閉 / 巡回 |
| `V` | `dired-vc-status` |
| `G` | ripgrep |
| `C-c w` | git 相対パスを kill-ring へ |
| `C-c e` | エクスプローラー / Finder で開く |
| `C-c x v` / `C-c x d` | exceldiff（リビジョン比較 / マークした 2 つ） |
| `C-c m` | MarkText で開く |

### org / markdown

| キー | |
|---|---|
| `C-c C-x h` | `#+FOLD_REGION:` の範囲を開閉（org） |
| `M-v` | クリップボードの画像を貼る（org） |
| `C-c .` | markdown の hydra |
| `C-c C-c p` / `C-c C-c o` | ブラウザでプレビュー / 外部エディタで開く |

### Go

`C-c C-l` で golangci-lint、`C-c C-t t` / `f` / `p` でテスト（`go-ts-mode` 組み込み）。

---

## 外部依存

git 管理外なので、マシンごとに入れる。

### 言語サーバ（eglot が使う。2026-08 時点）

| 言語 | サーバ | 入れ方 |
|---|---|---|
| TypeScript / JS | typescript-language-server 5.3.0 + **typescript 5.9.3** | `npm i -g typescript@5 typescript-language-server` |
| PHP | intelephense 1.18.5 | `npm i -g intelephense` |
| bash | bash-language-server 5.6.0 | `npm i -g bash-language-server` |
| Rust | rust-analyzer 1.97.1 | `rustup component add rust-analyzer` |
| Python | basedpyright 1.39.10 | `uv tool install basedpyright` |
| Go | gopls 0.23.0 | `go install golang.org/x/tools/gopls@latest` |

**TypeScript は 5.x に固定すること。** `npm i -g typescript` で入る 7.x
（Go 実装のネイティブ版）には `lib/tsserver.js` が無く、初期化に失敗する。

**npm グローバルは nvm のバージョンに紐づく。** Node を切り替えたら入れ直しが要る。

### tree-sitter の文法（`M-x my:install-treesit-grammars`）

bash / c-sharp / css / dockerfile / go / gomod / gowork / html / javascript /
jsdoc / json / python / rust / toml / tsx / typescript / yaml の 17 個。
コンパイラは scoop の `gcc`（mingw-w64）。

### その他のツール

| | |
|---|---|
| mupdf | `scoop install mupdf`。PDF プレビュー（`mutool`） |
| Rust toolchain | `gitd/` のビルド |
| Go toolchain | `ptyd/` のビルド、gopls |
| pandoc | markdown / org のエクスポート |
| ripgrep | `G` / `C-c p s` |
| Git 付属の `bash.exe` | Windows の `shell-file-name`（あるときだけ設定） |
| golangci-lint | `C-c C-l` |

---

## メンテナンス

straight は自動更新しない。

```elisp
(straight-pull-recipe-repositories)  ; レシピ定義 (melpa 等) を更新
(straight-pull-package "NAME")       ; 個別パッケージを更新
(straight-prune-build)               ; 使われていない build/ を消す
(straight-remove-unused-repos t)     ; 参照されない repos/ を消す
```

- **パッケージ本体を更新したら `straight/build/NAME` を消してから起動する。**
  straight の変更検知は取りこぼす（corfu / doom-modeline で実績あり）
- **掃除は GUI で実行すること。** batch では `:if window-system` のパッケージが
  登録されず、使用中のものまで削除対象になる
- 手元と upstream の差を一括で見る手順は
  [docs/packages/straight-maintenance.md](docs/packages/straight-maintenance.md)

### 設定を変えたら

該当モジュールを編集して、Emacs を再起動するか編集した式を `C-M-x` で評価する。
GUI 依存の設定（フォント、doom-modeline、IME、`format-mode-line`）は
**batch では検証できない**。詳細は [CLAUDE.md](CLAUDE.md) の「検証」。
