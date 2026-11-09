# Ghostel — libghostty の端末を Emacs の dynamic module で動かす

`user-lisp/my-ghostel.el` の根拠。2026-11-09 に Windows で導入可否を実測した記録。

`my-pty`（`ptyd` + `eat`）との**比較評価のために並べて入れてある**。どちらを
残すかはまだ決めていない。

---

## 何なのか

[Ghostel](https://github.com/dakra/ghostel)（`dakra/ghostel`）は、Ghostty の
コアである **libghostty-vt** を Emacs の dynamic module に載せた端末
エミュレータ。2026-11-06 に libghostty-vt 1.0 が出て C ABI が安定した
ことで現実的になった系統の実装。

層の分担が `my-pty` とは違う。

| | `my-pty` | Ghostel |
|---|---|---|
| PTY | `ptyd.exe`（Go）が ConPTY を持つ | **module が自分で持つ**（`src/ConPtyProcess.zig`） |
| VT 解釈 | `eat`（Emacs Lisp） | libghostty-vt（C/Zig） |
| 描画 | eat | 専用 `Renderer.zig`（incremental） |
| プロセス | Emacs + ptyd の 2 つ | **Emacs だけ** |
| 対応 OS | Windows のみ | Windows / macOS / Linux / FreeBSD / Android |

### libghostty-vt 自体は PTY を持たない

1.0 に入っているのは VT パーサ・画面状態・scrollback・selection・search・
入力のエンコード・Kitty graphics のデコードまで。**描画も PTY も入っていない。**
ドキュメントの言い方は「you give it the bytes a program writes」で、バイト列の
供給元は「a pty, a network connection, a recording」と例示されるだけ。

つまり **libghostty を入れても ConPTY の問題は消えない。** Ghostel はそこを
自分で実装している（下記）。

---

## Windows での導入可否（実測）

### libghostty-vt のビルド済みバイナリは存在しない

記事と `PACKAGING-LIBGHOSTTY.md` は Windows の zip
（`ghostty-vt-1.0.0-x86_64-windows-gnu.zip`）に触れているが、**実際には
公開されていない。** リリースの `SHA256SUMS` を引いて確認した。

```
libghostty-vt-1.0.0.tar.gz        libghostty-vt-1.0.0-deps.tar.gz
ghostty-vt-1.0.0.xcframework.zip  ghostty-vt-1.0.0.wasm
ghostty-vt-1.0.0-small.wasm       libghostty-vt-1.0.0-docs.zip
```

Windows の zip は 404。インストールページの
「We don't publish prebuilt packages for Linux, FreeBSD or Windows」が現状。
**自分で libghostty を使うなら Zig 0.17 でソースからビルドする**ことになる
（scoop の zig は 0.17.1 で minor が一致する）。

Windows x86_64 は Tier 1（CI でビルドもテストも実行）なので、対応そのものは
確か。ヘッダは `<ghostty/vt.h>` 1 本。DLL と import library に major version が
付かないため「side-by-side installs are not supported on Windows」。

### Ghostel は自分でビルド済み module を配布している

**こちらはツールチェイン不要。** `ghostel-module-x86_64-windows.dll`
（2.38 MB）がリリースに置かれていて、`M-x ghostel` の初回に取ってくる。
libghostty も Ghostel の `build.zig.zon` が
`libghostty-vt-1.0.0.tar.gz` を取り込んで静的に同梱しているので、
**libghostty を別にビルドする必要がない。**

### DLL の検査結果

この Emacs は `x86_64-w64-mingw32`（MinGW、GNU ABI）。噛み合うかを
`objdump` で確かめた。

| | |
|---|---|
| 形式 | `pei-x86-64` / PE32+ |
| export | **`emacs_module_init` と `plugin_is_GPL_compatible` の 2 つだけ** |
| import | `api-ms-win-crt-*`（UCRT）、`KERNEL32.dll`、`ntdll.dll` **のみ** |

`libgcc_s` も `libwinpthread` も `msvcrt` も要求しない。**MSVC / MinGW の
どちらの Emacs でも DLL 地獄が起きない**作り。Emacs module は Emacs 本体に
リンクせず runtime ポインタを受け取るので、リンク時の依存も無い。

`emacs -Q --batch` での `module-load` も通った（`featurep 'ghostel-module` が t、
module version `0.62.0`、67 シンボル / 28 関数）。

### ConPTY の実装

`src/ConPtyProcess.zig` が POSIX 実装（`src/PosixPtyProcess.zig`。
`posix_openpt` / `fork` / `TIOCSCTTY`）とは完全に別ファイルで存在する。
`CreatePseudoConsole` → `InitializeProcThreadAttributeList` →
`UpdateProcThreadAttribute(PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE)` →
`CreateProcessW` という正規の経路。

**Microsoft 再配布版の ConPTY を同梱できる。** module の隣に `conpty.dll` が
あれば `LoadLibraryW` でそれを使い、無ければ `kernel32.dll` の inbox 版に
フォールバックする。`ghostel-download-module` が
`conpty.dll` と `x64/OpenConsole.exe` も一緒に置く（実測）。README は
inbox 版より latency と correctness が改善すると言っている。
**`ptyd` にはこの仕組みが無い。**

### 動作確認

`M-x ghostel` で pwsh が起動し、starship のプロンプトが Nerd Font の
アイコンごと出た。

```
󰍲 masao@hakugei  …\.emacs.d   master !?   20:58
```

`ghostel-debug-info` の抜粋。

```
Module loaded:       yes          Module version: 0.62.0
Term size:           144x76 (cols x rows)
Size sync:           in sync
Rendering:           HackGen 113 regular
Shell integration:   t            Detected shell type: (unknown)
```

---

## 設定で直した 2 点

どちらも**放っておくと静かに壊れる**。

### `ghostel-module-directory` を straight の外に出す

既定（nil）では module がパッケージディレクトリ、つまり
`straight/build/ghostel/` に置かれる。`straight-rebuild-package` や
`straight-prune-build` はあのディレクトリを作り直すので、**Emacs が
ロード中の `.dll` が消える。** upstream の docstring も「パッケージ
マネージャのツリーの外に置け」と言っている。

`~/.emacs.d/ghostel/` にして `.gitignore` に入れた。

なお `ghostel-debug-info` の **`Module file: NOT FOUND in …/straight/build/ghostel/`
は誤報ではなく、見ている先が違うだけ**（resource root を見ている）。
`Module loaded: yes` が本当の状態。

**terminfo と shell integration のスクリプトは resource root から読まれる**
（`TERMINFO=…/straight/build/ghostel/etc/terminfo`）。こちらは straight が
再ビルドのたびに作り直すので問題ない。

### `ghostel-shell` の既定値は Windows では cmdproxy.exe になる

既定値は `(getenv "SHELL")`。**Windows の Emacs ではこれが
`cmdproxy.exe`**（実測）。cmdproxy は Emacs がパイプ経由で非対話シェルを
起こすための代理プロセスで、端末に置くものではない。

`my-shell.el` の `my:windows-pwsh` を使い回して pwsh に揃え、
`etc/pwsh-emacs-shell.ps1`（UTF-8 固定・starship・OSC 7）も読ませている。
**ConPTY のコンソールのコードページは cp932 なので、実コンソールでも
UTF-8 への固定は要る。**

---

## アイコンが「半分の高さ」で出た件（2026-11-09）

starship のプロンプトのアイコンだけが潰れて描かれた。原因は**フォントの
送り幅**で、Ghostel の不具合ではない。

グリフに付いていた `display` プロパティがそのまま答えだった。

```
(display ((min-width ((8))) (height 0.5333333333333333)))
```

`0.5333…` は **8 ÷ 15**。

| | |
|---|---|
| 端末のセル幅 | **8 px**（HackGen の半角） |
| `Symbols Nerd Font Mono` の送り幅 | **15 px**（em と同じ） |

Ghostel はセルに収まらないグリフを `display` の `height` で縮める
（`ghostel-glyph-scale-floor` が下限、既定 0.0）。**縮小は縦横に等しく
かかる**ので、横を 8/15 に詰めると高さも 8/15 になる。

**Ghostty 本体は Nerd Font のグリフを横だけ潰す処理を持っているが、
Emacs にその経路は無い。** `ghostel-glyph-scale-floor` を 1.0 にすれば
縮まないが、今度は 15 px のグリフが 8 px のセルからはみ出す。

### なぜフォールバックのフォントが使われるのか

`my-appearance.el` が fontset に貼っているため。

```elisp
(set-fontset-font nil '(#xe000 . #xf8ff) spec)     ; 私用領域ぜんぶ
(set-fontset-font nil '(#xf0000 . #xfffff) spec)   ; 第 15 面
```

**fontset は face の `:family` に勝つ。** 端末バッファ専用の face
（`ghostel-default`）に `:family` を当てても、その face 用の fontset を
作って `:font` に渡しても、私用領域は奪い返せなかった（2 通り実測）。
**バッファ単位では直せない。**

### 直し方

手元の `HackGen Console NF` が条件を満たしていた（pixel size 15 / セル 8x17）。

| family | ascent/descent | a | あ | ─ | powerline | MDI |
|---|---|---|---|---|---|---|
| HackGen | 14/3 | 8 | 16 | **16** | 8 | なし |
| **HackGen Console NF** | **14/3** | **8** | **16** | **8** | **8** | **8** |
| HackGenNerd Console | 13/4 | 8 | 16 | 8 | 8 | なし |
| Symbols Nerd Font Mono | 12/3 | — | — | — | **15** | **15** |

本文フォントとして HackGen と**完全に等価**（ascent も全角幅も同じ）で、
罫線とアイコンだけが端末向けの半角になる。罫線が 8 px なのは TUI に
とってはむしろ正しい（conhost と同じ扱い）。

手当ては 2 か所に分かれる。**同じことを 2 か所に書かないため、フォントの
選定は `my-appearance.el` に 1 つだけ置いてある。**

| | 直すもの | どこ |
|---|---|---|
| `ghostel-default` の `:family` | 罫線・記号（fontset が握っていない字） | `my-ghostel.el` |
| fontset への `prepend` | アイコン（私用領域） | `my-appearance.el` |

後者は `my:console-nerd-font-ranges` で範囲を選ぶ。**既定は `powerline`**
（`#xe0a0`-`#xe0d4`）で、プロンプトの区切りだけを直し、dired と
モードラインのアイコンには触らない。`all` にすると端末のアイコンが
すべて原寸になる代わりに、dired とモードラインのアイコンも半角になる。

**`prepend` なので、そのフォントが持っていない字は後ろに落ちる。**
`HackGen Console NF` が持たない U+E6AD（dired のディレクトリアイコン。
README が「豆腐になる」と名指ししているもの）は
`Symbols Nerd Font Mono` に落ちることを実測した。

実測（`powerline` 既定での結果）:

```
U+E0B6  font=HackGen Console NF      display=nil      ← 縮小なし
U+E0B0  font=HackGen Console NF      display=nil
U+F0372 font=Symbols Nerd Font Mono  display=((min-width ((8))) (height 0.5333…))
```

### `all` に広げないことにした（2026-11-09、決定事項）

`powerline` の既定では、プロンプトの区切りは直るが **Windows（U+F0372）・
git（U+F418）・時計（U+F43A）のアイコンは潰れたまま**になる。
`all` にすればこの 3 つも原寸になる。それでも広げないことにした。

理由は **dired が不揃いになる**こと。`all` での実測:

| | `powerline` | **`all`** |
|---|---|---|
| dired の**ディレクトリ** U+E6AD | 15 px | **15 px**（変わらない） |
| dired の `.el` U+E632 | 15 px | **8 px** |
| dired の `.md` U+F48A | 15 px | **8 px** |
| モードラインの git branch U+F418 | 15 px | **8 px** |

`HackGen Console NF` が **U+E6AD を持っていない**ので、そこだけ
`Symbols Nerd Font Mono` にフォールバックして大きいまま残る。結果、
dired は**フォルダだけ大きくファイルは小さい**という見た目になる。
「全部小さくなる」ではなく「一部だけ小さくなる」のが効いた。

**範囲では切り分けられない。** 端末が要るものと dired が使うものが
同じ範囲に混ざっている（U+F418 はプロンプトとモードラインの両方、
U+F43A と `.md` の U+F48A はどちらも octicons、U+F0372 は nerd-icons の
mdicon と同じ第 15 面）。

検討して見送った案も記録しておく。

- **コードポイント単位で prepend する**（U+F0372 / U+F418 / U+F43A だけ）。
  dired は無傷でプロンプトだけ直るが、**starship の設定を変えるたびに
  追記が要る**
- **`all` + dired のディレクトリアイコンを U+E5FF（`nf-custom-folder`。
  `HackGen Console NF` が持っている）に変える**。dired は 8 px で揃うが、
  フォルダの絵柄が変わる
- **`ghostel-glyph-scale-floor` を 1.0 にする**。縮小は止まるが、15 px の
  グリフが 8 px のセルからはみ出して**桁がずれる**（upstream の docstring
  も「cells slightly wider」と言っている）

### フォントの選び方

`my:console-nerd-font-family` は**名前で決め打ちせず実測で選ぶ**
（`my:nerd-font-family` と同じ方針）。条件は 2 つ。

1. **半角の送り幅が本文フォントと一致すること**（違うと端末の桁がずれる。
   `SauceCodePro NFM` は 9 px なのでこれで落ちる）
2. powerline の 4 字がすべて同じ送り幅であること

比較は固定の pixel size（32）で開いて行う。`emacs-font-setting` の中で
走るので、**`frame-char-width` はまだ新しいフォントを反映していない
可能性がある**ため、フレームの状態に依存させない。15 と 32 で同じ結論に
なることは実測済み。

## eaw の桁ずれは起きない（2026-11-09 に実測）

`my-pty` の最大の難所だった East Asian Ambiguous の桁ずれが、**Ghostel では
起きない**。CLAUDE.md §4 の「`★` と `※` は端末で桁が揃わない（未解決）」は
`my-pty` 固有の問題で、こちらには引き継がれていない。

端末に 10 文字ずつ並べて、行の実描画幅を測った（セル 8 px）。

| 行 | 行の実幅 | 文字 | `char-width` | 送り幅 | Ghostel の指定 |
|---|---|---|---|---|---|
| `\|##########\|` | **96 px** | `#` | 1 | 8 | — |
| `\|★★★★★★★★★★\|` | **96 px** | `★` U+2605 | **2** | **16** | `min-width 8` + `height 0.467` |
| `\|※※※※※※※※※※\|` | **96 px** | `※` U+203B | **2** | **16** | `min-width 8` + `height 0.467` |
| `\|││││││││││\|` | **96 px** | `│` U+2502 | 2 | **8** | — |
| `\|あああああ\|` | **96 px** | `あ` U+3042 | 2 | 16 | — |

**5 行すべて 96 px で一致している。** Ghostel は libghostty が「1 セル」と
判断した文字を `min-width` で桁に押さえ、収まらないぶんは `height` で縮める。
つまり **Emacs の `char-width-table` が何と言おうとグリッドは崩れない。**
`my-pty` のように `char-width-table` を書き換える必要も、フォントを
差し替える必要も無い。

`│`（U+2502）は `HackGen Console NF` が半角で持っているので縮小すら
かからない（本文の HackGen では 16 px なので潰れていた）。
`あ` は fontset が日本語を HackGen に向けているため 16 px = 2 セルで正しい。

**残るのは見た目だけ。** `★` と `※` は 47% の高さに潰れる。原因は Nerd Font の
アイコンと同じで、手元のどのフォントも U+2605 / U+203B を全角で持っている
こと。直すならフォントを 1 つ patch する（`│` が直っているのと同じ理屈）。

### 測り方の注意

**`string-pixel-width` で測ってはいけない。** あれは**既定の face** で測るので、
`ghostel-default` への face-remap を見ない。実際、最初はこれで
「`★` の行だけ 176 px ある」という誤った結論を出した。
`font-at` でバッファ上の実フォントを取り、`font-get-glyphs` の
グリフベクタ（添字 4）から送り幅を読むこと。

行全体の幅は `window-text-pixel-size` で取る。これなら face も display
プロパティも通った実寸になる。

## 端末の描画を Emacs の外でやる案（見送り）

GenPreview（`my-genpreview`）と同じ構成 — ネイティブ側が Direct2D で描いた
画像を Emacs のバッファに貼り、入力とヒットテストを自前で処理する — を
端末にも適用できないか検討した。**技術的には可能だが見送った**（CLAUDE.md §2）。

- **動機が消えた。** 上のとおり eaw の桁ずれは Ghostel が既に解いている。
  得られるのは `★` / `※` が潰れないことだけ
- **レンダラが自作になる。** `libghostty-vt` 1.0 に描画は入っていない
  （フォントのシェーピング・色・カーソル・選択のハイライト・Kitty graphics の
  合成がすべて対象）
- **フレームレートが質的に違う。** GenPreview はファイルが変わったときに
  1 回描けばよいが、端末は出力のたびに描き直す（既定 30fps）。Emacs には
  画像を共有メモリで渡す経路も、一部だけ差し替える経路も無い
- **バッファが画像になる。** `C-s` / `M-w` / `occur` / 他モジュールからの
  参照がすべて消える。GenPreview は「読むだけ」なので選択とコピーを
  実装すれば足りたが、端末では Emacs の機能を 1 つずつ作り直すことになる

**外部ウィンドウを埋め込む道は無い。** このビルドに `XWIDGETS` は入っておらず
（`system-configuration-features` で確認）、Windows の Emacs に外部 HWND を
バッファへ埋める API も無い。画像として貼る以外の方法が無い。

**再開条件は w32 ビルドが xwidget 相当を持つこと。** その日が来ても VT の
解釈は `libghostty-vt` のまま使えるので、差し替えるのは描画層だけで済む。

## nyagos が使えるようになる見込み

`my-shell.el` のコメントにあるとおり、`M-x shell` では **nyagos を採用
できなかった**。Emacs は Windows で擬似端末を持てずパイプでシェルを
起こすが、nyagos はコンソールが無いと行編集を諦めてプロンプトを一切
出力しない（バッチ実行になる）ため、comint が入力と出力を区切れない。

**Ghostel は本物の ConPTY を渡すので、この前提が崩れる。** 補完・履歴予測
といったコンソール前提の機能も含めて動くはず。**まだ測っていない。**

---

## 分かっている制限

- **ConPTY には foreground process group の概念が無い。** ソースの
  コメントがそのまま「ConPTY has no foreground process-group concept.」で
  `foregroundPgid` は `null` を返す。`pty_utils.zig` の `isPasswordMode` も
  `if (builtin.os.tag == .windows) return false;`。
  `ghostel-foreground-change-functions` とパスワード入力の検出のうち
  OS に頼る部分は効かない（`ghostel-password-prompt-regex` による
  カーソル行の照合は効く）
- **shell integration に PowerShell 用が無い。** `etc/shell/` にあるのは
  bash / zsh / fish / nushell だけ。`ghostel-debug-info` も
  `Detected shell type: (unknown)` を返す。**端末として動くことと、
  プロンプト境界やコマンド履歴の統合が効くことは別。** cmd / nyagos も同じ
- Windows arm64 は libghostty 側が Tier 2（ビルドのみ、テストなし）

## 有効にしていない統合

どれも既定で off のまま。**評価が済むまで既存の操作を置き換えない。**

| | 何をするか | 置き換わるもの |
|---|---|---|
| `ghostel-compile-global-mode` | `compile` を端末で走らせる | `C-c c` |
| `ghostel-shell-command-global-mode` | `shell-command` を端末で | `C-x !` / `C-x \|` |
| `ghostel-comint-global-mode` | comint のバッファを端末に | SQL（`my-lang-misc`）など |
| `ghostel-status-indicator-mode` | モードラインに状態 | doom-modeline と重なる |

`ghostel-ime-mode` は Emacs Lisp の input method（quail / hangul）用で、
**Windows の IME（tr-ime / w32-ime）とは無関係。** こちらには要らない。

---

## 検証の作法（このモジュール固有）

- **`emacs-eval`（`ec.sh`）のプローブで `sit-for` を使ってはいけない。**
  `ec.sh` は式を `inhibit-interaction` で包むので、`sit-for` は
  **「User interaction while inhibited」で落ちる**（実測。しかも
  バックトレースを採るまで Ghostel 側の問題と区別が付かない）。
  redraw タイマー（`ghostel-timer-delay` = 0.033 秒）を走らせたいなら
  **`sleep-for`** を使う。対話ではないので通り、タイマーも走る
- **`let` で `ghostel-*` の defcustom を束縛する前に `(require 'ghostel)` する。**
  未ロードのまま `let` すると lexical な束縛が先に出来てしまい、あとから
  走る `defcustom` が
  `Defining as dynamic an already lexical var: ghostel-kill-buffer-on-exit`
  で落ちる
- **プロンプトが出るまでの待ち条件を「バッファが空でないか」にしない。**
  ウィンドウに出ていない端末も 80x24（ないし計算されたサイズ）の空の
  グリッドを描くので、**改行だけで `buffer-size` が 76 になる。**
  実測で「プロセスは生きているのに何も出ていない」と誤診した。
  行の中身を見ること
- `ghostel--native-child-pid` は**引数を 1 つ取る**（CHILD ハンドル）。
  0 引数で呼んで `ignore-errors` で包むと、黙って nil になる
- ユーザーが使っている Emacs に送るときは、`ghostel` が `pop-to-buffer`
  するので **`current-window-configuration` を退避して戻す**。
  バッファの kill は `ghostel-query-before-killing` を nil に束縛してから
  （既定 `auto` だと確認を求め、`inhibit-interaction` 下では落ちる）

---

## 参照

- [libghostty 1.0](https://mitchellh.com/writing/libghostty-1-0) — Mitchell Hashimoto、2026-11-06
- [lib.ghostty.org](https://lib.ghostty.org/) — API ドキュメント（1.0.0 は `/vt/1.0.0/`）
- [PACKAGING-LIBGHOSTTY.md](https://github.com/ghostty-org/ghostty/blob/main/PACKAGING-LIBGHOSTTY.md) — プラットフォーム Tier と配布物
- [awesome-libghostty](https://github.com/Uzaaft/awesome-libghostty) — 採用例の一覧
- [docs/pty/my-pty.md](my-pty.md) — 比較対象（`ptyd` + `eat`）
