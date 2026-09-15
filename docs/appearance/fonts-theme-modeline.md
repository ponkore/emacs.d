<!-- -*- gfm -*- -->

# フォント・テーマ・モードライン

Nerd Fonts の世代の違い、日本語フォントの全角/半角ピッチ、modus-themes 5.x、doom-modeline。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## フォントとアイコン

本文フォントは `HackGen`（Windows 12pt / macOS 16pt）。アイコンは `nerd-icons`。

`nerd-icons` は **Nerd Fonts v3** のコードポイント割り当てを前提にしている。
とくに Material Design アイコンは第 15 面 `U+F0001`〜`U+F1AF0` にあり、
v2 世代のパッチ済みフォントはこの面をまるごと持っていない。
このマシンにインストール済みのフォントを実測した結果：

| フォント | 世代 | mdicon (U+F0001) | seti 上位 (U+E6AD) | codicon (U+EA60) |
|---|---|---|---|---|
| `HackGenNerd` / `HackGen35Nerd`（Console 版含む） | v2 | ✗ | ✗ | ✗ |
| `HackGen Console NF` | v3 系 | ○ | ✗ | ○ |
| `Symbols Nerd Font Mono` (`fonts/NFM.ttf`) | v3 | ○ | ○ | ○ |

dired のディレクトリアイコンが `U+E6AD` なので、HackGen 系だけでは豆腐になる。
`fonts/NFM.ttf` を入れてあり、これを使う。

**リポジトリに置いてあるだけでは効かない。OS 側にインストールすること。**
`my:nerd-font-family` はシステムに登録されたフォントの中から選ぶため、
`fonts/NFM.ttf` が未インストールの環境では HackGen 系（v2）にフォールバックし、
dired のディレクトリアイコンだけが豆腐になる（2026-08、macOS で実際に踏んだ）。
clone しただけの新しいマシンでは必ず必要になる手順。

| OS | 導入方法 |
|---|---|
| Windows | `%LOCALAPPDATA%\Microsoft\Windows\Fonts` へコピーし、`HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts` にレジストリ登録（ユーザー単位） |
| macOS | `cp fonts/NFM.ttf ~/Library/Fonts/` のみ。登録作業は不要で、OS が自動で拾う |

どちらもインストール後に **Emacs の再起動が要る**（`font-get-glyphs` の判定は
起動時に済んでいるため）。フォントのファミリ名は `Symbols Nerd Font Mono`
（PostScript 名 `SymbolsNFM`）。macOS には `fc-list` が無いので、入っているかの
確認は `ls ~/Library/Fonts` で足りる。

`my:nerd-font-family`（`user-lisp/my-appearance.el`）が `font-get-glyphs` で
実際のグリフ有無を見て選ぶので、**フォント名を決め打ちしないこと**。
名前で決め打ちすると、v2 のフォントを掴んでアイコンが全滅する。

`fonts/` に置くのは `NFM.ttf` だけ。all-the-icons 用の 6 フォントは
（all-the-icons をやめたので）リポジトリからも Windows からも削除済み。

## 日本語フォントの全角/半角ピッチ

HackGen は「全角＝半角×2」で設計されているが、**サイズによっては 1px ずれる**。
Windows で実測した結果:

| `:height` | 半角 | 全角 | |
|---|---|---|---|
| 110 / 113 / 116 | 8 | 16 | 一致 |
| **120 / 124** | 8 | **17** | **ずれる** |
| 128 / 130 | 9 | 18 | 一致 |
| 140 | 10 | 20 | 一致 |

以前は 120 を使っていて桁が揃っていなかった。11.6（= 116）にしてある。

**`face-font-rescale-alist` では直せない。** ASCII と日本語が同じフォント
なので、スケールすると両方が同じ比率で縮むだけ。サイズを変えるしかない。
確認は `(string-pixel-width "あ")` と `(string-pixel-width "aa")` の比較で。

## テーマ

Emacs 31.1 同梱の **modus-themes 5.2.0** を `load-theme` で使う。
`:straight` は付けない（組み込み優先。`org` / `transient` と同じ扱い）。

かつて straight に 2021 年の 1.7.0 が入っていて、そちらが読まれていた。
v2 世代の API（`modus-themes-load-themes` / `modus-themes-load-vivendi` /
`modus-themes-region`）は 5.x には存在しないので、書き換えが要る。

| 旧 (v1/v2) | 新 (v4/v5) |
|---|---|
| `(modus-themes-load-themes)` + `(modus-themes-load-vivendi)` | `(load-theme 'modus-vivendi :no-confirm)` |
| `modus-themes-region '(bg-only …)` | `modus-themes-common-palette-overrides '((fg-region unspecified))` |
| `modus-themes-region '(… no-extend)` | 廃止（移行先なし） |

色の調整はパレットの上書きで行う。パレット名は
`etc/themes/modus-themes.el` の `modus-vivendi-palette` を見る。
`:custom` は `:config` より先に走るので、上書きは `load-theme` に間に合う。

**`:no-require t` が必須。** `modus-themes` は `etc/themes/` にあり `load-path` に
載っていないため `(require 'modus-themes)` は失敗する。use-package は require に
失敗すると `:config` ごと飛ばすので、これが無いと `load-theme` が呼ばれない。

## モードライン (doom-modeline)

背景色は **modus のパレット上書き**で指定する。Emacs 29 以降 `mode-line` とは
別に `mode-line-active` があり、テーマはそちらを塗るため、`custom-face` で
`mode-line` だけ変えても効かない。

```elisp
(modus-themes-common-palette-overrides
 '((bg-mode-line-active "medium blue")
   (fg-mode-line-active "snow")
   (border-mode-line-active "medium blue")))
```

左端のバー (`doom-modeline-bar`) だけはテーマのアクセント色なので
`custom-set-faces` で別途揃える（`:custom-face` はテーマに負けるので使わない）。`mode-line-inactive` はテーマのまま（灰色）にして、
どのウィンドウが選択中か分かるようにしてある。

**セグメント名はバージョンで変わる。** 4.x で `checker` は `check` に改名された。
古い名前が残っていると `doom-modeline--prepare-segments` が
`"checker is not a defined segment"` で落ち、**モードライン自体が有効にならない**。
利用できるセグメントは `doom-modeline-segments.el` の
`doom-modeline-def-segment` を grep すれば分かる。

