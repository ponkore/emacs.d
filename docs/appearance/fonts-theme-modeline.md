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

## 【重要】豆腐を見たら「フォントが無い」と決めつけない（2026-11-09）

claude の TUI が出す `⏵⏵ auto mode on` の `⏵`（U+23F5）が、16 進の箱に
なっていた。**端末だけの問題ではなく、普通のバッファでも同じだった**
（`my-ghostel` でも素のバッファでも `font-at` が nil）。

原因は**フォントが無いこと**ではない。450 ファミリを全部あたった結果:

| | U+23F5 ⏵ | U+23F8 ⏸ | U+25B6 ▶ |
|---|---|---|---|
| HackGen | 無し | 無し | **あり**（16 px） |
| Segoe UI Emoji | **無し** | あり（21 px） | あり |
| **Segoe UI Symbol** | **あり**（15 px） | あり | あり |

`symbol` スクリプトの fontset には `Segoe UI Emoji` しか足していなかったので、
**持っている `Segoe UI Symbol` へ辿り着く経路が無かった**。最後の逃げ場として
append してある。

```elisp
(set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil 'append)
```

**`append` なので他の字には影響しない。** 実測で U+23F8 は `Segoe UI Emoji`、
U+25B6 / U+25B8 / U+2605 は `HackGen` のまま変わらなかった。

`docs/pty/my-pty.md` に「そのコードポイントのグリフを持つフォントが無いだけ」と
書いてあったのは誤りで、あわせて訂正した。

### 調べ方

- **`.notdef` を除く。** `font-get-glyphs` はグリフを持たない字にもベクタを
  返すことがある。**グリフ番号（添字 3）が 0 でないこと**まで見ないと、
  `System` / `FixedSys` / `Marlett` のような「何でも持っているように見える」
  フォントを拾う
- **`char-displayable-p` は当てにならない。** U+23F5 に対して `yes` を返すのに、
  実際の `font-at` は nil だった。**実際に表示したバッファで `font-at` を見る**
- **`ec.sh` に式を引数で渡さない。** U+23F5 は cp932 に無いので `?` に潰れ、
  `U+003F` を調べることになる（実際に一度踏んだ）。`-f` でファイル経由にする

### 端末では潰れる

`Segoe UI Symbol` の送り幅は 15 px なので、セル 8 px の端末では
`height 0.533` で縮められる（Nerd アイコンや `★` と同じ）。
**豆腐ではなくなるが原寸にはならない。** 半角で持つ実用的なフォントは
手元に無い（送り 8 px を返す `System` / `Marlett` などは別の絵が入っている）。

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

