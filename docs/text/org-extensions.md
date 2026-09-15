<!-- -*- gfm -*- -->

# org の拡張

アーカイブ先の `#YM`、`#+FOLD_REGION:` による範囲の折りたたみ、`M-v` でクリップボードの画像を貼る (`_assets/` の整合性チェック付き)。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## org のアーカイブ先の `#YM`

アーカイブ先の指定に `#YM` と書くと `YYYY-MM` に展開される。

```org
#+ARCHIVE: %s_#YM_archive::* From %s
```

→ `note.org_2026-08_archive` に `* From note.org` 見出しで格納される。
ファイル名部分でも見出し部分でも使える。

実装は `org-archive--compute-location` への `:filter-args` advice
（`my-text.el`）。旧実装は `org-extract-archive-file` への `:filter-return`
だったが、この関数は org 9.8 で削除された。後継の
`org-archive--compute-location` は戻り値が `(FILE . HEADING)` の cons なので
`:filter-return` は使えず、**入口を `:filter-args` で押さえる**形にしてある。
引数は `::` で区切る前の生の文字列なので戻り値の形に依存しない。

`org-archive-subtree` は
`(or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location)` を
この関数に渡すので、`#+ARCHIVE:` / `ARCHIVE` プロパティ / 変数のどれで
指定しても効く（`org-archive-all-*` からの呼び出しも同様）。

## org で範囲を畳む（`#+FOLD_REGION:`）

org ファイルの冒頭に `#+FOLD_REGION: 過去分` と書いておくと、バッファ内の

```org
-- 過去分(begin)
  ...
-- 過去分(end)
```

に挟まれた部分を畳んで隠す（`my-text.el`）。名前を変えて何行でも書ける。

| | |
|---|---|
| `C-c C-x h` | 範囲の開閉（`my:org-fold-region-toggle`） |
| `M-x my:org-fold-region-hide-all` / `-show-all` | すべて畳む / 開く |

`C-c C-x -` ではなく `h` なのは、前者を `org-timer-item` が持っているため。

| 変数 | 既定 |
|---|---|
| `my:org-fold-region-begin-format` | `"^[ \t]*--[ \t]*%s(begin)[ \t]*$"` |
| `my:org-fold-region-end-format` | `"^[ \t]*--[ \t]*%s(end)[ \t]*$"` |
| `my:org-fold-region-hide-on-open` | `t`（開いた時点で畳む） |

`%s` に名前が `regexp-quote` されて入るので、マーカーの書式ごと変えられる。
候補が複数あるときは point 位置の範囲を優先し、決まらなければ選ばせる。

### narrowing ではなく invisible overlay を使う

`narrow-to-region` は「その範囲**だけ**を見せる」ものなので、隠したい範囲が
バッファの末尾か先頭にあるときしか使えない。overlay なら中間にあっても
複数あっても効く。

org 自身の折りたたみ（TAB / `#+STARTUP:`）は `org-fold` の spec で動いており、
こちらは独自の invisibility spec なので干渉しない（`org-fold-show-all` を
呼んでも畳まれたままであることを実測）。

隠すのは**開始行の行末から終了行の行末まで**。開始行は残るので、そこに
ellipsis の `...` が出る。バッファの中身は変わらないので保存内容にも影響しない
（実測で `buffer-size` も `buffer-modified-p` も変化なし）。

### 【重要】isearch は overlay を残したまま `invisible` を nil にする

`isearch-invisible` の既定は `open` なので、畳んだ中に検索が入ると isearch は
その範囲を一時的に開く。このとき **overlay は消えず、`invisible` プロパティ
だけが nil になる**。

そのため「overlay があるか」で畳まれているかを判定してはいけない。実際に踏んだ:
overlay の有無で見ていたため、isearch が開いたあと `my:org-fold-region-hide` が
「既にある」と判断して何もせず、**そのバッファでは二度と畳めなくなった**。

- `my:org-fold-region--hidden-p` が `invisible` プロパティで判定する
- `hide` は既存 overlay を捨てて作り直す（マーカー行を書き換えたときに
  追随する役目も兼ねる）
- `isearch-open-invisible-temporary` を自前で持たせて、一時開放の戻し方を
  一意にする。持たない overlay に対しては isearch が `invisible` を自分で
  退避・復元するが、復元は isearch の終わり方に左右される

実測（一時開放を模して `invisible` を nil にしてから操作）:

| | 修正前 | 修正後 |
|---|---|---|
| その状態から `hide-all` / `toggle` | **畳まれない** | **畳まれる** |
| 恒久的に開いた（overlay 削除）あと `hide-all` | 畳まれる | 畳まれる |

### 検証での注意

**`#+FOLD_REGION` は "OLD" を含む。** `case-fold-search` は org バッファでは t
なので、プローブに `(search-forward "old")` と書くとキーワード行にマッチして
「畳まれていない」と誤診する。実際に 1 度誤診した。

`org-mode-hook` 経由で畳むので、`ec.sh -l` でモジュールを読み直しただけでは
`setup` は走らない（`:hook` は use-package ブロックの評価で張られる）。
プローブ側で
`(let ((org-mode-hook (cons #'my:org-fold-region-setup org-mode-hook))) (org-mode))`
と束縛して測ること。

## org でクリップボードの画像を貼る（`M-v`）

org バッファで `M-v` を押すと、クリップボードの画像を
`<buffer-file-name>_assets/`（例: `note.org_assets/`）に保存し、
リンクを挿入してその場でプレビューする（`my:org-yank-image`）。

Emacs 30 で MS-Windows も `yank-media` に対応し、org 9.7 以降が
`image/.*` のハンドラを登録しているので、自前で書くのは保存先と
プレビューだけでよい。**外部プロセスは要らない**。

これで用が足りるため、`powershell.exe` から `ms-screenclip:` を起動して
範囲選択させていた `my:org-screenshot` は削除した（Win+Shift+S で撮ってから
`M-v` で貼れば同じことができる）。`etc/screenclip.ps1` はその名残。

| 変数 | 設定値 |
|---|---|
| `org-yank-image-save-method` | `my:org-image-save-directory`。関数を渡せるのは org 9.8 から |
| `org-yank-image-file-name-function` | `my:org-yank-image-filename` |

- ディレクトリは `org--image-yank-media-handler` が `make-directory` で作るので、
  設定側は名前を返すだけでよい
- リンクが相対パスになるのは `org-link-file-path-type` が既定の `adaptive` で、
  保存先がバッファの下位ディレクトリだから
- ドラッグ&ドロップ（`org--dnd-*`）の保存先も同じ変数を見るので一緒に変わる
- 既定の `org-yank-image-autogen-filename` は **マイクロ秒がファイル名に残らない**。
  `clipboard-…T…%6N` とドットで繋ぐため、`file-name-with-extension` が
  それを拡張子とみなして落とす。結果として秒単位の名前になり、同じ秒に
  2 回貼ると 1 枚目が上書きされる。ハイフンで繋ぐ関数に差し替えてある
- `M-v`（`scroll-down-command`）は org バッファでだけ潰れる。スクロールは
  `C-z`（`my-keybind.el`）が使える。`cua-mode` も `M-v` を
  `delete-selection-repeat-replace-region` に割り当てるが、それが載る
  `cua--cua-keys-keymap` は `cua-enable-cua-keys` が nil なら有効にならない。
  **batch では有効に見える**（`cua--select-keymaps` は `pre-command-hook` で
  走るため、`cua-mode` を有効にした時点の値のまま止まる）ので、
  `key-binding` を batch で確認するときは `(cua--select-keymaps)` を先に呼ぶこと

クリップボードに画像が載っているかは batch でも確認できる:

```sh
emacs --batch --eval '(message "%S" (gui-get-selection (quote CLIPBOARD) (quote TARGETS)))'
```

### 保存時の `_assets/` 整合性チェック

org バッファを保存すると（`after-save-hook`）、`_assets/` があるときだけ
バッファ内のリンクと突き合わせる（`my:org-assets-check-on-save`）。

| 状態 | 動作 |
|---|---|
| `_assets/` にあるがリンクされていない | `map-y-or-n-p` で 1 つずつ確認してごみ箱へ（`y`/`n`/`!`/`q`） |
| リンクはあるが `_assets/` に無い | 保存は成功させ、`message` で警告 |

消す側の判断を誤るとファイルが失われるので、安全側に倒してある。

- **必ず `org-with-wide-buffer` で見る。** ナローイングされたバッファで
  `org-element-parse-buffer` を呼ぶと見えている範囲しか解析されず、
  範囲外からリンクされているファイルを消してしまう
- リンク判定は `org-element` だけに頼らず、**ファイル名がバッファ内に文字列と
  して現れるかも見る**（`my:org-assets--mentioned-p`）。`org-element` は
  コメント行や例示ブロックの中のリンクを拾わないため、それだけだと
  「コメントアウトして退避してある画像」を消してしまう
- 削除は `(delete-file f t)` でごみ箱へ送る。誤って消しても戻せるように
- パスの比較は `file-truename` で正規化し、`file-name-case-insensitive-p` が
  真なら `downcase` する（Windows / macOS）
- `directory-files` の MATCH に文字列先頭アンカー（バックスラッシュ +
  バッククォート）入りの正規表現は書かない。エスケープを
  1 つ落としても静かに「1 件も一致しない」になり、**全リンクが「リンク先が
  無い」と誤判定される**。述語で絞るほうが壊れにくい

`org-save-all-org-buffers` は 1 時間ごとのタイマーからも呼ばれる。そのまま
だとタイマーが `y-or-n-p` を出して作業を止めるので、`:around` advice で
`my:org-assets-inhibit-check` を束縛し、その間はチェックごと飛ばす。
手で `M-x org-save-all-org-buffers` したときも同じく黙って保存する。

