<!-- -*- gfm -*- -->

# Claude Code を Emacs から使う (`my-claude.el`)

Windows の Emacs には PTY が無いので、双方向のストリーミング JSON を素のパイプで駆動する。会話バッファ・ヘッダ行・許可プロンプト・画像添付・逐次表示の設計と実測。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)


Windows の Emacs には PTY が無いので claude の対話 TUI は動かない
（stdin が TTY でないと claude は自動で `--print` に落ちる）。代わりに
**双方向のストリーミング JSON を素のパイプで駆動する**。端末エミュレーションも
常駐プロキシも要らない。

検討の経緯（ConPTY プロキシ方式との比較、PoC の実測）は
`docs/claude/emacs-claude-pty-proxy-study.md`、設計は
`docs/claude/emacs-claude-stream-json-plan.md`。

改善の第 1 弾（作業ディレクトリ・レイアウト・整形・ステータス）は
`docs/claude/emacs-claude-improve-01.md` にまとめてある。

| キー | |
|---|---|
| `C-c a a` | セッションを開き、画面をレイアウトする（`C-u` で立て直す） |
| `C-c a l` | いつでも同じレイアウトに戻す |
| `C-c a e` | 環境（アカウント）を切り替える |
| `C-c a t` | ワークスペースを信頼済みにする（下記） |
| `C-c a c` | 直近の会話を継いで開く（`--continue`） |
| `C-c a r` | 過去のセッションを一覧から選んで再開（`--resume`） |
| `C-c a m` | モデルを変える（会話は `--resume` で継続） |
| `C-c a i` | 会話バッファを出して入力エリアへ（レイアウトも組む） |
| `C-c a s` | リージョンを送る（**レイアウトは変えない**） |
| `C-c a k` | 中断 |
| `C-c a q` | セッション終了 |

`*claude(PROJ)*` の中では、**確定した会話の側と入力エリアでキーが変わる**
（後述）。

| キー | 確定した会話（区切りより前） | 入力エリア（区切りより後） |
|---|---|---|
| `i` | 入力エリアへ移動 | 自己挿入 |
| `TAB` | 畳んだツール出力を別バッファに出す | `markdown-cycle` |
| `z` / `q` | 最大化トグル / ウィンドウを閉じる | 自己挿入 |
| `C-c C-c` | 送信（どちらからでも） | 送信 |
| `C-c C-k` | 書きかけを捨てる（**中断ではない。中断は `C-c a k`**） | 同左 |
| `C-c C-z` | 最大化トグル | 同左 |
| `M-p` / `M-n` | 入力の履歴 | 同左 |
| `M-v` / `C-c C-v` | クリップボードの画像 / ファイルを添付 | 同左 |

## 入力は会話バッファの中で行う（2026-09-09）

`*claude-input(PROJ)*` は**廃止した**。バッファは区切り
（`my:claude-prompt-string`）で 2 つに分かれる。

```
*claude(PROJ)*
┌────────────────────────────┐
│ ▌ 前の入力                  │  read-only + keymap プロパティ
│ ● Read(foo.el) … 42 行      │  1 文字キー (i / p / n / TAB / z / q)
│ 応答テキスト …              │  font-lock は触らない（自前装飾のまま）
├────────────────────────────┤ ← my:claude--output-marker / --input-marker
│ 書きかけの入力              │  素の編集領域。markdown の font-lock
└────────────────────────────┘
```

**応答は区切りの前に挿さる**ので、読みながら次を書ける。実測（haiku で
1 往復、応答が流れている最中に入力エリアへ書いた）:

| | |
|---|---|
| 応答後の入力エリア | `応答を待ちながら書いている`（**残る**） |
| 区切りの数 | 1（常に末尾） |
| 確定領域を `delete-char` | `text-read-only` |

区切りの案内文（`my:claude-prompt-string`）には **`C-c a k`（中断）も
並べてある**。あれはセッションへの操作なのでグローバルに割り当てて
あるが、考え中・応答中にいちばん押したくなるキーがそこに見えていないと
探せない。ヘッダ行はセッションの状態表示で埋まっている。

### マーカーは 2 つ。insertion-type が肝

| | 指す位置 | insertion-type | |
|---|---|---|---|
| `my:claude--output-marker` | 確定した会話の末尾（区切りの手前） | **t** | 出力を書くとその後ろへ動く |
| `my:claude--input-marker` | 区切りの直後 | **nil** | 入力エリアの先頭に打っても動かない |

**区切りを挟むのはこのためでもある。** 区切りが無いと、insertion-type t の
マーカーは「入力エリアの先頭に打った文字」の後ろへ動き、その文字が確定側に
取り込まれる。区切りは read-only なのでそこに打つことはできない。

**会話バッファに書き足す処理は `point-max` ではなく
`my:claude--output-end` を見ること。** `point-max` は入力エリアの末尾で、
そこに書くと書きかけを壊す。移行のときに直したのは 4 か所
（`my:claude--fold` / `--fontify-markdown` / `--mark-text-start` /
`--end-paragraph`）。とくに `--end-paragraph` は
`(delete-region (point) (point-max))` で**書きかけを丸ごと消す**ところだった。

### 【重要】区切りは詰め物の改行で行頭に留める（2026-09-09）

出力は区切りの手前に挿さるので、**delta の途中では末尾が改行で終わって
いない**。そのままだと区切りが応答の途中から始まり、1 文字届くたびに
横へ流れる。案内文は常に同じ場所にいてほしい。

`my:claude--pad-before-prompt` が `my:claude--at-end` の最後で調整する。

| 出力の末尾 | 詰め物 |
|---|---|
| 改行で終わっていない | **入れる**（区切りは次の行の行頭へ） |
| 改行で終わっている | **捨てる**（空行が 2 つ並ばないように） |

**詰め物はマーカーの後ろに置く。** 挿入の間だけ
`my:claude--output-marker` の insertion-type を nil に倒すので、
マーカーは詰め物の前に留まる。次の delta はこの改行より前 = 同じ行の
続きに挿さるので、**逐次表示の行が詰め物で分断されない**。

**区切りの側に改行を足して済ませてはいけない**（`my:claude-prompt-string`
を `"\n 🤖 …"` にする手）。応答が改行で終わったときに空行が 2 つ並ぶ。
なお**末尾**の改行は `my:claude--setup-input-area` が三角のあとに自分で
足すので、こちらに書く必要は無い（書いてあっても落とす）。

batch 実測（同じプローブを修正前後で流し、区切りの先頭で `bolp`）:

| | 修正前 | 修正後 |
|---|---|---|
| 開いた直後 / 送信直後 | t | t |
| **delta の途中**（`応答の途` → `中です`） | **nil** | **t**（詰め物 1） |
| delta が改行で終わったあと | t | t（詰め物 0） |
| `--end-paragraph` のあと | t | t（詰め物 0） |

詰め物も確定領域（`read-only` + `my:claude-view-map`）。書きかけの入力は
どの状態でも壊れない。

### 保護は 3 つのテキストプロパティ

`my:claude--protect` が挿入のたびに載せる。

| | |
|---|---|
| `read-only t` | 編集を拒む |
| `front-sticky (read-only)` | 直前への挿入も拒む |
| `keymap` | ここでだけ 1 文字キーを効かせる（`my:claude-view-map`） |

**`rear-nonsticky` は区切りの末尾 1 文字にだけ付ける**
（`my:claude--setup-input-area`）。これが無いと入力エリアに 1 文字も
打てない。実測での落とし穴が 2 つ:

- **挙げ忘れたプロパティは打った文字にそのまま継承される。**
  `self-insert-command` は `insert-and-inherit` で挿すため。**列挙する形に
  していると必ず取りこぼす**ので、`rear-nonsticky` は `t`（このテキストの
  全プロパティを継承させない）にしてある。実際に 2 回踏んだ:

  | 挙げ忘れ | 症状 |
  |---|---|
  | `keymap` | 入力エリアなのに `i` が `my:claude-goto-input` になり、文字が打てない |
  | `font-lock-face` | **入力した文字が区切りの face を引きずる**（2026-09-10） |

  後者は `my:claude-prompt-face` に背景を敷いて初めて見えた。それまでは
  前景だけ（`:inherit shadow`）だったので、継承されていても気づけなかった。
  A/B 実測（同じテキストで `rear-nonsticky` だけ変えて 1 文字打つ）:

  | `rear-nonsticky` | 打った文字の `font-lock-face` |
  |---|---|
  | `(read-only keymap)` | **`my:claude-prompt-face`** |
  | **`t`** | **nil** |

- **プロパティを付ける操作自体が read-only に阻まれる。**
  `inhibit-read-only` の束縛が要る

**既に打ってある文字は直らない。** プロパティは挿入時に決まるので、
`rear-nonsticky` を直しても遡及しない。検証するときは打ち直すこと。

### 区切りは Powerline で閉じる（2026-09-11）

区切りは 4 つの部分でできている（`my:claude--setup-input-area`）。

| | | face |
|---|---|---|
| `my:claude-prompt-begin-string` | 左半円 U+E0B6 | `my:claude--prompt-edge-face`（前景だけ。**背景は地のまま**） |
| `my:claude-prompt-string` | ` 🤖 (C-c C-c 送信 / …) ` | `my:claude-prompt-face`（背景 `dark slate blue` / 前景 `light steel blue`） |
| `my:claude-prompt-end-string` | 右半円 U+E0B4 | `my:claude--prompt-edge-face` |
| 改行 | | **無し** |

**`:extend` は nil**。以前は `:extend t` で行頭からウィンドウ右端まで
帯にしていたが、それでは Powerline の端を置く先が無くなる。帯は文字の
両端で切り、その外を半円で閉じて、残りは塗らない。

**記号の前景は `face-attribute` で帯の背景から引く**
（`my:claude--prompt-edge-face`）。同じ色を 2 か所に書くと、片方だけ
変えたときに継ぎ目が壊れて気づけない。

**改行に face を載せないこと。** `:extend` が nil なので帯が伸びることは
無いが、載せると記号の色が改行にも及ぶ。

U+E0B4 / U+E0B6 は私用領域なので **Nerd Font が要る**（`fonts/NFM.ttf` =
`Symbols Nerd Font Mono`。`my-appearance.el` が `#xe000-#xf8ff` をそこへ
回している）。無い環境では両方を空文字列にすれば、帯が文字の端で切れる
だけになる。記号は `etc/nerd-font-sample.org` の「Powerline」の節にある。

#### 【重要】記号の高さは行に合わせて動的に決める

**放っておくと記号の上に隙間ができる。** 行の高さは行内でいちばん背の
高いフォントが決め（ascent の最大 + descent の最大）、グリフはベース
ラインに揃うので、記号の ascent が行の ascent より小さいとその差が
上に残る。GUI 実測（15px）:

| フォント | ascent | descent | 高さ |
|---|---|---|---|
| Segoe UI Emoji（🤖） | **16** | **4** | **20** ← 行を決めている |
| HackGen（帯の文字） | 14 | 3 | 17 |
| Symbols Nerd Font Mono（記号） | 12 | 3 | **15** ← 4px 足りない |

Nerd Font は **ピクセルサイズと ascent + descent が一致する**（実測で
15→15 / 17→17 / 20→20）ので、必要な高さのサイズで開けば行に収まる。
20px なら ascent 16 / descent 4 で絵文字と完全に一致する。
`my:claude--prompt-edge-height` が `my:claude-prompt-string` の全文字を
`char-displayable-p` で引いて最大の高さを求め、`:height` の倍率
（実測で 20/15 = 1.333）を返す。

**固定の倍率にしない。** 行の高さを決めるのは区切りの文言の中でいちばん
背の高い文字なので、🤖 を外すだけで 20px から 17px に変わる。書き換える
たびに測り直すのは現実的でない。

GUI 実測（区切りを組み立てて各部分を観測）:

| | |
|---|---|
| 先頭 / 末尾の文字 | **U+E0B6** / **U+E0B4** |
| 両端の face | `(:foreground "dark slate blue" :height 1.333)` |
| 帯の最大の高さ / 記号の素の高さ | **20px** / **15px** |
| 改行のプロパティ | `read-only` / `keymap` / `front-sticky` / `rear-nonsticky` のみ（**`font-lock-face` 無し**） |
| `my:claude-prompt-face` の `:extend` | **nil** |
| 入力エリアに打った文字 | 打てる。**プロパティは 1 つも継承しない** |

背景を敷く以上、前景も指定しないといけない（`shadow` の灰色では読めない）。

#### 【重要】私用領域の文字はソースに直接書かない

`my:claude-prompt-begin-string` / `-end-string` の値は `""` の
エスケープで書いてある。**PUA の文字は経路によっては黙って落ちる**
（この設定を書いたときに実際に落ちた）。CLAUDE.md 側も同じ理由で
`U+E0B6` と書き、生の文字は置かない。

**どの記号を指しているかは、Emacs のバッファから読むのが確実。**
会話バッファを走査して PUA のコードポイントを数えれば分かる。

```elisp
(when (or (<= #xE000 c #xF8FF) (<= #xF0000 c #xF1FFF)) ...)
```

#### 【重要】検証で `string-pixel-width` に read-only な文字列を渡さない

`buffer-substring` の戻り値には `read-only` プロパティが載っている。
`string-pixel-width` は**中で作業バッファに `insert` する**ので、それを
そのまま渡すと **`Text is read-only` で落ちる**。

`condition-case` で `insert` を囲んでも捕まらない（落ちているのは幅を
測るところで、`insert` ではない）ので、**入力できないのだと誤診する**。
実際に一度そう読んだ。`buffer-substring-no-properties` で渡すこと。
face は色しか持たないので幅は変わらない。

### font-lock は入力エリアだけに効かせる

確定した会話は挿入時に `font-lock-face` を直に載せてある
（`my:claude--fontify-markdown`）。入力エリアは `markdown-mode` の
font-lock に任せる。両立させるのが `my:claude--fontify-region`。

**`beg` を入力エリアの先頭まで切り上げるだけでは足りない。**
`font-lock-extend-region-functions` がリージョンを押し戻すので、確定した
会話の `# 見出し` が `markdown-header-face-1` に塗り替えられる（実測）。
**`narrow-to-region` して呼ぶこと**（`font-lock-dont-widen` も立ててある）。

GUI 実測:

| | 確定した会話の見出し | 入力エリアの見出し |
|---|---|---|
| 切り上げるだけ | **`markdown-header-face-1`** | `markdown-header-face-1` |
| narrowing して呼ぶ | **`my:claude-heading-face`** | `markdown-header-face-1` |

### undo は入力エリアのためだけにある

出力は `buffer-undo-list` を t に束縛して記録しない。加えて、**前方に挿すと
既存の undo エントリの位置がずれる**（Emacs は調整しない）ので、実際に
書いたときは履歴ごと捨てる（`my:claude--at-end`）。応答が届くと書きかけの
undo は効かなくなるが、壊れた位置を undo するよりはよい。

### 入力履歴（`M-p`）は会話バッファごと（2026-09-10）

`my:claude--input-history` は **`defvar-local`**。グローバルな `defvar` に
していたため、`*claude(a)*` で `M-p` すると `*claude(b)*` に打った入力まで
混ざっていた。セッションはプロジェクトごとなので履歴もそうあるべき。

たどる位置（`my:claude--input-index`）と書きかけ（`my:claude--input-draft`）は
元から `defvar-local` だった。**位置だけがバッファごとで中身が共有**という
ちぐはぐな状態だった。

立て直し（`C-c a m` / `C-c a e` / `C-c a r`）は**会話バッファを使い回し、
`my:claude-mode` を立て直さない**（`my:claude--start` の `unless`）ので
履歴は残る。それでも `permanent-local` を立ててある。ここが
`kill-all-local-variables` を通ると「モデルを変えただけで履歴が消える」
という分かりにくい壊れ方をするため（`my-htnblog.el` で踏んだのと同じ罠）。

会話バッファを kill するとセッションごと終わるので、履歴も消える。

#### 【重要】`kill-buffer-query-functions` はローカル値が残る

検証でモックの会話バッファを 7 個残した。`my:claude-mode` は
`my:claude--kill-query` を**バッファローカルに**積むので、
`(let ((kill-buffer-query-functions nil)) (kill-buffer b))` では消えず、
入力エリアに文字があると `yes-or-no-p` が出る。batch / `ec.sh` では
`inhibit-interaction` で落ちて **kill されないままバッファが残る**。
片付けるときは `setq-local` で外すこと。

### 追従（自動スクロール）の仕掛けが要らなくなった

挿入位置が `point` より**前**になったので、`point` も `window-point` も
自動でずれて相対位置が保たれる。入力エリアにカーソルがある窓は redisplay が
それを可視に保つので末尾に追いつき、読み返している窓は動かない。
`my:claude--at-end` から `goto-char` / `set-window-point` を落とした
（残しておくと、入力の途中にあるカーソルを末尾へ飛ばす**害**になる）。

## 自分の発言は行頭の帯で示す（2026-09-16）

長い応答が流れたあとに「自分が何を頼んだか」を遡って探すのが、この
バッファのいちばん頻度の高い読み方になった。それまでは 1 行目だけに
`> ` を付けていたが、**2 行目以降には何も付かない**ので行頭が揃わず、
色も `font-lock-keyword-face` なので応答の中の見出しと見分けが付かない。

`my:claude--insert-echo` が echo を組み立てる。

| | 受け持ち |
|---|---|
| 行ごとに前置する目印 | **論理行** |
| `wrap-prefix` テキストプロパティ | **折り返し行** |

**両方が要る。** 会話バッファは `truncate-lines` が nil（`my:claude-mode`）
なので、1 行しか無い長い入力でも必ず折り返る。前置だけだと折り返した先が
揃わず、`wrap-prefix` だけだと論理行の 2 行目以降に付かない。

### 帯は背景色を敷いた空白（`my:claude-echo-bar`）

記号を使わない。`▌`（U+258C）も `┃`（U+2503）も East Asian Ambiguous で、
`site-lisp/eaw.el` が幅 2 と数えるのに対してフォントが幅 1 で描くと桁が
ずれる（CLAUDE.md「eaw」）。空白なら幅の食い違いが原理的に起きない。

色は `my:claude-user-prefix-face` のオレンジ。**このバッファで他に使って
いない色**だから選んだ（ヘッダ行がマゼンタ / シアン / グリーン / イエロー、
`my:claude-answer-face` がグリーン、差分が赤と緑）。目的は「色だけで
当たりが付くこと」なので、既存と混ざる色では意味が無い。

`:extend` は立てない。立てると帯が行末まで伸びて行全体が塗り潰される。

### 【重要】`wrap-prefix` の文字列には `face` を載せる（`font-lock-face` ではない）

バッファに挿すテキストは他の挿入と揃えて `font-lock-face` だが、
**`wrap-prefix` に渡す表示用の文字列だけは `face`**。`font-lock-face` が
face の別名として効くのは `char-property-alias-alist` を通る経路
（= バッファのテキスト）で、表示用の文字列にも及ぶとは限らない。

外れ方が悪い。**論理行の帯はそのまま出て、折り返し行だけ無色になる。**
`my:claude--echo-prefix` が PROPERTY 引数でどちらも作れるようにしてある。

### サムネイルには帯を前置しない

画像はベースラインからはみ出す高さがあり、1 文字ぶんの背景では帯にならず
色の付いた点にしか見えない。見出し（`[Image #1] 800x600 image/png 120k`）
のほうはテキストなので普通に前置する。

代わりにサムネイルの行にも `my:claude-echo` を立てる。テキストと画像は
続けて挿さるので、プロパティが繋がって **1 つの発言**として数えられる。

### 発言の間を飛ぶ（`p` / `n`、`C-c C-p` / `C-c C-n`）

帯を目で探してスクロールする作業そのものを減らす。`my:claude-echo` が
立っている連続領域の先頭を `my:claude--echo-starts` が拾い、
`my:claude--goto-message` が前後へ飛ぶ。

| | |
|---|---|
| 確定した会話（1 文字キー） | `p` / `n` |
| 入力エリアも含めてどこでも | `C-c C-p` / `C-c C-n` |

飛んだ先は `recenter 1` でウィンドウの上寄りに置く。見返したいのは発言
そのものではなく**それに対して claude が何をしたか**なので、発言を天井に
貼って続きを見せる。戻るときは `i`（`my:claude-goto-input`）。

探す範囲は確定した会話だけ（`my:claude--output-end` まで）。

**この変更より前に送った発言にはプロパティが無いので飛べない。**
`ec.sh` で読み直して既存の会話バッファを見ると `my:claude--echo-starts`
が nil を返すが、それは壊れているのではない（実測で確認）。

## セッションはプロジェクトごとに持てる（2026-09-08）

`~/.emacs.d` と `~/.config` でそれぞれ `C-c a a` すると、
`*claude(.emacs.d)*` と `*claude(.config)*` が別の claude プロセスとして並ぶ。
同じプロジェクトで押したときは動いているものに戻るだけ。
**環境（アカウント）はセッションごと**なので、プロジェクトごとに別の
アカウントを当てることもできる。

以前は「Emacs 全体で 1 つ」に限っていた（`my:claude--the-session`）。
`CLAUDE_CONFIG_DIR` はプロセス起動時にしか読まれず、複数あるとどちらに
送っているのか分からなくなる、というのが理由だった。**その心配は
「送り先をバッファで決める」ことで消える**ので、一覧
（`my:claude--sessions`）に変えた。

| 関数 | |
|---|---|
| `my:claude--current-session` | いま操作の対象。①バッファローカル ②このバッファのプロジェクト ③生きているのが 1 つだけならそれ |
| `my:claude--read-session` | 決まらないときに選ばせる（`C-c a k` / `C-c a q` / `C-c a l`） |
| `my:claude--session-for-directory` | ディレクトリで引く |
| `my:claude--live-sessions` | 使えなくなったものを畳んで捨てながら返す |

**③ で止めること。** 複数あるときに「直近のもの」で代用すると、別の
プロジェクトに向かって送ってしまう。決まらないなら選ばせるか、
`C-c a a` なら新しく起こす。

`C-c a a`（`my:claude--ensure-session`）だけは③を**プロジェクトが
決まらないバッファ（`*scratch*` など）に限る**。プロジェクトが決まる
バッファからは、そのプロジェクトのセッションしか使わない。そうしないと
「別プロジェクトで開いたつもりが、たまたま 1 つだけ動いていた別の
セッションに繋がる」ことになる。

## バッファ名にはプロジェクト名が入る

`my:claude--project-label` が作業ディレクトリの名前を付ける。上で
`*claude*` と書いてあるものは実際には `*claude(.emacs.d)*` になる
（`*claude-log*` も同じ）。

**basename が同じプロジェクトを 2 つ開いたら親をたどる。**
`~/work/foo/src` を開いている状態で `~/other/src` を開くと、後者は
`*claude(other/src)*` になる。同じディレクトリなら `#2` を付ける。
**先に開いていたほうの名前は変えない**（見えているバッファの名前が
後から変わるほうが分かりにくい）。名前は起動時に 1 回決めて
`my:claude-session-label` に持たせ、ヘッダ行の 2 列目にも同じものを出す。

**そのため「claude のバッファか」を名前で判定してはいけない。**
`my:claude--buffer-p` はメジャーモード（`my:claude-mode`）で見る。

**バッファは名前で `get-buffer-create` しない**（`my:claude--buffer-for`）。
死んだセッションの会話バッファは記録として残るので、別プロジェクトの
セッションが同じ名前を取ると、他人の記録の続きに書き足してしまう。
持ち主のディレクトリが違えば `generate-new-buffer` で別名にする。

`my:claude-layout` はセッションより先に呼ばれることがある（`C-c a l`）ので、
そのときは `my:claude--guess-directory`（**確認を出さない版**）で名前を決める。
`my:claude--project-directory` を使うと画面を整えるだけで `y/n` が出る。
あわせて 2 点:

- **上半分に残すバッファは会話バッファを作るより先に決める。**
  あとに回すと、まだメジャーモードが立っていない新品のバッファを
  `my:claude--buffer-p` が claude 系と見なせず、上半分に選んでしまう
- 作った会話バッファにはその場で `my:claude-mode` を立てる（同じ理由）

## 会話バッファを kill したらセッションも終わる

別プロジェクトに移るのに kill する必要は無い（そちらで `C-c a a` すれば
並ぶ）。**書きかけの入力があるときは `yes/no` で聞く**
（`my:claude--kill-query`）。入力エリアは会話バッファの中にあるので、
退避先はもう無い。

`kill-buffer-hook` では kill を止められないので
`kill-buffer-query-functions` に載せること。

**「セッションが生きているか」をプロセスだけで判定してはいけない。**
`make-process` の `:buffer` は nil（出力は自前のフィルタが捌く）なので、
会話バッファを kill してもプロセスは生き残る。プロセスだけを見ていると
`C-c a a` が消えたバッファを持つセッションを使い回そうとして
`Selecting deleted buffer` になる（2026-09-06 に修正）。

- `my:claude--session-usable-p` が**プロセスとバッファの両方**を見る。
  `my:claude--live-sessions` / `my:claude--current-session` はこれを通す
- バッファが死んでいたら `my:claude--live-sessions` がその場で
  `my:claude-quit-session`（EOF）を送って一覧から外す。
  呼び出し側は新しいセッションを起こす
- `my:claude-mode` の `kill-buffer-hook` でも同じことをする。ただし EOF を
  送ってから sentinel が走るまでには間があるので、**その隙に `C-c a a` しても
  古いセッションを掴まないよう `my:claude--live-sessions` 側でも見る**

## 作業ディレクトリの決め方

**さかのぼりはしない。**

1. projectile のプロジェクトルート
2. 取れなければ、cwd に `.claude/` があれば cwd
3. どちらも外れたら `y/n` で確認し、拒否されたら `read-directory-name`

`project.el` は見ない（projectile と役目が重なる）。判定は 2 つの関数に
分けてある。**`my:claude--guess-directory` は確認を出さない**版で、
「起動済みのセッションを使い回すだけ」の場面ではこちらを使う。
分けないと `.claude/` の無いディレクトリから `C-c a a` するたびに
`y/n` が出る。

## ウィンドウのレイアウト（`my:claude-layout`）

```
┌──────────────┐
│ 編集中のバッファ │  フレームの 1/2
├──────────────┤
│ *claude*      │  残り（カーソルは末尾＝入力エリア）
└──────────────┘
```

`my:claude-window-height-ratio`（既定 0.5）で変えられる。入力エリアは
会話バッファの中にあるので、**分割は 2 つで足りる**（入力バッファが
あった頃は 3 分割で、送信のたびに畳んでいた）。

**`window-configuration` は退避しない。** 最大化トグルの復帰先も
`C-c a l` も同じ関数を呼ぶだけなので、どこから何度押しても同じ形に
落ち着く。高さは `window-total-height` から採る（`window-body-height`
だとモードラインとヘッダ行を数え落とす）。

## 環境（アカウント）の切り替え

Pro / Enterprise / Max 20x を `CLAUDE_CONFIG_DIR` で使い分けている。
claude はこれを**プロセスの起動時にしか読まない**ので、切り替え
（`C-c a e`）は立て直すことでしか行えない。**環境はセッションごと**に
固定される（プロジェクトごとに別のアカウントを当てられる）。
どれに送っているかはヘッダ行の 1 列目で確かめる。

`my:claude-environments` に `(ラベル . CLAUDE_CONFIG_DIR)` で並べる。
選択時に `claude auth status --json` を呼んで実際のアカウントを見せる
（実測 0.24 秒。結果はキャッシュし、`M-x my:claude-refresh-auth` で捨てる）。

```
personal   pro         ponkore@gmail.com's Organization
jighead    max         masao.kato@jighead.co.jp's Organization
ESC-Web    enterprise  株式会社　熾火
```

ヘッダ行に

```
jighead(max) v2.1.260 | .emacs.d | master | claude-opus-5 (high) | ctx 103.2k 52% | (5h 4%)(7d 8%)(reset 09/05 03:00) | $6.17
```

を出す。残量は `rate_limit_event` から取っている。**アカウントを
切り替える判断はこの数字で行う**ので、常に見えるようにしてある。

## ステータスの表示はヘッダ行に集約する（モードラインには出さない）

`~/.claude/statusline-command.sh` が端末の TUI に出している項目を
Emacs 側で再現してある。**`statusLine` は端末 TUI の機能で、`-p`
（stream-json）経路では発火しない**（実測でイベントに一切現れない）ので、
スクリプトの出力をもらうのではなく同じ情報を stream-json から自前で
組み立てている。

ヘッダ行は 6 列で、色は statusline スクリプトが使っている ANSI 色に
合わせてある。

| 列 | 内容 | 色 | 取得元 |
|---|---|---|---|
| 1 | アカウント（プラン）と claude のバージョン | マゼンタ | auth cache と `system/init` の `claude_code_version` |
| 2 | プロジェクト名（フルパスは `help-echo`） | シアン | セッションの cwd |
| 3 | git ブランチ | グリーン | `.git/HEAD`（後述。git は呼ばない） |
| 4 | モデルと effort | イエロー | `system/init` の `model` と `my:claude--effort` |
| 5 | コンテキスト使用量 | グリーン | `assistant` の `message.usage` の `input_tokens` + `cache_read_input_tokens` + `cache_creation_input_tokens` / `result` の `modelUsage.<model>.contextWindow`（1M 版なら 1000000 が来る） |
| 6 | レート上限とリセット時刻 | シアン | `rate_limit_event` の `unifiedWindows` |
| 7 | 累計コスト | dim | `result` の `total_cost_usd` |

先頭にもう 1 桁、**応答待ち**（回る点 / 確認待ちは `?`）を置いてある（後述）。
列ではないので区切りを出さず、待っていないときも桁だけ空ける。

face は `my:claude-header-{plan,dir,branch,model,context,limit,cost}-face`。
**`:foreground` だけを指定する。** ヘッダ行では `header-line` face が
下地になり、テキストプロパティの face はその上に重なるので、背景は
テーマのものがそのまま残る。

**`claude --version` を別に呼ぶ必要は無い。** statusline スクリプトが
1 時間キャッシュまでして避けていたプロセス起動が、`system/init` に
最初から入っている。

### git ブランチは `.git/HEAD` を読む（git は呼ばない）

かつては「プロセス起動のコストに見合わない」として載せていなかったが、
**そもそも git を起動する必要が無い**。ブランチ名は `.git/HEAD` の
1 行目にそのまま入っている。実測（1 回あたり）:

| | |
|---|---|
| `file-attributes` で stat（キャッシュのヒット判定） | **0.043 ms** |
| `.git/HEAD` を読んでパース（キャッシュのミス時） | **0.061 ms** |
| `call-process git rev-parse --abbrev-ref HEAD` | **55.6 ms** |

1300 倍違うので「Emacs の `call-process` が Windows で遅い」（既知の課題）
を丸ごと迂回できる。

**列の中身は `header-line-format` の `(:eval ...)` で出す。**
ブランチの切り替えは Emacs の外（端末や magit）でも起きるため、
ターンごとの `my:claude--update-header` では古い表示が残る。`:eval` なら
再描画のたびに評価されるので、監視もタイマーも要らない。GUI 実測で
`:eval` 1 回 0.044 ms、ヘッダ行全体でも 0.048 ms。

そのため **`my:claude--header` の戻り値は文字列ではなくリスト**
（mode-line 構文）になっている。`mapconcat` で 1 本の文字列にすると
`:eval` が死ぬ。

- **列を出すかどうかは gitdir の有無で決める。** これはセッションの
  作業ディレクトリで決まり起動後に変わらないので、探索
  （`my:claude--git-dir`。`locate-dominating-file` で上へ辿るだけ）は
  セッションを作るときの 1 回だけ。`:eval` 側が空文字列を返すと区切りが
  二重に残るので、読めなければ `?` を出す
- **`%` の escape は `:eval` の戻り値にも要る。** `:eval` の結果は
  mode-line 構文として**再解釈される**ため。`my:claude--header-segment`
  を通すこと（`pct-100%-done` で確認済み）
- worktree と submodule では `.git` がファイルで、中身が `gitdir: PATH`。
  それを辿る
- detached HEAD では `ref:` ではなく生の SHA が入っているので短縮して出す
- **ブランチ名は UTF-8 で decode する。** `insert-file-contents-literally`
  は unibyte バッファを作るので、そのままでは非 ASCII のブランチ名が化ける

#### 【重要】検証で `call-process` から git にブランチを作らせない

日本語のブランチ名を `call-process` の引数で渡すと、**引数の側で化ける**。
実測（`emacs -Q`、「機能」= UTF-8 で `e6 a9 9f e8 83 bd`）:

| 作り方 | `.git/HEAD` に書かれたバイト列 |
|---|---|
| `call-process` の引数にそのまま | `e8 ae 96 e6 ba af e3 83 bb`（= 「讖溯・」） |
| 引数を `utf-8` で encode して渡す | 同上 |
| `cmd.exe /c chcp 65001 && git ...` 経由 | 同上 |

3 通りとも同じ。UTF-8 のバイト列が cp932 として解釈された結果で、
CLAUDE.md の「`call-process` の引数は cp932 でエンコードすること」と
同じ罠。**読み取り側は正しいのに壊れて見える**ので、検証では
`.git/HEAD` を直接書くこと。git が HEAD を UTF-8 で書くこと自体は
上の 3 通りとも一致していて確認できている。

#### 【重要】`format-mode-line` は選択ウィンドウのバッファで `:eval` を評価する

BUFFER 引数を省略すると、カレントバッファではなく**選択ウィンドウの
バッファ**が使われる。`with-temp-buffer` の中で

```elisp
(setq-local my:claude--session session)
(format-mode-line header-line-format)   ; ← :eval は *scratch* で評価される
```

としても、バッファローカルの `my:claude--session` が見えず `:eval` が
黙って空になる。**列が消えるだけでエラーは出ない。** 実際に
`switch-to-buffer` してから測ること（batch では `format-mode-line` が
常に `""` を返すのでそもそも検証できない）。

### effort level は stream-json に出てこない

全イベントの全キーを列挙して確認した。`system/init` には
`permissionMode` / `output_style` / `fast_mode_state` はあるが
`effort` は無い。そこで `my:claude--effort` が次の順で求める。

1. `my:claude-effort`（defcustom）。非 nil なら `--effort` で明示するので
   その値がそのまま効く
2. `settings.json` の `modelSettings.<model>.effortLevel`
3. `settings.json` の `effortLevel`

`settings.json` は claude 自身の優先順位に合わせて 3 つ見る。

```
<プロジェクト>/.claude/settings.local.json
<プロジェクト>/.claude/settings.json
<CLAUDE_CONFIG_DIR>/settings.json     ← 既定なら ~/.claude/settings.json
```

**`.claude.json` とは置き場が違う。** 信頼判定などが入る `.claude.json`
は `~/.claude.json` だが、`settings.json` は `~/.claude/settings.json`。
`my:claude--config-json` と `my:claude--settings-files` で組み立て方を
分けてある。

**`modelSettings` のキーは前方一致で突き合わせる。** キーは
`claude-opus-5` のように日付が付かないのに対し、`system/init` が返す
モデル名は `claude-haiku-4-5-20251001` のように日付付きのことがある。

effort は毎ターン求め直さない（`system/init` はターンごとに来る）。
モデルが変わったときだけ取り直す。**判定はモデルを更新するより先に
行うこと。**

### 【重要】`header-line-format` に出す `%` は `%%` に escape する

`header-line-format` / `mode-line-format` に**素の文字列**を渡すと、
Emacs が `%` を書式指定子として解釈し、**`%` と直後の 1 文字がまとめて
消える**。`%` の次が空白でも `)` でも同じ。

```
raw       : ... ctx 103.2k 52% | (5h 5%)(7d 8%)(reset 09/04 23:10)
displayed : ... ctx 103.2k 52| (5h 5(7d 8(reset 09/04 23:10)
```

**escape は列ごとに、色を付ける前に済ませる**
（`my:claude--header-segment`）。組み立てた**全体**に
`replace-regexp-in-string` を掛けると、**差し込まれる `%%` だけが face を
持たない**素の文字列になり、その桁で色が切れる。ディレクトリ名や
モデル名に `%` が入る場合もあるので escape 自体はやめられない。

**この検証は batch ではできない。** `format-mode-line` は batch では
常に `""` を返す。GUI で `(format-mode-line 文字列)` を見ること。
**「組み立てた文字列」ではなく「実際に表示される文字列」を見ないと、
`%` の扱いも face の生き死にも分からない。** `help-echo` も
`format-mode-line` を通って残る（実測）。

2026-09-04 に発見。`5h 4% 7d 8%` が `5h 47d 8` と表示されていた。
あわせてレート上限の表示を `(5h 5%)(7d 8%)(reset MM/DD HH:MM)` の形に
変えてある（`%` が区切りに埋もれず読めるように）。

### モードラインには何も出さない（2026-09-05）

かつては `mode-line-process` に `[.emacs.d ... $0.12]`（プロジェクト名 /
応答待ち / 累計コスト）を出していたが、3 項目ともヘッダ行と重複するので
やめた。累計コストはヘッダ行の 7 列目、応答待ちは先頭に移してある。

```
⠴ personal(pro) | .emacs.d | main | claude-opus-5 | ctx 103.2k 52% | $6.17
```

- コストは直近の `result` の `total_cost_usd`。**1 往復ぶんではなく
  セッション開始からの累計**が来る（`--resume` で継いだ会話ぶんを含む）
- **7 列目も `:eval`。** `result` が来たその場で更新するため
- **7 列目は区切りも自分で出す**（`my:claude--cost-segment`）。末尾の列
  なので、起動直後（`result` がまだ無く応答待ちでもない）に
  区切りだけが行末に残らないようにする必要がある。ブランチの列が
  「gitdir の有無で列ごと出し入れする」のに対し、こちらは毎回変わるため
  `:eval` の側で判断するしかない
- 色は `shadow` を継ぐ。statusline スクリプトがコストを `C_DIM`（ANSI の
  dim）で出しており、dim に対応する固定の色が無いため。ここだけ色名を
  直接書いていない

GUI 実測（`format-mode-line` を通した実表示。末尾だけ抜粋）:

| 状態 | 末尾 |
|---|---|
| `result` なし | `… ctx 103.2k 52%`（**区切りも出ない**） |
| `result` あり | `… 52% \| $6.17` |

`my:claude-mode` の `mode-line-process` は空。
**この検証はモードを実際に立てて行うこと。** 変数の既定値を見ても
「モードが設定しない」ことの証明にはならない。

### 応答待ちは 3 値（2026-09-17）

`busy` は真偽値ではない。**増やすときは `eq` で見ること。**

| 値 | 意味 | ヘッダ行の先頭 |
|---|---|---|
| `nil` | 待っていない | 空白（**桁は空ける**） |
| `t` | claude が動いている | 回る点 `⠋⠙⠹…`（青） |
| `asking` | ミニバッファで返事を待っている | **止まった `?`**（イエロー） |

**分けているのは、この 2 つで待っている側が逆だから。** 点が動いている間は
放っておけばよく、止まっているときは自分が答えないと進まない。許可プロンプトも
AskUserQuestion も `result` が来る**前**に聞くので、区別しないとどちらも
「考え中」に見える。以前はそうなっていた。

- 状態を変えるのは `my:claude--set-busy` だけ。**`busy` を直に `setf` しない**
  （タイマーの入り切りと再描画をここでまとめている）
- ミニバッファで待つ区間は `my:claude--with-asking` で包む。**戻すのは
  `unwind-protect` で。** `C-g` で抜けたときに `asking` が残ると、その
  セッションは以後ずっと点が止まったままになる
- 許可プロンプトは**ループごと**包む。`v`（入力を全部見る）で聞き直すたびに
  点が動き出しては意味が無い
- **色も分ける**（点は青 `my:claude-header-busy-face`、`?` はイエロー
  `my:claude-header-asking-face`）。動きだけでなく色でも区別が付くように

#### なぜ末尾ではなく先頭か（2026-09-17）

最初は 7 列目（累計コストの隣）に出していたが、**末尾は見ない場所**で、
しかもブランチ名やディレクトリ名が伸びるとウィンドウの右で切れて
消えてしまう。先頭なら必ず目に入り、何が伸びても位置が変わらない。

**待っていないときも桁を空ける。** 出したり消したりすると、後ろの列が
まるごと 1 桁動く。

#### 【重要】ブレイルは HackGen に無い（9 px / ASCII は 8 px）

点は `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏`。**フォントは HackGen ではなく Cascadia Code に
落ちている**（`font-at` で実測）。`char-width` は 1 なのに
`string-pixel-width` は **9 px** で、HackGen の ASCII（8 px）と 1 px ずれる。

10 面とも 9 px なので回っている間は揺れないが、`?`（8 px）や空白（8 px）と
入れ替わるところで後ろが 1 px 動く。**後ろの空白に
`(space :align-to 2)` を載せて桁を固定してある。**

| | 素のまま | `:align-to 2` |
|---|---|---|
| `"⠋ "` | 17 px | **16 px** |
| `"⠀ "`（ブレイル空白） | 17 px | **16 px** |
| `"? "` | 16 px | **16 px** |
| `"  "` | 16 px | **16 px** |

**埋める空白にも大きさの face を載せること。** 行の高さは行内でいちばん
高いグリフで決まるので、1 か所でも素のままだとヘッダ行が縮まない
（区切りの `" | "` と同じ話）。

#### 点は `run-at-time` で回す

`:eval` は再描画のたびに評価されるが、**再描画のきっかけが無い**。出力が
届いたときとコマンドの後しか動かないので、`my:claude--spinner-tick` が
`my:claude-spinner-interval`（既定 0.1 秒）ごとに
`force-mode-line-update` を呼ぶ。

- **タイマーは `busy` が `t` のセッションがある間だけ回る**
  （`my:claude--spinner-refresh`）。確認待ちの間も、誰も待っていない間も
  止まる。**止め忘れると Emacs が永久に 0.1 秒ごとに起きる**
- 描き直すのは待っているセッションのバッファだけ。`force-mode-line-update`
  に ALL は渡さない

GUI 実測（0.11 秒ごとにヘッダ行の先頭 10 桁を採った）:

```
"⠹ jighead(" "⠸ jighead(" "⠸ jighead(" "⠼ jighead(" "⠼ jighead(" "⠴ jighead("
```

3 つの状態（先頭 26 桁。**2 桁目から後ろが 1 桁も動いていない**）:

```
nil      "  jighead(max) v2.1.263 | "
asking   "? jighead(max) v2.1.263 | "
t        "⠴ jighead(max) v2.1.263 | "
```

確認待ちへの出入り（実測）:

| | `busy` | タイマー |
|---|---|---|
| `my:claude--with-asking` の中 | `asking` | **止まる** |
| 抜けた後 | `t` | 回り直す |
| 中で `C-g` | `t` | 回り直す |
| 待っているセッションが無い | — | 止まる |

## 逐次表示（`my:claude-stream`、既定 t）

`--include-partial-messages` を付けて `stream_event` を拾い、書かれる端から
バッファに流す。イベントの並びは実測でこうなっている。

```
content_block_start (thinking / text / tool_use)
content_block_delta … (thinking_delta / signature_delta /
                       text_delta / input_json_delta)
assistant                  ← そのブロックの確定版
content_block_stop
```

**`assistant` は `content_block_stop` より先に、ブロック 1 つぶんずつ届く。**
そのため text は delta で出し、`assistant` 側では出さない（出すと二重になる）。
tool_use は逆に delta（`input_json_delta`）を捨てて `assistant` の確定版だけ使う。
JSON の断片は揃うまで意味を持たないため。

中断すると `content_block_stop` が来ないことがあるので、`result` を受けた
ところでブロックを閉じる。

`thinking_delta` の本文は **haiku では空文字列で届く**。`my:claude-show-thinking`
を t にしても何も出ないことがある。

## 過去セッションの一覧（`C-c a r`）

`--continue` は「そのディレクトリの直近の 1 つ」しか選べない。
記録ファイルを直接読んで一覧にする。

セッションは
`<CLAUDE_CONFIG_DIR>/projects/<エンコードしたパス>/<session-id>.jsonl`
に貯まる。ディレクトリ名は **ワークスペースのパスの英数字以外をすべて
`-` に置き換えたもの**。`C:/Users/masao/.emacs.d` なら
`C--Users-masao--emacs-d`。手元の 10 個で突き合わせて確かめた
（合わなかった 1 つはドライブレターの大小違いだけで、Windows の
ファイルシステムでは同じ場所を指す）。

### 【重要】`message.content` は文字列とは限らない

一覧に出すプロンプトを取り出すとき、**文字列だけを見てはいけない。**
ブロックの配列で入っていることがあり、**Emacs から送ったものは必ず配列**。
文字列しか見ないと、自分で作ったセッションが全部「(プロンプトなし)」に
なる。実際にそうなっていた。`my:claude--content-string` が両方を扱う。

1 MB を超えるファイルもあるので、先頭 200 KB / 400 行で打ち切る。

## サブエージェントの表示

サブエージェントの発言は **`parent_tool_use_id` 付きの assistant / user
イベント**として届く。`--forward-subagent-text`（`my:claude-forward-subagent-text`、
既定 t）を付けると増えるが、**付けなくても一部は届く**（実測）。

**`stream_event` に `parent_tool_use_id` が付くことは無い。**
つまりサブエージェントの本文は delta では来ないので、
`streamed-text` を見ずに必ず出す。見てしまうと、本体のブロックが
開いている間はサブエージェントの発言が捨てられる。

表示は字下げ + `my:claude-subagent-face` で本体と区別する。

## ツールの実行結果は既定で全部畳む

`my:claude-tool-result-max-lines` の既定は **0**（= 常に畳む）。
畳んだ行は 1 行の要約になり、`TAB` で全体を `*claude tool output*` に出す。

```
  ● Read(user-lisp/my-claude.el) … 42 行
```

`Read(...)` の中身は `my:claude--tool-summary` の結果だが、
**`tool_result` には入力が入っていない**。`tool_use` を受けた時点で
名前と一緒に要約も覚えておく必要があるので、`tool-names` ハッシュの値は
`(NAME . SUMMARY)` の cons にしてある。

**エラーだけは畳まない**（`my:claude-error-result-max-lines`、既定 30 行まで）。
一律に畳むと「なぜ失敗したか」がその場から消え、雑音を減らすという
目的とは逆にいちばん見たいものが隠れる。

## Edit / Write の差分表示

`tool_use` の入力に `old_string` と `new_string`（Write は `content`）が
そのまま入っているので、行頭に `-` / `+` を付けて色分けする。
`my:claude-diff-max-lines`（既定 30）を超えたら行数だけ知らせる。

**外部の diff は呼ばない。** Windows に入っている保証が無いうえ、
Edit の入力は置換前と置換後がそのまま来るので、行単位で並べれば足りる。

差分に `TAB` は効かない。**「TAB で全体を表示」と案内していたのは嘘**
だった（`my:claude--show-edit` は `my:claude-full` を設定しないので
`ここには折りたたまれた出力が無い` になるだけ）。案内は
`(差分 %d 行。git diff で確認)` に直してある。

## 許可の `permission_suggestions`

要求には `permission_suggestions`（例: `acceptEdits` に切り替える）が
付いてくる。これを `updatedPermissions` に載せて allow を返すと
**claude 側が以後聞いてこなくなる**。実測で 2 回目の `Write` が
聞かれなくなった。

許可プロンプトの `a` がこれを使う。候補が付いていないときだけ
Emacs 側で覚える従来動作に落ちる。

## 【重要】AskUserQuestion の答えは `deny` の `message` に載せる

**AskUserQuestion はホスト側が実行するツール**で、選択 UI を出して答えを
`tool_result` の `answers` に載せるのは端末 TUI の仕事になっている。
`-p`（stream-json）にはその UI が無いので、**allow を返しても答えは返らない**。

しかし **ツール自体は `-p` でも提供されている**（`system/init` の `tools` に
入っている。149 個中に `AskUserQuestion` があることを実測）ので、claude は
普通に呼んでくる。放っておくと質問が出ないまま止まる。

そこで `can_use_tool` を横取りして Emacs 側で聞き、**答えを `deny` の
`message` に載せて返す**（`my:claude--answer-questions`）。deny の message は
そのまま claude に届く（別節）ので、これが唯一の回答経路になる。

実測（`--model haiku`、実際に 1 往復させた）:

| | |
|---|---|
| `tool_use` の入力 | `questions` が丸ごと来る（`question` / `header` / `multiSelect` / `options[].label` / `options[].description`） |
| `control_request` | `subtype=can_use_tool` / `tool_name=AskUserQuestion` で**必ず飛んで来る** |
| `permission_suggestions` | **`null`**（このツールには付かない） |
| deny の message | `is_error=true` の `tool_result` として**逐語で届く** |
| claude の反応 | 「**青**をお選びになりました」= **答えとして解釈された** |

`is_error` が立つのは避けられないので、message には
「Emacs には UI が無いので拒否の形で答えが届く。ツールを呼び直すな」を
併記してある。これが無いと claude が失敗と見て再試行しかねない。

- **`my:claude-auto-approve` より先に判定する**（`my:claude--ask-permission` の
  `cond` の先頭）。auto-approve に一致して allow で通すと、質問がどこにも
  出ないまま答えが返らない
- 質問と選択肢は**聞く前に会話バッファへ出す**。説明文は長く、ミニバッファの
  注釈だけでは読み切れない。出しておけば会話の記録にもなる
- **質問を 2 行に出さない**（下記）
- 候補の並びは `display-sort-function` を `identity` にして**claude が並べた
  ままにする**（推奨が先頭に来ることがある）
- `require-match` は nil。候補に無い文字列も返せる（本家 UI の「Other」）。
  空で確定されたら聞き直す
- `multiSelect` が `t` なら `completing-read-multiple`。答えは `, ` で連結する
- **`C-g` でも必ず応答を返す**（`condition-case` の `quit` 節）。返さないと
  claude が待ち続ける。エラー時も同じ
- 切るときは `my:claude-answer-questions` を nil にする（従来の許可プロンプトに
  戻る）。ただし `--permission-mode` が `dontAsk` / `bypassPermissions` のときや
  `permissions.allow` に `AskUserQuestion` があるときは `can_use_tool` 自体が
  飛んで来ないので、そもそもこの経路は効かない

### 質問は 1 行だけ出す（2026-09-15）

質問文を出す場所が 2 つあり、**質問が 1 つのときは必ず同じ行が 2 本並んでいた。**

```
▶ AskUserQuestion この設定リポジトリで、次にどれに手を付けますか？
  ? この設定リポジトリで、次にどれに手を付けますか？ [次の作業]
```

| | 出すもの |
|---|---|
| `my:claude--tool-summary` | `tool_use` の ▶ 行。`questions` の**先頭の質問** |
| `my:claude--show-question` | 選択肢の見出しの `?` 行 |

**消すのは ▶ 行の側。** `?` 行は質問の全文が出る唯一の場所で、
そのすぐ下に選択肢が続くので、こちらを残すのが読みやすい。▶ 行には
`my:claude--question-headers` で**見出しだけ**を出す。

```
▶ AskUserQuestion [次の作業]
  ? この設定リポジトリで、次にどれに手を付けますか？ [次の作業]
    1. …
```

`my:claude-answer-questions` が nil のときと、`--permission-mode` や
`permissions.allow` で `can_use_tool` 自体が飛んで来ないときは `?` 行が
出ないが、見出し（12 文字以内）が残るのでツール名だけにはならない。

#### 【重要】一致で省く方式は使えない（2026-09-15 に失敗）

最初は**両方に `QUESTION [HEADER]` を組ませ、一致したら `?` 行を省く**
形にした。要約は 100 桁で `…` に切るので長い質問では一致しないが、
そのときは `?` 行に全文が出るので実害は無い、という読みだった。

**桁であって文字数ではない。** 日本語は 1 文字 2 桁なので、見出しを
入れると**実質 44 文字で切れる**。ふつうの長さの質問がほぼ全部
「一致しない」側に落ち、**直したつもりで何も変わっていなかった**。

### 答えは赤で出さない（`my:claude-answer-face`、2026-09-09）

deny で返す以上 `is_error` は必ず立つので、素直に扱うと
**自分で選んだ答えが `my:claude-error-face`（赤）で返ってくる**。
エラーではないのに失敗したように見えるので、緑にしてある。

色は **IME ON のときのカーソルと同じ `green`**（`my-japanese.el` の
`input-method-activate-hook`）。当初は `yellow green` にしていたが、
黄色に寄って見えたので純緑にした（2026-09-09）。明るい背景では純緑が
読めないので、そちらは `dark olive green` のままにしてある。

| | face |
|---|---|
| 聞いた直後の `→ 選んだ答え` | `my:claude-answer-face` |
| claude から返る `is_error` の `tool_result` | `my:claude-answer-face` |
| 他のツールの `is_error` | `my:claude-error-face`（赤のまま） |
| `my:claude-answer-questions` が nil のときの deny | `my:claude-error-face`（本物の拒否） |

判定は `my:claude--handle-user` で `tool-names` に覚えたツール名が
`AskUserQuestion` かどうかで行う。**畳む閾値も error と同じ扱いにする**
（`my:claude--fold` の `verbatim`）。既定の `0` のままだと、答えが
灰色の 1 行に畳まれて消える。

## 入力待ちは音で知らせる（`my:claude-notify-sound`、2026-09-10）

許可プロンプトと AskUserQuestion は claude 側の都合で突然ミニバッファを
開くので、別の窓を見ていると気づけない。鳴らすのはこの 2 つだけ
（`my:claude--notify-input-wait`）。`C-c a e` のような自分で始めた選択は
待っていると分かっているので鳴らさない。許可プロンプトでは `v`（入力を
全部見る）で聞き直すぶんも鳴らさない（ループの外で 1 回だけ呼ぶ）。

**`C-g` の音とは別のものを選ぶこと。** Windows の既定のビープは
レジストリの
`HKCU\AppEvents\Schemes\Apps\.Default\.Default\.Current` にあり、この
マシンでは `Windows Background.wav`。既定値はそれを避けて
`chimes.wav`（1.23 秒）にしてある。

### 【重要】同期再生してはいけない

| 鳴らし方 | Emacs が止まる時間 | |
|---|---|---|
| `play-sound-file` | **1.43 秒** | Windows は PlaySound を SND_SYNC で呼ぶ |
| `make-thread` + `play-sound-file` | **1.40 秒** | spawn は 0.1 ms だが逃げられない |
| **powershell の SoundPlayer に投げる** | **7.9 ms** | 鳴り始めるのは約 0.6 秒後 |

同期で鳴らすと**音が鳴り終わってからプロンプトが出る**。順序が逆で、
気づかせるという目的を果たさない。

**`make-thread` でも逃げられない。** 再生はグローバルロックを握ったまま
走るので、メインスレッドが入力を待った瞬間にそこで止まる（実測: スレッドを
起こした直後の `sleep-for 0.3` が **1.40 秒**かかった）。押したキーは
失われないが、1.3 秒のあいだ反応が返らない。

そのため `my:claude--play-sound` が子プロセスに投げる（macOS は `afplay`、
Linux は `paplay` / `aplay`）。どれも見つからない環境でだけ
`play-sound-file` に落ちる。powershell の起動 0.6 秒は待つのが向こうなので
こちらは止まらない。

音が読めない / 鳴らせないときは `message` で知らせるだけにする。
**プロンプトは必ず出す**（聞きそびれるのと、応答が止まるのとでは重さが違う）。

wav の長さも実測してある。`chimes.wav` 1.23 秒、
`Windows Notify System Generic.wav` 1.29 秒、
`Windows Information Bar.wav` 0.13 秒、`ding.wav` 0.40 秒。

## 会話バッファの markdown 装飾

`my:claude--fontify-markdown` が 3 つを順に行う。**この順でなければ
ならない。**

1. ``` のブロックを塗る。言語指定があればその言語として着色する
2. `|` の表を罫線に組み直す。1 の結果を見てコードブロックの中を避ける
3. 見出しと行中のコード

**font-lock は使わない。** このバッファは `special-mode` 派生で、挿入時に
`font-lock-face` を直に載せているため、font-lock を有効にすると
そちらに上書きされて競合する。ブロックが確定した時点で一度だけ塗る。

塗る位置は 2 か所ある。逐次表示の経路（`content_block_stop`）と、
delta が来ない経路（スラッシュコマンドの `assistant`）。
**どちらか片方だけだと `/context` の見出しが素のままになる。**

### コードブロックの言語別着色

一時バッファで該当モードを立てて `font-lock-ensure` し、付いた `face` を
`font-lock-face` としてコピーする（org の
`org-src-font-lock-fontify-block` と同じ手口）。フックは
`delay-mode-hooks` で走らせず、全体を `condition-case` で囲んである。

言語 → メジャーモードは **`markdown-get-lang-mode` を流用**する。
`<lang>-mode` / `<lang>-ts-mode` の推測と `fboundp` の確認までやって
くれるので、自前の `my:claude-lang-mode-alist` に書くのは名前が
一致しないもの（`elisp` `sh` `console` `json` …）だけで済む。

**背景色を消さないこと。** `my:claude-code-face` は背景しか持たないので、
構文の face と**並べてリストで**載せる。帰結として `font-lock-face` の
値がリストになるため、「コードブロックの中か」の判定を `eq` で
書けなくなる（`my:claude--code-face-p` を使う）。旧コードのまま
`(eq (get-text-property …) 'my:claude-code-face)` にしておくと、
**コードブロックの中の `# …` が見出しとして塗り直される。**

描画コストは GUI 実測で **250 行のコードブロック 1 個につき 15.8 ms**。
ブロックが確定した時点で 1 回だけなので詰まらない。上限は
`my:claude-fontify-code-max-lines`（既定 300）で押さえてある。

### 【重要】罫線の表は「罫線素片が 1 文字 2 桁」を勘定に入れる

markdown のパイプ表は罫線（box-drawing）の表に組み直す
（`my:claude-render-tables`、既定 t）。**桁は Emacs の規則で決める。**
`site-lisp/eaw.el` が ambiguous を幅 2 にし、`my-appearance.el` が
罫線素片（JIS X 0208）を HackGen に割り当てるので、**論理幅と実描画幅が
一致する**。claude 側の桁組みには合わせず、セルの中身だけを取り出して
`string-width` で組み直す。

> `my-pty`（端末）で ambiguous を幅 1 に切り替えているのとは**逆の話**。
> あちらは桁を数えているのが conhost なので合わせにいくが、
> こちらは Emacs 自身が数えるので合わせる必要が無い。

罠は列幅の刻み方にある。**セルの詰め物は半角空白（1 桁）だが、罫線は
1 文字で 2 桁ある。** 列幅 `w` に対して `(make-string (+ w 2) ?─)` と
書くと罫線の行だけが倍の長さになる。

```
幅= 44 |┌─────┬────────┬─────┐|   ← 5 文字 = 10 桁
幅= 26 |│ 列  │ 説明   │  値 │|   ← セルは 5 桁
```

`w + 2` が罫線 1 文字の桁数の倍数になるまで列幅を広げて直した。
倍数の判定に使う値は決め打ちせず `(char-width ?─)` を実測する
（eaw を外した Emacs では 1 になる）。

GUI 実測（`string-width` だけでは検算にならないので
`string-pixel-width` も見る）:

```
幅= 28 px= 224 |┌───┬────┬───┐|
幅= 28 px= 224 |│ 列   │ 説明   │   値 │|
幅= 28 px= 224 |├───┼────┼───┤|
幅= 28 px= 224 |│ a    │ あいう │    1 │|
幅= 28 px= 224 |│ bb   │ ○△□ │   22 │|
幅= 28 px= 224 |│ ccc  │ ─│   │  333 │|
幅= 28 px= 224 |└───┴────┴───┘|
```

全角・ambiguous・罫線素片を混ぜても全 7 行が一致する。
`┌┬┐├┼┤└┴┘│─` はすべて `char-width` 2 / 16px。

変換するのは**区切り行（`|---|:---:|`）を伴う表だけ**。無いと
`a | b` のような何気ない行まで拾う。

### 【重要】会話バッファへの書き込みは必ず `my:claude--at-end` を通す

このマクロが 3 つを引き受ける。**`insert` を直接書いてはいけない。**

1. 挿す先を区切りの手前（`my:claude--output-end`）にする
2. 書いたぶんを `my:claude--protect` で確定領域にする
   （read-only / keymap を付け忘れるとそこだけ編集できてしまう）
3. undo を汚さない（前掲）

インライン入力にする前は「末尾を見ている窓だけ `set-window-point` で
追従させる」仕掛けがここにあった。**いまは要らない**（挿入位置が `point`
より前なので勝手に追従する）。当時の教訓は残しておく価値がある:
`save-excursion` のマーカーは `insertion-type` が nil なので、**末尾での
挿入では挿入したテキストの前に取り残される**。2026-09-04 には
`my:claude--insert-diff` と `my:claude--end-paragraph` が直接書いていた
せいで「差分が 1 回出ると自動スクロールが止まる」状態になっていた。
書き込み口を 1 か所に寄せる理由はこれで、その必要は今も変わらない。

### `my:claude-mode` は markdown-mode 派生

入力エリアを markdown として書けるようにするため、会話バッファごと
`markdown-mode` から派生させてある（`special-mode` はやめた。read-only は
テキストプロパティで実現している）。コードブロックの着色は
`markdown-fontify-code-blocks-natively` に任せる。

**`markdown-mode-hook` は走らせない。** `my-text.el` の
`my:setup-markdown-mode` は `.md` ファイルを編集する前提の設定で、
会話バッファに持ち込む理由が無い。`define-derived-mode` は親を
`delay-mode-hooks` で包み、最後に `run-mode-hooks` が `run-hooks` で
回すので、モード本体で `(setq-local markdown-mode-hook nil)` すれば
親のフックだけを外せる。**`text-mode-hook` は潰さない**ので、
`display-line-numbers-mode` は `my:claude-mode-hook` で個別に切る
（親のフックのほうが先に走るため、モード本体で切っても間に合わない）。

`C-c C-c` は markdown 側では prefix だが、子のキーマップが先に引かれる
ので `my:claude-send` が勝つ。`completion-at-point-functions` の
`my:claude--capf`（深さ -100）は**必ず張り直すこと**（落とすと行頭の
`/` が `cape-file` に食われて C: 直下の一覧が出る）。

`markdown-mode` は autoload なので `my-claude.el` から
`(require 'markdown-mode)` する必要は無い。`define-derived-mode` は
親のキーマップを**モード関数の中で** `set-keymap-parent` する
（`derived.el` のコメントが「親がまだロードされていないことがある」と
明記している）。

## 画像を送る（`M-v`）

端末版の claude は `M-v` でクリップボードの画像を送れる。同じことを
stream-json 経路でやる。user メッセージの content は**ブロックの配列**
なので、Anthropic API と同じ形の image ブロックを混ぜればよい。

```json
{"type":"image","source":{"type":"base64","media_type":"image/png","data":"..."}}
```

**一時ファイルに保存して Read ツールに読ませる必要は無い。** それだと
1 往復とツールの許可が余計に要る。実測（`--model haiku`、320x160 の PNG を
`赤い円 + BANANA` で作って送った）:

| | |
|---|---|
| CLI に直接パイプ | 「赤い円と BANANA という英単語が描かれています」 |
| **Emacs が組み立てた JSON をパイプ** | 「赤いピンク色の円と、青色で『BANANA』と書かれています」 |

後者は `my:claude--user-content` が作った 2903 バイトの行をそのまま
`claude -p --input-format stream-json` に流したもの。日本語のプロンプトと
画像が同居しても壊れない（base64 は ASCII なので
`default-process-coding-system` の影響を受けない）。

### クリップボードからは `image/png` が直接取れる

**OS ごとの分岐は要らない。** Emacs 30 で MS-Windows も `yank-media` に
対応し、クリップボードの DIB を PNG に変換して提供する。実測
（Emacs 31.1 / Windows 11、PowerShell の `Clipboard::SetImage` で載せた）:

```elisp
(gui-get-selection 'CLIPBOARD 'TARGETS)
;; => [DataObject BITMAP System.Drawing.Bitmap Ole\ Private\ Data DIB image/png]
(gui-get-selection 'CLIPBOARD 'image/png)
;; => 1960 バイトの **unibyte** 文字列。先頭は "\211PNG\n\n"
```

そのまま `base64-encode-string` に渡せる（multibyte だと落ちるので
`my:claude--clipboard-image` は念のため `encode-coding-string` を通す）。
送れる型は API の制約で png / jpeg / gif / webp の 4 つだけ。

### 送るかどうかはバッファの中身で決める

`M-v` が入力エリアに挿すのは `[Image #1]` というプレースホルダで、
画像そのものはバッファローカルの `my:claude--input-images` が持つ。
**送信時に本文へ残っているプレースホルダだけを送る**
（`my:claude--input-attachments`）。

- 消せば取り消せる。添付の管理コマンドが要らない
- 並べ替えれば送る順も変わる（**添付リストの順ではなく本文の出現順**）
- 同じ番号を 2 回書いても 1 枚しか送らない

**消えたことを警告してはいけない。** 当初は送信時に「プレースホルダが
消えていた画像 N 枚は送っていない」と知らせていたが、**貼り直すには
「消す → 貼る」の 2 手が要る**ので、正常な操作のたびに必ず出る。
画像は送れているのに失敗したように見えるだけだった。

代わりに、新しく貼るときに**本文から消えている添付を捨てて番号を
詰め直す**（`my:claude--input-prune-images`）。`[Image #1]` を消して
貼り直せばまた #1 になる。掃除を「貼るとき」に限るのは、編集のたびに
やると undo で戻したプレースホルダの画像が失われるため。

content は「ラベル → 画像 → …→ 本文」の順に並べる。Anthropic の
ドキュメントが複数画像のときはラベルを付けて先に置くことを勧めており、
本文からも `[Image #1]` と同じ表記で参照できる。

```
[{"type":"text","text":"[Image #1]"}, {"type":"image",...}, {"type":"text","text":"本文"}]
```

**履歴に残すテキストからはプレースホルダを外す**
（`my:claude--strip-placeholders`）。`M-p` で呼び出しても画像は付いて
こないので、`[Image #1]` だけが claude に届いて話が食い違う。

### プレビューは overlay。テキストプロパティでは駄目

`display` でプレースホルダを画像に置き換えてしまうと、上の
「消せば取り消せる」が壊れる（文字が見えないものは消しにくいし、
1 文字消しても残りに `display` が残る）。overlay の `before-string` なら
文字の前にサムネイルが並ぶだけで編集の邪魔にならず、`evaporate` を
立てておけばプレースホルダを消したときに overlay も消える。

face も overlay に載せる。`markdown-mode` の font-lock は `[...]` を
参照リンクとして着色するが、overlay の face はその上に重なる。

サムネイルは `create-image` の `:max-height` で行数に合わせる
（入力エリア 2 行、送信後のエコー 8 行）。ImageMagick は要らない
（Emacs 27 以降はネイティブに拡縮する）。実測で 320x160 → 272x136。

### ログの base64 は落とす

`my:claude-log` が t のとき、画像を送ると数百 KB の base64 が
`*claude-log(PROJ)*` に残ってログが読めなくなる。`my:claude--log-line`
が 200 文字以上続く `"data":"..."` だけを `<2616 文字>` に潰す。

### `M-v` は cua に奪われない

`cua-mode` は `emulation-mode-map-alists` 経由なのでメジャーモードの
ローカルマップより先に引かれるが、`cua-global-keymap` の `M-v`
（`cua-scroll-down`）が出るのは**ローカルマップに `M-v` が無いとき
だけ**。実測（`cua-enable-cua-keys` は nil）:

| バッファ | `M-v` |
|---|---|
| `my:claude-mode`（入力エリア） | `my:claude-input-yank-image` |
| `org-mode` | `my:org-yank-image` |
| `fundamental-mode` | `cua-scroll-down` |

画面送りが要るときは `C-z`（`my-keybind.el`）。org で `M-v` を
潰しているのと同じ流儀。

### 上限は 3.5 MB（エンコード前）

API の上限は**base64 にしたあとで 5 MB** なので、生バイトではその 3/4 が
天井（`my:claude-image-max-bytes`）。超えたら `user-error` で断る。黙って
送っても API がリクエストごと弾くだけで、理由の分からないエラーが返る。

## セッションの再開とモデルの変更

| | |
|---|---|
| `--continue` | そのディレクトリの直近の会話を継ぐ。Emacs を再起動しても、端末で続けていた会話でも繋がる |
| `--resume <id>` | `session_id` を指定して継ぐ |

どちらも stream-json と併用できる（実測）。`init` イベントの `session_id` を
覚えているので、`C-c a m` は **`--resume` でモデルだけ差し替える**。
Opus と Haiku を行き来しても、それまでの話は消えない（実測で確認）。

**アカウントをまたぐ再開はできない。** セッションの保存先が
`CLAUDE_CONFIG_DIR` の下なので、`C-c a e` で環境を変えると会話は切れる。

## スラッシュコマンドの補完

`initialize` の control_response に `commands`（名前・説明・引数ヒント）が
入っている。実測で 52 個。これを覚えて入力エリアの `completion-at-point`
に流す（確定した会話の側では何も出さない）。

**行頭の `/` だけを対象にすること。** 文中のスラッシュまで拾うと
`src/foo` のようなパスを書くたびに候補が出て邪魔になる。
2 つめの `/` が来たらパスだと見なして手を引き、`cape-file` に譲る。

### 【重要】補完領域に先頭の `/` を含めること

`/` の**後ろ**から補完領域を始めると接頭辞の長さが 0 になり、
`corfu-auto-prefix`（`my-completion.el` で 1）に満たないという理由で
**corfu の自動補完に捨てられる**。捨てられると次の capf が呼ばれ、
深さ 90 にいる `cape-file` が `/` を絶対パスと解釈して
C: 直下のディレクトリ一覧を出す。実際にそうなっていた。

領域を `/` から取り、候補も `/name` の形にすれば接頭辞長が 1 以上になる。

| 入力 | |
|---|---|
| `/` | claimed（接頭辞長 1、候補 52、corfu の条件を満たす） |
| `/cont` | claimed（接頭辞長 5） |
| `/c/Projects/foo` | 手を引く（`cape-file` がパスとして扱う） |
| `see src/foo` | 手を引く |
| `/context and more` | 手を引く |

capf は深さ `-100` で入れて `cape-file`（90）より確実に先に来るようにしてある。

### 【重要】許可と拒否で control_response の形が違う

claude が返してくるエラーが契約を明示している。

```
Expected {behavior: 'allow', updatedInput?: object}
      or {behavior: 'deny', message: string}
```

**拒否に `updatedInput` を付けてはいけない。`message` は必須。**
どちらを外しても不正な応答と判定され、claude には「拒否された」ではなく
「許可フックでエラーが起きた」と伝わる。実測:

| 送った形 | claude が受け取った tool_result |
|---|---|
| `{deny, updatedInput}` | `The canUseTool callback returned an invalid permission result. …` |
| `{deny}` だけ | 同上 |
| **`{deny, message}`** | **その message がそのまま届く** |

**ツールが実行されない点はどれも同じなので気づきにくい。**
違いは claude への伝わり方だけで、不正な形だと
「システム側の問題です」と的外れな返事をしてくる。

許可プロンプトの `r`（理由を書いて拒否）はこの `message` に載る。
日本語もそのまま届く。「そのファイルは触らないで、代わりに…」と
書くと claude が別の手を考える。

### 【重要】スラッシュコマンドは `stream_event` を伴わない

`num_turns=0` で API を通らないため、**`assistant` で本文が来るのに
`stream_event` が 1 つも来ない**。実測:

| 入力 | イベント | assistant 本文 |
|---|---|---|
| `/context` | `assistant` `result` のみ | 6948 文字 |
| `/mcp` | 同上 | 98 文字 |
| `/usage` | 同上 | 855 文字 |
| 普通の質問 | `stream/*` が並ぶ | 135 文字 |

そのため「逐次表示が有効なら `assistant` の text は捨てる」としてはいけない。
`my:claude-stream` ではなく **そのブロックを実際に delta で出したか**
（`streamed-text` フラグ）で判断する。これを間違えると
**`/mcp` などが送信できたのに何も表示されない**。実際にそうなっていた。

スラッシュコマンドは API を消費しない（`$0.0000`）ので気軽に使える。
ただし `/mcp` は「詳細は端末の `/mcp` で」と要約を返すだけで、対話 UI は出ない。
`init` の `terminal_slash_commands`（`doctor` / `color` / `reload-plugins`）は
端末が要るもので、補完の注釈に `[端末専用]` と出るようにしてある。

## 【重要】Emacs から起動すると cwd のドライブレターが小文字になる

`.claude/settings.json` を置いてあるプロジェクトで `C-c a a` すると、
かつては会話バッファにこれが出ていた。

```
Ignoring 17 permissions.allow entries from .claude/settings.json:
this workspace has not been trusted. ...
set projects["c:/Projects/ESC-Web/WebCoreSystem_v1"].hasTrustDialogAccepted: true
```

原因は **Emacs が子プロセスの作業ディレクトリのドライブレターを小文字にする**こと。
実測（Emacs 31.1 / Windows 11）:

| 式 | 値 |
|---|---|
| `(expand-file-name "C:/Users/masao/.emacs.d/")` | `C:/Users/masao/.emacs.d/`（明示した大文字は保つ） |
| `(expand-file-name "~/.emacs.d/")` | **`c:/Users/masao/.emacs.d/`** |
| `(directory-file-name "C:/Users/masao/.emacs.d/")` | **`c:/Users/masao/.emacs.d`** |
| **子プロセスが見る cwd** | **`c:\Users\masao\.emacs.d`** |

`default-directory` を大文字にしても変わらない。`make-process` は
`directory-file-name` と同じ経路で作業ディレクトリを組み立てるので、
そこで小文字に落ちる。**Lisp 側に逃げ道は無い。**

一方、端末で対話的に起動した claude は大文字のまま記録するので、
`.claude.json` の `projects` に**大小 2 つのエントリができる**。

```
C:/Projects/ESC-Web/WebCoreSystem_v1   trusted=True    ← 端末の TUI が書いた
c:/Projects/ESC-Web/WebCoreSystem_v1   trusted=False   ← Emacs 経由で作られた
```

JSON のキーなので claude は別のプロジェクトとして扱う。信頼設定も
MCP サーバの設定も片方にしか効かない。gopls が大文字のドライブレターを
返して診断が出なかったのとまったく同じ罠。

**`~/.claude/projects/` のディレクトリ名は分かれない。** Windows の
ファイルシステムが大小を区別しないので、`c--…` を作ろうとしても既にある
`C--…` が再利用される。記録された `cwd` は小文字なのにディレクトリ名は
大文字、という状態になっていた。**分かれるのは `.claude.json` のキーだけ。**

`--settings` でファイルや JSON 文字列を明示しても回避できない（実測）。
`-p` は仕様として信頼ダイアログを出さない。

### cmd.exe の `cd /d` を挟んで大文字に揃える（2026-09-07）

`cmd.exe` の `cd /d` は**ドライブレターを大文字に正規化する**（残りの桁も
ディスク上の綴りに揃う）。そこを通して起こせば、端末から起動したときと
同じ cwd になる。

```
cmd.exe /d /c cd /d C:\Users\masao\.emacs.d && C:\Users\masao\.local\bin\claude.exe -p …
```

`my:claude--wrap-command` がこれを組み立てる（`my:claude-uppercase-cwd`、
Windows で既定 t）。実測（`tmp/` に作った新しいディレクトリで `-p` を 1 往復）:

| | 出来た `projects/` のディレクトリ | 記録された `cwd` |
|---|---|---|
| `my:claude-uppercase-cwd` = t | `C--…-cwd-probe-dir` | **`C:\Users\masao\.emacs.d\tmp\cwd-probe-dir`** |
| `my:claude-uppercase-cwd` = nil | `c--…-cwd-probe-dir2` | — |

- **引数はそのまま素通しされる。** 空白を含む引数も壊れない（同じ引数列を
  node に直接渡した場合と cmd.exe 越しの場合で `argv` が一致することを実測）
- **cmd.exe が解釈する文字（`& | < > ^ " %`）が引数にあれば包まない。**
  `my:claude-extra-args` には何でも書けるので、壊すより諦める
- **UNC パスでも包まない。** cmd.exe は UNC をカレントディレクトリにできない
- プロセスの木に cmd.exe が 1 つ挟まるが、cmd.exe は stdin を自分では
  読まないので stdin / stdout はそのまま claude に繋がる。EOF での終了も効く
- **パスを `directory-file-name` / `expand-file-name` で組み立てないこと**
  （上の表のとおり、そこで小文字に落ちる）。`my:claude--dos-path` が
  文字列として `/` → `\` の置換と末尾の除去をして、ドライブレターを大文字にする

`.claude.json` のキー（`my:claude--workspace-key`）と過去セッションの
置き場（`my:claude--session-directory`）も**同じ関数から採る**。
起こし方と綴りがずれると別のプロジェクトを指してしまう。
`my:claude-uppercase-cwd` を nil にすれば両方とも小文字に戻る。

**既にできてしまった小文字のエントリは消えない。** `.claude.json` の
`projects` に大小 2 つ並んでいるなら、小文字側の設定（MCP サーバ、信頼）を
大文字側へ移してから消す。

`C-c a t`（`my:claude-trust-workspace`）が
`projects[KEY].hasTrustDialogAccepted` を `t` にする。KEY は claude が
警告で言ってきたものをそのまま使う。**セッションを先に終了させてから
書く**（claude 自身がこのファイルを書き戻すため）。書き換え前に
`.claude.json.bak-my-claude-<時刻>` を作る。

書き戻しは `json-parse-buffer` → `json-serialize` の往復で行う。
69 KB の設定で検証したところ、差分は追加した 1 エントリのみで
`oauthAccount` を含め無傷だった。

> 検証で `equal` を使ってハッシュテーブルを比べてはいけない。
> **Emacs の `equal` はハッシュテーブルの中身を見ない**ので、
> 同一でも nil になる。中身を比べるなら serialize してから。

### 【重要】既定の環境には `CLAUDE_CONFIG_DIR` を「設定しない」

`~/.claude` を明示的に指定してはいけない。claude は
`$CLAUDE_CONFIG_DIR/.claude.json` を探すが、実体は `~/.claude.json` に
あるため見つからない。実測:

| | `email` / `orgName` | 標準出力 |
|---|---|---|
| 未設定（既定） | `ponkore@gmail.com` / 取れる | JSON のみ |
| `CLAUDE_CONFIG_DIR=~/.claude` | **どちらも `null`** | **警告が混ざる** |

警告は stderr ではなく**標準出力**に出るので、stream-json の途中に
非 JSON の行が混ざることになる。`my:claude-environments` では
既定の環境の CONFIG-DIR を `nil` にすること。

### 【重要】nil のときは「設定しない」ではなく「消す」

Emacs 自身が `CLAUDE_CONFIG_DIR` の設定された環境から起動されていると、
何もしなければそれを継承する。**「既定（Pro）」を選んだつもりで別の
アカウントに繋がる。** 実際に踏んだ（`personal` が `max` と表示された）。

`my:claude--process-environment` が `setenv` に nil を渡して
明示的に削除している。

## 【重要】起動オプションは 4 つとも省略できない

```
claude -p --verbose --input-format stream-json --output-format stream-json        --permission-prompt-tool stdio
```

| 省略すると | |
|---|---|
| `--verbose` | **即エラー終了**（`--output-format=stream-json requires --verbose`） |
| `--permission-prompt-tool stdio` | **許可要求が黙って自動拒否される** |

後者がとくに厄介。`--permission-prompts` の既定は `host`（= クライアントが答える）
なのに、このオプションが無いと `control_request` が**一度も飛んで来ず**、
`system/permission_denied` が流れてツールが実行されないだけになる。
実測では `Write` が拒否され、付けると `can_use_tool` が届いて許可でき、
ファイルが実際に作られた。**ツールが動かないときの第一容疑者。**

## 【重要】`default-process-coding-system` を束縛して起動する

`my-japanese.el` がグローバルの cdr を cp932 にしているため、束縛せずに
起動すると**標準入力の日本語が壊れる**。この経路は引数ではなく標準入力で
本文を渡すので、`(utf-8-unix . utf-8-unix)` でよい。
「引数は cp932」の話（別節）とは逆になる点に注意。

## 割り込んでもセッションは死なない

`{"type":"control_request","request":{"subtype":"interrupt"}}` を送ると
`control_response` が返り、続けて `result` が
`terminal_reason=aborted_streaming` / `is_error=true` で来る。
**プロセスは生きており、次のターンもそのまま送れる**（実測）。

`result` が `is_error` のときに EOF を送るとプロセスの終了コードは 1 になるが、
異常終了ではない。sentinel で騒がないこと。

## `system/init` はターンごとに来る

起動直後ではなく**最初のメッセージを送ったあとに来る。しかも毎ターン来る**。
バッファに挿すと会話の途中に何度も見出しが混ざるので、`header-line-format`
に出している。

## イベントは `assistant` だけ見れば表示できる

`--include-partial-messages` を付けると `stream_event` でトークン単位に
刻まれて来るが、`assistant` イベントがブロック確定ごとに丸ごと来るので、
逐次表示が要らないうちは `stream_event` を捨ててよい。

## 検証はプローブで安く

`--model haiku --tools ""` にする。Opus だと 1 往復で $0.83 かかった
（大半はシステムプロンプトのキャッシュ作成）。
`my:claude-log` を t にすると生の JSON Lines が残るので、
上流のイベント種別が変わったときに気づける。

