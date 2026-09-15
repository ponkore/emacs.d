<!-- -*- gfm -*- -->

# 対話 TUI を Emacs で動かす (`ptyd/` + `my-pty.el`)

ConPTY を持つ Go の常駐プロセスが VT を stdio で流し、表示は eat が受ける。文字幅とフォントの桁揃えが最大の難所。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

Windows の Emacs には PTY が無く `make-process` は常にパイプになるので、
対話 TUI が動かない。`ptyd`（Go）が疑似コンソールを持って子プロセスを
動かし、VT バイト列を stdio で Emacs に流す。表示は term.el に任せる。

```
Emacs ──stdin (JSON Lines)──> ptyd ──ConPTY──> 子プロセス
      <──stdout (生の VT)──        <─────────
      <──stderr (診断の行)──
```

| | |
|---|---|
| `M-x my:pty-build` | `ptyd.exe` を作る（`gitd` と同じく各マシンで） |
| `M-x my:pty-run` | 任意のコマンドを端末で動かす（汎用） |
| `M-x my:claude-term` | claude の TUI を開く |

**バイナリが無ければ `user-error` になるだけ**で、他の設定には影響しない。

stdin だけ JSON にしてあるのは、キー入力のほかに画面サイズを送る必要が
あるため。stdout を生のままにしてあるのは、そちらが本流で量が多く、
base64 と JSON のエスケープを挟む意味が無いから。

## 表示は eat (`my:pty-backend`、既定 `eat`)

term.el では通常の TUI がまともに映らなかった。代替画面 (`ESC[?1049`) も
同期出力も持たず、私用パラメータ付きの CSI (`ESC[>4;2m`) を SGR と
誤解釈する。`--ax-screen-reader` に逃がせば崩れないが、平板で読みにくい。

`eat`（NonGNU ELPA、純 elisp）に差し替えた。**通常モードの TUI が
そのまま出る。** term.el は `my:pty-backend` を `term` にすれば残っている。

| | term.el | eat |
|---|---|---|
| 代替画面 `?1049` | ✗（`?47` のみ） | ○ |
| bracketed paste `?2004` | ✗ | ○ |
| マウス `?1000`〜`?1006` | ✗ | ○ |
| `ESC[>4;2m` | **SGR 0;2 と誤解釈** | 私用パラメータとして別扱い |
| UTF-8 の復号 | `locale-coding-system` 決め打ち | 自前 |
| アプリへの書き込み | `process-send-string`（advice が要る） | **`input-function` パラメータ** |

最後の行が効いた。eat は端末→アプリの書き込みを `input-function` から
出すので、**term.el のときに必要だった advice が丸ごと不要**になる。

プロセスの符号化も逆になる。**eat は復号済みの文字列**を受け取る
（パーサが文字を比較する）が、**term.el は生バイト**を要求して復号を
自分でやる。`:coding` を切り替えている。

## 【重要】起動時のサイズはメジャーモードを立ててから測る

`eat-mode` も `term-mode` も `kill-all-local-variables` を通るので、
**先にヘッダ行や `truncate-lines` を設定しても消える**。実際に消えていた。

順序は「モードを立てる → ヘッダ行と `truncate-lines` → サイズを測る →
端末を作る」。行数は `window-body-height` ではなく
`(floor (window-screen-lines))` で採る（ヘッダ行と端数行を勘定に入れる）。
`pop-to-buffer` で別のウィンドウに移ることがあるので、そのあとにも
`my:pty--sync-size` を呼ぶ。

**ヘッダ行を立てるのはサイズを測る前。** あとから足すと使える行数が 1 減り、
疑似コンソールと Emacs の行数が食い違って、以後の描画が 1 行ずつずれる。

## 【重要】端末を開いている間は ambiguous 幅を 1 に切り替える

**これを入れないとロゴが横に伸び、表の罫線が揃わない。**

claude も conhost も East Asian Ambiguous を **幅 1** として桁を組むが、
`site-lisp/eaw.el` を入れた Emacs はそれらを幅 2 で描く。実測:

| 文字 | この設定 | `emacs -Q` + 日本語環境 |
|---|---|---|
| `█` U+2588（マスコット） | **2** | 1 |
| `▀` U+2580 | **2** | 1 |
| `─` U+2500（罫線） | **2** | 1 |
| `│` U+2502 | **2** | 1 |
| `·` U+00B7 | **2** | 1 |
| `★` U+2605 | **2** | 1 |
| `○` U+25CB | 2 | 2（組み込みでも幅 2） |

同じ画面を WezTerm で出すと正しく揃うので、**ずれているのは Emacs 側だけ**
だと切り分けられる。

`char-width-table` はグローバルで**バッファ単位に変えられない**ため、
`my:pty-narrow-ambiguous`（既定 t）が「最初の端末を開いたら全体を幅 1 に
切り替え、最後の端末を閉じたら戻す」形にしている。切り替えたときは
`message` で知らせる。復帰はプロセスの sentinel とバッファの
`kill-buffer-hook` の両方から呼ぶ。

他のバッファの桁揃えも端末を開いている間だけ変わる。それが困るときは
`my:pty-narrow-ambiguous` を nil にする（端末の見た目は崩れる）。

**`my:pty-narrow-ambiguous` は defcustom なので `M-x` では出てこない。**
開いている端末にその場で反映して見比べたいので、
`M-x my:pty-toggle-ambiguous-width` を用意してある。崩れの原因が eaw か
どうかは、これで切り替えて見比べるのがいちばん早い。

## 【重要】端末バッファでは折り返さない (`truncate-lines` = t)

**折り返すと 1 桁ずれただけで以後の行が全部ずれる。**

`my:pty-narrow-ambiguous` を nil にしたときや、幅の解釈が食い違う文字が
残っているときの保険。折り返すとレイアウトが崩れるが、`truncate-lines`
なら右端が切れるだけで格子は保たれる。

## `⏵` が `[]` になるのはフォントの問題

`glyphless-char-display` の extra slot 0 を eat が `empty-box` にしている
（`eat--setup-glyphless-chars`）。**幅の問題ではない**（U+23F5 は幅 1）。
そのコードポイントのグリフを持つフォントが無いだけ。豆腐ではなく
eat が意図して出している空の箱。

## 【重要】幅表だけでは足りない。フォントも切り替える

`char-width` を 1 にしても、**フォントがその文字を 2 桁ぶんの幅で描けば
見た目はずれる。** GUI では `string-pixel-width` が `char-width` ではなく
フォントの送り幅を返すことからも分かる。`M-x my:pty-toggle-ambiguous-width`
で画面が変わらなかったのはこれが理由。

原因は `my-appearance.el` の

```elisp
(set-fontset-font nil 'japanese-jisx0208 jp-fontspec)  ; jp-fontspec = HackGen
```

`─` (U+2500) は **JIS X 0208 の罫線素片**なので、この行で HackGen に
割り当てられ、全角 16px で描かれる。

実測（`:height` 116、半角 8px / 全角 16px の設定）:

| フォント | `a` | `あ` | `─` | `①` | `★` | |
|---|---|---|---|---|---|---|
| HackGen（通常） | 8 | 16 | **16** | **16** | 16 | 全部ずれる |
| HackGen Console NF | 8 | 16 | 8 | **16** | 16 | 丸数字が残る |
| **Consolas** | 8 | 16 | 8 | **8** | 16 | **最良** |
| HackGen35 Console NF | 8 | 16 | 11 | 9 | — | 3:5 設計で合わない |
| Cascadia Mono | 9 | 16 | 9 | 9 | — | 半角が 9px |

**`my:pty-console-font` の既定は nil（切り替えない）。** 切り替えると
`char-width-table` と同じく **Emacs 全体**のフォントが変わり、編集中の
バッファまで巻き込む。端末の見た目を優先したいときだけ Consolas にする。 Consolas は日本語を
持たないが、`あ` はフォントセットのフォールバックで全角のまま描かれる
（実測で 16px）。`my-appearance.el` のコメントにある「Consolas だと
丸付き数字が半角幅になってしまっている」は、通常の編集では困る挙動だが
**端末では逆にそれが正しい**（claude は `①` を 1 桁として桁を組む）。

**`★` (U+2605) と `※` (U+203B) は手元のどのフォントでも全角。**
これらを含む行だけは揃わない（未解決）。

端末を開いているあいだだけ差し替え、幅表と同じ寿命で、最後の端末を
閉じたら戻す。

### 【重要】`set-fontset-font` はこの設定では効かない

`nil`（選択フレーム）にも `t`（既定）にも入れ、`clear-face-cache` と
`redraw-display` まで呼んでも、GUI の実測で `font-at` は元のフォントを
返し続け `string-pixel-width` も 16 のままだった。丸数字のレンジだけ
別フォントに回そうとしても同じだった。

**実際に効く経路は `set-face-attribute 'default nil :family`**
（`my-appearance.el` の `emacs-font-setting` と同じ）。こちらに変えたら
1 回で通った。

**したがって端末用に選べるフォントは 1 つだけで、レンジごとの割り当ては
できない。** だから `★` を諦めてでも `①` が直る Consolas を選んでいる。

```
▐ U+2590 width=1 pixel=8 font=HackGen Console NF
```

`:height` は触らないこと。サイズが変わると桁が全部ずれる。

確かめ方（`*claude-term*` で）:

```elisp
(with-current-buffer "*claude-term*"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[─▐█①]" nil t)
    (goto-char (1- (point)))
    (let* ((c (char-after)) (f (font-at (point))))
      (list c (char-width c) (string-pixel-width (string c))
            (and f (font-get f :family))))))
```

`font=` が切り替え先になっていること、`pixel` が 8 であることを見る。

**Nerd Font のアイコン領域（#xe000-#xf8ff）は触らない。**
`my-appearance.el` がそこを別のフォントに回しており、上書きすると
アイコンが豆腐になる。

## 桁が合っているかの確かめ方

見た目が崩れていても、**バッファの中で桁が合っているかは別**。切り分けは
`eat--t-invisible-space` を除いた「見える文字列」の `string-width` を測る。
eat は全角文字の前に invisible な詰め物を入れるので、素の
`buffer-substring` の長さで測ると必ずずれて見える。

既知の表を `powershell -File` で流し込んで実測した結果（端末が生きている
間に測ること。終了すると幅表が戻る）:

| 流したもの | 期待 | 実測 |
|---|---|---|
| `ABCDEFGHIJKLMNOPQRST\|` | 21 | 21 |
| `あいうえおかきくけこ\|` | 21 | 21（filler 10） |
| `────────────────────\|` | 21 | 21 |
| `┌────┬────┐\|` | 12 | 12 |

claude 自身が組んだ表でも、罫線行と内容行が同じ幅になることを確認した。

```
幅= 21 |  │ cd   │ abcd     │|
幅= 21 |  ├──────┼──────────┤|
幅= 21 |  │ ef   │ あいう   │|
```

**つまり ptyd → conhost → eat → バッファは正しい。** 生の VT にも余計な
空白は入っていない（conhost は素通し）。それでも画面が崩れて見えるなら、
残るのは**フォントの描画**。`char-width` が 1 でも、フォールバックの
フォントがその文字を 1 桁ぶんの幅で描くとは限らない。CLAUDE.md の
eaw の節にある「1098 文字はプロポーショナルなフォールバックで描かれ、
`char-width` をどちらにしても桁は揃わない」と同じ話。

GUI での確かめ方:

```elisp
;; 半角 1 桁のピクセル幅と、罫線 1 文字のピクセル幅を比べる
(list (string-pixel-width "a") (string-pixel-width "─")
      (string-pixel-width "○") (string-pixel-width "あ"))
```

`a` が 8 なら、`char-width` 1 の文字は 8、2 の文字は 16 になっているのが
正しい。そうなっていない文字はフォールバックで描かれている。

## 【重要】eaw.el の文字幅表で eat が無限ループする

`site-lisp/eaw.el` が East Asian Ambiguous を幅 2 にしていると、
**eat が claude の TUI 出力の処理から戻ってこない**。実測（同じ 2385 文字を
流し込む）:

| | |
|---|---|
| `emacs -Q` | 完了 |
| `emacs -Q` + `(eaw-fullwidth)` | **戻ってこない** |
| 設定全体 | **戻ってこない** |
| 幅表を戻して流す | 完了 |

`my:pty--narrow-width-table` が `char-width-table` の複製を作り、
`east-asian-ambiguous` の文字を幅 1 に戻す。それを
`eat-term-process-output` と `eat-term-redisplay` の間だけ `let` で束縛する。

**そもそも桁を数えているのは conhost** であり、Windows のコンソールは
ambiguous を幅 1 として扱う。Emacs 側だけ幅 2 で数えると、ループしなかった
としても桁がずれる。端末の中では conhost に合わせるのが正しい。
バッファの外（通常の編集）には影響しない。

## 【重要】`setf (eat-term-parameter …)` は使えない

このリポジトリはバイトコンパイルしない方針なので、`setf` の展開は
**my-pty.el を読み込んだ時点**で起きる。そのとき eat はまだロードされて
おらず、gv のセッタが無いため `void-function \(setf eat-term-parameter\)`
になる。素の関数 `eat-term-set-parameter` を使う。

## 【重要】プリミティブへの advice は native-compile されたコードに効かない

term.el はキーを `process-send-string` で送るので、最初はそれを advice で
包んで JSON に変換しようとした。**まったく効かなかった。**

`term.eln`（native-compile 済み）は**プリミティブを直接呼ぶ**ので、
symbol の function cell に張った advice を素通りする。実際、生の
`echo …
` がそのまま ptyd に届いて
`bad line: invalid character 'e'` になった。

包むなら **Lisp の関数**にする。そちらは symbol 経由で呼ばれる。
term.el が書き込む入口は 4 か所しかない。

| 関数 | いつ通るか |
|---|---|
| `term-send-raw-string` | char モードのキー入力（ほぼ全部） |
| `term-send-string` | 貼り付けなど |
| `term-send-eof` | `C-d` 相当 |
| `term-emulate-terminal` の中 | `ESC[6n`（CPR）への応答。claude は送ってこない（実測 0 回） |

前の 3 つを `:around` で包んでいる。使っているセッションが無くなったら外す。

`my-gitd.el` が `magit-process-file` を、`my-lsp.el` が `eglot-uri-to-path` を
包んでいるのは、どちらも Lisp の関数なので問題ない。

## 以下は `term` バックエンド（退避先）の話

## 【重要】`locale-coding-system` をバッファローカルに上書きする

term.el は復号に `locale-coding-system` を決め打ちしている（31.1 で 5 箇所）。
日本語 Windows では cp932 なので、UTF-8 を吐く TUI の罫線が壊れ、
`args-out-of-range` で落ちる。`my:pty-run` が
`(setq-local locale-coding-system 'utf-8-unix)` を入れている。

## term.el が読めない CSI は ptyd 側で落とす

term.el はプライベートな CSI の目印として `?` しか見ていないため、
`ESC[>4;2m`（modifyOtherKeys）を `>` ごと数値化して SGR 0;2、つまり
「全属性リセット + faint」として実行してしまう。

`ptyd -strip-unsupported-csi` が `ESC[<` `ESC[>` `ESC[=` を落とす。
**`ESC[?` は落とさない**（term.el が正しく扱う）。実測:

| | バイト数 | `ESC[>` | `ESC[<` | `ESC[?` |
|---|---|---|---|---|
| strip なし | 852 | 7 | 3 | 22 |
| strip あり | 801 | 0 | 0 | 22 |

途中で切れたシーケンスは ptyd 側で持ち越す。ConPTY からの読み取りは
任意の位置で切れるので、1 回の Write に収まっている保証が無い。

## 端末経由だと信頼ダイアログが出る

`-p`（案 A）は仕様として信頼ダイアログを飛ばすが、**`my:claude-term` では
本来のダイアログが出る**。ここで `y` を押せば、Emacs 起動時の小文字
ドライブレターのキーで `hasTrustDialogAccepted` が立つので、
案 A 側の「permissions.allow が無視される」警告も消える。

## リサイズ

`window-size-change-functions` でウィンドウの桁数・行数を見て、
`term-reset-size` と `ResizePseudoConsole` の両方を更新する。
実測で `ESC[8;24;80t` が返り、その幅で再描画された。

