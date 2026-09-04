# my-claude 改善 第 1 弾

`user-lisp/my-claude.el` の改善要件をまとめたもの。**第 1 弾**の位置づけで、
第 2 弾以降は `emacs-claude-improve-02.md` に書く。

各項目はチェック項目とし、着手・完了に応じて `- [ ]` → `- [x]` にする。
実装しながら分かったことは各項目の「実装メモ」に追記していく
（この設定リポジトリの流儀にならい、**実測値と踏んだ罠を残す**こと）。

- 起草: 2026-09-04
- 2026-09-04: 論点への回答を反映（各項目の **決定** 行）。未決の論点は `論点` のまま残す
- 2026-09-04: **A〜E をすべて実装した。** 各項目の「実装メモ」に実測値と踏んだ罠を残した
- 2026-09-05: ヘッダ行を **5 列 + 色**にした。プロジェクト名を 2 列目に戻し、
  モデルの列に effort を出すようにした（A-2 / E-1 の実装メモに追記）
- 対象: `user-lisp/my-claude.el`
- 関連: `docs/claude/emacs-claude-stream-json-plan.md`（設計）、
  `docs/claude/emacs-claude-pty-proxy-study.md`（方式の比較検討）

---

## A. 起動

### - [x] A-1. 作業ディレクトリの決め方を見直す（projectile 最優先、次に cwd の `.claude/`）

**さかのぼりはしない。** Emacs らしく projectile の判定を最優先にする。

決定した優先順位:

1. **projectile のプロジェクトルート**が取れればそこ
2. 取れなければ、**cwd に `.claude/` があれば cwd**
3. どちらも外れたら **`y/n` で確認**してから起動する

- **要確認**: claude 自身の探索規則と食い違わないか
  （設定の読み込み・信頼判定・セッション記録の置き場がすべて cwd に紐づく）

| | |
|---|---|
| 現状 | `my:claude--project-directory`（`my-claude.el:214`）が projectile → project.el → `default-directory` の順で決める。`.claude/` は見ていない |
| **決定** | **projectile を最優先**する（現状どおり） |
| **決定** | projectile が外れたときに **cwd の `.claude/` を見る**。あれば cwd を使う。**上位へはさかのぼらない** |
| **決定** | どちらも外れたら、**そのディレクトリで起動してよいか `y/n` で確認**する |
| 影響 | cwd が変わるとセッション記録の置き場（`<CLAUDE_CONFIG_DIR>/projects/<エンコードしたパス>/`）も変わるため、`C-c a r` の一覧に出る対象も変わる |
| **決定** | フォールバックは **`default-directory` のみ**。`project.el` は見ない |
| **決定** | `y/n` を拒否したら `read-directory-name` で選ばせる（**起動は中止しない**） |

実装メモ:

判定を 2 つに分けた。

| 関数 | |
|---|---|
| `my:claude--guess-directory` | 自動で決まる場合だけ返す。決まらなければ nil。**確認は出さない** |
| `my:claude--project-directory` | 上が nil なら `y/n` → `read-directory-name` |

**分けないと「セッションを使い回すだけ」の場面で確認が出る。**
`my:claude--ensure-session` は、起動済みのセッションを別プロジェクトから
呼んだときに「cwd が違う」と知らせるためにもう一度ディレクトリを求める。
ここで `my:claude--project-directory` を呼ぶと、`.claude/` の無い
ディレクトリから `C-c a a` するたびに `y/n` が出ることになる。

`project.el` を外したのは、projectile が同じ役目を負っているため。
両方並べると「どちらが決めたのか」を説明できなくなる。

claude 自身の探索規則との食い違いは無い。claude は cwd を基準に
設定・信頼判定・セッション記録の置き場を決めるので、
**こちらが決めた cwd がそのまま claude の基準になる**。
逆に言うと cwd を変えると `C-c a r` の一覧の対象も変わるので、
黙って決めてよい範囲を projectile と `.claude/` の 2 つに絞ってある。

GUI 実測:

| `default-directory` | `my:claude--guess-directory` |
|---|---|
| `c:/Users/masao/.emacs.d/` | `c:/Users/masao/.emacs.d/`（projectile） |
| `c:/Users/masao/` | `c:/Users/masao/`（`.claude/` がある） |
| `%TEMP%` | **nil**（`y/n` に落ちる） |

### - [x] A-2. モードラインにプロジェクト名（プロジェクトフォルダ名）を表示する

- `*claude*` のモードラインに `[プロジェクト名]` を出す

| | |
|---|---|
| 現状 | `my:claude--mode-line`（`my-claude.el:1468`）は `mode-line-process` に `[- $0.12]`（busy と累計コスト）だけを出している |
| 現状 | ディレクトリは**ヘッダ行**に出ている。`my:claude--header`（`my-claude.el:548`）が `jighead(max) \| claude-opus-5 \| 5h 0% 7d 6% \| ~/.emacs.d` を組む |
| **決定** | **E-1 とセットで検討する**。この項目単独では決めない（表示場所が競合するため） |
| 2026-09-04 | ヘッダ行からディレクトリを外し、モードラインに移した |
| **2026-09-05** | **ヘッダ行の 2 列目にもプロジェクト名を出す**（下記） |

実装メモ:

`my:claude--mode-line` が `[.emacs.d $0.12]` を出す。応答待ちの間は
`[.emacs.d ... $0.12]`。**フルパスは `help-echo`** に入れてあるので、
マウスを載せれば分かる。

### 2026-09-05: ヘッダ行の 2 列目にもプロジェクト名を出す

当初は「ディレクトリを両方に出すと、いちばん幅を食う項目が二重になる」
という理由でヘッダ行から外していたが、**フルパスではなくフォルダ名だけ**
にすれば幅は 10 桁前後で済む。E-1 の 5 列（色付き）に組み込んだ。

モードライン側は残してある。**入力バッファ（`*claude-input*`）の
ヘッダ行はキーの案内**なので、そちらではモードラインが唯一の表示場所に
なる。B-1 のレイアウトでは入力バッファにカーソルがあることが多い。

どちらもフルパスは `help-echo` に入れてある。

### - [x] A-3. `C-c a a` で `*claude*` を裏に回し、`*claude-input*` を前面に出す

- 起動直後にカーソルが入力バッファにある状態にする

| | |
|---|---|
| 現状 | `my:claude`（`my-claude.el:1024`）は `pop-to-buffer` で**会話バッファ**を選択して終わる。入力バッファは `C-c a i`（`my:claude-input`）で別途開く |
| **決定** | **B-1 とセットで検討する**。レイアウト構築は B-1 / B-2 と同じ関数にまとめ、その最後にカーソルを `*claude-input*` へ置く |

実装メモ:

`my:claude` を 2 つに割った。

| | |
|---|---|
| `my:claude--ensure-session` | セッションを返すだけ。**ウィンドウは触らない** |
| `my:claude`（`C-c a a`） | 上を呼んでから `my:claude-layout` |

**割らないと、リージョン送信（`C-c a s`）でウィンドウが組み替わる。**
`my:claude-send-string` はセッションが無いとき自分で起こすが、そこで
`my:claude` を呼ぶと編集中のレイアウトが毎回壊れる。`--ensure-session`
に差し替えてある。`my:claude-send-region` は従来どおり `display-buffer`
だけにしてある。

`my:claude--restart` / `my:claude-switch-environment` /
`my:claude-continue` の `pop-to-buffer` もレイアウト呼び出しに替えた。
GUI 実測でカーソルは `*claude-input*` に入る。

---

## B. バッファ操作

### - [x] B-1. 起動時（`C-c a a`）に画面を上下 2 分割し、下半分に出力バッファ＋入力バッファを表示する

- 上半分: 元のバッファ（編集中のファイルなど）
- 下半分: `*claude*`（出力）と `*claude-input*`（入力）

実装メモ:

`my:claude-layout` が組む。B-1 / B-2 / B-3 / A-3 と B-4 の復帰先は
**全部この 1 つの関数**で、`window-configuration` は一切退避しない。

上半分に残すバッファは `my:claude--keep-buffer` が選ぶ。

1. いま見ているバッファが claude 系でなければそれ
2. 表示中の他のウィンドウのバッファ
3. `buffer-list` の先頭にある claude 系でないバッファ

**2 と 3 が要る。** `*claude*` や `*claude-input*` から `C-c a l` を
押したときに 1 が空振りする。ここで諦めると上半分が claude 系で
埋まって「2 分割の意味が無い」形になる。

フレームが低すぎて 3 分割できないときは `pop-to-buffer` に落とす。
`split-window` は小さすぎると `Window too small` を投げるので、
レイアウトを壊すより諦めるほうがよい。

### - [x] B-2. 同じレイアウトに一発で戻すグローバルキーを用意する

- グローバルな操作として、いつでも「2 分割＋出力＋入力」の状態に復帰できるようにする
- `C-c a` プレフィクスの空きに割り当てる（既存: `a e t c r m i s k q`）

実装メモ:

**`C-c a l`**（layout）に割り当てた。`my:claude-layout` は
`interactive` なので `M-x` からも呼べる。

`C-c a` プレフィクスの使用状況（この実装後）: `a e t c r m i s k q` に
`l` が加わった。

### - [x] B-3. 入力バッファは 5 行程度、入力＋出力で画面全体の高さの半分にする

- 入力は 5 行あれば足りる
- 出力＋入力の合計がフレーム全体の高さの約 1/2

実装メモ:

`my:claude-window-height-ratio`（既定 0.5）と
`my:claude-input-window-height`（既定 5）の 2 つの defcustom。

高さは `window-total-height` の `frame-root-window` から採る。
`window-body-height` ではない（**モードラインとヘッダ行を勘定に入れる**
必要がある。`my-pty` で同じ間違いをして 1 行ずれた）。

GUI 実測（フレーム 56 行）:

| ウィンドウ | 高さ |
|---|---|
| init.el（上半分） | 28 |
| `*claude*` | 23 |
| `*claude-input*` | 5 |

出力 + 入力 = 28 行 = フレームの 0.50。カーソルは `*claude-input*`。

### - [x] B-4. 入力／出力それぞれ、キー 1 つで最大化↔復帰をトグルする

- 出力を読み込みたいとき、長い入力を書きたいときに一時的に広げる
- もう一度押すと B-1 のレイアウトに戻る

| | |
|---|---|
| **決定** | **復帰先は B-1 の正規レイアウト**。トグル前の状態を復元するのではなく、毎回レイアウトを組み直す |
| 帰結 | `window-configuration` の退避は不要。**B-2 のレイアウト関数をそのまま呼べばよい** |
| **決定** | 最大化は `delete-other-windows`。上半分の元バッファは一時的に消えてよい |

実装メモ:

`my:claude-toggle-maximize` を **`C-c C-z`**（会話・入力の両方）と
**`z`**（`*claude*` のみ）に割り当てた。

```elisp
(cond
 ((one-window-p 'no-mini) (my:claude-layout))          ; 最大化中 → 復帰
 ((my:claude--buffer-p (current-buffer)) (delete-other-windows))
 (t (user-error "claude のバッファではない")))
```

「最大化中かどうか」は `one-window-p` で見る。トグル前の状態を
覚えないので、**どこから何度押しても同じ形に落ち着く**。

GUI 実測:

| | ウィンドウ数 | カーソル |
|---|---|---|
| `*claude*` で 1 回目 | 1 | `*claude*` |
| 2 回目 | 3 | `*claude-input*` |

---

## C. 入出力バッファの整形・カラーリング

### - [x] C-1. 出力バッファのコードブロックを言語判別して着色する

- ` ```elisp ` のような言語指定を読み、中身をその言語として着色する

| | |
|---|---|
| 現状 | `my:claude--fontify-markdown`（`my-claude.el:645`）はフェンス行を `my:claude-code-fence-face`、中身を `my:claude-code-face` の**単色**で塗る。言語名は読んでいない |
| 制約 | **このバッファでは font-lock を使えない**。`special-mode` 派生で挿入時に `font-lock-face` を直に載せているため、font-lock を有効にすると上書きされて競合する（`my-claude.el:645` の docstring、CLAUDE.md にも明記） |
| 方針 | 一時バッファで該当モードを立てて `font-lock-ensure` し、`face` / `font-lock-face` プロパティを**コピーして貼る**方式になる見込み |
| **決定** | 言語名 → メジャーモードの対応は**実装時に試行して決める**。`markdown-code-lang-modes` / `org-src-lang-modes` の流用可否も含めて手を動かしてから判断する |
| **決定** | 言語名 → メジャーモードは **`markdown-get-lang-mode` を流用**する。名前が一致しないものだけ自前の `my:claude-lang-mode-alist` に持つ |
| **決定** | 塗る契機は 2 か所とも `my:claude--fontify-markdown` を通る（**元から両方通っていた**）ので追加の対応は不要 |
| **決定** | コストは `my:claude-fontify-code-max-lines`（既定 300）で頭を押さえる |

実装メモ:

`my:claude--fontify-code` が一時バッファで該当モードを立てて
`font-lock-ensure` し、付いた `face` を `font-lock-face` として
コピーする。org の `org-src-font-lock-fontify-block` と同じ手口。

**背景色を消さないこと。** `my:claude-code-face` は背景しか持たない
ので、構文の face と**並べてリストで**載せる。

```elisp
(put-text-property beg end 'font-lock-face
                   (append (my:claude--face-list f) (list 'my:claude-code-face)))
```

帰結として `font-lock-face` の値が**リストになる**。そのため
「コードブロックの中か」の判定を `eq` で書けなくなり、
`my:claude--code-face-p` を用意した。**旧コードの
`(eq (get-text-property …) 'my:claude-code-face)` のままだと、
コードブロックの中の `# …` が見出しとして塗り直される。**

一時バッファではフックを走らせない（`delay-mode-hooks`）。
他人の設定がここで動く道理が無いうえ、`funcall` がエラーになると
会話の表示ごと止まる。全体を `condition-case` で囲んであり、
壊れたモードに当たっても単色のまま先へ進む。

言語 → モードは `markdown-get-lang-mode` に任せる。あちらが
`<lang>-mode` / `<lang>-ts-mode` の推測と `fboundp` の確認まで
やってくれるので、自前の `my:claude-lang-mode-alist` に書くのは
名前が一致しないもの（`elisp` `sh` `console` `json` …）だけで済む。
`markdown-mode` は autoload 済みなので、**必要になった時点で
読み込まれる**（起動時のコストにはならない）。

GUI 実測（`emacs-lisp-mode`、`(defun foo (x) "docstring" ; コメント …)`）:

| face | 文字数 |
|---|---|
| `my:claude-code-face` | 48 |
| `my:claude-inline-code-face` | 13 |
| `font-lock-doc-face` | 11 |
| `my:claude-code-fence-face` | 11 |
| `my:claude-heading-face` | 8 |
| `font-lock-keyword-face` | 5 |
| `font-lock-comment-face` | 5 |
| `font-lock-function-name-face` | 3 |
| `font-lock-comment-delimiter-face` | 2 |

描画コスト（`elisp` のコードブロック 1 個、`my:claude--fontify-markdown`
の全体を計測）:

| ブロックの行数 | 時間 |
|---|---|
| 50（初回。モードのロードを含む） | 2.9 ms |
| 250 | **15.8 ms** |
| 350（上限 300 を超えて着色を飛ばす） | 0.5 ms |

**250 行で 16 ms。** ブロックが確定した時点で 1 回だけ走るので、
`magit-refresh-buffer` の 50 ms（`my-gitd` 段階 2b）より軽い。
詰まる心配は要らないと判断した。上限は保険として残す。

### - [x] C-2. 表っぽいものを罫線表示に変換する

- markdown のパイプ表（`| a | b |`）を検出して、罫線（box-drawing）の表として描き直す

| | |
|---|---|
| **決定** | **桁数は Emacs バッファの eaw ルール（`site-lisp/eaw.el`、ambiguous = 幅 2）で決める。claude 側の桁組みには合わせない** |
| **決定** | 描画は他の一般バッファと同じ扱い。**HackGen を等幅フォントとみなす前提**でよい |
| **決定** | したがって**罫線（box-drawing）への変換を行う**。空白パディング案は採らない |
| 根拠 | この前提なら**論理幅と実描画幅が一致する**。罫線素片（`─` `│` など）は JIS X 0208 にあり `my-appearance.el` が HackGen に割り当てるので全角 16px = 2 桁で描かれ、eaw が与える `char-width` 2 と揃う |
| 対比 | `my-pty`（端末）で ambiguous を幅 1 に切り替えているのは、**桁を数えているのが conhost 側**だから。C-2 は桁を数えるのが Emacs 自身なので**逆に揃えなくてよい**（`my-pty.el` の該当節と混同しないこと） |
| **決定** | セルの中身だけを取り出して `string-width` で組み直す（元の桁は使わない） |
| **要注意** | **罫線素片は 1 文字で 2 桁ある。** 列幅をそのまま罫線の文字数にしてはいけない（下記） |

実装メモ:

`my:claude--render-tables` が、区切り行（`|---|:---:|`）を伴うパイプ表
だけを罫線に組み直す。区切り行を必須にしているのは、`a | b` のような
何気ない行まで拾わないため。コードブロックの中は触らない
（`my:claude--code-face-p` で判定するので、**C-1 のフェンス処理を
先に済ませておく必要がある**）。

寄せ方（`:---` / `---:` / `:---:`）とエスケープした `\|` に対応する。

### 【重要】罫線素片は 1 文字で 2 桁ある

最初の実装は列幅 `w` に対して `(make-string (+ w 2) ?─)` を書いていた。
**罫線の行だけが倍の長さになる。** `─` は JIS X 0208 の罫線素片なので
`site-lisp/eaw.el` が幅 2 を与え、`my-appearance.el` が HackGen に
割り当てるので実描画も全角 16px になる。

```
幅= 44 px= 352 |┌─────┬────────┬─────┐|   ← 5 文字 = 10 桁
幅= 26 px= 208 |│ 列  │ 説明   │  値 │|   ← セルは 5 桁
```

セルの詰め物は半角空白（1 桁）なので列幅はどんな値でも組めるが、
**罫線側は 2 桁単位でしか刻めない**。そこで `w + 2` が罫線 1 文字の
桁数の倍数になるまで列幅を広げる。倍数の判定に使う値は決め打ちせず
`(char-width ?─)` を実測する（eaw を外した Emacs では 1 になるので、
そのときは何も広げない）。

```elisp
(rw (max 1 (char-width ?─)))
(+ w (mod (- rw (mod (+ w 2) rw)) rw))
```

### GUI 実測

`string-width` だけでは検算にならないので `string-pixel-width` も見る。

```
幅= 28 px= 224 |┌───┬────┬───┐|
幅= 28 px= 224 |│ 列   │ 説明   │   値 │|
幅= 28 px= 224 |├───┼────┼───┤|
幅= 28 px= 224 |│ a    │ あいう │    1 │|
幅= 28 px= 224 |│ bb   │ ○△□ │   22 │|
幅= 28 px= 224 |│ ccc  │ ─│   │  333 │|
幅= 28 px= 224 |└───┴────┴───┘|
```

**全 7 行が 28 桁 / 224px で一致した。** セルに全角（`あいう`）、
組み込みでも幅 2 になる ambiguous（`○△□`）、罫線素片そのもの
（`─│`）を混ぜても崩れない。

各文字の実測:

| | `a` | `あ` | `─` | `│` | `○` | `┌┬┐├┼┤└┴┘` |
|---|---|---|---|---|---|---|
| `char-width` | 1 | 2 | 2 | 2 | 2 | — |
| `string-pixel-width` | 8 | 16 | 16 | 16 | 16 | すべて 16 |

**論理幅と実描画幅が一致している**ので、決定どおり eaw のルールで
組んでよい。

### - [x] C-3. 入力バッファも markdown 扱いにし、コードブロックを言語判別して着色する

| | |
|---|---|
| 現状 | `my:claude-input-mode`（`my-claude.el:1485`）は `text-mode` 派生 |
| **決定** | **`markdown-mode` 派生にする。ただし `markdown-mode-hook` は走らせない** |
| 帰結 | 着色は `markdown-fontify-code-blocks-natively` に任せる。C-1 の仕掛けは使わない |

実装メモ:

```elisp
(define-derived-mode my:claude-input-mode markdown-mode "Claude-Input"
  ...
  (setq-local markdown-mode-hook nil)
  (setq-local markdown-fontify-code-blocks-natively t)
  (add-hook 'completion-at-point-functions #'my:claude--capf -100 t))
```

### `markdown-mode-hook` をバッファローカルに nil にすれば走らない

`define-derived-mode` は親を `delay-mode-hooks` で包み、最後に
`run-mode-hooks` が溜まったフックを `run-hooks` で回す。`run-hooks` は
**そのバッファの値**を見るので、モード本体で `setq-local` すれば
親のフックだけを外せる（ローカル値に `t` が無ければグローバル値も
見ない）。

`my-text.el` の `my:setup-markdown-mode` は「`.md` ファイルを編集する」
前提の設定なので、送信用の一時バッファに持ち込む理由が無い。将来
`my-text.el` を触ったときにこちらの挙動が黙って変わるのも避けたい。

### `C-c C-c` は衝突しない

`markdown-mode-map` では `C-c C-c` は prefix（`markdown-mode-command-map`）
だが、`my:claude-input-mode-map` が子で親より先に引かれるので
`my:claude-input-send` が勝つ。

### markdown-mode は autoload なので起動コストにならない

`define-derived-mode` は親のキーマップを**モード関数の中で**
`set-keymap-parent` する（`derived.el:268` のコメントが「親がまだ
ロードされていないことがある」と明記している）。したがって
`my-claude.el` の読み込み時に `markdown-mode` は要らない。
`(require 'markdown-mode)` をトップレベルに置く必要は無かった。

### GUI 実測

| | |
|---|---|
| `major-mode` | `my:claude-input-mode` |
| `(derived-mode-p 'markdown-mode)` | `markdown-mode` |
| `C-c C-c` | `my:claude-input-send` |
| `M-p` | `my:claude-input-previous` |
| `completion-at-point-functions` の先頭 | `my:claude--capf` |
| `markdown-mode-hook`（ローカル値） | `nil` |
| `font-lock-mode` | `t` |
| ```` ```elisp ```` の中の `defun` の face | `(font-lock-keyword-face markdown-code-face)` |

---

## D. claude からの出力

### - [x] D-1. 出力中はバッファ終端が必ず見えるように自動スクロールする

- 常に末端の更新状態が見えるのが望ましい

| | |
|---|---|
| 現状 | `my:claude--insert`（`my-claude.el:467`）は「**末尾を見ていたときだけ**追従する」。読み返し中に飛ばされるのを避けるための意図的な仕様 |
| 現状 | 追従判定に**バッファの `point`** を使っている（`(>= (point) (point-max))`）。ウィンドウが複数あるとき `window-point` とずれる余地がある |
| **決定** | **`point` が末尾にあるときだけ自動スクロールする。読み返し中（`point` が末尾にない）は追従しない** |
| 帰結 | **これは現状の仕様そのもの**。要件文の「必ず表示」は「末尾にいる限り必ず」の意味と解釈する。defcustom での切り替えは設けない |
| **結果** | 破綻していた。**判定をウィンドウごとの `window-point` に変えた**（下記） |

実装メモ:

「末尾を見ているときだけ追従する」という仕様は変えていない。
**判定の粒度だけを直した。**

旧実装はバッファの `point` 1 つで決めていた。

```elisp
(at-end (and (get-buffer-window buf) (>= (point) (point-max))))
...
(when at-end
  (dolist (w (get-buffer-window-list buf nil t))
    (with-selected-window w (goto-char (point-max)))))
```

**判定も追従も全ウィンドウ一括**なので、会話バッファを 2 つの
ウィンドウに出したときに次の壊れ方をする。

| | 旧 | 新 |
|---|---|---|
| 片方が末尾、片方が読み返し中 | **読み返し中の窓も末尾へ飛ぶ** | 末尾の窓だけ追従 |
| 片方が読み返し中で、バッファの `point` がそこ | **末尾を見ている窓が追従しない** | 末尾の窓は追従 |

B-1 のレイアウトでは `*claude*` が同時に 2 か所へ出ることが普通に
起きる（最大化トグルの前後、`display-buffer` の再利用）。

新実装は `window-point` でウィンドウごとに判定し、
`set-window-point` でそのウィンドウだけ進める。

バッファ自身の `point` も別に見る。`save-excursion` は挿入前の位置に
戻す（マーカーの `insertion-type` が nil）ので、**末尾にいたぶんは
明示的に `goto-char` しないと追従が切れる**。ウィンドウに出ていない
間に届いたぶんでこれが切れると、次に表示したときに古い位置から
始まってしまう。

GUI 実測（`*claude*` を 2 つの窓に出し、片方を先頭・片方を末尾に置いて
2 行挿入）:

| | |
|---|---|
| 読み返し中の窓の `window-point` | **1**（動かない） |
| 末尾の窓の `window-point` | **11** = `point-max` |

### - [x] D-2. 折りたたみ対象はデフォルトで全て畳む

- 現状は先頭 N 行を見せて残りを畳んでいる。これを**最初から全部畳む**
- 見たいときは `TAB` で開く想定

| | |
|---|---|
| 現状 | `my:claude-tool-result-max-lines`（`my-claude.el:112`）の既定は **12**（要件文の「20 行？」は 12 行が正しい） |
| 現状 | `my:claude--fold`（`my-claude.el:494`）が先頭 12 行＋`… 残り N 行 (TAB で全体を表示)` を出す |
| 現状 | `my:claude-toggle-fold`（`my-claude.el:1435`）は**別バッファ** `*claude tool output*` に全文を出す。トグル（閉じる）ではなく「別窓で見る」実装 |
| **決定** | 畳んだ行には**1 行要約**を出す（`● Read(foo.el) … 42 行` のような形）。`my:claude--tool-summary`（`my-claude.el:805`）を使う |
| **決定** | `TAB` は**現状どおり別バッファ方式**（`*claude tool output*` に全文）。その場で展開する方式には変えない |
| **決定** | Edit/Write の差分は**この一律折りたたみの対象外**。従来どおり `my:claude-diff-max-lines`（既定 30、`my-claude.el:786`）まではその場に表示する |
| **決定** | **Edit/Write の差分に `TAB` で別バッファを開く仕様は設けない**。30 行を超えたら行数を知らせるだけにする。全体の差分が見たければ**自分で `git diff` する** |
| **要修正** | 上の決定に伴い、`my:claude--show-edit`（`my-claude.el:799`）が出す `(差分 %d 行。TAB で全体を表示)` から **TAB の案内を消す**。そもそもこの関数は `my:claude-full` プロパティを設定していないため、現状 TAB を押しても `ここには折りたたまれた出力が無い` になる。**案内が嘘になっている** |
| **決定** | 行数は **tool_result のテキストの行数**（`split-string` して数える） |
| **追加** | **エラーは畳まない**（`my:claude-error-result-max-lines`、既定 30）。理由は下記 |

実装メモ:

`my:claude-tool-result-max-lines` の既定を **12 → 0** にした。
0 なら `(<= n 0)` がどんな結果にも成り立たないので常に畳まれる。
変数は残してあるので、戻したければ元の値を入れればよい。

畳んだ行:

```
  ● Read(user-lisp/my-claude.el) … 42 行
```

`Read(...)` の中身は `my:claude--tool-summary` の結果。**`tool_result`
には入力が入っていない**ので、`tool_use` を受けた時点で名前と一緒に
要約も覚えておく必要がある。`tool-names` ハッシュの値を
`NAME` から `(NAME . SUMMARY)` の cons に変えた。

### エラーだけは畳まない

一律に畳むと**「なぜ失敗したか」がその場から消える**。雑音を減らす
という目的とは逆に、いちばん見たいものが隠れることになる。
`my:claude-error-result-max-lines`（既定 30）行までのエラーは
そのまま出す。ビルドの失敗などで数百行来ることがあるので上限は設ける。

出力が空の結果（`(Read: 出力なし)`）も畳まない。畳んでも意味が無い。

### `my:claude-full` を載せる範囲

旧実装は `(put-text-property (line-beginning-position 0) (point-max) …)`
で、`my:claude--insert` が `save-excursion` を使うため
**`point` がどこにあるかに依存する**書き方だった。要約行 1 行だけに
載せるよう、`point-max` から `forward-line -1` で確実に求めている。

### `my:claude--show-edit` の嘘の案内を消した

`(差分 %d 行。TAB で全体を表示)` → `(差分 %d 行。git diff で確認)`。

この関数は `my:claude-full` を設定しないので、TAB を押しても
`ここには折りたたまれた出力が無い` になるだけだった。

### GUI 実測

```
  ● Read(user-lisp/my-claude.el) … 42 行     ← my:claude-full あり
  エラー 1 行目                               ← my:claude-full なし
  エラー 2 行目
```

---

## E. ステータスライン

### - [x] E-1. `statusline-command.sh` の表示内容をモードラインに出せないか検討する

`~/.claude/statusline-command.sh` が端末の TUI に出している項目を、
Emacs 側のモードライン（またはヘッダ行）で再現できるかを調べる。

**確認した: `statusLine` は `-p`（stream-json）経路では発火しない。**
プローブ（`claude -p --output-format stream-json`）の出力 8 行を
すべて調べたが、statusline に由来する行は 1 つも無い。端末 TUI の
機能なので当然といえば当然。したがって「スクリプトの出力をもらう」
のではなく **同じ情報を Emacs 側で自前に組み立てる**方向で実装した。

スクリプトの表示項目と、Emacs 側で取れるかの見立て:

| 項目 | スクリプトでの取得元 | Emacs 側 |
|---|---|---|
| プラン名 | `PLAN` 変数（ベタ書き） | 取れる。auth cache の `subscriptionType`（ヘッダ行に既出） |
| CONFIG_DIR 名 | `$CLAUDE_CONFIG_DIR` の basename | 取れる。`my:claude-session-name`（ヘッダ行に既出） |
| claude のバージョン | `claude --version`（1 時間キャッシュ） | **`system/init` の `claude_code_version` に入っていた**。別プロセスは要らない |
| カレントディレクトリ | `.workspace.current_dir` | 取れる。`my:claude-session-directory`（ヘッダ行に既出）。**A-2 と重複** |
| git ブランチ | `git symbolic-ref` | **載せない**（決定）。取ること自体は可能だがプロセス起動のコストがある（CLAUDE.md「`call-process` が Windows で遅い」）。必要になったら別途検討 |
| モデル名 | `.model.display_name` | 取れる。`init` イベント（ヘッダ行に既出） |
| effort level | `.effort.level` | **stream-json には出てこない**（プローブで全イベントの全キーを列挙して確認）。載せられない |
| コンテキスト使用量 (%) | transcript の `usage` を集計、無ければ `.context_window` | 取れる。`assistant` の `message.usage` の 3 つの和。上限は `result` の `modelUsage.<model>.contextWindow` |
| レート上限 % / リセット時刻 | `.rate_limits.five_hour` | % は取れる（ヘッダ行に 5h / 7d を既出）。**`resets_at` は現状使っていない**ので追加できる |
| 累計コスト / 経過時間 | `.cost.total_cost_usd` / `.total_duration_ms` | 取れる。`result` の `total_cost_usd`（モードラインに既出）。経過時間は未使用 |

| | |
|---|---|
| **決定** | **ヘッダ行を主に使う**。モードラインは幅が狭いので**あまり使わない**方向で設計する |
| **決定** | **git ブランチは載せない**。載せたくなったら別途検討する |
| **決定** | A-2（プロジェクト名の表示）は**この項目と一緒に設計する** |
| 2026-09-04 | ヘッダ行は 4 項目（アカウント + バージョン / モデル / ctx / レート上限） |
| **2026-09-05** | **5 列にして色を付けた**。2 列目にプロジェクト名、3 列目に effort（下記） |
| **決定** | モードラインは `mode-line-process` のまま。`doom-modeline-def-segment` は書かない |

実装メモ:

### `-p` 経路では statusLine は発火しない（実測）

`claude -p --verbose --input-format stream-json --output-format stream-json
--model haiku --tools ""` に 1 問投げて、返ってきた 8 行の全キーを
列挙した。届くイベントは次のとおり。

```
system/init
system/thinking_tokens ×3      ← 現状は使っていない
assistant ×2
rate_limit_event
result/success
```

statusline に由来するものは無い。

### stream-json から取れたもの

| 項目 | 取得元 | 実測値 |
|---|---|---|
| claude のバージョン | `system/init` の `claude_code_version` | `"2.1.260"` |
| コンテキスト使用量 | `assistant` の `message.usage` の `input_tokens` + `cache_read_input_tokens` + `cache_creation_input_tokens` | `10 + 0 + 103226` |
| コンテキスト上限 | `result` の `modelUsage.<model>.contextWindow` | `200000` |
| 5h のリセット時刻 | `rate_limit_event` の `unifiedWindows.five_hour.resetsAt` | `1788544800`（Unix 秒） |

**`claude --version` を別に呼ぶ必要は無かった。** statusline スクリプトが
1 時間キャッシュまでして避けていたプロセス起動が、`system/init` に
最初から入っている。

コンテキストの上限を決め打ちしないのも大事で、`modelUsage` が
モデルごとに返してくる。1M 版なら `1000000` が来るはずなので、
statusline スクリプトのようにモデル名から `*1M*` を判定する必要が無い。

### 取れなかったもの

**effort level は stream-json に出てこない。** 全イベントの全キーを
再帰的に列挙して確認した。`system/init` には `permissionMode` /
`output_style` / `fast_mode_state` はあるが `effort` は無い。

→ **2026-09-05 に別経路で出せるようにした。** 下の「effort level」を参照。

### モードライン

`[.emacs.d ... $0.12]`。doom-modeline はモードラインをまるごと差し替える
が、`mode-line-process` は `doom-modeline` の `process` セグメントが
そのまま拾うので、素の変数に載せるだけでよい。セグメント名がバージョンで
変わる問題（CLAUDE.md 既出）も踏まない。

---

## 2026-09-05 の追加（E-1 の続き）

### ヘッダ行を 5 列にして色を付けた

```
jighead(max) v2.1.260 | .emacs.d | claude-opus-5 (high) | ctx 103.2k 52% | (5h 4%)(7d 8%)(reset 09/05 03:00)
```

| 列 | 内容 | 色 | face |
|---|---|---|---|
| 1 | アカウント（プラン）と claude のバージョン | マゼンタ | `my:claude-header-plan-face` |
| 2 | プロジェクト名（フルパスは `help-echo`） | シアン | `my:claude-header-dir-face` |
| 3 | モデルと effort | イエロー | `my:claude-header-model-face` |
| 4 | コンテキスト使用量 | グリーン | `my:claude-header-context-face` |
| 5 | レート上限とリセット時刻 | シアン | `my:claude-header-limit-face` |

色は `~/.claude/statusline-command.sh` が端末の TUI で使っている ANSI 色に
合わせた。`:foreground` だけを指定する。ヘッダ行では `header-line` face が
下地になり、テキストプロパティの face はその上に重なるので、背景は
テーマのものがそのまま残る。GUI 実測で 108 桁。

### 【重要】`%` の escape は列ごとに、色を付ける前に済ませる

`header-line-format` に素の文字列を渡しているので `%` は mode-line の
書式指定子として解釈される（`0e8badd` で踏んだ。`5h 4% 7d 8%` が
`5h 47d 8` になっていた）。escape 自体はやめられない。

問題は**掛ける場所**。組み立てた全体に `replace-regexp-in-string` を
掛けると、**差し込まれる `%%` だけが face を持たない**素の文字列になり、
その桁で色が切れる。`my:claude--header-segment` が
「escape してから `propertize`」の順で 1 列を作る。

検算は `format-mode-line` で行う。**「組み立てた文字列」ではなく
「実際に表示される文字列」を見ないと、`%` の扱いも face の生き死にも
分からない。** GUI 実測:

```
組み立て: … | ctx 103.2k 52%% | (5h 4%%)(7d 8%%)(reset 09/05 03:00)
表示後  : … | ctx 103.2k 52%  | (5h 4%)(7d 8%)(reset 09/05 03:00)

my:claude-header-plan-face       |jighead(max) v2.1.260|
nil                              | | |
my:claude-header-dir-face        |.emacs.d|
nil                              | | |
my:claude-header-model-face      |claude-opus-5 (high)|
nil                              | | |
my:claude-header-context-face    |ctx 103.2k 52%|
nil                              | | |
my:claude-header-limit-face      |(5h 4%)(7d 8%)(reset 09/05 03:00)|
```

色の付かない区間は区切りの ` | ` だけ（これは `header-line` の色で
出るのが正しい）。`help-echo` も `format-mode-line` を通って残る。

### effort level

**stream-json は effort を返さない**ので、次の順で求める
（`my:claude--effort`）。

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

**`.claude.json`（信頼判定などが入るファイル）とは置き場が違う。**
あちらは `~/.claude.json` だが、`settings.json` は
`~/.claude/settings.json`。`my:claude--config-json` と
`my:claude--settings-files` で組み立て方を分けてある。

**`modelSettings` のキーは前方一致で突き合わせる。** キーは
`claude-opus-5` のように日付が付かないが、`system/init` が返すモデル名は
`claude-haiku-4-5-20251001` のように日付付きのことがある。

GUI 実測（`~/.claude-config/jighead/settings.json` は
`effortLevel: high` と `modelSettings.claude-fable-5.effortLevel: xhigh`）:

| `model` | 結果 |
|---|---|
| `claude-opus-5` | `high`（`modelSettings` が一致） |
| `claude-fable-5` | **`xhigh`**（`modelSettings` が一致） |
| `claude-haiku-4-5-20251001` | `high`（一致せず、上位の `effortLevel`） |
| `nil` | `high`（同上） |
| `my:claude-effort` = `"max"` | **`max`**（defcustom が最優先） |

effort は**毎ターン求め直さない**。`system/init` はターンごとに来るが、
settings.json を 3 つまで読むファイル I/O が要るのに対し、モデルが
変わらない限り結果は変わらない。**判定はモデルを更新するより先に
行うこと**（更新してしまうと「変わったかどうか」が分からなくなる）。

---

## 全体にかかる確認事項

- [x] A-1 の作業ディレクトリの決め方が claude 自身の規則と食い違わないか確認する
      → claude は cwd を基準に決めるので、こちらが決めた cwd がそのまま基準になる。食い違わない
- [x] E-1 の「`statusLine` は `-p` 経路では発火しない」を確認する → **発火しない**
- [x] E-1 の effort level / コンテキスト使用量が stream-json から取れるか調べる
      → コンテキスト使用量は取れる。**effort level は取れない**
- [x] C-1 / C-2 の描画コストを実測する（長い出力で会話バッファが詰まらないか）
      → 250 行のコードブロックで 15.8 ms。ブロック確定時に 1 回だけなので詰まらない
- [x] C-2 の桁揃えを `string-pixel-width` で実測する（`string-width` だけでは検算にならない）
      → 全行 28 桁 / 224px で一致。**罫線素片が 1 文字 2 桁である点を踏んだ**
- [x] D-2 に伴い、`my:claude--show-edit` の「TAB で全体を表示」という嘘の案内を消す
- [x] 検証は **GUI で行う**。フォント幅・doom-modeline・ウィンドウ分割はいずれも batch では評価されない
      → `emacs -l probe1.el` で GUI 起動して結果をファイルに書き出す形にした

## 第 2 弾以降に回すもの

（ここに、第 1 弾の実装中に出てきたが今回はやらないと決めたものを書き足す。
まとまったら `emacs-claude-improve-02.md` に移す）

- **`system/thinking_tokens` を使っていない。** `-p` 経路で毎ターン数回
  届き、`estimated_tokens` / `estimated_tokens_delta` を持つ。思考中の
  進み具合をヘッダ行やモードラインに出せる。現状は `_ nil` で捨てている
- **経過時間（`result` の `duration_ms` の累計）を出していない。**
  statusline スクリプトは `$0.12 / 3m` の形で出している。1 ターンぶんは
  会話に出しているが、セッションの累計は持っていない
- **`result` の `permission_denials` / `subagent_stats` を使っていない。**
  サブエージェントを何個起こしたか、どこで拒否されたかが入っている
- **C-2 の表は区切り行を必須にしている。** 区切り行の無いパイプ表は
  変換されない。誤変換を避けるためにわざとそうしているが、必要になったら
  「2 行以上続く」などの条件を足す余地がある
- **C-1 の言語判別は `markdown-get-lang-mode` 任せ。** 対応していない
  言語（`mermaid` など）は単色のまま。必要になったら
  `my:claude-lang-mode-alist` に足す
