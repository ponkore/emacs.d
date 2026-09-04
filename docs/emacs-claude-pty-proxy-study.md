# Emacs から Claude Code を使う — PTY プロキシ方式の実現可能性検討

2026-09-04 調査。着手判断のための資料であって、実装計画ではない。

すべて実測に基づく。プロキシは実際に書いて動かした（付録 A）。

## 結論

**3 つの道があり、いちばん安い道は「PTY を作らない」道だった。**

| 案 | 中身 | プロキシ | 実現性 | 工数感 |
|---|---|---|---|---|
| **A. stream-json** | `claude -p --input-format stream-json` を素のパイプで駆動 | **不要** | **実証済み**（Emacs から複数ターン往復まで確認） | Elisp のみ。小 |
| **B. ConPTY プロキシ** | Go/Rust の常駐プロセスが疑似端末を持ち、生 ANSI を stdio で中継 | 必要（PoC 済み） | **実証済み**（TUI 描画・キー入力とも成功） | プロキシ + 端末エミュ調整。大 |
| **C. B + `--ax-screen-reader`** | 同じプロキシで claude をスクリーンリーダーモードで動かす | 必要（B と同じ） | **実証済み**（term.el でエラー 0） | B と同じだが端末側の難所が消える |

- **「Emacs のバッファで Claude と対話したい」だけなら A。** プロキシを書く必要がそもそも無い。
- **「claude の TUI をそのまま Emacs に映したい」なら C から入る。** B の課題の大半が消える。
- **B を汎用 PTY プロキシとして作る価値は別にある**（claude 以外の対話 CLI にも効く）が、
  claude のためだけに作るなら費用対効果は悪い。

## 検証環境

| | |
|---|---|
| OS | Windows 11 Pro 10.0.26200.9168 |
| Emacs | 31.1（GUI / batch とも） |
| Claude Code | 2.1.260（`C:\Users\masao\.local\bin\claude.exe`、218 MB のネイティブ単一バイナリ） |
| Go | 1.27.1 windows/amd64（`golang.org/x/sys` はモジュールキャッシュにあり、オフラインでビルド可） |
| Rust | cargo 1.98.0（`gitd/` で使用中） |

---

## 前提の確認 — Windows の Emacs に PTY は無い

事実関係を先に固めた。

- Windows の Emacs は `make-process` が常にパイプを使う。`process-connection-type` は無視される
  （`emacs -Q` の既定値は `t` だが、これは Unix でのみ意味を持つ）
- claude は **stdin が TTY でないと自動的に `--print` モードへ落ちる**。実測:

  ```sh
  echo "" | claude
  # => Error: Input contained only whitespace. Provide a prompt with text through
  #    stdin or as a prompt argument when using --print
  ```

  「TUI が化ける」のではなく「TUI に入らない」。よって**対話 TUI を出すには本物の端末が要る**、
  という前提は正しい。

ここまでは想定どおり。ただし、その先で前提が 1 つ崩れた。

---

## 案 A: そもそも PTY を使わない（推奨）

### claude には双方向のストリーミング JSON 入出力がある

`claude --help` より:

```
--input-format <format>    Input format (only works with --print):
                           "text" (default), or "stream-json" (realtime streaming input)
--output-format <format>   Output format (only works with --print):
                           "text" (default), "json" (single result), or "stream-json"
```

これは Agent SDK が使っている経路で、**端末を一切必要としない**。
`--output-format stream-json` は `--verbose` が必須（無いとエラーで即終了する）。

### Emacs から駆動できることを実測した

`emacs -Q --batch` から `make-process`（`:connection-type 'pipe`）で起動し、
1 プロセスに 2 ターン投げた。

```elisp
(make-process
 :name "claude" :connection-type 'pipe :noquery t
 :command (list "c:/Users/masao/.local/bin/claude.exe"
                "-p" "--verbose"
                "--input-format" "stream-json"
                "--output-format" "stream-json"
                "--model" "haiku" "--tools" "")
 :filter #'my-filter)
```

送るのは 1 行 1 JSON:

```json
{"type":"user","message":{"role":"user","content":[{"type":"text","text":"..."}]}}
```

実測結果:

| 観点 | 結果 |
|---|---|
| プロセス起動・接続 | OK |
| 受信イベント列 | `rate_limit_event` → `system`×5 → `assistant`×2 → `result` を 2 ターンぶん |
| **複数ターンの継続** | **OK**（2 ターン目の返答が 1 ターン目の内容を参照していた） |
| **日本語の往復** | **OK**（`表計算・髙﨑・①②` を含む送信、日本語の返信とも正しく復号） |
| stdin を閉じたとき | プロセスが正常終了（`SENTINEL: finished`） |

`default-process-coding-system` を `(utf-8-unix . utf-8-unix)` に束縛して起動した。
**この経路では cp932 に落とす必要は無い**（引数ではなく標準入力で本文を渡すため。
CLAUDE.md の「引数は cp932」の話とは別経路）。

### 受け取れる情報

`result` イベントだけでもこれだけ入っている:

- `result`（本文）、`session_id`、`num_turns`、`is_error`
- `total_cost_usd`、`usage`（入出力トークン、キャッシュ読み書き）、`modelUsage` の内訳
- `duration_ms` / `ttft_ms` / `duration_api_ms`
- `permission_denials`

`system`/`init` イベントには `tools` 一覧、`mcp_servers` の接続状態、`slash_commands`、
`model`、`permissionMode`、`cwd` が入る。**モードラインに出したい情報がそのまま取れる。**

さらに使える系統（今回は未検証）:

- `--include-partial-messages` … トークン単位のストリーミング（タイプライタ表示に必要）
- `--forward-subagent-text` … サブエージェントの出力も流す
- `capabilities: ["interrupt_receipt_v1", "interrupt_cancel_queued_v1", "msg_lifecycle_v1"]`
  が `init` に来ていた。割り込み（`C-g` 相当）に対応する制御要求がある

### この案の位置づけ

- **プロキシが要らない**。Rust も Go も書かない。`user-lisp/my-claude.el` 一枚で済む
- ANSI を解釈する必要が無い。`comint` ですら要らず、普通のバッファに好きな体裁で描ける
- Emacs 側の UI を自由に作れる（org 風、magit 風、いずれも可）
- **移植性がある。** macOS / Linux でも同じコードが動く

制約:

- claude の TUI 固有の UI（`/` メニュー、ファイル補完、差分表示、`shift+tab` のモード切替）は
  自分で作ることになる
- 権限プロンプトは TUI が出してくれない。`--permission-mode` で決め打ちするか、
  制御プロトコルで自前実装するか（`--permission-prompt-tool` / MCP）を選ぶ必要がある
- 対話 UI をゼロから作る以上、「claude 本体の UI 改善に自動で追従する」ことはない

---

## 案 B: ConPTY プロキシ（PoC を実装して検証した）

### ConPTY とは

Windows 10 1809 以降の `CreatePseudoConsole` / `ResizePseudoConsole` / `ClosePseudoConsole`。
Unix の PTY 相当で、conhost が端末エミュレータ役として **VT シーケンスを生成してくれる**。
Windows 11 なら確実に使える。

Go なら `golang.org/x/sys/windows` に全部揃っている（外部依存ゼロ、既にキャッシュ済み）。
Rust なら `portable-pty`（wezterm）や `conpty` crate。

### PoC: 約 150 行の Go で動いた

`conpty-probe.exe`（付録 A）。stdin を疑似端末へ、疑似端末の出力を stdout へ中継するだけのもの。

```
Emacs ──pipe──> conpty-probe ──ConPTY──> claude.exe
      <─pipe───              <─VT bytes─
```

### 【重要】`STARTF_USESTDHANDLES` を立てないと静かに壊れる

最初の実装は**子プロセスが疑似端末に attach するのに、出力が 1 バイトも出てこなかった**。
2 時間近く溶かしたので記録しておく。症状:

| 観測 | |
|---|---|
| conhost の起動シーケンス（`ESC[?9001h` 等） | 出る |
| **コンソールタイトル**（`ESC]0;...cmd.exe`） | **出る**（= 子は確かに疑似端末に attach している） |
| 子プロセスの出力 | **1 バイトも出ない** |
| `cmd.exe /c ver` の終了コード | **1**（`ver` が失敗している） |
| `cmd.exe /c "mode con > file"` | **ハングする** |

原因は ConPTY ではなく `CreateProcess` の標準ハンドルの規則だった。
`bInheritHandles = FALSE` かつ `STARTF_USESTDHANDLES` 無しで起動すると、
**子は親の標準ハンドルの「値」をそのまま受け取る**。親（プロキシ）の標準入出力が
パイプ（Emacs に繋がっている）だと、継承していないその値は子にとって無効なハンドルになる。
結果、子はコンソールに attach しているのに書き込み先が壊れている、という状態になる。

対策は `STARTF_USESTDHANDLES` を立て、**3 つのハンドルを NULL のままにする**こと。
こうすると ConPTY が疑似端末のハンドルを割り当てる。1 行:

```go
si.StartupInfo.Flags |= windows.STARTF_USESTDHANDLES   // 3 ハンドルは 0 のまま
```

これを入れた瞬間に `cmd /c ver` が正しい VT を吐いた。

> 教訓: **自分で ConPTY を書くなら、実績のあるライブラリ**
> （`portable-pty` / `UserExistsError/conpty` / `aymanbagabas/go-pty`）**を使うこと。**
> 罠が「エラーを出さずに沈黙する」形をしている。

### claude の TUI は ConPTY プロキシ越しに完全に動いた

`conpty-probe.exe -cols 100 -rows 30 -- claude.exe` で起動し、生バイトを採取した。

- ロゴ、バージョン、モデル名、cwd、ステータスライン、入力ボックスすべて描画された
- **12 秒後に `HELLO-FROM-EMACS` を送り込んだら、TUI の入力欄にそのまま現れた**
  （= キー入力の往復も成立）
- 25〜30 秒のアイドルで **3439 バイト**。帯域は問題にならない

つまりユーザーの挙げた 4 つの問い（起動できるか / シーケンスを伝えられるか /
バッファで再現できるか / Enter で送れるか）のうち、**前 2 つは解決済み**。

### claude の TUI が要求する端末機能（実測）

採取したバイト列を数えた。

| 機能 | シーケンス | 出現数 |
|---|---|---|
| 代替画面 | `ESC[?1049h` | 1 |
| 同期出力 | `ESC[?2026h/l` | 10 |
| 24bit カラー | `ESC[38;2;R;G;Bm` | 26 |
| 絶対カーソル位置 | `ESC[row;colH` | 16 |
| マウス追跡 | `ESC[?1000h`〜`?1003h`, `?1006h` | 6 |
| kitty キーボードプロトコル | `ESC[>5u` / `ESC[<u` | 4 |
| modifyOtherKeys | `ESC[>4;2m` | 4 |
| bracketed paste | `ESC[?2004h` | 2 |
| フォーカス通知 | `ESC[?1004h` | 3 |
| テーマ変更通知 | `ESC[?2031h` | 3 |
| 端末バージョン問い合わせ | `ESC[>0q` | 2 |
| UTF-8 の罫線・パワーライン記号 | — | 非 ASCII 1322 バイト |

`ESC[c`（DA1）や `ESC[6n`（カーソル位置問い合わせ）は**来なかった**。
つまり **Emacs 側が応答を返さなくてもハングしない**。ここは楽な材料。

---

## Emacs 側で端末を再現できるか — term.el を実測した

採取した生バイトを `term-emulate-terminal` に 1024 バイトずつ流し込んで、
バッファに何が残るかを見た。

### 素の状態: レイアウトは出るが日本語が壊れ、3 回エラーになる

```
;; fed 3439 bytes -> 464 chars, 55 lines
;; term-width=100 term-height=30 errors=3
;; CHUNK 1024-2048 ERROR: (args-out-of-range #("...(charset cp932-2-byte)) 0 -70)
;; CHUNK 2048-3072 ERROR: (args-out-of-range #("...(charset katakana-sjis)) 0 -110)
;; CHUNK 3072-3439 ERROR: (args-out-of-range "7m" 0 -110)
```

レイアウト自体（`Claude Code v2.1.260` / `Opus 5 (1M context) ... Claude Max` / `~\.emacs.d`）は
正しい桁位置に出ていた。壊れていたのは罫線とアイコンで、`charset cp932-2-byte` が示すとおり
**UTF-8 のバイト列を cp932 として復号していた**。

### 【重要】term.el は `locale-coding-system` を決め打ちしている

`term.el` は復号に `locale-coding-system` を**ハードコードしている**（31.1 で 5 箇所）:

```elisp
(decode-coding-string ... locale-coding-system t)
```

`set-process-coding-system` は効かない。日本語 Windows では `locale-coding-system` が
cp932 なので、**UTF-8 を吐く現代的な TUI は軒並み壊れる**。
CLAUDE.md にある `default-process-coding-system` の話とは別の穴。

対策はバッファローカルに上書きすること。これで**エラーが 0 になり、描画も正しくなった**:

```elisp
(setq-local locale-coding-system 'utf-8-unix)
```

実測（同じ 3439 バイト、`errors=0`）:

```
 ▐▛███▛█   Claude Code v2.1.260
▝▜██████▀  Opus 5 (1M context) with high effort · Claude Max
  ▝▝ ▝▝    ~\.emacs.d
────────────────────────────────────────────────────────────────────────────────
❯ HELLO-FROM-EMACS
────────────────────────────────────────────────────────────────────────────────
  ⚠ Transcript saving is off — inherited CLAUDE_CODE_CHILD_SESSION marker …
  Claude Max(jighead) | .emacs.d | master | Opus 5 (1M context)(high) | ctx 0 (0%)
  ⏵⏵ auto mode on (shift+tab to cycle)
```

**ロゴも罫線も入力欄も、送り込んだ文字列も、そのまま出た。**
つまり 3 つ目の問い（バッファで再現できるか）の答えは **「概ねできる。ただし要修正」**。

### term.el が解さないもの

| シーケンス | term.el | 影響 |
|---|---|---|
| `ESC[?1049h`（代替画面） | **非対応**（`?47` のみ実装） | 画面が通常バッファに直接描かれ、スクロールバックが混ざる |
| `ESC[?2004h`（bracketed paste） | 無視 | 貼り付けが 1 文字ずつのキー入力扱いになる |
| `ESC[?1000h`〜`?1006h`（マウス） | 無視 | マウス操作不可（実害小） |
| `ESC[?2026h/l`（同期出力） | 無視 | 描画途中の状態が見える（ちらつき） |
| `ESC[?2031h`（テーマ通知） | 無視 | 実害なし |
| `ESC[>0q`（バージョン問い合わせ） | 無応答 | 実害なし（claude 側は待たない） |

未知のプライベートモードは黙って捨てるので、**落ちはしない**。

### 【重要】`ESC[>4;2m` が「文字装飾」として誤解釈される

term.el の CSI パーサは、プライベートプレフィクスとして **`?` しか見ていない**（`term.el:3259`）:

```elisp
(let* ((private (string-prefix-p "?" ctl-params))
       (params (mapcar #'string-to-number (split-string ...))))
```

claude が出す `ESC[>4;2m`（modifyOtherKeys=2）は `>` が剥がされず、
`string-to-number ">4"` が **0** になる。結果 `SGR 0;2` = **「全属性リセット + faint」**として
実行される。今回の採取では 4 回出ていた。

`ESC[>5u` / `ESC[<u`（kitty キーボード）は最終文字が `u` で term.el に処理がないため無視される
（安全側）。**危ないのは `m` で終わるものだけ**。

これは term.el を使うなら **advice で `>` / `<` / `=` 始まりの CSI を捨てる**必要がある、
という具体的な作業項目になる。

### term.el 以外の選択肢（未検証）

- **eat**（Emulate A Terminal）… 純 Elisp。term.el より新しく、対応シーケンスが広いとされる。
  端末エミュレータ部（`eat-term`）がプロセスから独立していて、
  **バイト列を流し込むだけで使える設計**なので、このプロキシ方式と相性が良さそう。
  ただし straight に未導入で、**今回は一切検証していない**
- **vterm**（emacs-libvterm）… C モジュール + libvterm。**自前で PTY を握る実装**なので、
  Windows では素直に使えない。プロキシと組み合わせるには改造が要る。現実的でない

---

## 案 C: `--ax-screen-reader` — B の難所をほぼ全部消す

claude には**スクリーンリーダー向けの出力モード**がある。

```
--ax-screen-reader   Render screen-reader friendly output
                     (flat text, no decorative borders or animations).
```

同じプロキシで比較採取した結果:

| | 通常 | `--ax-screen-reader` |
|---|---|---|
| バイト数（同条件） | 3439 | **834**（約 1/4） |
| 代替画面 `?1049h` | 1 | **0** |
| マウス追跡 | 6 | **0** |
| 同期出力 `?2026` | 10 | **0** |
| 24bit カラー | 26 | **0** |
| 絶対カーソル位置 | 16 | 4 |

term.el に流した結果（`errors=0`）:

```
[Screen Reader Mode: on via flag]
Claude Code v2.1.260
Opus 5 (1M context) with high effort · Claude Max
~\.emacs.d
⚠ Transcript saving is off — inherited CLAUDE_CODE_CHILD_SESSION marker …
Claude Max(jighead) | .emacs.d | master | Opus 5 (1M context)(high) | ctx 0 (0%) | $0.00 / 0m
auto mode on (shift+tab to cycle)
/rc
$  HELLO-AX
```

**上から下に流れる平文になり、shell-mode の体験にそのまま近い。**
代替画面もカラーも同期出力も消えるので、term.el の弱点の大半が問題にならなくなる。

B に着手するなら **まず C で作り、必要になってから通常モードに広げる**のが筋が良い。

---

## 課題の整理

### ユーザーが挙げた 4 点

| # | 課題 | 結果 |
|---|---|---|
| 1 | プロキシから claude を起動できるか | **できる**。ConPTY で PoC 実装・実行済み |
| 2 | claude のシーケンスを Emacs に伝えられるか | **できる**。生バイトを stdout に流すだけ。DA1/CPR の応答要求も来ないので単方向で足りる |
| 3 | Emacs のバッファで端末を再現できるか | **概ねできる**。term.el で描画確認済み。ただし `locale-coding-system` の上書きと `>` 始まり CSI の除去が必須 |
| 4 | Enter でバッファから claude に送れるか | **できる**。送り込んだ文字列が TUI の入力欄に現れることを確認済み |

### まだ潰していない課題

| 課題 | 状況 | 難易度 |
|---|---|---|
| **ウィンドウサイズの伝達** | `ResizePseudoConsole` を呼ぶには**制御チャネルが要る**（下記）。未実装 | 中 |
| **`C-g` / 割り込み** | ConPTY には `GenerateConsoleCtrlEvent` があるが、プロキシ経由だと制御チャネル越しに頼むことになる。gitd で同じ問題（`C-g` で git が止まらない）を未解決のまま抱えている | 中 |
| **日本語入力（IME）** | tr-ime の変換中の文字を TUI にどう渡すか。確定後に bracketed paste で流すのが素直だが、term.el が `?2004` を解さないので 1 文字ずつ送ることになる。**未検証** | 中〜大 |
| **キーの衝突** | `term-char-mode` では Emacs のキーバインドがほぼ全部 TUI に持っていかれる。`C-x` だけ残す等の妥協が要る。`C-h` を `delete-backward-char` にしている本設定とは特に相性が悪い | 中 |
| **文字幅（East Asian Ambiguous）** | claude は罫線・アイコンを多用する。`site-lisp/eaw.el` が `char-width` を 2 にしている文字を conhost が幅 1 で数えていると**桁がずれる**。CLAUDE.md の実測で「1098 文字はどちらにしても桁が揃わない」と分かっている領域に直撃する。**未検証だが、いちばん嫌な課題** | 大 |
| **スループット** | 応答ストリーミング中は今回のアイドル計測（3.4 KB）より桁違いに多い。term.el の再描画は速くない。`--ax-screen-reader` なら 1/4 になる | 中 |
| **認証・セッション** | `claude --resume` / `--continue` をどう UI に出すか。`init` イベントに `session_id` が来るので案 A なら簡単 | 小 |
| **プロキシが無い環境** | gitd と同じく「バイナリが無ければ従来動作」に倒す。案 A があるので**フォールバック先が用意できる**のは強み | 小 |

### 制御チャネルの設計（B / C を進める場合）

stdout を生 VT に、stdin を生キーに使い切ってしまうと、**リサイズと割り込みを送る口が無くなる**。
選択肢:

1. **stderr を制御用に使う** … `make-process` の `:stderr` に別プロセス/バッファを割り当てられる。
   実装は最も簡単だが、プロキシ自身のエラー出力と混ざる
2. **stdin にフレーミングを入れる** … 制御フレームを特殊バイトで包む。
   生キー入力（任意のバイト列）と衝突するのでエスケープが必要
3. **stdio 全体を JSON Lines にする** … 端末バイト列は base64 で包む。
   **`gitd/` が既に jsonrpc でやっていること**なので、設計・実装ともそのまま流用できる。
   オーバーヘッドは gitd の実測で往復 0.13 ms なので無視できる

**3 が良い。** `my-gitd.el` の知見（jsonrpc、サーキットブレーカ、フォールバック、
`process-environment` に JSON に載らないバイトが混ざる問題、`expand-file-name` の必要性）が
そっくり効く。既に一度通った道になっている。

---

## 既存資産との関係

このリポジトリには **同じ形の常駐プロセスを既に 1 つ運用している**（`gitd/`）。
そこで得た知見はそのまま使える。

| gitd で分かっていること | このプロキシでの意味 |
|---|---|
| jsonrpc over stdio が Emacs と Rust の間で安定に動く | 制御チャネルの設計がそのまま流用できる |
| `process-environment` に JSON 化できないバイトが混ざる（`PSModulePath`） | 環境変数を渡すなら同じ対策が要る |
| 引数は ANSI コードページで復号し直す必要がある | 同上 |
| バイナリが無ければ黙って従来動作に戻す | 「プロキシが無い環境では案 A に落ちる」が作れる |
| `default-directory` は `expand-file-name` する | 同じ |
| **`C-g` で子プロセスが止まらない**（未解決） | 同じ問題を引き継ぐ |

一方で **gitd と決定的に違う点**がある。gitd は「速くするだけで、壊れても素の
`process-file` に戻せば済む」ものだった。PTY プロキシは**それ無しでは機能そのものが無い**ので、
フォールバック先が「機能が使えない」になる。案 A を先に作っておくと、
ここが「機能が劣化する」で済むようになる。

---

## 工数感（勘）

| | 内訳 | 規模 |
|---|---|---|
| **案 A** | `my-claude.el` 1 枚。プロセス管理 + JSON パース + バッファ表示 + キーバインド | 300〜600 行の Elisp。プロキシなし |
| **案 C** | プロキシ（Go 200 行 + jsonrpc 制御チャネル）+ term.el の修正 advice + モード | プロキシ 400 行、Elisp 400 行 |
| **案 B** | C に加えて、代替画面・bracketed paste・同期出力・IME・文字幅の対応 | C の 2〜3 倍。eat を採れば減るが未知数 |

---

## 推奨

1. **まず案 A を試す。** プロキシを書かずに済み、既に Emacs から往復することを確認してある。
   「Emacs で Claude と会話する」という目的にはこれで届く可能性が高い。
   ここで満足すれば B / C は不要になる。
2. **A では足りない（TUI そのものが欲しい）と分かったら案 C。**
   `--ax-screen-reader` で始めれば端末側の難所が消える。PoC は既にある。
3. **案 B（完全な TUI 再現）は、汎用 PTY プロキシとして他にも使い道がある場合に限る。**
   claude 単体のために払うコストとしては見合わない。

なお **A と C は排他ではない**。同じプロキシの上で、
「普段は A の構造化 UI、TUI が要るときだけ C」という併存もできる。

---

## 未検証の項目（この文書で断定していないこと）

- eat パッケージの実力（対応シーケンス、速度、プロキシとの相性）
- `--include-partial-messages` によるトークン単位ストリーミングの挙動
- 割り込み（`control_request` / `interrupt_receipt_v1`）の実際の送り方
- 日本語入力（tr-ime）と TUI の組み合わせ
- East Asian Ambiguous 幅と conhost の桁計算の食い違い
- GUI の Emacs（コンソールを持たないプロセス）から起動した場合のプロキシの挙動
  — 今回の検証はすべて bash 経由（親がコンソールを持つ）。
  `STARTF_USESTDHANDLES` の問題は親のハンドル種別に依存するので、**GUI で再確認すること**
- 応答ストリーミング中の実スループットと term.el の再描画負荷

---

## 付録 A: PoC の全ソース（Go、約 150 行）

`golang.org/x/sys` のみに依存。`GOPROXY=off go build` でオフラインビルドできた。

```go
// conpty-probe: minimal ConPTY host. Relays stdin -> pty -> stdout,
// and optionally tees the raw child output to a file.
package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"
	"unsafe"

	"golang.org/x/sys/windows"
)

type pty struct {
	hpc  windows.Handle
	in   *os.File // write here -> child stdin
	outH windows.Handle
	pi   windows.ProcessInformation
}

func start(cmdline string, cols, rows int16) (*pty, error) {
	var inR, inW, outR, outW windows.Handle
	if err := windows.CreatePipe(&inR, &inW, nil, 0); err != nil {
		return nil, fmt.Errorf("CreatePipe(in): %w", err)
	}
	if err := windows.CreatePipe(&outR, &outW, nil, 0); err != nil {
		return nil, fmt.Errorf("CreatePipe(out): %w", err)
	}
	var hpc windows.Handle
	if err := windows.CreatePseudoConsole(windows.Coord{X: cols, Y: rows}, inR, outW, 0, &hpc); err != nil {
		return nil, fmt.Errorf("CreatePseudoConsole: %w", err)
	}
	// The ConPTY owns duplicates of these now.
	windows.CloseHandle(inR)
	windows.CloseHandle(outW)

	al, err := windows.NewProcThreadAttributeList(1)
	if err != nil {
		return nil, fmt.Errorf("NewProcThreadAttributeList: %w", err)
	}
	if err := al.Update(windows.PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
		unsafe.Pointer(hpc), unsafe.Sizeof(hpc)); err != nil {
		return nil, fmt.Errorf("UpdateProcThreadAttribute: %w", err)
	}
	si := new(windows.StartupInfoEx)
	si.ProcThreadAttributeList = al.List()
	si.Cb = uint32(unsafe.Sizeof(*si))
	// Critical: without STARTF_USESTDHANDLES (with all three handles left NULL)
	// the child inherits the parent's std handle *values*, which are invalid in
	// the child when bInheritHandles is false. The child then attaches to the
	// pseudoconsole (its title is set) but every write fails silently.
	si.StartupInfo.Flags |= windows.STARTF_USESTDHANDLES

	var pi windows.ProcessInformation
	if err := windows.CreateProcess(nil, windows.StringToUTF16Ptr(cmdline),
		nil, nil, false,
		windows.EXTENDED_STARTUPINFO_PRESENT|windows.CREATE_UNICODE_ENVIRONMENT,
		nil, nil, &si.StartupInfo, &pi); err != nil {
		return nil, fmt.Errorf("CreateProcess: %w", err)
	}
	al.Delete()
	return &pty{hpc: hpc, in: os.NewFile(uintptr(inW), "pty-in"), outH: outR, pi: pi}, nil
}

func main() {
	cols := flag.Int("cols", 100, "columns")
	rows := flag.Int("rows", 30, "rows")
	dur := flag.Duration("dur", 0, "kill child after this long (0 = wait)")
	tee := flag.String("tee", "", "write raw child output to this file")
	send := flag.String("send", "", "keys to send after -delay; <CR> <ESC> <C-c> recognized")
	delay := flag.Duration("delay", 1500*time.Millisecond, "delay before -send")
	flag.Parse()

	p, err := start(strings.Join(flag.Args(), " "), int16(*cols), int16(*rows))
	if err != nil {
		fmt.Fprintln(os.Stderr, "ERR:", err)
		os.Exit(1)
	}

	var w io.Writer = os.Stdout
	if *tee != "" {
		f, err := os.Create(*tee)
		if err != nil {
			fmt.Fprintln(os.Stderr, "ERR:", err)
			os.Exit(1)
		}
		defer f.Close()
		w = io.MultiWriter(os.Stdout, f)
	}

	// Read the pty with raw ReadFile; os.NewFile on a synchronous pipe handle
	// is an extra variable we do not need here.
	done := make(chan struct{})
	go func() {
		defer close(done)
		buf := make([]byte, 8192)
		for {
			var n uint32
			if err := windows.ReadFile(p.outH, buf, &n, nil); err != nil || n == 0 {
				return
			}
			w.Write(buf[:n])
		}
	}()
	go func() { io.Copy(p.in, os.Stdin) }()

	if *send != "" {
		go func() {
			time.Sleep(*delay)
			s := strings.ReplaceAll(*send, "<CR>", "\r")
			s = strings.ReplaceAll(s, "<ESC>", "\x1b")
			s = strings.ReplaceAll(s, "<C-c>", "\x03")
			p.in.WriteString(s)
		}()
	}

	if *dur > 0 {
		select {
		case <-done:
		case <-time.After(*dur):
			windows.TerminateProcess(p.pi.Process, 1)
		}
	} else {
		windows.WaitForSingleObject(p.pi.Process, windows.INFINITE)
	}
	windows.ClosePseudoConsole(p.hpc)
	select {
	case <-done:
	case <-time.After(2 * time.Second):
	}
	var code uint32
	windows.GetExitCodeProcess(p.pi.Process, &code)
	fmt.Fprintf(os.Stderr, "\n[conpty-probe] child exit=%d\n", code)
}
```

`go.mod`:

```
module conptyprobe

go 1.24

require golang.org/x/sys v0.47.0
```

使い方:

```sh
# claude の TUI を 25 秒動かして生バイトを採取
./conpty-probe.exe -cols 100 -rows 30 -dur 25s -tee claude.bin -- claude.exe

# 12 秒後に文字を送り込む
./conpty-probe.exe -dur 30s -delay 12s -send 'hello<CR>' -tee out.bin -- claude.exe
```

## 付録 B: 採取したバイト列を term.el に流し込む検証スクリプト

```elisp
;;; -*- lexical-binding: t -*-
(require 'term)
(defvar rt-buf (generate-new-buffer "*rt*"))
(let ((raw (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally (getenv "RT_FILE"))
             (buffer-string)))
      (proc nil))
  (setq proc (make-pipe-process :name "rt-dummy" :buffer rt-buf :noquery t))
  (with-current-buffer rt-buf
    (term-mode)
    (term-char-mode)
    (term-reset-size 30 100)
    (setq-local locale-coding-system 'utf-8-unix)   ; ← これが無いと cp932 で壊れる
    (set-marker (process-mark proc) (point))
    (let ((i 0) (n (length raw)) (chunk 1024))
      (while (< i n)
        (let ((end (min n (+ i chunk))))
          (condition-case e
              (term-emulate-terminal proc (substring raw i end))
            (error (message "CHUNK %d ERROR: %S" i e)))
          (setq i end))))))
```

注意点:

- `term-emulate-terminal` はプロセスオブジェクトを要求するので、
  `make-pipe-process` でダミーを作って渡す
- 結果を書き出すときは `coding-system-for-write` を `raw-text` にする。
  復号に失敗した生バイトが残っている可能性があるため
- **`text` や `lines` という名前の変数を `defvar` してはいけない**。
  term.el 内のレキシカル変数がダイナミック束縛に変わって無限ループする（実際に踏んだ）

## 付録 C: 案 A の検証スクリプト

```elisp
;;; -*- lexical-binding: t -*-
(defvar pr-buf "") (defvar pr-results nil) (defvar pr-proc nil)

(defun pr-filter (_proc str)
  (setq pr-buf (concat pr-buf str))
  (while (string-match "\n" pr-buf)
    (let ((line (substring pr-buf 0 (match-beginning 0))))
      (setq pr-buf (substring pr-buf (match-end 0)))
      (unless (string-empty-p (string-trim line))
        (let* ((o (json-parse-string line :object-type 'alist)))
          (when (equal (alist-get 'type o) "result")
            (push (alist-get 'result o) pr-results)))))))

(let ((default-process-coding-system '(utf-8-unix . utf-8-unix)))
  (setq pr-proc
        (make-process
         :name "claude" :buffer nil :connection-type 'pipe :noquery t
         :command (list "c:/Users/masao/.local/bin/claude.exe"
                        "-p" "--verbose"
                        "--input-format" "stream-json"
                        "--output-format" "stream-json"
                        "--model" "haiku" "--tools" "")
         :filter #'pr-filter)))

(defun pr-send (text)
  (process-send-string
   pr-proc
   (concat (json-serialize
            `((type . "user")
              (message . ((role . "user")
                          (content . [((type . "text") (text . ,text))])))))
           "\n")))
```

`--output-format stream-json` には **`--verbose` が必須**（無いとエラーで即終了する）。
