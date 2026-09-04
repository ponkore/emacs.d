# 案 A 実装メモ — stream-json で Emacs から Claude Code を使う

2026-09-04 着手。実現可能性の検討は `docs/claude/emacs-claude-pty-proxy-study.md`。
そこで「PTY プロキシを書かなくても済む」と分かったので、その道（案 A）を実装する。

将来 C（`--ax-screen-reader` + ConPTY プロキシ）→ B（完全な TUI）へ進む余地は残す。
**A で作るものは B/C と排他ではない**（プロセスの起動方法が違うだけで、
バッファ・キーバインド・モードラインの層は共有できる）。

---

## 確定した事実（すべて実測）

実測環境: Windows 11 26200.9168 / Emacs 31.1 / Claude Code 2.1.260。
プローブはすべて `emacs -Q --batch` から `make-process` で実行した。

### 起動オプション — この 4 つは省略できない

```
claude -p --verbose --input-format stream-json --output-format stream-json \
       --permission-prompt-tool stdio
```

| オプション | 省略すると |
|---|---|
| `-p` / `--print` | `--input-format` も `--output-format` も効かない |
| `--verbose` | **即エラー終了**（`--output-format=stream-json requires --verbose`） |
| `--input-format stream-json` | 標準入力からの逐次投入ができない（1 回で終わる） |
| `--permission-prompt-tool stdio` | **許可プロンプトが黙って自動拒否される**（後述） |

`--include-partial-messages` を足すとトークン単位の `stream_event` が来る（任意）。

### プロトコル — 1 行 1 JSON

送信（stdin）:

```json
{"type":"user","message":{"role":"user","content":[{"type":"text","text":"..."}]}}
```

受信（stdout）で実際に来た `type`（`--include-partial-messages` 有りの 1 ターン）:

```
rate_limit_event
system/init            ← tools, mcp_servers, slash_commands, model, cwd, session_id
system/status
stream_event/message_start
stream_event/content_block_start
system/thinking_tokens
stream_event/content_block_delta   ← テキストはここに刻まれて来る
assistant                          ← ブロック確定ごとに丸ごと来る
stream_event/content_block_stop
stream_event/message_delta
stream_event/message_stop
user                               ← tool_result はここ
result/success                     ← ターン終了
```

**`assistant` イベントだけ見れば表示は作れる。** `stream_event` はタイプライタ表示が
欲しくなってから足せばよい（段階を分けられる）。

`result` に入っている使える値:

| フィールド | 用途 |
|---|---|
| `result` | 最終テキスト |
| `subtype` | `success` / `error_during_execution` |
| `is_error`, `terminal_reason` | 割り込み判定（`aborted_streaming`） |
| `session_id` | `--resume` に使う |
| `total_cost_usd`, `usage`, `modelUsage` | モードライン表示 |
| `duration_ms`, `ttft_ms` | 同上 |
| `num_turns`, `permission_denials` | 同上 |

### 【重要】許可プロンプトは `--permission-prompt-tool stdio` が無いと黙って拒否される

`--permission-prompts` の既定は `host`、つまり **「SDK ホスト（= 我々）が答える」** 建前。
しかしそれだけでは要求が飛んで来ない。実測（`Write` で cwd 内のファイルを作らせた）:

| 起動オプション | 結果 |
|---|---|
| `--permission-prompt-tool` なし | `system/permission_denied` が流れて**ファイルは作られない**。`control_request` は**一度も来ない** |
| `--permission-prompt-tool stdio` | `control_request` / `can_use_tool` が届き、`allow` を返すと**ファイルが作られた** |

**壊れ方が「静かに拒否」なので気づきにくい。**
「なぜかツールが動かない」ときはまずこのオプションを疑うこと。

実際に届いた要求:

```json
{"type":"control_request","request_id":"3995683c-...",
 "request":{"subtype":"can_use_tool",
            "tool_name":"Write","display_name":"Write",
            "input":{"file_path":"c:\\Users\\masao\\.emacs.d\\tmp\\perm-test.txt",
                     "content":"PROBE"},
            "description":"tmp\\perm-test.txt",
            "permission_suggestions":[{"type":"setMode","mode":"acceptEdits",
                                       "destination":"session"}],
            "tool_use_id":"toolu_0121..."}}
```

返す形（これで通った）:

```json
{"type":"control_response",
 "response":{"subtype":"success","request_id":"3995683c-...",
             "response":{"behavior":"allow","updatedInput":{ ...request.input と同じ... }}}}
```

- `description` は人間向けの短い説明。プロンプトの見出しにそのまま使える
- `permission_suggestions` は「今後はこうする？」の候補（`acceptEdits` に切り替える等）。
  UI の選択肢に反映できる
- `updatedInput` を書き換えて返せば**入力を編集して許可**できる（今回は素通し）
- 拒否は `behavior: "deny"` のはず（**未検証**）

### 制御プロトコルのハンドシェイク

```json
{"type":"control_request","request_id":"init-1","request":{"subtype":"initialize"}}
```

を送ると `control_response` が返る。**中身は 16 KB あって、スラッシュコマンドの
一覧（`name` / `description` / `argumentHint`）が入っていた。**
コマンド補完の材料としてそのまま使える。

なお `initialize` を送らなくても `can_use_tool` は届いた（`--permission-prompt-tool stdio`
だけで足りる）。ただし SDK は送るので、送っておくのが無難。

### 割り込み（`C-g` 相当）

```json
{"type":"control_request","request_id":"int-1","request":{"subtype":"interrupt"}}
```

実測:

| | |
|---|---|
| 応答 | `control_response` / `success` / `{"still_queued":[]}` |
| その直後 | `result` が `subtype=error_during_execution`, `is_error=true`, `terminal_reason=aborted_streaming`, `result=null` |
| **プロセスの生死** | **生きている** |
| **次のターン** | **通る**（続けて送った質問に `STILL-ALIVE` が返った） |

**割り込んでもセッションは死なない。** これは UX 上とても大きい。
`init` イベントの `capabilities` に `interrupt_receipt_v1` /
`interrupt_cancel_queued_v1` / `msg_lifecycle_v1` が出ている。

### 文字コード

**この経路では cp932 を気にしなくてよい。**

- 本文は「引数」ではなく「標準入力」で渡すので、CLAUDE.md の
  「`call-process` の引数は cp932」の話は当てはまらない
- `default-process-coding-system` を `(utf-8-unix . utf-8-unix)` に**束縛して**起動する。
  `my-japanese.el` がグローバルを `(utf-8 . cp932)` にしているので、
  **束縛しないと標準入力が cp932 になって日本語が壊れる**
- 実測: `表計算・髙﨑・①②` を送って正しく届き、日本語の返信も正しく復号できた

### プロセスの寿命

- stdin を閉じる（`process-send-eof`）とプロセスは正常終了する
- 1 プロセスで複数ターンを継続できる（2 ターン目が 1 ターン目を参照した）
- `result` が `is_error` のときプロセスに EOF を送ると終了コードは 1 になる。
  **異常終了ではない**ので sentinel で騒がないこと

---

## 設計

### 置き場所と命名

| | |
|---|---|
| モジュール | `user-lisp/my-claude.el`（`init.el` の `require` 列の末尾に足す） |
| プレフィクス | `my:claude-` |
| キープレフィクス | **`C-c a`**（`a` = assistant）。`C-c c` は `compile`、`g` は diff-hl、`l` は eglot、`p` は projectile、`!` は flymake で埋まっている |
| 依存 | 組み込みのみ（`json-serialize` / `json-parse-string` / `make-process`）。straight 不要 |

### バッファ構成

**2 バッファ構成にする。**

| バッファ | 役割 |
|---|---|
| `*claude: <project>*` | 会話の記録。読み取り専用。`M-x my:claude` で開く |
| `*claude-log: <project>*` | 生の JSON Lines。デバッグ用。既定では作らない |

入力は**ミニバッファではなく専用の編集バッファ**（`*claude-input*`）にする。
理由: 複数行のプロンプトを書きたい、org のリンクやコード片を貼りたい、
`C-c C-c` で送る、という体験が Emacs らしい。`log-edit` や `git-commit` と同じ形。

> shell-mode 風の「同一バッファの末尾にプロンプト」も考えたが、
> comint は「1 行 = 1 コマンド」の前提が強く、複数行プロンプトと相性が悪い。
> 会話の記録は読み取り専用にしておくほうが、あとから `C-c C-l` で
> ツール出力を折りたたむような加工がしやすい。

### プロセスは「セッション」単位

- 1 セッション = 1 プロセス = 1 バッファ
- `default-directory` はプロジェクトルート（`projectile-project-root`、無ければ `default-directory`）。
  **`expand-file-name` すること**（gitd で `~` 未展開が原因の os error 267 を踏んでいる）
- 複数プロジェクトで同時にセッションを持てるよう、バッファ名にプロジェクト名を入れる

### JSON Lines の受信

プロセスフィルタには**行の途中で届く**。gitd と同じく、
未処理バイトをバッファリングして `\n` で切り出す。

```elisp
(setq buf (concat buf str))
(while (string-match "\n" buf)
  (let ((line (substring buf 0 (match-beginning 0))))
    (setq buf (substring buf (match-end 0)))
    (my:claude--handle-line line)))
```

**1 行が非常に長い。** `init` の control_response は 16 KB あった。
`json-parse-string` は C 実装なので速度は問題にならない。

パースに失敗した行は**捨てずにログへ**。上流のフォーマット変更に気づけなくなるため。

### イベント → 表示のマッピング

| イベント | 表示 |
|---|---|
| `system/init` | ヘッダに model / cwd / mcp_servers / session_id を 1 行 |
| `assistant` の `text` ブロック | そのまま本文として挿入 |
| `assistant` の `tool_use` ブロック | `▶ Bash: git status` のような 1 行 + 入力を折りたたみ |
| `user` の `tool_result` | ツール行の下にインデントして。長いものは既定で折りたたむ |
| `system/permission_denied` | 目立つ face で警告 |
| `result` | 区切り線 + コスト / トークン / 所要時間 |
| `stream_event` | 段階 4 まで無視 |
| `control_request/can_use_tool` | 許可 UI（下記） |

### 許可 UI

`y-or-n-p` では足りない（「今回だけ」「以後このツールは許可」「モードを変える」がある）。
`read-multiple-choice` を使う。選択肢:

| キー | 意味 | 返すもの |
|---|---|---|
| `y` | 今回だけ許可 | `behavior: allow` |
| `n` | 拒否 | `behavior: deny` |
| `a` | 以後このセッションでは聞かない | `allow` + `permission_suggestions` を適用 |
| `e` | 入力を編集してから許可 | `updatedInput` を書き換えて `allow` |
| `v` | 入力の全文を別バッファで見る | 選択に戻る |

**`request_id` を必ずそのまま返すこと。** 取り違えるとどの要求への答えか分からなくなる。

**要求が届いてから答えるまで claude は待つ。** ミニバッファを占有するので、
`my:claude-auto-approve`（既定 nil）で「自動許可するツール名の正規表現」を
持てるようにしておく。

### 割り込み

- `C-c C-k`（`my:claude-interrupt`）で `interrupt` を送る
- 応答待ちの `accept-process-output` ループは作らない。**すべて非同期**にする。
  同期で待つと `C-g` が効かない設計になってしまう
- 実測どおりセッションは生き残るので、割り込み後もそのまま次を送れる

### モードライン

`init` と `result` から取れる情報を `my:claude-mode-line` に出す:

```
claude[opus/auto] 3turns $0.83 12.4s
```

doom-modeline のセグメントにするかはあとで決める（まずは `mode-line-format` に直接）。

---

## 実装の段階

**各段階の終わりに GUI で起動して確認する。** batch では
`accept-process-output` の挙動が違ううえ、face もミニバッファも見えない。

| 段階 | 内容 | 完了の判定 |
|---|---|---|
| **0** | プロセスの起動・停止・生 JSON をログバッファへ。表示なし | `M-x my:claude` でプロセスが立ち、`init` が流れる |
| **1** | 送信と本文表示（`assistant/text` と `result` だけ） | 日本語で 2 往復できる |
| **2** | `tool_use` / `tool_result` の表示。折りたたみ | `git status` を実行させて読める |
| **3** | 許可プロンプト（`can_use_tool`） | `Write` を許可してファイルができる／拒否できる |
| **4** | 割り込み、`--include-partial-messages` による逐次表示 | 長い出力を `C-c C-k` で止められる |
| **5** | セッション管理（`--resume` / `--continue`）、スラッシュコマンド補完 | 前回の会話を再開できる |

**2026-09-04 時点で 0〜5 すべて実装・検証済み。** あわせて計画に無かった
2 つを足した。

- 環境（アカウント）の切り替え（`CLAUDE_CONFIG_DIR`）
- ワークスペースの信頼（Emacs から起動するとドライブレターが小文字になる問題）

段階 4 / 5 で分かったことは CLAUDE.md に移した。要点だけ再掲する。

| | |
|---|---|
| `assistant` の到達順 | `content_block_stop` **より先**にブロック 1 つぶんずつ届く。text を delta と両方で出すと二重になる |
| `thinking_delta` | haiku では本文が空文字列で届く |
| 中断時 | `content_block_stop` が来ないことがある。`result` でブロックを閉じる |
| `--continue` / `--resume` | stream-json と併用できる。モデルだけ差し替えて会話を継げる |
| スラッシュコマンド | `initialize` の control_response に 52 個。`/context` は `num_turns=0` で返る |
| 補完の範囲 | **行頭の `/` だけ**。文中まで拾うと `src/foo` のようなパスで誤爆する |

段階 0〜3 で「使える」状態になる。4 と 5 は快適さの話。

## キーバインド案

グローバル:

| キー | コマンド |
|---|---|
| `C-c a a` | `my:claude` — セッションを開く（無ければ起動） |
| `C-c a s` | `my:claude-send-region` — リージョンを送る |
| `C-c a k` | `my:claude-interrupt` |
| `C-c a q` | `my:claude-quit` — セッション終了 |

会話バッファ（`my:claude-mode`）:

| キー | コマンド |
|---|---|
| `i` / `C-c C-i` | 入力バッファを開く |
| `TAB` | ツール出力の折りたたみ切り替え |
| `C-c C-k` | 割り込み |
| `g` | （何もしない。誤爆防止に明示的に潰す） |
| `q` | `quit-window` |

入力バッファ（`my:claude-input-mode`）:

| キー | コマンド |
|---|---|
| `C-c C-c` | 送信 |
| `C-c C-k` | 破棄 |

---

## 踏むと分かっている落とし穴

`gitd/` と今回のプローブで既に踏んだもの。**同じ穴を二度掘らないこと。**

| 落とし穴 | 対処 |
|---|---|
| `--verbose` を忘れる | 即エラー終了。起動時に必ず付ける |
| `--permission-prompt-tool stdio` を忘れる | **静かに拒否される**。ツールが動かないときの第一容疑者 |
| `default-process-coding-system` を束縛し忘れる | `my-japanese.el` が cdr を cp932 にしているので**標準入力の日本語が壊れる**。`(utf-8-unix . utf-8-unix)` に束縛して起動 |
| `default-directory` が `~/...` のまま | gitd で os error 267 を踏んだ。`expand-file-name` する |
| フィルタが行の途中で呼ばれる | 未処理バイトを持ち越す |
| `result` が `is_error` のときの終了コード 1 | 異常ではない。sentinel で警告しない |
| `use-package` の名前 | `my-claude` は実在する feature になる（自分で `provide` する）ので問題ない。`:hook` 等を使うなら `:config` が `eval-after-load` に包まれる点に注意 |
| `:custom` にマイナーモード変数を書く | CLAUDE.md のとおり動かない。`:config` から明示的に呼ぶ |
| 同期で応答を待つ | `C-g` が効かなくなる。全部非同期にする |

## 未確定・要判断（実装しながら決める）

- **`behavior: "deny"` の正確な形**（未検証）。`message` を添えられるかも要確認
- **`permission_suggestions` の適用方法**。`setMode` を返す形が別にあるのか、
  `control_request` を自分から送るのか
- **`stream_event` の `content_block_delta` の粒度**。ちらつくなら間引く
- **サブエージェント**（`--forward-subagent-text`）の表示をどうするか
- **`system/thinking_tokens`** の意味（今回 1 ターンに 2〜4 回来た）
- モデル選択の UI。`--model` は起動時のみなので、変えるならプロセス再起動 +
  `--resume` になる
- 会話の永続化。`session_id` があるので Emacs 側で保存する必要は薄いが、
  バッファの中身を org に落とすと便利かもしれない

## 検証方法

- **最終確認は必ず GUI で。** ミニバッファ・face・`read-multiple-choice` は batch で評価できない
- 生 JSON を残す `my:claude-log` を最初から入れておく。
  上流のイベント種別が増えたときに気づけるのはこれだけ
- API を叩くテストは `--model haiku --tools ""` で安くする。
  実測でも Opus 1 回で $0.83 かかった（大半はシステムプロンプトのキャッシュ作成）
- `emacs -Q --batch` で「プロセスが立って `init` が来る」ところまでは自動化できる。
  そこまでは CI 的に回せる

## 参考

- 実現可能性の検討と ConPTY の PoC: `docs/claude/emacs-claude-pty-proxy-study.md`
- 常駐プロセスと jsonrpc の前例: `docs/magit/magit-gitd-2a-design.md`, `docs/magit/magit-gitd-2b-design.md`
