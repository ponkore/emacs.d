<!-- -*- gfm -*- -->

# eglot と flymake の注意

上流の非互換で didOpen が飛ぶ壊れ方、大文字ドライブレターで診断が出ない件、elisp の flymake と `trusted-content`。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## 上流の非互換で eglot が黙って壊れることがある

eglot は `eglot--maybe-activate-editing-mode` の中で

```elisp
(eglot--managed-mode)                  ; ここで eglot--managed-mode-hook が走る
(eglot--signal-textDocument/didOpen)   ; ← ここが飛ぶ
(eglot-inlay-hints-mode 1) ...
```

の順に呼ぶ。**フックの中でエラーが出ると `textDocument/didOpen` が送られない**。
接続は成立してモードラインにも出るのに、サーバはバッファの存在を知らないため
診断も補完も一切出ない、という分かりにくい壊れ方をする。

実例: doom-modeline 4.3.0 の eglot セグメントが Emacs 31.1 で無くなった
`jsonrpc--request-continuations` / `eglot--spinner` / `eglot--major-mode` を
呼んでおり、`my-appearance.el` で差し替えている（upstream 未修正）。
同種の症状が出たら、まず `eglot--managed-mode-hook` の中身を疑うこと。

## 【重要】Windows で大文字のドライブレターを返すサーバは診断が出ない

**gopls で実際に踏んだ。** 接続もジャンプも補完も整形も効くのに、
flymake の診断だけが 1 件も出ない、という壊れ方をする。

gopls は `textDocument/publishDiagnostics` の uri を

```
file:///C:/Users/masao/...          ← 大文字 C
```

で返す（eglot が送る `workspaceFolders` は `file:///c%3A/...` と小文字）。
受け取り側の `eglot--flymake-handle-push` は `eglot-uri-to-path` の結果を
`eglot--find-buffer-visiting` に渡すが、そこは `buffer-file-name` との
**文字列 `equal`** で突き合わせる（`file-truename` が遅いので避けている。
bug#70036）。Emacs の `buffer-file-name` はドライブレターが小文字なので
一致せず、診断は `flymake-list-only-diagnostics` に回されて
**警告も出ないまま消える**。

`eglot-uri-to-path` 自身が持つ正規化（`trueroot` で始まるならプロジェクトの
root に置換する）も `string-prefix-p` が大文字小文字を区別するので効かない。

実測（gopls v0.23.0 / Emacs 31.1）:

| | |
|---|---|
| `(eglot-uri-to-path "file:///C:/...")` | `"C:/..."` → `eglot--find-buffer-visiting` は nil |
| advice で `"c:/..."` に直す | **0.5 秒で診断が出る** |

`my-lsp.el` で `eglot-uri-to-path` に `:filter-return` の advice
（`my:eglot-normalize-drive-letter`）を張って、Windows のときだけ
ドライブレターを小文字に揃えている。既に小文字なら no-op なので
他のサーバには影響しない。

**診断だけ出ないときは、まずサーバが返す uri の綴りを疑うこと。**
`eglot-events-buffer-config` を一時的に有効にして
`publishDiagnostics` の uri を見る（既定では `:size 0` で記録されない）。

## php-mode は 1.28 (2026-08) で cc-mode 依存が外れた

`c-set-style` / `c-basic-offset` は使えない（`Buffer ... is not a CC Mode buffer`）。
インデントは `php-mode-coding-style` で指定する。
cc-mode 版が要るときは `php-cc-mode` が別に残っている。

## elisp の flymake は「信頼されたバッファ」でしか動かない

Emacs 30 で `trusted-content` が入った（`files.el:718`）。
`elisp-flymake-byte-compile` はバッファをバイトコンパイルする
（= マクロ展開でそのバッファのコードが走りうる）ため、`trusted-content-p` が
偽なら自ら降りる（`elisp-mode.el:2733`）。

```
Disabling elisp-flymake-byte-compile in *scratch* (untrusted content)
```

`my-lsp.el` の `(prog-mode-hook . flymake-mode)` が `*scratch*`
（lisp-interaction-mode → emacs-lisp-mode → prog-mode）にも付くので、
起動のたびにこれが出ていた。エラーを返したバックエンドは flymake が
そのバッファで無効化する（`flymake.el:736`）ので、メッセージ自体は
1 バッファにつき 1 回。

**信頼の例外は `user-init-file`（init.el）だけ。** 明示しないと
`early-init.el` も `user-lisp/` も `site-lisp/` も診断が出ない。
`my-lsp.el` の `:custom` で 3 箇所を登録してある。`~/.emacs.d/` を丸ごと
信頼させると `straight/repos/` のパッケージソースまで対象になるので広げない。

| バッファ | `trusted-content-p` |
|---|---|
| `init.el` | t（`user-init-file` の例外） |
| `early-init.el` / `user-lisp/` / `site-lisp/` | t（登録したもの） |
| `straight/repos/*.el` | nil |
| `*scratch*` | ファイル名が無いので `trusted-content` では救えない |

### 【重要】`*scratch*` の設定は `prog-mode-hook` に depth 付きで載せる

`*scratch*` は `buffer-file-truename` が nil なのでバッファローカルに
`(setq-local trusted-content :all)` するしかない（ielm.el:715 と
simple.el:2072 が同じことをしている）。

**`lisp-interaction-mode-hook` に置いても間に合わない。** `flymake-mode` は
有効化した時点でチェックを 1 回走らせる（`flymake.el:1487`。表示済みの
バッファなら即座に）が、`run-mode-hooks` は `delay-mode-hooks` で溜めた
**親のフックを子のフックより先に**回すので、`prog-mode-hook` の
`flymake-mode` のほうが早い。`add-hook` の depth を `-100` にして
`prog-mode-hook` に載せること。

GUI 実測（`lisp-interaction-mode-hook` → depth -100）:

| | 修正前 | 修正後 |
|---|---|---|
| `*scratch*` の `trusted-content` | `:all` | `:all` |
| `flymake-disabled-backends` | **`(elisp-flymake-byte-compile)`** | **nil** |
| `*Messages*` にメッセージ | **t** | **nil** |

フック自体はどちらも走っているので、**「変数が設定されていること」を
確かめても検証にならない**。バックエンドが生きているかを見ること。

### 【重要】`init.el` では byte-compile バックエンドを外してある（2026-09-09）

`init.el` は `user-init-file` なので `trusted-content` の**組み込みの例外**で、
上の 3 箇所を登録する前から `elisp-flymake-byte-compile` が動いていた。
ところが**このバックエンドは 147 行目より先へ進めない**。

```
147: Cannot open load file: No such file or directory, my-core
```

子プロセスは `emacs -Q` 相当なので `load-path` に `user-lisp/` が無く、
**バイトコンパイラが評価する `(require 'my-core)` が落ちる**。その 4 行上の
`(prepare-user-lisp t)` はただの関数呼び出しなのでコンパイラは実行せず、
`load-path` は伸びない。require の失敗はハードエラーなのでそこで中断する。

**つまり偽診断が 1 件出るだけで、本物の誤りは 1 件も検出できていなかった。**
2026-08-30 の `c60a17f`（flycheck → flymake 移行）から 10 日ほどこの状態。

`my-lsp.el` の `my:flymake-disable-byte-compile-in-init` が `init.el` でだけ
`remove-hook` する。**`my:trust-scratch-content` と同じく `prog-mode-hook` に
depth `-100`**（`flymake-mode` は有効化した時点でチェックを走らせるので
`emacs-lisp-mode-hook` では間に合わない）。`elisp-flymake-checkdoc` は残る。

#### `elisp-flymake-byte-compile-load-path` に足す手は採らない

`user-lisp/` を足せば require は通る。**が、子プロセスが 25 モジュールを
全部ロードするようになる。** 子プロセスを実装どおり再現して実測:

| | `("./")` | `user-lisp/` を追加 |
|---|---|---|
| `my-core` のエラー | 出る | 消える |
| 残る診断 | 上記 1 件 | `straight-use-package' is not known to be defined`（別の偽診断） |
| 所要時間 | **0.51 秒** | **1.10 秒** |
| **`recentf` / `history`** | **変化なし** | **毎回書き戻す**（mtime で確認） |

`flymake-no-changes-timeout` は 1.0 秒なので、`init.el` を編集するたびに
これが走る。偽診断が別の偽診断に入れ替わるだけで、副作用だけが増える。

再現は次の 1 行で足りる（`elisp-flymake-byte-compile` が組み立てる引数列と同じ）。

```sh
emacs -Q --batch -L ./ -f elisp-flymake--batch-compile-for-flymake FILE
```

#### 【重要】バックエンドを外しても報告済みの診断は消えない

flymake は診断をバックエンドごとに持つので、**そのバックエンドが再報告
しない限り古いものが残る**。検証中、`flymake-diagnostic-functions` が
`(elisp-flymake-checkdoc t)` になっているのに 147 行目の診断がまだ出ていて
一度誤診した。`flymake-mode` を入れ直すと消える。

実セッションではフックがモード設定時（`flymake-mode` が有効になる前）に
走るのでバックエンドは登録されず、この残留は起きない。**「外れているか」は
バックエンドの一覧で、「消えたか」は診断で、別々に見ること。**

### 偽警告は避けられない

`elisp-flymake-byte-compile` は `emacs -Q` 相当の子プロセスでコンパイルする。
`user-lisp/` は use-package / straight でパッケージを読む前提なので、
パッケージ由来のマクロが未定義扱いになる。バイトコンパイルしない方針
（`user-lisp/` の節）と同じ理由。実測（byte-compile / checkdoc）:

| | byte-compile | checkdoc |
|---|---|---|
| `my-appearance.el` | 21（`doom-modeline-def-segment` など） | 5 |
| `my-lsp.el` | 9（全部 `defhydra`） | 0 |
| `my-editor.el` | 9 | 1 |
| `my-claude.el` | 2 | 206 |
| `my-core.el` | 0 | 8 |

checkdoc 側は `trusted-content-p` を見ないので、こちらは信頼設定とは
無関係に以前から出ていた。うるさければ `trusted-content` から
`user-lisp/` を落とせば byte-compile 側だけ止まる。

### 検証は GUI で、遅延に付き合うこと

`flymake-mode` の初回チェックは `flymake-start-on-flymake-mode` の
ドキュメントどおり**バッファが実際に表示されてから**走る。
`find-file-noselect` してプローブすると `flymake-start` を呼んでも
何も起きず、診断 0 件になる。`switch-to-buffer` してから
`(flymake-start nil t)`（deferred を nil、force を t）で今すぐ走らせる。

プローブは必ず `condition-case` で囲み、`unwind-protect` で
`kill-emacs` すること。タイマーの中でエラーが出ると GUI の Emacs が
そのまま残り、外から見ると「固まった」ようにしか見えない。

