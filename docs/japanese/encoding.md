<!-- -*- gfm -*- -->

# 文字コード（Windows でのプロセス起動と外部コマンド）

`call-process` の引数は cp932。`prefer-coding-system` が `default-process-coding-system` を上書きする件と、grep / ripgrep がシェル経由で別経路になる件。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## 【重要】`call-process` の引数は cp932 でエンコードすること

`my-japanese.el` は Windows で `default-process-coding-system` を
`(utf-8 . utf-8)` にしている。**`call-process` の引数はこの cdr で
エンコードされる**が、Emacs のプロセス起動は ANSI API なので、送った
UTF-8 のバイト列が受け取り側で cp932 として解釈される。
結果、**日本語を含むパスは存在しないファイル名になる**。

実測（`KOB00100_チェック仕様・メッセージ一覧.md`、GUI・実設定）:

| `default-process-coding-system` | cmd の `if exist %1` |
|---|---|
| `(utf-8 . utf-8)`（設定のまま） | **MISSING** |
| `(utf-8 . cp932)` | EXIST |
| `emacs -Q`（既定） | EXIST |

**この壊れ方は何の手がかりも残さない。** MarkText は受け取ったパスを
`isMarkdownFile`（存在チェックを含む）で黙って捨て、ログにも書かずに
`startUpAction`（`blank`）へフォールバックする。`start` 経由なので終了
コードも必ず 0。つまり「**MarkText は起動するが空白**」だけが見える。

そのため `markdown-open-command` には**文字列ではなく関数**
（`my:markdown-open-external`）を渡している。文字列だと `markdown-open`
自身が `call-process` するので、束縛する隙が無い。関数の中で
`grep`（`my-utils.el`）や `org-pandoc`（`my-text.el`）と同じく cdr だけ
`locale-coding-system` に戻している。

なお Windows の `my:open-file-externally`（`my-core.el`）は
`w32-shell-execute`（ワイド API）なのでこの問題は無い。

2026-09-04 に `default-process-coding-system` そのものを
`(utf-8 . cp932)` に直したので、`M-x compile` / `shell-command` /
ripgrep など `user-lisp/` の外を通る経路も含めて一掃してある（次節）。
`my:markdown-open-external` の束縛は冗長になったが、macOS / Linux では
`locale-coding-system` が utf-8 で no-op になるため、そのまま残してある。

## `prefer-coding-system` が `default-process-coding-system` を上書きする

**`(setq default-process-coding-system '(utf-8 . utf-8))` は GUI では
2015 年からずっと無意味だった。**

`my-japanese.el` の `*encoding` 相当のブロックでこれを設定しても、後続の
w32 ブロックにある `(prefer-coding-system 'utf-8-unix)` が
`set-default-coding-systems` 経由で `default-process-coding-system` を
`(CODING . CODING)` に書き戻す。実測:

```elisp
(setq default-process-coding-system '(utf-8 . cp932))  ; => (utf-8 . cp932)
(prefer-coding-system 'utf-8-unix)                     ; => (utf-8 . utf-8)
```

`default-file-name-coding-system` が `set-file-name-coding-system 'cp932` で
打ち消されているのとまったく同じ構図で、こちらは打ち消しが無かった。
そのため **cdr を変えるには `prefer-coding-system` より後で入れ直す**
必要がある。

なお w32 ブロックは `:if (eq window-system 'w32)` なので **batch では走らない**。
batch での最終値は前段の `setq` が決める。両方に置いてあるのはそのため。

## `(utf-8 . cp932)` に変えたときの影響（2026-09-04 に GUI で実測）

| 観点 | 変更前 `(utf-8 . utf-8)` | 変更後 `(utf-8 . cp932)` |
|---|---|---|
| 引数（日本語パス）を `if exist` で確認 | **MISSING** | **EXIST** |
| 出力の復号（日本語のコミット件名） | OK | OK |
| 標準入力（`call-process-region` → `git hash-object`） | utf-8 | **cp932 に変わる** |
| pandoc（`markdown-preview`） | OK | OK |
| `shell-command-on-region`（往復） | OK | OK |
| magit | OK | OK |
| `markdown-open`（MarkText） | NG | OK |

cdr は**引数と標準入力の両方**を兼ねるので、標準入力に UTF-8 を要求する
相手には `process-coding-system-alist` で個別に指定する。現状は pandoc
（`markdown-preview` と `org-pandoc` がバッファを `call-process-region` で
流し込む）だけ。

magit は `magit-process-git-arguments` が引数を自分で cp932 に
`encode-coding-string` し（unibyte 文字列になるので二重エンコードは
起きない）、標準入力も `magit-run-git-with-input` が自分で utf-8 に
`encode-coding-region` するので、どちらの設定でも影響を受けない
（magit issue #3250）。**magit だけが壊れていなかったのはこれが理由。**

## シェル経由の経路は `process-coding-system-alist` が優先される

`M-x grep` と `my:ripgrep-regexp` は `compilation-start` 経由で
`shell-file-name`（Git の `bash.exe`）に `-c "コマンド行"` を渡す。
ここは **`default-process-coding-system` を直しても効かない。**
`my-shell.el` の

```elisp
(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
```

が `process-coding-system-alist` に載り、そちらが優先されて car / cdr とも
utf-8 に固定されるため。実測（`検索対象キーワード` を bash に渡して
`od` で見る）:

| | 届いたバイト |
|---|---|
| 期待（UTF-8） | `e6 a4 9c e7 b4 a2 …` |
| そのまま | `e8 ae 80 e6 87 83 ef bd b4 …`（UTF-8 を cp932 と解釈した化け） |
| `coding-system-for-write` = cp932 | **一致** |
| alist を `(utf-8 . cp932)` に差し替え | **一致** |

**alist は書き換えていない。** cdr はコマンド行と標準入力を兼ねるので、
alist を `(utf-8 . cp932)` にすると `M-!` / `M-|`
（`shell-command-on-region`）や `M-x shell` の標準入力まで cp932 になる。
代わりに、**標準入力を使わない grep / ripgrep の側だけ**
`coding-system-for-write` を `locale-coding-system` に束縛した
（`my:grep-with-cp932` と `my:ripgrep-with-cp932`）。`coding-system-for-write`
は alist より強い。非 Windows では `locale-coding-system` が utf-8 なので
no-op になる。

実測（`grep検証/日本語ファイル.md` に日本語の行を置いて検索）:

| | 変更前 | 変更後 |
|---|---|---|
| `M-x grep` で日本語を検索 | **一致なし** | **ヒット** |
| dired から `my:ripgrep-regexp` | **exit code 1** | **ヒット** |
| `M-|`（`shell-command-on-region`）の往復 | OK | OK |

**日本語の検索語で「一致なし」になったら、まずこれを疑うこと。**
grep も rg もエラーを出さず、ただ 0 件を返す。

### 【重要】束縛はコマンドではなく `ripgrep-regexp` に張る

当初 `my:ripgrep-regexp`（dired の `G`）が本家 `ripgrep-regexp` を
**コピーして**その中でだけ束縛していたため、本家を呼ぶ経路が漏れていた。

| 入口 | 通る関数 | 2026-09-04 まで |
|---|---|---|
| dired の `G` | `my:ripgrep-regexp`（コピー） | 束縛あり → **ヒット** |
| `C-c p s`（`my:projectile-search-dwim`） | `projectile-ripgrep` → **本家** `ripgrep-regexp` | 束縛なし → **0 件** |
| `M-x ripgrep-regexp` | 同上 | 同上 |

`ripgrep-regexp` に `:around` advice（`my:ripgrep-with-cp932`）を張る形に
変え、`my:ripgrep-regexp` は `(dired-current-directory)` を渡すだけの薄い
ラッパにした。`ripgrep-regexp` は autoload なので、定義前に advice を
張っておけば `ripgrep.el` のロード時に引き継がれる（実測で確認）。

**この検証は batch ではできない。** PowerShell 7 から `--batch` で起動すると
`locale-coding-system` が **`cp65001`**（コンソールのコードページ）になり、
束縛が no-op になって修正前後の区別がつかない（4 通り試して全部 0 件になった）。
GUI では `cp932` で、ANSI コードページと一致する。gitd の
`my:gitd-ansi-coding` が `locale-coding-system` を避けているのと同じ話。

GUI プローブでの実測（`~/.emacs.d` で `プロキシ` を検索）:

| | |
|---|---|
| 修正後 `ripgrep-regexp` / `projectile-ripgrep` / `my:ripgrep-regexp` | **3 つともヒット** |
| advice を外した `projectile-ripgrep` | **0 件**（報告された症状を再現） |

