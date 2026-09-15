<!-- -*- gfm -*- -->

# はてなブログへ投稿する (`my-htnblog.el`)

`M-x htnblog` でひな形を開き `C-c C-c` で公開する。AtomPub API を Basic 認証で叩くだけ。前日分の参考表示と、踏みやすい文字コード・XML の罠。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

毎日 1 記事、カテゴリー・タイトル・本文 1 行目が決まっているので、
`M-x htnblog` でそれをプリセットしたバッファを開き、本文を書いて `C-c C-c`
すると公開される。外部コマンドは要らない。

| キー | |
|---|---|
| `M-x htnblog` | 記事を書くバッファを開く（`C-u` で書きかけを捨ててひな形を入れ直す） |
| `C-c C-c` | 確認して投稿。成功したらバッファを閉じ、記事 URL を kill-ring に入れる |
| `C-c C-k` | 書きかけを捨てて閉じる |

区切り（`--- 前日分 ---`）から下には最新の公開記事が参考として入る（後述）。
読み取り専用で、投稿には含まれない。

ヘッダの書式は [htnblog コマンド](https://github.com/hymkor/htnblog-go) の
`new` と同じにしてある。あちらで書いた下書きをそのまま貼っても通る。
`Category` は複数行書ける。`Draft: yes` を足すと公開せず下書きになる。

````
```header
Category: 体調管理
Title: 9月7日(月)の記録
```

* 9月7日(月)
````

## API は薄い。Basic 認証 1 本で足りる

WSSE も OAuth も要らない（htnblog-go の `post.go` も `SetBasicAuth` 1 行）。
認証情報は htnblog コマンドと同じ `~/.htnblog` から読む
（`userid` / `endpointurl` / `apikey`）。投稿は `<endpointurl>/entry` へ
Atom の entry を POST するだけ。

**`app:draft` を `no` にすれば最初から公開状態で投稿できる。**
htnblog コマンドの `new` → `publish` が 2 手なのは、あちらの `new` が
下書き固定だからで、API の制約ではない。

HTTP は組み込みの url.el。子プロセスを起こさないので、この設定の持病である
「Windows の `call-process` が遅い」とは無関係。

## 踏みやすい 3 点

- **`url-request-data` は unibyte にする。** `encode-coding-string` を通さないと
  日本語が化ける。`default-process-coding-system` の話（別節）と同じ構図だが、
  こちらはプロセスを通らないので `encode-coding-string` で明示するしかない
- **CDATA に `]]>` が現れたら分割する**（`]]]]><![CDATA[>`）。本文は
  ユーザーが自由に書くので必ず起こりうる。実測で `]]>` 単体・連続とも往復した
- **曜日は `format-time-string` の `%a` に頼らない。** `system-time-locale` 次第で
  英語になる。`my:htnblog--day-names` に自前で持つ（`decoded-time-weekday` は
  0 = 日曜）

`apikey` を残さないため、`url-debug` を nil に束縛し、応答バッファは
`unwind-protect` で必ず kill する。失敗した応答だけ `*htnblog-error*` に出す
（本文に認証情報は含まれない）。

## 実測（2026-09-07）

| | |
|---|---|
| `GET <endpointurl>/entry`（記事一覧） | **0.31 秒** / 18465 バイト / entry 10 件 |
| `Draft: yes` で 1 本 POST | 成功。サーバ側で `app:draft` が `yes`、`rel=alternate` から記事 URL が取れた |

`M-x htnblog` から `C-c C-c` までの経路（ヘッダ解析 → 確認 → POST →
URL を kill-ring → バッファ kill）を batch で通してある。**`y-or-n-p` だけは
batch では stdin を待つので `cl-letf` でスタブする。**

なお既存の記事はタイトルが揺れていた（`9月7日の記録` / `9月05日(土)の記録`）。
手で打っている限り避けられないので、ひな形を固定する動機はここにある。

## 前日分（最新の公開記事）を参考に貼る

`my:htnblog-show-previous`（既定 t）が非 nil なら、ひな形の後ろに区切り
（`my:htnblog-previous-separator`、既定 `--- 前日分 ---`）を置いて最新の
公開記事を貼る。読み取り専用で、**投稿には含まれない**。

### 取得は非同期にする

同期にすると `M-x htnblog` が散発的に固まる。GUI 実測:

| | |
|---|---|
| GET 1 回目 | **15.39 秒** |
| GET 2〜4 回目 | 0.11〜0.14 秒 |
| DNS 解決 / TCP 接続 / TLS 接続 | 0.01 / 0.02 / 0.06 秒 |
| url の接続を捨ててから GET | 0.16 秒 |

**Emacs 側ではない。** 再接続すら 0.16 秒なので、15 秒はサーバ応答の揺らぎ。
同期で待つ限り防げないので `url-retrieve` に変えた。バッファは即座に開き
（GUI 実測 **0.014 秒**）、記事は 0.15 秒ほど遅れて入る。

書いている最中に届くので、`buffer-undo-list` を `t` に束縛し、挿入前後で
`buffer-modified-p` を保つ。`point` は `save-excursion` で動かさない。

### 投稿に混ざらない仕掛けは 2 重

- **境目はテキストプロパティ `my:htnblog-previous` で決める**
  （`my:htnblog--body-end`）。区切りの文字列だけに頼ると、行頭に 1 文字
  入っただけで `^区切り` の一致が外れ、**前日分が丸ごと本文に混ざる**
- **read-only で手入力を防ぐ。** ただし挿入と削除で挙動が違う

| | |
|---|---|
| `delete-char` / `kill-line` / `delete-region` | プロパティだけで防げる |
| `self-insert-command` | **stickiness 次第。素では防げない** |

`front-sticky '(read-only)` が要る。逆に **`rear-nonsticky` は付けてはいけない**
（read-only な文字の直後への挿入が継承されず素通りする）。ただし
**区切りの前の改行 1 つは領域の外に置く**こと。カーソルの初期位置＝領域の
先頭になると、front-sticky のせいで**本文が書けなくなる**（実際に踏んだ）。

自分で貼るときは `inhibit-read-only` を束縛する。防ぎたいのは手入力だけ。

### 【重要】`defvar-local` の世代カウンタは `permanent-local` にする

`C-u` で入れ直したとき、古いリクエストの応答を捨てるために世代を持たせて
いるが、**`define-derived-mode` は `kill-all-local-variables` を通る**ので、
`permanent-local` を付けないとグローバル値に戻る。世代が常に 1 になり、
古い応答まで一致して二重に貼り、2 回目の挿入が read-only に当たって
`error in process filter: Text is read-only` で落ちる。

保険として `my:htnblog--insert-previous` 自身も、既に貼ってあれば何もしない。

### libxml には切り替えない

`libxml-parse-xml-region` は速い（0.001 秒）が、**名前空間の prefix を落とす**。
`app:control` が `control` になるので、`xml-get-children` の呼び出しが
静かに nil を返すようになる。`xml-parse-region` は prefix を保ち、18 KB の
feed でも **0.002 秒**なので、切り替える理由が無い。

| | `xml-parse-region` | `libxml-parse-xml-region` |
|---|---|---|
| 速度（18 KB） | 0.002 秒 | 0.001 秒 |
| `app:control` | **取れる** | **nil**（`control` になる） |
| CDATA | 分割されたぶんが別ノードで返る（`my:htnblog--node-text` で連結） | 同じ |

