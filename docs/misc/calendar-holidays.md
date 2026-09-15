<!-- -*- gfm -*- -->

# カレンダーで日本の祝日を出す

日本の祝日は Emacs 本体に入っていない。`japanese-holidays` の接続は 2 箇所とも設定しないと 1 つも出ず、どちらもエラーを出さない。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

**日本の祝日は Emacs 本体に入っていない。** 31.1 の `lisp/calendar/` を
`japan` で grep しても 1 件も出ない。`calendar-holidays` の既定値 39 件は
アメリカ・キリスト教・ユダヤ・イスラム・バハイ・中国の祝日で、日本のものは
1 つも無い。したがって外部パッケージが要る。

**`japanese-holidays`（emacs-jp）が事実上唯一の選択肢で、これで足りる。**
upstream は 2020-12-29 で止まっている（2026-09 に fetch して確認。behind=0）が、
**祝日法が 2021 年以降変わっていないため現行法に完全対応している**。
2026 年の 18 件が内閣府の一覧と一致することを実測した。

| | |
|---|---|
| 2026/5/6 | 振替休日（憲法記念日が日曜） |
| **2026/9/22** | **国民の休日**（敬老の日 9/21 と秋分の日 9/23 に挟まれた日） |
| 2027/3/22 | 振替休日（春分の日が日曜） |
| 2050/3/20・9/23 | 春分・秋分（天文計算なので遠い将来も出る） |

法改正があれば追随されないが、そのときは `holiday-other-holidays` に足せる。

## 【重要】2 箇所とも設定しないと 1 つも出ない（2026-09-08 に発見）

長いあいだ祝日が表示されていなかった。原因は 2 つあり、**どちらもエラーを
出さない**。

1. **`mark-holidays-in-calendar` という変数は存在しない。**
   Emacs 23 で `calendar-mark-holidays-flag` に改名され、obsolete alias も
   残っていない。`customize-set-variable` は defcustom でない変数にも
   `set-default` するので、**警告も出ないまま同名の変数が 1 つ増えるだけ**で、
   本物のフラグは nil のままだった。

   **見分け方は `(get 'VAR 'custom-type)` が nil かどうか。** `boundp` は
   自分で作ってしまった変数にも t を返すので判定に使えない。同じことが
   2026-09-09 に `cape-dabbrev-min-length` でも起きていた（cape から変数
   自体が無くなっていた。`my-completion.el` の `:custom` から削除済み）
2. **`calendar-holidays` に `japanese-holidays` を設定する行が要る。**
   パッケージを入れただけでは `japanese-holidays` という変数が定義されるだけで、
   どこにも接続されない

```elisp
(customize-set-variable
 'calendar-holidays (append japanese-holidays
                            holiday-local-holidays
                            holiday-other-holidays))
```

## `:custom` ではなく `:config` に置く

`:custom` は **`require` より前**に展開されるので、値の式にある
`japanese-holidays` が void になる。`macroexpand-1` で確認した展開:

```elisp
(eval-after-load 'calendar
  (customize-set-variable 'japanese-holiday-weekend ...)   ; :custom
  (require 'japanese-holidays)                             ; ここでロード
  (require 'holidays)                                      ; :config
  (customize-set-variable 'calendar-holidays japanese-holidays))
```

あわせて `:after calendar` + `:demand t` にする。祝日のマーク
（`calendar-mark-holidays`）は `calendar-generate` の中で走るので、
`calendar-today-visible-hook` 経由の遅延ロードでは**初回の表示に間に合わない**。
`holiday-local-holidays` / `holiday-other-holidays` は `holidays.el` の
defcustom で、`calendar.el` はそれを autoload するだけなので `:config` で
`(require 'holidays)` する。

## 【重要】マークは overlay。テキストプロパティを見ても分からない

`calendar-mark-visible-date` は face のとき
`(overlay-put (make-overlay ...) 'face mark)` を使う（`calendar.el:2776`）。
`font-lock-face` を見て「マークされていない」と誤診した。

```elisp
(overlays-in (1- (point)) (1+ (point)))   ; ← こちらで見る
```

`calendar-check-holidays` は overlay と無関係に正しい値を返すので、
**「祝日として認識されているか」と「画面にマークが付いているか」は別々に
確かめること。**

## 実測（2026-09-08、init 経由で初回の `M-x calendar`）

| | |
|---|---|
| init を読んだ直後 | `calendar` も `japanese-holidays` も**未ロード**（起動を重くしない） |
| 初回の `(calendar)` | japanese-holidays が自動でロードされ `calendar-holidays` が 19 件に |
| 9/21 敬老の日 / 9/22 国民の休日 / 9/23 秋分の日 | いずれも `holiday` face の overlay |
| 9/24（平日） | overlay 無し |
| 土曜 | `japanese-holiday-saturday`（`japanese-holiday-weekend` が `(0 6)` なので日曜は `holiday`） |

**この検証は init 経由で、かつ calendar を一度も開いていない状態から
行うこと。** 手で `customize-set-variable` してから開くと、遅延ロードの
タイミングの問題を見逃す。batch で init を読むので、`recentf` と `history` は
バックアップしてから実行し、終わったら戻す（`kill-emacs-hook` も空にする）。

