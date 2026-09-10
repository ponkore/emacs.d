;;; make-emoji-sample.el --- 絵文字サンプルを作る  -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; Unicode の emoji-test.txt から `etc/emoji-sample.org' を作る。
;;
;; 元のファイルは 1 行 1 絵文字でコードポイントと名前が並ぶため、
;; 4400 行あって「目で見て選ぶ」用途には向かない。こちらは絵文字だけを
;; 分類ごとに横へ並べる。コードポイントとの対応は落とす。
;;
;;   emacs --batch -l etc/make-emoji-sample.el \
;;     --eval '(my:emoji-sample-generate "~/Downloads/その他/emoji-test.txt")'
;;
;; 出力先を変えたいときは第 2 引数に渡す。
;;
;; 【重要】肌の色付き (1F3FB..1F3FF を含むシーケンス) は落とす。
;; v13.0 では fully-qualified 3298 件のうち 1490 件がそれで、5 段階が
;; 並ぶだけで一覧が埋まる。付けたいときは修飾子そのものを絵文字の直後に
;; 足せばよいので、「肌の色」の節に修飾子と例を残してある。
;;; Code:

(require 'subr-x)

(defconst my:emoji-sample-columns 20
  "1 行に並べる絵文字の数。

GUI 実測 (HackGen 11.6、半角 8px) では絵文字 1 文字が 21px、空白を
挟んで 20 個で 572px = 約 72 桁。80 桁のウィンドウに収まる。24 個に
すると 688px (約 86 桁) で、半分に割ったウィンドウでは折り返す。

**桁は揃わない。** 絵文字はプロポーショナルなフォールバックで描かれ、
国旗 (🇦🇨) は 14px、顔 (😀) は 21px と幅が違う。表として組む用途では
ないので、揃えようとしないこと。")

(defconst my:emoji-sample-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "このファイルの置き場。出力先の既定に使う。

【重要】ロード時に採ること。`--eval' が走るのはロードが終わったあとで、
そのとき `load-file-name' は nil に戻っている (実測: 出力が etc/ ではなく
カレントディレクトリに落ちた)。")

(defconst my:emoji-sample-group-names
  '(("Smileys & Emotion" . "顔と感情")
    ("People & Body"     . "人と体")
    ("Component"         . "部品")
    ("Animals & Nature"  . "動物と自然")
    ("Food & Drink"      . "食べ物と飲み物")
    ("Travel & Places"   . "旅行と場所")
    ("Activities"        . "アクティビティ")
    ("Objects"           . "もの")
    ("Symbols"           . "記号")
    ("Flags"             . "旗"))
  "グループ名の日本語。")

(defconst my:emoji-sample-subgroup-names
  '(("face-smiling"           . "笑顔")
    ("face-affection"         . "好意・愛情")
    ("face-tongue"            . "舌を出す顔")
    ("face-hand"              . "手を添えた顔")
    ("face-neutral-skeptical" . "無表情・懐疑")
    ("face-sleepy"            . "眠い顔")
    ("face-unwell"            . "体調不良")
    ("face-hat"               . "帽子")
    ("face-glasses"           . "メガネ")
    ("face-concerned"         . "心配・驚き")
    ("face-negative"          . "怒り・否定")
    ("face-costume"           . "仮装・空想")
    ("cat-face"               . "猫の顔")
    ("monkey-face"            . "猿の顔")
    ("emotion"                . "感情の記号")
    ("hand-fingers-open"      . "開いた手")
    ("hand-fingers-partial"   . "一部の指")
    ("hand-single-finger"     . "指さし")
    ("hand-fingers-closed"    . "握った手")
    ("hands"                  . "両手")
    ("hand-prop"              . "手と小物")
    ("body-parts"             . "体の部位")
    ("person"                 . "人")
    ("person-gesture"         . "しぐさ")
    ("person-role"            . "職業・役割")
    ("person-fantasy"         . "空想の人物")
    ("person-activity"        . "動作")
    ("person-sport"           . "スポーツ")
    ("person-resting"         . "休んでいる人")
    ("family"                 . "家族")
    ("person-symbol"          . "人の記号")
    ("skin-tone"              . "肌の色")
    ("hair-style"             . "髪型")
    ("animal-mammal"          . "哺乳類")
    ("animal-bird"            . "鳥")
    ("animal-amphibian"       . "両生類")
    ("animal-reptile"         . "爬虫類")
    ("animal-marine"          . "海の生き物")
    ("animal-bug"             . "虫")
    ("plant-flower"           . "花")
    ("plant-other"            . "植物")
    ("food-fruit"             . "果物")
    ("food-vegetable"         . "野菜")
    ("food-prepared"          . "料理")
    ("food-asian"             . "アジアの料理")
    ("food-marine"            . "魚介")
    ("food-sweet"             . "お菓子")
    ("drink"                  . "飲み物")
    ("dishware"               . "食器")
    ("place-map"              . "地図")
    ("place-geographic"       . "地形")
    ("place-building"         . "建物")
    ("place-religious"        . "宗教施設")
    ("place-other"            . "その他の場所")
    ("transport-ground"       . "陸の乗り物")
    ("transport-water"        . "水の乗り物")
    ("transport-air"          . "空の乗り物")
    ("hotel"                  . "ホテル")
    ("time"                   . "時計")
    ("sky & weather"          . "空と天気")
    ("event"                  . "行事")
    ("award-medal"            . "賞・メダル")
    ("sport"                  . "スポーツ用品")
    ("game"                   . "ゲーム")
    ("arts & crafts"          . "美術・工芸")
    ("clothing"               . "衣類")
    ("sound"                  . "音")
    ("music"                  . "音楽")
    ("musical-instrument"     . "楽器")
    ("phone"                  . "電話")
    ("computer"               . "コンピュータ")
    ("light & video"          . "照明・映像")
    ("book-paper"             . "本と紙")
    ("money"                  . "お金")
    ("mail"                   . "郵便")
    ("writing"                . "筆記具")
    ("office"                 . "事務用品")
    ("lock"                   . "鍵")
    ("tool"                   . "道具")
    ("science"                . "科学")
    ("medical"                . "医療")
    ("household"              . "生活用品")
    ("other-object"           . "その他のもの")
    ("transport-sign"         . "交通標識")
    ("warning"                . "警告")
    ("arrow"                  . "矢印")
    ("religion"               . "宗教")
    ("zodiac"                 . "星座")
    ("av-symbol"              . "再生・音量")
    ("gender"                 . "性別")
    ("math"                   . "数学")
    ("punctuation"            . "約物")
    ("currency"               . "通貨")
    ("other-symbol"           . "その他の記号")
    ("keycap"                 . "キーキャップ")
    ("alphanum"               . "英数字")
    ("geometric"              . "図形")
    ("flag"                   . "旗")
    ("country-flag"           . "国旗")
    ("subdivision-flag"       . "地域の旗"))
  "サブグループ名の日本語。")

(defun my:emoji-sample--label (name table)
  "NAME を TABLE で引いて「日本語 (原名)」にする。訳が無ければ原名のまま。"
  (let ((ja (cdr (assoc name table))))
    (if ja (format "%s (%s)" ja name) name)))

(defun my:emoji-sample--skin-tone-p (codepoints)
  "コードポイント列 CODEPOINTS が肌の色の修飾子を **含む** シーケンスか。
修飾子そのもの (1 個だけの行) は真としない。"
  (and (string-match-p "\\_<1F3F[B-F]\\_>" codepoints)
       (string-match-p " " (string-trim codepoints))))

(defun my:emoji-sample--parse (file)
  "FILE を読んで ((GROUP (SUBGROUP EMOJI...) ...) ...) を出現順に返す。"
  (let ((groups nil) (group nil) (subgroup nil))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "\\`# group: *\\(.*?\\) *\\'" line)
            (setq group (match-string 1 line) subgroup nil)
            (push (list group) groups))
           ((string-match "\\`# subgroup: *\\(.*?\\) *\\'" line)
            (setq subgroup (match-string 1 line))
            (setcdr (car groups) (cons (list subgroup) (cdr (car groups)))))
           ;; 1F636 200D 1F32B FE0F  ; fully-qualified  # 😶‍🌫️ E13.1 face in clouds
           ((string-match
             "\\`\\([0-9A-F][0-9A-F ]*?\\) *; *\\([a-z-]+\\) *# *\\([^ ]+\\) "
             line)
            (let ((cps (match-string 1 line))
                  (status (match-string 2 line))
                  (emoji (match-string 3 line)))
              (when (and group subgroup
                         (member status '("fully-qualified" "component"))
                         (not (my:emoji-sample--skin-tone-p cps)))
                (let ((cell (car (cdr (car groups)))))
                  (setcdr cell (cons emoji (cdr cell)))))))))
        (forward-line 1)))
    ;; push で積んだぶんを全部ひっくり返す。
    (mapcar (lambda (g)
              (cons (car g)
                    (mapcar (lambda (s) (cons (car s) (nreverse (cdr s))))
                            (nreverse (cdr g)))))
            (nreverse groups))))

(defun my:emoji-sample--insert-rows (emojis)
  "EMOJIS を `my:emoji-sample-columns' 個ずつ空白区切りで挿入する。
空白で区切るのは、ZWJ で繋がった絵文字 (👨‍👩‍👧 など) の境目を目で
見て分かるようにするため。詰めて並べるとどこまでが 1 文字か分からない。"
  (while emojis
    (let ((row nil) (n my:emoji-sample-columns))
      (while (and emojis (> n 0))
        (push (pop emojis) row)
        (setq n (1- n)))
      (insert (string-join (nreverse row) " ") "\n"))))

(defun my:emoji-sample-generate (input &optional output)
  "INPUT (emoji-test.txt) から OUTPUT に絵文字サンプルを書き出す。
OUTPUT の既定は このファイルと同じディレクトリの emoji-sample.org。"
  (let* ((output (expand-file-name
                  (or output
                      (expand-file-name "emoji-sample.org"
                                        my:emoji-sample-directory))))
         (data (my:emoji-sample--parse (expand-file-name input)))
         (version "?") (date "?") (total 0))
    ;; ヘッダから版と日付を拾う。
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents (expand-file-name input) nil 0 4000))
      (goto-char (point-min))
      (when (re-search-forward "^# Version: *\\(.*\\)$" nil t)
        (setq version (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^# Date: *\\(.*?\\),.*$" nil t)
        (setq date (string-trim (match-string 1)))))
    (with-temp-file output
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert "#+TITLE: 絵文字サンプル\n"
              "#+STARTUP: overview\n\n")
      (insert (format "Unicode Emoji %s (%s) の emoji-test.txt から、絵文字だけを\n"
                      version date))
      (insert "分類ごとに並べたもの。目で見て選んでコピーするためのファイルなので、\n"
              "コードポイントとの対応は持たない。対応が要るときは元の\n"
              "emoji-test.txt を見ること。\n\n"
              "肌の色付き (👍🏽 など) は 5 段階が並んで一覧が埋まるので落としてある。\n"
              "付けたいときは「肌の色」の節の修飾子を絵文字の直後に足す。\n\n"
              "見出しは畳んだ状態で開く (=#+STARTUP: overview=)。TAB で開閉、\n"
              "=C-c C-n= / =C-c C-p= で見出しを渡り歩ける。\n")
      (dolist (group data)
        (insert (format "\n* %s\n"
                        (my:emoji-sample--label (car group)
                                                my:emoji-sample-group-names)))
        (when (equal (car group) "Component")
          (insert "\n肌の色は 5 段階。人や手の絵文字の直後に置くと色が変わる。\n"
                  ": 👍 + 🏻🏼🏽🏾🏿 → 👍🏻 👍🏼 👍🏽 👍🏾 👍🏿\n"))
        (dolist (sub (cdr group))
          (when (cdr sub)
            (setq total (+ total (length (cdr sub))))
            (insert (format "\n** %s\n"
                            (my:emoji-sample--label
                             (car sub) my:emoji-sample-subgroup-names)))
            (my:emoji-sample--insert-rows (cdr sub))))))
    (message "%s: %d 文字 / %d グループ" output total (length data))
    output))

(provide 'make-emoji-sample)
;;; make-emoji-sample.el ends here
