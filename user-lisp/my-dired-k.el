;;; my-dired-k.el --- dired の更新日時とサイズを色分けする  -*- lexical-binding: t -*-

;;; Commentary:

;; dired の**更新日時**と**ファイルサイズ**の桁に色を付ける。新しいものほど
;; 明るく、大きいものほど赤い。色の表と段階は `dired-k' (syohex、zsh の
;; `k' の移植) をそのまま引き継いでいる。
;;
;; `dired-k' は 2026-08-30 に diff-hl へ統合して外した (my-vc.el)。あちらが
;; 担ったのは **git の状態**だけで、`dired-k' のもう 1 つの仕事だった
;; **サイズと日時の色分け**は落ちていた。それをここで取り戻す。git の状態は
;; 従来どおり `diff-hl-dired' が出すので、ここでは一切触らない。
;;
;;; dired-k との違い: 一覧を作るときに印を付け、色は font-lock が塗る
;;
;; `dired-k' は `dired-after-readin-hook' から全行を舐め、1 行ごとに
;; `file-attributes' を叩いて overlay を 2 つ張っていた。ここでは張らない。
;;
;;   1. **一覧を作る ls-lisp に advice を当て**、サイズと日時の文字列に
;;      「元の値」をテキストプロパティ (`my:dired-k-size' /
;;      `my:dired-k-time') として載せる。桁の位置は ls-lisp 自身が知って
;;      いるので、**行をパースしなくてよい**。stat も増えない
;;      (ls-lisp は既に属性を持っている)
;;   2. font-lock のキーワードでそのプロパティを探し、**その場で色を計算
;;      して塗る**
;;
;; この形には実利がある。
;;
;;   - 4862 件でも overlay が 0 個。色が付くのは jit-lock が実際に表示した
;;     行だけなので、大きなディレクトリでも代金を払わない
;;   - `dired-relist-entry' での **1 行の貼り替え** (`my-dired-watch')、
;;     `dired-subtree' のサブツリー挿入、`dired-add-entry' のどれも同じ
;;     ls-lisp を通るので、**追加の手当てが要らない**。`nerd-icons' の
;;     アイコンを自分で付け直しているのとは対照的
;;   - 色は塗るときに計算するので、`font-lock-flush' で今の時刻に揃う
;;
;;; 【重要】face をテキストプロパティで載せてはいけない
;;
;; `dired-mode' の `font-lock-defaults' は
;; `(dired-font-lock-keywords t nil nil beginning-of-line)' で、2 番目の
;; **KEYWORDS-ONLY が t**。それでも `font-lock-default-fontify-region' は
;; 先頭で `font-lock-unfontify-region' を**無条件に**呼ぶ (font-lock.el)。
;; あれは `face' と `font-lock-face' を `remove-list-of-text-properties' で
;; 消すので、一覧を作るときに `face' を載せても **jit-lock が最初にその行を
;; 表示した瞬間に消える**。`dired-k' が overlay を使っていた理由がこれ。
;;
;; そこで載せるのは **face ではない自前のプロパティ**にして、`face' は
;; font-lock に塗らせる。font-lock が付けた face は font-lock が管理する
;; ので消えない。
;;
;;; 【重要】効くのは ls-lisp が一覧を作っているときだけ
;;
;; Windows は ls-lisp が既定、macOS は `my-dired.el' が
;; `ls-lisp-use-insert-directory-program' を nil にしている。**Linux は
;; GNU ls** なので印が付かず、色も付かない (エラーにはならない)。
;; 必要になったら行をパースする経路を足すことになるが、桁の構成が
;; `ls-lisp-verbosity' や switches で変わるので安くはない。

;;; Code:

(require 'dired)

(defgroup my:dired-k nil
  "dired の更新日時とサイズの色分け。"
  :group 'dired)

;; `dired-k' の `dired-k-date-colors' (dark 版) と同じ。car は「それより
;; 新しければ」の境界で、単位は秒。60 = 1 分 / 3600 = 1 時間 /
;; 86400 = 1 日 / 604800 = 1 週 / 2419200 = 4 週 / 15724800 = 半年 /
;; 31449600 = 1 年 / 62899200 = 2 年。
;;
;; 先頭の `(0 . "red")' は**未来の日時**。経過秒が負になるので当たる。
;;
;; 末尾の car が nil のエントリは `dired-k' の `finally return' 相当で、
;; どの境界にも当たらなかったとき (= 2 年より古い) に使う。**grey35 より
;; 明るい grey50 に戻る**のは upstream もそうなっている。古い順に暗く
;; したいなら "grey30" などに変える。
;;
;; 明るい背景向けの表は upstream にもあり、こうだった。
;;   ((0 . "red") (60 . "grey0") (3600 . "grey10") (86400 . "grey25")
;;    (604800 . "grey40") (2419200 . "grey40") (15724800 . "grey50")
;;    (31449600 . "grey65") (62899200 . "grey85") (nil . "grey50"))
(defcustom my:dired-k-date-colors
  '((0        . "red")
    (60       . "white")
    (3600     . "grey90")
    (86400    . "grey80")
    (604800   . "grey65")
    (2419200  . "grey65")
    (15724800 . "grey50")
    (31449600 . "grey45")
    (62899200 . "grey35")
    (nil      . "grey50"))
  "更新日時の色。car は経過秒の上限、cdr は色。car が nil なら残り全部。
前から順に見て、経過秒がその car より小さい最初のものを使う。"
  :type '(repeat (cons (choice (integer :tag "経過秒の上限") (const :tag "残り全部" nil))
                       (string :tag "色")))
  :group 'my:dired-k)

;; `dired-k' の `dired-k-size-colors' と同じ。単位はバイト。
;; 1K / 2K / 3K / 5K / 10K / 20K / 40K / 100K / 256K / 512K が境界で、
;; それより大きければ赤。
(defcustom my:dired-k-size-colors
  '((1024   . "chartreuse4")
    (2048   . "chartreuse3")
    (3072   . "chartreuse2")
    (5120   . "chartreuse1")
    (10240  . "yellow3")
    (20480  . "yellow2")
    (40960  . "yellow")
    (102400 . "orange3")
    (262144 . "orange2")
    (524288 . "orange")
    (nil    . "red"))
  "ファイルサイズの色。car はバイト数の上限、cdr は色。car が nil なら残り全部。"
  :type '(repeat (cons (choice (integer :tag "バイト数の上限") (const :tag "残り全部" nil))
                       (string :tag "色")))
  :group 'my:dired-k)

(defcustom my:dired-k-weight 'bold
  "色分けした桁に付ける太さ。nil なら太さを指定しない。
`dired-k' は常に bold だった。"
  :type '(choice (const :tag "太字 (dired-k と同じ)" bold)
                 (const :tag "指定しない" nil)
                 (symbol :tag "face の :weight に渡す値"))
  :group 'my:dired-k)

;;; ---------------------------------------------- 色を決める

(defun my:dired-k--color (table key)
  "TABLE を前から見て、KEY がその car より小さい最初の cdr を返す。
car が nil のエントリは無条件に当たる。"
  (let (color)
    (while (and table (not color))
      (let ((border (caar table)))
        (when (or (null border) (< key border))
          (setq color (cdar table))))
      (setq table (cdr table)))
    color))

(defun my:dired-k--spec (color)
  "COLOR から face の属性リストを作る。COLOR が nil なら nil。"
  (when color
    (if my:dired-k-weight
        (list :foreground color :weight my:dired-k-weight)
      (list :foreground color))))

(defun my:dired-k--size-face ()
  "font-lock から呼ばれる。今のマッチのサイズに対応する face を返す。"
  (let ((size (get-text-property (match-beginning 0) 'my:dired-k-size)))
    (and (numberp size)
         (my:dired-k--spec (my:dired-k--color my:dired-k-size-colors size)))))

(defun my:dired-k--date-face ()
  "font-lock から呼ばれる。今のマッチの日時に対応する face を返す。

経過秒は**塗るときに**計算する。`font-lock-flush' すれば今の時刻で
塗り直される。"
  (let ((time (get-text-property (match-beginning 0) 'my:dired-k-time)))
    (and time
         (my:dired-k--spec
          (my:dired-k--color my:dired-k-date-colors
                             (float-time (time-subtract nil time)))))))

;;; ---------------------------------------------- 一覧に印を付ける

(defun my:dired-k--mark-time (orig file-attr &optional time-index)
  "ORIG (`ls-lisp-format-time') の戻り値に、その時刻を載せて返す。

TIME-INDEX は `-u' / `-c' で 4 (atime) や 6 (ctime) になる。**表示して
いる時刻**で色を決めるので、`nth' の添字はそれに合わせる。"
  (let ((str (copy-sequence (funcall orig file-attr time-index)))
        (time (nth (or time-index 5) file-attr)))
    (when (and time (> (length str) 0))
      (put-text-property 0 (length str) 'my:dired-k-time time str))
    str))

(defun my:dired-k--mark-size (orig file-size human-readable)
  "ORIG (`ls-lisp-format-file-size') の戻り値に、そのサイズを載せて返す。

戻り値は `ls-lisp-filesize-d-fmt' による**右詰め**なので先頭に空白が
並ぶ。印は数字の部分だけに付ける。`dired-align-file' は桁を揃えるために
空白を足したり削ったりするので、**空白を印の中に含めない**こと。"
  (let* ((str (copy-sequence (funcall orig file-size human-readable)))
         (beg (string-match-p "[^ ]" str)))
    (when (and beg (numberp file-size))
      (put-text-property beg (length str) 'my:dired-k-size file-size str))
    str))

;;; ---------------------------------------------- font-lock

(defun my:dired-k--search (prop limit)
  "PROP が載っている次の範囲を LIMIT までに探し、`match-data' に入れる。

font-lock は `font-lock-extend-region-wholelines' で領域を行単位に
丸めてから呼ぶので、印の途中から始まることはない。"
  (let (found)
    (while (and (not found) (< (point) limit))
      (if (get-text-property (point) prop)
          (let ((end (or (next-single-property-change (point) prop nil limit)
                         limit)))
            (set-match-data (list (point) end))
            (goto-char end)
            (setq found t))
        (goto-char (or (next-single-property-change (point) prop nil limit)
                       limit))))
    found))

(defun my:dired-k--match-size (limit)
  "font-lock のマッチャ。サイズの桁を探す。"
  (my:dired-k--search 'my:dired-k-size limit))

(defun my:dired-k--match-date (limit)
  "font-lock のマッチャ。日時の桁を探す。"
  (my:dired-k--search 'my:dired-k-time limit))

(defconst my:dired-k--keywords
  '((my:dired-k--match-size (0 (my:dired-k--size-face) t))
    (my:dired-k--match-date (0 (my:dired-k--date-face) t)))
  "`dired-mode' に足す font-lock キーワード。")

(defun my:dired-k--setup ()
  "`dired-mode-hook'。font-lock キーワードをこのバッファに足す。

MODE に nil を渡すのは意図的。`font-lock-add-keywords' の docstring に
あるとおり、シンボルを渡すと**派生モードに効かない**
 (`dired-sidebar-mode' / `wdired-mode')。フックから nil で呼べば
`font-lock-set-defaults' も内側で済ませてくれる。"
  (font-lock-add-keywords nil my:dired-k--keywords t))

(defun my:dired-k--refresh-buffers (add)
  "既にある dired バッファのキーワードを足す / 外して塗り直す。

**既に開いている一覧には印が付いていない** (印は ls-lisp が一覧を作る
ときに載せる) ので、有効にした直後に色を出すには `g' が要る。"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'dired-mode)
        (if add
            (font-lock-add-keywords nil my:dired-k--keywords t)
          (font-lock-remove-keywords nil my:dired-k--keywords))
        (font-lock-flush)))))

;;; ---------------------------------------------- モード

;;;###autoload
(define-minor-mode my:dired-k-mode
  "dired の更新日時とファイルサイズを色分けする。"
  :global t
  :lighter nil
  (if my:dired-k-mode
      (progn
        ;; ls-lisp が未ロードでも advice は張れる (`defalias' が引き継ぐ)。
        (advice-add 'ls-lisp-format-time :around #'my:dired-k--mark-time)
        (advice-add 'ls-lisp-format-file-size :around #'my:dired-k--mark-size)
        (add-hook 'dired-mode-hook #'my:dired-k--setup)
        (my:dired-k--refresh-buffers t))
    (advice-remove 'ls-lisp-format-time #'my:dired-k--mark-time)
    (advice-remove 'ls-lisp-format-file-size #'my:dired-k--mark-size)
    (remove-hook 'dired-mode-hook #'my:dired-k--setup)
    (my:dired-k--refresh-buffers nil)))

(my:dired-k-mode 1)

(provide 'my-dired-k)
;;; my-dired-k.el ends here
