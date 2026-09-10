;;; make-nerd-font-sample.el --- Nerd Font と starship の記号一覧を作る  -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; `etc/nerd-font-sample.org' を作る。starship のプロンプトに出ている
;; アイコン (󰍲 など) を選ぶためのもの。
;;
;;   M-x my:nerd-font-sample-generate
;;
;; 【重要】GUI で実行すること。グリフの有無は `font-get-glyphs' で実際の
;; フォントに問い合わせるので、batch では判定できない (フォントを開けない)。
;;
;; 出来上がるのは 2 部構成。
;;
;;   1. starship で使っている記号。~/.config/starship.toml と
;;      `starship print-config' (既定値込み) から、値に非 ASCII を含む
;;      ものを拾ってキーと並べる
;;   2. Nerd Fonts のコードチャート。16 個ずつ並べ、行頭にその行の
;;      先頭コードポイントを書く。数えれば任意の 1 個の綴りが分かる
;;
;; 対象のフォントは `my:nerd-font-family' が選んだもの
;; (このマシンでは fonts/NFM.ttf = Symbols Nerd Font Mono)。
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst my:nerd-font-sample-columns 16
  "コードチャート 1 行あたりの文字数。

16 にしてあるのは、行頭のコードポイントの下位 1 桁が 0 で揃い、
何番目かを数えるだけで綴りが分かるため。")

(defconst my:nerd-font-sample-missing "·"
  "グリフが無いコードポイントの代わりに置く文字。

空けてしまうと 16 個の並びがずれて数えられなくなるので、必ず 1 文字
埋める。`·' (U+00B7) を使うのは、`site-lisp/eaw.el' が ambiguous を
幅 2 にしているこの設定では 16px になり、Nerd Font のアイコン (15px)
とほぼ同じ幅で並ぶため。")

(defconst my:nerd-font-sample-ranges
  '((#xE000  #xE00A  "Pomicons")
    (#xE0A0  #xE0D7  "Powerline (Symbols + Extra)")
    (#xE200  #xE2A9  "Font Awesome Extension")
    (#xE300  #xE3E3  "Weather")
    (#xE5FA  #xE6BB  "Seti-UI + Custom")
    (#xE700  #xE958  "Devicons")
    (#xEA60  #xEC84  "Codicons")
    (#xED00  #xEFCF  "Font Awesome")
    (#xF000  #xF2FF  "Font Awesome (v2 の旧位置)")
    (#xF300  #xF385  "Font Logos (OS・ディストリ)")
    (#xF400  #xF533  "Octicons")
    (#xF0001 #xF1AF0 "Material Design Icons"))
  "Nerd Fonts のアイコンセットと、そのコードポイントの範囲。

**このマシンのフォントを実際に走査して決めた値**であって、Nerd Fonts の
公式表そのままではない (版によってずれる)。走査の結果は 23 ブロック /
10610 グリフで、すべてこの表のどれかに収まった。範囲の外にグリフが
見つかったときは「範囲外」の節に出る。")

(defconst my:nerd-font-sample-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "このファイルの置き場。出力先の既定に使う。
ロード時に採ること (`--eval' の時点では `load-file-name' は nil)。")

;;; フォント

(defun my:nerd-font-sample--font ()
  "Nerd Font を開いて (FAMILY . FONT-OBJECT) を返す。"
  (unless (display-graphic-p)
    (user-error "GUI で実行すること (batch ではフォントを開けない)"))
  (let* ((family (or (and (fboundp 'my:nerd-font-family) (my:nerd-font-family))
                     "Symbols Nerd Font Mono"))
         (entity (and family (find-font (font-spec :family family))))
         (font (and entity (open-font entity))))
    (unless font
      (user-error "Nerd Font が見つからない (fonts/NFM.ttf を OS にインストールすること)"))
    (cons family font)))

(defun my:nerd-font-sample--glyphs (font from to)
  "FONT が FROM..TO のどれを持つかを、真偽値のベクタで返す。
1 個ずつ聞くと遅いので範囲ごとにまとめて渡す (実測 14592 個で 1.2 秒)。"
  (let* ((vec (vconcat (number-sequence from to)))
         (glyphs (font-get-glyphs font 0 (length vec) vec)))
    (cl-map 'vector
            (lambda (i) (and (vectorp glyphs) (aref glyphs i) t))
            (number-sequence 0 (1- (length vec))))))

;;; starship

(defconst my:nerd-font-sample--key-re "\\`\\([A-Za-z0-9_.\"$-]+\\) *= *\\(.*\\)\\'"
  "TOML の `キー = 値' にあたる行。`#' 始まりのコメントは弾く。")

(defun my:nerd-font-sample--toml-pairs (text)
  "TOML の TEXT から (絶対キー . 値) を出現順に返す。

【重要】`\"\"\"' の複数行文字列を 1 つに繋ぐこと。starship の
`format' はプロンプトの並びを書くところで、**Powerline の区切り
 () はここにしか出てこない**。行ごとに見ていると、値が
`\"\"\"' だけの行になって記号を 1 つも拾えない (実際に取りこぼした)。"
  (let ((section "") (pairs nil) (key nil) (buf nil))
    (dolist (line (split-string text "\n"))
      (setq line (string-trim-right line "\r"))
      (cond
       ;; 複数行文字列の途中。
       (key
        (if (string-match "\"\"\"" line)
            (progn
              (push (cons key (concat buf "\n" (substring line 0 (match-beginning 0))))
                    pairs)
              (setq key nil buf nil))
          (setq buf (concat buf "\n" line))))
       ((string-match "\\`\\[\\([^]]+\\)\\]" line)
        (setq section (match-string 1 line)))
       ((string-match my:nerd-font-sample--key-re line)
        (let ((k (if (string-empty-p section)
                     (match-string 1 line)
                   (concat section "." (match-string 1 line))))
              (value (match-string 2 line)))
          (cond
           ;; key = """ で始まる。同じ行で閉じることもある。
           ((string-prefix-p "\"\"\"" value)
            (let ((rest (substring value 3)))
              (if (string-match "\"\"\"" rest)
                  (push (cons k (substring rest 0 (match-beginning 0))) pairs)
                (setq key k buf rest))))
           (t (push (cons k value) pairs)))))))
    (nreverse pairs)))

(defun my:nerd-font-sample--toml-entries (text)
  "TOML の TEXT から (KEY . SYMBOLS) を出現順に返す。

SYMBOLS はその値に現れた非 ASCII 文字を、出現順に重複を除いて連ねた
文字列。**値そのものは返さない。** `format' は 1 行に収まらない長さの
ことがあり、欲しいのはそこに埋まっている記号だけなので。"
  (delq nil
        (mapcar (lambda (pair)
                  (let ((symbols (my:nerd-font-sample--extract-symbols (cdr pair))))
                    (unless (string-empty-p symbols)
                      (cons (car pair) symbols))))
                (my:nerd-font-sample--toml-pairs text))))

(defun my:nerd-font-sample--extract-symbols (string)
  "STRING から非 ASCII の文字だけを取り出して重複を除いた文字列を返す。"
  (my:nerd-font-sample--dedup
   (apply #'string (seq-filter (lambda (c) (> c 127)) (string-to-list string)))))

(defun my:nerd-font-sample--dedup (string)
  "STRING から重複した文字を落とす (出現順は保つ)。"
  (let ((seen nil) (out nil))
    (dolist (c (string-to-list string))
      (unless (or (memq c seen) (memq c '(?\s #xFE0F #x200D)))
        (push c seen)
        (push c out)))
    (apply #'string (nreverse out))))

(defun my:nerd-font-sample--starship-default ()
  "`starship print-config' の出力を返す。starship が無ければ nil。"
  (when (executable-find "starship")
    (with-temp-buffer
      (when (zerop (call-process "starship" nil t nil "print-config"))
        (buffer-string)))))

(defun my:nerd-font-sample--starship-version ()
  "starship のバージョン文字列。取れなければ nil。"
  (when (executable-find "starship")
    (with-temp-buffer
      (when (zerop (call-process "starship" nil t nil "--version"))
        (goto-char (point-min))
        (string-trim (buffer-substring (point) (line-end-position)))))))

(defun my:nerd-font-sample--insert-entries (entries)
  "(KEY . SYMBOLS) の ENTRIES をキーで揃えて挿入する。"
  (let ((width (apply #'max 8 (mapcar (lambda (e) (length (car e))) entries))))
    (dolist (e entries)
      (insert (format "%s  %s\n"
                      (string-pad (car e) width) (cdr e))))))

;;; コードチャート

(defun my:nerd-font-sample--insert-chart (font from to)
  "FONT の FROM..TO を 16 個ずつ挿入する。グリフが 1 つも無い行は飛ばす。
挿入した文字数を返す。"
  (let ((base (* my:nerd-font-sample-columns
                 (/ from my:nerd-font-sample-columns)))
        (found 0))
    (while (<= base to)
      (let* ((have (my:nerd-font-sample--glyphs
                    font base (+ base my:nerd-font-sample-columns -1)))
             (n (seq-count #'identity have)))
        (when (> n 0)
          (setq found (+ found n))
          (insert (format "%-8X" base))
          (dotimes (i my:nerd-font-sample-columns)
            (insert (if (aref have i)
                        (string (+ base i))
                      my:nerd-font-sample-missing)
                    (if (= i (1- my:nerd-font-sample-columns)) "" " ")))
          (insert "\n")))
      (setq base (+ base my:nerd-font-sample-columns)))
    found))

;;;###autoload
(defun my:nerd-font-sample-generate (&optional output)
  "Nerd Font と starship の記号一覧を OUTPUT に書き出す。
OUTPUT の既定は このファイルと同じディレクトリの nerd-font-sample.org。"
  (interactive)
  (let* ((output (expand-file-name
                  (or output (expand-file-name "nerd-font-sample.org"
                                               my:nerd-font-sample-directory))))
         (font-cell (my:nerd-font-sample--font))
         (family (car font-cell))
         (font (cdr font-cell))
         (user-toml (expand-file-name "~/.config/starship.toml"))
         (total 0))
    (with-temp-file output
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert "#+TITLE: Nerd Font と starship の記号\n"
              "#+STARTUP: overview\n\n")
      (insert "starship のプロンプトに出ている記号を選ぶための一覧。\n"
              (format "グリフの有無は =%s= に実際に問い合わせて作った。\n" family)
              "作り直すときは =M-x my:nerd-font-sample-generate= (GUI で)。\n\n")
      (insert "コードチャートは 16 個ずつ並べてあり、行頭がその行の先頭の\n"
              "コードポイント。左から数えれば任意の 1 個の綴りが分かる。\n"
              (format "グリフが無いところは =%s= で埋めてある。\n"
                      my:nerd-font-sample-missing)
              "starship.toml には記号をそのまま貼ればよい (=\\uXXXX= の\n"
              "エスケープは要らない)。\n\n")
      (insert "**桁は揃わない。** アイコンは 15px、半角は 8px なので、\n"
              "16 個並べても列は一致しない。数えるための並びであって、\n"
              "表ではない。\n")

      ;; 1. starship
      (insert "\n* starship で使っている記号\n")
      (when (file-readable-p user-toml)
        (insert "\n** いまの設定 (~/.config/starship.toml)\n")
        (let ((entries (my:nerd-font-sample--toml-entries
                        (with-temp-buffer
                          (let ((coding-system-for-read 'utf-8))
                            (insert-file-contents user-toml))
                          (buffer-string)))))
          (my:nerd-font-sample--insert-entries entries)))
      (let ((default (my:nerd-font-sample--starship-default)))
        (when default
          (insert (format "\n** starship の既定値 (%s)\n"
                          (or (my:nerd-font-sample--starship-version) "starship")))
          (insert "\n=starship print-config= の出力。設定で上書きしていない\n"
                  "モジュールはこの記号が出る。\n\n")
          (my:nerd-font-sample--insert-entries
           (my:nerd-font-sample--toml-entries default))))

      ;; 2. コードチャート
      (insert "\n* Nerd Fonts の一覧\n")
      (dolist (range my:nerd-font-sample-ranges)
        (cl-destructuring-bind (from to name) range
          (if (equal name "Material Design Icons")
              ;; 6896 個あるので 1024 ごとに区切る。畳んだときの
              ;; 見出しが 7 行で済み、開いても 64 行で見渡せる。
              (progn
                (insert (format "\n** %s (%X–%X)\n" name from to))
                (let ((base (logand from (lognot 1023))))
                  (while (<= base to)
                    (let ((end (min to (+ base 1023))))
                      (insert (format "\n*** %X–%X\n" (max from base) end))
                      (setq total (+ total (my:nerd-font-sample--insert-chart
                                            font (max from base) end))))
                    (setq base (+ base 1024)))))
            (insert (format "\n** %s (%X–%X)\n" name from to))
            (setq total (+ total (my:nerd-font-sample--insert-chart font from to))))))

      ;; 範囲表から漏れたグリフ。表が古くなったときにここに出る。
      (let ((others nil))
        (dolist (block '((#xE000 . #xF8FF) (#xF0000 . #xF1FFF)))
          (let* ((from (car block)) (to (cdr block))
                 (have (my:nerd-font-sample--glyphs font from to)))
            (dotimes (i (length have))
              (let ((c (+ from i)))
                (when (and (aref have i)
                           (not (cl-find-if (lambda (r) (<= (nth 0 r) c (nth 1 r)))
                                            my:nerd-font-sample-ranges)))
                  (push c others))))))
        (when others
          (setq others (nreverse others))
          (insert (format "\n** 範囲外 (%d 個)\n" (length others)))
          (insert "`my:nerd-font-sample-ranges' のどの範囲にも入らなかったもの。\n"
                  "フォントを更新して増えたぶんはここに出るので、表に足すこと。\n")
          (let ((n 0))
            (dolist (c others)
              (insert (format "%X %s%s" c (string c)
                              (if (zerop (% (setq n (1+ n)) 8)) "\n" "  "))))
            (unless (zerop (% n 8)) (insert "\n"))))))
    (message "%s: %d グリフ (%s)" output total family)
    (when (called-interactively-p 'interactive)
      (find-file output))
    output))

(provide 'make-nerd-font-sample)
;;; make-nerd-font-sample.el ends here
