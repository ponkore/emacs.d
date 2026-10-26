;;; my-keyhelp.el --- 有効なキーバインドを一覧表で見る  -*- lexical-binding: t -*-
;;; Commentary:
;; `describe-bindings' (<f1> b) の置き換え。モード別のセクションに分かれた
;; テキストではなく、`tabulated-list-mode' の 1 枚の表として出す。
;;
;;   Key | Command | Context | State
;;
;; `describe-buffer-bindings' (C 実装) の出力はパースしない。`map-keymap' を
;; 自前で回して (キー . 束縛) を集める。**こうしないと Context 列が作れない**
;; (テキストからは、その行がどのキーマップ由来かが分からない)。
;;
;; 実測 (2026-10-27、dired バッファ):
;;
;;   有効マップの走査          3 ms / 1189 件 (global 1014 + dired 159 + minor 16)
;;   `key-binding' による判定  1 ms / 159 件
;;   `help-fns-find-keymap-name'  1〜5 ms (`mapatoms' なのでキャッシュする)
;;
;;; Code:

(require 'tabulated-list)
(require 'help-fns)                     ; help-fns-find-keymap-name
(require 'seq)

(defgroup my-keyhelp nil
  "有効なキーバインドを一覧表で見る。"
  :group 'help)

(defcustom my:keyhelp-exclude-events
  '(menu-bar tool-bar tab-bar tab-line mode-line header-line
    vertical-line horizontal-scroll-bar vertical-scroll-bar
    left-fringe right-fringe left-margin right-margin
    left-edge right-edge top-edge bottom-edge
    top-left-corner top-right-corner bottom-left-corner bottom-right-corner
    left-divider right-divider bottom-divider)
  "先頭イベントがこれらのキーは表に出さない。
`menu-bar' 配下はメニュー定義 (`menu-item' の入れ子) なので、キーバインドの
一覧としては雑音にしかならない。実測で dired バッファの `<menu-bar> <subdir>'
1 件だけで 2 KB を超えた。"
  :type '(repeat symbol)
  :group 'my-keyhelp)

(defcustom my:keyhelp-show-shadowed t
  "非 nil なら、他のキーマップに隠されて実際には効かない束縛も出す。
dired バッファの実測では 159 件中 12 件がこれ (`special-mode-map' の SPC /
DEL / ? / > / < が `dired-mode-map' に上書きされている)。"
  :type 'boolean
  :group 'my-keyhelp)

(defconst my:keyhelp--max-depth 8
  "プレフィクスを掘る深さの上限。キーマップの循環参照に対する保険。")

(defconst my:keyhelp--fields '(all key command context state)
  "絞り込みの対象列。TAB でこの順に回る。")

(defcustom my:keyhelp-default-field 'all
  "`/' や `C-s' を押したときの、絞り込みの既定の対象列。
`all' なら Key / Command / Context / State を連結したものに当てる。

空白を AND 区切りにしないので、`all' でも絞り込みは十分に鋭い
\(実測で \"C-c e\" は Key 列でも全体でも 1 件)。列を絞るのは、
同じ語が複数の列に現れるとき (\"dired\" が Command 列と Context 列の
両方に出る) に効く。"
  :type '(choice (const :tag "全体" all)
                 (const :tag "キー" key)
                 (const :tag "コマンド" command)
                 (const :tag "由来 (Context)" context)
                 (const :tag "状態 (State)" state))
  :group 'my-keyhelp)

;;; --------------------------------------------------
;;; 収集
;;; --------------------------------------------------

(defvar my:keyhelp--name-cache (make-hash-table :test 'eq :weakness 'key)
  "キーマップ -> その値を持つ変数名。`help-fns-find-keymap-name' の結果。")

(defun my:keyhelp--map-name (map)
  "MAP を値に持つ変数のシンボルを返す。見つからなければ nil。"
  (let ((hit (gethash map my:keyhelp--name-cache 'miss)))
    (if (eq hit 'miss)
        ;; nil もキャッシュしたいので 'miss をセンチネルにする。
        (puthash map (ignore-errors (help-fns-find-keymap-name map))
                 my:keyhelp--name-cache)
      hit)))

(defun my:keyhelp--toplevel-names ()
  "いま有効になりうるキーマップに「モードの名前」を付けた alist を返す。
`current-active-maps' が返すキーマップは、この表から `eq' で引ける
\(実測で 12 マップ全てに名前が付いた)。`help-fns-find-keymap-name' だと
`dired-mode-map' のような変数名になるが、Context 列には `dired-mode' /
`yas-minor-mode' のようなモード名のほうが分かりやすい。"
  (let (tbl)
    (push (cons (current-global-map) 'global) tbl)
    (when (current-local-map)
      (push (cons (current-local-map) major-mode) tbl))
    (dolist (e minor-mode-map-alist)
      (when (consp e) (push (cons (cdr e) (car e)) tbl)))
    (dolist (e minor-mode-overriding-map-alist)
      (when (consp e) (push (cons (cdr e) (car e)) tbl)))
    ;; cua-mode などはここ経由で入る。要素は alist か、alist を値に持つ変数。
    (dolist (al emulation-mode-map-alists)
      (dolist (e (if (symbolp al)
                     (and (boundp al) (symbol-value al))
                   al))
        (when (consp e) (push (cons (cdr e) (car e)) tbl))))
    (when overriding-terminal-local-map
      (push (cons overriding-terminal-local-map 'overriding-terminal-local-map) tbl))
    (when overriding-local-map
      (push (cons overriding-local-map 'overriding-local-map) tbl))
    tbl))

(defun my:keyhelp--walk (map prefix depth acc)
  "MAP を再帰的に走査し (KEYVEC . DEF) を ACC の car に積む。
親キーマップの束縛も含む (分離は `my:keyhelp--own' が行う)。"
  (when (and (keymapp map) (< depth my:keyhelp--max-depth))
    (map-keymap
     (lambda (event def)
       (let ((key (vconcat prefix (vector event))))
         (cond
          ;; char-table の範囲 (START . END) と default binding (t) は掘らない。
          ;; `key-description' はどちらも扱える (実測で "&..~" / "<t>")。
          ((or (consp event) (eq event t))
           (push (cons key def) (car acc)))
          ((keymapp def)
           (my:keyhelp--walk (if (and (symbolp def) (fboundp def))
                                 (symbol-function def)
                               def)
                             key (1+ depth) acc))
          (t (push (cons key def) (car acc))))))
     map)))

(defun my:keyhelp--own (map)
  "MAP 自身の束縛だけを ((KEYVEC . DEF) ...) で返す。

【重要】`map-keymap' は親キーマップの束縛も返す。親を別に走査して差し引かないと、
`dired-mode-map' の SPC (`dired-next-line') と `special-mode-map' の SPC
\(`scroll-up-command') が同じ Context の行として並ぶ。

比較はキーの文字列と束縛の `eq' で行う。DEF にラムダや `menu-item' の入れ子が
来るので、`equal' のハッシュを張ると深い比較になる。"
  (let ((acc (list nil)))
    (my:keyhelp--walk map [] 0 acc)
    (let ((own (car acc))
          (parent (keymap-parent map)))
      (if (null parent)
          own
        (let ((pacc (list nil))
              (ptbl (make-hash-table :test 'equal)))
          (my:keyhelp--walk parent [] 0 pacc)
          (dolist (p (car pacc))
            (let ((k (my:keyhelp--key-string (car p))))
              (puthash k (cons (cdr p) (gethash k ptbl)) ptbl)))
          (seq-remove (lambda (e)
                        (memq (cdr e)
                              (gethash (my:keyhelp--key-string (car e)) ptbl)))
                      own))))))

(defun my:keyhelp--key-string (key)
  "KEY (キーベクタ) を表示用の文字列にする。"
  (if (and (> (length key) 1) (eq (aref key 0) 'remap))
      ;; `key-description' は "<remap> <self-insert-command>" と出すが、
      ;; 表の Key 列では [remap ...] のほうが短く読みやすい。
      (format "[remap %s]"
              (mapconcat (lambda (e) (format "%s" e)) (substring key 1) " "))
    (condition-case nil
        (key-description key)
      (error (format "%S" key)))))

(defun my:keyhelp--def-string (def)
  "DEF (束縛) を表示用の文字列にする。"
  (cond
   ;; `keymapp' はシンボルにも真を返すので symbolp を先に見る。
   ((symbolp def) (symbol-name def))
   ((eq (car-safe def) 'menu-item) (format "(menu) %s" (cadr def)))
   ;; 古い形式のメニュー項目 (STRING . DEFN)。`C-down-mouse-2' 配下の
   ;; facemenu などがこれで、そのままだと Command 列に
   ;; ("Background Color" . facemenu-background-menu) と出る。
   ((and (consp def) (stringp (car def)))
    (format "(menu) %s" (car def)))
   ((keymapp def) "(prefix)")
   ((or (stringp def) (vectorp def))
    (format "(macro) %s" (condition-case nil (key-description def)
                           (error "?"))))
   ((functionp def) "(lambda)")
   (t (format "%S" def))))

(defun my:keyhelp--active-p (key def)
  "KEY を押したときに実際に DEF が走るなら非 nil。
判定を `current-active-maps' の順序ではなく `key-binding' で行うのは、
プレフィクス自体が他のマップで潰されている場合 (その配下が丸ごと到達不能に
なる) まで正しく拾えるため。"
  (condition-case nil
      (let ((eff (key-binding key t)))
        (or (eq eff def)
            (and (symbolp def) (fboundp def) eff
                 (eq (indirect-function eff) (indirect-function def)))))
    ;; 範囲イベントを含むキーは `key-binding' に渡せない。安全側 (= 効いて
    ;; いることにする) へ倒す。
    (error t)))

(defun my:keyhelp--collect (buffer local-only)
  "BUFFER で有効な束縛を集めて返す。優先度の高いキーマップから順に並ぶ。
各行は (KEY-STRING COMMAND-STRING CONTEXT-STRING ACTIVE KEYVEC DEF)。
LOCAL-ONLY が非 nil ならグローバルマップを除く。"
  (with-current-buffer buffer
    (let ((names (my:keyhelp--toplevel-names))
          (seen (make-hash-table :test 'eq))
          (rows nil))
      (dolist (top (current-active-maps t (point)))
        (let ((tname (or (cdr (assq top names))
                         (my:keyhelp--map-name top)
                         'unknown)))
          (unless (and local-only (eq tname 'global))
            ;; 親チェーンも 1 段ずつ別の Context として出す。
            (let ((m top) (depth 0))
              (while (and (keymapp m) (< depth my:keyhelp--max-depth))
                (unless (gethash m seen)
                  (puthash m t seen)
                  (let ((cname (if (eq m top)
                                   tname
                                 (or (my:keyhelp--map-name m) tname))))
                    (dolist (e (my:keyhelp--own m))
                      (let ((key (car e))
                            (def (cdr e)))
                        ;; DEF が nil / `undefined' の束縛は「親の定義を打ち消す」
                        ;; ためのもので、キーバインドではない。出すと
                        ;; 「TAB / nil / yas-minor-mode / shadowed」のような
                        ;; 誤解を招く行になる (実際には TAB をそこで外して、
                        ;; 下のマップに落としている)。
                        (unless (or (null def)
                                    (eq def 'undefined)
                                    ;; メニューの区切り線 ("--" ...)。
                                    (and (consp def) (stringp (car def))
                                         (string-prefix-p "--" (car def)))
                                    (memq (aref key 0) my:keyhelp-exclude-events))
                          (push (list (my:keyhelp--key-string key)
                                      (my:keyhelp--def-string def)
                                      (symbol-name cname)
                                      (my:keyhelp--active-p key def)
                                      key def)
                                rows))))))
                (setq m (keymap-parent m)
                      depth (1+ depth)))))))
      (nreverse rows))))

;;; --------------------------------------------------
;;; 表
;;; --------------------------------------------------

(defvar-local my:keyhelp--all nil "収集した全行。")
(defvar-local my:keyhelp--query "" "絞り込み文字列。")
(defvar-local my:keyhelp--field 'key "絞り込みの対象列。")
(defvar-local my:keyhelp--shadowed t "シャドウされた束縛も出すか。")
(defvar-local my:keyhelp--source nil "どのバッファのバインドを見ているか。")
(defvar-local my:keyhelp--local-only nil "グローバルマップを除いているか。")

(defun my:keyhelp--row-state (row)
  "ROW の State 列の文字列。"
  (if (nth 3 row) "" "shadowed"))

(defun my:keyhelp--row-field (row field)
  "ROW の FIELD 列の文字列を返す。"
  (pcase field
    ('key (nth 0 row))
    ('command (nth 1 row))
    ('context (nth 2 row))
    ('state (my:keyhelp--row-state row))
    ;; `all' は 4 列を連結したものに当てる。State も含むので、"shadowed" で
    ;; 隠された束縛だけを引ける。
    (_ (concat (nth 0 row) " " (nth 1 row) " " (nth 2 row) " "
               (my:keyhelp--row-state row)))))

(defun my:keyhelp--field-label (&optional field)
  "FIELD (既定は現在の対象列) の表示名。"
  (pcase (or field my:keyhelp--field)
    ('key "Key")
    ('command "Command")
    ('context "Context")
    ('state "State")
    (_ "All")))

(defun my:keyhelp--match-p (row)
  "ROW が現在の絞り込み条件に一致するなら非 nil。

【重要】**空白で分割しない。** orderless のように空白を AND 区切りにすると、
\"C-c e\" が「C-c を含む」かつ「e を含む」になり、`C-c & C-n'
\(`yas-new-snippet' の e) まで一致する。これが `*Help*' + `consult-line' で
不要な行が大量に出ていた原因。

先頭の `^' と末尾の `$' だけアンカーとして効く (\"^C-c\" で `C-c' から
始まるキーだけ)。`C-c' を含むキーは dired バッファで 140 件あるが、
\"^C-c\" なら 20 件、\"C-c e\" なら 1 件になる。"
  (let ((q my:keyhelp--query))
    (or (string-empty-p q)
        (let* ((head (string-prefix-p "^" q))
               (tail (and (> (length q) 1) (string-suffix-p "$" q)))
               ;; `^' と `$' だけアンカーとして解釈する。それ以外は全部
               ;; リテラル (`C-c' の `-' や `[remap ...]' の `[' を正規表現に
               ;; 取られると、打った通りに絞れない)。
               (body (substring q (if head 1 0) (if tail -1 nil)))
               (re (concat (if head "\\`" "") (regexp-quote body)
                           (if tail "\\'" "")))
               (hay (my:keyhelp--row-field row my:keyhelp--field))
               ;; 大文字を含むクエリのときだけ区別する。キーの表記では C-c と
               ;; c-c が別物だが、区別を既定にすると打ちにくい。
               (case-fold-search (string= q (downcase q))))
          (and (string-match-p re hay) t)))))

(defun my:keyhelp--fit (s width)
  "S を表示幅 WIDTH に収める。溢れるぶんは省略記号で切る。

【重要】**`tabulated-list' は溢れた値を切り詰めない。** 列幅を超えると次の列を
右へ押すので、1 行だけ桁がずれる (`[remap exchange-point-and-mark]' は 31 文字
あり、Key 列 24 文字を 7 文字はみ出していた)。桁が揃わないのを直すのがこの
モードの目的なので、ここで詰める。"
  (if (<= (string-width s) width)
      s
    (truncate-string-to-width s width nil nil t)))

(defun my:keyhelp--entry (row widths)
  "ROW から `tabulated-list-entries' の 1 要素を作る。
WIDTHS は (KEY COMMAND CONTEXT) の列幅。"
  (let ((active (nth 3 row)))
    (list row
          (vector (propertize (my:keyhelp--fit (nth 0 row) (nth 0 widths))
                              'face (if active 'help-key-binding 'shadow))
                  (propertize (my:keyhelp--fit (nth 1 row) (nth 1 widths))
                              'face (if active 'default 'shadow))
                  (propertize (my:keyhelp--fit (nth 2 row) (nth 2 widths))
                              'face (if active 'font-lock-type-face 'shadow))
                  ;; 最後の列は幅で切られないのでそのまま。
                  (propertize (if active "" "shadowed") 'face 'shadow)))))

(defun my:keyhelp--column-width (rows field min max)
  "ROWS の FIELD 列に必要な幅を MIN..MAX に収めて返す。"
  (let ((w min))
    (dolist (r rows)
      (setq w (max w (1+ (string-width (my:keyhelp--row-field r field))))))
    (min w max)))

(defun my:keyhelp--format ()
  "`tabulated-list-format' を組み立てる。
幅は**絞り込み前の全行**から決める。絞り込みのたびに計算し直すと、
1 文字打つたびに列が動いて目で追えなくなる。

【重要】**列名は固定にする。** `tabulated-list-sort-key' は列名の文字列で
\(`assoc' で) 列を引くので、見出しに絞り込みのクエリを埋めると
**その列でソート中だった場合にソートが外れる**。絞り込みの状態は
`mode-name' と `mode-line-process' に出す。"
  (let* ((rows my:keyhelp--all)
         (kw (my:keyhelp--column-width rows 'key 8 24))
         (xw (my:keyhelp--column-width rows 'context 10 26))
         (sw 9)
         ;; 【重要】**表示しているウィンドウの幅で測る。** まだウィンドウに
         ;; 出ていないバッファで `window-body-width' を呼ぶと、選択ウィンドウ
         ;; (呼び出し元) の幅が返る。`my:keyhelp' が `pop-to-buffer' のあとに
         ;; もう一度 `my:keyhelp--refresh' を呼ぶのはこのため。
         (width (if-let* ((w (get-buffer-window (current-buffer) t)))
                    (window-body-width w)
                  (window-body-width)))
         (avail (max 16 (- width kw xw sw 6)))
         (cw (my:keyhelp--column-width rows 'command 12 avail)))
    (vector (list "Key" kw t)
            (list "Command" cw t)
            (list "Context" xw t)
            (list "State" sw t))))

(defun my:keyhelp--apply ()
  "絞り込みを適用して `tabulated-list-entries' を作り直す。表示はしない。"
  (let ((rows (seq-filter (lambda (r)
                            (and (or my:keyhelp--shadowed (nth 3 r))
                                 (my:keyhelp--match-p r)))
                          my:keyhelp--all)))
    (setq tabulated-list-format (my:keyhelp--format))
    ;; 列幅が決まってから行を作る (幅を超える値をここで詰めるため)。
    (let ((widths (list (nth 1 (aref tabulated-list-format 0))
                        (nth 1 (aref tabulated-list-format 1))
                        (nth 1 (aref tabulated-list-format 2)))))
      (setq tabulated-list-entries
            (mapcar (lambda (r) (my:keyhelp--entry r widths)) rows)))
    ;; 絞り込みの状態はモードラインに出す。`header-line' は列見出しで
    ;; 埋まっていて余白が無く (Command 列がウィンドウ幅の残りを全部使う)、
    ;; 列名に埋めるとソートが外れる (`my:keyhelp--format' を参照)。
    (setq mode-name (format "KeyHelp:%s" (my:keyhelp--field-label)))
    (setq mode-line-process
          (cond ((not (string-empty-p my:keyhelp--query))
                 ;; 【重要】`format-mode-line' は `%' を escape として解釈する。
                 ;; `%' はキーバインドの一つ (`my:match-paren') なので、
                 ;; 検索語にそのまま現れる。
                 (format " [%s %d/%d]"
                         (string-replace "%" "%%" my:keyhelp--query)
                         (length rows) (length my:keyhelp--all)))
                (my:keyhelp--local-only " [local]")
                (t nil)))
    (tabulated-list-init-header)))

(defun my:keyhelp--refresh (&optional keep-point)
  "絞り込みを適用して描き直す。"
  (my:keyhelp--apply)
  (tabulated-list-print keep-point)
  (unless keep-point
    (goto-char (point-min))))

(defun my:keyhelp--reload ()
  "元のバッファからバインドを取り直す。`tabulated-list-revert-hook' 用。"
  (setq my:keyhelp--all
        (and (buffer-live-p my:keyhelp--source)
             (my:keyhelp--collect my:keyhelp--source my:keyhelp--local-only)))
  (my:keyhelp--apply))

;;; --------------------------------------------------
;;; 絞り込み
;;; --------------------------------------------------

(defvar my:keyhelp--filter-buffer nil
  "絞り込み中の対象バッファ。ミニバッファローカル。")

(defun my:keyhelp--filter-update (&rest _)
  "ミニバッファの内容を対象バッファの絞り込みに反映する。
`after-change-functions' から呼ばれる。"
  (when (minibufferp)
    (let ((q (minibuffer-contents-no-properties))
          (buf my:keyhelp--filter-buffer))
      (when (buffer-live-p buf)
        ;; ここでエラーを出すとミニバッファの入力自体が壊れる
        ;; (`after-change-functions' の中なので、打った 1 文字が
        ;; 入ったのか入っていないのか分からなくなる)。
        (condition-case err
            (progn
              (with-current-buffer buf
                (setq my:keyhelp--query q)
                (my:keyhelp--refresh))
              ;; 絞り込むたび先頭から見せる。**`point' を動かすだけでは
              ;; 足りない。** いま選択されているのはミニバッファなので、
              ;; 表のウィンドウは window-point を別に持っている。
              (dolist (w (get-buffer-window-list buf nil t))
                (set-window-point w (with-current-buffer buf (point-min)))
                (set-window-start w (with-current-buffer buf (point-min)))))
          (error (message "keyhelp: %s" (error-message-string err))))))))

(defun my:keyhelp-filter-cycle-field ()
  "絞り込みの対象列を切り替える。ミニバッファの TAB。"
  (interactive)
  (let ((buf my:keyhelp--filter-buffer))
    (when (buffer-live-p buf)
      (let (label)
        (with-current-buffer buf
          (setq my:keyhelp--field
                (or (cadr (memq my:keyhelp--field my:keyhelp--fields))
                    (car my:keyhelp--fields)))
          (setq label (my:keyhelp--field-label))
          (my:keyhelp--refresh))
        ;; ミニバッファ使用中は `message' が echo area に出ない。
        ;; `minibuffer-message' なら入力の後ろに一時的に出せる。
        (minibuffer-message "[%s]" label)))))

(defun my:keyhelp-filter ()
  "行をインクリメンタルに絞り込む。
入力はミニバッファで読むが、結果は表の行が減る形で出る。既定では
Key / Command / Context / State の全体に当たり、TAB で
全体 -> Key -> Command -> Context -> State の順に対象列を絞れる
\(現在の対象列は `mode-name' に出る)。`C-g' で元に戻す。"
  (interactive)
  (unless (derived-mode-p 'my:keyhelp-mode)
    (user-error "このバッファでは使えません"))
  (let* ((buf (current-buffer))
         (old-query my:keyhelp--query)
         (old-field my:keyhelp--field)
         (map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; TAB は既定では補完。ここでは対象列の切り替えに使う。
    (define-key map (kbd "TAB") #'my:keyhelp-filter-cycle-field)
    (define-key map (kbd "<tab>") #'my:keyhelp-filter-cycle-field)
    (condition-case nil
        (minibuffer-with-setup-hook
            (lambda ()
              (setq-local my:keyhelp--filter-buffer buf)
              (add-hook 'after-change-functions #'my:keyhelp--filter-update nil t))
          (read-from-minibuffer
           (format "Filter [%s] (TAB:列): " (my:keyhelp--field-label old-field))
           old-query map)
          ;; 戻り値は使わない。1 文字ごとに適用済み。
          nil)
      (quit
       (with-current-buffer buf
         (setq my:keyhelp--query old-query
               my:keyhelp--field old-field)
         (my:keyhelp--refresh))
       (signal 'quit nil)))))

(defun my:keyhelp-clear-filter ()
  "絞り込みを解除する。"
  (interactive)
  (setq my:keyhelp--query "")
  (my:keyhelp--refresh))

(defun my:keyhelp-cycle-field ()
  "絞り込みの対象列を切り替える (絞り込み中でないとき)。"
  (interactive)
  (setq my:keyhelp--field
        (or (cadr (memq my:keyhelp--field my:keyhelp--fields))
            (car my:keyhelp--fields)))
  (my:keyhelp--refresh t)
  (message "絞り込みの対象: %s" (my:keyhelp--field-label)))

;;; --------------------------------------------------
;;; コマンド
;;; --------------------------------------------------

(defun my:keyhelp-toggle-shadowed ()
  "隠されて効かない束縛の表示を切り替える。"
  (interactive)
  (setq my:keyhelp--shadowed (not my:keyhelp--shadowed))
  (my:keyhelp--refresh t)
  (message "シャドウされた束縛: %s" (if my:keyhelp--shadowed "表示" "非表示")))

(defun my:keyhelp-toggle-local-only ()
  "グローバルマップを含めるかを切り替える。"
  (interactive)
  (setq my:keyhelp--local-only (not my:keyhelp--local-only))
  (my:keyhelp--reload)
  (my:keyhelp--refresh)
  (message "グローバルマップ: %s" (if my:keyhelp--local-only "除く" "含む")))

(defun my:keyhelp-describe ()
  "その行のコマンドの説明を出す。"
  (interactive)
  (let ((row (tabulated-list-get-id)))
    (unless row (user-error "行がありません"))
    (let ((def (nth 5 row)))
      (if (and (symbolp def) (fboundp def))
          (describe-function def)
        (message "%s" (nth 1 row))))))

(defun my:keyhelp-copy-key ()
  "その行のキーを kill-ring に入れる。"
  (interactive)
  (let ((row (tabulated-list-get-id)))
    (unless row (user-error "行がありません"))
    (kill-new (nth 0 row))
    (message "%s" (nth 0 row))))

(defvar-keymap my:keyhelp-mode-map
  :doc "`my:keyhelp-mode' のキーマップ。"
  "/"       #'my:keyhelp-filter
  "C-s"     #'my:keyhelp-filter
  "TAB"     #'my:keyhelp-cycle-field
  "<tab>"   #'my:keyhelp-cycle-field
  "DEL"     #'my:keyhelp-clear-filter
  "RET"     #'my:keyhelp-describe
  "t"       #'my:keyhelp-toggle-shadowed
  "L"       #'my:keyhelp-toggle-local-only
  "w"       #'my:keyhelp-copy-key)

(define-derived-mode my:keyhelp-mode tabulated-list-mode "KeyHelp"
  "有効なキーバインドを 1 枚の表で見るモード。

\\{my:keyhelp-mode-map}"
  ;; 行頭のマーク用の余白は要らない (このモードは行をマークしない)。
  (setq tabulated-list-padding 0)
  (setq-local truncate-lines t)
  ;; g (revert) で元のバッファから取り直す。
  (add-hook 'tabulated-list-revert-hook #'my:keyhelp--reload nil t))

;;;###autoload
(defun my:keyhelp (&optional local-only)
  "いまのバッファで有効なキーバインドを一覧表で表示する。
LOCAL-ONLY (`C-u') を付けるとグローバルマップを除き、メジャーモードと
マイナーモード固有の束縛だけを出す。"
  (interactive "P")
  (let ((src (current-buffer))
        (buf (get-buffer-create "*Key Bindings*")))
    (with-current-buffer buf
      ;; `define-derived-mode' は `kill-all-local-variables' を通るので、
      ;; バッファローカルの設定はモードを立てたあとに置く。
      (my:keyhelp-mode)
      (setq my:keyhelp--source src
            my:keyhelp--local-only (and local-only t)
            my:keyhelp--shadowed my:keyhelp-show-shadowed
            my:keyhelp--query ""
            my:keyhelp--field my:keyhelp-default-field)
      (my:keyhelp--reload))
    (pop-to-buffer buf)
    ;; 列幅は表示後に決め直す (上の `my:keyhelp--format' のコメントを参照)。
    (with-current-buffer buf
      (my:keyhelp--refresh))))

;; <f1> b を置き換える。C-h は `delete-backward-char' に割り当ててあるので、
;; ヘルプのプレフィクスは <f1> と help キー (どちらも `help-map')。
;; 元の `describe-bindings' は <f1> C-b に退避する。
(use-package emacs
  :bind
  (:map help-map
        ("b" . my:keyhelp)
        ("C-b" . describe-bindings)))

(provide 'my-keyhelp)
;;; my-keyhelp.el ends here
