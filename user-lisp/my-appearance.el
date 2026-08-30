;;; my-appearance.el --- フォント・フレーム・テーマ・モードライン  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; --------------------------------------------------
;;; フォント設定
;;; --------------------------------------------------

;;; [3] Nerd Font の選択

;; nerd-icons は Nerd Fonts v3 のコードポイント割り当てを前提にしている。
;; とくに Material Design アイコン (mdicon) は第 15 面 U+F0001〜U+F1AF0 にあり、
;; v2 世代のパッチ済みフォント (HackGenNerd / HackGen35Nerd など) はここを
;; 持っていないため、名前だけで選ぶと dired などのアイコンが軒並み豆腐になる。
;; そこで実際にグリフを持っているかを見て選ぶ。

(defvar my:nerd-font-family--cache 'unset
  "Nerd Font のファミリ名。未判定なら 'unset、見つからなければ nil。")

(defun my:nerd-font-family ()
  "Nerd Fonts v3 のグリフを実際に持っているフォントファミリを返す。"
  (when (eq my:nerd-font-family--cache 'unset)
    (setq my:nerd-font-family--cache
          (and (display-graphic-p)
               (seq-find
                (lambda (family)
                  (condition-case nil
                      (let* ((entity (find-font (font-spec :family family)))
                             (font (and entity (open-font entity))))
                        (and font
                             ;; mdicon (第 15 面) と sucicon の v3 拡張分で
                             ;; v2/v3 を判別する
                             (seq-every-p
                              (lambda (c)
                                (let ((glyphs (font-get-glyphs font 0 1 (vector c))))
                                  (and (vectorp glyphs) (aref glyphs 0))))
                              '(#xf0001 #xe6ad))))
                    (error nil)))
                '("Symbols Nerd Font Mono" "Symbols Nerd Font"
                  "HackGen Console NF" "HackGen35 Console NF"
                  "HackGenNerd" "HackGen35Nerd")))))
  my:nerd-font-family--cache)

;;; [3] フォント設定

(leaf *font-setting
  :if window-system
  ;; 以前は :after all-the-icons だったが、フォント設定自体はアイコン
  ;; パッケージに依存しない。:after を付けると leaf が :config を
  ;; (eval-after-load 'nerd-icons ...) で包むため、nerd-icons が
  ;; require されない構成では設定が永久に適用されず、既定の
  ;; Courier New のままになっていた。
  :config
  (defun emacs-font-setting (font-name size)
    "Set emacs japanese fonts."
    ;; Note:
    ;; https://qiita.com/melito/items/238bdf72237290bc6e42
    ;; [NG] noto mono だと全角文字が半角の２倍幅になっていない
    ;; (set-frame-font "noto mono-10")
    ;; [△] Consolas & Meiryoke_Console だと丸付き数字(①等)が半角幅になってしまっている
    ;; [△] Inconsolata & Meiryoke_Console だと全角○が半角幅になってしまっている
    ;; [△] Meiryoke_Console 統一だと文字幅問題はないが、行高さが詰まりすぎ、O0liの区別がつきにくい
    ;;あいうえお あいうえお あいうえお あいうえお あいうえお あいうえお ◎●○①㈱
    ;;abcdefghij klmnopqrst uvwxyzABCD EFGHIJKLMN OPQRSTUVWX YZilO0     1234567890
    ;;
    ;; JIS第２水準：Ricty / HackGenNerd は〇、Ricty Diminished は×
    ;; Italic: Ricty Diminished / PlemolJP は〇、Ricty / HackGenNerd は×
    ;;    ただし、Ricty Diminished で×は半角になってしまう
    ;; HackGenNerd の Nerd フォントは、一部漢字コードに割当たっている
    (let* ((asciifont font-name)
           (jpfont font-name)
           (h (round (* size 10)))
           (ascii-fontspec (font-spec :family asciifont))
           (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      ;; Japanese
      (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      ;; Latin with pronounciation annotations
      (set-fontset-font nil '(#x0080 . #x024F) ascii-fontspec)
      ;; Math symbols
      (set-fontset-font nil '(#x2200 . #x22FF) ascii-fontspec)
      ;; Greek
      (set-fontset-font nil '(#x0370 . #x03FF) ascii-fontspec)
      ;; アイコン類 (Nerd Font の私用領域)
      ;; 本文は HackGen のまま、アイコンの範囲だけ Nerd グリフを持つ
      ;; フォントに回す。以前はここを ascii-fontspec (= HackGen) に
      ;; 割り当てていて豆腐になっていた。
      (let ((nerd (my:nerd-font-family)))
        (when nerd
          (let ((spec (font-spec :family nerd)))
            ;; BMP の私用領域。Powerline / Font Awesome / Devicons /
            ;; Codicons / Octicons / Seti などはすべてこの中にある。
            (set-fontset-font nil '(#xe000 . #xf8ff) spec)
            ;; Material Design アイコンは第 15 面 (Nerd Fonts v3)
            (set-fontset-font nil '(#xf0000 . #xfffff) spec))))
      (when (eq window-system 'ns)
        (set-fontset-font t '(#x1f300 . #x1f9ff) "Apple Color Emoji" nil 'append)
        (set-fontset-font t '(#x1fa70 . #x1fbff) "Apple Color Emoji" nil 'append)
        (set-fontset-font t '(#x1f900 . #x1f9e0) "Apple Color Emoji" nil 'append))
      (when (eq window-system 'w32)
        (set-fontset-font t '(#x1f300 . #x1f9ff) "Segoe UI Emoji" nil 'append)
        (set-fontset-font t '(#x1fa70 . #x1fbff) "Segoe UI Emoji" nil 'append)
        (set-fontset-font t '(#x1f900 . #x1f9e0) "Segoe UI Emoji" nil 'append))
      (setq face-font-rescale-alist `((,font-name . 1.0)))))

  (defun setup-font ()
    (interactive)
    ;; 以前は ns / w32 しか分岐が無く、Linux (x / pgtk) では
    ;; フォントが一切設定されなかった。
    (pcase window-system
      ('ns  (emacs-font-setting "HackGen" 16))   ;; previous: "Ricty"
      ('w32 (emacs-font-setting "HackGen" 12))   ;; previous: ("HackGenNerd" 11)
      ((or 'x 'pgtk)
       ;; Linux では入っているものを順に探す
       (let ((family (seq-find (lambda (f) (member f (font-family-list)))
                               '("HackGen" "HackGenNerd" "Ricty"
                                 "Noto Sans Mono CJK JP" "DejaVu Sans Mono"))))
         (when family (emacs-font-setting family 12))))))
  (setup-font))

;;; [3] text-scale

(leaf text-scale
  :hydra (hydra-zoom ()
                     "Zoom"
                     ("g" text-scale-increase "in")
                     ("l" text-scale-decrease "out")
                     ("r" (text-scale-set 0) "reset")
                     ("0" (text-scale-set 0) :bind nil :exit t))
  :bind ("<f2>" . hydra-zoom/body))

;;; [3] nerd-icons

;; 以前は all-the-icons を使っていたが、事実上メンテナンスが止まっており
;; doom-modeline も 4.x で nerd-icons 前提になったため移行した。
;; nerd-icons は Nerd Font 1 本 (HackGenNerd など) で全アイコンをまかなえるので、
;; all-the-icons のように 6 種類のフォントを個別に入れる必要がない。

(leaf nerd-icons
  :straight t
  ;; nerd-icons-dired / -ibuffer / -completion が :after nerd-icons で
  ;; ぶら下がっているため、ここで必ずロードしておく。
  ;; そうしないと feature が読まれず、それらが一切有効にならない。
  :require t
  :config
  ;; 使うフォントは my:nerd-font-family がグリフの有無を見て決める。
  ;; HackGenNerd は Nerd Fonts v2 世代で mdicon (第 15 面) を持たないため
  ;; 名前で決め打ちすると dired などのアイコンが豆腐になる。
  (when-let* ((family (my:nerd-font-family)))
    (setq nerd-icons-font-family family)))

;;; [4] nerd-icons-dired

(leaf nerd-icons-dired
  :straight t
  :after nerd-icons
  :hook (dired-mode-hook . nerd-icons-dired-mode))

;;; [4] nerd-icons-ibuffer

(leaf nerd-icons-ibuffer
  :straight t
  :after nerd-icons
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  :bind (("C-x C-b" . ibuffer)))

;;; [4] nerd-icons-completion

;; vertico / marginalia の候補にアイコンを出す
(leaf nerd-icons-completion
  :straight t
  :after nerd-icons marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; [3] Mac用

;; 注意: default-frame-alist は early-init.el でも設定している
;; (ツールバー等の非表示)。setq で丸ごと上書きするとそれが消えるため、
;; 以下ではいずれも append で既存の値を残している。

(leaf frame-setting-mac
  :if (eq system-type 'darwin)
  :config
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t) ;; タイトルバーを透過
           (vertical-scroll-bars . nil) ;; スクロールバーを消す
           ;; (ns-appearance . dark) ;; 26.1 {light, dark}
           (internal-border-width . 0)
           (top . 0)
           (width . 180)
           (height . 83))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] Windows用

(leaf frame-setting-windows
  :if (eq system-type 'windows-nt)
  :config
  (setq initial-frame-alist
        (append
         ;; ns-transparent-titlebar は macOS 専用パラメータなので削除した
         '((vertical-scroll-bars . nil) ;; スクロールバーを消す
           (internal-border-width . 0)
           ;; position
           (top . 40)
           (left . 670)
           (width . 136)
           (height . 50))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] Linux用

(leaf frame-setting-linux
  ;; これまで Linux の分岐が無く default-frame-alist が未設定だった
  :if (memq system-type '(gnu/linux berkeley-unix))
  :config
  (setq initial-frame-alist
        (append
         '((vertical-scroll-bars . nil)
           (internal-border-width . 0)
           (top . 0)
           (left . 0)
           (width . 160)
           (height . 50))
         initial-frame-alist))
  (setq default-frame-alist (append initial-frame-alist default-frame-alist)))

;;; [3] 共通

(leaf frame-setting-common
  :config
  ;; フレームタイトルの設定
  (setq frame-title-format "%b")
  ;; 背景の透明度
  (set-frame-parameter nil 'alpha 85)
  ;; scroll bar を表示しない
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
  ;; 行番号のface
  ;; TODO: `linum` という face が Emacs29? からなくなった
  ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
  ;; (set-face-attribute 'linum nil :height 0.8)
  )

;;; [3] テーマ

;; https://zenn.dev/lambdagonbei/articles/1b2bce27673078
(leaf modus-themes
  :straight t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; (leaf color-theme-sanityinc-tomorrow
;;   :straight t
;;   :config
;;   ;; (load-theme 'pastels-on-dark t)
;;   ;; (enable-theme 'pastels-on-dark)
;;   (color-theme-sanityinc-tomorrow-blue))

;;; --------------------------------------------------
;;; モードライン
;;; --------------------------------------------------

;;; [3] diminish

(leaf diminish :straight t)

;;; [3] doom-modeline

(leaf doom-modeline
  :straight t
  :if window-system
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style . 'truncate-with-project)
  (doom-modeline-icon . t)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-minor-modes . t)
  (doom-modeline-buffer-encoding . t)
  `(doom-modeline-icon . ,(display-graphic-p))
  :custom-face
  (mode-line                       . '((t (:background "medium blue" :foreground "snow" :box nil)))) ;; firebrick3
  (doom-modeline-buffer-minor-mode . '((t (:inherit mode-line :slant normal))))
  :hook (emacs-startup-hook . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (which-function-mode 0)
  ;;
  (doom-modeline-def-segment my:buffer-encoding
    "Displays the encoding and eol style of the buffer."
    (when doom-modeline-buffer-encoding
      (propertize
       (concat
        (let ((sys (coding-system-plist buffer-file-coding-system)))
          (cond ((memq (plist-get sys :category)
                       '(coding-category-undecided coding-category-utf-8))
                 " U")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided japanese-iso-8bit))
                 " E")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided iso-2022-jp))
                 " J")
                ((memq (plist-get sys :name)
                       '(coding-category-undecided japanese-shift-jis japanese-cp932))
                 " S")
                (t " =")))
        (pcase (coding-system-eol-type buffer-file-coding-system)
          (0 "")
          (1 ".CRLF")
          (2 ".CR")))
       'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
       'help-echo 'mode-line-mule-info-help-echo
       'mouse-face '(:box 0)
       'local-map mode-line-coding-system-map)))
  ;;
  (doom-modeline-def-modeline
    'main
    ;; '(workspace-number bar window-number evil-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(bar my:buffer-encoding matches buffer-info buffer-position selection-info major-mode vcs)
    '(misc-info debug minor-modes "-" input-method process checker)))

;;; [4] doom-modeline の eglot セグメントの修正

;; doom-modeline 4.3.0 (upstream 6f911e9, 2026-08-28) の eglot セグメントは
;; Emacs 31.1 で無くなった内部関数を 3 つ呼んでいる:
;;   jsonrpc--request-continuations -> jsonrpc-continuation-count に置換
;;   eglot--spinner                 -> 廃止
;;   eglot--major-mode              -> eglot--major-modes (リストになった)
;;
;; これは表示の乱れでは済まない。doom-modeline は eglot--managed-mode-hook に
;; ぶら下がっており、eglot--maybe-activate-editing-mode は
;;   (eglot--managed-mode)                  ; ここで上記フックが走る
;;   (eglot--signal-textDocument/didOpen)
;;   (eglot-inlay-hints-mode 1) ...
;; の順に呼ぶ。1 つ目で void-function が投げられると 2 つ目以降が実行されず、
;; textDocument/didOpen が送られない。接続だけ成立してサーバはバッファを
;; 知らないままなので、診断も補完も一切出ない状態になる。
;;
;; upstream が直したらこのブロックごと削除してよい。

(defun my:doom-modeline-update-eglot ()
  "`doom-modeline-update-eglot' を現行の eglot / jsonrpc API で書き直したもの。"
  (setq doom-modeline--eglot
        (let* ((server (eglot-current-server))
               (nick (and server (eglot--project-nickname server)))
               (pending (and server (jsonrpc-continuation-count server)))
               (busy (and pending (> pending 0)))
               (last-error (and server (jsonrpc-last-error server)))
               (face (cond (last-error 'doom-modeline-lsp-error)
                           (busy 'doom-modeline-lsp-warning)
                           (nick 'doom-modeline-lsp-success)
                           (t 'doom-modeline-lsp-warning))))
          (propertize
           (doom-modeline-lsp-icon "EGLOT" face)
           'help-echo
           (cond
            (last-error
             (format "EGLOT\nAn error occured: %s\nmouse-3: Clear this status"
                     (plist-get last-error :message)))
            (busy (format "EGLOT\n%d outstanding requests" pending))
            (nick
             (format (concat "EGLOT Connected (%s/%s)\n"
                             "C-mouse-1: Go to server errors\n"
                             "mouse-1: Go to server events\n"
                             "mouse-2: Quit server\n"
                             "mouse-3: Reconnect to server")
                     nick (eglot--major-modes server)))
            (t "EGLOT Disconnected\nmouse-1: Start server"))
           'mouse-face 'mode-line-highlight
           'local-map
           (let ((map (make-sparse-keymap)))
             (cond
              (last-error
               (define-key map [mode-line mouse-3] #'eglot-clear-status))
              (busy
               (define-key map [mode-line mouse-3] #'eglot-forget-pending-continuations))
              (nick
               (define-key map [mode-line C-mouse-1] #'eglot-stderr-buffer)
               (define-key map [mode-line mouse-1] #'eglot-events-buffer)
               (define-key map [mode-line mouse-2] #'eglot-shutdown)
               (define-key map [mode-line mouse-3] #'eglot-reconnect))
              (t (define-key map [mode-line mouse-1] #'eglot)))
             map)))))

(with-eval-after-load 'doom-modeline-segments
  (unless (fboundp 'jsonrpc--request-continuations)
    (advice-add 'doom-modeline-update-eglot
                :override #'my:doom-modeline-update-eglot)))

(provide 'my-appearance)
;;; my-appearance.el ends here
