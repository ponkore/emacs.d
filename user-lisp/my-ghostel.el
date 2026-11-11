;;; my-ghostel.el --- libghostty の端末を Emacs の中で動かす  -*- lexical-binding: t -*-
;;; Commentary:
;; Ghostel は Ghostty のコア (libghostty-vt) を Emacs の dynamic module に
;; 載せた端末エミュレータ。VT の解釈と描画をネイティブ側が持ち、PTY も
;; module が自分で開く (Windows は ConPTY、POSIX は posix_openpt + fork)。
;;
;;     Emacs ──module 呼び出し──> ghostel-module.dll ──ConPTY──> シェル
;;                                (libghostty-vt + 専用 Renderer)
;;
;; `my-pty' との住み分け。**どちらを採るかは評価中なので両方残してある。**
;;
;;   my-pty  … ptyd.exe (Go) が ConPTY を持ち、VT は eat (Emacs Lisp)。
;;             プロセス 2 つ。Windows 専用
;;   ghostel … module 1 つで完結。VT は C/Zig。Kitty graphics と
;;             Kitty keyboard protocol を持つので yazi の画像まで出る。
;;             Windows / macOS / Linux / FreeBSD で動く
;;
;; **module が無ければ何もしない。** 初回の `M-x ghostel' が入手方法を
;; 尋ねるだけで、他の設定には影響しない。
;;
;; 導入の経緯と実測は docs/pty/ghostel.md。
;;; Code:

(eval-when-compile
  (require 'subr-x))

;; シェルの選択で my-shell.el の判定を使い回す。init.el では my-shell の
;; 後に require してあるので、この時点で両方とも定義済み。
(declare-function my:windows-pwsh "my-shell" ())

(defun my:ghostel--default-shell ()
  "Ghostel に起動させるシェルを返す。

Ghostel の既定値は `(getenv \"SHELL\")' だが、**Windows の Emacs では
これが cmdproxy.exe になる** (実測)。cmdproxy は Emacs がパイプ経由で
非対話シェルを起こすための代理プロセスで、端末に置くものではない。

`M-x shell' と同じ pwsh に揃える (`my-shell.el' の
`explicit-shell-file-name')。UTF-8 固定・starship・OSC 7 の設定が入った
`my:pwsh-emacs-init-file' も読ませる。ConPTY のコンソールのコードページは
cp932 なので、**実コンソールでも UTF-8 への固定は要る**。"
  (cond
   ((not (eq system-type 'windows-nt))
    (or (let ((process-environment (default-value 'process-environment)))
          (getenv "SHELL"))
        "/bin/sh"))
   ((when-let* ((pwsh (and (fboundp 'my:windows-pwsh) (my:windows-pwsh))))
      (let ((init (bound-and-true-p my:pwsh-emacs-init-file)))
        (if (and init (file-exists-p init))
            (list pwsh "-NoLogo" "-NoExit" "-Command" (format ". '%s'" init))
          (list pwsh "-NoLogo")))))
   (t (or (getenv "COMSPEC") "cmd.exe"))))

;;; --------------------------------------------------
;;; 端末のフォント
;;; --------------------------------------------------

;; 【重要】セルに収まらないグリフは Ghostel に縮められる。
;;
;; ネイティブ側はセルより広いグリフに `display' の `height' を付けて縮める
;; (`ghostel-glyph-scale-floor' が下限。既定 0.0 = 下限なし)。本文の
;; HackGen は罫線 ─ を全角 (16 px) で持つので、セル 8 px に対して
;; 16/8 の縮小がかかり **TUI の枠線が潰れる**。アイコンも同じ理由で
;; 潰れるが、そちらは fontset の話なので `my-appearance.el' の
;; 「コンソール幅の Nerd Font」に置いてある。
;;
;; ここで直すのは face で間に合う範囲 (罫線や記号)。`ghostel-default' は
;; docstring が「Customize this to give ghostel buffers a different … font」
;; と言っているとおり、そのための face。**Emacs 全体のフォントは変えない**
;; (`my:pty-console-font' を既定 nil にしてあるのと同じ理由)。
;;
;; 実測 (pixel size 15 / セル 8x17):
;;
;;   family                  ascent/descent   a   あ   ─   powerline
;;   HackGen                     14/3         8   16  16      8
;;   HackGen Console NF          14/3         8   16   8      8
;;
;; `HackGen Console NF' は本文フォントとして HackGen と等価 (ascent も
;; 全角幅も同じ) で、**罫線だけが端末向けの半角**になる。
;; 罫線が 8 px なのは TUI にとっては正しい (conhost と同じ扱い)。

;; フォントの選定は my-appearance.el に置いてある (同じことを 2 か所に
;; 書かない)。init.el では my-appearance のほうが先に読まれる。
(declare-function my:console-nerd-font-family "my-appearance" (body-font))

;;; --------------------------------------------------
;;; Ghostel
;;; --------------------------------------------------

(use-package ghostel
  ;; dynamic module が無いビルドでは何もできないので clone もしない
  ;; (init.el が :straight を :if より後に処理させてある)。
  :if module-file-suffix
  :straight t
;; `C-c s' は EmbTerm (`my-embterm.el') に譲った。比べる間はこちらを `C-c S' に置く
  ;; (EmbTerm の TODO T-057。Ghostel を外すかは T-058 で決める)。
  :bind (("C-c S s" . ghostel)
         ("C-c S n" . ghostel-new)
         ("C-c S p" . ghostel-project)
         ("C-c S b" . ghostel-list-buffers))
  :custom
  ;; 【重要】module の置き場を straight の build ツリーの外に出す。
  ;; 既定 (nil) ではパッケージディレクトリ = straight/build/ghostel/ に
  ;; 置かれるが、`straight-rebuild-package' や `straight-prune-build' は
  ;; あのディレクトリを作り直すので、**Emacs がロード中の .dll が消える**。
  ;; upstream の docstring も「パッケージマネージャのツリーの外に置け」と
  ;; 言っている。ここは .gitignore に入れてある。
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  ;; Windows の既定値が cmdproxy.exe になる問題。上の関数を参照。
  (ghostel-shell (my:ghostel--default-shell))
  :config
  ;; 罫線などが縮むのを防ぐ。上の「端末のフォント」を参照。
  ;; `:custom-face' は使わない (テーマに負ける。CLAUDE.md)。
  ;; 見つからなければ何もしない = 既定の見た目のまま。
  (when-let* ((family (my:console-nerd-font-family
                       (face-attribute 'default :family))))
    (set-face-attribute 'ghostel-default nil :family family)))

;; 入れていない統合（どれも既定で off。必要になったら足す）:
;;
;;   ghostel-compile-global-mode       `compile' を端末で走らせる
;;   ghostel-shell-command-global-mode `shell-command' を端末で走らせる
;;   ghostel-comint-global-mode        comint のバッファを端末にする
;;   ghostel-status-indicator-mode     モードラインに状態を出す
;;
;; 前 3 つは既存の `C-c c' / `C-x !' / SQL の comint を黙って置き換えるので
;; 評価が済むまで有効にしない。最後のものは doom-modeline と重なる。
;;
;; `ghostel-ime-mode' は Emacs Lisp の input method (quail / hangul) 用で、
;; Windows の IME (tr-ime / w32-ime) とは無関係。こちらには要らない。

(provide 'my-ghostel)
;;; my-ghostel.el ends here
