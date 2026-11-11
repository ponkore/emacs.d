;;; my-embterm.el --- EmbTerm の端末を Emacs のウィンドウに重ねて動かす  -*- lexical-binding: t -*-
;;; Commentary:
;; EmbTerm (C:/Projects/EmbTerm) の Emacs 連携。Windows のみ。
;;
;; libghostty-vt で VT を解釈し、Direct2D で描いた子 HWND を Emacs の
;; ウィンドウに重ねる端末。バッファはプレースホルダで、描画もキー入力の
;; エンコードも module (embterm_module.dll) が持つ。PTY は同梱の再配布版
;; ConPTY (conpty.dll + x64/OpenConsole.exe)。
;;
;;   C-c s s   端末に切り替える (無ければ開く)        `embterm'
;;   C-c s n   新しい端末を開く                      `embterm-new'
;;   C-c s p   プロジェクトのルートで開く            `embterm-project'
;;   C-c s b   端末のバッファの一覧                  `embterm-list-buffers'
;;
;; **Ghostel の置き換え候補として並べて入れてある** (EmbTerm の TODO T-057)。
;; 評価の間、Ghostel は `C-c S' に退かせてある (`my-ghostel.el')。
;;
;; 配布物は EmbTerm のリポジトリで `cargo xtask install' を実行すると
;; `my:embterm-directory' に置かれる。embterm.el は同じディレクトリの
;; embterm_module.dll と conpty.dll を読む。初めて端末を開くときに読み込まれる
;; ので、ここでは autoload を張るだけにしてある (`my-genpreview.el' と同じ形)。
;;
;; 【重要】Emacs は module を unload できない。embterm.el は DLL を
;; 版付きの名前で一時ディレクトリにコピーしてから読むので、Emacs を
;; 動かしたまま install し直せるが、**新しい module に入れ替わるのは
;; Emacs を再起動したとき**。conpty.dll と OpenConsole.exe は配布物の場所の
;; ものを使い続けるので、install は使用中のものを `.old-<時刻>' に退避して
;; 置き換える (次の install で、使われなくなったものから消える)。
;;
;; use-package を使っていないのは `my-genpreview.el' と同じ理由
;; (`:load-path' が `:if' より前に処理され、Windows 以外でも
;; "C:/Apps/embterm/" が load-path に入ってしまう)。

;;; Code:

(eval-when-compile
  (require 'subr-x))

;; シェルの選択で my-shell.el の判定を使い回す。init.el では my-shell の
;; 後に require してあるので、この時点で両方とも定義済み。
(declare-function my:windows-pwsh "my-shell" ())
;; 下の autoload は配布物があるときだけ張るので、バイトコンパイラに教えておく
(declare-function embterm "embterm" (&optional arg))
(declare-function embterm-new "embterm" ())
(declare-function embterm-project "embterm" (&optional arg))
(declare-function embterm-list-buffers "embterm" ())
(defvar embterm-shell)

(defcustom my:embterm-directory "C:/Apps/embterm/"
  "EmbTerm の配布物を入れたディレクトリ。
embterm.el、embterm_module.dll、conpty.dll、x64/OpenConsole.exe を置く
\(EmbTerm のリポジトリの `cargo xtask install' の既定の置き場)。"
  :type 'directory)

(defun my:embterm-available-p ()
  "EmbTerm が使えるか。"
  (and (eq system-type 'windows-nt)
       module-file-suffix
       (file-exists-p (expand-file-name "embterm.el" my:embterm-directory))))

(defun my:embterm--default-shell ()
  "EmbTerm に起動させるシェル (コマンドと引数のリスト) を返す。

`M-x shell' と同じ pwsh に揃え、`my:pwsh-emacs-init-file' (UTF-8 固定・
starship・OSC 7) を読ませる。ConPTY のコンソールのコードページは cp932
なので、**本物のコンソールでも UTF-8 への固定は要る**。OSC 7 は
EmbTerm も受け取って `default-directory' を追う。

pwsh が無ければ nil (embterm.el の既定 = PATH の pwsh、無ければ cmd)。

`my-ghostel.el' の `my:ghostel--default-shell' と同じ中身。Ghostel を
外すとき (EmbTerm の TODO T-058) にあちらを消す。"
  (when-let* ((pwsh (and (fboundp 'my:windows-pwsh) (my:windows-pwsh))))
    ;; CreateProcess に渡すので区切りを \ にしておく
    (let ((pwsh (subst-char-in-string ?/ ?\\ pwsh))
          (init (bound-and-true-p my:pwsh-emacs-init-file)))
      (if (and init (file-exists-p init))
          (list pwsh "-NoLogo" "-NoExit" "-Command"
                (format ". '%s'" (subst-char-in-string ?/ ?\\ init)))
        (list pwsh "-NoLogo")))))

;;; --------------------------------------------------
;;; 有効化 (Windows で配布物があるときだけ)
;;; --------------------------------------------------

(when (my:embterm-available-p)
  (add-to-list 'load-path (directory-file-name my:embterm-directory))
  (autoload 'embterm "embterm" "端末に切り替える。無ければ開く。" t)
  (autoload 'embterm-new "embterm" "新しい端末を開く。" t)
  (autoload 'embterm-project "embterm" "プロジェクトのルートで端末を開く。" t)
  (autoload 'embterm-list-buffers "embterm" "端末のバッファの一覧。" t)
  ;; embterm.el はまだ読まれていないが、defcustom は既に値があれば上書きしない。
  ;; フォント (HackGen Console NF → Cascadia Mono → Consolas) と大きさ (12pt) は
  ;; module の既定のまま。EmbTerm は Emacs の fontset を通らずに DirectWrite で
  ;; 描くので、`my-appearance.el' の Nerd Font の調整はこちらには効かない (要らない)。
  (when-let* ((shell (my:embterm--default-shell)))
    (setq embterm-shell shell))
  (keymap-global-set "C-c s s" #'embterm)
  (keymap-global-set "C-c s n" #'embterm-new)
  (keymap-global-set "C-c s p" #'embterm-project)
  (keymap-global-set "C-c s b" #'embterm-list-buffers))

(provide 'my-embterm)
;;; my-embterm.el ends here
