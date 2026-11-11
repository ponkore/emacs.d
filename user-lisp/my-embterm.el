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
;;   M-x nyagos / pwsh / yazi   カレントディレクトリで開く (`*nyagos(dir)*' など)
;;   M-x claude                 プロジェクトで claude を開く (`*claude-term(proj)*')
;;                              C-c a a (stream-json) と並べて評価している
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
(declare-function embterm-run "embterm" (command &rest keys))
(defvar embterm-shell)
;; M-x claude は my-claude.el のアカウントの選択と環境の組み立てを使い回す
(declare-function my:claude--project-directory "my-claude" ())
(declare-function my:claude--read-environment "my-claude" ())
(declare-function my:claude--config-dir "my-claude" (env))
(declare-function my:claude--process-environment "my-claude" (config-dir))
(defvar my:claude-executable)
(defvar my:claude-model)

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
;;; アプリを端末で開くコマンド (M-x nyagos / pwsh / yazi / claude)
;;; --------------------------------------------------

;; どれも `embterm-run' で開き、バッファ名は `*アプリ(ディレクトリ名)*'。
;; 同じ名前の端末が同じディレクトリで動いていればそこへ切り替え、
;; C-u を付けると必ず新しく開く (名前に `<2>' などが付く)。
;; プロセスが終わるとバッファは消える (`embterm-kill-buffer-on-exit')。

(defvar-local my:embterm--start-directory nil
  "`my:embterm--run' で開いた端末の、起動したディレクトリ。
`default-directory' は OSC 7 で cd に追従して変わるので、同じ端末かどうかの
判定には使えない。")

(defun my:embterm--directory-label (dir)
  "DIR のバッファ名用の名前 (最後の要素。ドライブのルートならそのまま)。"
  (let* ((path (directory-file-name (expand-file-name dir)))
         (name (file-name-nondirectory path)))
    (if (string-empty-p name) path name)))

(defun my:embterm--run (app command dir new &rest keys)
  "APP の端末で COMMAND を DIR で動かす。バッファを返す。
バッファ名は `*APP(DIR の名前)*'。NEW が nil で、同じ名前の端末が DIR で
動いていればそこへ切り替える。KEYS は `embterm-run' に渡す。"
  (unless (fboundp 'embterm-run)
    (user-error "EmbTerm がありません (%s)" my:embterm-directory))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (name (format "*%s(%s)*" app (my:embterm--directory-label dir)))
         (existing (get-buffer name)))
    (if (and existing (not new)
             (equal (buffer-local-value 'my:embterm--start-directory existing) dir))
        (progn (pop-to-buffer-same-window existing) existing)
      (let ((buf (apply #'embterm-run command :directory dir :name name keys)))
        (with-current-buffer buf
          (setq my:embterm--start-directory dir))
        buf))))

(defun my:embterm--executable (program)
  "PROGRAM の実行ファイルのパス。見つからなければ `user-error'。"
  (or (executable-find program)
      (user-error "%s が見つかりません (PATH を確かめてください)" program)))

(defun nyagos (&optional new)
  "`default-directory' で nyagos を EmbTerm の端末に開く。
バッファ名は `*nyagos(ディレクトリ名)*'。NEW (前置引数) なら必ず新しく開く。"
  (interactive "P")
  (my:embterm--run "nyagos" (list (my:embterm--executable "nyagos")) default-directory new))

(defun pwsh (&optional new)
  "`default-directory' で pwsh を EmbTerm の端末に開く。
`M-x shell' と同じ初期化スクリプトを読ませる (`my:embterm--default-shell')。
バッファ名は `*pwsh(ディレクトリ名)*'。NEW (前置引数) なら必ず新しく開く。"
  (interactive "P")
  (my:embterm--run "pwsh"
                   (or (my:embterm--default-shell)
                       (list (my:embterm--executable "pwsh") "-NoLogo"))
                   default-directory new))

(defun yazi (&optional new)
  "`default-directory' で yazi を EmbTerm の端末に開く。
バッファ名は `*yazi(ディレクトリ名)*'。NEW (前置引数) なら必ず新しく開く。"
  (interactive "P")
  (my:embterm--run "yazi" (list (my:embterm--executable "yazi")) default-directory new))

(defun claude (&optional new)
  "プロジェクトのディレクトリで claude の対話 TUI を EmbTerm の端末に開く。

ディレクトリは `C-c a a' (`my:claude') と同じ決め方 (projectile のルート。
決まらなければ確認する)。アカウント (`my:claude-environments') も
`C-c a a' と同じく選ばせ、CLAUDE_CONFIG_DIR を差し替えた環境で起動する。
ヘッダ行に環境とディレクトリを出す。

バッファ名は `*claude-term(プロジェクト名)*'。`*claude(…)*' にしないのは、
`C-c a a' の会話バッファと同じ名前になり、`my:claude--buffer-for' が
こちらのバッファを会話の書き込み先に使ってしまうため。
NEW (前置引数) なら必ず新しく開く。"
  (interactive "P")
  (require 'my-claude)
  (let* ((dir (file-name-as-directory (my:claude--project-directory)))
         (name (format "*claude-term(%s)*" (my:embterm--directory-label dir)))
         (existing (get-buffer name)))
    (if (and existing (not new)
             (equal (buffer-local-value 'my:embterm--start-directory existing)
                    (file-name-as-directory (expand-file-name dir))))
        (progn (pop-to-buffer-same-window existing) existing)
      (let* ((env (my:claude--read-environment))
             (buf (my:embterm--run
                   "claude-term"
                   (append (list my:claude-executable)
                           (when my:claude-model (list "--model" my:claude-model)))
                   dir t
                   ;; CLAUDE_CONFIG_DIR は「設定しない」ではなく「消す」必要がある
                   ;; (Emacs が継承していると別のアカウントに繋がる。my-claude.el と同じ)
                   :environment (my:claude--process-environment (my:claude--config-dir env)))))
        (with-current-buffer buf
          (setq header-line-format
                (format " claude: %s | %s" env (abbreviate-file-name (directory-file-name dir)))))
        buf))))

;;; --------------------------------------------------
;;; 有効化 (Windows で配布物があるときだけ)
;;; --------------------------------------------------

(when (my:embterm-available-p)
  (add-to-list 'load-path (directory-file-name my:embterm-directory))
  (autoload 'embterm "embterm" "端末に切り替える。無ければ開く。" t)
  (autoload 'embterm-new "embterm" "新しい端末を開く。" t)
  (autoload 'embterm-project "embterm" "プロジェクトのルートで端末を開く。" t)
  (autoload 'embterm-list-buffers "embterm" "端末のバッファの一覧。" t)
  (autoload 'embterm-run "embterm" "コマンドを端末で起動する。")
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
