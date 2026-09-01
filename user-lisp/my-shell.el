;;; my-shell.el --- Shell 関連  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'shell)
  (require 'ansi-osc))

;; いずれも shell-mode の中でしか呼ばないので、ロードは実行時に任せる。
(declare-function shell-dirtrack-mode "shell" (&optional arg))
(declare-function ansi-osc-apply-on-region "ansi-osc" (begin end))

(defun my:windows-bash ()
  "Windows で使う bash.exe のパスを返す。見つからなければ nil。
C:/Windows/System32/bash.exe は WSL の bash なので除外する
(Linux 側のファイルシステムで動くため Windows のパスを扱えない)。"
  (seq-find
   (lambda (path)
     (and path
          (file-executable-p path)
          (not (string-match-p "[/\\]system32[/\\]" (downcase path)))))
   (list
    ;; Git for Windows (git.exe の位置から辿る)
    (when-let* ((git (executable-find "git")))
      (expand-file-name "../usr/bin/bash.exe" (file-name-directory git)))
    "C:/Program Files/Git/usr/bin/bash.exe"
    "C:/Program Files (x86)/Git/usr/bin/bash.exe"
    ;; scoop
    (expand-file-name "scoop/shims/bash.exe" (or (getenv "USERPROFILE") "~"))
    ;; 最後に PATH から (System32 のものは上の述語で弾かれる)
    (executable-find "bash.exe"))))

(defun my:windows-pwsh ()
  "PowerShell 7 (pwsh.exe) のパスを返す。見つからなければ nil。
Windows 5.1 の powershell.exe ではなく 7 系のみを対象にする。"
  (seq-find
   (lambda (path) (and path (file-executable-p path)))
   (list
    ;; Microsoft Store 版が置くアプリ実行エイリアス。実体のパスは
    ;; バージョン番号を含んで更新のたびに変わるので、こちらを使う。
    (expand-file-name "AppData/Local/Microsoft/WindowsApps/pwsh.exe"
                      (or (getenv "USERPROFILE") "~"))
    ;; MSI 版
    "C:/Program Files/PowerShell/7/pwsh.exe"
    (executable-find "pwsh.exe"))))

(defvar my:pwsh-emacs-init-file
  (expand-file-name "etc/pwsh-emacs-shell.ps1" user-emacs-directory)
  "`M-x shell' の pwsh に読み込ませる初期化スクリプト。
UTF-8 固定・starship・OSC 7 の設定が入っている。")

;;; --------------------------------------------------
;;; Shell
;;; --------------------------------------------------

;;; [3] exec-path-from-shell

(use-package exec-path-from-shell
  :straight t
  ;; macOS 限定だったが、Linux でも GUI 起動時はログインシェルの
  ;; 環境変数を引き継がないため必要になる。
  :if (memq system-type '(darwin gnu/linux berkeley-unix))
  :config
  (exec-path-from-shell-initialize))

;;; [3] Windows環境用 Shell

(defun my:comint-osc-process-output (_string)
  "comint の出力に含まれる OSC シーケンスを解釈して表示から取り除く。
`ansi-color-process-output' の OSC 版。ansi-osc.el は compilation 用の
フィルタしか用意していないので comint 用は自前で定義する。

これがないとプロンプトが出すタイトル設定 (OSC 0/2) が
\"^[]0;~/foo^G\" のような生のエスケープシーケンスとして表示される。
OSC 7 は `ansi-osc-directory-tracker' がカレントディレクトリの通知として
解釈する。"
  (let ((start-marker (if (and (markerp comint-last-output-start)
                               (eq (marker-buffer comint-last-output-start)
                                   (current-buffer))
                               (marker-position comint-last-output-start))
                          comint-last-output-start
                        (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (ansi-osc-apply-on-region start-marker end-marker)))

;; shell.el の内部変数。起動したシェルのファイル名が入る。
(defvar shell--start-prog)

(defun my:windows-shell-setup ()
  "Windows の `shell-mode' バッファを設定する。
`shell-mode-hook' が走る時点でプロセスは既に生成されている
\(shell.el は make-comint-in-buffer の後で shell-mode を呼ぶ)。"
  (require 'ansi-osc)
  (add-hook 'comint-output-filter-functions
            #'my:comint-osc-process-output nil t)
  ;; pwsh 側も [Console]::{Input,Output}Encoding を UTF-8 にしてある
  ;; (etc/pwsh-emacs-shell.ps1)。Git bash も UTF-8 なので分岐しない。
  ;; 出力の改行は CRLF なので decode 側だけ -dos にする。
  (set-process-coding-system (get-buffer-process (current-buffer))
                             'utf-8-dos 'utf-8-unix)
  (when (string-match-p "pwsh" (or shell--start-prog ""))
    ;; Emacs は Windows では擬似端末を使えずパイプでシェルを起動する。
    ;; PowerShell はリダイレクトされた stdin から読んだ行をそのまま
    ;; echo し返すので、comint 側で重複表示を消す。
    (setq-local comint-process-echoes t)
    ;; cd の追跡は OSC 7 に任せる。shell-dirtrack-mode は
    ;; bash の "cd foo; pushd bar" のような構文を前提にしていて
    ;; PowerShell では誤検出するだけなので止める。
    (shell-dirtrack-mode -1)))

;; 疑似パッケージなので use-package の名前は emacs にする。
;; 実在しない feature 名にすると :config が with-eval-after-load に包まれて
;; 永久に実行されない (leaf で :leaf-defer nil を付けていたのと同じ理由)。
(use-package emacs
  :if (eq system-type 'windows-nt)
  :hook (shell-mode-hook . my:windows-shell-setup)
  :config
  ;; ユーザー名を直書きしていたので USERPROFILE 基準にする
  (let ((shims (expand-file-name "scoop/shims" (getenv "USERPROFILE"))))
    (when (file-directory-p shims)
      (add-to-list 'exec-path shims)))
  (require 'shell)
  ;; shell-file-name は M-! / M-| / M-x compile / M-x grep が使う非対話シェル。
  ;; これらは POSIX シェルに "-c 'コマンド行'" を渡す前提で組み立てられるので
  ;; bash のままにする (見つからない場合は Emacs 既定の cmdproxy)。
  ;;
  ;; 注意: (executable-find "bash.exe") をそのまま使うと
  ;; C:/Windows/System32/bash.exe すなわち WSL の bash を拾ってしまう。
  ;; WSL の bash は Linux 側のファイルシステムで動くため、Windows の
  ;; 絶対パスを渡すコマンド (M-x grep など) が
  ;; "No such file or directory" で失敗する。System32 のものは除外する。
  (when-let* ((bash (my:windows-bash)))
    (setq shell-command-switch "-c")
    (setq shell-file-name bash))
  ;; explicit-shell-file-name は M-x shell の対話シェル。こちらは pwsh にする。
  ;;
  ;; nyagos は採用できなかった。Emacs は Windows では擬似端末を持てず
  ;; パイプでシェルを起動するが、nyagos はコンソールが無いと行編集を諦めて
  ;; プロンプトを一切出力しない (バッチ実行になる)。comint はプロンプトで
  ;; 入力と出力を区切るので、これでは M-x shell として成立しない。
  ;; 補完・履歴予測といった nyagos の利点もコンソール前提で全て無効になる。
  (when-let* ((pwsh (my:windows-pwsh)))
    (setq explicit-shell-file-name pwsh)
    ;; shell.el は (concat "explicit-" (file-name-nondirectory prog) "-args")
    ;; を intern-soft で引く。つまり変数名には .exe が付く。
    (set (intern (concat "explicit-" (file-name-nondirectory pwsh) "-args"))
         (list "-NoLogo" "-NoExit" "-Command"
               (format ". '%s'" my:pwsh-emacs-init-file))))
  ;; (M-! and M-| and compile.el)
  (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
  ;; エスケープシーケンス処理の設定
  (autoload 'ansi-color-for-comint-mode-on "ansi-color"
    "Set `ansi-color-for-comint-mode' to t." t))

;;; [3] Shell

;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
;; プロンプトに comint-highlight-prompt の色が乗るとシェル側の色が潰れる。
;; 無名関数にすると my-shell.el を再読み込みするたびにフックへ溜まるので
;; 名前付きにしておく。
(defun my:shell-unhighlight-prompt ()
  "プロンプトへの `comint-highlight-prompt' の適用をやめる。"
  (face-remap-set-base 'comint-highlight-prompt :inherit nil))

(use-package shell
  :hook (shell-mode-hook . my:shell-unhighlight-prompt)
  ;; or shellモードの時の^M抑制 (どっちが正しい？)
  ;; (comint-output-filter-functions . shell-strip-ctrl-m nil t)
  :config
  ;; shell-modeでの補完 (for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-"))

(provide 'my-shell)
;;; my-shell.el ends here
