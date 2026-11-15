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

;; GUI から起動した Emacs は launchd (mac) / デスクトップ環境 (Linux) が渡す
;; 最小限の PATH しか持たないので、ログインシェルから引き直す必要がある。
;; 問題はその値段で、対話シェル (-i) を起こすと zim / compinit / nvm /
;; starship / zoxide が全部走る。実測 (mac, 2026-11-16):
;;
;;   zsh -l       13 ms   .zshrc を読まないので PATH も SSH_AUTH_SOCK も空
;;   zsh -l -i   650 ms   必要な値は揃う (うち nvm_auto だけで 282 ms)
;;
;; Emacs の中では 1051 ms かかっていて、exec-path-from-shell 自身が
;; 「execution took 1051ms」と警告を出していた。
;;
;; 欲しいのは 3 つの文字列だけで、しかもほとんど変わらない。そこで
;;
;;   起動時   : キャッシュを当てる (子プロセス 0 本、ファイル 1 つを読むだけ)
;;   起動直後 : 裏でシェルを 1 回起こし、違っていればその場で直してキャッシュを更新
;;
;; という形にした。シェルは起動のたびに必ず起こすので「キャッシュだから古い」
;; 状態は起動から 1 秒ほどで閉じる。dotfiles の構造には触らない。
;;
;; SSH_AUTH_SOCK を引いているのは Bitwarden の SSH agent のため。GUI から
;; 起動した Emacs は launchd が渡す macOS 標準 agent
;; (/var/run/com.apple.launchd.*/Listeners) を指したままで、そこには鍵が
;; 1 本も入っていない。ローカルに秘密鍵ファイルを置かない運用だと magit や
;; TRAMP の公開鍵認証が黙って全滅する。
;;
;; 【重要】ソケットの export は .zshrc 側に書いてある = 対話シェル専用なので、
;; `exec-path-from-shell-arguments' から "-i" を外すと引けなくなる
;; (zsh の非対話ログインシェルは .zshrc を読まない。実測済み)。既定値が
;; '("-l" "-i") であることに依存している。**速くするために "-i" を外すのでは
;; なく、起動時に起こさないことで速くしている。**

;; exec-path-from-shell は mac / Linux でしか読まないので、flymake のために
;; 宣言しておく (バイトコンパイルしない方針なのでロード時には出ない)。
(declare-function exec-path-from-shell-setenv "exec-path-from-shell" (name value))
(declare-function exec-path-from-shell-getenvs "exec-path-from-shell" (names))
(defvar exec-path-from-shell-variables)
(defvar exec-path-from-shell-arguments)
(defvar exec-path-from-shell-shell-name)
(defvar exec-path-from-shell-warn-duration-millis)

(defvar my:shell-env-cache-file
  (expand-file-name "shell-env-cache.el" user-emacs-directory)
  "ログインシェルから引いた環境変数のキャッシュ。
マシンごとに違う値なので git の管理外に置く (.gitignore 済み)。
消しても壊れない (次の起動が同期で引き直して作り直す)。")

(defconst my:shell-env--unset "__my:shell-env-unset__"
  "シェル側で変数が未設定だったことを表す印。
空文字列では「未設定」と「空」が区別できない。たとえば MANPATH を
空文字列で設定すると man の既定の探索パスが消える。")

(defconst my:shell-env--path-like '("PATH" "MANPATH")
  "コロン区切りの探索パスとして扱う変数。重複を落とす対象。")

(defvar my:shell-env--initial nil
  "この Emacs が起動した時点、つまり何も当てる前の値。
取り直すときにこれを復元してシェルへ渡す。理由は
`my:shell-env-refresh' の docstring を参照。")

(defvar my:shell-env--process nil
  "`my:shell-env-refresh' が起こしたプロセス。二重起動の抑止に使う。")

(defun my:shell-env--read-cache ()
  "`my:shell-env-cache-file' を読んで ((NAME . VALUE) ...) を返す。
読めない・壊れているときは nil。起動を止めてはいけないので握り潰す。"
  (ignore-errors
    (when (file-readable-p my:shell-env-cache-file)
      (with-temp-buffer
        (insert-file-contents my:shell-env-cache-file)
        (let ((alist (read (current-buffer))))
          (and (consp alist) (consp (car alist)) alist))))))

(defun my:shell-env--write-cache (alist)
  "ALIST を `my:shell-env-cache-file' に書く。"
  (ignore-errors
    (with-temp-file my:shell-env-cache-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- lexical-binding: t -*-\n"
                ";; my-shell.el が自動生成する。手で書いても次の起動で上書きされる。\n")
        (prin1 alist (current-buffer))
        (insert "\n")))))

(defun my:shell-env--normalize (alist)
  "ALIST の PATH / MANPATH から重複した要素を落とす (先に出たものを残す)。

【重要】これが無いと PATH が起動のたびに伸びる。.zshrc は
\"PATH=/opt/homebrew/bin:$PATH\" の形で既存の値の前に積んでいくので、
既に PATH が通った Emacs からシェルを起こすと同じ要素が二重に入る
\(実測: 27 要素が 37 要素になった)。それをキャッシュすると次の起動が
その値から始まるので、際限なく積み上がる。探索は先に一致したものが
勝つので、重複を落としても意味は変わらない。"
  (mapcar (lambda (pair)
            (if (and (member (car pair) my:shell-env--path-like)
                     (cdr pair))
                (cons (car pair)
                      (string-join (delete-dups (split-string (cdr pair) ":")) ":"))
              pair))
          alist))

(defun my:shell-env--apply (alist)
  "ALIST の環境変数をこのセッションに反映し、実際に変えた名前の一覧を返す。
PATH のときは `exec-path' と `eshell-path-env' も揃う
\(`exec-path-from-shell-setenv' の仕事)。"
  (let (changed)
    (pcase-dolist (`(,name . ,value) alist)
      (unless (equal value (getenv name))
        ;; SSH_AUTH_SOCK の実体が消えていることがある (Bitwarden を終了した
        ;; 後など)。死んだソケットを掴ませると ssh は agent が無い場合と違って
        ;; 「繋げない」で失敗するので、launchd が渡したものを残したまま
        ;; 裏の取り直しに任せる。
        (unless (and (equal name "SSH_AUTH_SOCK")
                     value
                     (not (file-exists-p value)))
          (exec-path-from-shell-setenv name value)
          (push name changed))))
    (nreverse changed)))

(defun my:shell-env--command ()
  "ログインシェルに環境変数を出力させるコマンド行を返す。"
  (let* ((names exec-path-from-shell-variables)
         (shell (or exec-path-from-shell-shell-name
                    shell-file-name
                    (getenv "SHELL")
                    "/bin/sh"))
         (printf (or (executable-find "printf") "printf"))
         ;; __RESULT で囲むのは exec-path-from-shell と同じ理由。対話シェルの
         ;; 初期化スクリプトや zsh 自身 (-i で tty が無いと "can't change
         ;; option: zle") が標準出力・標準エラーに書くので、素で読むと混ざる。
         (expr (concat (shell-quote-argument printf)
                       " '__RESULT\\000"
                       (mapconcat (lambda (_) "%s") names "\\000")
                       "\\000__RESULT' "
                       (mapconcat (lambda (name)
                                    (format "\"${%s-%s}\"" name my:shell-env--unset))
                                  names " "))))
    (append (list shell) exec-path-from-shell-arguments (list "-c" expr))))

(defun my:shell-env--parse (buffer)
  "BUFFER に出たシェルの出力を ((NAME . VALUE) ...) に直す。読めなければ nil。"
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "__RESULT\0\\(.*\\)\0__RESULT" nil t)
      (let ((values (split-string (match-string 1) "\0")))
        (when (= (length values) (length exec-path-from-shell-variables))
          (my:shell-env--normalize
           (seq-mapn (lambda (name value)
                       (cons name (unless (equal value my:shell-env--unset) value)))
                     exec-path-from-shell-variables values)))))))

(defun my:shell-env--sentinel (proc _event)
  "シェルが終わったら値を反映してキャッシュを更新する。"
  (when (memq (process-status proc) '(exit signal))
    (let ((buffer (process-buffer proc)))
      (unwind-protect
          (let ((alist (and (eq (process-status proc) 'exit)
                            (zerop (process-exit-status proc))
                            (buffer-live-p buffer)
                            (my:shell-env--parse buffer))))
            (if (not alist)
                ;; 黙って失敗させない。ここが落ちると PATH が永久に
                ;; キャッシュのまま古くなるので、気づく手がかりを残す。
                (message "my:shell-env-refresh: シェルから値を取れなかった (exit %s)"
                         (process-exit-status proc))
              (let ((changed (my:shell-env--apply alist)))
                (my:shell-env--write-cache alist)
                (when changed
                  (message "シェルの環境変数を更新した: %s"
                           (mapconcat #'identity changed " "))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun my:shell-env-refresh (&optional interactive)
  "ログインシェルから環境変数を取り直し、キャッシュを更新する。
非同期なのでこの呼び出し自体はすぐ返る。値が変わっていればその場で反映する。
`brew install' や .zshrc の編集を今すぐ反映したいときは手で呼ぶ。

シェルに渡す環境は `my:shell-env--initial' に戻してから起動する。
キャッシュを当てた後の PATH を渡すと、.zshrc が既にある要素をもう一度
前に積むため、素の `exec-path-from-shell-initialize' とは違う値になる
\(重複は `my:shell-env--normalize' も落とすが、二重の保険にしている)。"
  (interactive (list t))
  (unless (boundp 'exec-path-from-shell-arguments)
    (user-error "この環境では exec-path-from-shell を使っていない"))
  (if (process-live-p my:shell-env--process)
      (when interactive (message "取り直しはすでに走っている"))
    ;; TRAMP のバッファから呼ばれるとリモートでシェルが起動してしまう。
    (let* ((default-directory (expand-file-name "~"))
           (process-environment (copy-sequence process-environment))
           (buffer (generate-new-buffer " *shell-env*")))
      (pcase-dolist (`(,name . ,value) my:shell-env--initial)
        (setenv name value))
      (setq my:shell-env--process
            (make-process :name "shell-env"
                          :buffer buffer
                          :command (my:shell-env--command)
                          :connection-type 'pipe
                          :coding '(utf-8-unix . utf-8-unix)
                          :noquery t
                          :sentinel #'my:shell-env--sentinel))
      (when interactive
        (message "シェルの環境変数を取り直している…")))))

(use-package exec-path-from-shell
  :straight t
  ;; macOS 限定だったが、Linux でも GUI 起動時はログインシェルの
  ;; 環境変数を引き継がないため必要になる。
  ;; berkeley-unix (FreeBSD) は対象外にした (計画書の L-4)
  :if (memq system-type '(darwin gnu/linux))
  :custom (exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  :config
  ;; 当てる前の値を覚えておく (取り直しのときにこれを渡す)。
  (setq my:shell-env--initial
        (mapcar (lambda (name) (cons name (getenv name)))
                exec-path-from-shell-variables))
  (let ((cache (my:shell-env--read-cache)))
    (if (and cache
             ;; 変数を足した直後はキャッシュに無いものがある。
             (seq-every-p (lambda (name) (assoc name cache))
                          exec-path-from-shell-variables))
        (my:shell-env--apply cache)
      ;; キャッシュが無い初回だけ同期で引く。my-shell より後ろのモジュールが
      ;; executable-find を使うので、ここを非同期にすると PATH が決まる前に
      ;; 判定が走る。1 回払えば次の起動からは 0 になるので警告は黙らせる。
      (let* ((exec-path-from-shell-warn-duration-millis most-positive-fixnum)
             (alist (my:shell-env--normalize
                     (exec-path-from-shell-getenvs exec-path-from-shell-variables))))
        (my:shell-env--apply alist)
        (my:shell-env--write-cache alist))))
  ;; 起動が済んでから裏で取り直す。make-process はすぐ返るので起動時間には
  ;; 乗らない。
  (add-hook 'emacs-startup-hook #'my:shell-env-refresh))

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
