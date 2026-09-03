;;; my-core.el --- 汎用ヘルパと基礎ライブラリ  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; [3] s

(use-package s
  :straight t
  :commands s-join s-split)

;;; [3] hydra

;; hydra は my-appearance / my-dired / my-lsp / my-editor / my-text の
;; :init に置いた defhydra から使う。defhydra は autoload マクロなので
;; straight-use-package で autoloads が読まれていれば足りるが、最初に使う
;; my-appearance より前に宣言しておく必要がある。my-core は init.el が
;; 最初に require するモジュールなのでここに置く
;; (leaf のときは init.el のブートストラップで導入していた)。
(use-package hydra
  :straight t
  :defer t)

;;; [3] 汎用ヘルパ

(defun my:pandoc-data-directory ()
  "pandoc のユーザーデータディレクトリを返す。
Windows は %APPDATA%/pandoc、それ以外は XDG または ~/.pandoc。"
  (cond
   ((eq system-type 'windows-nt)
    (expand-file-name "pandoc" (or (getenv "APPDATA") "~")))
   (t
    (let ((xdg (expand-file-name "pandoc" (or (getenv "XDG_DATA_HOME")
                                              "~/.local/share"))))
      (if (file-directory-p xdg) xdg (expand-file-name "~/.pandoc"))))))

(defun my:pandoc-data-file (name)
  "pandoc のユーザーデータディレクトリ配下の NAME を返す。"
  (expand-file-name name (my:pandoc-data-directory)))

(defun my:open-file-externally (file)
  "FILE を OS のファイル関連付けに渡して外部アプリで開く。
Windows は `w32-shell-execute' (ShellExecuteW)、macOS は open(1)、
それ以外は xdg-open。いずれも Emacs を待たせない。"
  (let ((file (expand-file-name file)))
    (unless (file-exists-p file)
      (user-error "No such file: %s" file))
    (pcase system-type
      ;; ShellExecuteW はバックスラッシュ区切りを期待する。
      ;; convert-standard-filename が w32 では unix->dos 変換をする。
      ('windows-nt (w32-shell-execute "open" (convert-standard-filename file)))
      ;; DESTINATION に 0 を渡すと非同期になり、終了を待たない。
      ('darwin (call-process "open" nil 0 nil file))
      (_ (call-process "xdg-open" nil 0 nil file)))
    (message "Opened externally: %s" (abbreviate-file-name file))))

;;; [3] tree-sitter

;; Emacs 29 以降の組み込み tree-sitter。文法 (grammar) は共有ライブラリなので
;; 別途ビルドが必要で、C コンパイラと git が要る。
;; 文法が無い環境で *-ts-mode に切り替えると何も動かなくなるため、
;; 「文法が実際に使えるときだけ従来のモードを *-ts-mode に差し替える」形にする。
;; 文法の導入は M-x my:install-treesit-grammars

(require 'treesit nil t)

(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c-sharp    . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          ;; go / gomod / gowork は go-ts-mode.el 自身も
          ;; treesit-language-source-alist に追加するが、この setq が
          ;; 丸ごと上書きするため my:install-treesit-grammars からは見えない。
          ;; go-ts-mode.el と同じ内容 (commit まで) を書いておくこと。
          ;; add-to-list は equal で重複を避けるので二重登録にはならないが、
          ;; 1 文字でも違うと両方が登録されて 2 回ビルドされる。
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"
                         :commit "12fe553fdaaa7449f764bc876fd777704d4fb752"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"
                         :commit "3b01edce2b9ea6766ca19328d1850e456fde3103"))
          (gowork     . ("https://github.com/omertuc/tree-sitter-go-work"
                         :commit "949a8a470559543857a62102c84700d291fc984c"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          ;; js-ts-mode がコメントの解析に使う (treesit-ensure-installed される)
          (jsdoc      . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          ;; typescript のリポジトリは typescript と tsx の 2 つの文法を持つ
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript"
                         nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                         nil "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml")))))

(defun my:treesit-available-p (language)
  "LANGUAGE の tree-sitter 文法が実際に使えるなら non-nil。"
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language)))

(defun my:treesit-remap (mode ts-mode language)
  "LANGUAGE の文法が使えるときだけ MODE を TS-MODE に差し替える。
差し替えたときは non-nil を返す。"
  (when (and (my:treesit-available-p language) (fboundp ts-mode))
    (add-to-list 'major-mode-remap-alist (cons mode ts-mode))
    t))

(defun my:install-treesit-grammars (&optional force)
  "`treesit-language-source-alist' の文法をまとめて導入する。
導入済みのものは飛ばす。前置引数 FORCE を付けると入れ直す。
C コンパイラ (gcc など) と git が必要。"
  (interactive "P")
  (require 'treesit)
  (dolist (source treesit-language-source-alist)
    (let ((language (car source)))
      (if (and (not force) (treesit-language-available-p language))
          (message "treesit: %s は導入済み" language)
        (message "treesit: %s を導入中..." language)
        (condition-case err
            (treesit-install-language-grammar language)
          (error (message "treesit: %s の導入に失敗: %s"
                          language (error-message-string err)))))))
  (message "treesit: 完了。反映には再起動が必要"))

(provide 'my-core)
;;; my-core.el ends here
