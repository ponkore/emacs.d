;;; my-lang-native.el --- ネイティブ系 (Rust / C++ / C#)  -*- lexical-binding: t -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;;; Code:

;;; [3] rust

;; (use-package racer
;;   :straight t)

(use-package cargo
  :straight t
  :defer t)

(use-package rust-mode
  ;; 文法が入っていない環境向けのフォールバック。文法があれば組み込みの
  ;; rust-ts-mode に差し替わる。
  :straight t
  :custom
  (rust-format-on-save t)
  :preface
  (defun my:rust-ts-setup ()
    "rust-ts-mode には rust-format-on-save が無いので eglot で整形する。"
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  :hook
  ;; 診断は rust-analyzer が eglot 経由で flymake に出すため、
  ;; flycheck-rust (cargo check をラップするもの) は外した。
  ((rust-mode-hook rust-ts-mode-hook) . eglot-ensure)
  ((rust-mode-hook rust-ts-mode-hook) . cargo-minor-mode)
  ((rust-mode-hook rust-ts-mode-hook) . yas-minor-mode)
  (rust-ts-mode-hook . my:rust-ts-setup))

;; use-package の :config は (with-eval-after-load 'rust-mode ...) に包まれる。
;; rust-ts-mode に差し替えると rust-mode 自体がロードされなくなるので、
;; 差し替えとインデント設定はトップレベルで済ませる。
(setq rust-ts-mode-indent-offset 4)
(my:treesit-remap 'rust-mode 'rust-ts-mode 'rust)

;;; [3] C++

(use-package cc-mode
  :hook (c-mode-common-hook . my:cc-mode-setup)
  :config
  (defun my:cc-mode-setup ()
    ;; BSDスタイルをベースにする
    (c-set-style "bsd")
    ;; スペースでインデントをする
    (setq indent-tabs-mode nil)
    ;; インデント幅を2にする
    (setq c-basic-offset 2)
    ;; 自動改行（auto-new-line）と
    ;; 連続する空白の一括削除（hungry-delete）を
    ;; 有効にする
    (c-toggle-auto-hungry-state 1)
    ;; CamelCaseの語でも単語単位に分解して編集する
    ;; GtkWindow         => Gtk Window
    ;; EmacsFrameClass   => Emacs Frame Class
    ;; NSGraphicsContext => NS Graphics Context
    (subword-mode 1)))

;;; [3] C#

(use-package csharp-mode
  ;; Emacs 29 以降 csharp-mode / csharp-ts-mode は組み込み。
  ;; 外部パッケージ (v1.1.1) が組み込みを上書きしていたため :straight t を外す。
  ;; csharp-mode は cc-mode 派生、csharp-ts-mode は tree-sitter 派生で
  ;; インデントの設定方法が違うため、セットアップ関数を分けてある。
  :hook
  (csharp-mode-hook . my:csharp-mode-setup)
  (csharp-ts-mode-hook . my:csharp-ts-mode-setup)
  :config
  (defun my:csharp-ts-mode-setup ()
    "csharp-ts-mode 用のセットアップ。"
    (turn-on-auto-revert-mode)
    (setq indent-tabs-mode nil)
    (setq comment-column 40)
    (yas-minor-mode-on)
    (eglot-ensure))
  (defun my:csharp-mode-setup ()
    "my function that runs when csharp-mode is initialized for a buffer."
    (turn-on-font-lock)
    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
    (setq indent-tabs-mode nil) ;; tabs are evil
    (yas-minor-mode-on)
    (setq comment-column 40)
    (setq c-basic-offset 4)
    ;; (font-lock-add-magic-number)
    ;; オフセットの調整
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    ;; see http://qiita.com/masnagam/items/e3313dc9a66bd7fd76fa
    ;; csharp-want-imenu は旧・外部 csharp-mode (v1.1.1) の変数で
    ;; 組み込みの csharp-mode には存在しないため無効化
    ;; (setq csharp-want-imenu nil)
    ))

;; csharp-mode の :config も同じ理由でトップレベルに出す。
;; csharp-ts-mode-indent-offset は Emacs 31 で csharp-ts-indent-offset に
;; 改名された (obsolete variable alias)。別名なので代入自体は通っていたが、
;; コンパイル時に警告が出るので新しい名前を使う。
(setq csharp-ts-indent-offset 4)
(my:treesit-remap 'csharp-mode 'csharp-ts-mode 'c-sharp)

;;; [3] Go

;; Emacs 31.1 同梱の go-ts-mode / go-mod-ts-mode / go-work-ts-mode を使う。
;;
;; 外部の go-mode は入れない。go-ts-mode.el が autoload で
;;   (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode-maybe))
;;   (add-to-list 'treesit-major-mode-remap-alist '(go-mode . go-ts-mode))
;; を済ませており、go-mode を :mode で足すと auto-mode-alist の先頭に
;; 積まれて必ず go-mode が勝つ。しかも treesit-major-mode-remap-alist は
;; treesit-enabled-modes が非 nil のときしか major-mode-remap-alist に
;; 反映されない (既定は nil) ので、tree-sitter 版への差し替えも起きない。
;; つまり go-mode を「フォールバック」として足すと、フォールバックのまま
;; 固定される。Rust / C# のような my:treesit-remap も要らない。
;;
;; 文法が無い環境では go-ts-mode-maybe が fundamental-mode にする。
;; M-x my:install-treesit-grammars で go / gomod / gowork を入れること。
;;
;; 外部ツールは gopls だけでよい:
;;   go install golang.org/x/tools/gopls@latest
;; 整形の gofumpt と静的解析の staticcheck は gopls に内蔵されており、
;; goimports 相当は source.organizeImports の code action が担う。
;; いずれも my-lsp.el の eglot-workspace-configuration で有効にしてある。

(use-package go-ts-mode
  ;; Emacs 31.1 組み込みなので :straight は付けない。:mode も書かない (上記)。
  :custom
  ;; gofmt はタブでインデントするので indent-tabs-mode は t のまま
  ;; (go-ts-mode 自身が設定する)。go-ts-indent-offset は「タブ何個ぶんか」
  ;; ではなく桁数なので、tab-width と揃えないと継続行がずれる。
  (go-ts-indent-offset 4)
  :bind
  ;; C-c C-d (docstring) と C-c C-t t/f/p (テスト実行) は go-ts-mode-map に
  ;; 最初から入っているので張り直さない。
  (:map go-ts-mode-map
   ("C-c C-l" . my:go-golangci-lint))
  :hook
  ;; go.mod / go.work でも gopls は動く (依存の診断が出る)。
  ((go-ts-mode-hook go-mod-ts-mode-hook go-work-ts-mode-hook) . eglot-ensure)
  (go-ts-mode-hook . yas-minor-mode)
  (go-ts-mode-hook . my:go-ts-setup)
  :preface
  (defun my:go-organize-imports ()
    "gopls に source.organizeImports を適用させる。
`eglot-code-actions' を対話的に (INTERACTIVE 非 nil で) 呼ぶと、該当が
0 件のときに `eglot--error' が飛んで before-save-hook ごと止まり、
保存できなくなる。非対話で候補を取り出して、あるときだけ実行する。"
    (when-let* ((server (eglot-current-server))
                (actions (eglot-code-actions (point-min) (point-max)
                                             "source.organizeImports")))
      (dolist (action actions)
        (eglot-execute server action))))

  (defun my:go-before-save ()
    "import を整理してから gofumpt で整形する。
順序は逆にできない。整形が先だと、あとから足された import 行が
整形されないまま残る。"
    (when (eglot-managed-p)
      (my:go-organize-imports)
      (eglot-format-buffer)))

  (defun my:go-ts-setup ()
    (setq-local tab-width 4)
    (add-hook 'before-save-hook #'my:go-before-save nil t))

  (defun my:go-golangci-lint ()
    "モジュールのルートで golangci-lint を走らせる。
gopls 内蔵の staticcheck より重く、保存のたびに走らせると
flymake-no-changes-timeout (1.0 秒) に間に合わないので、
flymake には載せずに明示的に呼ぶ形にしてある。"
    (interactive)
    (let ((default-directory
           (or (locate-dominating-file default-directory "go.mod")
               default-directory)))
      (compile "golangci-lint run ./..."))))

(provide 'my-lang-native)
;;; my-lang-native.el ends here
