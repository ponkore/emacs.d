;;; my-lang-native.el --- ネイティブ系 (Rust / C++ / C#)  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] rust

;; (leaf racer
;;   :straight t)

(leaf cargo
  :straight t)

(leaf rust-mode
  :straight t
  :custom
  (rust-format-on-save . t)
  :hook
  ;; 診断は rust-analyzer が eglot 経由で flymake に出すため、
  ;; flycheck-rust (cargo check をラップするもの) は外した。
  (rust-mode-hook . eglot-ensure)
  (rust-mode-hook . cargo-minor-mode)
  (rust-mode-hook . yas-minor-mode))

;;; [3] C++

(leaf cc-mode
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

(leaf csharp-mode
  ;; Emacs 29 以降 csharp-mode / csharp-ts-mode は組み込み。
  ;; 外部パッケージ (v1.1.1) が組み込みを上書きしていたため :straight t を外す。
  :hook (csharp-mode-hook . my:csharp-mode-setup)
  :config
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

(provide 'my-lang-native)
;;; my-lang-native.el ends here
