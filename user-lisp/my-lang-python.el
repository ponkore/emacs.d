;;; my-lang-python.el --- Python  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] Python

;;
;; python
;;
(leaf pyvenv
  :straight t
  :config
  ;; 起動時に固定の venv を有効化していたが、その venv は elpy の RPC 用で
  ;; あって開発対象のものではなかった。elpy を外したので参照もやめる。
  ;; venv はプロジェクトごとに M-x pyvenv-activate / pyvenv-workon で選ぶ。
  )

(leaf py-isort
  :straight t
  ;; before-save-hook にグローバル登録していたため、Python 以外の
  ;; すべてのファイル保存時にも py-isort が走っていた。python 限定にする。
  ;; フックはパッケージのロード前に登録されるので関数定義は :preface に置く。
  :preface
  (defun my:py-isort-on-save ()
    "Python のバッファにだけ py-isort を仕込む。"
    (add-hook 'before-save-hook #'py-isort-before-save nil t))
  :hook
  ((python-mode-hook python-ts-mode-hook) . my:py-isort-on-save))

(leaf python
  ;; elpy は廃止した。elpy は独自の RPC サーバ・company・flymake モジュールを
  ;; 抱えた統合環境で、eglot / corfu / flymake への移行と二重になる。
  ;; 補完・定義ジャンプ・診断は eglot (pylsp や basedpyright) が担当し、
  ;; 整形は blacken、import 整理は py-isort に任せる。
  ;; REPL は組み込みの python-mode が持っている (C-c C-c / C-c C-p)。
  :mode ("\\.py\\'" . python-mode)
  :preface
  (defun my:python-setup ()
    (setq-local indent-tabs-mode nil)
    (eglot-ensure))
  :hook
  ((python-mode-hook python-ts-mode-hook) . my:python-setup)
  :bind
  ;; 旧 elpy-format-code の置き換え
  (:python-mode-map
   ("C-c C-r f" . blacken-buffer)))

;; leaf の :config は (eval-after-load 'python ...) に包まれるため、
;; そこで差し替えても「その回に開いたバッファ」には間に合わない。
;; メジャーモードの差し替えは起動時に済ませておく必要がある。
(my:treesit-remap 'python-mode 'python-ts-mode 'python)

(leaf blacken
  :straight t
  :custom ((blacken-line-length . 100)
           ;; (blacken-skip-string-normalization . t)
           ))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
