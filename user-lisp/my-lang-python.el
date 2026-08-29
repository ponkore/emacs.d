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
  ;; 存在しない環境で pyvenv-activate を呼ぶとエラーになるため存在確認する。
  ;; パスも user-emacs-directory 基準にする。
  (let ((venv (expand-file-name "elpy/rpc-venv" user-emacs-directory)))
    (when (file-directory-p venv)
      (pyvenv-activate venv))))

(leaf py-isort
  :straight t
  ;; before-save-hook にグローバル登録していたため、Python 以外の
  ;; すべてのファイル保存時にも py-isort が走っていた。python-mode 限定にする。
  ;; フックはパッケージのロード前に登録されるので関数定義は :preface に置く。
  :preface
  (defun my:py-isort-on-save ()
    "python-mode のバッファにだけ py-isort を仕込む。"
    (add-hook 'before-save-hook #'py-isort-before-save nil t))
  :hook
  (python-mode-hook . my:py-isort-on-save))

(leaf python
  :mode ("\\.py$" . python-mode)
  :hook
  (python-mode-hook . my:python-mode-hook-0)
  :preface
  (defun my:python-mode-hook-0 ()
    (setq-local indent-tabs-mode nil)))

(leaf elpy
  ;; https://elpy.readthedocs.io/en/latest/index.html
  :straight t
  ;; :init で (elpy-enable) を呼ぶと Python を使わない起動でも elpy 一式が
  ;; 読み込まれる。python-mode に入ったときだけ有効化する。
  ;; フックはパッケージのロード前に登録されるので、関数定義は :preface に置く
  ;; (:config はロード後に走るため間に合わない)。
  :preface
  (defvar my:elpy-enabled nil)
  (defun my:elpy-enable-once ()
    (unless my:elpy-enabled
      (setq my:elpy-enabled t)
      (elpy-enable)))
  :hook
  (python-mode-hook . my:elpy-enable-once)
  (elpy-mode-hook . flycheck-mode)
  :bind (elpy-mode-map
         ("C-c C-r f" . elpy-format-code))
  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
  (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
  :custom
  (elpy-rpc-python-command . "python")
  (flycheck-python-flake8-executable . "flake8"))

(leaf blacken
  :straight t
  :custom ((blacken-line-length . 100)
           ;; (blacken-skip-string-normalization . t)
           ))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
