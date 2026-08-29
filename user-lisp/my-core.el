;;; my-core.el --- 汎用ヘルパと基礎ライブラリ  -*- lexical-binding: nil -*-
;;; Commentary:
;; init.el から機械的に分割したもの。読み込み順は init.el を参照。
;; lexical-binding は分割前と同じ意味論を保つため nil のまま。
;;; Code:

;;; [3] s

(leaf s
  :straight t
  :commands s-join s-split)

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

(provide 'my-core)
;;; my-core.el ends here
