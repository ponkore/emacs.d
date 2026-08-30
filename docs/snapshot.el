;;; snapshot.el --- 設定状態のスナップショット  -*- lexical-binding: t -*-
;;; Commentary:
;; leaf → use-package 移行の等価性検証に使う。
;; 設定を読み込んだ Emacs の「観測可能な状態」を決定的な順序でダンプする。
;; 変換の前後で採取して diff を取り、差分が意図したものだけかを確認する。
;;
;; 使い方:
;;   emacs --batch -l early-init.el -l init.el -l docs/snapshot.el \
;;         --eval '(my:snapshot-dump "before.txt")'
;;
;; 採取する対象:
;;   [VAR]   standard-value を持つ全変数 (= defcustom) の default-value
;;           → leaf/use-package の :custom の効果がすべて入る
;;   [HOOK]  *-hook / *-functions の値
;;   [KEY]   global-map と minor-mode-map-alist の全キーバインド
;;   [FACE]  face の theme-face / defface-spec / 実効属性
;;           custom-set-faces (user テーマ) と face-spec-set (defface spec) は
;;           テーマとの優先順位が違うので、両方を別々に記録する
;;   [FEAT]  ロード済み feature
;;; Code:

(require 'cl-lib)

(defun my:snapshot--pp (v)
  "V を決定的な文字列にする。"
  (let ((print-length nil) (print-level nil) (print-circle t)
        (print-quoted t) (float-output-format nil))
    (condition-case e (prin1-to-string v)
      (error (format "<unprintable: %S>" e)))))

(defun my:snapshot--keymap-lines (map prefix out)
  "MAP を PREFIX 付きで走査し、行を OUT (リストのセル) に積む。"
  (when (keymapp map)
    (map-keymap
     (lambda (key def)
       (let ((seq (vconcat prefix (if (consp key) (vector (car key)) (vector key)))))
         (cond
          ;; 文字範囲 (cons) は代表点だけ記録して展開しない
          ((keymapp def)
           ;; 循環を避けるため深さを 4 までに制限
           (when (< (length seq) 4)
             (my:snapshot--keymap-lines def seq out)))
          (def
           (push (format "%s -> %s" (key-description seq) (my:snapshot--pp def))
                 (car out))))))
     map)))

(defun my:snapshot--keymap-dump (name map)
  "MAP の全バインドを NAME 付きの行リストで返す。"
  (let ((out (list nil)))
    (ignore-errors (my:snapshot--keymap-lines map [] out))
    (mapcar (lambda (l) (format "[KEY] %s: %s" name l)) (sort (car out) #'string<))))

(defun my:snapshot--face-lines ()
  "全 face の指定と実効属性を行リストで返す。"
  (let (out)
    (dolist (f (face-list))
      (let ((n (symbol-name f)))
        (push (format "[FACE] %s theme-face=%s defface=%s attrs=%s"
                      n
                      (my:snapshot--pp (get f 'theme-face))
                      (my:snapshot--pp (get f 'face-defface-spec))
                      (my:snapshot--pp
                       (condition-case nil (face-all-attributes f) (error 'err))))
              out)))
    (sort out #'string<)))

(defun my:snapshot-lines ()
  "スナップショットの行リストを返す。"
  (let (vars hooks keys feats)
    (mapatoms
     (lambda (s)
       (let ((n (symbol-name s)))
         (cond
          ;; フック類は別枠 (値が大きく、意味も違うため)
          ((and (boundp s)
                (or (string-suffix-p "-hook" n) (string-suffix-p "-functions" n)))
           (push (format "[HOOK] %s = %s" n (my:snapshot--pp (default-value s))) hooks))
          ;; defcustom (standard-value を持つもの)
          ((and (boundp s) (get s 'standard-value))
           (push (format "[VAR] %s = %s" n (my:snapshot--pp (default-value s))) vars))))))
    (setq keys (my:snapshot--keymap-dump "global-map" global-map))
    (dolist (entry minor-mode-map-alist)
      (when (and (symbolp (car entry)) (keymapp (cdr entry)))
        (setq keys (append keys
                           (my:snapshot--keymap-dump
                            (format "minor:%s" (car entry)) (cdr entry))))))
    (setq feats (mapcar (lambda (f) (format "[FEAT] %s" f))
                        (sort (mapcar #'symbol-name features) #'string<)))
    (append (sort vars #'string<) (sort hooks #'string<) (sort keys #'string<)
            (my:snapshot--face-lines) feats)))

(defun my:snapshot-dump (file)
  "スナップショットを FILE に書き出す。"
  (with-temp-file file
    (dolist (l (my:snapshot-lines)) (insert l "\n")))
  (message "snapshot -> %s" file))

(provide 'snapshot)
;;; snapshot.el ends here
