;;; probe.el --- hwnd-probe.dll を emacs -Q の GUI で動かして測る  -*- lexical-binding: t -*-
;;
;; emacs.exe -Q -l probe.el で起動する。手順を 0.5 秒おきに進め
;; (コマンドループに戻って再描画と入力を処理させるため)、結果を out.txt に
;; 書いて kill-emacs する。

(setq inhibit-startup-screen t)

(defvar hp-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar hp-log nil)
(defvar hp-parent nil)
(defvar hp-frame nil)
(defvar hp-buf nil)
(defvar hp-views nil "((WINDOW . ID) ...)")
(defvar hp-steps nil)
(defvar hp-next-delay nil)
(defvar hp-f24-times nil)
(defvar hp-t0 nil)
(defvar hp-sync-count 0)
(defvar hp-child nil)

(defun hp-log (fmt &rest args)
  (push (apply #'format fmt args) hp-log))

(defun hp-finish ()
  (ignore-errors
    (dolist (c hp-views) (hwnd-probe-destroy (cdr c)))
    (hwnd-probe-topmost hp-parent nil))
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (expand-file-name "out.txt" hp-dir)
      (insert (mapconcat #'identity (reverse hp-log) "\n") "\n")))
  (kill-emacs 0))

(run-at-time 120 nil (lambda () (hp-log "WATCHDOG") (hp-finish)))

(defun hp-f24 ()
  (interactive)
  (push (float-time) hp-f24-times))
(global-set-key (kbd "<f24>") #'hp-f24)

;;; ---------- 位置合わせ (本番の設計の雛形) ----------

(defun hp-sync (&optional _frame)
  (setq hp-sync-count (1+ hp-sync-count))
  (dolist (c hp-views)
    (let ((w (car c)) (id (cdr c)))
      (if (and (window-live-p w) (eq (window-buffer w) hp-buf))
          (pcase-let ((`(,l ,tp ,r ,b) (window-inside-pixel-edges w)))
            (hwnd-probe-move id l tp (- r l) (- b tp))
            (hwnd-probe-show id t))
        (hwnd-probe-show id nil)))))

(add-hook 'window-size-change-functions #'hp-sync)
(add-hook 'window-buffer-change-functions #'hp-sync)

;;; ---------- 測る小道具 ----------

(defun hp-px (x y) (hwnd-probe-pixel hp-parent x y))

(defun hp-sample (w tag)
  "W の本文領域の境界の内側と外側の色を 1 px ずつ読む。"
  (pcase-let ((`(,l ,tp ,r ,b) (window-inside-pixel-edges w)))
    (hp-log "%s edges=%S" tag (list l tp r b))
    (hp-log "  inside : TL(l,t)=%s  BR(r-1,b-1)=%s  near-TL(l+5,t+5)=%s"
            (hp-px l tp) (hp-px (1- r) (1- b)) (hp-px (+ l 5) (+ tp 5)))
    (hp-log "  outside: left(l-1)=%s  above(t-1)=%s  right(r)=%s  below(b)=%s"
            (hp-px (1- l) (+ tp 5)) (hp-px (+ l 5) (1- tp))
            (hp-px r (+ tp 5)) (hp-px (+ l 5) b))))

(defun hp-capture (name)
  (hwnd-probe-capture hp-parent (expand-file-name name hp-dir)))

(defun hp-focus-str (id)
  (let ((v (hwnd-probe-focus id)))
    (if (< v 0) "UI thread timeout"
      (format "focus=%s foreground=%s"
              (pcase (logand v 15) (0 "none") (1 "emacs-frame") (2 "child-view") (_ "other"))
              (if (/= 0 (logand v 16)) "emacs" "other")))))

;;; ---------- 手順 ----------

(defmacro hp-step (name &rest body)
  `(push (cons ,name (lambda () ,@body)) hp-steps))

(hp-step "setup"
  (setq hp-frame (selected-frame))
  (set-frame-position hp-frame 120 80)
  (set-frame-size hp-frame 100 36)
  (setq hp-buf (get-buffer-create "*probe*"))
  (switch-to-buffer hp-buf)
  (dotimes (i 200) (insert (format "line %03d  the quick brown fox jumps over the lazy dog\n" i)))
  (goto-char (point-min))
  (setq hp-parent (string-to-number (frame-parameter hp-frame 'window-id)))
  (hp-log "window-id=%s  emacs=%s" hp-parent emacs-version)
  (hp-log "load: %S" (module-load (expand-file-name "hwnd-probe.dll" hp-dir)))
  (hp-log "topmost: %S" (hwnd-probe-topmost hp-parent t))
  (setq hp-next-delay 1.0))

(hp-step "env"
  (hp-log "env (awareness dpi root-p client-w client-h sx sy) = %S" (hwnd-probe-env hp-parent))
  (hp-log "frame-geometry: %S" (frame-geometry hp-frame))
  (hp-log "frame-inner-size: %S x %S" (frame-inner-width hp-frame) (frame-inner-height hp-frame))
  (hp-log "frame-native-size: %S x %S" (frame-native-width hp-frame) (frame-native-height hp-frame)))

(hp-step "create A"
  (let* ((w (selected-window))
         (t0 (float-time))
         (id (hwnd-probe-create hp-parent "#1e90ff" "view A")))
    (hp-log "create A => %S (%.1f ms)" id (* 1000 (- (float-time) t0)))
    (push (cons w id) hp-views)
    (hp-sync)))

(hp-step "sample single"
  (hp-sample (selected-window) "[single]")
  (hp-log "  info A (paints clicks mouseact setfocus) = %S" (hwnd-probe-info 0))
  (hp-capture "1-single.bmp"))

(hp-step "redisplay over view"
  ;; Emacs に本文を書き換えさせ、スクロールさせ、強制再描画させる
  (with-current-buffer hp-buf
    (goto-char (point-min))
    (insert (make-string 300 ?X) "\n")
    (set-window-start (selected-window) 500)
    (forward-line 20))
  (redisplay t))

(hp-step "sample after redisplay"
  (hp-sample (selected-window) "[after redisplay]")
  (hp-log "  info A = %S" (hwnd-probe-info 0)))

(hp-step "split"
  (let* ((w2 (split-window-below))
         (id (hwnd-probe-create hp-parent "#32cd32" "view B")))
    (push (cons w2 id) hp-views)
    (hp-log "split: B id=%S sync-count=%d" id hp-sync-count)
    (hp-sync)))

(hp-step "sample split"
  (dolist (c (reverse hp-views))
    (hp-sample (car c) (format "[split id=%d]" (cdr c))))
  (hp-capture "2-split.bmp"))

(hp-step "resize frame"
  (setq hp-sync-count 0)
  (set-frame-size hp-frame 80 30))

(hp-step "sample resized"
  (hp-log "sync-count after resize = %d (hook driven)" hp-sync-count)
  (dolist (c (reverse hp-views))
    (hp-sample (car c) (format "[resized id=%d]" (cdr c)))))

(hp-step "switch buffer in B"
  (set-window-buffer (car (car hp-views)) (messages-buffer)))

(hp-step "sample hidden B"
  (hp-sample (car (car hp-views)) "[B shows *Messages*]")
  (set-window-buffer (car (car hp-views)) hp-buf))

;; 子フレームは corfu / posframe と同じく、ツールバー・メニューバー・
;; スクロールバー無しで作る (-Q だと子フレームにもツールバーが付き、
;; 測る点がアイコンに当たる)。
(defvar hp-child-xy nil "子フレームの本文の中の 1 点 (親のクライアント座標)。")

(defun hp-make-child ()
  (pcase-let* ((wa (car (car (last hp-views))))
               (`(,l ,tp ,_r ,_b) (window-inside-pixel-edges wa)))
    (setq hp-child-xy (list (+ l 60) (+ tp 60)))
    (prog1 (make-frame `((parent-frame . ,hp-frame)
                         (left . ,(+ l 40)) (top . ,(+ tp 40))
                         (width . 20) (height . 4)
                         (minibuffer . nil) (undecorated . t)
                         (no-accept-focus . t)
                         (tool-bar-lines . 0) (menu-bar-lines . 0)
                         (vertical-scroll-bars . nil)
                         (background-color . "#ff00ff")))
      (select-frame hp-frame))))

(defun hp-who-on-top (tag)
  (let ((c (apply #'hp-px hp-child-xy)))
    (hp-log "%-44s pixel=%s => %s" tag c
            (pcase c ("#ff00ff" "CHILD FRAME on top") ("#1e90ff" "VIEW on top") (_ "?")))))

(hp-step "child frame"
  (setq hp-child (hp-make-child)))

(hp-step "z: child frame created after view"
  (hp-who-on-top "[z] child frame created after view")
  (hp-capture "3-childframe.bmp")
  (hwnd-probe-zorder 0 t))

(hp-step "z: view raised to HWND_TOP"
  (hp-who-on-top "[z] view raised to HWND_TOP")
  (make-frame-invisible hp-child))

(hp-step "z: show again"
  (make-frame-visible hp-child))

(hp-step "z: after invisible->visible"
  (hp-who-on-top "[z] child frame invisible->visible")
  (hwnd-probe-zorder 0 t))

(hp-step "z: move child"
  (hp-who-on-top "[z] view raised again")
  ;; corfu は子フレームを使い回し、位置だけ変えて出し直す
  (set-frame-position hp-child
                      (+ (frame-parameter hp-child 'left) 4)
                      (+ (frame-parameter hp-child 'top) 4))
  (setq hp-child-xy (mapcar (lambda (v) (+ v 4)) hp-child-xy)))

(hp-step "z: after set-frame-position"
  (hp-who-on-top "[z] child frame moved (set-frame-position)")
  (hwnd-probe-zorder 0 nil))

(hp-step "z: view at bottom"
  (hp-who-on-top "[z] view lowered to HWND_BOTTOM")
  (hp-sync))

(hp-step "z: after hp-sync (move with SWP_NOZORDER)"
  (hp-who-on-top "[z] view moved/shown again by hp-sync")
  (delete-frame hp-child)
  ;; 子フレームが先にある状態で view を作る
  (setq hp-child (hp-make-child)))

(hp-step "z: view created after child frame"
  (let ((id (hwnd-probe-create hp-parent "#1e90ff" "view C")))
    (pcase-let ((`(,l ,tp ,r ,b) (window-inside-pixel-edges (car (car (last hp-views))))))
      (hwnd-probe-move id l tp (- r l) (- b tp))
      (hwnd-probe-show id t))
    (setq hp-views (append hp-views (list (cons nil id))))))

(hp-step "z: result"
  (hp-who-on-top "[z] view C created after child frame")
  (hwnd-probe-destroy (cdr (car (last hp-views))))
  (setq hp-views (butlast hp-views))
  (delete-frame hp-child)
  (select-frame-set-input-focus hp-frame)
  (setq hp-next-delay 1.0))

(hp-step "focus before click"
  (hp-log "[focus before click] %s" (hp-focus-str 0))
  (hp-log "  click A: %S" (hwnd-probe-click 0)))

(hp-step "focus after click"
  (hp-log "[focus after click] %s" (hp-focus-str 0))
  (hp-log "  info A (paints clicks mouseact setfocus) = %S" (hwnd-probe-info 0))
  (hp-log "  selected-frame is probe frame: %S  frame-focus-state=%S"
          (eq (selected-frame) hp-frame) (frame-focus-state hp-frame)))

(hp-step "key baseline"
  (setq hp-f24-times nil hp-t0 (float-time))
  (hp-log "[key baseline] sent=%S" (hwnd-probe-key hp-parent))
  (setq hp-next-delay 1.0))

(hp-step "key baseline result"
  (hp-log "  F24 latency: %s"
          (if hp-f24-times (format "%.1f ms" (* 1000 (- (car hp-f24-times) hp-t0))) "NOT RECEIVED")))

(hp-step "block, key only"
  (setq hp-f24-times nil)
  (hp-log "[UI thread blocked 2000ms, F24 only] block=%S" (hwnd-probe-block 2000))
  (sleep-for 0.05)
  (setq hp-t0 (float-time))
  (hp-log "  sent=%S  ping(emacs GUI thread)=%S ms" (hwnd-probe-key hp-parent) (hwnd-probe-ping hp-parent))
  (setq hp-next-delay 3.0))

(hp-step "block, key only result"
  (hp-log "  F24 latency: %s"
          (if hp-f24-times (format "%.1f ms" (* 1000 (- (car hp-f24-times) hp-t0))) "NOT RECEIVED")))

(hp-step "block, no key (control: does Lisp/redisplay keep running?)"
  (setq hp-f24-times nil)
  (hp-log "[UI thread blocked 2000ms, nothing sent] block=%S" (hwnd-probe-block 2000))
  (setq hp-t0 (float-time))
  (run-at-time 0.3 nil (lambda () (hp-log "  timer fired after %.1f ms"
                                          (* 1000 (- (float-time) hp-t0)))))
  (setq hp-next-delay 3.0))

(hp-step "block, Lisp-side move"
  (hp-log "[UI thread blocked 2000ms, move from Lisp] block=%S" (hwnd-probe-block 2000))
  (sleep-for 0.05)
  (let ((t0 (float-time)))
    (hp-log "  move => %S after %.1f ms (Lisp thread waits for the UI thread)"
            (hwnd-probe-move 0 0 0 10 10) (* 1000 (- (float-time) t0))))
  (setq hp-next-delay 3.0))

(hp-step "restore"
  (hp-sync))

(hp-step "block, click then key"
  (setq hp-f24-times nil)
  (hp-log "[UI thread blocked 2000ms, click on view then F24] block=%S" (hwnd-probe-block 2000))
  (sleep-for 0.05)
  (hp-log "  click: %S" (hwnd-probe-click 0))
  (setq hp-t0 (float-time))
  (hp-log "  sent=%S  ping=%S ms" (hwnd-probe-key hp-parent) (hwnd-probe-ping hp-parent))
  (setq hp-next-delay 3.0))

(hp-step "block, click then key result"
  (hp-log "  F24 latency: %s"
          (if hp-f24-times (format "%.1f ms" (* 1000 (- (car hp-f24-times) hp-t0))) "NOT RECEIVED"))
  (hp-log "  info A = %S" (hwnd-probe-info 0))
  (hp-log "  %s" (hp-focus-str 0)))

(setq hp-steps (nreverse hp-steps))

(defun hp-run ()
  (if (null hp-steps)
      (hp-finish)
    (let ((s (pop hp-steps)))
      (setq hp-next-delay nil)
      (condition-case e
          (funcall (cdr s))
        (error (hp-log "[%s] ERROR %S" (car s) e)))
      (run-at-time (or hp-next-delay 0.5) nil #'hp-run))))

(run-at-time 1.0 nil #'hp-run)
