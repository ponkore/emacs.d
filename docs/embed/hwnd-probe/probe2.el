;;; probe2.el --- 第 2 ラウンド: DPI / ホイールとドラッグ / 実際の設定  -*- lexical-binding: t -*-
;;
;; emacs.exe -l probe2.el (init.el を読む) で起動する。
;; 終了時は kill-emacs-hook を空にして recentf / history を書き戻さない。

(defvar hp-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar hp-log nil)
(defvar hp-parent nil)
(defvar hp-frame nil)
(defvar hp-buf nil)
(defvar hp-views nil "((WINDOW . ID) ...)")
(defvar hp-steps nil)
(defvar hp-next-delay nil)
(defvar hp-sync-count 0)
(defvar hp-mons nil)
(defvar hp-emacs-wheel 0)
(defvar hp-ws nil)
(defvar hp-saved-w nil)

(defun hp-log (fmt &rest args)
  (push (apply #'format fmt args) hp-log))

(defun hp-finish ()
  (ignore-errors
    (dolist (c hp-views) (hwnd-probe-destroy (cdr c)))
    (hwnd-probe-topmost hp-parent nil))
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (expand-file-name "out2.txt" hp-dir)
      (insert (mapconcat #'identity (reverse hp-log) "\n") "\n")))
  (setq kill-emacs-hook nil)
  (kill-emacs 0))

(run-at-time 180 nil (lambda () (hp-log "WATCHDOG") (hp-finish)))

(add-hook 'pre-command-hook
          (lambda ()
            (when (memq (event-basic-type last-input-event)
                        '(wheel-up wheel-down mouse-4 mouse-5))
              (setq hp-emacs-wheel (1+ hp-emacs-wheel)))))

(defun hp-sync (&optional _frame)
  (setq hp-sync-count (1+ hp-sync-count))
  (dolist (c hp-views)
    (let ((w (car c)) (id (cdr c)))
      (if (and (window-live-p w) (eq (window-buffer w) hp-buf)
               (eq (window-frame w) hp-frame))
          (pcase-let ((`(,l ,tp ,r ,b) (window-inside-pixel-edges w)))
            (hwnd-probe-move id l tp (- r l) (- b tp))
            (hwnd-probe-show id t))
        (hwnd-probe-show id nil)))))

(add-hook 'window-size-change-functions #'hp-sync)
(add-hook 'window-buffer-change-functions #'hp-sync)

(defun hp-px (x y) (hwnd-probe-pixel hp-parent x y))

(defun hp-sample (w tag)
  (pcase-let ((`(,l ,tp ,r ,b) (window-inside-pixel-edges w)))
    (hp-log "%s edges=%S" tag (list l tp r b))
    (hp-log "  inside : exact TL=%s BR=%s | +2px TL=%s BR=%s"
            (hp-px l tp) (hp-px (1- r) (1- b))
            (hp-px (+ l 2) (+ tp 2)) (hp-px (- r 3) (- b 3)))
    (hp-log "  outside: exact L=%s T=%s R=%s B=%s | -2px L=%s T=%s R=%s B=%s"
            (hp-px (1- l) (+ tp 5)) (hp-px (+ l 5) (1- tp))
            (hp-px r (+ tp 5)) (hp-px (+ l 5) b)
            (hp-px (- l 3) (+ tp 5)) (hp-px (+ l 5) (- tp 3))
            (hp-px (+ r 2) (+ tp 5)) (hp-px (+ l 5) (+ b 2)))))

(defun hp-capture (name)
  (hwnd-probe-capture hp-parent (expand-file-name name hp-dir)))

(defun hp-view-id () (cdr (car (last hp-views))))
(defun hp-view-window () (car (car (last hp-views))))

(defmacro hp-step (name &rest body)
  `(push (cons ,name (lambda () ,@body)) hp-steps))

(defun hp-find-mon (pw dpi)
  ;; system-aware のスレッドでは GetDpiForMonitor も仮想化されて 96 が返るので、
  ;; 物理幅で探し、倍率は物理幅 / 論理幅で確かめる
  (let ((m (seq-find (lambda (m) (= (nth 6 m) pw)) hp-mons)))
    (when m
      (hp-log "  monitor scale = %.2f (expected %.2f)"
              (/ (float (nth 6 m)) (nth 2 m)) (/ dpi 96.0)))
    m))

;;; ---------- 準備 ----------

(hp-step "setup"
  (setq hp-frame (selected-frame))
  (delete-other-windows)
  (setq hp-buf (get-buffer-create "*probe*"))
  (switch-to-buffer hp-buf)
  (dotimes (i 400) (insert (format "line %03d  the quick brown fox jumps over the lazy dog\n" i)))
  (goto-char (point-min))
  (setq hp-parent (string-to-number (frame-parameter hp-frame 'window-id)))
  (hp-log "emacs=%s  init=%s  tab-bar-mode=%S  corfu=%S  doom-modeline=%S"
          emacs-version user-init-file (bound-and-true-p tab-bar-mode)
          (bound-and-true-p global-corfu-mode) (bound-and-true-p doom-modeline-mode))
  (hp-log "load: %S" (module-load (expand-file-name "hwnd-probe.dll" hp-dir)))
  (setq hp-mons (hwnd-probe-monitors))
  (dolist (m hp-mons)
    (hp-log "monitor: logical=%S physical=%S dpi=%S primary=%S"
            (seq-take m 4) (seq-subseq m 4 8) (nth 8 m) (nth 9 m)))
  (hp-log "topmost: %S" (hwnd-probe-topmost hp-parent t)))

;;; ---------- 1 枚のディスプレイでやること ----------

(defun hp-display-steps (label pw dpi)
  (hp-step (format "%s: move frame" label)
    (let ((m (hp-find-mon pw dpi)))
      (if (not m)
          (hp-log "[%s] monitor %dx? dpi=%d NOT FOUND" label pw dpi)
        (set-frame-position hp-frame (+ (nth 0 m) 100) (+ (nth 1 m) 100))
        (set-frame-size hp-frame 100 34)
        (unless hp-views
          (push (cons (selected-window)
                      (hwnd-probe-create hp-parent "#1e90ff" "view A"))
                hp-views))
        (hp-sync)))
    (setq hp-next-delay 1.5))

  (hp-step (format "%s: sample" label)
    (hp-log "[%s] env (awareness dpi root-p cw ch sx sy) = %S"
            label (hwnd-probe-env hp-parent))
    (hp-log "[%s] frame-position=%S  frame-geometry tab-bar=%S tool-bar=%S"
            label (frame-position hp-frame)
            (alist-get 'tab-bar-size (frame-geometry hp-frame))
            (alist-get 'tool-bar-size (frame-geometry hp-frame)))
    (hp-sample (hp-view-window) (format "[%s]" label))
    (hp-capture (format "%s.bmp" label)))

  (hp-step (format "%s: wheel" label)
    (setq hp-ws (window-start (hp-view-window))
          hp-emacs-wheel 0)
    (hp-log "[%s] wheel 3 notches down: %S" label (hwnd-probe-wheel (hp-view-id) 3))
    (setq hp-next-delay 0.8))

  (hp-step (format "%s: wheel result" label)
    (let ((i (hwnd-probe-info (hp-view-id))))
      (hp-log "  view: wheel msgs=%d delta=%d | emacs: wheel events=%d window-start %s"
              (nth 4 i) (nth 5 i) hp-emacs-wheel
              (if (= hp-ws (window-start (hp-view-window))) "unchanged" "CHANGED"))))

  (hp-step (format "%s: drag" label)
    (pcase-let ((`(,_l ,tp ,_r ,b) (window-inside-pixel-edges (hp-view-window))))
      ;; 中央から view の下端を 80px 越えるところまで
      (hp-log "[%s] drag down past the bottom edge: %S" label
              (hwnd-probe-drag (hp-view-id) 0 (+ (/ (- b tp) 2) 80))))
    (setq hp-next-delay 0.8))

  (hp-step (format "%s: drag result" label)
    (let ((i (hwnd-probe-info (hp-view-id))))
      (hp-log "  view: clicks=%d captured moves=%d (outside view=%d) setfocus=%d"
              (nth 1 i) (nth 6 i) (nth 7 i) (nth 3 i))
      (hp-log "  emacs: selected-window is view window=%S  frame-focus-state=%S"
              (eq (selected-window) (hp-view-window)) (frame-focus-state hp-frame)))))

(hp-display-steps "D2-100pct" 2560 96)
(hp-display-steps "D3-150pct" 3840 144)
(hp-display-steps "D2-back" 2560 96)

;;; ---------- tab-bar ----------

(hp-step "tab: new tab"
  (setq hp-sync-count 0
        hp-saved-w (hp-view-window))
  (tab-bar-new-tab)
  (setq hp-next-delay 0.8))

(hp-step "tab: sample new tab"
  (pcase-let ((`(,l ,tp ,_r ,_b) (window-inside-pixel-edges (selected-window))))
    (hp-log "[tab] new tab: sync calls=%d  old window live=%S  new tab buffer=%s  pixel=%s"
            hp-sync-count (window-live-p hp-saved-w)
            (buffer-name (window-buffer (selected-window)))
            (hp-px (+ l 5) (+ tp 5))))
  (hp-capture "tab-new.bmp")
  (setq hp-sync-count 0)
  (tab-bar-switch-to-prev-tab)
  (setq hp-next-delay 0.8))

(hp-step "tab: sample back"
  (hp-log "[tab] back: sync calls=%d  old window live=%S  same object=%S"
          hp-sync-count (window-live-p hp-saved-w)
          (eq (selected-window) hp-saved-w))
  (hp-sample (hp-view-window) "[tab back]"))

;;; ---------- 最小化 ----------

(hp-step "iconify"
  (iconify-frame hp-frame)
  (setq hp-next-delay 1.0))

(hp-step "iconified"
  (hp-log "[iconify] frame-visible-p=%S" (frame-visible-p hp-frame))
  (make-frame-visible hp-frame)
  (hwnd-probe-topmost hp-parent t)
  (setq hp-next-delay 1.2))

(hp-step "restored"
  (hp-log "[restore] frame-visible-p=%S" (frame-visible-p hp-frame))
  (hp-sample (hp-view-window) "[restored]")
  (select-frame-set-input-focus hp-frame))

;;; ---------- corfu の本物のポップアップ ----------

(hp-step "corfu: layout"
  ;; 上: *scratch* (補完する)、下: *probe* (view)
  (let* ((top (hp-view-window))
         (bottom (split-window-below)))
    (set-window-buffer top (get-buffer-create "*scratch*"))
    (set-window-buffer bottom hp-buf)
    (dolist (c hp-views) (hwnd-probe-destroy (cdr c)))   ; A は以後使わない
    (setq hp-views (list (cons bottom (hwnd-probe-create hp-parent "#1e90ff" "view B"))))
    (hp-sync)
    (select-window top)
    (with-current-buffer "*scratch*"
      (unless (derived-mode-p 'lisp-interaction-mode) (lisp-interaction-mode))
      (goto-char (point-max))
      (insert "\n(with-"))
    ;; 補完する行を上のウィンドウの最下行にして、ポップアップが下 (view) に
    ;; はみ出すようにする
    (recenter -1))
  (setq hp-next-delay 0.6))

(hp-step "corfu: complete"
  (let ((this-command 'completion-at-point))
    (condition-case e
        (progn (completion-at-point)
               (run-hooks 'post-command-hook))
      (error (hp-log "[corfu] completion error %S" e))))
  (setq hp-next-delay 1.0))

(hp-step "corfu: sample"
  (let ((cf (seq-find (lambda (f) (and (eq (frame-parent f) hp-frame)
                                       (frame-visible-p f)))
                      (frame-list))))
    (if (not cf)
        (hp-log "[corfu] no visible child frame (popup did not appear)")
      (pcase-let* ((`(,x . ,y) (frame-position cf))
                   (w (frame-pixel-width cf)) (h (frame-pixel-height cf))
                   (`(,_l ,vt ,_r ,vb) (window-inside-pixel-edges (hp-view-window)))
                   (sy (max (+ y 3) (+ vt 3))))
        (hp-log "[corfu] popup at (%d,%d) %dx%d  view top=%d bottom=%d  overlaps view=%S"
                x y w h vt vb (and (< vt (+ y h)) (> vb y)))
        (hp-log "[corfu] pixel inside popup over the view (%d,%d)=%s  (view=#1e90ff)"
                (+ x (/ w 2)) sy (hp-px (+ x (/ w 2)) sy)))))
  (hp-capture "corfu.bmp")
  (ignore-errors (corfu-quit)))

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

(run-at-time 3.0 nil #'hp-run)
