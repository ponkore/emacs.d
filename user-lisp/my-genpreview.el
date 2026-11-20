;;; my-genpreview.el --- GenPreview でファイルをプレビューする  -*- lexical-binding: t -*-
;;; Commentary:
;; GenPreview (`my:genpreview-source-directory') の Emacs 連携。Windows と macOS。
;;
;; 画像 (HEIC を含む)、SVG、PDF、Markdown、CSV を、OS の描画 API で描いた面を
;; Emacs のウィンドウに重ねて見せる。描画・スクロール・選択・検索の強調は
;; dynamic module が持ち、Emacs はキーと isearch だけを受け持つ
;; (GenPreview の ADR 0008)。OS で違うのは次の 3 点だけで、どちらも
;; genpreview.el 自身が吸収している (こちらは配布物を写して autoload を張るだけ)。
;;
;;   | | Windows | macOS |
;;   |---|---|---|
;;   | 重ねるもの | 子 HWND (Direct2D) | CALayer を EmacsView に足す (CoreGraphics) |
;;   | module     | genpreview_module.dll | libgenpreview_module.dylib |
;;   | 同梱物     | pdfium.dll + heif.dll + libde265.dll | libpdfium.dylib のみ (HEIC は ImageIO) |
;;
;;   M-x genpreview-open           ファイルを開いてプレビューする
;;   M-x genpreview-side-preview   今のバッファのファイルを横に出す (保存で更新)
;;   M-x genpreview-dired-preview  dired のポイント上のファイルを出す
;;
;;   dired の v                    絵として見たい形式を GenPreview に回す
;;                                 (`my:genpreview-dired-view-file')
;;
;; 配布物は `M-x my:genpreview-update' で入れる。GenPreview のリポジトリ
;; (`my:genpreview-source-directory') で `cargo xtask dist' が作った
;; target/dist/genpreview/ を `my:genpreview-directory' に写す。C-u を付けると
;; 先に `cargo xtask dist' を走らせる。genpreview.el は同じディレクトリの
;; module (と pdfium) を読む。初めてプレビューするときに読み込まれるので、
;; ここでは autoload を張るだけにしてある (`my-embterm.el' と同じ形)。
;;
;; use-package を使っていないのは、`:load-path' が `:if' より前に処理される
;; ため (use-package-core.el の `use-package-keywords')。この設定は
;; Windows / macOS / Linux で共有しているので、使えない OS で配布物の場所が
;; load-path に入るのは避ける。
;;
;; 【重要】Emacs は module を unload できない。genpreview.el は module を
;; 版付きの名前で一時ディレクトリにコピーしてから読むので、Emacs を動かした
;; まま写し直せるが、**新しい module に入れ替わるのは Emacs を再起動したとき**。
;;
;; 【履歴】2026-11-20 に macOS へ移植された (GenPreview の ADR 0009)。
;; それまで macOS では dired の `v' を Preview.app に回していたが、
;; GenPreview が同じ形式を Emacs の中で開けるようになったので撤去した。
;; 外部アプリで開きたいときは dired の `&' (`dired-do-async-shell-command') で
;; `open -b com.apple.Preview' に渡す。`C-u v' なら素の `dired-view-file'。

;;; Code:

(eval-when-compile
  (require 'subr-x))

;; 下の autoload は配布物があるときだけ張るので、バイトコンパイラに教えておく
(declare-function genpreview-open "genpreview" (file))
(declare-function genpreview-side-preview "genpreview" ())
(declare-function genpreview-dired-preview "genpreview" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-view-file "dired" ())

(defcustom my:genpreview-directory
  (if (eq system-type 'windows-nt)
      "C:/Apps/genpreview/"
    (expand-file-name "genpreview/" user-emacs-directory))
  "GenPreview の配布物を入れたディレクトリ (git の管理外)。
genpreview.el と module (Windows は genpreview_module.dll + pdfium.dll +
heif.dll + libde265.dll、macOS は libgenpreview_module.dylib +
libpdfium.dylib) を置く。`my:genpreview-update' が
`my:genpreview-source-directory' の target/dist/genpreview/ から写す。"
  :type 'directory)

(defcustom my:genpreview-source-directory
  ;; Windows の C:/Projects は `~/Projects' のジャンクションでもあるが
  ;; (`my-platform.el')、無いマシンでも動くよう実体のほうを既定にする。
  (if (eq system-type 'windows-nt) "C:/Projects/GenPreview/" "~/Projects/GenPreview/")
  "GenPreview のリポジトリ。`my:genpreview-update' はここの
target/dist/genpreview/ を写す。"
  :type 'directory)

(defun my:genpreview-available-p ()
  "GenPreview が使えるか。
module があるのは Windows と macOS だけ (genpreview.el も同じ 2 つで分岐する)。"
  (and (memq system-type '(windows-nt darwin))
       module-file-suffix
       (file-exists-p (expand-file-name "genpreview.el" my:genpreview-directory))))

;;; --------------------------------------------------
;;; dired の v からプレビューする
;;; --------------------------------------------------

;; defcustom と defun は使えない OS でも定義する。
;; `my:genpreview-dired-view-file' は GenPreview が無ければ素の
;; `dired-view-file' に落ちるので、my-dired.el の hydra (. v) から
;; 無条件に参照してよい。

(defcustom my:genpreview-extensions
  '(;; 画像 (Windows は WIC、macOS は ImageIO。RAW を含む)
    "png" "jpg" "jpeg" "jpe" "jfif" "gif" "bmp" "dib" "tif" "tiff" "ico"
    "webp" "avif" "jxr" "wdp" "dng" "cr2" "cr3" "nef" "arw" "orf" "rw2"
    ;; HEIF / HEIC (Windows は同梱の heif.dll、macOS は ImageIO)
    "heic" "heif" "hif"
    ;; SVG (svgz = gzip も開ける)
    "svg" "svgz"
    ;; PDF (同梱の pdfium)
    "pdf"
    ;; Markdown
    "md" "markdown" "mdown" "mkd" "mkdn" "mdx"
    ;; 表
    "csv" "tsv")
  "dired の `v' を GenPreview に回す拡張子。ドット無し・小文字で書く。

GenPreview 側の各フォーマットの `extensions()' から取ってある。

**ソースコードとプレーンテキストは入れていない。** GenPreview はそれらも
開けるが (gp-format-text が txt / el / py / rs など数十の拡張子を持ち、
ハイライタが知っているものはさらに広い)、Emacs で読んだほうが速く、
`view-mode' のまま検索も移動もできる。ここに置くのは「バッファに
読み込んでも意味が取れない / 絵として見たい」形式だけにしてある。"
  :type '(repeat string))

(defun my:genpreview-previewable-p (file)
  "FILE を GenPreview に回すなら non-nil。

`my:genpreview-extensions' に載っている拡張子のファイルだけが対象。"
  ;; `genpreview-open' の autoload が張られているかどうかで、GenPreview が
  ;; 使えるかを見る。`my:genpreview-available-p' と違ってファイルを stat
  ;; しないので、キーを押すたびに呼んでよい。
  (and (fboundp 'genpreview-open)
       (let ((ext (file-name-extension file)))
         (and ext (member (downcase ext) my:genpreview-extensions)))
       ;; 拡張子が一致したときだけ stat する (f.md という名前のディレクトリ)。
       (not (file-directory-p file))
       t))

(defun my:genpreview-dired-view-file (&optional plain)
  "dired の `v'。絵として見たい形式を GenPreview に回す。

`my:genpreview-extensions' に載っている拡張子なら
`genpreview-dired-preview'。それ以外 (ディレクトリ、ソースコード、
GenPreview が無い環境) は素の `dired-view-file'。

\\[universal-argument] を付けると PLAIN が non-nil になり、常に
`dired-view-file' で開く。CSV や Markdown を `view-mode' の中で
検索したいときの逃げ道。

外部アプリ (Preview.app など) で開きたいときは dired の `&'
\(`dired-do-async-shell-command') に `open -b com.apple.Preview' を渡す。"
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (cond
     (plain (dired-view-file))
     ((my:genpreview-previewable-p file) (genpreview-dired-preview))
     (t (dired-view-file)))))

;;; --------------------------------------------------
;;; 配布物を入れる (M-x my:genpreview-update)
;;; --------------------------------------------------

(defun my:genpreview--dist-directory ()
  "GenPreview のリポジトリの配布物 (target/dist/genpreview/)。"
  (expand-file-name "target/dist/genpreview/" my:genpreview-source-directory))

(defun my:genpreview--same-file-p (a b)
  "ファイル A と B の中身が同じか。"
  (and (file-exists-p b)
       (= (file-attribute-size (file-attributes a))
          (file-attribute-size (file-attributes b)))
       (equal (with-temp-buffer (set-buffer-multibyte nil)
                                (insert-file-contents-literally a) (secure-hash 'sha1 (current-buffer)))
              (with-temp-buffer (set-buffer-multibyte nil)
                                (insert-file-contents-literally b) (secure-hash 'sha1 (current-buffer))))))

(defun my:genpreview--copy-dist ()
  "target/dist/genpreview/ を `my:genpreview-directory' に写す。
写したファイルの数などを返す。

中身が同じファイルは写さない。使用中 (Emacs が読み込んだ pdfium) で
上書きできないものは `<名前>.old-<時刻>' に名前を変えてから写す
\(`my-embterm.el' の `my:embterm--copy-dist' と同じ作り)。"
  (let* ((src (my:genpreview--dist-directory))
         (dst my:genpreview-directory)
         (stamp (format-time-string "%Y%m%d%H%M%S"))
         (copied 0) (same 0) (moved nil))
    (unless (file-exists-p (expand-file-name "genpreview.el" src))
      (user-error "GenPreview の配布物がありません: %s (`cargo xtask dist' か C-u M-x my:genpreview-update)" src))
    (make-directory dst t)
    ;; 前に退避したもの (使用中なら消せないので残す)
    (dolist (old (directory-files-recursively dst "\\.old-[0-9]+\\'"))
      (ignore-errors (delete-file old)))
    (dolist (file (directory-files-recursively src ""))
      (let ((to (expand-file-name (file-relative-name file src) dst)))
        (make-directory (file-name-directory to) t)
        (if (my:genpreview--same-file-p file to)
            (setq same (1+ same))
          (condition-case nil
              (copy-file file to t t)
            (file-error
             ;; 使用中は書けないが、名前は変えられる (Windows)
             (let ((aside (format "%s.old-%s" to stamp)))
               (rename-file to aside t)
               (copy-file file to t t)
               (push (file-name-nondirectory to) moved))))
          ;; 【重要】`copy-file' は PRESERVE-PERMISSIONS を渡さないと実行ビットを
          ;; 落とす。macOS では配布物の genpreview (単体の実行ファイル) が
          ;; 起動できなくなる。copy-file の第 6 引数は ACL まで写そうとして
          ;; 環境によっては失敗しうるので、モードだけ後から合わせる。
          (unless (eq system-type 'windows-nt)
            (set-file-modes to (file-modes file)))
          (setq copied (1+ copied)))))
    (list copied same moved)))

(defun my:genpreview-update (&optional build)
  "GenPreview の配布物を `my:genpreview-directory' に写す。
BUILD (前置引数) なら、先に `cargo xtask dist' を走らせてから写す
\(裏で動かし、終わったら写す。出力は *genpreview-dist* バッファ)。

新しい module に入れ替わるのは Emacs を再起動したとき。初めて入れたときは
その場で使えるようにする。"
  (interactive "P")
  (if (not build)
      (my:genpreview--update-1)
    ;; `~' のままだと子プロセスの作業ディレクトリに使えない
    (let ((default-directory (file-name-as-directory
                              (expand-file-name my:genpreview-source-directory)))
          (buf (get-buffer-create "*genpreview-dist*")))
      (with-current-buffer buf (let ((inhibit-read-only t)) (erase-buffer)))
      (message "GenPreview: cargo xtask dist を走らせています (*genpreview-dist*)...")
      (make-process
       :name "genpreview-dist" :buffer buf :noquery t
       :command '("cargo" "xtask" "dist")
       :sentinel (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (if (zerop (process-exit-status proc))
                         (my:genpreview--update-1)
                       (display-buffer buf)
                       (message "GenPreview: cargo xtask dist が失敗しました (*genpreview-dist*)"))))))))

(defun my:genpreview--update-1 ()
  "写して、結果を知らせる。"
  (let ((first (not (my:genpreview-available-p))))
    (pcase-let ((`(,copied ,same ,moved) (my:genpreview--copy-dist)))
      (when first (my:genpreview--activate))
      (message "GenPreview: %d 個写した (%d 個は同じ)%s%s"
               copied same
               (if moved (format "。使用中のものは退避して入れ替えた: %s" (string-join moved " ")) "")
               (cond (first "。使えるようになった")
                     ((and (> copied 0) (featurep 'genpreview)) "。新しい module は Emacs の再起動後に使われる")
                     (t ""))))))

;;; --------------------------------------------------
;;; 有効化 (配布物があるときだけ)
;;; --------------------------------------------------

(defun my:genpreview--activate ()
  "GenPreview の autoload を張る (配布物があるときだけ)。"
  (add-to-list 'load-path (directory-file-name my:genpreview-directory))
  (autoload 'genpreview-open "genpreview" "ファイルを開いてプレビューする。" t)
  (autoload 'genpreview-side-preview "genpreview" "今のバッファのファイルを横にプレビューする。" t)
  (autoload 'genpreview-dired-preview "genpreview" "dired のポイントのファイルをプレビューする。" t)
  ;; 表示は、単体ウィンドウ版のビューアを Emacs のウィンドウに重ねる方式が既定
  ;; (`genpreview-view'。GenPreview の ADR 0008、2026-11-10 から)。不調なら
  ;; (setq genpreview-view nil) で画像の短冊を貼る従来の表示に戻る。
  ;; genpreview.el はまだ読まれていないが、defcustom は既に値があれば上書きしない。
  )

(when (my:genpreview-available-p)
  (my:genpreview--activate))

;; dired の v (view) を乗せ替える。GenPreview が無い環境 (Linux、配布物が
;; 無いマシン) では素の `dired-view-file' のままにしておく。
;;
;; dired-x は v を触らないので (奪うのは V / F / M-! / M-( / C-x M-o)、
;; my-dired.el の V のようにロード順を気にしなくてよい。
;; `dired-sidebar-mode-map' は `dired-mode-map' を継承するので
;; サイドバー (F8) でもそのまま効く。
(when (my:genpreview-available-p)
  (with-eval-after-load 'dired
    (define-key dired-mode-map "v" #'my:genpreview-dired-view-file)))

(provide 'my-genpreview)
;;; my-genpreview.el ends here
