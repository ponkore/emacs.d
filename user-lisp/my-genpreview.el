;;; my-genpreview.el --- GenPreview でファイルをプレビューする  -*- lexical-binding: t -*-
;;; Commentary:
;; GenPreview (~/Projects/GenPreview) の Emacs 連携。Windows のみ。
;;
;; 画像 (HEIC を含む)、SVG、PDF、Markdown、ソースコードを、Direct2D で描いた
;; 画像として Emacs のバッファに表示する。描画は dynamic module
;; (genpreview_module.dll) が行い、Emacs には PNG が渡る。
;;
;;   M-x genpreview-open           ファイルを開いてプレビューする
;;   M-x genpreview-side-preview   今のバッファのファイルを横に出す (保存で更新)
;;   M-x genpreview-dired-preview  dired のポイント上のファイルを出す
;;
;; 配布物は `cargo xtask dist' の出力を `my:genpreview-directory' に置く。
;; genpreview.el は同じディレクトリの genpreview_module.dll を読み、DLL は
;; 同梱の pdfium.dll / heif.dll を自分の隣から読む。初めてプレビューするときに
;; 読み込まれるので、ここでは autoload を張るだけにしてある。
;;
;; use-package を使っていないのは、`:load-path' が `:if' より前に処理される
;; ため (use-package-core.el の `use-package-keywords')。この設定は
;; Windows / macOS / Linux で共有しているので、Windows 以外で
;; "C:/Apps/genpreview/" が load-path に入るのは避ける。
;;
;; 【重要】Emacs は module を unload できず、読み込み中の DLL はロックされる。
;; genpreview.el はバージョン付きの名前でコピーしてから読み込むので、Emacs を
;; 動かしたまま dist を上書きできるが、新しい版に入れ替わるのは再起動のとき。

;;; Code:

(defcustom my:genpreview-directory "C:/Apps/genpreview/"
  "GenPreview の配布物を入れたディレクトリ。
genpreview.el、genpreview_module.dll、同梱 DLL (pdfium / heif / libde265) を置く。"
  :type 'directory)

(defun my:genpreview-available-p ()
  "GenPreview が使えるか。"
  (and (eq system-type 'windows-nt)
       (file-exists-p (expand-file-name "genpreview.el" my:genpreview-directory))))

;;; --------------------------------------------------
;;; dired の v からプレビューする
;;; --------------------------------------------------

;; defcustom と defun は Windows 以外でも定義する。
;; `my:genpreview-dired-view-file' は GenPreview が無ければ素の
;; `dired-view-file' に落ちるので、my-dired.el の hydra (. v) から
;; 無条件に参照してよい。

(defcustom my:genpreview-extensions
  '(;; 画像 (WIC。RAW を含む)
    "png" "jpg" "jpeg" "jpe" "jfif" "gif" "bmp" "dib" "tif" "tiff" "ico"
    "webp" "avif" "jxr" "wdp" "dng" "cr2" "cr3" "nef" "arw" "orf" "rw2"
    ;; HEIF / HEIC (heif.dll)
    "heic" "heif" "hif"
    ;; SVG
    "svg" "svgz"
    ;; PDF (pdfium.dll)
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
  "dired の `v'。GenPreview が扱える形式ならプレビューする。

`my:genpreview-extensions' に載っている拡張子なら
`genpreview-dired-preview' に渡す。それ以外 (ディレクトリ、ソースコード、
GenPreview が無い環境) は素の `dired-view-file'。

\\[universal-argument] を付けると PLAIN が non-nil になり、常に
`dired-view-file' で開く。CSV や Markdown を `view-mode' の中で
検索したいときの逃げ道。"
  (interactive "P")
  (if (and (not plain)
           (my:genpreview-previewable-p (dired-get-file-for-visit)))
      (genpreview-dired-preview)
    (dired-view-file)))

;;; --------------------------------------------------
;;; 有効化 (Windows で配布物があるときだけ)
;;; --------------------------------------------------

(when (my:genpreview-available-p)
  (add-to-list 'load-path (directory-file-name my:genpreview-directory))
  (autoload 'genpreview-open "genpreview" "ファイルを開いてプレビューする。" t)
  (autoload 'genpreview-side-preview "genpreview" "今のバッファのファイルを横にプレビューする。" t)
  (autoload 'genpreview-dired-preview "genpreview" "dired のポイントのファイルをプレビューする。" t)
  ;; dired の v (view) を乗せ替える。
  ;;
  ;; dired-x は v を触らないので (奪うのは V / F / M-! / M-( / C-x M-o)、
  ;; my-dired.el の V のようにロード順を気にしなくてよい。
  ;; `dired-sidebar-mode-map' は `dired-mode-map' を継承するので
  ;; サイドバー (F8) でもそのまま効く。
  (with-eval-after-load 'dired
    (define-key dired-mode-map "v" #'my:genpreview-dired-view-file)))

(provide 'my-genpreview)
;;; my-genpreview.el ends here
