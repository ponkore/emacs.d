#!/usr/bin/env bash
# 起動中の Emacs (server) に S 式を評価させ、結果を UTF-8 で標準出力に返す。
#
#   ec.sh '(式)'      式を評価して結果を出す
#   ec.sh -f FILE     FILE 内の全ての式を順に評価し、最後の値を出す
#   ec.sh -l FILE     FILE を load-file する（モジュールの再読込）
#   ec.sh -n ...      ガード (inhibit-interaction) を外す
#
# 既定では式を (let ((inhibit-interaction t)) ...) で包む。ミニバッファを
# 開こうとする式はハングせず inhibited-interaction エラーになる。
# エラーは emacsclient がそのまま `*ERROR*: ...` として返し、終了コードは 1。
set -u

guard=1
mode=expr
arg=
have_arg=0

while [ $# -gt 0 ]; do
  case "$1" in
    -n|--no-guard) guard=0; shift ;;
    -f|--file)     mode=file; arg=${2-}; have_arg=1; shift 2 ;;
    -l|--load)     mode=load; arg=${2-}; have_arg=1; shift 2 ;;
    -h|--help)     sed -n '2,11p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    --)            shift; arg=${1-}; have_arg=1; shift ;;
    *)             arg=$1; have_arg=1; shift ;;
  esac
done

if [ "$have_arg" = 0 ] || [ -z "$arg" ]; then
  echo "ec.sh: 式またはファイルを指定してください (-h でヘルプ)" >&2
  exit 2
fi

# emacsclient を見つける。EMACSCLIENT で明示的に上書きできる。
client=${EMACSCLIENT:-}
if [ -z "$client" ]; then
  for c in emacsclient \
           "C:/Apps/emacs/emacs-31.1/bin/emacsclient.exe" \
           "/c/Apps/emacs/emacs-31.1/bin/emacsclient.exe"; do
    if command -v "$c" >/dev/null 2>&1; then client=$c; break; fi
  done
fi
if [ -z "$client" ]; then
  echo "ec.sh: emacsclient が見つかりません。EMACSCLIENT に絶対パスを設定してください" >&2
  exit 127
fi

# Emacs は MSYS 形式のパス (/c/...) を解釈しないので Windows 形式に直す。
winpath() {
  if command -v cygpath >/dev/null 2>&1; then
    cygpath -m -- "$1"
  else
    printf '%s' "$1"
  fi
}

case "$mode" in
  expr) body=$arg ;;
  load)
    [ -f "$arg" ] || { echo "ec.sh: ファイルがありません: $arg" >&2; exit 2; }
    body="(load-file \"$(winpath "$arg")\")" ;;
  file)
    [ -f "$arg" ] || { echo "ec.sh: ファイルがありません: $arg" >&2; exit 2; }
    body="(with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents \"$(winpath "$arg")\"))
             (goto-char (point-min))
             (let ((v nil))
               (condition-case nil
                   (while t (setq v (eval (read (current-buffer)) t)))
                 (end-of-file v))))" ;;
esac

[ "$guard" = 1 ] && body="(let ((inhibit-interaction t)) $body)"

out=$("$client" -e "$body" 2>&1); rc=$?

# Windows では結果が server 接続プロセスの coding (cp932) で返るので UTF-8 に
# 直して出す。mac / Linux は最初から UTF-8。変換すると、たまたま cp932 として
# 読めるバイト列（"ア" など）がエラーにならずに化ける。
conv=$(printf '%s' "$out" | tr -d '\r')
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    conv=$(printf '%s' "$out" | tr -d '\r' | iconv -f CP932 -t UTF-8 2>/dev/null) \
      || conv=$(printf '%s' "$out" | tr -d '\r') ;;
esac

if [ "$rc" != 0 ]; then
  printf '%s\n' "$conv" >&2
  case "$conv" in
    *"can't find socket"*|*"No such file or directory"*|*"connect"*)
      echo "ec.sh: Emacs server に繋がりません。Emacs で M-x server-start してください" >&2 ;;
  esac
  exit "$rc"
fi

printf '%s\n' "$conv"
