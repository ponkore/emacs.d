#!/usr/bin/env python3
# init.el を user-lisp/ 配下のモジュールへ機械的に分割する。
# 行範囲は init.el のセクション見出しに合わせて決めてある（順序は元のまま）。
import io, os

SRC = 'init.el'
DST = 'user-lisp'
NL = '\r\n'

# (module, 開始行, 終了行, 説明)  ※1-indexed / 両端含む
MODULES = [
    ('my-core',        89,  111, '汎用ヘルパと基礎ライブラリ'),
    ('my-japanese',   119,  297, '日本語環境 (encoding / IME / migemo)'),
    ('my-appearance', 298,  633, 'フォント・フレーム・テーマ・モードライン'),
    ('my-completion', 634,  903, '補完 (vertico / consult / company など)'),
    ('my-keybind',    904,  946, 'グローバルキーバインド'),
    ('my-editor',     947, 1321, 'エディタ全般の設定'),
    ('my-dired',     1322, 1453, 'dired と neotree'),
    ('my-text',      1454, 1707, 'テキストモード (org / markdown / rst / adoc)'),
    ('my-lang-lisp', 1708, 1831, 'Lisp 系 (Emacs Lisp / Clojure / Common Lisp)'),
    ('my-lang-python',1832, 1896, 'Python'),
    ('my-lang-web',  1897, 2069, 'Web 系 (PHP / JavaScript / TypeScript)'),
    ('my-lang-native',2070, 2140, 'ネイティブ系 (Rust / C++ / C#)'),
    ('my-lang-misc', 2141, 2294, 'その他の言語 (SQL / bat / Swift / Lua / VB)'),
    ('my-lsp',       2295, 2345, 'LSP と flycheck'),
    ('my-fileformat',2346, 2400, '特定ファイルフォーマット'),
    ('my-project',   2401, 2440, 'プロジェクト管理 (projectile)'),
    ('my-vc',        2441, 2522, '構成管理 (magit / git-gutter / SVN)'),
    ('my-shell',     2523, 2585, 'Shell 関連'),
    ('my-utils',     2586, 2858, 'ユーティリティ'),
    ('my-platform',  2859, 2920, 'OS 固有設定 (Windows / macOS)'),
]

# init.el に残す範囲（ブートストラップ部と custom.el 読み込み）
KEEP_HEAD = (1, 88)
KEEP_CUSTOM = (112, 118)

lines = io.open(SRC, encoding='utf-8', newline='').read().split(NL)


def seg(a, b):
    return lines[a - 1:b]


os.makedirs(DST, exist_ok=True)

# --- 各モジュールを書き出す ---
covered = set()
for name, a, b, desc in MODULES:
    body = seg(a, b)
    # 前後の空行を落とす
    while body and not body[0].strip():
        body.pop(0)
    while body and not body[-1].strip():
        body.pop()
    header = [
        ';;; %s.el --- %s  -*- lexical-binding: nil -*-' % (name, desc),
        ';;; Commentary:',
        ';; init.el から機械的に分割したもの。読み込み順は init.el を参照。',
        ';; lexical-binding は分割前と同じ意味論を保つため nil のまま。',
        ';;; Code:',
        '',
    ]
    footer = ['', "(provide '%s)" % name, ';;; %s.el ends here' % name]
    out = NL.join(header + body + footer) + NL
    io.open(os.path.join(DST, name + '.el'), 'w', encoding='utf-8', newline='').write(out)
    covered.update(range(a, b + 1))
    print('  %-18s lines %5d-%-5d (%4d lines)' % (name, a, b, b - a + 1))

# --- init.el を組み立て直す ---
requires = [
    ';; user-lisp/ 配下のモジュールを読み込む。',
    ';; 順序は分割前の init.el の記述順と同じ。',
]
for name, a, b, desc in MODULES:
    if name == 'my-core':
        continue
    requires.append("(require '%s)   ; %s" % (name, desc))

# 元の記述順を厳密に保つため、my-core は custom.el の読み込みより前に置く
# (分割前は 89-111 行が leaf s とヘルパ、112-118 行が custom.el だった)
new_init = (
    seg(*KEEP_HEAD)
    + [
        '',
        ';;; --------------------------------------------------',
        ';;; user-lisp/ の読み込み',
        ';;; --------------------------------------------------',
        '',
        ';; early-init.el で user-lisp-auto-scrape を nil にしてある。',
        ';; 既定では straight のブートストラップ前に prepare-user-lisp が走り、',
        ';; leaf マクロが未定義のままバイトコンパイルされて壊れた .elc ができるため。',
        ';; ここまでで straight と leaf が使える状態になっているので明示的に呼ぶ。',
        ';; user-lisp/ 配下は再帰的にバイトコンパイルされ load-path に追加される。',
        '(prepare-user-lisp)',
        '',
        "(require 'my-core)   ; 汎用ヘルパと基礎ライブラリ",
        '',
    ]
    + seg(*KEEP_CUSTOM)
    + ['']
    + requires
    + [
        '',
        ';;; --------------------------------------------------',
        ';;; end',
        ';;; --------------------------------------------------',
        '',
        "(provide 'init)",
        ';;; init.el ends here',
    ]
)
io.open(SRC, 'w', encoding='utf-8', newline='').write(NL.join(new_init) + NL)

# --- 網羅チェック（取りこぼした行がないか） ---
allrange = set(range(1, len(lines) + 1))
kept = set(range(KEEP_HEAD[0], KEEP_HEAD[1] + 1)) | set(range(KEEP_CUSTOM[0], KEEP_CUSTOM[1] + 1))
missing = sorted(allrange - covered - kept)
# 末尾の provide/ends here と空行・セクション見出しは意図的に落としている
print('\n未割り当ての行番号:', missing[:20], '...' if len(missing) > 20 else '')
print('未割り当て行数:', len(missing))
for n in missing[:30]:
    print('   %5d: %s' % (n, lines[n - 1]))
