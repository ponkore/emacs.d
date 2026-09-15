<!-- -*- gfm -*- -->

# straight のメンテナンス

更新状況の棚卸し、不要なパッケージの掃除、更新の手順。straight は自動更新しない。

最終更新: 2026-09-15 ｜ [README.md](../../README.md) ｜ [CLAUDE.md](../../CLAUDE.md)

## 更新状況の棚卸し

`straight/repos/*` を一括で fetch して、手元と upstream の差を見る:

```sh
cd ~/.emacs.d/straight/repos
for d in */; do r="${d%/}"
  case " melpa gnu-elpa-mirror nongnu-elpa emacsmirror-mirror el-get straight.el " in
    *" $r "*) continue;; esac
  ( cd "$r" && git fetch -q origin && git remote set-head origin -a >/dev/null
    up=$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD)
    printf '%-24s behind=%s
' "$r" "$(git rev-list --count HEAD..$up)" )
done
```

更新したあとは **`straight/build` をまるごと消してから起動する**。
straight の変更検知は当てにならない（corfu / doom-modeline で取りこぼした実績あり）。
再ビルドは GUI 起動で数分かかる。

2026-08 の棚卸しでは 93 個中 50 個が遅れていた。**全 50 個を更新済み**
（段階 1〜4 に分けて、各段階で GUI 起動して検証した）。

更新の過程で、設定側の非互換が 2 件と、更新とは無関係の既存バグが 3 件見つかった。
棚卸しは「古いまま放置していると壊れているのに気づけない」ことの確認になった。

## org のクローンは使っていない

`init.el` で `(org :type built-in)` と宣言しているので Emacs 同梱の org を使う。
`straight/repos/org` と `straight/build/org` があっても `load-path` には載らない。
recipe cache には残るため `straight-prune-build` では消えないので、手で消す。
（2026-08 に削除。合わせて 120 MB あった）

## 不要になったパッケージの掃除

```elisp
(straight-prune-build)             ; 今のセッションで使われていない build/ を消す
(straight-remove-unused-repos t)   ; どのビルドからも参照されない repos/ を消す
```

**GUI で起動してから実行すること。** batch では `:if window-system` の
パッケージ（doom-modeline、org-bullets など）が登録されず、
使用中のものまで削除対象になる。
OS 判定で外れるもの（`exec-path-from-shell` は macOS / Linux 専用）も同様に
消えるが、`straight/` は git 管理外なので他マシンには影響しない。

## 更新の手順

straight は自動更新しない。追従が必要なときは：

```elisp
(straight-pull-recipe-repositories)  ; レシピ定義（melpa 等）を更新
(straight-pull-package "NAME")       ; 個別パッケージを更新
```

レシピリポジトリを更新しても、**すでに clone 済みのパッケージ本体は古いまま**
であることに注意。`straight/repos/NAME` の HEAD は clone 時点で止まる。
2026-08 時点で vertico / consult / marginalia / orderless などは
まだ 2021 年のままになっている。

パッケージ本体を更新したあとは **`straight/build/NAME` を消してから起動**する。
straight の変更検知はこれを取りこぼすことがあり、`straight-rebuild-package` でも
再ビルドされない場合がある (corfu の extensions がコピーされない事例があった)。

過去に **レシピリポジトリと straight.el 本体が 2021 年で凍結**しており、
それが「新しいバージョンに追従できていない」原因になっていた。
upstream がデフォルトブランチを `master` → `main` に変えている場合は
`straight/repos/NAME` で手動チェックアウトが必要になることがある（magit で発生）。

