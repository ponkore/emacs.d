# ideco DB (PostgreSQL) の MCP サーバを起こすラッパ。.mcp.json から呼ばれる。
#
# パスワードはリポジトリにもレジストリにも置かない。my-ideco.el と同じく
# ~/.authinfo.gpg の次の行から、起動のたびに gpg で復号して取り出す。
#
#   machine 192.168.1.5 port 5432 login postgres password ********
#
# サーバ本体は Docker の crystaldba/postgres-mcp。Windows では PyPI 版の
# 0.3 系が依存の pglast (Windows 向けの配布物が無い) で解決できず、
# uvx で入るのは今の mcp ライブラリと合わない 0.1.0 だけになる。
#
# stdout は MCP の通信路なので、ここで何かを書いてはいけない。
# 診断はすべて stderr へ。

$ErrorActionPreference = 'Stop'

$PgHost = if ($env:IDECO_PGHOST) { $env:IDECO_PGHOST } else { '192.168.1.5' }
$PgPort = if ($env:IDECO_PGPORT) { $env:IDECO_PGPORT } else { '5432' }
$PgUser = if ($env:IDECO_PGUSER) { $env:IDECO_PGUSER } else { 'postgres' }
$PgDb   = if ($env:IDECO_PGDATABASE) { $env:IDECO_PGDATABASE } else { 'ideco_db' }
$Image  = 'crystaldba/postgres-mcp'

function Fail([string]$msg) {
    [Console]::Error.WriteLine("ideco-postgres: $msg")
    exit 1
}

# Emacs (epg) と同じ gpg を使う。Git for Windows 付属のもの
# (C:\Program Files\Git\usr\bin) で、鍵束は ~/.gnupg。
$Gpg = $env:IDECO_GPG
if (-not $Gpg) {
    $Gpg = Get-Command gpg -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1 -ExpandProperty Source
}
if (-not $Gpg) { Fail 'gpg が見つからない (IDECO_GPG で指定できる)' }

$AuthFile = Join-Path $HOME '.authinfo.gpg'
if (-not (Test-Path $AuthFile)) { Fail "$AuthFile が無い" }

# 平文は変数に持つだけでディスクには書かない。
$plain = & $Gpg --quiet --batch --yes --decrypt $AuthFile 2>$null
if ($LASTEXITCODE -ne 0) { Fail "$AuthFile の復号に失敗した (gpg の終了コード $LASTEXITCODE)" }

# netrc 形式。値は空白区切りで、空白を含むものは "..." で囲まれる。
$Password = $null
foreach ($line in $plain) {
    $tokens = [regex]::Matches($line, '"((?:\\.|[^"\\])*)"|(\S+)') | ForEach-Object {
        if ($_.Groups[1].Success) { $_.Groups[1].Value -replace '\\(.)', '$1' } else { $_.Groups[2].Value }
    }
    $kv = @{}
    for ($i = 0; $i + 1 -lt $tokens.Count; $i += 2) { $kv[$tokens[$i]] = $tokens[$i + 1] }
    if ($kv['machine'] -eq $PgHost -and $kv['login'] -eq $PgUser -and
        (-not $kv.ContainsKey('port') -or $kv['port'] -eq $PgPort) -and $kv.ContainsKey('password')) {
        $Password = $kv['password']
        break
    }
}
$plain = $null
if (-not $Password) { Fail "$AuthFile に machine $PgHost port $PgPort login $PgUser の行が無い" }

# docker の引数にはパスワードを載せない (-e NAME だけ渡すと親の環境変数を引き継ぐ)。
$enc = [uri]::EscapeDataString($Password)
$env:DATABASE_URI = "postgresql://${PgUser}:${enc}@${PgHost}:${PgPort}/${PgDb}"
$Password = $null; $enc = $null

# postgres-mcp は stdin が EOF になっても終了しない (実測)。しかも claude が
# このラッパを殺しても docker.exe とコンテナは道連れにならないので、放っておくと
# セッションのたびにコンテナ (と DB への接続) が 1 つずつ残る。
# ラベルに持ち主 (このラッパの PID と起動時刻) を書いておき、次に起動したときに
# 持ち主が死んでいるものを消す。PID だけだと再利用で生きていると誤認する。
# 生きている持ち主のものは消さない (別の claude セッションが使っている)。
$Label = 'emacs-d.ideco-mcp.owner'
function Owner-Key([System.Diagnostics.Process]$p) { "$($p.Id)-$($p.StartTime.ToFileTimeUtc())" }
$stale = & docker ps -a --filter "label=$Label" --format "{{.ID}} {{.Label `"$Label`"}}" 2>$null
foreach ($row in $stale) {
    $id, $owner = $row -split ' ', 2
    $ownerPid = [int]($owner -split '-')[0]
    $p = Get-Process -Id $ownerPid -ErrorAction SilentlyContinue
    if (-not $p -or (Owner-Key $p) -ne $owner) {
        & docker rm -f $id *> $null
    }
}

# restricted = 読み取り専用 (トランザクションを READ ONLY で張り、文も検査する)。
& docker run -i --rm --label "$Label=$(Owner-Key (Get-Process -Id $PID))" `
    -e DATABASE_URI $Image --access-mode=restricted
exit $LASTEXITCODE
