# 領域選択スクリーンショットを撮って PNG として保存する。
# Windows の「切り取り & スケッチ」(Win+Shift+S) のオーバーレイを呼び出し、
# クリップボードに載った画像をファイルへ書き出す。
#
# 必ず Windows PowerShell (powershell.exe) の STA で実行すること。
#   - pwsh 7 の Get-Clipboard には -Format が無い
#   - このファイルは UTF-8 BOM 付きで保存すること
#     (BOM が無いと PowerShell 5.1 は cp932 として読み、日本語コメントが化けて
#      改行を巻き込み param ブロックごと壊れる)
#
# 終了コード: 0 = 保存成功、1 = キャンセルまたはタイムアウト
param(
    [Parameter(Mandatory = $true)][string]$Path,
    [int]$TimeoutSeconds = 60
)
Add-Type -AssemblyName System.Windows.Forms, System.Drawing
# キャンセルを検出できるよう、いったんクリップボードを空にする
[System.Windows.Forms.Clipboard]::Clear()
Start-Process -FilePath 'ms-screenclip:'
$deadline = (Get-Date).AddSeconds($TimeoutSeconds)
do {
    Start-Sleep -Milliseconds 200
    $img = Get-Clipboard -Format Image
} while (-not $img -and (Get-Date) -lt $deadline)
if (-not $img) { exit 1 }
$img.Save($Path, [System.Drawing.Imaging.ImageFormat]::Png)
$img.Dispose()
exit 0
