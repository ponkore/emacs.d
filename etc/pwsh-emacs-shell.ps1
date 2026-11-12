# Emacs の M-x shell から `-NoExit -Command ". <このファイル>"` で読み込まれる。
#
# Emacs は Windows ではサブプロセスを擬似端末ではなくパイプで起動する。
# コンソールが無いと PowerShell の入出力エンコーディングがコンソール
# コードページ (cp932) になり日本語が化けるため、明示的に UTF-8 に固定する。
# BOM を付けないので [Text.UTF8Encoding]::new() を使う (::UTF8 は BOM 付き)。
[Console]::InputEncoding  = [System.Text.UTF8Encoding]::new()
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

# starship のプロンプト。パイプ越しでも ANSI エスケープはそのまま流れるので、
# Emacs 側の ansi-color-process-output が色を付けてくれる。
Invoke-Expression (& starship init powershell)

# shell-mode の cd 追跡 (shell-dirtrack-mode) は bash の構文前提なので使えない。
# 代わりに OSC 7 でカレントディレクトリを Emacs に通知する。
# Emacs 側は ansi-osc-directory-tracker がこれを受けて default-directory を更新する。
$global:EmacsStarshipPrompt = $function:prompt
function global:prompt {
    $dir = $PWD.ProviderPath.Replace([char]92, '/').Replace(' ', '%20')
    "$([char]27)]7;file:///$dir$([char]7)" + (& $global:EmacsStarshipPrompt)
}

# EmbTerm のシェル統合（embterm_find_file などで Emacs を呼ぶ・プロンプトの境界 OSC 133）。
# EmbTerm の端末の中だけ。prompt を包むので starship と OSC 7 の後に読み込む。
# 配布物は M-x my:embterm-update が ~/.emacs.d/embterm/ に写す（my-embterm.el）。
$embtermShell = Join-Path $PSScriptRoot '..\embterm\etc\embterm.ps1'
if ($env:TERM_PROGRAM -eq 'EmbTerm' -and (Test-Path $embtermShell)) { . $embtermShell }
Remove-Variable embtermShell
