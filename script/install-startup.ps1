#Requires -Version 5.0
<#
.SYNOPSIS
  Put a Startup shortcut pointing at org-upwell-watch.ahk.

.DESCRIPTION
  Does not copy the script.  The .lnk targets the .ahk file in the
  package, so updating org-upwell and logging back in is what picks up a
  new version.

  Idempotent: a shortcut already pointing at the same target is left
  alone, so a configuration that installs this on every update may call
  it as often as it likes.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File .\install-startup.ps1
#>
$ErrorActionPreference = 'Stop'

$ahk = Join-Path $PSScriptRoot 'org-upwell-watch.ahk'
if (-not (Test-Path -LiteralPath $ahk -PathType Leaf)) {
    Write-Error "watcher not found: $ahk"
}

$startup = [System.Environment]::GetFolderPath('Startup')
$lnkPath = Join-Path $startup 'org-upwell-watch.ahk.lnk'
$ws = New-Object -ComObject WScript.Shell

if (Test-Path -LiteralPath $lnkPath) {
    $existing = $ws.CreateShortcut($lnkPath).TargetPath
    if ($existing -and ([string]::Equals(
            (Resolve-Path $existing).Path,
            (Resolve-Path $ahk).Path,
            [System.StringComparison]::OrdinalIgnoreCase))) {
        Write-Host "already installed: $lnkPath -> $ahk"
        exit 0
    }
    Write-Host "replacing shortcut: $lnkPath"
}

$shortcut = $ws.CreateShortcut($lnkPath)
$shortcut.TargetPath = $ahk
$shortcut.WorkingDirectory = $PSScriptRoot
$shortcut.Save()
Write-Host "created $lnkPath -> $ahk"
Write-Host "traces go to $env:USERPROFILE\.local\share\org-upwell"
Write-Host "a logon (or restart) starts it; git pull then restart picks up edits"
