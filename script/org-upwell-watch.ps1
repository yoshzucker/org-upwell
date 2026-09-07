<#
.SYNOPSIS
  Resident org-upwell watcher for Windows, without AutoHotkey.

.DESCRIPTION
  Same contract as org-upwell-watch.ahk: poll the front window, write
  one JSONL line when the document or URL changes.  Use this if you
  would rather not run AHK; otherwise the AHK file is the one to
  compile to an exe and put in Startup.

.PARAMETER OutDir
  Directory for trace-YYYY-MM-DD.jsonl (created if missing).

.PARAMETER Interval
  Seconds between polls (default 15).
#>
param(
    [string]$OutDir = $(Join-Path $env:USERPROFILE '.local\share\org-upwell'),
    [int]$Interval = 15
)

$ErrorActionPreference = 'SilentlyContinue'
New-Item -ItemType Directory -Path $OutDir -Force | Out-Null

Add-Type @"
using System;
using System.Runtime.InteropServices;
public class OrgUpwellWin {
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
}
"@

function Get-UnixNow {
    [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
}

function Write-Trace($app, $title, $path, $url, $kind) {
    if (-not $path -and -not $url) { return }
    $day = Get-Date -Format 'yyyy-MM-dd'
    $file = Join-Path $OutDir "trace-$day.jsonl"
    $obj = [ordered]@{
        ts    = Get-UnixNow
        app   = "$app"
        title = "$title"
        path  = "$path"
        url   = "$url"
        kind  = "$kind"
    }
    ($obj | ConvertTo-Json -Compress) + "`n" | Add-Content -Path $file -Encoding utf8
}

function Get-OfficeFullName($progId, $property) {
    try {
        $app = [Runtime.InteropServices.Marshal]::GetActiveObject($progId)
        $doc = $app.$property
        if ($null -eq $doc) { return '' }
        return [string]$doc.FullName
    } catch {
        return ''
    }
}

$last = ''
$lastBrowser = ''
$lastDl = Get-Date
$downloads = Join-Path $env:USERPROFILE 'Downloads'

while ($true) {
    try {
        $hwnd = [OrgUpwellWin]::GetForegroundWindow()
        # Not $pid: that name is PowerShell's own, and read-only.  Writing
        # to it fails quietly here and every window looks like this script.
        $procId = 0
        [void][OrgUpwellWin]::GetWindowThreadProcessId($hwnd, [ref]$procId)
        $proc = Get-Process -Id $procId
        $exe = $proc.ProcessName
        $title = $proc.MainWindowTitle
        $path = ''
        $url = ''
        $kind = 'file'
        $isBrowser = $false

        switch -Regex ($exe) {
            '^EXCEL'    { $path = Get-OfficeFullName 'Excel.Application' 'ActiveWorkbook' }
            '^POWERPNT' { $path = Get-OfficeFullName 'PowerPoint.Application' 'ActivePresentation' }
            '^WINWORD'  { $path = Get-OfficeFullName 'Word.Application' 'ActiveDocument' }
            '^(msedge|chrome|firefox)$' {
                # Same rule as the AutoHotkey watcher: the title changes
                # whenever the tab or the page does, and reading the address
                # bar costs a UI Automation walk.
                $kind = 'url'
                $isBrowser = $true
                $stamp = "$($hwnd.ToInt64())|$title"
                if ($stamp -ne $lastBrowser) {
                    $lastBrowser = $stamp
                    $helper = Join-Path $PSScriptRoot 'front-url.ps1'
                    if (Test-Path $helper) {
                        $url = (& $helper -Hwnd $hwnd.ToInt64() | Select-Object -First 1)
                    }
                }
            }
        }
        if (-not $isBrowser) { $lastBrowser = '' }

        if ($path -match '^https?://') {
            $url = $path
            $path = ''
            $kind = 'url'
        }

        $payload = if ($path) { $path } else { $url }
        if ($payload -and $payload -ne $last) {
            $last = $payload
            Write-Trace $exe $title $path $url $kind
        }

        if (Test-Path $downloads) {
            Get-ChildItem $downloads -File -ErrorAction SilentlyContinue |
                Where-Object {
                    $_.LastWriteTime -gt $lastDl -and
                    $_.Extension -notin '.tmp', '.crdownload', '.partial', '.download'
                } |
                ForEach-Object {
                    Write-Trace 'Downloads' $_.Name $_.FullName '' 'file'
                    if ($_.LastWriteTime -gt $lastDl) { $lastDl = $_.LastWriteTime }
                }
        }
    } catch {
        # keep looping
    }
    Start-Sleep -Seconds $Interval
}
