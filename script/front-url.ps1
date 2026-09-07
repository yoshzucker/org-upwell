<#
.SYNOPSIS
  Print the frontmost Edge/Chrome URL, or nothing.

.DESCRIPTION
  Called by org-upwell-watch.ahk when the front window is a browser.
  Uses UI Automation on the address bar.  Fragile across browser
  versions, which is why a miss is silent rather than an error -- the
  watcher still has the window title, and Office COM is the reliable
  path for the files in front.

.PARAMETER Hwnd
  Window to read.  The caller usually knows it already; without it this
  script asks Windows for the foreground window, which costs a C#
  compile on every run.

.PARAMETER Out
  File to write the answer to, instead of standard output.  A hidden
  process has no pipe to read, and hidden is how the watcher starts
  this: a console appearing every fifteen seconds takes the keyboard
  with it.  The file is written even when nothing was found, so the
  caller never reads an answer from a previous run.
#>
param(
    [long]$Hwnd = 0,
    [string]$Out = ''
)
$ErrorActionPreference = 'SilentlyContinue'
Add-Type -AssemblyName UIAutomationClient
Add-Type -AssemblyName UIAutomationTypes

function Write-Result($value) {
    if ($Out) {
        Set-Content -LiteralPath $Out -Value "$value" -Encoding utf8 -NoNewline
    } elseif ($value) {
        Write-Output $value
    }
}

if ($Hwnd -ne 0) {
    $handle = [IntPtr]$Hwnd
} else {
    Add-Type @"
using System;
using System.Runtime.InteropServices;
public class OrgUpwellWin {
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
}
"@
    $handle = [OrgUpwellWin]::GetForegroundWindow()
}
if ($handle -eq [IntPtr]::Zero) { Write-Result ''; exit 0 }
$root = [System.Windows.Automation.AutomationElement]::FromHandle($handle)
if (-not $root) { Write-Result ''; exit 0 }
$cond = New-Object System.Windows.Automation.PropertyCondition(
    [System.Windows.Automation.AutomationElement]::ControlTypeProperty,
    [System.Windows.Automation.ControlType]::Edit)

function Get-Url($element) {
    $val = $element.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
    if ($val -and $val.Current.Value -match '^https?://') { return $val.Current.Value }
    return ''
}

# The address bar is the first Edit in the tree, so one search usually
# answers.  Walking every descendant is the fallback, not the rule: on a
# page with many fields that walk is the whole cost of this script.
$first = $root.FindFirst([System.Windows.Automation.TreeScope]::Descendants, $cond)
if ($first) {
    $url = Get-Url $first
    if ($url) { Write-Result $url; exit 0 }
}
foreach ($e in $root.FindAll([System.Windows.Automation.TreeScope]::Descendants, $cond)) {
    $url = Get-Url $e
    if ($url) { Write-Result $url; exit 0 }
}
Write-Result ''
exit 0
