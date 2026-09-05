<#
.SYNOPSIS
  Print the frontmost Edge/Chrome URL, or nothing.

.DESCRIPTION
  Called by org-upwell-watch.ahk when the front window is a browser.
  Uses UI Automation on the address bar.  Fragile across browser
  versions, which is why a miss is silent rather than an error -- the
  watcher still has the window title, and Office COM is the reliable
  path for the files in front.
#>
$ErrorActionPreference = 'SilentlyContinue'
Add-Type -AssemblyName UIAutomationClient
Add-Type @"
using System;
using System.Runtime.InteropServices;
public class OrgUpwellWin {
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
}
"@
$hwnd = [OrgUpwellWin]::GetForegroundWindow()
if ($hwnd -eq [IntPtr]::Zero) { exit 0 }
$root = [System.Windows.Automation.AutomationElement]::FromHandle($hwnd)
if (-not $root) { exit 0 }
$cond = New-Object System.Windows.Automation.PropertyCondition(
    [System.Windows.Automation.AutomationElement]::ControlTypeProperty,
    [System.Windows.Automation.ControlType]::Edit)
$edits = $root.FindAll([System.Windows.Automation.TreeScope]::Descendants, $cond)
foreach ($e in $edits) {
    $val = $e.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)
    if ($val -and $val.Current.Value -match '^https?://') {
        Write-Output $val.Current.Value
        exit 0
    }
}
exit 0
