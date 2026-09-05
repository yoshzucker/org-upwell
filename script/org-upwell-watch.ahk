#Requires AutoHotkey v2.0
#SingleInstance Force
Persistent()

;; org-upwell resident watcher (Windows).
;;
;; Independent of remap-windows-keys.ahk.  A Startup folder .lnk should
;; point at *this file*, not a copy: git pull then a Windows restart is
;; what picks up a new version.  script/install-startup.ps1 (and
;; bootstrap.ps1's Setup-StartupShortcuts) create that shortcut.
;;
;;   org-upwell-watch.ahk --out C:\Users\you\.local\share\org-upwell

A_IconTip := "org-upwell watcher"
try TraySetIcon("shell32.dll", 147)

outDir := EnvGet("USERPROFILE") "\.local\share\org-upwell"
i := 1
while i <= A_Args.Length {
    if (A_Args[i] = "--out" && i < A_Args.Length) {
        outDir := RTrim(A_Args[i + 1], "\/")
        i += 2
    } else {
        i++
    }
}

DirCreate outDir

downloads := EnvGet("USERPROFILE") "\Downloads"
lastFront := ""
lastDl := A_Now
pollMs := 15000

SetTimer Watch, pollMs
Watch()

Watch() {
    global lastFront, lastDl, downloads
    try {
        hwnd := WinExist("A")
        if !hwnd {
            return
        }
        pid := WinGetPID(hwnd)
        exe := ProcessGetName(pid)
        title := WinGetTitle(hwnd)
        path := ""
        url := ""
        kind := "file"

        if (InStr(exe, "EXCEL") = 1) {
            path := OfficeFullName("Excel.Application", "ActiveWorkbook")
        } else if (InStr(exe, "POWERPNT") = 1) {
            path := OfficeFullName("PowerPoint.Application", "ActivePresentation")
        } else if (InStr(exe, "WINWORD") = 1) {
            path := OfficeFullName("Word.Application", "ActiveDocument")
        } else if (InStr(exe, "msedge") = 1 || InStr(exe, "chrome") = 1
                   || InStr(exe, "firefox") = 1) {
            url := BrowserUrl(exe)
            kind := "url"
        } else if (InStr(exe, "explorer") = 1) {
            path := ExplorerSelected()
        }

        if (IsHttp(path)) {
            url := path
            path := ""
            kind := "url"
        }

        payload := path != "" ? path : url
        if (payload != "" && payload != lastFront) {
            lastFront := payload
            WriteTrace(exe, title, path, url, kind)
        }

        lastDl := ScanDownloads(downloads, lastDl)
    } catch as err {
        ; Stay resident.  A COM refusal is not a reason to exit.
    }
}

OfficeFullName(progId, active) {
    try {
        app := ComObjActive(progId)
        if (active = "ActiveWorkbook")
            return app.ActiveWorkbook.FullName
        if (active = "ActivePresentation")
            return app.ActivePresentation.FullName
        if (active = "ActiveDocument")
            return app.ActiveDocument.FullName
    } catch {
        return ""
    }
    return ""
}

BrowserUrl(_exe) {
    ; Address bar via Ctrl+L would steal focus and is not silent.
    ; UI Automation lives in the sibling .ps1; this helper calls it
    ; only for browser windows, so Excel polling stays in-process.
    script := A_ScriptDir "\front-url.ps1"
    if !FileExist(script) {
        return ""
    }
    try {
        return Trim(ComObject("WScript.Shell").Exec(
            'powershell -NoProfile -ExecutionPolicy Bypass -File "' script '"').StdOut.ReadAll())
    } catch {
        return ""
    }
}

ExplorerSelected() {
    try {
        shell := ComObject("Shell.Application")
        hwnd := WinExist("A")
        for window in shell.Windows {
            try {
                if (window.HWND = hwnd) {
                    sel := window.Document.SelectedItems
                    if (sel.Count > 0)
                        return sel.Item(0).Path
                    return window.Document.Folder.Self.Path
                }
            } catch {
                continue
            }
        }
    } catch {
        return ""
    }
    return ""
}

ScanDownloads(dir, since) {
    newest := since
    if !DirExist(dir) {
        return since
    }
    Loop Files dir "\*.*" {
        skip := A_LoopFileExt = "tmp" || A_LoopFileExt = "crdownload"
            || A_LoopFileExt = "partial" || A_LoopFileExt = "download"
        if skip {
            continue
        }
        if (A_LoopFileTimeModified > since) {
            WriteTrace("Downloads", A_LoopFileName, A_LoopFileFullPath, "", "file")
            if (A_LoopFileTimeModified > newest)
                newest := A_LoopFileTimeModified
        }
    }
    return newest
}

IsHttp(s) {
    return s != "" && (InStr(s, "https://") = 1 || InStr(s, "http://") = 1)
}

UnixNow() {
    return DateDiff(A_NowUTC, "19700101000000", "Seconds")
}

JsonEsc(s) {
    s := StrReplace(s, "\", "\\")
    s := StrReplace(s, "`n", "\n")
    s := StrReplace(s, "`r", "")
    s := StrReplace(s, "`t", "\t")
    s := StrReplace(s, '"', '\"')
    return s
}

WriteTrace(app, title, path, url, kind) {
    global outDir
    if (path = "" && url = "") {
        return
    }
    day := FormatTime(, "yyyy-MM-dd")
    file := outDir "\trace-" day ".jsonl"
    line := '{"ts":' UnixNow()
        . ',"app":"' JsonEsc(app) '"'
        . ',"title":"' JsonEsc(title) '"'
        . ',"path":"' JsonEsc(path) '"'
        . ',"url":"' JsonEsc(url) '"'
        . ',"kind":"' JsonEsc(kind) '"}`n'
    FileAppend line, file, "UTF-8"
}
