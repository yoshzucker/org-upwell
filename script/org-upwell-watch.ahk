#Requires AutoHotkey v2.0
#SingleInstance Force
Persistent()

;; org-upwell resident watcher (Windows).
;;
;; Runs on its own, and does not belong inside another AutoHotkey script:
;; a watcher that shares a process with your hotkeys stops when they are
;; reloaded.  A Startup folder .lnk should point at *this file* rather
;; than a copy, so updating the package and logging back in is what picks
;; up a new version.  script/install-startup.ps1 creates that shortcut.
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
lastBrowser := ""
lastDl := A_Now
pollMs := 15000

SetTimer Watch, pollMs
Watch()

Watch() {
    global lastFront, lastBrowser, lastDl, downloads
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
        ; Leaving the browser and coming back is a new appearance, and the
        ; document in front is worth recording again.  Only an unbroken
        ; spell on one tab is the silent case.
        isBrowser := (InStr(exe, "msedge") = 1 || InStr(exe, "chrome") = 1
                      || InStr(exe, "firefox") = 1)
        if !isBrowser {
            lastBrowser := ""
        }

        if (InStr(exe, "EXCEL") = 1) {
            path := OfficeFullName("Excel.Application", "ActiveWorkbook")
        } else if (InStr(exe, "POWERPNT") = 1) {
            path := OfficeFullName("PowerPoint.Application", "ActivePresentation")
        } else if (InStr(exe, "WINWORD") = 1) {
            path := OfficeFullName("Word.Application", "ActiveDocument")
        } else if (isBrowser) {
            ; The title is free and the URL costs a process, so the title
            ; decides whether to pay: a tab or a page cannot change without
            ; it changing.  Staying on one tab is silent, which is what the
            ; window title is asked here to prove.
            kind := "url"
            stamp := hwnd "|" title
            if (stamp != lastBrowser) {
                lastBrowser := stamp
                url := BrowserUrl(hwnd)
            }
        } else if (InStr(exe, "explorer") = 1) {
            path := ExplorerSelected(&dirOnly)
            if (dirOnly) {
                kind := "dir"
            }
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

BrowserUrl(hwnd) {
    ; Address bar via Ctrl+L would steal focus and is not silent.
    ; UI Automation lives in the sibling .ps1; this helper calls it
    ; only for browser windows, so Excel polling stays in-process.
    ;
    ; Run, not Exec: Exec cannot hide the console it starts, and a
    ; console appearing every sample takes the keyboard with it.  Hidden
    ; means no pipe to read, so the helper is given a file to write and
    ; the window handle this script already has.
    script := A_ScriptDir "\front-url.ps1"
    if !FileExist(script) {
        return ""
    }
    out := A_Temp "\org-upwell-url.txt"
    url := ""
    try {
        RunWait('powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass'
                . ' -File "' script '" -Hwnd ' hwnd ' -Out "' out '"', , "Hide")
    } catch {
        return ""
    }
    try url := Trim(FileRead(out, "UTF-8"))
    try FileDelete out
    return url
}

ExplorerSelected(&dirOnly) {
    ; dirOnly says the answer is the window's own directory rather than
    ; something picked out in it.  Emacs keeps files and drops the directory
    ; somebody merely had open, and cannot tell the two apart from a path.
    ; Shell.Application calls it Folder because Windows does; the rest of
    ; org-upwell says directory.
    dirOnly := false
    try {
        shell := ComObject("Shell.Application")
        hwnd := WinExist("A")
        for window in shell.Windows {
            try {
                if (window.HWND = hwnd) {
                    sel := window.Document.SelectedItems
                    if (sel.Count > 0)
                        return sel.Item(0).Path
                    dirOnly := true
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
