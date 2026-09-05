;;; org-upwell-watch.el --- Optional starter for the resident watcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The watcher is a process of its own.  It must not be an Emacs subprocess
;; as the *primary* way it runs: on macOS a privacy prompt spawned by Emacs
;; is attributed to Emacs, which has no usage string for Automation, and the
;; dialog never appears -- the same hole org-calsync hit with Calendar.app.
;; On Windows, a watcher that dies with Emacs stops catching the hour after
;; you close the frame.
;;
;; So the real install is a Login Item / Startup shortcut / launchd plist
;; pointing at `script/org-upwell-watch.ahk' or `script/org-upwell-watch.macos'.
;; What this file offers is a convenience start for a machine being tested,
;; and a way to ask whether the log is moving.

;;; Code:

(require 'org-upwell-core)

(defvar org-upwell-watch-process nil
  "Convenience process, or nil.  Production does not use this slot.")

(defun org-upwell-watch-command ()
  "Return (PROGRAM . ARGS) for the resident watcher on this OS."
  (pcase system-type
    ('windows-nt
     (let ((ahk (or (executable-find "AutoHotkey64")
                    (executable-find "AutoHotkey")
                    (executable-find "autohotkey")))
           (script (org-upwell--script "org-upwell-watch.ahk"))
           (out (org-upwell-trace-directory)))
       (cond
        (ahk (list ahk script "--out" out))
        ((executable-find "powershell")
         (list "powershell" "-NoProfile" "-ExecutionPolicy" "Bypass"
               "-File" (org-upwell--script "org-upwell-watch.ps1")
               "-OutDir" out))
        (t nil))))
    ('darwin
     (list "bash" (org-upwell--script "org-upwell-watch.macos")
           (org-upwell-trace-directory)))
    (_ nil)))

;;;###autoload
(defun org-upwell-watch-start ()
  "Start the watcher as a convenience process.

Prefer installing it independently (see README).  This command is for a
session that is testing capture, particularly on macOS where the script
is an osascript loop."
  (interactive)
  (when (process-live-p org-upwell-watch-process)
    (user-error "org-upwell: watcher already running in this Emacs"))
  (let ((cmd (org-upwell-watch-command)))
    (unless cmd
      (user-error "org-upwell: no watcher command for %s" system-type))
    (make-directory (org-upwell-trace-directory) t)
    (setq org-upwell-watch-process
          (apply #'start-process "org-upwell-watch" "*org-upwell-watch*" cmd))
    (set-process-query-on-exit-flag org-upwell-watch-process nil)
    (message "org-upwell: watcher started (convenience); install it independently for daily use")))

;;;###autoload
(defun org-upwell-watch-stop ()
  "Stop the convenience watcher started from this Emacs."
  (interactive)
  (when (process-live-p org-upwell-watch-process)
    (kill-process org-upwell-watch-process))
  (setq org-upwell-watch-process nil))

(defun org-upwell-watch-running-p ()
  "Return non-nil if today's trace file has been written to in the last minute.

Used as a cheap liveness check that does not care who started the watcher."
  (let ((f (org-upwell-trace-file)))
    (and (file-exists-p f)
         (< (float-time (time-subtract (current-time)
                                       (file-attribute-modification-time
                                        (file-attributes f))))
            90))))

(provide 'org-upwell-watch)

;;; org-upwell-watch.el ends here
