;;; org-upwell-trace.el --- The watcher's log, read back into Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The resident watcher writes one JSON object per sample, without knowing
;; anything about Org.  This file is the only thing that reads those lines.
;;
;; A trace is an observation: at this unix second, this app was in front,
;; with this path or URL if the watcher could get one.  It is not a claim.
;; Intersection with a clock -- live, or filled in later with foresight's
;; C -- is org-upwell-claim.el's job.

;;; Code:

(require 'org-upwell-core)
(require 'json)
(require 'parse-time)
(require 'seq)
(require 'cl-lib)

(defun org-upwell--trace-unix (raw)
  "Return a unix seconds number from RAW, or nil.

The watcher writes `ts' as an integer.  A string ISO timestamp is also
accepted, so a hand-written fixture does not have to do the arithmetic."
  (cond
   ((numberp raw) raw)
   ((stringp raw)
    (let ((parsed (ignore-errors (parse-iso8601-time-string raw))))
      (and parsed (floor (float-time parsed)))))
   (t nil)))

(defun org-upwell--json-get (obj key)
  "Lookup KEY in JSON alist OBJ.

`json-parse-string' interned keys as symbols on Emacs 30 when
`:key-type' is left alone, and rejected `:key-type string' on this
build.  Accept both spellings so a hand-written fixture and a watcher
line parse the same way."
  (or (alist-get key obj nil nil #'equal)
      (alist-get (intern key) obj)))

(defun org-upwell--trace-from-alist (obj)
  "Turn a JSON alist OBJ into a trace plist, or nil if it is unusable."
  (let* ((get (lambda (k)
                (org-upwell--json-get obj k)))
         (ts (org-upwell--trace-unix (or (funcall get "ts")
                                         (funcall get "timestamp"))))
         (path (let ((p (funcall get "path")))
                 (and p (not (string-empty-p p)) p)))
         (url (let ((u (funcall get "url")))
                (and u (not (string-empty-p u)) u)))
         (office (let ((o (funcall get "office")))
                   (and o (not (string-empty-p o)) o)))
         (title (funcall get "title"))
         (app (funcall get "app"))
         (file-id (let ((f (funcall get "file_id")))
                    (and f (not (string-empty-p f)) f)))
         (kind (intern (or (funcall get "kind") "file"))))
    (when (and ts (or path url office))
      (when (and path (org-upwell--looks-like-url path))
        (setq url (or url path)
              path nil))
      (list :ts ts
            :app app
            :title title
            :path path
            :url url
            :office (or office (and url (org-upwell-mint-office url)))
            :file-id file-id
            :kind kind
            :name (or (org-upwell-basename path)
                      (let ((clean (org-upwell--strip-title-noise title)))
                        (and clean (not (string-empty-p clean)) clean))
                      url)))))

(defun org-upwell-trace-write (&rest plist)
  "Append one sighting to today's trace file.

PLIST takes `:path\=', `:url\=', `:kind\=', `:title\=' and `:app\='.  The line is
the same six fields the resident watcher writes, because the reader and
the clock intersection downstream must not be able to tell which of the
two saw the thing.

Written with `json-encode\=' rather than by hand: the watchers escape their
own strings, and a path with a quote or a backslash in it is exactly the
one that would otherwise write a line nothing can parse."
  (let* ((file (org-upwell-trace-file))
         (line (json-encode
                `(("ts" . ,(floor (float-time)))
                  ("app" . ,(or (plist-get plist :app) "Emacs"))
                  ("title" . ,(or (plist-get plist :title) ""))
                  ("path" . ,(or (plist-get plist :path) ""))
                  ("url" . ,(or (plist-get plist :url) ""))
                  ("kind" . ,(or (plist-get plist :kind) "file"))))))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      ;; Appended, and one line at a time: the watcher is appending to this
      ;; same file from another process all day.
      (write-region (concat line "\n") nil file t 'silent))
    file))

(defun org-upwell-read-trace-file (file)
  "Return traces recorded in FILE, skipping unreadable lines."
  (if (not (file-readable-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      (let (out)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
            (unless (string-empty-p line)
              (let* ((obj (ignore-errors
                            (json-parse-string line
                                               :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)))
                     (tr (and obj (org-upwell--trace-from-alist obj))))
                (when tr (push tr out)))))
          (forward-line 1))
        (nreverse out)))))

(defun org-upwell--trace-files-between (from to)
  "Return the day files that can hold traces in [FROM, TO].

Every day is walked, not just the ends: a clock filled in across a
long weekend covers days in the middle, and reading only the edges
loses them.  The day before FROM is read too, because a sample taken
just before local midnight is written by a watcher whose day may
still have been the previous one."
  (let ((day (time-subtract from (days-to-time 1)))
        (last (org-upwell-trace-file to))
        files)
    (while (progn
             (push (org-upwell-trace-file day) files)
             (and (not (equal (car files) last))
                  (time-less-p day to)))
      (setq day (time-add day (days-to-time 1))))
    (delete-dups (nreverse files))))

(defun org-upwell-read-traces (&optional from-time to-time)
  "Return traces whose unix ts sits in [FROM-TIME, TO-TIME].

Times are Emacs time values.  Nil FROM-TIME is the start of today; nil
TO-TIME is now.  Every day file the window touches is read."
  (let* ((from (or from-time (org-upwell--day-start 0)))
         (to (or to-time (current-time)))
         (from-u (floor (float-time from)))
         (to-u (floor (float-time to)))
         (files (org-upwell--trace-files-between from to))
         traces)
    (dolist (f files)
      (dolist (tr (org-upwell-read-trace-file f))
        (let ((ts (plist-get tr :ts)))
          (when (and ts (>= ts from-u) (<= ts to-u))
            (push tr traces)))))
    (seq-sort (lambda (a b) (< (plist-get a :ts) (plist-get b :ts)))
              traces)))

(defun org-upwell--day-start (&optional day-offset)
  "Local midnight, DAY-OFFSET days back (0 = today)."
  (let ((d (decode-time (current-time))))
    (encode-time 0 0 0 (- (nth 3 d) (or day-offset 0)) (nth 4 d) (nth 5 d))))

(defun org-upwell-trace-in-interval-p (trace from to)
  "Return non-nil when TRACE's timestamp sits in [FROM, TO).

FROM and TO are Emacs time values.  A sample is a point, not a span: the
watcher polls, it does not measure duration.  Closed-open so a sample at
the clock-out instant belongs to the next spell, not both."
  (let ((ts (plist-get trace :ts)))
    (and ts
         (>= ts (floor (float-time from)))
         (< ts (floor (float-time to))))))

(defun org-upwell-traces-in (from to)
  "Return traces sampled in [FROM, TO)."
  (seq-filter (lambda (tr) (org-upwell-trace-in-interval-p tr from to))
              (org-upwell-read-traces from to)))

(defun org-upwell-unique-traces (traces)
  "Drop duplicate path/url/office samples, keeping the last of each."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (tr (reverse traces))
      (let ((key (or (plist-get tr :path)
                     (plist-get tr :url)
                     (plist-get tr :office))))
        (when (and key (not (gethash key seen)))
          (puthash key t seen)
          (push tr out))))
    out))

(defcustom org-upwell-title-noise
  '(" - Microsoft Edge\\'"
    " - Profile [0-9]+\\'"
    " - Google Chrome\\'"
    " [-\u2014] Mozilla Firefox\\'"
    " - Chromium\\'"
    " - Brave\\'")
  "Regexps stripped from a window title before it becomes an item\='s name.

A browser puts its own name on the end of every window it owns, and some put
the profile there too.  Where the watcher can only read the window title --
Windows, where there is no scripting bridge to ask the tab -- that suffix
arrives on every URL alike.  It is the same handful of characters on every
row, and it is the part a narrow column has least reason to keep.

Anchored at the end, so a page actually about a browser keeps its subject.

One segment only.  Edge writes the profile between the page and its own name,
and it is tempting to take both -- but nothing tells a profile apart from the
last part of a title, so a rule that took two would quietly eat the subject of
every page whose title happens to end in a phrase.  Losing real words is worse
than keeping a few noisy ones; add a rule here for a profile that is actually
in the way.

Edge\='s default profile name is the exception, and is listed: \"Profile 3\" at
the end of a title is a profile and not a subject.  A profile somebody has
named is not guessable, and is left."
  :type '(repeat regexp)
  :group 'org-upwell)

(defun org-upwell--strip-title-noise (title)
  "Return TITLE without whatever `org-upwell-title-noise\=' matches."
  (when title
    (let ((out title))
      (dolist (re org-upwell-title-noise)
        (setq out (replace-regexp-in-string re "" out)))
      (string-trim out))))

(defun org-upwell-trace-directory-p (trace)
  "Return non-nil when TRACE is a directory somebody had open.

A directory in front is where the work was kept, not a thing to open from
a heading, and the bench is a list of things to open.  One a person put
there on purpose -- a pin, a drop -- is a different act and is kept.

`kind\=' cannot answer this on its own: the Windows watcher reports the
directory it read as a file, and a selected item may be a directory too.
So the disk is asked, after `kind\=' has had its say."
  (let ((path (plist-get trace :path)))
    (or (eq (plist-get trace :kind) 'dir)
        (and path (file-directory-p path)))))

(define-obsolete-function-alias 'org-upwell-trace-folder-p
  'org-upwell-trace-directory-p "0.2"
  "Renamed: a directory is a directory everywhere but in Windows\=' own
vocabulary, and this package speaks its own.")

(defun org-upwell-trace-to-spec (trace)
  "Turn TRACE into an item spec plist.

`:kind\=' is decided by `org-upwell-trace-directory-p\=' rather than copied
from the trace: the Windows watcher reports a selected directory as a
file, so the field alone is not the answer.  It goes to the store as a
string, because a property value is a string."
  (list :name (plist-get trace :name)
        :path (plist-get trace :path)
        :url (plist-get trace :url)
        :office (plist-get trace :office)
        :file-id (plist-get trace :file-id)
        :kind (if (org-upwell-trace-directory-p trace) "dir" "file")
        :provenance "trace"))

(provide 'org-upwell-trace)

;;; org-upwell-trace.el ends here
