;;; org-upwell-core.el --- Identity of the stuff work is done with  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dependency root.  Everything else requires this file and this file
;; requires nothing of ours.
;;
;; An item is not a path.  Paths, URLs and Office protocols are
;; appearances of one identity.  Claims point *up* at a user heading's org-id
;; -- a project, a task, a meeting -- never the other way around.  The heading
;; does not list files; the list is a query.
;;
;; upwell.org is storage.  Its headings are not the work.  They carry no TODO
;; keyword and the file is kept out of `org-agenda-files', for the same reason
;; org-convect keeps goals out of the daily agenda.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-upwell nil
  "Files and URLs rising to the heading that is being lived."
  :group 'org
  :prefix "org-upwell-")

(defcustom org-upwell-file-name "upwell.org"
  "Basename of the store, under `org-upwell-directory'."
  :type 'string
  :group 'org-upwell)

(defcustom org-upwell-directory nil
  "Directory of the store, or nil to use `org-directory'.

The demo redirects this rather than `org-directory', so invented files
never land among the real ones."
  :type '(choice (const :tag "org-directory" nil) directory)
  :group 'org-upwell)

(defcustom org-upwell-trace-directory nil
  "Directory of the watcher's daily JSONL files.

Nil means `~/.local/share/org-upwell/'.  The watcher and Emacs must agree,
which is why the default is a path that does not depend on `org-directory'
-- the watcher runs without Emacs."
  :type '(choice (const :tag "XDG data home" nil) directory)
  :group 'org-upwell)

(defcustom org-upwell-open-function nil
  "Function of one path used to open a local file, or nil.

Nil uses `org-open-file'.  Dotfiles that already have an open policy
\(office files to the OS, and so on) should set this to that function."
  :type '(choice (const :tag "org-open-file" nil) function)
  :group 'org-upwell)

(defconst org-upwell-prop-flag "UPWELL"
  "Property whose presence marks a heading as a stored item.")

(defconst org-upwell-prop-path "UPWELL_PATH")
(defconst org-upwell-prop-url "UPWELL_URL")
(defconst org-upwell-prop-office "UPWELL_OFFICE")
(defconst org-upwell-prop-file-id "UPWELL_FILE_ID")
(defconst org-upwell-prop-claims "UPWELL_CLAIMS")
(defconst org-upwell-prop-provenance "UPWELL_PROVENANCE")
(defconst org-upwell-prop-captured "UPWELL_CAPTURED")
(defconst org-upwell-prop-opened "UPWELL_OPENED")
(defconst org-upwell-prop-stale "UPWELL_STALE")

(defvar org-upwell-last-expanded-id nil
  "Org-id of the heading last expanded.

Used when a new heading is clocked in: items already claimed to this
id is provisionally claimed to the new heading, so a DONE job's files
follow the next action without being pinned again.")

;;;; Paths

(defun org-upwell-file ()
  "Absolute path of the store.

Resolved on each call rather than at load: `org-directory' is often set
after this library is loaded."
  (expand-file-name org-upwell-file-name
                    (or org-upwell-directory
                        (bound-and-true-p org-directory)
                        "~/")))

(defun org-upwell-trace-directory ()
  "Absolute directory of the daily trace logs."
  (file-name-as-directory
   (expand-file-name
    (or org-upwell-trace-directory
        (expand-file-name "org-upwell"
                          (or (getenv "XDG_DATA_HOME")
                              (expand-file-name ".local/share"
                                                (expand-file-name "~"))))))))

(defun org-upwell-trace-file (&optional time)
  "JSONL path for the local day of TIME (now when omitted)."
  (expand-file-name
   (format "trace-%s.jsonl" (format-time-string "%Y-%m-%d" time))
   (org-upwell-trace-directory)))

(defun org-upwell--script (name)
  "Path of bundled helper NAME.

Looked for under `script/' beside this library, then beside it: a package
manager may flatten the directory, and the watcher should still find its
half of the contract."
  (let* ((dir (file-name-directory (or (locate-library "org-upwell")
                                       (locate-library "org-upwell-core")
                                       default-directory)))
         (candidates (list (expand-file-name (concat "script/" name) dir)
                           (expand-file-name name dir))))
    (or (seq-find #'file-exists-p candidates)
        (car candidates))))

;;;; File-id

(defun org-upwell-file-id (path)
  "Return a durable id for PATH, or nil if it cannot be read.

Emacs's inode and device numbers survive a rename on the same volume.
They are the portable half of the Windows file index / macOS ino pair;
the watcher may also send its own id, which is stored as given."
  (when (and path (file-exists-p path))
    (let* ((attrs (file-attributes path 'integer))
           (ino (and attrs (file-attribute-inode-number attrs)))
           (dev (and attrs (file-attribute-device-number attrs))))
      (when (and ino dev)
        (format "%s:%s" dev ino)))))

(defun org-upwell-file-id-equal (a b)
  "Return non-nil when file-ids A and B name the same object."
  (and a b (string= a b)))

;;;; Office protocol

(defun org-upwell-mint-office (url)
  "Return an `ms-*:ofe|u|' protocol for URL, or nil if it is not Office.

SharePoint and OneDrive links are the reason this exists: a browser URL
does not open in Excel, and the hand-built prefix is the whole ritual
this package is here to stop repeating."
  (when (and url (stringp url) (not (string-empty-p url)))
    (let ((url (string-trim url)))
      (cond
       ((string-match-p "\\`ms-\\(excel\\|powerpoint\\|word\\):" url) url)
       ((not (string-match-p "\\`https?://" url)) nil)
       (t
        (let ((app
               (cond
                ((string-match-p "/:x:/" url) "excel")
                ((string-match-p "/:p:/" url) "powerpoint")
                ((string-match-p "/:w:/" url) "word")
                ((string-match-p "\\.xlsx\\(\\|[?#]\\)" url) "excel")
                ((string-match-p "\\.xlsm\\(\\|[?#]\\)" url) "excel")
                ((string-match-p "\\.xls\\(\\|[?#]\\)" url) "excel")
                ((string-match-p "\\.pptx\\(\\|[?#]\\)" url) "powerpoint")
                ((string-match-p "\\.ppt\\(\\|[?#]\\)" url) "powerpoint")
                ((string-match-p "\\.docx\\(\\|[?#]\\)" url) "word")
                ((string-match-p "\\.doc\\(\\|[?#]\\)" url) "word")
                ((string-match-p "onedrive\\.live\\.com.*excel" url) "excel")
                ((string-match-p "office\\.com/launch/excel" url) "excel")
                ((string-match-p "office\\.com/launch/powerpoint" url) "powerpoint")
                ((string-match-p "office\\.com/launch/word" url) "word"))))
          (when app
            (concat "ms-" app ":ofe|u|" url))))))))

(defun org-upwell--looks-like-url (s)
  "Return non-nil when S is an http(s) URL."
  (and s (string-match-p "\\`https?://" s)))

;;;; Claims

(defconst org-upwell-claim-statuses '(provisional confirmed rejected)
  "Statuses a claim may carry.

`provisional' is what a machine wrote -- the watcher, or the clock
intersection.  `confirmed' and `rejected' are what a person said, and
neither is undone by a later intersection.")

(defun org-upwell--parse-claims (s)
  "Parse UPWELL_CLAIMS string S into a list of (:id ID :status STATUS).
STATUS is one of `org-upwell-claim-statuses'."
  (let (out)
    (dolist (part (and s (split-string s "," t "[ \t\n]+")) out)
      (let* ((bits (split-string part "|" t))
             (word (and (cadr bits) (string-remove-prefix ":" (cadr bits))))
             (status (car (member (and word (intern-soft word))
                                  org-upwell-claim-statuses))))
        (when (car bits)
          (push (list :id (car bits)
                      :status (or status 'provisional))
                out))))
    (nreverse out)))

(defun org-upwell--format-claims (claims)
  "Serialize CLAIMS back into a property value."
  (mapconcat
   (lambda (c)
     (format "%s|%s"
             (plist-get c :id)
             (or (car (member (plist-get c :status) org-upwell-claim-statuses))
                 'provisional)))
   claims
   ","))

(defun org-upwell-claim-status (claims heading-id)
  "Return the status of HEADING-ID in CLAIMS, or nil."
  (let ((hit (seq-find (lambda (c) (equal (plist-get c :id) heading-id))
                       claims)))
    (and hit (plist-get hit :status))))

(defun org-upwell-claims-put (claims heading-id status)
  "Return CLAIMS with HEADING-ID set to STATUS.

An answer a person gave is never overwritten by a machine: the watcher
and the clock intersection write `provisional', and neither `confirmed'
nor `rejected' gives way to it.  A person may still change their mind --
a pin or the review writes those two directly."
  (let ((found nil)
        out)
    (dolist (c claims)
      (if (equal (plist-get c :id) heading-id)
          (progn
            (setq found t)
            (push (list :id heading-id
                        :status (if (and (eq status 'provisional)
                                         (memq (plist-get c :status)
                                               '(confirmed rejected)))
                                    (plist-get c :status)
                                  status))
                  out))
        (push c out)))
    (unless found
      (push (list :id heading-id :status status) out))
    (nreverse out)))

(defun org-upwell-claims-remove (claims heading-id)
  "Return CLAIMS without HEADING-ID."
  (seq-remove (lambda (c) (equal (plist-get c :id) heading-id)) claims))

(defun org-upwell-claim-live-p (claim)
  "Return non-nil when CLAIM attaches its item to a heading.

A rejection is recorded as a claim so a later intersection does not
propose it again, but it does not put the file on the heading."
  (memq (plist-get claim :status) '(provisional confirmed)))

(defun org-upwell-unclaimed-p (item)
  "Return non-nil when no heading holds ITEM.

An item every heading has rejected is unclaimed again: the file is
still a signal, it just has nowhere to rise to."
  (null (seq-filter #'org-upwell-claim-live-p (plist-get item :claims))))

;;;; Store

(defun org-upwell--ensure-file ()
  "Create the store if it does not exist.  Return its path."
  (let ((f (org-upwell-file)))
    (unless (file-exists-p f)
      (make-directory (file-name-directory f) t)
      (with-temp-file f
        (insert "#+title: upwell\n"
                "#+startup: overview\n\n"
                "# Stored items.  Not work.  Not in the agenda.\n")))
    f))

(defun org-upwell--heading-plist ()
  "Read the store heading at point into a plist.  Point must be on it."
  (let* ((id (org-entry-get (point) "ID"))
         (title (org-get-heading t t t t))
         (path (org-entry-get (point) org-upwell-prop-path))
         (url (org-entry-get (point) org-upwell-prop-url))
         (office (org-entry-get (point) org-upwell-prop-office))
         (file-id (org-entry-get (point) org-upwell-prop-file-id))
         (claims (org-upwell--parse-claims
                  (org-entry-get (point) org-upwell-prop-claims))))
    (list :id id
          :name title
          :path path
          :url url
          :office office
          :file-id file-id
          :claims claims
          :provenance (org-entry-get (point) org-upwell-prop-provenance)
          :captured (org-entry-get (point) org-upwell-prop-captured)
          :opened (org-entry-get (point) org-upwell-prop-opened)
          :stale (org-entry-get (point) org-upwell-prop-stale)
          :marker (point-marker))))

(defvar org-upwell--store nil
  "Items read once, while `org-upwell-with-store' holds the store open.

Nil outside that form, and every read walks the file again -- which is right
for a command, where the file may have been edited since the last one.")

(defvar org-upwell--store-held nil
  "Non-nil while `org-upwell-with-store' is holding the store open.")

(defvar org-upwell--store-walks 0
  "How many times the file has actually been walked.  Read by the tests.")

(defmacro org-upwell-with-store (&rest body)
  "Run BODY with the store read once and written once.

Reading is the expensive half.  `org-upwell-save' asks
`org-upwell-find-any' whether an item is already stored, and that asks
`org-upwell-find' up to five times, and each of those used to walk the whole
file.  One trace therefore walked a hundred-heading store several times over,
and a background pass over a day of traces walked it hundreds of times --
seconds of work, and enough consing to send the garbage collector round
several times in the middle of somebody's typing.

Writing is the other half.  Saving after every item wrote the file once per
trace.  Here the writing waits until the end and happens once, if anything
changed at all."
  (declare (indent 0) (debug t))
  `(if org-upwell--store-held
       (progn ,@body)                   ; already held; do not save twice
     (let ((org-upwell--store nil)
           (org-upwell--store-held t))
       (unwind-protect (progn ,@body)
         (org-upwell--save-store)))))

(defun org-upwell--save-store ()
  "Write the store if anything changed.  Quiet: this runs from a timer."
  (let ((buf (get-file-buffer (org-upwell-file))))
    (when (and buf (buffer-modified-p buf))
      (with-current-buffer buf
        (let ((save-silently t))
          (save-buffer))))))

(defun org-upwell-items ()
  "Return every stored item as a plist.

The store is small -- live work, not an archive -- so this is a walk, not an
index.  Inside `org-upwell-with-store' the walk happens once and the rest of
the reads come back from what it found."
  (or org-upwell--store
      (let ((f (org-upwell-file))
            items)
        (when (file-exists-p f)
          (cl-incf org-upwell--store-walks)
          (with-current-buffer (find-file-noselect f)
            (org-with-wide-buffer
             (org-map-entries
              (lambda ()
                (when (org-entry-get (point) org-upwell-prop-flag)
                  (push (org-upwell--heading-plist) items)))
              nil 'file))))
        (setq items (nreverse items))
        (when org-upwell--store-held
          (setq org-upwell--store items))
        items)))

(defun org-upwell--store-remember (item)
  "Put ITEM into the held store, replacing the entry with the same id.

Dropping the whole thing instead would make the next read walk the file
again, which is one walk per write -- the cost this exists to remove."
  (when org-upwell--store-held
    (let ((id (plist-get item :id)))
      (setq org-upwell--store
            (append (seq-remove (lambda (m) (equal (plist-get m :id) id))
                                org-upwell--store)
                    (list item))))))

(defun org-upwell-find (key value)
  "Return the first item whose KEY equals VALUE.

KEY is `:id', `:path', `:url', `:office' or `:file-id'."
  (seq-find (lambda (m)
              (let ((v (plist-get m key)))
                (and v value (string= v value))))
            (org-upwell-items)))

(defun org-upwell-find-any (spec)
  "Return an existing item matching SPEC, a plist of appearances.

Match order: id, file-id, path, url, office.  The first hit wins, so a
renamed file that kept its file-id is found before a new path is created."
  (or (and (plist-get spec :id)
           (org-upwell-find :id (plist-get spec :id)))
      (and (plist-get spec :file-id)
           (org-upwell-find :file-id (plist-get spec :file-id)))
      (and (plist-get spec :path)
           (org-upwell-find :path (plist-get spec :path)))
      (and (plist-get spec :url)
           (org-upwell-find :url (plist-get spec :url)))
      (and (plist-get spec :office)
           (org-upwell-find :office (plist-get spec :office)))))

(defun org-upwell--unchanged-p (item existing)
  "Return non-nil when writing ITEM would leave the store exactly as it is.

The background pass sees the same traces again every time it runs, and
rewriting a heading with the values it already holds costs a property edit
apiece and marks the file dirty, so the store is written out once a minute
for nothing."
  (and existing
       (equal (org-upwell--format-claims (plist-get item :claims))
              (org-upwell--format-claims (plist-get existing :claims)))
       (seq-every-p (lambda (key)
                      (equal (plist-get item key) (plist-get existing key)))
                    '(:id :name :path :url :office :file-id
                      :provenance :captured :opened :stale))))

(defun org-upwell--write-at-point (item)
  "Write ITEM's properties onto the heading at point."
  (org-entry-put (point) org-upwell-prop-flag "t")
  (when (plist-get item :id)
    (org-entry-put (point) "ID" (plist-get item :id)))
  (dolist (pair `((,org-upwell-prop-path :path)
                  (,org-upwell-prop-url :url)
                  (,org-upwell-prop-office :office)
                  (,org-upwell-prop-file-id :file-id)
                  (,org-upwell-prop-provenance :provenance)
                  (,org-upwell-prop-captured :captured)
                  (,org-upwell-prop-opened :opened)))
    (let ((val (plist-get item (cadr pair))))
      (if (and val (not (string-empty-p val)))
          (org-entry-put (point) (car pair) val)
        (org-entry-delete (point) (car pair)))))
  (if (plist-get item :stale)
      (org-entry-put (point) org-upwell-prop-stale "t")
    (org-entry-delete (point) org-upwell-prop-stale))
  (let ((claims (plist-get item :claims)))
    (if claims
        (org-entry-put (point) org-upwell-prop-claims
                       (org-upwell--format-claims claims))
      (org-entry-delete (point) org-upwell-prop-claims))))

(defun org-upwell-save (item)
  "Insert or update ITEM in the store.  Return the saved plist.

Identity is the `:id'.  An item without one is given a UUID.  Appearances
already stored are filled in, never cleared, by a save that omits them --
a trace that knows the path must not wipe a protocol the pin already had.
`:claims' and `:stale' are the exceptions: a caller that names them owns
them, so the last claim can actually be taken off."
  (org-upwell--ensure-file)
  (let* ((existing (or (and (plist-get item :id)
                            (org-upwell-find :id (plist-get item :id)))
                       (org-upwell-find-any item)))
         (id (or (plist-get item :id)
                 (and existing (plist-get existing :id))
                 (org-id-uuid)))
         (merged
          (list :id id
                :name (or (plist-get item :name)
                          (and existing (plist-get existing :name))
                          (and (plist-get item :path)
                               (file-name-nondirectory (plist-get item :path)))
                          (or (plist-get item :url) "file"))
                :path (or (plist-get item :path)
                          (and existing (plist-get existing :path)))
                :url (or (plist-get item :url)
                         (and existing (plist-get existing :url)))
                :office (or (plist-get item :office)
                            (and existing (plist-get existing :office)))
                :file-id (or (plist-get item :file-id)
                             (and existing (plist-get existing :file-id)))
                :claims (if (plist-member item :claims)
                            (plist-get item :claims)
                          (and existing (plist-get existing :claims)))
                :provenance (or (plist-get item :provenance)
                                (and existing (plist-get existing :provenance)))
                :captured (or (and existing (plist-get existing :captured))
                              (plist-get item :captured)
                              (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                :opened (or (plist-get item :opened)
                            (and existing (plist-get existing :opened)))
                :stale (if (plist-member item :stale)
                           (plist-get item :stale)
                         (and existing (plist-get existing :stale)))
                :marker (and existing (plist-get existing :marker)))))
    (when (and (plist-get merged :url) (not (plist-get merged :office)))
      (let ((minted (org-upwell-mint-office (plist-get merged :url))))
        (when minted (setq merged (plist-put merged :office minted)))))
    (when (and (plist-get merged :path)
               (not (plist-get merged :file-id))
               (file-exists-p (plist-get merged :path)))
      (setq merged (plist-put merged :file-id
                              (org-upwell-file-id (plist-get merged :path)))))
    (when (org-upwell--unchanged-p merged existing)
      (org-upwell--store-remember merged)
      (setq merged nil))
    (when merged
     (with-current-buffer (find-file-noselect (org-upwell-file))
      (org-with-wide-buffer
       (if (and (plist-get merged :marker)
                (eq (marker-buffer (plist-get merged :marker))
                    (current-buffer)))
           (goto-char (plist-get merged :marker))
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert "* " (plist-get merged :name) "\n")
         (forward-line -1))
       (org-upwell--write-at-point merged)
       (let ((name (plist-get merged :name)))
         (unless (equal (org-get-heading t t t t) name)
           (org-edit-headline name)))
       (setq merged (plist-put merged :marker (point-marker)))
       (unless org-upwell--store-held
         (save-buffer)))))
    (if merged
        (progn (org-upwell--store-remember merged) merged)
      existing)))

(defun org-upwell-claim (item heading-id status)
  "Claim ITEM to HEADING-ID at STATUS.  Return the saved item.

Confirmed is sticky: a later provisional write for the same heading does
not undo a person having said this belongs there."
  (let* ((m (if (plist-get item :id) item (org-upwell-save item)))
         (claims (org-upwell-claims-put (plist-get m :claims)
                                        heading-id status)))
    (org-upwell-save (plist-put (copy-sequence m) :claims claims))))

(defun org-upwell-unclaim (item heading-id)
  "Forget that HEADING-ID was ever considered for ITEM.

The intersection is free to propose it again.  To say no and have it
stay said, use `org-upwell-reject'."
  (let ((claims (org-upwell-claims-remove (plist-get item :claims)
                                          heading-id)))
    (org-upwell-save (plist-put (copy-sequence item) :claims claims))))

(defun org-upwell-reject (item heading-id)
  "Record that ITEM does not belong to HEADING-ID.

The clock intersection runs again every minute and would otherwise
attribute the same trace to the same heading, so a rejection has to be
written down.  It is a person's answer, and `org-upwell-claims-put'
keeps it against later provisional writes."
  (org-upwell-claim item heading-id 'rejected))

(defun org-upwell-claimed-to (heading-id)
  "Return items that claim HEADING-ID, confirmed first.

Rejections are claims in the store but not on the heading: they are
what stops the intersection asking twice, not a file to open."
  (let ((items (seq-filter
                (lambda (m) (memq (org-upwell-claim-status
                                   (plist-get m :claims) heading-id)
                                  '(provisional confirmed)))
                (org-upwell-items))))
    (seq-sort
     (lambda (a b)
       (let ((sa (org-upwell-claim-status (plist-get a :claims) heading-id))
             (sb (org-upwell-claim-status (plist-get b :claims) heading-id)))
         (cond
          ((and (eq sa 'confirmed) (not (eq sb 'confirmed))) t)
          ((and (eq sb 'confirmed) (not (eq sa 'confirmed))) nil)
          (t (string< (or (plist-get a :name) "")
                      (or (plist-get b :name) ""))))))
     items)))

(defun org-upwell-heading-id (&optional marker)
  "Return (creating if needed) the org-id of the user heading at MARKER.

This is the only write this package makes into the user's work files:
an id, so a claim can survive refile.  They already use ids for links."
  (org-with-point-at (or marker (point))
    (org-back-to-heading t)
    (org-id-get-create)))

(provide 'org-upwell-core)

;;; org-upwell-core.el ends here
