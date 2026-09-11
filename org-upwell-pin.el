;;; org-upwell-pin.el --- Catch a file or URL without classifying it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pin, drop, and org-protocol are the explicit catch.  They never ask
;; where something belongs.  A running clock, or a heading at point, is
;; a confirmed claim; otherwise the item is unclaimed.
;;
;; Dropping onto an Org heading or an agenda row is a claim, not a move.
;; The file stays where the OS left it.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-protocol)
(require 'org-upwell-core)
(require 'dnd)
(require 'seq)

(declare-function org-upwell--maybe-refresh-bench "org-upwell-bench")
(declare-function org-upwell-bench-id "org-upwell-bench" (id))
(defvar org-upwell-bench-domain)

(defun org-upwell--current-heading-marker ()
  "Return a marker for the heading that should receive an explicit pin.

Priority: Org heading at point, agenda row, running clock.  Nil means
the pin is unclaimed -- catching and deciding stay different acts."
  (cond
   ((and (derived-mode-p 'org-mode)
         (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (point-marker)))
   ((and (derived-mode-p 'org-upwell-bench-mode)
         org-upwell-bench-domain
         (plist-get org-upwell-bench-domain :marker)))
   ((and (derived-mode-p 'org-agenda-mode)
         ;; A row put there by a custom block may carry only `org-marker'.
         ;; Reading just one of the two is how the agenda came to fall
         ;; through to a completing-read of every heading.
         (or (org-get-at-bol 'org-hd-marker)
             (org-get-at-bol 'org-marker))))
   ((and (markerp org-clock-hd-marker)
         (marker-buffer org-clock-hd-marker))
    org-clock-hd-marker)
   (t nil)))

(defun org-upwell--spec-from-path (path &optional provenance)
  "Build an item spec from PATH, minting an office protocol when it is a URL."
  (let* ((path (and path (string-trim path)))
         (url (and path (org-upwell--looks-like-url path) path))
         (file (and path (not url) (expand-file-name path))))
    (list :name (or (org-upwell-basename file)
                    url
                    "file")
          :path (and file (file-exists-p file) file)
          :url url
          :office (and url (org-upwell-mint-office url))
          :file-id (and file (org-upwell-file-id file))
          :provenance (or provenance "pin"))))

(defun org-upwell-pin (path &optional marker provenance)
  "Catch PATH (a file or a URL) and return the saved item.

MARKER, when given, is the user heading to claim confirmed.  When nil,
the heading at point / the running clock is used; when those are also
nil, the item is unclaimed.  PATH has to be a file that is there, or a
URL; there is nothing to catch otherwise."
  (interactive
   (list (read-file-name "Pin: " nil nil t
                         (or (buffer-file-name) default-directory))))
  (let* ((spec (org-upwell--spec-from-path path (or provenance "pin")))
         (_ (unless (or (plist-get spec :path)
                        (plist-get spec :url)
                        (plist-get spec :office))
              ;; An item is its appearances.  One with none can never be
              ;; resolved or opened, and would sit on the foresight board
              ;; as an unclaimed name forever.
              (user-error "org-upwell: no such file, and not a URL: %s" path)))
         (saved (org-upwell-save spec))
         (dest (or marker (org-upwell--current-heading-marker))))
    (when dest
      (setq saved (org-upwell-claim saved (org-upwell-heading-id dest)
                                    'confirmed))
      (when (fboundp 'org-upwell--maybe-refresh-bench)
        (org-upwell--maybe-refresh-bench dest)))
    (when (called-interactively-p 'interactive)
      (message "org-upwell: pinned %s%s"
               (plist-get saved :name)
               (if dest
                   (format " → %s"
                           (org-with-point-at dest
                             (org-get-heading t t t t)))
                 " (unclaimed)")))
    saved))

(defun org-upwell-pin-url (url &optional title)
  "Catch URL.  TITLE is used as the name when given."
  (interactive "sURL: ")
  (let ((saved (org-upwell-pin url nil "pin")))
    (when (and title (not (string-empty-p title)))
      (setq saved (org-upwell-save (plist-put (copy-sequence saved) :name title))))
    saved))

(defun org-upwell-dnd-file (uri _action)
  "DND handler for a local file.  Claims; never moves."
  (let ((path (dnd-get-local-file-name uri t)))
    (when path
      (org-upwell-pin path nil "drop")
      'private)))

(defun org-upwell-dnd-url (uri _action)
  "DND handler for an http(s) URL."
  (org-upwell-pin uri nil "drop")
  'private)

(defconst org-upwell-dnd-handlers
  '(("^file:" . org-upwell-dnd-file)
    ("^https?:" . org-upwell-dnd-url))
  "Drop handlers this package puts in front of `dnd-protocol-alist'.")

(defun org-upwell-enable-dnd ()
  "Accept file and URL drops as pins in this buffer.

Installed on Org, agenda and the bench.  Returning `private' from the
handler is what stops Emacs treating the drop as a move into the
buffer's directory -- dired's own DND is a move, and that is the
wrong act here.

Idempotent: the bench redraws on every heading it follows, and an
`append' would have grown the buffer's alist by two entries each time."
  (setq-local dnd-protocol-alist
              (append org-upwell-dnd-handlers
                      (seq-remove (lambda (h) (member h org-upwell-dnd-handlers))
                                  dnd-protocol-alist))))

(defun org-upwell-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://upwell?url=URL&title=TITLE
  org-protocol://upwell?path=PATH
  org-protocol://upwell?bench=ID

The first two pin.  The third lays a heading out, by org-id.

`expand=\=' is taken as `bench=\=' too.  It was the name before the command
was, and a URL already sitting in somebody\='s bookmarks or shell history is
not something a rename gets to break."
  (let ((url (plist-get info :url))
        (path (plist-get info :path))
        (title (plist-get info :title))
        (bench (or (plist-get info :bench) (plist-get info :expand))))
    (cond
     (bench
      (require 'org-upwell-bench)
      (org-upwell-bench-id bench)
      nil)
     (path
      (org-upwell-pin path nil "protocol")
      nil)
     (url
      (org-upwell-pin-url url title)
      nil)
     (t
      (message "org-upwell: protocol URL had no url=, path= or bench=")
      nil))))

(defun org-upwell-protocol-setup ()
  "Register the `upwell' org-protocol."
  (add-to-list 'org-protocol-protocol-alist
               '("org-upwell"
                 :protocol "upwell"
                 :function org-upwell-protocol)))

(defcustom org-upwell-create-directory "~/Documents/upwell/"
  "Where `org-upwell-create' puts a new file.

Overridden per heading by the `:UPWELL_DIR:' property.  Asking for a
location every time is the ritual this command exists to stop, so there
is a default rather than a prompt."
  :type 'directory
  :group 'org-upwell)

(defun org-upwell-create (kind)
  "Create a new file of KIND and claim it to the heading at point.

KIND is `xlsx', `pptx', `docx', `md' or `txt'.  The location is not
asked: `:UPWELL_DIR:' on the heading, else `org-upwell-create-directory'."
  (interactive
   (list (intern (completing-read "Create: " '("xlsx" "pptx" "docx" "md" "txt")
                                  nil t))))
  (let* ((marker (or (org-upwell--current-heading-marker)
                     (user-error "No heading at point and no running clock")))
         (title (org-with-point-at marker
                  (org-get-heading t t t t)))
         (dir (or (org-with-point-at marker
                    (let ((d (org-entry-get (point) "UPWELL_DIR" t)))
                      (and d (file-directory-p (expand-file-name d))
                           (expand-file-name d))))
                  (expand-file-name org-upwell-create-directory)))
         (name (format "%s-%s.%s"
                       (replace-regexp-in-string "[/\\\\:]" "-" title)
                       (format-time-string "%Y%m%d")
                       kind))
         (path (expand-file-name name dir)))
    (make-directory dir t)
    (unless (file-exists-p path)
      (write-region "" nil path))
    (org-upwell-pin path marker "create")))

(provide 'org-upwell-pin)

;;; org-upwell-pin.el ends here
