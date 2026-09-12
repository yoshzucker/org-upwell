;;; org-upwell-sight.el --- What Emacs itself sees  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The resident watcher reads the frontmost window of the desktop, and its
;; branches are the applications work arrives in: a browser, Excel,
;; PowerPoint, Word, the file manager.  Emacs is in none of them.  So for as
;; long as Emacs is the window in front, nothing is recorded at all -- not
;; the directory dired is showing, not the file being edited.  For anybody
;; who does their directory work in dired rather than in Explorer, that is
;; the whole of capture missing.
;;
;; This is the other half: Emacs writing down what Emacs is looking at, into
;; the same JSONL the watcher appends to.  Nothing downstream is told the
;; difference -- the clock intersection that turns a sighting into a claim
;; reads one file and does not care which process wrote a line.  That is
;; also why a clock filled in after the fact still picks these up.
;;
;; What is worth writing down is not everything Emacs opens.  A file under
;; `org-upwell-search-roots' is work; one outside them is a configuration
;; file, a library, an agenda, or this package's own store, and a bench
;; buried in those would be worse than a bench missing a file.  The roots
;; are a setting that already exists and already means "the trees my work
;; lives in", so the condition is one somebody can read rather than one they
;; have to infer.
;;
;; A sighting is written when what you are looking at changes, which is the
;; rule the watcher follows too (`lastFront' in the AutoHotkey script).  It
;; has the same consequence: sitting in one directory across a clock-out and
;; a clock-in leaves the second clock without a sighting.  The watcher has
;; always behaved that way, and `org-foresight' filling the clock in
;; afterwards covers the case that matters -- the interval is what moves,
;; not the trace.

;;; Code:

(require 'org-upwell-core)
(require 'org-upwell-trace)

(defcustom org-upwell-sight-dired t
  "When non-nil, a directory dired is showing is written down.

The reason this file exists.  Explorer and Finder are watched from
outside; dired is not watched by anything, and a person who does their
directory work in it has no way to tell org-upwell where the work is
being done."
  :type 'boolean
  :group 'org-upwell)

(defcustom org-upwell-sight-files t
  "When non-nil, a file Emacs visits under the search roots is written down.

Limited to `org-upwell-search-roots' on purpose -- see the commentary in
org-upwell-sight.el.  Set to nil to record only directories."
  :type 'boolean
  :group 'org-upwell)

(defvar org-upwell-sight--buffer nil
  "The buffer last considered.  A pointer compare, so the hook is cheap.")

(defvar org-upwell-sight--payload nil
  "What was written down last, so the same thing is not written twice.")

(defun org-upwell-sight--own-p (file)
  "Return non-nil when FILE belongs to org-upwell rather than to the work.

The store and the trace logs can sit under a search root -- somebody may
keep everything in one tree -- and a package that recorded its own
bookkeeping as work would fill the bench with itself."
  (or (equal (file-truename file) (file-truename (org-upwell-file)))
      (file-in-directory-p file (org-upwell-trace-directory))))

(defun org-upwell-sight--worth-writing (file)
  "Return non-nil when FILE is work rather than machinery."
  (and file
       (not (org-upwell-sight--own-p file))
       (seq-some (lambda (root) (file-in-directory-p file root))
                 (org-upwell--search-roots))))

(defun org-upwell-sight--payload ()
  "What the current buffer is worth writing down, as a plist, or nil."
  (cond
   ;; The bench lists sightings; it is not one.
   ((derived-mode-p 'org-upwell-bench-mode) nil)
   ((and org-upwell-sight-dired (derived-mode-p 'dired-mode))
    (let ((dir (and (stringp default-directory)
                    (directory-file-name
                     (expand-file-name default-directory)))))
      (and dir (file-directory-p dir)
           (list :path dir :kind "dir" :title (buffer-name)))))
   ((and org-upwell-sight-files (buffer-file-name))
    (let ((file (expand-file-name (buffer-file-name))))
      (and (org-upwell-sight--worth-writing file)
           (list :path file :kind "file" :title (buffer-name)))))))

(defun org-upwell-sight--update ()
  "Write down what Emacs is looking at, when it has changed.

On `post-command-hook', so the first thing it does is the cheapest thing
it can: a pointer compare against the buffer it saw last."
  (unless (eq (current-buffer) org-upwell-sight--buffer)
    (setq org-upwell-sight--buffer (current-buffer))
    (when-let ((payload (ignore-errors (org-upwell-sight--payload))))
      (let ((key (plist-get payload :path)))
        (unless (equal key org-upwell-sight--payload)
          (setq org-upwell-sight--payload key)
          (ignore-errors (apply #'org-upwell-trace-write payload)))))))

;;;###autoload
(define-minor-mode org-upwell-sight-mode
  "Write down what Emacs itself is looking at.

The resident watcher cannot see inside Emacs, so without this a dired
directory and a file edited here are never recorded.  Turned on by
`org-upwell-mode'."
  :global t
  :group 'org-upwell
  (if org-upwell-sight-mode
      (add-hook 'post-command-hook #'org-upwell-sight--update)
    (remove-hook 'post-command-hook #'org-upwell-sight--update)
    (setq org-upwell-sight--buffer nil
          org-upwell-sight--payload nil)))

(provide 'org-upwell-sight)

;;; org-upwell-sight.el ends here
