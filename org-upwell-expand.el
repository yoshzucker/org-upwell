;;; org-upwell-expand.el --- Invoke a heading's domain  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Expand is not "open the folder" and not "restore Emacs windows".  It is
;; the heading's world: the Org entry itself, its LOCATION, its next
;; actions, and the files that claim it.  Most of what it opens lives
;; outside Emacs -- Excel, PowerPoint, a browser -- because that is where
;; the work is.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-clock)
(require 'org-upwell-core)
(require 'org-upwell-pin)
(require 'org-upwell-claim)
(require 'seq)
(require 'cl-lib)
(require 'button)
(require 'eldoc)

(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag))

(defcustom org-upwell-expand-max 8
  "Most items opened in one expand.

A domain with thirty files is a domain that cannot be laid on a desk.
The rest stay listed on the bench, one click each."
  :type 'integer
  :group 'org-upwell)

(defcustom org-upwell-expand-clock-in nil
  "When non-nil, expand clocks in if nothing else is running.

Off by default: expanding to look is not the same as starting the work,
and a clock nobody asked for is worse than no clock, because it has to
be found and corrected before the day's record can be read."
  :type 'boolean
  :group 'org-upwell)

(defcustom org-upwell-expand-open-location t
  "When non-nil, open LOCATION if it looks like a URL."
  :type 'boolean
  :group 'org-upwell)

(defcustom org-upwell-bench-width 0.5
  "Fraction of the host window's width given to the bench, when it
is cut side by side.

A normal window, not a side window, so the default is half of what
it was cut from.  Height, when the bench sits underneath, is the
number of lines in the buffer (capped at a third of the host), not
a fraction."
  :type 'float
  :group 'org-upwell)

;;;; Resolve / open

(defcustom org-upwell-search-roots '("~/Downloads/")
  "Directories to look in when a stored path has gone.

A file that was renamed, or filed away into a `done' folder, is found
again by its file-id or its basename under these.  Only directories that
exist are searched, so one list may name the trees of several machines.

Keep it short.  This is walked on every appearance that has gone stale,
and a root the size of a whole home directory turns a resolve into a
wait."
  :type '(repeat directory)
  :group 'org-upwell)

(defun org-upwell--search-roots ()
  "Existing directories among `org-upwell-search-roots'."
  (seq-filter #'file-directory-p
              (delq nil (mapcar (lambda (d) (and d (expand-file-name d)))
                                org-upwell-search-roots))))

(defun org-upwell-resolve (item)
  "Return a still-usable appearance of ITEM, updating the store.

Order: live path, file-id under the search roots, basename search,
office protocol, URL.  A miss marks the path stale; the item itself
does not die."
  (let ((path (plist-get item :path))
        (file-id (plist-get item :file-id))
        (office (plist-get item :office))
        (url (plist-get item :url))
        found)
    (cond
     ((and path (file-exists-p path))
      (when (plist-get item :stale)
        (org-upwell-save (plist-put (copy-sequence item) :stale nil)))
      (list :kind 'path :value path))
     ;; One search, not one to test the branch and another to take it:
     ;; each of these runs fd over the whole of Documents.
     ((and file-id
           (setq found (org-upwell--find-by-file-id file-id)))
      (org-upwell-save (list :id (plist-get item :id)
                             :path found
                             :stale nil
                             :file-id file-id))
      (list :kind 'path :value found))
     ((and path
           (setq found (org-upwell--find-by-basename
                        (file-name-nondirectory path))))
      (org-upwell-save (list :id (plist-get item :id)
                             :path found
                             :stale nil))
      (list :kind 'path :value found))
     ((and office (not (string-empty-p office)))
      (org-upwell-save (plist-put (copy-sequence item) :stale t))
      (list :kind 'office :value office))
     ((and url (not (string-empty-p url)))
      (org-upwell-save (plist-put (copy-sequence item) :stale t))
      (list :kind 'url :value url))
     (t
      (org-upwell-save (plist-put (copy-sequence item) :stale t))
      nil))))

(defun org-upwell--files-named (name)
  "Return absolute paths named NAME under `org-upwell-search-roots'.

Uses fd when it is on PATH.  A project tree big enough to be worth
searching is also big enough that a Lisp walk of it is a visible hitch."
  (let ((rx (concat "\\`" (regexp-quote name) "\\'"))
        hits)
    (dolist (root (org-upwell--search-roots) hits)
      (when (and root (file-directory-p root))
        (setq hits
              (append hits
                      (if (executable-find "fd")
                          (ignore-errors
                            (process-lines "fd" "-a" "-t" "f" "--glob" name root))
                        (directory-files-recursively root rx))))))))

(defun org-upwell--find-by-file-id (file-id)
  "Return a path whose file-id is FILE-ID.

Does not walk the whole of Documents: it stats files that share the
stored basename, which is the cheap way to recover a rename or a move
into `done&info'."
  (let* ((known (org-upwell-find :file-id file-id))
         (name (and known (plist-get known :path)
                    (file-name-nondirectory (plist-get known :path)))))
    (when name
      (seq-find (lambda (f) (org-upwell-file-id-equal file-id
                                                      (org-upwell-file-id f)))
                (org-upwell--files-named name)))))

(defun org-upwell--find-by-basename (name)
  "Return the first file named NAME under the search roots."
  (car (org-upwell--files-named name)))

(defun org-upwell-open (item &optional method)
  "Open ITEM via the first still-usable appearance.

METHOD is `external' (the OS default app; the default) or `emacs'
\(inside Emacs, never in the bench window).  The bench is a strip of
materials: replacing it with a file is the same class of mistake as
growing the frame to keep the main window's size."
  (let ((app (org-upwell-resolve item))
        (method (or method 'external)))
    (unless app
      (user-error "org-upwell: no live appearance for %s"
                  (plist-get item :name)))
    (org-upwell-save
     (plist-put (copy-sequence item)
                :opened (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (pcase (plist-get app :kind)
      ('path
       (if (eq method 'emacs)
           (org-upwell--open-path-emacs (plist-get app :value))
         (org-upwell--open-path-external (plist-get app :value))))
      ('office (org-upwell--open-url (plist-get app :value)))
      ('url (org-upwell--open-url (plist-get app :value))))))

(defun org-upwell--open-path-external (path)
  "Open PATH the way this machine opens files.

`org-upwell-open-function' is the whole of the policy when it is set:
a configuration that already knows which extensions belong to the OS
and which belong to Emacs should not have that knowledge duplicated
here.  It may put a buffer on screen, so it is called from a window
that is not the bench.  Unset, the OS default application is used and
nothing opens in Emacs."
  (cond
   (org-upwell-open-function
    (org-upwell--call-away-from-bench
     (lambda () (funcall org-upwell-open-function path))))
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)))
   ((eq system-type 'darwin)
    (start-process "org-upwell-open" nil "open" path))
   ((eq system-type 'gnu/linux)
    (start-process "org-upwell-open" nil "xdg-open" path))
   (t (user-error "No system open command for %S" system-type))))

(defun org-upwell--reveal-external (path)
  "Show PATH in this machine's file manager, selected where it can be.

Not `org-upwell-open-function': that setting decides which application a
*file* belongs to, and this is a request for the desktop's own window onto
the folder around it."
  (let ((dir (file-name-directory (directory-file-name path))))
    (cond
     ((eq system-type 'windows-nt)
      ;; explorer.exe answers 1 even when it worked, so the call is made
      ;; and not asked about.
      (call-process "explorer.exe" nil 0 nil
                    (concat "/select,"
                            (replace-regexp-in-string "/" "\\" path t t))))
     ((eq system-type 'darwin)
      (start-process "org-upwell-reveal" nil "open" "-R" path))
     ((eq system-type 'gnu/linux)
      (start-process "org-upwell-reveal" nil "xdg-open" dir))
     (t (user-error "No system reveal command for %S" system-type)))))

(defun org-upwell-open-folder (&optional item)
  "Open the folder ITEM lives in, with ITEM selected where the OS can.

The bench answers \"which of these two same-named files is it?\" with the
folder; this is how to go and stand in that folder.  A URL has no folder
and says so.  A path that moved is resolved first, so the folder opened is
the one the file is in now."
  (interactive)
  (let* ((m (or item
                (org-upwell--bench-item-at-point)
                (user-error "No item on this line")))
         (app (org-upwell-resolve m)))
    (unless (eq (plist-get app :kind) 'path)
      (user-error "org-upwell: %s is not a file on this machine"
                  (or (plist-get m :name) "this item")))
    (org-upwell--reveal-external (plist-get app :value))))

(defun org-upwell--bench-window-p (&optional window)
  "Return non-nil if WINDOW (default selected) shows the bench."
  (let ((buf (window-buffer (or window (selected-window)))))
    (eq (buffer-local-value 'major-mode buf) 'org-upwell-bench-mode)))

(defun org-upwell--window-away-from-bench ()
  "Return a window on this frame that is not the bench, or nil."
  (seq-find (lambda (w) (not (org-upwell--bench-window-p w)))
            (window-list nil 'nomini)))

(defun org-upwell--show-away-from-bench (buffer)
  "Display BUFFER on this frame without replacing the bench."
  (let ((win (org-upwell--window-away-from-bench)))
    (if win
        (progn (set-window-buffer win buffer)
               (select-window win))
      (let ((display-buffer-overriding-action
             '((display-buffer-pop-up-window)
               (inhibit-same-window . t))))
        (pop-to-buffer buffer)))))

(defun org-upwell--call-away-from-bench (fn)
  "Call FN with a window that is not the bench selected, if there is one.

The bench is a strip of materials: whatever FN decides to show, it must
not land in the listing the user is picking from."
  (let ((win (and (org-upwell--bench-window-p)
                  (org-upwell--window-away-from-bench))))
    (if (window-live-p win)
        (with-selected-window win (funcall fn))
      (funcall fn))))

(defun org-upwell--open-path-emacs (path)
  "Visit PATH in Emacs, in a window that is not the bench."
  (org-upwell--show-away-from-bench (find-file-noselect path)))

(defun org-upwell--open-url (url)
  "Open URL, including `ms-excel:ofe|u|' style protocols."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" url))
   ((eq system-type 'darwin)
    (start-process "org-upwell-open" nil "open" url))
   (t (browse-url url))))

;;;; Domain

(defun org-upwell-domain (&optional marker)
  "Return the domain of the user heading at MARKER (or point) as a plist."
  (org-upwell-with-store
   (org-with-point-at (or marker (point))
    (org-back-to-heading t)
    (let* ((id (org-id-get))
           (end (save-excursion (org-end-of-subtree t t)))
           nexts)
      (save-excursion
        (let ((level (org-current-level)))
          (while (and (outline-next-heading) (< (point) end))
            (when (and (= (org-current-level) (1+ level))
                       (member (org-get-todo-state) '("NEXT" "ONGO" "WAIT")))
              (push (org-get-heading t t t t) nexts)))))
      (list :marker (point-marker)
            :id id
            :title (org-get-heading t t t t)
            :todo (org-get-todo-state)
            :location (org-entry-get (point) "LOCATION" t)
            :people (org-entry-get (point) "PEOPLE" t)
            :area (org-entry-get (point) "CONVECT_AREA" t)
            :dir (org-entry-get (point) "UPWELL_DIR" t)
            :next (nreverse nexts)
            :items (and id (org-upwell-claimed-to id)))))))

;;;; Expand

(defun org-upwell--heading-label (marker)
  "Return a completing-read label for the heading at MARKER."
  (org-with-point-at marker
    (format "%s  [%s]"
            (org-get-heading t t t t)
            (file-name-nondirectory (or (buffer-file-name) "")))))

(defun org-upwell--expand-candidates ()
  "Return (LABEL . MARKER) for headings C-c v may expand from anywhere.

Running clock first, then today's clocked work, then open NEXT/ONGO,
then headings that already have files.  Titles are disambiguated by
file name so two \"File the photos\" do not collapse."
  (let (out seen)
    (cl-labels ((add (mk)
                  (when (and (markerp mk) (marker-buffer mk))
                    (let ((label (org-upwell--heading-label mk)))
                      (unless (member label seen)
                        (push (cons label mk) out)
                        (push label seen))))))
      (when (and (markerp org-clock-hd-marker)
                 (marker-buffer org-clock-hd-marker))
        (add org-clock-hd-marker))
      (dolist (seg (and (fboundp 'org-upwell-clock-segments)
                        (org-upwell-clock-segments 1)))
        (add (plist-get seg :marker)))
      (dolist (file (org-agenda-files))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (org-map-entries
             (lambda ()
               (when (member (org-get-todo-state) '("NEXT" "ONGO" "WAIT"))
                 (add (point-marker))))
             nil 'file))))
      (dolist (m (org-upwell-items))
        (dolist (c (plist-get m :claims))
          (when-let ((id (plist-get c :id))
                     (mk (org-id-find id 'marker)))
            (add mk)))))
    (nreverse out)))

(defun org-upwell--read-heading-marker ()
  "Completing-read a heading to expand.  Return its marker, or nil."
  (let ((cands (org-upwell--expand-candidates)))
    (unless cands
      (user-error "No heading to expand (clock something, or open a NEXT)"))
    (cdr (assoc (completing-read "Expand: " (mapcar #'car cands) nil t)
                cands))))

(defun org-upwell-expand (&optional marker choose)
  "Expand the domain of a heading.

With a heading at point (Org, agenda, or the bench), that heading.
With a running clock and nothing at point, the clocked heading.
From anywhere else, completing-read among today's clocks, open
NEXT/ONGO, and headings that already have files.  So C-c v is the
same key in every buffer.

With a prefix argument, CHOOSE is non-nil and the heading is always
read from that list, whatever point is on.

If the bench is showing, it switches to this heading.  If agenda
stole the window, it puts the bench back.  After `q', it stays
gone -- expanding does not open a listing the user dismissed.
Neither the bench nor an agenda gives up its window to the Org
file; from an Org buffer, point lands on the heading."
  (interactive (list nil current-prefix-arg))
  (let* ((marker (or marker
                     (and (not choose) (org-upwell--current-heading-marker))
                     (org-upwell--read-heading-marker)))
         (domain (org-upwell-domain marker))
         (items (plist-get domain :items))
         (opened 0)
         (here (selected-window)))
    (when (and org-upwell-expand-clock-in
               (not (org-clocking-p)))
      (org-with-point-at marker (org-clock-in)))
    (org-with-point-at marker
      (org-fold-show-entry)
      (org-fold-show-children))
    (when (and org-upwell-expand-open-location
               (org-upwell--looks-like-url (plist-get domain :location)))
      (org-upwell--open-url (plist-get domain :location)))
    (dolist (m items)
      (when (< opened org-upwell-expand-max)
        (condition-case err
            (progn (org-upwell-open m)
                   (setq opened (1+ opened)))
          (error (message "org-upwell: %s" (error-message-string err))))))
    (setq org-upwell-last-expanded-id (plist-get domain :id))
    (org-upwell--maybe-refresh-bench marker)
    ;; The window C-c v was pressed in keeps its buffer when that buffer
    ;; is the bench or an agenda.  Both are ways of reading the heading,
    ;; not somewhere to put its file: replacing the bench with the Org
    ;; file deleted the listing this very call had just redrawn.
    (when (and (window-live-p here)
               (not (org-upwell--bench-window-p here))
               (not (with-current-buffer (window-buffer here)
                      (derived-mode-p 'org-agenda-mode))))
      (with-selected-window here
        (unless (eq (current-buffer) (marker-buffer marker))
          (switch-to-buffer (marker-buffer marker)))
        (goto-char marker)
        (org-fold-show-entry)))
    (message "org-upwell: expanded \"%s\" (%d file%s)"
             (plist-get domain :title)
             opened
             (if (= opened 1) "" "s"))))

(defun org-upwell-expand-clock ()
  "Expand the heading currently being clocked."
  (interactive)
  (unless (and (markerp org-clock-hd-marker)
               (marker-buffer org-clock-hd-marker))
    (user-error "Nothing is being clocked"))
  (org-upwell-expand org-clock-hd-marker))

(defun org-upwell-expand-id (id)
  "Expand the heading whose org-id is ID."
  (let ((m (org-id-find id 'marker)))
    (unless m (user-error "org-upwell: no heading with id %s" id))
    (org-upwell-expand m)))

(defun org-upwell-open-named (name)
  "Open an item by NAME, completing over the store."
  (interactive
   (list (completing-read
          "File: "
          (mapcar (lambda (m) (plist-get m :name)) (org-upwell-items))
          nil t)))
  (let ((m (seq-find (lambda (it) (equal (plist-get it :name) name))
                     (org-upwell-items))))
    (unless m (user-error "No item named %s" name))
    (org-upwell-open m)))

;;;; Bench

(defvar-local org-upwell-bench-domain nil)
(defvar-local org-upwell-bench-marked nil
  "List of item ids marked in the bench, for opening a subset.")

(defvar org-upwell--follow-seen nil
  "Heading identity the bench was last drawn for, while following.")

(defvar org-upwell--bench-intent nil
  "nil if never shown, `wanted' if it should be on screen, `dismissed' after `q'.

Agenda's reorganize-frame deletes the window without dismissing.
C-c v and follow must put it back in that case, and must not after `q'.")

(defvar org-upwell-follow-mode nil)

(defun org-upwell--bench-host-window ()
  "Return the window the bench should be cut out of.

Not the window it was asked from: that one is holding the agenda row or
the heading being read, and taking its height is what makes the strip
feel like it arrived at the reader's expense.  Largest first, then least
recently used -- the order `display-buffer-pop-up-window' picks in, and
so the order the agenda itself lands by.  A frame with one window has
only that one to offer."
  (or (get-largest-window nil nil t)
      (get-lru-window nil nil t)
      (selected-window)))

(defun org-upwell--bench-split-side (&optional window)
  "Return `below' or `right' for a bench cut out of WINDOW.

Side by side only when WINDOW is wide enough that Emacs itself would
split it that way -- `split-width-threshold', the test
`split-window-sensibly' applies and the reason the agenda lands beside
its buffer on a wide frame rather than under it.  Anything narrower gets
the strip underneath.

Measured on the host window and not on the frame, because those are only
the same thing while the frame holds one window.  Two panes on a wide
frame are each too narrow to halve again, and a bench that halves the
frame anyway leaves a quarter, a quarter and a half."
  (let ((width (window-total-width (or window (org-upwell--bench-host-window)))))
    (if (and split-width-threshold (>= width split-width-threshold))
        'right
      'below)))

(defun org-upwell--bench-desired-height (buf host)
  "Lines to give a below-split bench for BUF cut out of HOST.

Dense, but not a one-liner: heading, file list, drop hint.  Floor of
10 so a handful of files is still clickable; cap at a third of HOST
so it cannot become a second agenda."
  (let* ((content (with-current-buffer buf
                    (count-lines (point-min) (point-max))))
         (total (window-total-height host)))
    (min (1- total)
         (max 10 (1+ content))
         (max 10 (/ total 3)))))

(defun org-upwell--bench-maybe-resize (win buf)
  "If WIN is a below-split bench, fit it to BUF's contents."
  (when (window-in-direction 'above win)
    (let* ((root (frame-root-window (window-frame win)))
           (want (org-upwell--bench-desired-height buf root))
           (delta (- want (window-total-height win))))
      (unless (zerop delta)
        (ignore-errors (window-resize win delta nil nil t))))))

(defun org-upwell--split-for-bench (host buf)
  "Cut a bench-sized window out of HOST for BUF, or return nil."
  (and (window-live-p host)
       (let* ((side (org-upwell--bench-split-side host))
              (give (if (eq side 'below)
                        (org-upwell--bench-desired-height buf host)
                      (max 16 (round (* (abs org-upwell-bench-width)
                                        (window-total-width host)))))))
         ;; Negative SIZE = size of the new window (Emacs split-window API).
         (ignore-errors (split-window host (- give) side)))))

(defun org-upwell--show-bench-buffer (buf)
  "Put BUF in a split of one window.  Never grow the frame.

Splits a single window the way agenda does, rather than the frame root:
a root split pushes every pane already on the frame into the half that
is left, so a bench asked for beside an agenda used to leave a quarter,
a quarter and a half.  Does not set `no-delete-other-windows': if agenda
reorganizes the frame, the bench is allowed to disappear."
  (let ((existing (get-buffer-window buf nil)))
    (if (window-live-p existing)
        (progn
          (set-window-buffer existing buf)
          (org-upwell--bench-maybe-resize existing buf)
          existing)
      (let* ((frame-inhibit-implied-resize t)
             (pxw (frame-pixel-width))
             (pxh (frame-pixel-height))
             (here (unless (org-upwell--bench-window-p) (selected-window)))
             (win (or (org-upwell--split-for-bench
                       (org-upwell--bench-host-window) buf)
                      ;; Nothing else on the frame could hold it: the
                      ;; window in hand is better than no bench.
                      (org-upwell--split-for-bench here buf)
                      (display-buffer buf
                                      '(display-buffer-pop-up-window)))))
        (when (window-live-p win)
          (set-window-buffer win buf)
          (unless (and (= pxw (frame-pixel-width))
                       (= pxh (frame-pixel-height)))
            (set-frame-size nil pxw pxh t)))
        win))))

(defun org-upwell-bench (&optional domain)
  "Show DOMAIN (or the heading at point) in a strip of this frame."
  (interactive)
  (setq org-upwell--bench-intent 'wanted)
  (let* ((domain (or domain
                     (and (org-upwell--current-heading-marker)
                          (org-upwell-domain
                           (org-upwell--current-heading-marker)))))
         (buf (get-buffer-create "*org-upwell*")))
    (unless domain
      (user-error "No heading to show"))
    (with-current-buffer buf
      (let ((old-id (and (derived-mode-p 'org-upwell-bench-mode)
                         org-upwell-bench-domain
                         (plist-get org-upwell-bench-domain :id))))
        (unless (derived-mode-p 'org-upwell-bench-mode)
          (org-upwell-bench-mode))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (equal old-id (plist-get domain :id))
            (setq org-upwell-bench-marked nil))
          (setq org-upwell-bench-domain domain)
          (org-upwell-enable-dnd)
          (unless (listp org-upwell-bench-marked)
            (setq org-upwell-bench-marked nil))
          ;; Title + store.  TODO/NEXT are not repeated: they already sit
          ;; on the heading.  The file list and the drop line stay -- they are
          ;; why this window exists.
          (insert (propertize (or (plist-get domain :title) "?")
                              'face 'org-level-1)
                  "  ")
          (insert-text-button
           (file-name-nondirectory (org-upwell-file))
           'follow-link t
           'help-echo (org-upwell-file)
           'action (lambda (_) (org-upwell-visit-store)))
          (insert "\nFiles\n")
          (if (null (plist-get domain :items))
              (insert (propertize "  (none)\n" 'face 'shadow))
            (let ((widths (org-upwell--bench-widths
                           (plist-get domain :items) buf)))
              (dolist (m (plist-get domain :items))
                (org-upwell--bench-insert-item m domain widths))))
          (insert (propertize "Drop a file or URL here to pin it to this heading.\n"
                              'face 'shadow))
          (goto-char (point-min))
          (setq buffer-read-only t))))
    (let ((win (org-upwell--show-bench-buffer buf)))
      (when (and (called-interactively-p 'interactive)
                 (window-live-p win))
        (select-window win))
      win)))

(defun org-upwell--maybe-refresh-bench (marker)
  "Redraw the bench for MARKER when it is showing or still wanted.

Expanding switches the list.  If agenda stole the window, C-c v
puts it back.  After `q', the bench stays gone."
  (when (or (get-buffer-window "*org-upwell*" nil)
            (eq org-upwell--bench-intent 'wanted))
    (org-upwell-bench (org-upwell-domain marker))))

(defun org-upwell--pad (s width)
  "Return S with spaces after it, to WIDTH columns."
  (concat s (make-string (max 0 (- width (string-width s))) ?\s)))

(defun org-upwell--tail (s width)
  "Return S in WIDTH columns, keeping the end when it has to be cut.

The end is the part that tells two of them apart: files with the same
name are in different folders, and the folder is the last thing on the
line before the name."
  (let ((w (string-width s)))
    (if (or (<= w width) (< width 2))
        s
      (concat "…" (truncate-string-to-width s w (- w (1- width)))))))

(defun org-upwell--url-host (url)
  "Return the host of URL, or nil.
Also reads the URL out of a minted office protocol, which has one inside it."
  (and url
       (string-match "[a-z][a-z0-9+.-]*://\\([^/?#]+\\)" url)
       (match-string 1 url)))

(defun org-upwell--item-where (item)
  "Return where ITEM is: the folder for a file, the host for a URL.

Never the name again.  A bench with two lines called the same thing is
answered by the folder, and by nothing else on the line."
  (let ((path (plist-get item :path))
        (url (or (plist-get item :url) (plist-get item :office))))
    (cond
     (path (abbreviate-file-name
            (directory-file-name (file-name-directory path))))
     (url (or (org-upwell--url-host url) url))
     (t ""))))

(defun org-upwell--fit-where (item width)
  "Where ITEM is, cut to WIDTH from whichever end can be spared.

A folder is cut from the left: two copies of one file sit in the same tree
down to the last name or two, so the end is the whole of the answer.  A host
is cut from the right, because it says the same thing the other way round --
what tells two of them apart is the first label, and every one of them ends
in the same handful of characters."
  (let ((where (org-upwell--item-where item)))
    (if (plist-get item :path)
        (org-upwell--tail where width)
      (truncate-string-to-width where width nil nil t))))

(defun org-upwell--ago (item)
  "How long since ITEM was last opened, or caught.  Empty when neither is known."
  (let* ((stamp (or (plist-get item :opened) (plist-get item :captured)))
         (time (and stamp (ignore-errors (date-to-time stamp))))
         (secs (and time (max 0 (floor (float-time (time-subtract nil time)))))))
    (cond
     ((null secs) "")
     ((< secs 3600) (format "%dm" (max 1 (/ secs 60))))
     ((< secs 86400) (format "%dh" (/ secs 3600)))
     ((< secs (* 30 86400)) (format "%dd" (/ secs 86400)))
     ((< secs (* 365 86400)) (format "%dmo" (/ secs (* 30 86400))))
     (t (format "%dy" (/ secs (* 365 86400)))))))

(defun org-upwell--bench-widths (items buf)
  "Return (NAME-WIDTH . WHERE-WIDTH) for ITEMS listed in BUF.

The name column is as wide as the longest name, within reason.  The folder
column is as wide as the longest folder, up to what the window has left
after the columns that follow it -- so a wide frame does not put a hand's
width of blank between two short columns."
  (let* ((win (get-buffer-window buf nil))
         (total (if (window-live-p win) (window-body-width win) (frame-width)))
         (name (min 40 (max 20 (apply #'max 0
                                      (mapcar (lambda (m)
                                                (string-width
                                                 (or (plist-get m :name) "?")))
                                              items)))))
         (asked (apply #'max 0
                       (mapcar (lambda (m)
                                 (string-width (org-upwell--item-where m)))
                               items))))
    (cons name (max 12 (min asked (- total name 37))))))

(defun org-upwell--bench-insert-item (m domain widths)
  "Insert one item line for M under DOMAIN, in the columns WIDTHS."
  (let* ((name (or (plist-get m :name) "?"))
         (id (plist-get m :id))
         (st (let ((hid (plist-get domain :id)))
               (and hid (org-upwell-claim-status
                         (plist-get m :claims) hid))))
         (marked (and id (member id org-upwell-bench-marked)))
         (name-w (car widths))
         (where-w (cdr widths))
         ;; The name keeps its beginning and the folder keeps its end, which
         ;; are opposite on purpose: a name says what the thing is in its
         ;; first word, and a folder says which of two it is in its last.
         ;; Cutting a name from the same end as a folder left every row that
         ;; came from one browser reading the same three words.
         (shown (truncate-string-to-width name name-w nil nil t))
         (start (point)))
    (insert (if marked "* " "  "))
    (insert-text-button
     shown
     'follow-link t
     'org-upwell m
     'help-echo (or (plist-get m :path) (plist-get m :url) "")
     'action (lambda (b)
               (org-upwell-open (button-get b 'org-upwell))))
    (insert (make-string (max 0 (- name-w (string-width shown))) ?\s) "  ")
    (insert
     (string-trim-right
      (concat
       (propertize (org-upwell--pad
                    (org-upwell--fit-where m where-w) where-w)
                   'face 'shadow)
       "  "
       (propertize (org-upwell--pad (or (plist-get m :provenance) "") 6)
                   'face 'shadow
                   'help-echo "how this file came to be here")
       "  "
       (propertize (org-upwell--pad (org-upwell--ago m) 5) 'face 'shadow)
       (if (eq st 'provisional)
           (propertize "  provisional"
                       'face 'shadow
                       'help-echo "clock attributed this; not yet kept")
         "")
       (if (plist-get m :stale)
           (propertize "  stale" 'face 'warning)
         ""))))
    (insert "\n")
    (put-text-property start (1- (point)) 'org-upwell m)))

(defun org-upwell-visit-store (&optional item)
  "Visit `upwell.org', at ITEM's heading when given.

This is the file the claims are written in -- the same shape in demo
and in real use.  Point's item button, if any, selects the entry."
  (interactive)
  (let* ((m (or item (get-text-property (point) 'org-upwell)))
         (file (org-upwell-file)))
    (unless (file-exists-p file)
      (user-error "No store yet at %s" file))
    (org-upwell--show-away-from-bench (find-file-noselect file))
    (when-let ((id (and m (plist-get m :id))))
      (goto-char (point-min))
      (when (re-search-forward (concat "^[ \t]*:ID:[ \t]+"
                                       (regexp-quote id)
                                       "[ \t]*$")
                               nil t)
        (org-back-to-heading t)
        (org-fold-show-entry)))))

(defun org-upwell--bench-item-at-point ()
  "Return the item plist on this line, or nil."
  (or (get-text-property (point) 'org-upwell)
      (get-text-property (line-beginning-position) 'org-upwell)))

(defun org-upwell-bench-open-at-point ()
  "Open the item on this line with the OS default app.
The bench window is left as the bench."
  (interactive)
  (let ((m (org-upwell--bench-item-at-point)))
    (if m (org-upwell-open m 'external)
      (push-button))))

(defun org-upwell-bench-open-in-emacs ()
  "Visit the item on this line inside Emacs, not in the bench window."
  (interactive)
  (let ((m (org-upwell--bench-item-at-point)))
    (unless m (user-error "No item on this line"))
    (org-upwell-open m 'emacs)))

(defun org-upwell-bench-open-all ()
  "Open every item on the current domain."
  (interactive)
  (let* ((domain org-upwell-bench-domain)
         (items (and domain (plist-get domain :items)))
         (n 0))
    (unless items
      (user-error "No files on this heading"))
    (dolist (m items)
      (condition-case err
          (progn (org-upwell-open m) (setq n (1+ n)))
        (error (message "org-upwell: %s" (error-message-string err)))))
    (message "org-upwell: opened %d file%s" n (if (= n 1) "" "s"))))

(defun org-upwell-bench-open-marked ()
  "Open marked items, or the item at point if none are marked."
  (interactive)
  (if (null org-upwell-bench-marked)
      (org-upwell-bench-open-at-point)
    (let ((n 0)
          (wanted org-upwell-bench-marked)
          (items (plist-get org-upwell-bench-domain :items)))
      (dolist (m items)
        (when (member (plist-get m :id) wanted)
          (condition-case err
              (progn (org-upwell-open m) (setq n (1+ n)))
            (error (message "org-upwell: %s" (error-message-string err))))))
      (message "org-upwell: opened %d marked file%s" n (if (= n 1) "" "s")))))

(defun org-upwell-bench-toggle-mark ()
  "Mark or unmark the item on this line, then move to the next."
  (interactive)
  (let* ((m (org-upwell--bench-item-at-point))
         (id (and m (plist-get m :id)))
         (line (line-number-at-pos)))
    (unless id
      (user-error "No item on this line"))
    (setq org-upwell-bench-marked
          (if (member id org-upwell-bench-marked)
              (delete id org-upwell-bench-marked)
            (cons id org-upwell-bench-marked)))
    (org-upwell-bench org-upwell-bench-domain)
    (goto-char (point-min))
    (forward-line line)))

(defun org-upwell-bench-unmark ()
  "Unmark the item on this line, then move to the next."
  (interactive)
  (let* ((m (org-upwell--bench-item-at-point))
         (id (and m (plist-get m :id)))
         (line (line-number-at-pos)))
    (when id
      (setq org-upwell-bench-marked
            (delete id org-upwell-bench-marked))
      (org-upwell-bench org-upwell-bench-domain)
      (goto-char (point-min))
      (forward-line line))))

(defun org-upwell-bench-mark-toggle-all ()
  "Mark all items, or unmark all if every item is already marked."
  (interactive)
  (let ((ids (delq nil
                   (mapcar (lambda (m) (plist-get m :id))
                           (plist-get org-upwell-bench-domain :items)))))
    (setq org-upwell-bench-marked
          (if (and org-upwell-bench-marked
                   (null (seq-difference ids org-upwell-bench-marked)))
              nil
            ids))
    (org-upwell-bench org-upwell-bench-domain)))

(defun org-upwell-bench-unmark-all ()
  "Unmark every item on the bench.

No prompt: the bench is a handful of files, not a dired of thousands."
  (interactive)
  (setq org-upwell-bench-marked nil)
  (org-upwell-bench org-upwell-bench-domain))

(defun org-upwell--bench-target-items ()
  "Return the marked items, or the item on this line.

The rule `x' already uses: marks when there are any, this line when there
are none.  A command that writes to the store is not the place to guess
more widely than that."
  (let* ((items (plist-get org-upwell-bench-domain :items))
         (marked (and org-upwell-bench-marked
                      (seq-filter (lambda (m)
                                    (member (plist-get m :id)
                                            org-upwell-bench-marked))
                                  items))))
    (or marked
        (when-let ((m (org-upwell--bench-item-at-point)))
          (list m))
        (user-error "No item on this line"))))

(defun org-upwell--bench-heading-id ()
  "Return the org-id of the heading this bench is showing."
  (or (plist-get org-upwell-bench-domain :id)
      (user-error "This bench is not showing a heading with an id")))

(defun org-upwell--bench-redraw ()
  "Read the store again and draw this bench, leaving point on its line.

Marks go: what they pointed at may not be there any more."
  (let ((line (line-number-at-pos))
        (marker (plist-get org-upwell-bench-domain :marker)))
    (setq org-upwell-bench-marked nil)
    (org-upwell-bench (org-upwell-domain marker))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun org-upwell-bench-keep ()
  "Keep the marked items, or this one, on this heading.

The clock proposes; this is where a person agrees.  A kept claim is not
downgraded by a later pass."
  (interactive)
  (let ((id (org-upwell--bench-heading-id))
        (items (org-upwell--bench-target-items)))
    (org-upwell-with-store
      (dolist (m items)
        (org-upwell-claim m id 'confirmed)))
    (org-upwell--bench-redraw)
    (message "org-upwell: kept %d" (length items))))

(defun org-upwell-bench-drop ()
  "Take the marked items, or this one, off this heading.

Written down as a rejection rather than forgotten: the intersection runs
again on a timer and would otherwise put the same file back on the same
heading.  The item itself stays in the store, and so does the file."
  (interactive)
  (let ((id (org-upwell--bench-heading-id))
        (items (org-upwell--bench-target-items)))
    (org-upwell-with-store
      (dolist (m items)
        (org-upwell-reject m id)))
    (org-upwell--bench-redraw)
    (message "org-upwell: dropped %d from this heading" (length items))))

(defun org-upwell-bench-forget ()
  "Delete the marked items, or this one, from the store.

`d' says the file does not belong here.  This says the item was not worth
recording at all.  The file on disk is not touched."
  (interactive)
  (let* ((items (org-upwell--bench-target-items))
         (n (length items)))
    (when (yes-or-no-p
           (format "Delete %d item%s from %s?  The file%s stay%s. "
                   n (if (= n 1) "" "s")
                   (file-name-nondirectory (org-upwell-file))
                   (if (= n 1) "" "s") (if (= n 1) "s" "")))
      (org-upwell-with-store
        (dolist (m items)
          (org-upwell-forget m)))
      (org-upwell--bench-redraw)
      (message "org-upwell: forgot %d" n))))

(defun org-upwell-bench-reassign ()
  "Move the marked items, or this one, to another heading."
  (interactive)
  (let ((id (org-upwell--bench-heading-id))
        (items (org-upwell--bench-target-items)))
    (org-upwell--reassign-loop items id)
    (org-upwell--bench-redraw)))

(defun org-upwell-bench-add-file (path)
  "Claim PATH to the heading this bench is showing."
  (interactive (list (read-file-name "Add to this heading: " nil nil t)))
  (org-upwell-pin path (plist-get org-upwell-bench-domain :marker) "pin"))

(defun org-upwell-bench-add-url (url)
  "Claim URL to the heading this bench is showing."
  (interactive "sAdd URL to this heading: ")
  (org-upwell-pin url (plist-get org-upwell-bench-domain :marker) "pin"))

(defcustom org-upwell-bench-eldoc t
  "Whether the bench says in the echo area what the line at point is.

A row is a set of columns, and a column is a width: a name longer than its
own is cut, and two files kept in the same tree are told apart by a folder
that has been cut as well.  The row is what a page of them is *for* -- it is
read down, at a glance -- so the answer to \"which one is this, exactly\" goes
where a glance already goes when it wants more, rather than into a wider
column that would cost every other row."
  :type 'boolean
  :group 'org-upwell)

(defun org-upwell-bench-eldoc-function (&rest _)
  "Say what the item on this line is, whole.

For `eldoc-documentation-functions\='.  The name in full and where it actually
is: the two things the row had to shorten."
  (when (and org-upwell-bench-eldoc
             (derived-mode-p 'org-upwell-bench-mode))
    (when-let ((m (org-upwell--bench-item-at-point)))
      (let ((where (or (and (plist-get m :path)
                            (abbreviate-file-name (plist-get m :path)))
                       (plist-get m :url)
                       (plist-get m :office))))
        (concat (or (plist-get m :name) "?")
                (if where (concat "  " where) ""))))))

(defvar org-upwell-bench-mode-map (make-sparse-keymap))
(let ((map org-upwell-bench-mode-map))
  (set-keymap-parent map special-mode-map)
  (define-key map (kbd "q") #'org-upwell-bench-quit)
  (define-key map (kbd "g") #'org-upwell-bench)
  (define-key map (kbd "n") #'next-line)
  (define-key map (kbd "p") #'previous-line)
  (define-key map (kbd "j") #'next-line)
  (define-key map (kbd "k") #'previous-line)
  (define-key map (kbd "o") #'org-upwell-visit-store)
  (define-key map (kbd "e") #'org-upwell-bench-open-in-emacs)
  (define-key map (kbd "f") #'org-upwell-open-folder)
  (define-key map (kbd "RET") #'org-upwell-bench-open-at-point)
  (define-key map (kbd "a") #'org-upwell-bench-open-all)
  (define-key map (kbd "x") #'org-upwell-bench-open-marked)
  (define-key map (kbd "m") #'org-upwell-bench-toggle-mark)
  (define-key map (kbd "u") #'org-upwell-bench-unmark)
  (define-key map (kbd "t") #'org-upwell-bench-mark-toggle-all)
  (define-key map (kbd "U") #'org-upwell-bench-unmark-all)
  (define-key map (kbd "c") #'org-upwell-bench-keep)
  (define-key map (kbd "d") #'org-upwell-bench-drop)
  (define-key map (kbd "D") #'org-upwell-bench-forget)
  (define-key map (kbd "r") #'org-upwell-bench-reassign)
  (define-key map (kbd "+") #'org-upwell-bench-add-file)
  (define-key map (kbd "L") #'org-upwell-bench-add-url))

(define-derived-mode org-upwell-bench-mode special-mode "Upwell"
  "Thin listing of a heading's materials.

Not an editing buffer.  Emacs state (same as org-dayflow): `o' must
open the store in another window, not `evil-open-below', and not by
replacing the bench.  RET / a / x open with the OS.  `e' visits inside
Emacs, still not in the bench.  `j' / `k' still move.  `U' unmarks
all, like dired.  `g' redraws.  `q' deletes the strip; follow will
not cut another one until the heading changes.

`f' opens the folder the file is in, with the file selected: the folder
is what tells two files of the same name apart, so it is also where to
go and look.

The echo area says what the line at point is, in full -- the name and
where it actually is, which are the two things a column had to shorten.

`c' and `d' are the two answers to a claim the clock proposed: keep it
on this heading, or take it off and have that stay said.  `D' deletes
the item from the store.  `r' moves it to another heading.  `+' and
`L' add a file and a URL.  All of them act on the marks when there are
marks, and on this line when there are none.

\\{org-upwell-bench-mode-map}"
  (setq truncate-lines t)
  (add-hook 'eldoc-documentation-functions
            #'org-upwell-bench-eldoc-function nil t)
  (eldoc-mode 1)
  (when (and (boundp 'evil-state) (fboundp 'evil-emacs-state))
    (evil-emacs-state)))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'org-upwell-bench-mode 'emacs))

;;;; Follow

(defun org-upwell--heading-identity (&optional marker)
  "Stable identity of the heading at MARKER (or point).

An org-id when the heading has one; otherwise the file and the
heading's buffer position, so `j'/`k' inside the heading is not a
change."
  (org-with-point-at (or marker (point))
    (ignore-errors
      (org-back-to-heading t)
      (or (org-id-get)
          (cons (or (buffer-file-name) (buffer-name)) (point))))))

(defun org-upwell--follow-draw (marker)
  "Draw the bench for MARKER when the heading changed, or the window was stolen.

`q' dismisses until the heading changes.  Agenda deleting the
window is not `q': the bench is still wanted, so the same heading
is drawn again."
  (when (and (markerp marker) (marker-buffer marker))
    (let* ((id (org-upwell--heading-identity marker))
           (showing (get-buffer-window "*org-upwell*" nil))
           (stolen (and (eq org-upwell--bench-intent 'wanted) (not showing))))
      (when (and id (or stolen (not (equal id org-upwell--follow-seen))))
        (setq org-upwell--follow-seen id)
        (ignore-errors (org-upwell-bench (org-upwell-domain marker)))))))

(defun org-upwell--quit-bench-window ()
  "Delete the bench window on this frame, if it is not the only window."
  (when-let ((buf (get-buffer "*org-upwell*"))
             (win (get-buffer-window buf nil)))
    (if (one-window-p)
        (quit-window nil win)
      (delete-window win))))

(defun org-upwell-bench-quit ()
  "Dismiss the bench.  Follow and expand do not put it back until
the heading changes, or the bench is asked for again."
  (interactive)
  (setq org-upwell--bench-intent 'dismissed)
  (org-upwell--quit-bench-window))

(defun org-upwell--follow-update ()
  "Redraw the bench if point moved to a different Org heading."
  (unless (eq this-command 'org-upwell-bench-quit)
    (when (and org-upwell-follow-mode
               (derived-mode-p 'org-mode)
               (not (org-before-first-heading-p)))
      (org-upwell--follow-draw (point-marker)))))

(defcustom org-upwell-agenda-follow nil
  "When non-nil, moving in the agenda redraws the bench for the row.

Org's own follow (`F') is a separate thing and opens the entry's file in
another window.  This one draws the bench and nothing else, so the frame
stays as it was: the agenda where it is, the listing where it is.  A day
designed in the agenda and worked from the bench is what it is for."
  :type 'boolean
  :group 'org-upwell)

(defun org-upwell--agenda-follow (&rest _)
  "After agenda context action, show the bench for that heading.

Runs when `org-upwell-agenda-follow' or Org's own follow mode is on.
Does not open files; that is expand."
  (when (and (or org-upwell-agenda-follow
                 (bound-and-true-p org-agenda-follow-mode))
             (derived-mode-p 'org-agenda-mode))
    (when-let ((m (or (org-get-at-bol 'org-hd-marker)
                      (org-get-at-bol 'org-marker))))
      (org-upwell--follow-draw m))))

;;;###autoload
(define-minor-mode org-upwell-follow-mode
  "Show the bench for the Org heading at point, and keep it in step.

Turning the mode on draws the bench.  Turning it off deletes the
bench window.  Motion inside a heading does not redraw.  Does not
open files; that is expand.

Agenda follow (`F') is separate: with `org-upwell-mode' on, `F'
already updates the bench from the agenda row."
  :global t
  :group 'org-upwell
  :lighter " Upwell-F"
  (setq org-upwell--follow-seen nil)
  (if org-upwell-follow-mode
      (progn
        (add-hook 'post-command-hook #'org-upwell--follow-update)
        (org-upwell--follow-update))
    (remove-hook 'post-command-hook #'org-upwell--follow-update)
    (org-upwell--quit-bench-window)))

(defun org-upwell--window-above-bench ()
  "Return (WINDOW . TYPE) for the window directly above the bench, or nil.

TYPE is what `display-buffer' calls the window: `reuse' for one that
was already there, `window' for one made here.  An agenda is not
somewhere to put a file, so an agenda above the bench is split and the
lower half -- the half touching the bench -- is the answer."
  (when-let ((bench (get-buffer-window "*org-upwell*" nil)))
    (let ((above (window-in-direction 'above bench)))
      (cond
       ((not (window-live-p above)) nil)
       ((not (with-current-buffer (window-buffer above)
               (derived-mode-p 'org-agenda-mode)))
        (cons above 'reuse))
       (t (when-let ((win (ignore-errors (split-window above nil 'below))))
            (cons win 'window)))))))

(defun org-upwell-display-above-bench (buffer alist)
  "Display BUFFER above the bench.  A `display-buffer' action function.

Returns nil when no bench is showing, which leaves the rest of the
action list to decide as it always did."
  (when-let ((found (org-upwell--window-above-bench)))
    (window--display-buffer buffer (car found) (cdr found) alist)))

(defun org-upwell--agenda-goto-above-bench (fn &rest args)
  "Call FN with Org displayed above the bench rather than into it.

`org-agenda-goto' picks a window itself, and a short strip is an
inviting thing to split or replace.  The bench is neither: it is what
the row was being read with."
  (let ((display-buffer-overriding-action
         (if (get-buffer-window "*org-upwell*" nil)
             '((org-upwell-display-above-bench
                display-buffer-reuse-window
                display-buffer-pop-up-window))
           display-buffer-overriding-action)))
    (apply fn args)))

(defun org-upwell-follow-setup ()
  "Hook the bench into agenda motion, and keep the agenda out of it."
  (with-eval-after-load 'org-agenda
    (advice-add 'org-agenda-do-context-action :after
                #'org-upwell--agenda-follow)
    ;; `org-agenda-show' (SPC, and Org's own follow) goes through this one
    ;; too, so one piece of advice covers both ways in.
    (advice-add 'org-agenda-goto :around
                #'org-upwell--agenda-goto-above-bench)))

(defun org-upwell-follow-teardown ()
  "Remove the agenda follow hook and stop Org follow."
  (advice-remove 'org-agenda-do-context-action #'org-upwell--agenda-follow)
  (advice-remove 'org-agenda-goto #'org-upwell--agenda-goto-above-bench)
  (when org-upwell-follow-mode
    (org-upwell-follow-mode -1)))

;; Eldoc speaks only after a command it has been told about: it keeps an
;; obarray of them and says nothing after anything else, which is why the
;; line at point is announced when `n' moves to it and was silent when `m'
;; did.  `m' marks and moves down, and the line it lands on is exactly the
;; one somebody is about to act on.
;;
;; Registered by prefix rather than one at a time, so a command added to the
;; bench later is announced without anybody remembering to come back here.
;; Every one of them either moves point or changes what the line at point
;; says, which is the whole of what eldoc is being asked to report.
(eldoc-add-command-completions "org-upwell-bench")

(provide 'org-upwell-expand)

;;; org-upwell-expand.el ends here
