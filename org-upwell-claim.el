;;; org-upwell-claim.el --- Clock intervals, live or filled in later  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Claims are not written at capture time from "whatever is clocked now".
;; That would miss the common case here: a stretch of work whose clock is
;; filled in afterwards, from the agenda, with org-foresight's C.
;;
;; The watcher records traces with unix timestamps and no Org in them.
;; This file intersects those traces with CLOCK intervals -- a running
;; clock, a clock just closed, or a clock written onto a heading an hour
;; later.  The same function covers all three.
;;
;; What it writes is provisional.  A person confirms it (clock-out review,
;; an explicit pin, opening the file from an expanded domain) or rejects
;; it.  A confirmed claim is never overwritten by a later intersection.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-upwell-core)
(require 'org-upwell-trace)
(require 'seq)
(require 'cl-lib)

;; The bench lives in org-upwell-expand, which requires this file's store
;; through the core.  Clock-out shows the listing when it is loaded.
(declare-function org-upwell-bench "org-upwell-expand" (&optional domain))
(declare-function org-upwell-domain "org-upwell-expand" (marker))

(defcustom org-upwell-review-on-clock-out 'bench
  "What clock-out does with the claims the spell just produced.

`bench' shows the listing for the heading and asks nothing.  The claims
stay provisional, which is what the listing marks them as, and the keys
that keep or drop them are on the bench.  One spell often touches several
files, and a question that names none of them can only be answered by
saying yes.

`ask' is the older single-line question at the minibuffer.

Nil records and says nothing.  Nothing is lost either way: the claims are
already written by then.  A spell with no matching traces is silent under
all three."
  :type '(choice (const :tag "Show the bench" bench)
                 (const :tag "Ask in the minibuffer" ask)
                 (const :tag "Say nothing" nil))
  :group 'org-upwell)

(defcustom org-upwell-inherit-on-clock-in t
  "When non-nil, clocking in copies items from the last expanded heading.

The copy is provisional.  This is the case where a DONE task's files are
reopened (expand) and a new NEXT is clocked: the same files attach to
the new heading without being dropped on it again.  Same heading, or
nothing last expanded, is a no-op.  A confirmed claim is not downgraded."
  :type 'boolean
  :group 'org-upwell)

(defvar org-upwell--clock-in-time nil
  "Start of the running clock, captured at `org-clock-in-hook'.")
(defvar org-upwell--clock-in-marker nil
  "Heading of the running clock, captured at `org-clock-in-hook'.")

;;;; Clock segments

(defun org-upwell--running-clock-p (start)
  "Return non-nil when the CLOCK line starting at START is the running one.
Point must be on the heading that carries it."
  (and (org-clocking-p)
       (markerp org-clock-hd-marker)
       (eq (marker-buffer org-clock-hd-marker) (current-buffer))
       (= (point) (org-with-point-at org-clock-hd-marker
                    (org-back-to-heading t)
                    (point)))
       (or (null org-clock-start-time)
           (< (abs (float-time (time-subtract start org-clock-start-time)))
              60))))

(defun org-upwell-clock-segments (&optional days now)
  "Return CLOCK segments in the last DAYS days (default 1, today).

Each segment is a plist `:from :to :marker :title', clamped to the window
asked for, so attribution never reaches outside the days requested.

Only the running clock is closed at NOW.  A CLOCK line with no end that
is *not* running is a clock somebody forgot to close, and reading it as
work still going on hands every file of today to a heading last touched
in June.  Filling such a line in is org-foresight's C; guessing at it
here is not.

Independent of org-foresight, so a filled-in clock is visible here the
moment Org has written the line, whether or not the agenda has been
redrawn."
  (let* ((days (or days 1))
         (now (or now (current-time)))
         (from (org-upwell--day-start (1- days)))
         (today1 (time-add (org-upwell--day-start 0) (days-to-time 1)))
         (re (concat "^[ \t]*" org-clock-string
                     "[ \t]*\\(\\[[^]\n]+\\]\\)\\(?:--\\(\\[[^]\n]+\\]\\)\\)?"))
         segments
         (files (seq-filter #'file-exists-p
                            (or (org-agenda-files) nil))))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward re nil t)
           (let* ((s-str (match-string-no-properties 1))
                  (e-str (match-string-no-properties 2))
                  (s (org-time-string-to-time s-str)))
             (save-excursion
               (org-back-to-heading t)
               (let* ((e (cond (e-str (org-time-string-to-time e-str))
                               ((org-upwell--running-clock-p s) now)))
                      (s* (and e (if (time-less-p s from) from s)))
                      (e* (and e (if (time-less-p today1 e) today1 e))))
                 (when (and e (time-less-p s* e*))
                   (push (list :from s* :to e*
                               :marker (point-marker)
                               :title (org-get-heading t t t t))
                         segments)))))))))
    (nreverse segments)))

(defun org-upwell--segments-overlapping (from to &optional segments)
  "Return SEGMENTS that overlap [FROM, TO)."
  (seq-filter
   (lambda (seg)
     (and (time-less-p (plist-get seg :from) to)
          (time-less-p from (plist-get seg :to))))
   (or segments (org-upwell-clock-segments 2))))

;;;; Intersection

(defun org-upwell-claim-interval (marker from to &optional provenance)
  "Provisionally claim traces in [FROM, TO) to the heading at MARKER.

Return the list of item plists newly or still provisionally attributed.
Already-confirmed claims for this heading are left alone, and items a
person rejected for it are not proposed again.  PROVENANCE is recorded
on a newly created item (default `trace')."

  (let* ((traces (seq-remove #'org-upwell-trace-directory-p
                             (org-upwell-unique-traces
                              (org-upwell-traces-in from to))))
         ;; Filtered before the id: `org-upwell-heading-id' writes an ID
         ;; into the user's file, and a spell that only walked through
         ;; directories has nothing to claim and no reason to leave a mark.
         (heading-id (and traces (org-upwell-heading-id marker)))
         (prov (or provenance "trace"))
         claimed)
    (dolist (tr traces)
      (let* ((spec (plist-put (org-upwell-trace-to-spec tr) :provenance prov))
             (saved (org-upwell-save spec))
             (status (org-upwell-claim-status (plist-get saved :claims)
                                              heading-id)))
        (pcase status
          ('rejected nil)
          ('confirmed (push saved claimed))
          (_ (push (org-upwell-claim saved heading-id 'provisional) claimed)))))
    (nreverse claimed)))

(defun org-upwell-sync (&optional days)
  "Intersect today's (or DAYS days of) traces with CLOCK intervals.

This is the whole of attribution.  Called from a timer, from clock-out,
and from after a clock is filled in after the fact.  Traces that sit in
no interval become unclaimed items, so a file opened off the clock is
still findable and still a signal."
  (interactive)
  (org-upwell-with-store
   (let* ((days (or days 1))
          (segments (org-upwell-clock-segments days))
          (from (org-upwell--day-start (1- days)))
          (to (current-time))
          (traces (seq-remove #'org-upwell-trace-directory-p
                              (org-upwell-unique-traces
                               (org-upwell-read-traces from to))))
          (n 0))
     (dolist (seg segments)
       (when (org-upwell-claim-interval
              (plist-get seg :marker)
              (plist-get seg :from)
              (plist-get seg :to)
              "trace")
         (setq n (1+ n))))
     (dolist (tr traces)
       (let* ((ts-from (seconds-to-time (plist-get tr :ts)))
              (ts-to (time-add ts-from 1)))
         (unless (org-upwell--segments-overlapping ts-from ts-to segments)
           (org-upwell-save (org-upwell-trace-to-spec tr)))))
     (when (called-interactively-p 'interactive)
       (message "org-upwell: synced %d interval(s)" n))
     n)))

;;;; Review

(defun org-upwell--review-line (item heading-id)
  "One completing-read candidate for ITEM attributed to HEADING-ID."
  (format "%s  [%s]  %s"
          (or (plist-get item :name) "?")
          (or (org-upwell-claim-status (plist-get item :claims) heading-id)
              "unclaimed")
          (or (plist-get item :path)
              (plist-get item :url)
              "")))

(defun org-upwell--review-names (items width)
  "Names of ITEMS, joined, cut to WIDTH columns."
  (truncate-string-to-width
   (mapconcat (lambda (m) (or (plist-get m :name) "?")) items ", ")
   width nil nil t))

(defun org-upwell--review-ask (items heading-id title)
  "Ask in the minibuffer what to do with ITEMS on HEADING-ID, called TITLE."
  (let ((key (read-char-choice
              (format "%s → %s  [RET keep / n reject / r reassign]: "
                      (org-upwell--review-names items 50)
                      (truncate-string-to-width (or title "") 30 nil nil t))
              '(?\r ?n ?r ?q))))
    (pcase key
      (?n
       (dolist (m items)
         (org-upwell-reject m heading-id))
       (message "org-upwell: dropped %d" (length items)))
      (?r
       (org-upwell--reassign-loop items heading-id))
      (_
       (dolist (m items)
         (org-upwell-claim m heading-id 'confirmed))
       (message "org-upwell: kept %d on %s" (length items) title)))))

(defun org-upwell-review-interval (marker from to)
  "Answer for the provisional claims in [FROM, TO) on MARKER.

The claims are already written by the time this runs; what it decides is
whether to show them, ask about them, or say nothing.  See
`org-upwell-review-on-clock-out'.  Called from clock-out and from after
`org-foresight--file-clocked'; silent when there is nothing new.  Returns
the items either way."
  (org-upwell-with-store
   (let* ((heading-id (org-upwell-heading-id marker))
          (items (seq-filter
                 (lambda (m)
                   (eq 'provisional
                       (org-upwell-claim-status (plist-get m :claims)
                                                heading-id)))
                 (org-upwell-claim-interval marker from to)))
         (title (org-with-point-at marker
                  (org-get-heading t t t t))))
    (when items
      (pcase org-upwell-review-on-clock-out
        ('ask (org-upwell--review-ask items heading-id title))
        ('bench (org-upwell--review-bench marker items))
        (_ nil)))
    items)))

(defun org-upwell--review-bench (marker items)
  "Show the bench for MARKER, and say how many claims ITEMS brought.

The window is not selected: clock-out is often followed by typing that
was already begun, and a listing that takes the keyboard eats it."
  (when (fboundp 'org-upwell-bench)
    (save-selected-window
      (org-upwell-bench (org-upwell-domain marker)))
    (message "org-upwell: %d file(s) attributed -- c keeps, d drops"
             (length items))))

(defun org-upwell--reassign-loop (items from-id)
  "Interactively reassign ITEMS away from FROM-ID."
  (let* ((candidates (org-upwell--reassign-candidates))
         (names (mapcar #'car candidates)))
    (dolist (m items)
      (let ((choice (completing-read
                     (format "Reassign %s to: " (plist-get m :name))
                     (append '("*unclaimed*") names)
                     nil t)))
        (org-upwell-reject m from-id)
        (unless (string= choice "*unclaimed*")
          (let ((dest (cdr (assoc choice candidates))))
            (when dest
              (org-upwell-claim m (org-upwell-heading-id dest) 'confirmed))))))))

(defun org-upwell--reassign-candidates ()
  "Return (TITLE . MARKER) for headings a reassignment may land on.

Today's clocked work, then the running clock, then open NEXT/ONGO in
the agenda files.  Not every project in the file: Org has already
narrowed the plausible set, and a list of a hundred is how this
decision starts costing more than leaving the file unclaimed."
  (let (out seen)
    (when (and (markerp org-clock-hd-marker)
               (marker-buffer org-clock-hd-marker))
      (let ((title (org-with-point-at org-clock-hd-marker
                     (org-get-heading t t t t))))
        (push (cons title org-clock-hd-marker) out)
        (push title seen)))
    (dolist (seg (org-upwell-clock-segments 1))
      (let ((title (plist-get seg :title)))
        (unless (member title seen)
          (push (cons title (plist-get seg :marker)) out)
          (push title seen))))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let ((todo (org-get-todo-state))
                   (title (org-get-heading t t t t)))
               (when (and todo
                          (member todo '("NEXT" "ONGO" "WAIT"))
                          (not (member title seen)))
                 (push (cons title (point-marker)) out)
                 (push title seen))))
           nil 'file))))
    (nreverse out)))

;;;; Hooks -- live clock and after-the-fact fill

(defun org-upwell-inherit-to-heading (from-id to-id)
  "Provisionally claim every item of FROM-ID onto TO-ID.
What a person already settled on TO-ID -- kept or rejected -- is left
alone.  Return how many were copied."
  (let ((n 0))
    (dolist (m (org-upwell-claimed-to from-id) n)
      (let ((status (org-upwell-claim-status (plist-get m :claims) to-id)))
        (unless (memq status '(confirmed rejected))
          (org-upwell-claim m to-id 'provisional)
          (setq n (1+ n)))))))

(defun org-upwell--inherit-from-last-expanded (marker)
  "Copy items from `org-upwell-last-expanded-id' onto MARKER, if different."
  (when (and org-upwell-inherit-on-clock-in
             org-upwell-last-expanded-id
             (markerp marker)
             (marker-buffer marker))
    (let ((to (org-upwell-heading-id marker)))
      (unless (equal to org-upwell-last-expanded-id)
        (org-upwell-inherit-to-heading org-upwell-last-expanded-id to)))))

(defun org-upwell--on-clock-in ()
  "Remember where this spell started, so clock-out can intersect it."
  (setq org-upwell--clock-in-time (or org-clock-start-time (current-time))
        org-upwell--clock-in-marker
        (and (markerp org-clock-hd-marker)
             (copy-marker org-clock-hd-marker)))
  (org-upwell--inherit-from-last-expanded org-upwell--clock-in-marker))

(defun org-upwell--on-clock-out ()
  "Intersect traces with the spell that just ended, then offer review."
  (when (and org-upwell--clock-in-time org-upwell--clock-in-marker
             (marker-buffer org-upwell--clock-in-marker))
    (org-upwell-review-interval
     org-upwell--clock-in-marker
     org-upwell--clock-in-time
     (current-time)))
  (setq org-upwell--clock-in-time nil
        org-upwell--clock-in-marker nil))

(defun org-upwell--after-file-clocked (marker from to)
  "Run after `org-foresight--file-clocked' writes a CLOCK line.

This is how agenda C (fill) and H (split) attribute files: the clock
did not run live, but the traces did, and the interval is now known."
  (when (and (markerp marker) (marker-buffer marker))
    (org-upwell-review-interval marker from to)))

(defun org-upwell--install-foresight-hook ()
  "Advise org-foresight's after-the-fact clock writer, when it exists."
  (when (fboundp 'org-foresight--file-clocked)
    (advice-add 'org-foresight--file-clocked :after
                #'org-upwell--after-file-clocked)))

(defun org-upwell--remove-foresight-hook ()
  "Remove the org-foresight advice if it was added."
  (when (fboundp 'org-foresight--file-clocked)
    (advice-remove 'org-foresight--file-clocked
                   #'org-upwell--after-file-clocked)))

(defun org-upwell-claim-setup ()
  "Install clock hooks.  Called from `org-upwell-mode'."
  (add-hook 'org-clock-in-hook #'org-upwell--on-clock-in)
  (add-hook 'org-clock-out-hook #'org-upwell--on-clock-out)
  (org-upwell--install-foresight-hook)
  (with-eval-after-load 'org-foresight-plan
    (org-upwell--install-foresight-hook))
  (with-eval-after-load 'org-foresight
    (org-upwell--install-foresight-hook)))

(defun org-upwell-claim-teardown ()
  "Remove clock hooks."
  (remove-hook 'org-clock-in-hook #'org-upwell--on-clock-in)
  (remove-hook 'org-clock-out-hook #'org-upwell--on-clock-out)
  (org-upwell--remove-foresight-hook))

(provide 'org-upwell-claim)

;;; org-upwell-claim.el ends here
