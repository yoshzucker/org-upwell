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

(defcustom org-upwell-review-on-clock-out t
  "When non-nil, offer to confirm provisional claims at clock-out.

Only asked when this spell actually produced new attributions.  A clock
with no matching traces is silent -- there is nothing to confirm."
  :type 'boolean
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

  (let* ((traces (org-upwell-unique-traces (org-upwell-traces-in from to)))
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
          (traces (org-upwell-unique-traces (org-upwell-read-traces from to)))
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

(defun org-upwell-review-interval (marker from to)
  "Ask about provisional claims in [FROM, TO) on MARKER.

Enter keeps them (and promotes to confirmed).  `n' rejects them, which
is written down: the intersection runs again every minute and would
otherwise put the same files back.  `r' reassigns one.  Called from
clock-out and from after `org-foresight-clock-fill'; silent when there
is nothing new."
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
    (cond
     ((null items) nil)
     ((not org-upwell-review-on-clock-out)
      items)
     (t
      (let ((key (read-char-choice
                  (format "%d file(s) under \"%s\"  [RET keep / n reject / r reassign]: "
                          (length items)
                          (truncate-string-to-width (or title "") 40 nil nil t))
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
           (message "org-upwell: kept %d on %s" (length items) title))))
      items)))))

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
