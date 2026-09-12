;;; org-upwell.el --- Files rising to the heading that is being lived  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, files, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; org-foresight answers when a thing fits on the day.  org-convect takes
;; purpose at the top of the ladder and carries it down as far as areas
;; (H2); the climb back is a reduction -- purpose written again from what
;; the lower rungs did.  org-upwell is the other vertical.  It happens at
;; the day's feet (H1): the files and URLs work is done with rise to the
;; heading that is being lived, the way deep water rises to the light.
;; It does not climb the ladder.  It supplies the surface convect and
;; foresight already stand on.
;;
;; A user heading -- a project, a task, a meeting -- has a domain.
;; Expanding the domain brings back the Org entry and the files and URLs
;; that claim it.  Those files are not listed under the heading; they
;; point up at its id.  upwell.org is storage for those identities, not
;; the work.
;;
;; Capture happens outside Emacs.  A resident watcher (AutoHotkey on
;; Windows, osascript on macOS) records the frontmost Office document and
;; browser URL without being asked.  Attribution is the intersection of
;; those traces with CLOCK intervals, including clocks filled in after the
;; fact from the agenda (org-foresight's C).  The watcher does not know Org;
;; Emacs does not have to be focused for a file to be remembered.
;;
;; Layout:
;;
;;   org-upwell-core.el     identity, appearances, claims
;;   org-upwell-trace.el    the watcher's JSONL
;;   org-upwell-claim.el    clock intersection, live and retroactive
;;   org-upwell-pin.el      explicit catch (pin, drop, org-protocol)
;;   org-upwell-bench.el    the listing, opening, resolve, follow
;;   org-upwell-sight.el    what Emacs itself sees, written as traces
;;   org-upwell-matrix.el   one family of headings, and what each holds
;;   org-upwell-watch.el    convenience starter for the watcher
;;   org-upwell-plan.el     foresight board signals
;;   org-upwell-demo.el     generated data, behind a toggle
;;
;; Requiring `org-upwell' and turning on `org-upwell-mode' is the whole of
;; the Emacs side.  The watcher is installed separately; see README.org.

;;; Code:

(require 'org-upwell-core)
(require 'org-upwell-trace)
(require 'org-upwell-claim)
(require 'org-upwell-pin)
(require 'org-upwell-bench)
(require 'org-upwell-sight)
(require 'org-upwell-matrix)
(require 'org-upwell-watch)
(require 'org-upwell-plan)
(require 'org-upwell-demo)

(defcustom org-upwell-sync-interval 60
  "Seconds of idleness before traces and clocks are intersected.

Idleness, not an interval.  A repeating timer fires while you are typing, and
this pass reads the whole store; landing in the middle of a keystroke, it is
felt.  An idle timer runs once each time Emacs goes quiet, which is when a
background pass belongs.

The watcher samples far more often than this, and its log waits on disk until
Emacs next has a moment.  Nothing is lost by going a long stretch without a
pass: a clock-out and a foresight fill both intersect on the spot and do not
wait for the timer."
  :type 'number
  :group 'org-upwell)

(defvar org-upwell--sync-timer nil)

(defun org-upwell--sync-quietly ()
  "Timer function: ingest traces, do not prompt."
  (let ((org-upwell-review-on-clock-out nil))
    (ignore-errors (org-upwell-sync 1))))

;;;###autoload
(define-minor-mode org-upwell-mode
  "Catch files and URLs against Org headings, and expand a heading's domain."
  :global t
  :group 'org-upwell
  (if org-upwell-mode
      (progn
        (org-upwell-protocol-setup)
        (org-upwell-claim-setup)
        (org-upwell-plan-setup)
        (org-upwell-follow-setup)
        (org-upwell-sight-mode 1)
        (add-hook 'org-mode-hook #'org-upwell-enable-dnd)
        (add-hook 'org-agenda-mode-hook #'org-upwell-enable-dnd)
        (when org-upwell-sync-interval
          (setq org-upwell--sync-timer
                (run-with-idle-timer org-upwell-sync-interval t
                                     #'org-upwell--sync-quietly))))
    (org-upwell-claim-teardown)
    (org-upwell-plan-teardown)
    (org-upwell-follow-teardown)
    (org-upwell-sight-mode -1)
    (remove-hook 'org-mode-hook #'org-upwell-enable-dnd)
    (remove-hook 'org-agenda-mode-hook #'org-upwell-enable-dnd)
    (when org-upwell--sync-timer
      (cancel-timer org-upwell--sync-timer)
      (setq org-upwell--sync-timer nil))))

(provide 'org-upwell)

;;; org-upwell.el ends here
