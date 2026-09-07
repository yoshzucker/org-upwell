;;; org-upwell-plan.el --- Signals on the foresight board  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contributes three groups to `org-foresight-signal-functions' when
;; org-foresight is loaded.  The board is already the place unsettled
;; work is read; unclaimed and stale items belong there, not on a
;; second visor that would have to be opened.

;;; Code:

(require 'org-upwell-core)
(require 'org-upwell-claim)
(require 'seq)

;; Declared, not required: org-foresight is optional, and this file is
;; loaded whether or not the board exists.
(defvar org-foresight-signal-functions)
(defvar org-foresight-signal-kinds)

(defconst org-upwell-signal-unclaimed "Unclaimed")
(defconst org-upwell-signal-stale "Broken path")
(defconst org-upwell-signal-empty "Clocked with no files")

(defun org-upwell--finding (item note)
  "A foresight-shaped finding for ITEM, described by NOTE."
  (list :file (org-upwell-file)
        :point (and (plist-get item :marker)
                    (marker-position (plist-get item :marker)))
        :marker (plist-get item :marker)
        :title (or (plist-get item :name) "?")
        :note note))

(defun org-upwell-foresight-signals (&optional _scan)
  "Return (LABEL . FINDINGS) groups for the foresight board."
  (org-upwell-with-store
   (let (unclaimed stale empty)
    (dolist (m (org-upwell-items))
      (when (org-upwell-unclaimed-p m)
        (push (org-upwell--finding m "no heading claims this") unclaimed))
      (when (plist-get m :stale)
        (push (org-upwell--finding m "path gone; protocol/url may still work")
              stale)))
    (dolist (seg (org-upwell-clock-segments 1))
      (let ((id (org-with-point-at (plist-get seg :marker)
                  (org-id-get))))
        (when (and id (null (org-upwell-claimed-to id)))
          (push (list :file (buffer-file-name
                             (marker-buffer (plist-get seg :marker)))
                      :point (marker-position (plist-get seg :marker))
                      :marker (plist-get seg :marker)
                      :title (plist-get seg :title)
                      :note "clocked, nothing attributed")
                empty))))
    (delq nil
          (list (and unclaimed (cons org-upwell-signal-unclaimed
                                     (nreverse unclaimed)))
                (and stale (cons org-upwell-signal-stale (nreverse stale)))
                (and empty (cons org-upwell-signal-empty
                                 (delete-dups (nreverse empty)))))))))

(defun org-upwell-plan-setup ()
  "Contribute signals when org-foresight is present."
  (with-eval-after-load 'org-foresight-plan
    (add-to-list 'org-foresight-signal-functions
                 #'org-upwell-foresight-signals)
    (dolist (pair `((,org-upwell-signal-unclaimed . owed)
                    (,org-upwell-signal-stale . fact)
                    (,org-upwell-signal-empty . fact)))
      (unless (assoc (car pair) org-foresight-signal-kinds)
        (setq org-foresight-signal-kinds
              (append org-foresight-signal-kinds (list pair)))))))

(defun org-upwell-plan-teardown ()
  "Stop contributing signals."
  (when (boundp 'org-foresight-signal-functions)
    (setq org-foresight-signal-functions
          (delq #'org-upwell-foresight-signals
                org-foresight-signal-functions))))

(provide 'org-upwell-plan)

;;; org-upwell-plan.el ends here
