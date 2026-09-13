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
(defvar org-foresight-signal-commands)
;; The grid is where the unclaimed are settled; this file only names it.
(declare-function org-upwell-matrix "org-upwell-matrix")
(defvar org-foresight-signal-summarised)

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

(defun org-upwell-plan-contribute ()
  "Add this package\'s groups, kinds and answers to the board.

Separate from `org-upwell-plan-setup\=' because the two are different
questions: this is *what* is contributed, and that is *when*.  Wrapped
together there was nowhere to stand to check the what -- the when is an
`with-eval-after-load\=', and a test cannot make a feature present by
saying so."
  (add-to-list 'org-foresight-signal-functions
               #'org-upwell-foresight-signals)
  (dolist (pair `((,org-upwell-signal-unclaimed . owed)
                  (,org-upwell-signal-stale . fact)
                  (,org-upwell-signal-empty . fact)))
    (unless (assoc (car pair) org-foresight-signal-kinds)
      (setq org-foresight-signal-kinds
            (append org-foresight-signal-kinds (list pair)))))
  ;; The count, and the way in.  A store is what was seen, so most of what
  ;; is in it was never work and the unclaimed run to hundreds: drawn row by
  ;; row they would push the rest of the board off the screen, and nobody
  ;; reads three hundred names on a page they came to for a verdict.  The
  ;; number is the news.  Where it is settled is the grid, where those rows
  ;; are already drawn with every cell empty and the key that answers a
  ;; proposal attaches them.
  (add-to-list 'org-foresight-signal-summarised org-upwell-signal-unclaimed)
  (unless (assoc org-upwell-signal-unclaimed org-foresight-signal-commands)
    (setq org-foresight-signal-commands
          (append org-foresight-signal-commands
                  (list (cons org-upwell-signal-unclaimed
                              #'org-upwell-matrix))))))

(defun org-upwell-plan-setup ()
  "Contribute signals when org-foresight is present."
  (with-eval-after-load 'org-foresight-plan
    (org-upwell-plan-contribute)))

(defun org-upwell-plan-teardown ()
  "Stop contributing signals."
  (when (boundp 'org-foresight-signal-functions)
    (setq org-foresight-signal-functions
          (delq #'org-upwell-foresight-signals
                org-foresight-signal-functions)))
  (when (boundp 'org-foresight-signal-summarised)
    (setq org-foresight-signal-summarised
          (delete org-upwell-signal-unclaimed
                  org-foresight-signal-summarised)))
  (when (boundp 'org-foresight-signal-commands)
    (setq org-foresight-signal-commands
          (assoc-delete-all org-upwell-signal-unclaimed
                            org-foresight-signal-commands))))

(provide 'org-upwell-plan)

;;; org-upwell-plan.el ends here
