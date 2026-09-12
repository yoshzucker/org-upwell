;;; org-upwell-matrix.el --- One family of headings, and everything they hold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-upwell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The bench answers one heading at a time, which is the right unit for
;; working and the wrong one for tidying.  Tasks come in families -- the next
;; one is the last one continued, and it was given the same three files --
;; and after a few rounds of that the same document is on six headings, one
;; of which should never have had it.  Nothing on a single bench can show
;; that, because what is wrong is a relation between two rows of two
;; different benches.
;;
;; So: rows are the things, columns are the headings, and a cell is a claim.
;; Reading a row says where one document is used; reading a column says what
;; one heading holds.  Both readings come out of one picture, which is why
;; this is a grid and not a third listing.
;;
;; The columns are numbered, and the numbers are keyed to a list above the
;; grid.  Heading titles as column headers fit six or eight of them in eighty
;; columns, cut to four characters each, which is not a word; numbers fit
;; twenty and lose nothing, because the list above says the rest.  The echo
;; area says which heading the cursor's column is, which is the only
;; practical way to read a column two characters wide.

;;; Code:

(require 'org-upwell-core)
(require 'org-upwell-bench)

(defconst org-upwell-matrix-buffer "*Org Upwell Matrix*"
  "Name of the grid buffer.")

(defconst org-upwell-matrix-glyphs '((confirmed . "X") (provisional . "?"))
  "What a cell says for each claim status.

Two marks, because the difference is the whole of what tidying is about: a
confirmed claim is somebody\='s answer and a provisional one is a proposal
nobody has looked at -- hence `?\='.  A rejection has no mark and leaves the
cell empty; it is recorded so the intersection stops asking, not so it can
be read off a grid.

ASCII, and not because ASCII is prettier.  `×\=' and `·\=' are East Asian
Ambiguous: in a CJK locale Emacs counts them as two columns and a font may
draw them either way, so a grid built out of them is a grid whose columns
move depending on whose Emacs is drawing it.  Every column here is two
characters wide and the ruler above has to land on them.")

(defface org-upwell-matrix-column
  '((t :inherit bold))
  "The heading the cursor\\='s column is, in the list above the grid.

Bold rather than a band of background, because the row already has one:
`hl-line-mode\\=' says which thing the cursor is on and this says which
heading, and two bands would be two claims of the same kind."
  :group 'org-upwell)

(defvar-local org-upwell-matrix--column-marks nil
  "Where each column\\='s line is in the list above the grid, as (BEG . END).")

(defvar-local org-upwell-matrix--column-overlay nil
  "The overlay marking the cursor\\='s column in that list.")

(defvar-local org-upwell-matrix--columns nil
  "This grid's headings, as (NUMBER ID TITLE LEVEL MARKER).")

(defvar-local org-upwell-matrix--rows nil
  "This grid's items, in the order they are drawn.")

(defvar-local org-upwell-matrix--root nil
  "The marker this grid was built from.")

;;;; What the grid is of

(defun org-upwell-matrix--root (marker)
  "Return the heading MARKER's family root: its parent, or itself.

The parent, because the family is what somebody is tidying.  Standing on a
leaf task and asking about its own subtree gives one column, which is a
list; standing on it and asking about its parent's gives the siblings it
was continued from, which is the question."
  (org-with-point-at marker
    (org-back-to-heading t)
    (if (org-up-heading-safe) (point-marker) (copy-marker marker))))

(defun org-upwell-matrix--headings (root)
  "Return ROOT and its subtree as (NUMBER ID TITLE LEVEL MARKER), in order.

Only headings that already have an org-id are given one to read: this
walks somebody's own file, and minting an id for every heading in a
subtree in order to draw a picture of it would be a write nobody asked
for.  A heading with no id holds nothing, so it has nothing to show."
  (org-with-point-at root
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (n 0)
          out)
      (while (< (point) end)
        (setq n (1+ n))
        (push (list n (org-id-get) (org-get-heading t t t t)
                    (org-outline-level) (point-marker))
              out)
        (unless (outline-next-heading) (goto-char end)))
      (nreverse out))))

(defun org-upwell-matrix--items (columns)
  "Return the items claimed by any of COLUMNS, ordered as the bench orders."
  (let ((ids (delq nil (mapcar (lambda (c) (nth 1 c)) columns)))
        seen out)
    (dolist (id ids)
      (dolist (m (org-upwell-claimed-to id))
        (unless (member (plist-get m :id) seen)
          (push (plist-get m :id) seen)
          (push m out))))
    (setq out (nreverse out))
    ;; Directories first, as on the bench, and for the same reason.
    (append (seq-filter #'org-upwell-directory-item-p out)
            (seq-remove #'org-upwell-directory-item-p out))))

(defun org-upwell-matrix--status (item column)
  "Return ITEM's claim status on COLUMN, or nil.

Whatever the store says, including a refusal -- which then draws nothing,
because `org-upwell-matrix-glyphs\=' has no mark for one.  Filtering here
as well would be a second place saying the same thing, and the one that
would be forgotten when a status is added."
  (when-let ((id (nth 1 column)))
    (org-upwell-claim-status (plist-get item :claims) id)))

;;;; Drawing

(defconst org-upwell-matrix-grid-column 10
  "Screen column the grid starts at.

The indent a row opens with, then the form column and its gap: what a
thing is comes before which headings hold it, and the marks then sit
against the name.")

(defun org-upwell-matrix--widths (rows columns width)
  "Return (NAME . WHERE) for ROWS under COLUMNS in WIDTH columns."
  (let* ((grid (max 1 (1- (* 2 (length columns)))))
         ;; the indent a row starts with, the grid, the form, three gaps
         (fixed (+ 2 grid 2 6 2 2))
         (room (max 16 (- width fixed)))
         (asked-name (apply #'max 4 (mapcar (lambda (m)
                                              (string-width
                                               (or (plist-get m :name) "?")))
                                            rows)))
         (asked-where (apply #'max 4
                             (mapcar (lambda (m)
                                       (string-width
                                        (org-upwell--item-where m)))
                                     rows)))
         (name (min asked-name (max 8 (- room 10)))))
    ;; Neither column is given more than it asks for: a wide frame should
    ;; not put a hand's width of blank between two short columns, and the
    ;; grid is what the room is for.
    ;; Neither column is given more than it asks for, and the where column
    ;; is held short besides: the grid is what this buffer is for, and every
    ;; column of path between the name and the grid is one the eye crosses on
    ;; every row.  The whole of a path is in the echo area.
    (cons name (max 6 (min asked-where 24 (- room name))))))

(defun org-upwell-matrix--rule (columns _widths)
  "Return the line of column numbers standing over the grid, or lines.

Two lines once there are ten columns: a single digit cannot say which of
1 and 11 it is, and the list above the grid is a list, not a ruler."
  (let* ((lead (make-string org-upwell-matrix-grid-column ?\s))
         (n (length columns))
         (units (mapconcat (lambda (c) (number-to-string (% (nth 0 c) 10)))
                           columns " ")))
    (concat
     (when (> n 9)
       (concat lead
               (string-trim-right
                (mapconcat (lambda (c)
                             (let ((tens (/ (nth 0 c) 10)))
                               (if (> tens 0) (number-to-string tens) " ")))
                           columns " "))
               "\n"))
     lead (string-trim-right units) "\n")))

(defun org-upwell-matrix--insert-columns (columns width)
  "Insert the list keying each number in COLUMNS to its heading, in WIDTH."
  (let (marks)
    (dolist (c columns)
      (pcase-let ((`(,n ,id ,title ,level ,_) c))
        (let* ((lead (format "  %2d  %s" n (make-string (* 2 (1- level)) ?\s)))
               (room (max 8 (- width (string-width lead) 2)))
               ;; From the number, not from the indent: the number is the half
               ;; of this line the grid shows, so it is the half to light up.
               (beg (+ (point) 2)))
          (insert lead
                  (truncate-string-to-width (or title "?") room nil nil t))
          (push (cons beg (point)) marks)
          (insert (if id "" (propertize "  (no id)" 'face 'shadow)) "\n"))))
    (setq org-upwell-matrix--column-marks (nreverse marks))))

(defun org-upwell-matrix--insert-row (m columns widths)
  "Insert the row for item M across COLUMNS, in WIDTHS."
  (let* ((name (or (plist-get m :name) "?"))
         (start (point)))
    (insert "  ")
    (insert (propertize (org-upwell--pad (org-upwell-form m) 6) 'face 'shadow)
            "  ")
    ;; The grid between the form and the name, not past the path: a mark and
    ;; the thing it is about have to be read together, and every column
    ;; between them is one the eye crosses on every row.
    (insert (mapconcat (lambda (c)
                         (or (cdr (assq (org-upwell-matrix--status m c)
                                        org-upwell-matrix-glyphs))
                             " "))
                       columns " ")
            "  ")
    (insert (org-upwell--pad
             (truncate-string-to-width name (car widths) nil nil t)
             (car widths))
            "  ")
    (insert (string-trim-right
             (propertize (org-upwell--fit-where m (cdr widths))
                         'face 'shadow))
            "\n")
    (put-text-property start (1- (point)) 'org-upwell m)))

(defun org-upwell-matrix--grid-index ()
  "Return the index of the grid column point is over, or nil.

The gap between two marks reads as the mark to its left, so landing a
column short of a cell still names the cell somebody meant."
  (let ((off (- (current-column) org-upwell-matrix-grid-column)))
    (when (and (>= off 0) org-upwell-matrix--columns
               (< (/ off 2) (length org-upwell-matrix--columns)))
      (/ off 2))))

(defun org-upwell-matrix--grid-column ()
  "Return the column point is over, or nil when it is not over the grid."
  (when-let ((i (org-upwell-matrix--grid-index)))
    (nth i org-upwell-matrix--columns)))

(defun org-upwell-matrix--goto-index (i)
  "Put point on grid column I of this line."
  (move-to-column (+ org-upwell-matrix-grid-column (* 2 i))))

(defun org-upwell-matrix--row-line-p ()
  "Return non-nil when this line is one of the grid\='s rows."
  (and (org-upwell-matrix--item-at-point) t))

(defun org-upwell-matrix--goto-first-cell ()
  "Put point on the first cell, if the grid has one.

A grid whose commands all want a cell has to open on one.  Opening at
the top of the buffer leaves every one of them saying \"not on the
grid\", which is true and useless."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (org-upwell-matrix--row-line-p)))
    (forward-line 1))
  (if (org-upwell-matrix--row-line-p)
      (org-upwell-matrix--goto-index 0)
    (goto-char (point-min))))

(defun org-upwell-matrix--width ()
  "Columns available to this grid."
  (let ((win (get-buffer-window (current-buffer) nil)))
    (if (window-live-p win) (window-body-width win) (frame-width))))

(defun org-upwell-matrix--item-at-point ()
  "Return the item on this line, or nil."
  (or (get-text-property (point) 'org-upwell)
      (get-text-property (line-beginning-position) 'org-upwell)))

;;;; The commands

(defconst org-upwell-matrix-commands
  '((org-upwell-matrix-backward-column move   "a column left")
    (org-upwell-matrix-forward-column   move   "a column right")
    (org-upwell-matrix-previous-row     move   "a row up")
    (org-upwell-matrix-next-row         move   "a row down")
    (org-upwell-matrix-widen            family "out to the parent")
    (org-upwell-matrix-narrow           family "in to this column")
    (org-upwell-matrix-choose           family "another heading")
    (org-upwell-matrix-unlink           cell   "take it off")
    (org-upwell-matrix-keep             cell   "keep it there")
    (org-upwell-matrix-add              column "bring one in")
    (org-upwell-matrix-copy-column      column "copy this column")
    (org-upwell-matrix-goto             column "go to the heading")
    (org-upwell-matrix-rename           row    "rename it")
    (org-upwell-matrix-forget           row    "forget it")
    (org-upwell-matrix-visit-store      row    "the store file")
    (org-upwell-matrix-redraw           page   "read again")
    (org-upwell-matrix-quit             page   "close"))
  "What the foot of the grid names: (COMMAND SCOPE WHAT).

Moving is named here and not on the bench because on a bench the cursor
is already where the commands act -- a line -- and on a grid it is not.

Four places to stand rather than the bench\\='s two, because a grid has more
of them: a cell is one heading\\='s claim on one thing, a column is the
heading whatever row you are on, a row is the thing itself, and none of
them is the whole page.  Pressing a cell command off the grid says so
rather than guessing which column was meant.")

(defun org-upwell-matrix--cell ()
  "Return (ITEM . COLUMN) for the cell at point, or signal."
  (let ((m (or (org-upwell-matrix--item-at-point)
               (user-error "No item on this line")))
        (c (or (org-upwell-matrix--grid-column)
               (user-error "Not on the grid; move right to a column"))))
    (cons m c)))

(defun org-upwell-matrix-unlink ()
  "Take this thing off this heading, leaving both of them."
  (interactive)
  (pcase-let* ((`(,m . ,c) (org-upwell-matrix--cell)))
    (unless (nth 1 c)
      (user-error "That heading holds nothing"))
    (org-upwell-unclaim m (nth 1 c))
    (org-upwell-matrix-redraw)
    (message "org-upwell: %s off \"%s\"" (plist-get m :name) (nth 2 c))))

(defun org-upwell-matrix-keep ()
  "Keep this thing on this heading: the answer a proposal was waiting for."
  (interactive)
  (pcase-let* ((`(,m . ,c) (org-upwell-matrix--cell)))
    (unless (nth 1 c)
      (user-error "That heading holds nothing"))
    (org-upwell-claim m (nth 1 c) 'confirmed)
    (org-upwell-matrix-redraw)
    (message "org-upwell: %s kept on \"%s\"" (plist-get m :name) (nth 2 c))))

(defun org-upwell-matrix-goto ()
  "Go to the heading this column is."
  (interactive)
  (let ((c (or (org-upwell-matrix--grid-column)
               (user-error "Not on the grid; move right to a column"))))
    (org-upwell--show-away-from-listing (marker-buffer (nth 4 c)))
    (goto-char (nth 4 c))
    (org-fold-show-entry)))

(defun org-upwell-matrix-copy-column (to)
  "Copy this column\\='s things onto the heading of column TO.

The same act as \\[org-upwell-copy-from], asked the way a grid makes
natural: the column under the cursor is the one being read, and the
number is what the list above the grid is for."
  (interactive
   (list (read-number
          (format "Copy column %s onto which column? "
                  (or (car (org-upwell-matrix--grid-column)) "?")))))
  (let* ((from (or (org-upwell-matrix--grid-column)
                   (user-error "Not on the grid; move right to a column")))
         (target (or (seq-find (lambda (c) (= (nth 0 c) to))
                              org-upwell-matrix--columns)
                     (user-error "No column %d" to))))
    (when (eq from target)
      (user-error "That is this column"))
    (unless (and (nth 1 from) (nth 1 target))
      (user-error "A heading with no id holds nothing"))
    (let ((n (org-upwell-inherit-to-heading (nth 1 from) (nth 1 target))))
      (org-upwell-matrix-redraw)
      (message "org-upwell: %d copied onto \"%s\"" n (nth 2 target)))))

(defun org-upwell-matrix--candidates (heading-id)
  "Return (LABEL . ITEM) for store items HEADING-ID does not hold.

Already held is left out because the answer for those is a cell, not a
prompt: the row is on the grid and \[org-upwell-matrix-keep] is the key
for it.  A rejection is not holding, so a thing said no to can be
brought back -- saying no and changing your mind is the ordinary case."
  (delq nil
        (mapcar
         (lambda (m)
           (unless (memq (org-upwell-claim-status (plist-get m :claims)
                                                  heading-id)
                         '(provisional confirmed))
             (cons (format "%-6s %s  %s"
                           (org-upwell-form m)
                           (or (plist-get m :name) "?")
                           (or (org-upwell--item-where m) ""))
                   m)))
         (org-upwell-items))))

(defun org-upwell-matrix-add ()
  "Put something this column does not hold onto it, and keep it.

The grid can only draw what some heading in the family already holds, so
the file that belongs on this task but was filed under last month\='s is
exactly the one missing from the picture -- and no amount of reading the
grid will show it.  This is the way in: anything in the store that this
heading does not hold, claimed here as an answer rather than a proposal,
because somebody chose it."
  (interactive)
  (let* ((c (or (org-upwell-matrix--grid-column)
                (user-error "Not on the grid; move right to a column")))
         (id (or (nth 1 c) (user-error "That heading holds nothing")))
         (cands (org-upwell-with-store (org-upwell-matrix--candidates id))))
    (unless cands
      (user-error "This heading already holds everything in the store"))
    (let* ((label (completing-read (format "Keep on \"%s\": " (nth 2 c))
                                   (mapcar #'car cands) nil t))
           (chosen (cdr (assoc label cands)))
           ;; Re-read: the snapshot above is from before the prompt, and
           ;; `org-upwell-claim' writes the claims it is handed.
           (m (or (org-upwell-find :id (plist-get chosen :id)) chosen)))
      (org-upwell-claim m id 'confirmed)
      (org-upwell-matrix-redraw)
      (message "org-upwell: %s kept on \"%s\""
               (plist-get m :name) (nth 2 c)))))

(defun org-upwell-matrix-rename ()
  "Rename this thing, in the store.  The file is not touched."
  (interactive)
  (let* ((m (or (org-upwell-matrix--item-at-point)
                (user-error "No item on this line")))
         (new (string-trim
               (read-string "Name: " (or (plist-get m :name) "")))))
    (when (string-empty-p new)
      (user-error "A thing with no name cannot be found again"))
    (org-upwell-save (plist-put (copy-sequence m) :name new))
    (org-upwell-matrix-redraw)))

(defun org-upwell-matrix-forget ()
  "Delete this thing from the store, off every heading.  The file stays."
  (interactive)
  (let ((m (or (org-upwell-matrix--item-at-point)
               (user-error "No item on this line"))))
    (when (yes-or-no-p (format "Forget %s everywhere?  The file stays.  "
                               (plist-get m :name)))
      (org-upwell-forget (plist-get m :id))
      (org-upwell-matrix-redraw))))

(defun org-upwell-matrix-visit-store ()
  "Visit this thing's heading in the store."
  (interactive)
  (when-let ((m (org-upwell-matrix--item-at-point)))
    (org-upwell-visit-store m)))

(defun org-upwell-matrix-forward-column (&optional n)
  "Move N grid columns to the right, stopping at the last one."
  (interactive "p")
  (let ((i (or (org-upwell-matrix--grid-index) -1))
        (last (1- (length org-upwell-matrix--columns))))
    (when (< last 0) (user-error "This grid has no columns"))
    (org-upwell-matrix--goto-index
     (max 0 (min last (+ i (or n 1)))))))

(defun org-upwell-matrix-backward-column (&optional n)
  "Move N grid columns to the left, stopping at the first one."
  (interactive "p")
  (org-upwell-matrix-forward-column (- (or n 1))))

(defun org-upwell-matrix-next-row (&optional n)
  "Move N rows down, staying on the grid column point is in.

Plain line motion walks off the grid onto the legend, and `next-line\='
keeps a goal column that the header lines do not have; both leave the
cursor somewhere no cell command can be used from."
  (interactive "p")
  (let ((i (or (org-upwell-matrix--grid-index) 0))
        (step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (let ((from (point)))
        (forward-line step)
        (while (and (not (if (< step 0) (bobp) (eobp)))
                    (not (org-upwell-matrix--row-line-p)))
          (forward-line step))
        (unless (org-upwell-matrix--row-line-p)
          (goto-char from))))
    (org-upwell-matrix--goto-index i)))

(defun org-upwell-matrix-previous-row (&optional n)
  "Move N rows up, staying on the grid column point is in."
  (interactive "p")
  (org-upwell-matrix-next-row (- (or n 1))))

(defun org-upwell-matrix--reroot (marker what)
  "Draw the family of MARKER instead, saying WHAT changed.

The rows are a different set, so the cursor goes back to the first cell
rather than to the line it was on: the line number it held meant a thing
that may not be in this grid at all."
  (org-upwell-matrix--draw marker)
  (message "org-upwell: %s -- %s" what
           (org-with-point-at marker (org-get-heading t t t t))))

(defun org-upwell-matrix-widen ()
  "Show the parent\='s family instead: one step out.

The important direction.  A stray file usually came from a neighbouring
project, and that is a relation between two families -- invisible until
both of their tasks are columns of one grid, for the same reason a single
bench cannot show what is wrong inside one family."
  (interactive)
  (let ((up (org-with-point-at org-upwell-matrix--root
              (org-back-to-heading t)
              (and (org-up-heading-safe) (point-marker)))))
    (unless up
      (user-error "This family is already the top of the file"))
    (org-upwell-matrix--reroot up "out to")))

(defun org-upwell-matrix-narrow ()
  "Show the family of the column at point instead: one step in.

What is left after widening twice is a grid too wide to read, and most of
it is not what you are working on."
  (interactive)
  (let ((c (or (org-upwell-matrix--grid-column)
               (user-error "Not on the grid; move right to a column"))))
    (when (equal (nth 4 c) org-upwell-matrix--root)
      (user-error "That column is this family"))
    (org-upwell-matrix--reroot (nth 4 c) "in to")))

(defun org-upwell-matrix-choose ()
  "Show the family of a heading read from a list.

Rooted at the heading itself rather than at its parent: a heading somebody
picks out of a list is the family they mean, not one of its members.
\[org-upwell-matrix-widen] is one press away when it was not."
  (interactive)
  (org-upwell-matrix--reroot (org-upwell--read-heading-marker) "showing"))

(defun org-upwell-matrix-show-column ()
  "Say which heading the cursor\\='s column is, up in the list.

Two characters is not a word.  The echo area answers too, but it is gone
the moment anything else speaks; the list above the grid is where the
number is spelled out, so it is where the answer keeps.

Bound to nothing and run from `post-command-hook\\=' instead: the question is
asked by every move of the cursor, and answering it only when somebody
presses a key would be answering it after they had given up."
  (interactive)
  (when (derived-mode-p 'org-upwell-matrix-mode)
    (let* ((i (org-upwell-matrix--grid-index))
           (at (and i (nth i org-upwell-matrix--column-marks))))
      (if (null at)
          (when (overlayp org-upwell-matrix--column-overlay)
            (delete-overlay org-upwell-matrix--column-overlay))
        (unless (overlayp org-upwell-matrix--column-overlay)
          (setq org-upwell-matrix--column-overlay (make-overlay 1 1))
          (overlay-put org-upwell-matrix--column-overlay
                       'face 'org-upwell-matrix-column))
        ;; With the buffer named: a deleted overlay has none of its own, and
        ;; `overlayp' stays true of it, so this is also how the one deleted on
        ;; stepping off the grid comes back when the cursor returns.
        (move-overlay org-upwell-matrix--column-overlay
                      (car at) (cdr at) (current-buffer))))))

(defun org-upwell-matrix-quit ()
  "Close the grid."
  (interactive)
  (quit-window t))

(defun org-upwell-matrix-redraw ()
  "Draw the grid again, from the store, keeping the cursor where it is."
  (interactive)
  (unless (derived-mode-p 'org-upwell-matrix-mode)
    (user-error "Not the matrix"))
  (let ((line (line-number-at-pos))
        (column (current-column))
        (root org-upwell-matrix--root))
    (org-upwell-matrix--draw root)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

;;;; The mode

(defun org-upwell-matrix-eldoc-function (&rest _)
  "Say which heading the cursor\\='s column is, and what the row is.

The only practical way to read a column two characters wide."
  (when (derived-mode-p 'org-upwell-matrix-mode)
    (let ((m (org-upwell-matrix--item-at-point))
          (c (org-upwell-matrix--grid-column)))
      (cond
       ((and m c) (format "%s  —  %s" (nth 2 c) (plist-get m :name)))
       (c (nth 2 c))
       (m (plist-get m :name))))))

(defvar org-upwell-matrix-mode-map (make-sparse-keymap))
(let ((map org-upwell-matrix-mode-map))
  (set-keymap-parent map special-mode-map)
  (define-key map (kbd "q") #'org-upwell-matrix-quit)
  (define-key map (kbd "g") #'org-upwell-matrix-redraw)
  (define-key map (kbd "r") #'org-upwell-matrix-redraw)
  (define-key map (kbd "n") #'org-upwell-matrix-next-row)
  (define-key map (kbd "p") #'org-upwell-matrix-previous-row)
  (define-key map (kbd "j") #'org-upwell-matrix-next-row)
  (define-key map (kbd "k") #'org-upwell-matrix-previous-row)
  (define-key map (kbd "<down>") #'org-upwell-matrix-next-row)
  (define-key map (kbd "<up>") #'org-upwell-matrix-previous-row)
  (define-key map (kbd "l") #'org-upwell-matrix-forward-column)
  (define-key map (kbd "h") #'org-upwell-matrix-backward-column)
  (define-key map (kbd "<right>") #'org-upwell-matrix-forward-column)
  (define-key map (kbd "<left>") #'org-upwell-matrix-backward-column)
  (define-key map (kbd "d") #'org-upwell-matrix-unlink)
  (define-key map (kbd "c") #'org-upwell-matrix-keep)
  (define-key map (kbd "a") #'org-upwell-matrix-add)
  ;; Left is out and right is in, because the list above the grid prints the
  ;; family as an indented tree: a child sits to the right of its parent
  ;; there, so the key that moves to a child has to point the same way.  The
  ;; agenda has `<' the other way round and no indented tree on screen for a
  ;; direction to disagree with.
  (define-key map (kbd "<") #'org-upwell-matrix-widen)
  (define-key map (kbd ">") #'org-upwell-matrix-narrow)
  (define-key map (kbd "f") #'org-upwell-matrix-choose)
  ;; TAB, because this is the act the agenda puts on TAB: go to the entry and
  ;; leave the listing standing.  `org-upwell-matrix-goto' shows the heading in
  ;; a window that is not this one, which is `org-agenda-goto' exactly.
  (define-key map (kbd "TAB") #'org-upwell-matrix-goto)
  (define-key map (kbd "RET") #'org-upwell-matrix-goto)
  (define-key map (kbd "y") #'org-upwell-matrix-copy-column)
  (define-key map (kbd "R") #'org-upwell-matrix-rename)
  (define-key map (kbd "D") #'org-upwell-matrix-forget)
  (define-key map (kbd "o") #'org-upwell-matrix-visit-store))

(define-derived-mode org-upwell-matrix-mode special-mode "Upwell-Matrix"
  "One family of headings, and everything they hold.

Rows are things, columns are headings, a cell is a claim.  `X' is an
answer somebody gave; `?' is a claim proposed and not yet answered --
provisional, the same word the bench uses for it.  An empty cell is this
heading not holding this thing.

The buffer opens on a cell and `h' and `l' walk along the grid, because
every command below wants one and a row of marks two columns apart is
not something to find by counting.  `n' and `p' change row and stay in
the column.  Which heading the cursor's column is shows in bold up in
the list above the grid, and in the echo area.

`<' and `>' move the family itself -- out to the parent and in to the
column under the cursor, the same directions the indented list above the
grid is drawn in -- and `f' reads another heading.

`d' takes a thing off this heading, emptying the cell.  `c' keeps it
here, which answers a `?' and fills an empty cell alike.  `a' brings in
something no heading in this family holds, which is the one thing the
grid cannot show you.  `y' copies a whole column onto another, the move
that made this buffer necessary and the one that makes a mess of it.
`R' renames a thing, `D' forgets it everywhere; no file is touched.

The echo area says which heading the cursor's column is, because two
characters is not a word.

\\{org-upwell-matrix-mode-map}"
  (setq truncate-lines t)
  ;; A grid is read by holding a row and a column at once, and a row of marks
  ;; two characters apart is the easiest thing in the world to slip off.
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'org-upwell-matrix-show-column nil t)
  (add-hook 'eldoc-documentation-functions
            #'org-upwell-matrix-eldoc-function nil t)
  (eldoc-mode 1)
  (when (and (boundp 'evil-state) (fboundp 'evil-emacs-state))
    (evil-emacs-state)))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'org-upwell-matrix-mode 'emacs))

;; A grid is a place somebody is choosing from, so whatever they choose must
;; not be put into it -- see `org-upwell-listing-modes'.
(add-to-list 'org-upwell-listing-modes 'org-upwell-matrix-mode)

(eldoc-add-command-completions "org-upwell-matrix")

;;;; Entry

(defun org-upwell-matrix--draw (root)
  "Draw ROOT's family into the grid buffer."
  (let ((buf (get-buffer-create org-upwell-matrix-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-upwell-matrix-mode)
        (org-upwell-matrix-mode))
      (let* ((inhibit-read-only t)
             (width (org-upwell-matrix--width))
             (columns (org-upwell-matrix--headings root))
             (rows (org-upwell-with-store
                    (org-upwell-matrix--items columns)))
             (widths (org-upwell-matrix--widths rows columns width)))
        (erase-buffer)
        (setq org-upwell-matrix--root root
              org-upwell-matrix--columns columns
              org-upwell-matrix--rows rows)
        (insert (propertize (org-with-point-at root
                              (org-get-heading t t t t))
                            'face 'org-level-1)
                "\n")
        (org-upwell-matrix--insert-columns columns width)
        (insert "\n")
        (if (null rows)
            (insert (propertize "  (nothing held here yet)\n" 'face 'shadow))
          (insert (propertize
                   (concat "  "
                           (cdr (assq 'confirmed org-upwell-matrix-glyphs))
                           " kept here    "
                           (cdr (assq 'provisional org-upwell-matrix-glyphs))
                           " provisional, nobody has answered\n")
                   'face 'shadow))
          (insert (propertize (org-upwell-matrix--rule columns widths)
                              'face 'shadow))
          (dolist (m rows)
            (org-upwell-matrix--insert-row m columns widths)))
        (insert "\n"
                (org-upwell--legend
                 org-upwell-matrix-commands width
                 '((move "moving: the commands below want a cell"
                         "moving")
                   (family "which family is shown" "which family")
                   (cell "on this cell -- a heading and a thing"
                         "on this cell")
                   (column "on this column -- the heading" "on this column")
                   (row "on this line -- the thing itself" "on this line")
                   (page "on the grid" "on the grid"))))
        (org-upwell-matrix--goto-first-cell)
        ;; Called here as well as from the hook: a grid that is drawn and then
        ;; waits for a keypress before saying which column it opened on has
        ;; said it too late.
        (org-upwell-matrix-show-column)
        (setq buffer-read-only t)))
    buf))

;;;###autoload
(defun org-upwell-matrix (&optional marker choose)
  "Lay one family of headings out as a grid, with what each of them holds.

The bench answers one heading, which is the unit for working and the
wrong one for tidying: tasks come in families, a family passes the same
files along, and after a few rounds the same document is on six headings
and one of them should never have had it.  That is a relation between two
benches, and no bench can show it.

Where the family is rooted depends on how you got here, and the two cases
want different answers:

  Stood on -- MARKER, or the heading at point -- roots at its *parent*, so
  standing on a leaf task shows the siblings it was continued from.  Its
  own subtree would be one column, which is a list.

  Named -- read from a list, because CHOOSE is non-nil under a prefix
  argument or because there is no heading at point -- roots at the heading
  itself.  What somebody picks out of a list is nearly always a container
  and they mean what is under it; its parent would put every sibling
  project into the grid at once.

Neither is final: \\[org-upwell-matrix-widen] and \\[org-upwell-matrix-narrow]
move the root out and in once the grid is up."
  (interactive (list nil current-prefix-arg))
  (let* ((here (or marker
                   (and (not choose) (org-upwell--current-heading-marker))))
         (root (if here
                   (org-upwell-matrix--root here)
                 (org-upwell--read-heading-marker)))
         (buf (org-upwell-matrix--draw root)))
    (pop-to-buffer buf)))

(provide 'org-upwell-matrix)

;;; org-upwell-matrix.el ends here
