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
  (let* ((lead "  ")
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
  (dolist (c columns)
    (pcase-let ((`(,n ,id ,title ,level ,_) c))
      (let* ((lead (format "  %2d  %s" n (make-string (* 2 (1- level)) ?\s)))
             (room (max 8 (- width (string-width lead) 2))))
        (insert lead
                (truncate-string-to-width (or title "?") room nil nil t)
                (if id "" (propertize "  (no id)" 'face 'shadow))
                "\n")))))

(defun org-upwell-matrix--insert-row (m columns widths)
  "Insert the row for item M across COLUMNS, in WIDTHS."
  (let* ((name (or (plist-get m :name) "?"))
         (start (point)))
    (insert "  ")
    ;; The grid first, beside the name rather than across a column of path
    ;; from it: a mark and the thing it is about have to be read together,
    ;; and every column between them is one the eye crosses on every row.
    (insert (mapconcat (lambda (c)
                         (or (cdr (assq (org-upwell-matrix--status m c)
                                        org-upwell-matrix-glyphs))
                             " "))
                       columns " ")
            "  ")
    (insert (propertize (org-upwell--pad (org-upwell-form m) 6) 'face 'shadow)
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

(defconst org-upwell-matrix-grid-column 2
  "Screen column the grid starts at: the indent a row opens with.")

(defun org-upwell-matrix--grid-column ()
  "Return the column point is over, or nil when it is not over the grid."
  (let ((off (- (current-column) org-upwell-matrix-grid-column)))
    (when (and (>= off 0) org-upwell-matrix--columns)
      (nth (/ off 2) org-upwell-matrix--columns))))

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
  '((org-upwell-matrix-unlink       cell "take it off")
    (org-upwell-matrix-keep         cell "keep it there")
    (org-upwell-matrix-goto         cell "go to the heading")
    (org-upwell-matrix-copy-column  cell "copy this column")
    (org-upwell-matrix-rename       row  "rename it")
    (org-upwell-matrix-forget       row  "forget it")
    (org-upwell-matrix-visit-store  row  "the store file")
    (org-upwell-matrix-redraw       page "read again")
    (org-upwell-matrix-quit         page "close"))
  "What the foot of the grid names: (COMMAND SCOPE WHAT).

Three scopes here rather than the bench\\='s two, because a grid has a third
place to stand: a cell is one heading\\='s claim on one thing, a row is the
thing itself, and neither is the whole page.  Pressing a cell command off
the grid says so rather than guessing which column was meant.")

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
    (org-upwell--show-away-from-bench (marker-buffer (nth 4 c)))
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
  (define-key map (kbd "n") #'next-line)
  (define-key map (kbd "p") #'previous-line)
  (define-key map (kbd "j") #'next-line)
  (define-key map (kbd "k") #'previous-line)
  (define-key map (kbd "d") #'org-upwell-matrix-unlink)
  (define-key map (kbd "c") #'org-upwell-matrix-keep)
  (define-key map (kbd "RET") #'org-upwell-matrix-goto)
  (define-key map (kbd "y") #'org-upwell-matrix-copy-column)
  (define-key map (kbd "R") #'org-upwell-matrix-rename)
  (define-key map (kbd "D") #'org-upwell-matrix-forget)
  (define-key map (kbd "o") #'org-upwell-matrix-visit-store))

(define-derived-mode org-upwell-matrix-mode special-mode "Upwell-Matrix"
  "One family of headings, and everything they hold.

Rows are things, columns are headings, a cell is a claim.  `×' is an
answer somebody gave; `·' is a proposal nobody has looked at.

`d' takes a thing off one heading and leaves both.  `c' keeps it there.
`y' copies a whole column onto another, which is the move that made this
buffer necessary and the one that makes a mess of it.  `R' renames a
thing, `D' forgets it everywhere; the files are not touched by either.

The echo area says which heading the cursor's column is, because two
characters is not a word.

\\{org-upwell-matrix-mode-map}"
  (setq truncate-lines t)
  ;; A grid is read by holding a row and a column at once, and a row of marks
  ;; two characters apart is the easiest thing in the world to slip off.
  (hl-line-mode 1)
  (add-hook 'eldoc-documentation-functions
            #'org-upwell-matrix-eldoc-function nil t)
  (eldoc-mode 1)
  (when (and (boundp 'evil-state) (fboundp 'evil-emacs-state))
    (evil-emacs-state)))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'org-upwell-matrix-mode 'emacs))

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
                           " proposed, not answered yet\n")
                   'face 'shadow))
          (insert (propertize (org-upwell-matrix--rule columns widths)
                              'face 'shadow))
          (dolist (m rows)
            (org-upwell-matrix--insert-row m columns widths)))
        (insert "\n"
                (org-upwell--legend
                 org-upwell-matrix-commands width
                 '((cell "on this cell -- a heading and a thing"
                         "on this cell")
                   (row "on this line -- the thing itself" "on this line")
                   (page "on the grid" "on the grid"))))
        (goto-char (point-min))
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

The family is the heading at point\\='s *parent* and everything under it, so
standing on a leaf task shows the siblings it was continued from.  With a
prefix argument, CHOOSE is non-nil and the family is read from a list."
  (interactive (list nil current-prefix-arg))
  (let* ((here (or marker
                   (and (not choose) (org-upwell--current-heading-marker))
                   (org-upwell--read-heading-marker)))
         (root (org-upwell-matrix--root here))
         (buf (org-upwell-matrix--draw root)))
    (pop-to-buffer buf)))

(provide 'org-upwell-matrix)

;;; org-upwell-matrix.el ends here
