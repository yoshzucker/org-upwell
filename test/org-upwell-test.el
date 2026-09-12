;;; org-upwell-test.el --- Tests for org-upwell  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run from the package root:
;;
;;   emacs --batch -Q -L . -l test/org-upwell-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Every test here was checked against a deliberately broken implementation
;; before being kept.  A test that passes either way is not a test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-upwell)
(require 'org-upwell-core)
(require 'org-upwell-trace)
(require 'org-upwell-claim)
(require 'org-upwell-pin)
(require 'org-upwell-bench)
(require 'org-upwell-demo)

(defmacro org-upwell-test--with-dir (&rest body)
  "Run BODY with org-directory and traces pointed at a scratch dir."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "org-upwell" t)))
          (org-directory dir)
          (org-upwell-directory nil)
          (org-agenda-files nil)
          (org-id-track-globally nil)
          (org-id-locations-file (expand-file-name ".org-id-locations" dir))
          (org-upwell-file-name "upwell.org")
          (org-upwell-trace-directory (expand-file-name "trace" dir))
          (org-upwell-review-on-clock-out nil)
          (org-upwell-follow-mode nil)
          (org-upwell--follow-seen nil)
          ;; Global, and `wanted' once any test has shown the bench.  Left
          ;; alone it makes every later test think agenda stole the window.
          (org-upwell--bench-intent nil))
     (make-directory org-upwell-trace-directory t)
     (when-let ((b (get-buffer "*org-upwell*")))
       (dolist (w (get-buffer-window-list b nil t))
         (when (and (window-live-p w) (not (one-window-p)))
           (delete-window w)))
       (kill-buffer b))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun org-upwell-test--write-journal (text)
  "Write TEXT to journal.org under `org-directory' and visit it."
  (let ((f (expand-file-name "journal.org" org-directory)))
    (with-temp-file f (insert text))
    (setq org-agenda-files (list f))
    f))

(defun org-upwell-test--write-trace (unix path &optional url)
  "Append one JSONL sample at UNIX seconds for PATH/URL."
  (let ((f (org-upwell-trace-file (seconds-to-time unix))))
    (make-directory (file-name-directory f) t)
    (with-temp-buffer
      (insert (format
               "{\"ts\":%d,\"app\":\"Excel\",\"title\":\"%s\",\"path\":\"%s\",\"url\":\"%s\",\"kind\":\"file\"}\n"
               unix
               (or (and path (file-name-nondirectory path)) "x")
               (or path "")
               (or url "")))
      (append-to-file (point-min) (point-max) f))))

(defun org-upwell-test--two-panes (buffer)
  "Split the frame left and right, put BUFFER on the right and select it.

Return (LEFT . RIGHT).  This is the shape a wide frame is in after
`org-agenda-window-setup' is `reorganize-frame': one pane reading the
entry, one holding the agenda, and the agenda selected."
  (delete-other-windows)
  (let* ((left (selected-window))
         (right (split-window left nil 'right)))
    (set-window-buffer right buffer)
    (select-window right)
    (cons left right)))

(defun org-upwell-test--heading-marker (file &optional n)
  "Return a marker on heading N (1-based, default first) in FILE.

Must not use `org-next-visible-heading' from point-min when the
first line is already a heading: that skips the first heading, and
a test against the wrong entry is how a C-c v bug hides."
  (with-current-buffer (find-file-noselect file)
    (org-mode)
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (re-search-forward org-heading-regexp nil t)
      (goto-char (match-beginning 0)))
    (dotimes (_ (1- (or n 1)))
      (outline-next-heading))
    (point-marker)))

(defun org-upwell-test--marker-at-id (file id)
  "Return a marker on the heading in FILE whose :ID: is ID."
  (with-current-buffer (find-file-noselect file)
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$")
             nil t)
      (error "org-upwell-test: no heading with id %s in %s" id file))
    (org-back-to-heading t)
    (point-marker)))

;;;; Mint

(ert-deftest org-upwell-test-mint-sharepoint-excel ()
  "A SharePoint :x: link becomes an Excel ofe protocol."
  (should (equal (org-upwell-mint-office
                  "https://contoso.sharepoint.com/:x:/r/sites/a/foo.xlsx")
                 "ms-excel:ofe|u|https://contoso.sharepoint.com/:x:/r/sites/a/foo.xlsx")))

(ert-deftest org-upwell-test-mint-pptx-extension ()
  (should (equal (org-upwell-mint-office "https://files.example/deck.pptx?web=1")
                 "ms-powerpoint:ofe|u|https://files.example/deck.pptx?web=1")))

(ert-deftest org-upwell-test-mint-already-protocol ()
  (should (equal (org-upwell-mint-office "ms-excel:ofe|u|https://x")
                 "ms-excel:ofe|u|https://x")))

(ert-deftest org-upwell-test-mint-ignores-plain-web ()
  (should (null (org-upwell-mint-office "https://example.com/readme")))
  (should (null (org-upwell-mint-office "/tmp/foo.xlsx"))))

;;;; Claims

(ert-deftest org-upwell-test-confirmed-is-not-downgraded ()
  "A later provisional write must not undo a confirmed claim."
  (let ((c (org-upwell-claims-put
            (org-upwell-claims-put nil "abc" 'confirmed)
            "abc" 'provisional)))
    (should (eq 'confirmed (org-upwell-claim-status c "abc")))))

(ert-deftest org-upwell-test-provisional-promotes ()
  (let ((c (org-upwell-claims-put
            (org-upwell-claims-put nil "abc" 'provisional)
            "abc" 'confirmed)))
    (should (eq 'confirmed (org-upwell-claim-status c "abc")))))

(ert-deftest org-upwell-test-claims-point-up ()
  "Claims store the user heading's id, not a path back from the heading."
  (should (equal (org-upwell--format-claims
                  (list (list :id "HEAD-ID" :status 'provisional)))
                 "HEAD-ID|provisional")))

;;;; Store

(ert-deftest org-upwell-test-save-and-find-by-path ()
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "doc.xlsx" dir)))
     (write-region "x" nil p)
     (org-upwell-save (list :path p :name "doc.xlsx" :provenance "pin"))
     (let ((m (org-upwell-find :path p)))
       (should m)
       (should (equal (plist-get m :name) "doc.xlsx"))
       (should (org-entry-get (plist-get m :marker) org-upwell-prop-flag))))))

(ert-deftest org-upwell-test-save-fills-appearances-does-not-clear ()
  "A trace that knows the path must not wipe a protocol a pin already had."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "doc.xlsx" dir))
          (office "ms-excel:ofe|u|https://share/doc.xlsx"))
     (write-region "x" nil p)
     (org-upwell-save (list :path p :office office :name "doc.xlsx"))
     (org-upwell-save (list :path p :name "doc.xlsx" :provenance "trace"))
     (let ((m (org-upwell-find :path p)))
       (should (equal (plist-get m :office) office))))))

(ert-deftest org-upwell-test-unclaimed-until-claimed ()
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/f.xlsx"
                                   :name "f.xlsx"))))
     (should (org-upwell-unclaimed-p m))
     (setq m (org-upwell-claim m "HEAD" 'provisional))
     (should-not (org-upwell-unclaimed-p m))
     (should (eq 'provisional
                 (org-upwell-claim-status (plist-get m :claims) "HEAD"))))))

;;;; Resolve after move

(ert-deftest org-upwell-test-resolve-after-rename ()
  "A rename on the same volume keeps the file-id; expand still finds it."
  (org-upwell-test--with-dir
   (let* ((a (expand-file-name "a.txt" dir))
          (sub (expand-file-name "done" dir))
          (b (expand-file-name "a.txt" sub)))
     (write-region "hello" nil a)
     (let ((m (org-upwell-save (list :path a :name "a.txt"))))
       (make-directory sub t)
       (rename-file a b)
       (let ((org-upwell-search-roots (list dir)))
         (let ((app (org-upwell-resolve (org-upwell-find :id (plist-get m :id)))))
           (should (eq (plist-get app :kind) 'path))
           (should (equal (file-truename (plist-get app :value))
                          (file-truename b)))))))))

;;;; Traces and retroactive clocks

(ert-deftest org-upwell-test-trace-point-in-interval ()
  (let* ((from (encode-time 0 0 10 5 9 2026))
         (to (encode-time 0 0 11 5 9 2026))
         (inside (list :ts (floor (float-time (encode-time 0 30 10 5 9 2026)))))
         (edge (list :ts (floor (float-time to))))
         (before (list :ts (floor (float-time (encode-time 0 0 9 5 9 2026))))))
    (should (org-upwell-trace-in-interval-p inside from to))
    (should-not (org-upwell-trace-in-interval-p edge from to))
    (should-not (org-upwell-trace-in-interval-p before from to))))

(ert-deftest org-upwell-test-no-clock-means-unclaimed ()
  "Traces with no overlapping clock become unclaimed, but no heading is guessed."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "off-clock.xlsx" dir))
          (now (floor (float-time))))
     (write-region "x" nil p)
     (org-upwell-test--write-trace now p)
     (org-upwell-sync 1)
     (let ((m (org-upwell-find :path p)))
       (should m)
       (should (org-upwell-unclaimed-p m))))))

(ert-deftest org-upwell-test-retroactive-clock-claims-traces ()
  "A clock written after the fact still attributes traces in its interval.

This is foresight's C: the work already happened, the clock lands later."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "later.xlsx" dir))
          (now (current-time))
          (from (time-subtract now 3600))
          (to now)
          (mid (time-subtract now 1800))
          journal marker)
     (write-region "x" nil p)
     (setq journal
           (org-upwell-test--write-journal
            "* buy the thing\n:PROPERTIES:\n:ID: BUY-1\n:END:\n"))
     (org-upwell-test--write-trace (floor (float-time mid)) p)
     ;; Before the clock exists, the trace is unclaimed.
     (org-upwell-sync 1)
     (should (org-upwell-unclaimed-p (org-upwell-find :path p)))
     ;; A clock is filled in afterwards, covering the sample.
     (setq marker (org-upwell-test--heading-marker journal))
     (org-with-point-at marker
       (org-clock-find-position nil)
       (insert-before-markers
        "\nCLOCK: " (format-time-string (org-time-stamp-format t t) from)
        "--" (format-time-string (org-time-stamp-format t t) to)))
     (org-upwell-claim-interval marker from to)
     (let ((m (org-upwell-find :path p)))
       (should (eq 'provisional
                   (org-upwell-claim-status (plist-get m :claims)
                                            (org-with-point-at marker
                                              (org-id-get)))))))))

(ert-deftest org-upwell-test-pin-without-heading-is-unclaimed ()
  (org-upwell-test--with-dir
   (let ((p (expand-file-name "manual.pdf" dir)))
     (write-region "x" nil p)
     (cl-letf (((symbol-function 'org-upwell--current-heading-marker)
                (lambda () nil)))
       (let ((m (org-upwell-pin p nil "pin")))
         (should (org-upwell-unclaimed-p m)))))))

(ert-deftest org-upwell-test-pin-to-heading-is-confirmed ()
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "manual.pdf" dir))
          (journal (org-upwell-test--write-journal
                    "* a project\n:PROPERTIES:\n:ID: PROJ-1\n:END:\n"))
          (marker (org-upwell-test--heading-marker journal)))
     (write-region "x" nil p)
     (let ((m (org-upwell-pin p marker "drop")))
       (should (eq 'confirmed
                   (org-upwell-claim-status (plist-get m :claims) "PROJ-1")))))))

(ert-deftest org-upwell-test-dnd-returns-private ()
  "A drop must not be reported as a move."
  (org-upwell-test--with-dir
   (let ((p (expand-file-name "dropped.txt" dir)))
     (write-region "x" nil p)
     (should (eq 'private
                 (org-upwell-dnd-file (concat "file://" p) 'move)))
     (should (file-exists-p p)))))

(ert-deftest org-upwell-test-claimed-to-returns-only-that-heading ()
  (org-upwell-test--with-dir
   (let ((a (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/a.xlsx"
                                   :name "a.xlsx")))
         (b (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/b.xlsx"
                                   :name "b.xlsx"))))
     (org-upwell-claim a "ONE" 'confirmed)
     (org-upwell-claim b "TWO" 'provisional)
     (should (equal (mapcar (lambda (m) (plist-get m :name))
                            (org-upwell-claimed-to "ONE"))
                    '("a.xlsx")))
     (should-not (seq-find (lambda (m) (equal (plist-get m :name) "b.xlsx"))
                           (org-upwell-claimed-to "ONE"))))))

(ert-deftest org-upwell-test-one-file-two-headings ()
  "A single item can be claimed by two user headings."
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/shared.xlsx"
                                   :name "shared.xlsx"))))
     (org-upwell-claim m "H1" 'confirmed)
     (org-upwell-claim (org-upwell-find :name "shared.xlsx") "H2" 'confirmed)
     (setq m (org-upwell-find :name "shared.xlsx"))
     (should (eq 'confirmed (org-upwell-claim-status (plist-get m :claims) "H1")))
     (should (eq 'confirmed (org-upwell-claim-status (plist-get m :claims) "H2")))
     (should (equal '("shared.xlsx")
                    (mapcar (lambda (x) (plist-get x :name))
                            (org-upwell-claimed-to "H1"))))
     (should (equal '("shared.xlsx")
                    (mapcar (lambda (x) (plist-get x :name))
                            (org-upwell-claimed-to "H2")))))))

(ert-deftest org-upwell-test-inherit-copies-provisionally ()
  "Clocking a new heading copies the last expanded heading's items."
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/deck.pptx"
                                   :name "deck.pptx"))))
     (org-upwell-claim m "DONE-HEAD" 'confirmed)
     (org-upwell-inherit-to-heading "DONE-HEAD" "NEXT-HEAD")
     (setq m (org-upwell-find :name "deck.pptx"))
     (should (eq 'confirmed
                 (org-upwell-claim-status (plist-get m :claims) "DONE-HEAD")))
     (should (eq 'provisional
                 (org-upwell-claim-status (plist-get m :claims) "NEXT-HEAD"))))))

(ert-deftest org-upwell-test-bench-split-side-follows-the-host-width ()
  "Side by side only where Emacs itself would split side by side."
  (org-upwell-test--with-dir
   (delete-other-windows)
   (let ((full (window-total-width (selected-window))))
     (let ((split-width-threshold full))
       (should (eq 'right (org-upwell--bench-split-side (selected-window)))))
     (let ((split-width-threshold (1+ full)))
       (should (eq 'below (org-upwell--bench-split-side (selected-window)))))
     ;; Nil disables side-by-side splitting everywhere in Emacs.
     (let ((split-width-threshold nil))
       (should (eq 'below (org-upwell--bench-split-side (selected-window))))))))

(ert-deftest org-upwell-test-bench-split-side-defaults-to-the-host ()
  "Omitting the window must consult the host, not the frame."
  (org-upwell-test--with-dir
   (delete-other-windows)
   (split-window (selected-window) nil 'right)
   (should (eq (org-upwell--bench-split-side)
               (org-upwell--bench-split-side
                (org-upwell--bench-host-window))))))

(ert-deftest org-upwell-test-bench-host-window-is-not-the-caller ()
  "The pane the bench was asked from keeps its size; another gives way."
  (org-upwell-test--with-dir
   (delete-other-windows)
   (let ((other (split-window (selected-window) nil 'right)))
     (should (eq other (org-upwell--bench-host-window)))
     (select-window other)
     (should-not (eq other (org-upwell--bench-host-window))))
   (delete-other-windows)
   ;; One window is the only offer there is.
   (should (eq (selected-window) (org-upwell--bench-host-window)))))

(ert-deftest org-upwell-test-bench-splits-a-window-not-the-root ()
  "A bench beside an agenda must not leave a quarter, a quarter and a half.

Splitting `frame-root-window' pushed both panes already on the frame
into whatever half was left over.  The pane the bench was asked from
keeps its width *and* its height; the other one pays for the strip."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (panes (org-upwell-test--two-panes (marker-buffer mk)))
          (left (car panes))
          (right (cdr panes))
          (lw (window-total-width left))
          (lh (window-total-height left))
          (rw (window-total-width right))
          (rh (window-total-height right))
          ;; Force the strip underneath, so a root split would show up
          ;; as the caller losing height rather than losing width.
          (split-width-threshold (1+ (max lw rw))))
     (org-upwell--bench-draw (org-upwell-domain mk))
     (should (get-buffer-window "*org-upwell*" nil))
     (should (= rw (window-total-width right)))
     (should (= rh (window-total-height right)))
     (should (= lw (window-total-width left)))
     (should (< (window-total-height left) lh)))))

(ert-deftest org-upwell-test-bench-hosts-a-window-other-than-the-caller ()
  "The strip is cut out of the pane that was not being read."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (panes (org-upwell-test--two-panes (marker-buffer mk)))
          (left (car panes))
          (right (cdr panes))
          (split-width-threshold (1+ (window-total-width left))))
     (org-upwell--bench-draw (org-upwell-domain mk))
     (let ((bench (get-buffer-window "*org-upwell*" nil)))
       (should (window-live-p bench))
       (should (eq left (window-in-direction 'above bench)))
       (should-not (eq right (window-in-direction 'above bench)))))))

(ert-deftest org-upwell-test-heading-marker-is-the-first-heading ()
  "The helper must land on the first star, not skip it."
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT Photos\n:PROPERTIES:\n:ID: DROP\n:END:\n* NEXT Quotes\n:PROPERTIES:\n:ID: Q\n:END:\n")))
     (should (equal "DROP"
                    (org-with-point-at (org-upwell-test--heading-marker file)
                      (org-id-get))))
     (should (equal "Q"
                    (org-with-point-at (org-upwell-test--heading-marker file 2)
                      (org-id-get)))))))

(ert-deftest org-upwell-test-bench-opens-nothing ()
  "Which files a heading has is a question, and the answer is a list.

Expand used to open up to `org-upwell-bench-open-max\=' of them before anybody
had seen what they were -- with an opener that hands a path to the OS,
that is eight applications taking the screen.  Opening is a second act,
from the bench."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (opened 0))
     (dolist (name '("a.txt" "b.txt" "c.txt"))
       (let ((p (expand-file-name name dir)))
         (write-region "x" nil p)
         (org-upwell-claim
          (org-upwell-save (list :path p :name name :provenance "pin"))
          "T1" 'confirmed)))
     (cl-letf (((symbol-function 'org-upwell-open)
                (lambda (&rest _) (setq opened (1+ opened)))))
       (org-upwell-bench (org-upwell-test--heading-marker file)))
     (should (= 0 opened))
     ;; and what it did instead is show them
     (should (get-buffer "*org-upwell*"))
     (with-current-buffer "*org-upwell*"
       (dolist (name '("a.txt" "b.txt" "c.txt"))
         (goto-char (point-min))
         (should (search-forward name nil t)))))))

(ert-deftest org-upwell-test-bench-answers-after-q ()
  "`q\=' dismissed the listing; asking for it again is asking for it again.

It declined once, from the days when expand opened the files and the
listing was a side effect.  Now the listing is the whole answer, so
declining would make C-c v do nothing at all."
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n")))
     (let ((org-upwell--bench-intent 'dismissed))
       (org-upwell-bench (org-upwell-test--heading-marker file))
       (should (get-buffer "*org-upwell*"))
       (should (eq org-upwell--bench-intent 'wanted))))))

(ert-deftest org-upwell-test-bench-switches-a-showing-one ()
  "C-c v on another heading must redraw the bench if it is already up.

The demo opens File the photos.  Expanding Compare the three quotes
must not leave the empty drop list on screen.  A test that only
checks expand does not *create* a bench cannot see this."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Photos\n:PROPERTIES:\n:ID: DROP\n:END:\n* NEXT Quotes\n:PROPERTIES:\n:ID: Q\n:END:\n"))
          (drop (org-upwell-test--marker-at-id file "DROP"))
          (quotes (org-upwell-test--marker-at-id file "Q")))
     (org-upwell-save (list :name "quote.csv" :url "https://x/q.csv"))
     (org-upwell-claim (org-upwell-find :name "quote.csv") "Q" 'confirmed)
     (org-upwell--bench-draw (org-upwell-domain drop))
     (with-current-buffer "*org-upwell*"
       (should (equal "DROP" (plist-get org-upwell-bench-domain :id)))
       (should-not (plist-get org-upwell-bench-domain :items)))
     (cl-letf (((symbol-function 'org-upwell-open) #'ignore)
               ((symbol-function 'org-upwell--open-url) #'ignore))
       (org-upwell-bench quotes))
     (with-current-buffer "*org-upwell*"
       (should (equal "Q" (plist-get org-upwell-bench-domain :id)))
       (should (equal '("quote.csv")
                      (mapcar (lambda (m) (plist-get m :name))
                              (plist-get org-upwell-bench-domain :items))))))))

(ert-deftest org-upwell-test-heading-identity-stable-inside-heading ()
  "Motion inside a heading is not a follow change."
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT One\n:PROPERTIES:\n:ID: A\n:END:\nbody\nmore\n* NEXT Two\n:PROPERTIES:\n:ID: B\n:END:\n")))
     (with-current-buffer (find-file-noselect file)
       (org-mode)
       (goto-char (point-min))
       (let ((id (org-upwell--heading-identity)))
         (should (equal id "A"))
         (forward-line 2)
         (should (equal id (org-upwell--heading-identity)))
         (re-search-forward "^\\* NEXT Two" nil t)
         (should (equal (org-upwell--heading-identity) "B")))))))

(ert-deftest org-upwell-test-bench-unmark-all ()
  "U clears every mark, with no prompt."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (org-upwell-save (list :name "a.xlsx" :path "/tmp/a.xlsx"))
     (org-upwell-claim (org-upwell-find :name "a.xlsx") "T1" 'confirmed)
     (org-upwell-save (list :name "b.xlsx" :path "/tmp/b.xlsx"))
     (org-upwell-claim (org-upwell-find :name "b.xlsx") "T1" 'confirmed)
     (org-upwell--bench-draw (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (setq org-upwell-bench-marked
             (mapcar (lambda (m) (plist-get m :id))
                     (plist-get org-upwell-bench-domain :items)))
       (should org-upwell-bench-marked)
       (org-upwell-bench-unmark-all)
       (should (null org-upwell-bench-marked))))))

(ert-deftest org-upwell-test-demo-does-not-touch-real-store ()
  "Invented quotes live in the demo directory, not in the real upwell.org."
  (org-upwell-test--with-dir
   (let ((org-upwell-demo-directory (expand-file-name "demo" dir)))
     (org-upwell-save (list :url "https://real.example/x.xlsx"
                            :name "real.xlsx"))
     (org-upwell-demo-regenerate)
     (should (file-exists-p
              (expand-file-name "projects.org" org-upwell-demo-directory)))
     (should (file-exists-p
              (expand-file-name "files/quote-acme.csv"
                                org-upwell-demo-directory)))
     (let ((org-upwell-directory org-upwell-demo-directory))
       (should (org-upwell-find :name "quote-acme.csv"))
       (should (org-upwell-claimed-to org-upwell-demo-id-quotes)))
     (should (org-upwell-find :name "real.xlsx"))
     (should-not (org-upwell-find :name "quote-acme.csv")))))

(ert-deftest org-upwell-test-store-writes-upwell-properties ()
  "The store's flag and path are the UPWELL_* properties, in upwell.org."
  (org-upwell-test--with-dir
   (let ((p (expand-file-name "a.xlsx" dir)))
     (write-region "x" nil p)
     (org-upwell-save (list :path p :name "a.xlsx"))
     (should (equal (file-name-nondirectory (org-upwell-file)) "upwell.org"))
     (org-with-point-at (plist-get (org-upwell-find :path p) :marker)
       (should (org-entry-get (point) "UPWELL"))
       (should (equal (org-entry-get (point) "UPWELL_PATH") p))
       (should (equal (org-entry-get (point) org-upwell-prop-path) p))))))

(ert-deftest org-upwell-test-unclaim-drops-only-that-heading ()
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/x.xlsx"
                                   :name "x.xlsx"))))
     (org-upwell-claim m "H1" 'confirmed)
     (org-upwell-claim (org-upwell-find :name "x.xlsx") "H2" 'confirmed)
     (org-upwell-unclaim (org-upwell-find :name "x.xlsx") "H1")
     (setq m (org-upwell-find :name "x.xlsx"))
     (should-not (org-upwell-claim-status (plist-get m :claims) "H1"))
     (should (eq 'confirmed (org-upwell-claim-status (plist-get m :claims) "H2"))))))

(ert-deftest org-upwell-test-current-heading-marker-from-org ()
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT Photos\n:PROPERTIES:\n:ID: DROP\n:END:\n* NEXT Quotes\n:PROPERTIES:\n:ID: Q\n:END:\n")))
     (with-current-buffer (find-file-noselect file)
       (org-mode)
       (goto-char (org-upwell-test--marker-at-id file "Q"))
       (should (equal "Q"
                      (org-with-point-at (org-upwell--current-heading-marker)
                        (org-id-get))))))))

(ert-deftest org-upwell-test-pin-from-bench-claims-the-shown-heading ()
  "A drop on the bench must claim the heading the bench is showing,
not whatever Org buffer happens to be selected."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Photos\n:PROPERTIES:\n:ID: DROP\n:END:\n* NEXT Quotes\n:PROPERTIES:\n:ID: Q\n:END:\n"))
          (p (expand-file-name "shot.jpg" dir)))
     (write-region "x" nil p)
     (org-upwell--bench-draw (org-upwell-domain
                        (org-upwell-test--marker-at-id file "DROP")))
     (with-current-buffer "*org-upwell*"
       (let ((m (org-upwell-pin p nil "drop")))
         (should (eq 'confirmed
                     (org-upwell-claim-status (plist-get m :claims) "DROP")))
         (should-not (org-upwell-claim-status (plist-get m :claims) "Q")))))))

(ert-deftest org-upwell-test-maybe-refresh-is-silent-when-bench-is-hidden ()
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
         (called nil))
     (cl-letf (((symbol-function 'org-upwell--bench-draw)
                (lambda (&rest _) (setq called t))))
       (org-upwell--maybe-refresh-bench
        (org-upwell-test--heading-marker file)))
     (should-not called))))

(ert-deftest org-upwell-test-follow-draw-skips-the-same-heading ()
  "j/k inside a heading must not rebuild the bench."
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT One\n:PROPERTIES:\n:ID: A\n:END:\nbody\n* NEXT Two\n:PROPERTIES:\n:ID: B\n:END:\n"))
         (n 0))
     (setq org-upwell--follow-seen "A")
     (cl-letf (((symbol-function 'org-upwell--bench-draw)
                (lambda (&rest _) (setq n (1+ n)))))
       (org-upwell--follow-draw (org-upwell-test--marker-at-id file "A"))
       (should (= n 0))
       (org-upwell--follow-draw (org-upwell-test--marker-at-id file "B"))
       (should (= n 1))))))

(ert-deftest org-upwell-test-bench-quit-deletes-the-window ()
  "q must remove the strip, not bury the buffer in that pane."
  (org-upwell-test--with-dir
   (let ((file (org-upwell-test--write-journal
                "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n")))
     (org-upwell--bench-draw
      (org-upwell-domain (org-upwell-test--heading-marker file)))
     (should (get-buffer-window "*org-upwell*" nil))
     (org-upwell-bench-quit)
     (should-not (get-buffer-window "*org-upwell*" nil)))))

(ert-deftest org-upwell-test-follow-quit-does-not-split-again ()
  "q with follow on must not cut another strip out of the frame.

The old `quit-window' left a pane, and follow treated a hidden
bench as a heading change, so each q split the root once more."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT One\n:PROPERTIES:\n:ID: A\n:END:\n* NEXT Two\n:PROPERTIES:\n:ID: B\n:END:\n"))
          (a (org-upwell-test--marker-at-id file "A"))
          (orig (symbol-function 'split-window))
          (n 0))
     (cl-letf (((symbol-function 'split-window)
                (lambda (&optional window size side pixelwise)
                  (setq n (1+ n))
                  (funcall orig window size side pixelwise))))
       (org-upwell-follow-mode -1)
       (setq org-upwell--follow-seen nil)
       (org-upwell--follow-draw a)
       (let ((after-show n))
         (should (get-buffer-window "*org-upwell*" nil))
         (org-upwell-bench-quit)
         ;; The command after q -- j on the same heading -- used to
         ;; treat the hidden bench as a change and split the root.
         (let ((org-upwell-follow-mode t)
               (this-command 'next-line))
           (with-current-buffer (marker-buffer a)
             (org-mode)
             (goto-char a)
             (org-upwell--follow-update)))
         (should-not (get-buffer-window "*org-upwell*" nil))
         (should (= n after-show)))))))

(ert-deftest org-upwell-test-follow-after-quit-still-tracks-a-new-heading ()
  "Dismissing the bench must not disable follow: a new heading still draws."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT One\n:PROPERTIES:\n:ID: A\n:END:\n* NEXT Two\n:PROPERTIES:\n:ID: B\n:END:\n"))
          (a (org-upwell-test--marker-at-id file "A"))
          (b (org-upwell-test--marker-at-id file "B"))
          (orig (symbol-function 'split-window))
          (n 0))
     (cl-letf (((symbol-function 'split-window)
                (lambda (&optional window size side pixelwise)
                  (setq n (1+ n))
                  (funcall orig window size side pixelwise))))
       (setq org-upwell--follow-seen nil)
       (org-upwell--follow-draw a)
       (org-upwell-bench-quit)
       (should-not (get-buffer-window "*org-upwell*" nil))
       (let ((after-quit n)
             (org-upwell-follow-mode t)
             (this-command 'next-line))
         (org-upwell--follow-draw b)
         (should (get-buffer-window "*org-upwell*" nil))
         (should (= n (1+ after-quit))))))))

(ert-deftest org-upwell-test-demo-quotes-heading-shows-every-column ()
  "The demo is what somebody checks the bench against, so it has to have
one of everything the bench can say: both sections, and a form column
with more than one answer in it."
  (org-upwell-test--with-dir
   (let ((org-upwell-demo-directory (expand-file-name "demo" dir)))
     (org-upwell-demo-regenerate)
     (let ((org-upwell-directory org-upwell-demo-directory)
           (file (expand-file-name "projects.org" org-upwell-demo-directory)))
       (should (equal '("Vendor day deck" "Vendor list" "brief.txt"
                        "files" "kickoff.pptx" "quote-acme.csv"
                        "quote-beta.txt" "shared")
                      (sort (mapcar (lambda (m) (plist-get m :name))
                                    (plist-get (org-upwell-domain
                                                (org-upwell-test--marker-at-id
                                                 file org-upwell-demo-id-quotes))
                                               :items))
                            #'string<)))))))

(ert-deftest org-upwell-test-demo-bench-switches-from-drop-to-quotes ()
  "The path the demo teaches: bench opens on File the photos, C-c v on
Compare the three quotes must put the quotes on the bench."
  (org-upwell-test--with-dir
   (let ((org-upwell-demo-directory (expand-file-name "demo" dir)))
     (org-upwell-demo-regenerate)
     (let* ((org-upwell-directory org-upwell-demo-directory)
            (org-upwell-bench-open-max 0)
            (org-upwell-bench-open-location nil)
            (file (expand-file-name "projects.org" org-upwell-demo-directory))
            (drop (org-upwell-test--marker-at-id file org-upwell-demo-id-drop))
            (quotes (org-upwell-test--marker-at-id
                     file org-upwell-demo-id-quotes)))
       (org-upwell--bench-draw (org-upwell-domain drop))
       (with-current-buffer "*org-upwell*"
         (should (equal org-upwell-demo-id-drop
                        (plist-get org-upwell-bench-domain :id))))
       (org-upwell-bench quotes)
       (with-current-buffer "*org-upwell*"
         (should (equal org-upwell-demo-id-quotes
                        (plist-get org-upwell-bench-domain :id)))
         (should (seq-find (lambda (m) (equal (plist-get m :name)
                                              "quote-acme.csv"))
                           (plist-get org-upwell-bench-domain :items))))))))

(ert-deftest org-upwell-test-create-uses-upwell-dir-when-set ()
  (org-upwell-test--with-dir
   (let* ((pool (expand-file-name "pool" dir))
          (file (org-upwell-test--write-journal
                 (concat "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:UPWELL_DIR: "
                         pool "\n:END:\n")))
          (marker (org-upwell-test--heading-marker file)))
     (make-directory pool t)
     (cl-letf (((symbol-function 'org-upwell--current-heading-marker)
                (lambda () marker)))
       (let ((m (org-upwell-create 'txt)))
         (should (string-prefix-p (file-truename pool)
                                  (file-truename (plist-get m :path))))
         (should (eq 'confirmed
                     (org-upwell-claim-status (plist-get m :claims) "T1"))))))))

;;;; Claims a person settled

(ert-deftest org-upwell-test-unclaim-removes-the-last-claim ()
  "Taking the only claim off must actually take it off.

`org-upwell-save' fills in appearances a caller omitted, and claims
were caught by that rule: an empty list read as \"omitted\" and the
claim came straight back, so review's `n' was a silent no-op."
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/a.xlsx"
                                   :name "a.xlsx"))))
     (org-upwell-claim m "H1" 'confirmed)
     (org-upwell-unclaim (org-upwell-find :name "a.xlsx") "H1")
     (setq m (org-upwell-find :name "a.xlsx"))
     (should (null (plist-get m :claims)))
     (should (org-upwell-unclaimed-p m)))))

(ert-deftest org-upwell-test-rejected-is-not-a-claim-on-the-heading ()
  "A rejection is stored, but the file is not on the heading."
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://a.sharepoint.com/:x:/r/b.xlsx"
                                   :name "b.xlsx"))))
     (org-upwell-claim m "H1" 'provisional)
     (org-upwell-reject (org-upwell-find :name "b.xlsx") "H1")
     (setq m (org-upwell-find :name "b.xlsx"))
     (should (eq 'rejected (org-upwell-claim-status (plist-get m :claims) "H1")))
     (should-not (org-upwell-claimed-to "H1"))
     (should (org-upwell-unclaimed-p m))
     ;; Written down, so it survives a round trip through upwell.org.
     (should (equal "H1|rejected"
                    (org-with-point-at (plist-get m :marker)
                      (org-entry-get (point) org-upwell-prop-claims)))))))

(ert-deftest org-upwell-test-rejected-is-not-downgraded ()
  "A later intersection must not put a rejected file back."
  (let ((c (org-upwell-claims-put
            (org-upwell-claims-put nil "abc" 'rejected)
            "abc" 'provisional)))
    (should (eq 'rejected (org-upwell-claim-status c "abc"))))
  ;; A person may still change their mind.
  (let ((c (org-upwell-claims-put
            (org-upwell-claims-put nil "abc" 'rejected)
            "abc" 'confirmed)))
    (should (eq 'confirmed (org-upwell-claim-status c "abc")))))

(ert-deftest org-upwell-test-rejected-survives-a-sync ()
  "The timer runs the intersection again every minute.  Saying no once
has to be enough."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "work.xlsx" dir))
          (now (current-time))
          (from (time-subtract now 3600)))
     (write-region "x" nil p)
     (org-upwell-test--write-journal
      (concat "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n:LOGBOOK:\nCLOCK: "
              (format-time-string (org-time-stamp-format t t) from) "--"
              (format-time-string (org-time-stamp-format t t) now)
              "\n:END:\n"))
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1800))) p)
     (org-upwell-sync 1)
     (should (eq 'provisional
                 (org-upwell-claim-status
                  (plist-get (org-upwell-find :path p) :claims) "T1")))
     (org-upwell-reject (org-upwell-find :path p) "T1")
     (let ((writes 0)
           (orig (symbol-function 'org-upwell-save)))
       (cl-letf (((symbol-function 'org-upwell-save)
                  (lambda (item)
                    (when (equal (plist-get item :path) p)
                      (setq writes (1+ writes)))
                    (funcall orig item))))
         (org-upwell-sync 1))
       ;; The timer runs a sync a minute.  An answer already given is
       ;; read, not written back three times over.
       (should (= writes 1)))
     (should (eq 'rejected
                 (org-upwell-claim-status
                  (plist-get (org-upwell-find :path p) :claims) "T1")))
     (should-not (org-upwell-claimed-to "T1")))))

;;;; Clocks that were never closed

(ert-deftest org-upwell-test-open-clock-is-ignored-unless-running ()
  "A clock somebody forgot to close is not work still going on.

Read as running, a CLOCK line opened in June hands every file of today
to a heading nobody has touched since."
  (org-upwell-test--with-dir
   (org-upwell-test--write-journal
    (concat "* NEXT Stale\n:PROPERTIES:\n:ID: S1\n:END:\n:LOGBOOK:\nCLOCK: "
            (format-time-string (org-time-stamp-format t t)
                                (time-subtract (current-time) (days-to-time 90)))
            "\n:END:\n"))
   (should-not (org-upwell-clock-segments 1))))

(ert-deftest org-upwell-test-open-clock-counts-while-it-is-running ()
  "The one open CLOCK that is the running clock is still closed at now."
  (org-upwell-test--with-dir
   (let* ((start (time-subtract (current-time) 1800))
          (journal (org-upwell-test--write-journal
                    (concat "* NEXT Live\n:PROPERTIES:\n:ID: L1\n:END:\n"
                            ":LOGBOOK:\nCLOCK: "
                            (format-time-string (org-time-stamp-format t t) start)
                            "\n:END:\n")))
          (org-clock-hd-marker (copy-marker
                                (org-upwell-test--heading-marker journal)))
          (org-clock-start-time start))
     (cl-letf (((symbol-function 'org-clocking-p) (lambda () t)))
       (let ((segs (org-upwell-clock-segments 1)))
         (should (= 1 (length segs)))
         (should (equal "NEXT Live" (plist-get (car segs) :title))))))))

(ert-deftest org-upwell-test-clock-segments-are-clamped-to-the-window ()
  "A spell that started before the window starts at the window's edge."
  (org-upwell-test--with-dir
   (let* ((midnight (org-upwell--day-start 0))
          (before (time-subtract midnight 7200))
          (after (time-add midnight 3600)))
     (org-upwell-test--write-journal
      (concat "* NEXT Late\n:PROPERTIES:\n:ID: N1\n:END:\n:LOGBOOK:\nCLOCK: "
              (format-time-string (org-time-stamp-format t t) before) "--"
              (format-time-string (org-time-stamp-format t t) after)
              "\n:END:\n"))
     (let ((segs (org-upwell-clock-segments 1)))
       (should (= 1 (length segs)))
       (should-not (time-less-p (plist-get (car segs) :from) midnight))))))

;;;; Trace files

(ert-deftest org-upwell-test-read-traces-covers-every-day ()
  "The middle of a window is read, not only its ends."
  (org-upwell-test--with-dir
   (let* ((from (encode-time 0 0 9 3 9 2026))
          (to (encode-time 0 0 18 6 9 2026))
          (files (mapcar #'file-name-nondirectory
                         (org-upwell--trace-files-between from to))))
     (should (equal files
                    '("trace-2026-09-02.jsonl"
                      "trace-2026-09-03.jsonl"
                      "trace-2026-09-04.jsonl"
                      "trace-2026-09-05.jsonl"
                      "trace-2026-09-06.jsonl"))))))

;;;; Expand and the windows it was called from

(ert-deftest org-upwell-test-bench-does-not-take-its-own-window ()
  "C-c v on the bench must not replace the listing it just redrew."
  (org-upwell-test--with-dir
   (let* ((org-upwell-bench-open-max 0)
          (org-upwell-bench-open-location nil)
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (org-upwell--bench-draw (org-upwell-domain mk))
     (let ((bench (get-buffer-window "*org-upwell*" nil)))
       (should (window-live-p bench))
       (select-window bench)
       (org-upwell-bench)
       (should (window-live-p bench))
       (should (eq (window-buffer bench) (get-buffer "*org-upwell*")))))))

;;;; Opening

(ert-deftest org-upwell-test-bench-does-not-take-the-agenda-window ()
  "V in the agenda expands the row.  It does not close the agenda."
  (org-upwell-test--with-dir
   (let* ((org-upwell-bench-open-max 0)
          (org-upwell-bench-open-location nil)
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (agenda (get-buffer-create "*Org Agenda*")))
     (with-current-buffer agenda (org-agenda-mode))
     (delete-other-windows)
     (set-window-buffer (selected-window) agenda)
     (org-upwell-bench mk)
     (should (eq (window-buffer (selected-window)) agenda))
     (kill-buffer agenda))))

(ert-deftest org-upwell-test-open-uses-org-upwell-open-function ()
  "The package's own knob, not a function name borrowed from a dotfile."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "sheet.xlsx" dir))
          seen
          (org-upwell-open-function (lambda (path) (push path seen))))
     (write-region "x" nil p)
     (org-upwell-open (org-upwell-save (list :path p :name "sheet.xlsx")))
     (should (equal seen (list p))))))

;;;; Small mercies

(ert-deftest org-upwell-test-search-roots-skips-what-is-not-there ()
  "Roots are a setting, and a setting names machines this is not.

A directory that does not exist on this one is dropped rather than
walked, and a nil in the list must not reach `file-directory-p'."
  (org-upwell-test--with-dir
   (let ((org-upwell-search-roots (list dir "~/does-not-exist-org-upwell/" nil)))
     (should (equal (mapcar #'file-truename (org-upwell--search-roots))
                    (list (file-truename dir)))))))

(ert-deftest org-upwell-test-enable-dnd-is-idempotent ()
  "The bench redraws on every heading it follows."
  (with-temp-buffer
    (org-upwell-enable-dnd)
    (let ((once (length dnd-protocol-alist)))
      (org-upwell-enable-dnd)
      (org-upwell-enable-dnd)
      (should (= once (length dnd-protocol-alist)))
      (should (equal (seq-take dnd-protocol-alist 2)
                     org-upwell-dnd-handlers)))))

(ert-deftest org-upwell-test-pin-refuses-a-path-that-is-not-there ()
  "An item with no appearance can never be resolved, opened or cleared."
  (org-upwell-test--with-dir
   (cl-letf (((symbol-function 'org-upwell--current-heading-marker)
              (lambda () nil)))
     (should-error (org-upwell-pin (expand-file-name "gone.xlsx" dir))
                   :type 'user-error)
     (should-not (org-upwell-find :name "gone.xlsx")))))

(ert-deftest org-upwell-test-demo-off-without-on-keeps-the-agenda ()
  "Switching off a demo that never ran must not empty `org-agenda-files'."
  (org-upwell-test--with-dir
   (let* ((real (expand-file-name "real.org" dir))
          (org-agenda-files (list real))
          (org-upwell-demo--saved nil))
     (org-upwell-demo-mode -1)
     (should (equal org-agenda-files (list real))))))


;;;; What the background pass costs

(defun org-upwell-test--trace-day (dir n)
  "Write N traces for today under DIR and return the paths they name."
  (let ((now (floor (float-time))) paths)
    (dotimes (i n)
      (let ((path (expand-file-name (format "doc-%d.xlsx" i) dir)))
        (write-region "x" nil path)
        (org-upwell-test--write-trace (- now 3600 (- i)) path)
        (push path paths)))
    (nreverse paths)))

(ert-deftest org-upwell-test-one-pass-walks-the-store-once ()
  "The pass runs on a timer while somebody is working, so what it costs is
felt.  `org-upwell-save' asks whether an item is stored already, that asks up
to five times, and each ask used to walk the whole file -- so one trace walked
the store several times over and a day of them walked it hundreds of times."
  (org-upwell-test--with-dir
   (org-upwell-test--trace-day dir 20)
   (setq org-upwell--store-walks 0)
   (org-upwell-sync 1)
   (should (= 1 org-upwell--store-walks))))

(ert-deftest org-upwell-test-one-pass-writes-the-store-once ()
  "Saving after every item wrote the file once per trace."
  (org-upwell-test--with-dir
   (org-upwell-test--trace-day dir 20)
   (let ((writes 0)
         (orig (symbol-function 'save-buffer)))
     (cl-letf (((symbol-function 'save-buffer)
                (lambda (&rest a) (setq writes (1+ writes)) (apply orig a))))
       (org-upwell-sync 1))
     (should (<= writes 1)))))

(ert-deftest org-upwell-test-a-pass-that-changes-nothing-writes-nothing ()
  "The same traces come round again every time the timer fires.  Rewriting a
heading with the values it already holds costs a property edit apiece and
marks the file dirty, so the store went to disk once a minute for nothing."
  (org-upwell-test--with-dir
   (org-upwell-test--trace-day dir 20)
   (org-upwell-sync 1)
   (let ((writes 0)
         (orig (symbol-function 'save-buffer)))
     (cl-letf (((symbol-function 'save-buffer)
                (lambda (&rest a) (setq writes (1+ writes)) (apply orig a))))
       (org-upwell-sync 1))
     (should (= 0 writes)))))

(ert-deftest org-upwell-test-holding-the-store-changes-nothing-it-holds ()
  "Reading once instead of a hundred times has to give the same answer."
  (org-upwell-test--with-dir
   (let ((paths (org-upwell-test--trace-day dir 12)))
     (org-upwell-sync 1)
     (let ((held (sort (mapcar (lambda (m) (plist-get m :name))
                               (org-upwell-items))
                       #'string<)))
       ;; every trace is in the store, and once each
       (should (= (length paths) (length held)))
       (should (equal held (sort (mapcar #'file-name-nondirectory paths)
                                 #'string<)))
       ;; and a second pass with the store held open finds them all again
       (should (org-upwell-with-store
                 (seq-every-p (lambda (p) (org-upwell-find :path p)) paths)))))))

(ert-deftest org-upwell-test-the-pass-waits-for-a-quiet-moment ()
  "A repeating timer fires in the middle of a keystroke.  This pass reads the
whole store, so landing there is felt as the typing catching."
  (org-upwell-test--with-dir
   (let ((org-upwell-sync-interval 60)
         (org-upwell--sync-timer nil))
     (unwind-protect
         (progn
           (org-upwell-mode 1)
           (should (timerp org-upwell--sync-timer))
           (should (timer--idle-delay org-upwell--sync-timer)))
       (org-upwell-mode -1)))))

;;;; Directories

(ert-deftest org-upwell-test-basename-of-a-directory ()
  "A directory arrives with a separator on the end, and
`file-name-nondirectory' answers nothing at all for such a path."
  (should (equal (org-upwell-basename "/a/b/project/") "project"))
  (should (equal (org-upwell-basename "/a/b/c.txt") "c.txt"))
  (should (null (org-upwell-basename "/")))
  (should (null (org-upwell-basename nil))))

(ert-deftest org-upwell-test-a-directory-trace-is-named-after-the-directory ()
  "The watcher reports the window a person had open when nothing in it
was selected.  A nameless item is a blank line on the bench."
  (let ((tr (org-upwell--trace-from-alist
             (list (cons "ts" 1757000000)
                   (cons "app" "Finder")
                   (cons "title" "project")
                   (cons "path" "/Users/me/Documents/project/")
                   (cons "kind" "dir")))))
    (should (equal (plist-get tr :name) "project"))
    (should (equal (plist-get (org-upwell-trace-to-spec tr) :name) "project"))))

(ert-deftest org-upwell-test-pinning-a-directory-names-it ()
  (org-upwell-test--with-dir
   (let ((directory (file-name-as-directory (expand-file-name "papers" dir))))
     (make-directory directory t)
     (should (equal (plist-get (org-upwell--spec-from-path directory) :name)
                    "papers")))))

(ert-deftest org-upwell-test-open-directory-reveals-the-file-that-is-there ()
  "Not the directory the path was stored with: a file filed away into a `done'
directory is resolved first, and the directory to stand in is the new one."
  (org-upwell-test--with-dir
   (let* ((a (expand-file-name "sheet.xlsx" dir))
          (sub (expand-file-name "done" dir))
          (b (expand-file-name "sheet.xlsx" sub))
          revealed)
     (write-region "x" nil a)
     (let ((m (org-upwell-save (list :path a :name "sheet.xlsx"))))
       (make-directory sub t)
       (rename-file a b)
       (let ((org-upwell-search-roots (list dir)))
         (cl-letf (((symbol-function 'org-upwell--reveal-external)
                    (lambda (path) (setq revealed path))))
           (org-upwell-open-directory (org-upwell-find :id (plist-get m :id))))))
     (should revealed)
     (should (equal (file-truename revealed) (file-truename b))))))

(ert-deftest org-upwell-test-open-directory-says-a-url-has-none ()
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://example.com/a"
                                   :name "a page"))))
     (should-error (org-upwell-open-directory m) :type 'user-error))))

(ert-deftest org-upwell-test-a-directory-trace-is-known-for-one ()
  "`kind' cannot answer alone: the Windows watcher calls the directory it
read a file, so the disk is asked as well."
  (org-upwell-test--with-dir
   (let ((directory (expand-file-name "papers" dir))
         (file (expand-file-name "papers/a.txt" dir)))
     (make-directory directory t)
     (write-region "x" nil file)
     (should (org-upwell-trace-directory-p (list :path directory :kind 'file)))
     (should (org-upwell-trace-directory-p (list :path "/gone/away/" :kind 'dir)))
     (should-not (org-upwell-trace-directory-p (list :path file :kind 'file))))))

(ert-deftest org-upwell-test-a-directory-walked-through-is-kept ()
  "It was dropped once, on the reasoning that the bench lists things to open
and the directory a file came from is already on the file's line.  That
reasoning holds only for somebody whose file manager is watched from
outside.  Where the work is kept is a thing to go to, and for anybody
working in dired it is the thing nothing else records."
  (org-upwell-test--with-dir
   (let* ((directory (expand-file-name "procurement" dir))
          (sheet (expand-file-name "procurement/quote.csv" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time)))
     (make-directory directory t)
     (write-region "x" nil sheet)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1800))) directory)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1500))) sheet)
     (org-upwell-claim-interval mk (time-subtract now 3600) now)
     (should (org-upwell-find :path sheet))
     (should (org-upwell-find :path directory))
     (should (org-upwell-directory-item-p (org-upwell-find :path directory)))
     (should (equal (list "procurement" "quote.csv")
                    (sort (mapcar (lambda (m) (plist-get m :name))
                                  (org-upwell-claimed-to "T1"))
                          #'string<))))))

(ert-deftest org-upwell-test-a-spell-of-only-directories-writes-an-id ()
  "`org-upwell-heading-id\=' creates an ID in the user's own file, so it is
asked for only when there is something to claim.  A spell that saw only
directories used to be nothing; now the directory is the claim."
  (org-upwell-test--with-dir
   (let* ((directory (expand-file-name "procurement" dir))
          (file (org-upwell-test--write-journal "* NEXT Task\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time)))
     (make-directory directory t)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1800))) directory)
     (org-upwell-claim-interval mk (time-subtract now 3600) now)
     (should (org-with-point-at mk (org-id-get)))
     (should (org-upwell-find :path directory)))))

(ert-deftest org-upwell-test-a-spell-that-saw-nothing-writes-no-id ()
  "The guard that is still load-bearing: an ID in somebody's own file is
the only write this package makes there, and a spell with nothing in it
has no business leaving a mark."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal "* NEXT Task\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time)))
     (org-upwell-claim-interval mk (time-subtract now 3600) now)
     (should-not (org-with-point-at mk (org-id-get))))))

(ert-deftest org-upwell-test-sync-keeps-a-directory-seen-off-the-clock ()
  "Off the clock a trace becomes an unclaimed item, a row to deal with
later.  A directory is such a row like any other now."
  (org-upwell-test--with-dir
   (let ((directory (expand-file-name "procurement" dir)))
     (make-directory directory t)
     (org-upwell-test--write-trace (floor (float-time (current-time))) directory)
     (org-upwell-sync 1)
     (should (org-upwell-directory-item-p
              (org-upwell-find :path directory))))))

;;;; What the bench line says

(ert-deftest org-upwell-test-where-is-the-directory-not-the-name ()
  "The name is already the first thing on the line."
  (should (equal (org-upwell--item-where (list :path "/tmp/a/b/c.xlsx"))
                 "/tmp/a/b"))
  (should (equal (org-upwell--item-where
                  (list :url "https://contoso.sharepoint.com/sites/a/f.xlsx"))
                 "contoso.sharepoint.com"))
  (should (equal (org-upwell--item-where
                  (list :office "ms-excel:ofe|u|https://share.example/a/f.xlsx"))
                 "share.example")))

(ert-deftest org-upwell-test-a-directory-too-long-keeps-its-end ()
  "Cutting the end off leaves every deep path looking like every other.

Two copies of one file are in the same tree up to the last directory or
two, so the end is the whole of the answer."
  (let ((cut (org-upwell--tail "/home/me/Documents/project/2026/q3/meeting" 20)))
    (should (= 20 (string-width cut)))
    (should (string-suffix-p "q3/meeting" cut))
    (should-not (string-prefix-p "/home" cut))))

(ert-deftest org-upwell-test-ago-counts-in-the-right-unit ()
  (cl-flet ((ago (secs)
              (org-upwell--ago
               (list :opened (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                 (time-subtract nil secs))))))
    (should (equal (ago (* 3 3600)) "3h"))
    (should (equal (ago (* 3 86400)) "3d"))
    (should (equal (ago (* 70 86400)) "2mo")))
  (should (equal (org-upwell--ago (list :name "never opened")) "")))

(ert-deftest org-upwell-test-bench-tells-two-of-the-same-name-apart ()
  "Two files called the same thing, kept in two directories.

A listing that shows only the name asks which one to open and gives
nothing to answer with."
  (org-upwell-test--with-dir
   (let* ((one (expand-file-name "a/quote.csv" dir))
          (two (expand-file-name "b/quote.csv" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (make-directory (file-name-directory one) t)
     (make-directory (file-name-directory two) t)
     (write-region "x" nil one)
     (write-region "x" nil two)
     (org-upwell-claim
      (org-upwell-save (list :path one :name "quote.csv" :provenance "pin"))
      "T1" 'confirmed)
     (org-upwell-claim
      (org-upwell-save (list :path two :name "quote.csv" :provenance "drop"))
      "T1" 'confirmed)
     (org-upwell--bench-draw (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (let ((text (buffer-string)))
         (should (= 2 (cl-count-if (lambda (l) (string-match-p "quote\\.csv" l))
                                   (split-string text "\n"))))
         (should (string-match-p "/a +pin" text))
         (should (string-match-p "/b +drop" text)))))))

(ert-deftest org-upwell-test-the-directory-column-is-only-as-wide-as-it-needs ()
  "On a wide frame, a column padded to the width left over puts a hand's
width of blank between two short columns."
  (let ((short (list (list :path "/a/b/x.txt")))
        (long (list (list :path (concat "/" (make-string 90 ?d) "/x.txt")))))
    (should (= 12 (cdr (org-upwell--bench-widths short (current-buffer)))))
    (should (< 12 (cdr (org-upwell--bench-widths long (current-buffer)))))))

(ert-deftest org-upwell-test-a-title-loses-the-window-and-keeps-the-page ()
  "A browser writes its own name on every window it owns, and on Windows the
watcher can read nothing finer than the window.  The same handful of
characters then arrives on every URL alike."
  (should (equal (org-upwell--strip-title-noise
                  "納品スケジュールの調整 - Profile 1 - Microsoft Edge")
                 "納品スケジュールの調整"))
  (should (equal (org-upwell--strip-title-noise "Some page - Google Chrome")
                 "Some page"))
  (should (equal (org-upwell--strip-title-noise "Some page — Mozilla Firefox")
                 "Some page"))
  ;; and a page that is about one keeps its subject
  (should (equal (org-upwell--strip-title-noise "菌根 - Wikipedia")
                 "菌根 - Wikipedia"))
  (should (equal (org-upwell--strip-title-noise
                  "Microsoft Edge のリリースノート - Microsoft Edge")
                 "Microsoft Edge のリリースノート")))

(ert-deftest org-upwell-test-a-named-profile-is-left-alone ()
  "Nothing tells a profile somebody named apart from the last part of a
title, so a rule that took two segments would quietly eat the subject of
every page whose title ends in a phrase."
  (should (equal (org-upwell--strip-title-noise "会議資料 - 共有 - Microsoft Edge")
                 "会議資料 - 共有")))

(ert-deftest org-upwell-test-a-name-keeps-its-beginning ()
  "A name says what the thing is in its first word.  Cut from the same end as
a directory, every row that came out of one browser read the same three words."
  (org-upwell-test--with-dir
    (let* ((file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (dolist (pair '(("納品スケジュールの調整について先方と詰める"
                       "https://example.com/a")
                      ("見積書の差し替えについて先方と詰める"
                       "https://example.com/b")))
        (org-upwell-claim
         (org-upwell-save (list :url (cadr pair) :name (car pair)
                                :provenance "trace"))
         "T1" 'confirmed))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (let ((text (buffer-string)))
          (should (string-match-p "納品スケジュール" text))
          (should (string-match-p "見積書の差し替え" text)))))))

(ert-deftest org-upwell-test-a-host-is-cut-from-the-other-end ()
  "A directory is told apart by its last name and a host by its first, so the
two are cut from opposite ends.  Cut alike, every site behind one tenant
reads as the same dozen characters."
  (org-upwell-test--with-dir
    (let ((one (list :url "https://contoso.sharepoint.com/sites/a/x.aspx"
                     :name "x"))
          (two (list :url "https://fabrikam.sharepoint.com/sites/a/x.aspx"
                     :name "x"))
          (dir (list :path "/var/tmp/deep/tree/procurement/quote.csv"
                     :name "quote.csv")))
      (should-not (equal (org-upwell--fit-where one 14)
                         (org-upwell--fit-where two 14)))
      (should (string-prefix-p "contoso" (org-upwell--fit-where one 14)))
      ;; and the directory still keeps its end
      (should (string-suffix-p "procurement" (org-upwell--fit-where dir 14))))))

(ert-deftest org-upwell-test-the-bench-keys-are-announced ()
  "Eldoc keeps an obarray of the commands it will speak after and says
nothing after anything else.  Motion was in it and the bench\='s own keys were
not, so the line was announced when `n' moved to it and silent when `m' did
-- and `m' marks and moves down, so the line it lands on is exactly the one
somebody is about to act on."
  (dolist (command '("org-upwell-bench-toggle-mark"
                     "org-upwell-bench-unmark"
                     "org-upwell-bench-drop"
                     "org-upwell-bench"))
    (should (intern-soft command eldoc-message-commands))))

(ert-deftest org-upwell-test-marking-says-where-it-landed ()
  "And what it says is the line it moved to, not the one it marked."
  (org-upwell-test--with-dir
    (let* ((file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (dolist (name '("alpha.txt" "beta.txt"))
        (let ((p (expand-file-name name dir)))
          (write-region "x" nil p)
          (org-upwell-claim
           (org-upwell-save (list :path p :name name :provenance "pin"))
           "T1" 'confirmed)))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (goto-char (point-min))
        (should (search-forward "alpha.txt" nil t))
        (beginning-of-line)
        (org-upwell-bench-toggle-mark)
        ;; marked alpha, moved to beta, and says beta
        (should (string-match-p "beta\\.txt" (org-upwell-bench-eldoc-function)))))))

(ert-deftest org-upwell-test-the-echo-area-says-the-whole-of-it ()
  "The two things a column had to shorten, where a glance already goes."
  (org-upwell-test--with-dir
    (let* ((file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (name "納品スケジュールの調整について先方と詰める")
           (url "https://contoso.sharepoint.com/sites/sales/schedule.aspx"))
      (org-upwell-claim
       (org-upwell-save (list :url url :name name :provenance "trace"))
       "T1" 'confirmed)
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (goto-char (point-min))
        (should (search-forward "納品" nil t))
        (beginning-of-line)
        (let ((said (org-upwell-bench-eldoc-function)))
          (should said)
          (should (string-match-p (regexp-quote name) said))
          (should (string-match-p (regexp-quote url) said)))))))

;;;; Clock-out

(ert-deftest org-upwell-test-clock-out-shows-the-bench-and-asks-nothing ()
  "One spell touches several files, and a question naming none of them
can only be answered by saying yes."
  (org-upwell-test--with-dir
   (let* ((org-upwell-review-on-clock-out 'bench)
          (p (expand-file-name "doc.xlsx" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time))
          (from (time-subtract now 3600)))
     (write-region "x" nil p)
     (org-upwell-test--write-trace (floor (float-time (time-subtract now 1800))) p)
     (cl-letf (((symbol-function 'read-char-choice)
                (lambda (&rest _) (error "org-upwell asked at clock-out"))))
       (should (org-upwell-review-interval mk from now)))
     (should (get-buffer-window "*org-upwell*" nil))
     (should (eq 'provisional
                 (org-upwell-claim-status
                  (plist-get (org-upwell-find :path p) :claims) "T1"))))))

(ert-deftest org-upwell-test-the-question-names-the-files ()
  "`ask' is not the default any more, but it is still asked of a person."
  (org-upwell-test--with-dir
   (let* ((org-upwell-review-on-clock-out 'ask)
          (p (expand-file-name "doc.xlsx" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time))
          (from (time-subtract now 3600))
          prompt)
     (write-region "x" nil p)
     (org-upwell-test--write-trace (floor (float-time (time-subtract now 1800))) p)
     (cl-letf (((symbol-function 'read-char-choice)
                (lambda (p &rest _) (setq prompt p) ?\r)))
       (org-upwell-review-interval mk from now))
     (should prompt)
     (should (string-match-p "doc\\.xlsx" prompt)))))

;;;; Which heading the agenda row is

(ert-deftest org-upwell-test-agenda-row-with-only-org-marker ()
  "A row from a custom block carries `org-marker' and no `org-hd-marker'.

Reading one of the two made V in such an agenda fall through to a
completing-read of every heading, which looks like a different command."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (agenda (get-buffer-create "*Org Agenda*")))
     (unwind-protect
         (with-current-buffer agenda
           (org-agenda-mode)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize "  Task\n" 'org-marker mk)))
           (goto-char (point-min))
           (should (equal mk (org-upwell--current-heading-marker))))
       (kill-buffer agenda)))))

(ert-deftest org-upwell-test-bench-with-a-prefix-asks-anyway ()
  "C-u C-c v is the way to the list when point is on a heading."
  (org-upwell-test--with-dir
   (let* ((org-upwell-bench-open-max 0)
          (org-upwell-bench-open-location nil)
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (asked 0))
     (cl-letf (((symbol-function 'org-upwell--read-heading-marker)
                (lambda () (setq asked (1+ asked)) mk)))
       (with-current-buffer (marker-buffer mk)
         (goto-char mk)
         (let ((current-prefix-arg '(4)))
           (call-interactively #'org-upwell-bench))
         (should (= 1 asked))
         (let ((current-prefix-arg nil))
           (call-interactively #'org-upwell-bench))
         (should (= 1 asked)))))))

;;;; Keeping and dropping from the bench

(ert-deftest org-upwell-test-bench-drop-has-the-no-stay-said ()
  "The intersection runs again on a timer.  A claim merely forgotten
comes back on the next pass, so dropping has to be written down."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "doc.xlsx" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (write-region "x" nil p)
     (org-upwell-claim
      (org-upwell-save (list :path p :name "doc.xlsx" :provenance "trace"))
      "T1" 'provisional)
     (org-upwell--bench-draw (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (goto-char (point-min))
       (should (search-forward "doc.xlsx" nil t))
       (beginning-of-line)
       (org-upwell-bench-drop)
       (should-not (plist-get org-upwell-bench-domain :items)))
     (should (eq 'rejected
                 (org-upwell-claim-status
                  (plist-get (org-upwell-find :path p) :claims) "T1"))))))

(ert-deftest org-upwell-test-bench-keep-confirms ()
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "doc.xlsx" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (write-region "x" nil p)
     (org-upwell-claim
      (org-upwell-save (list :path p :name "doc.xlsx" :provenance "trace"))
      "T1" 'provisional)
     (org-upwell--bench-draw (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (goto-char (point-min))
       (should (search-forward "doc.xlsx" nil t))
       (beginning-of-line)
       (org-upwell-bench-keep))
     (should (eq 'confirmed
                 (org-upwell-claim-status
                  (plist-get (org-upwell-find :path p) :claims) "T1"))))))

(ert-deftest org-upwell-test-bench-forget-deletes-the-item ()
  "Dropping keeps the item and says it is not this heading's.  Forgetting
leaves nothing to propose it again anywhere."
  (org-upwell-test--with-dir
   (let* ((p (expand-file-name "doc.xlsx" dir))
          (other (expand-file-name "keep.xlsx" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (write-region "x" nil p)
     (write-region "x" nil other)
     (org-upwell-claim
      (org-upwell-save (list :path p :name "doc.xlsx" :provenance "trace"))
      "T1" 'confirmed)
     (org-upwell-claim
      (org-upwell-save (list :path other :name "keep.xlsx" :provenance "pin"))
      "T1" 'confirmed)
     (org-upwell--bench-draw (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (goto-char (point-min))
       (should (search-forward "doc.xlsx" nil t))
       (beginning-of-line)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (org-upwell-bench-forget)))
     (should-not (org-upwell-find :path p))
     (should (org-upwell-find :path other)))))

;;;; The window the file lands in

(ert-deftest org-upwell-test-nothing-above-a-bench-that-is-not-showing ()
  "With no strip on the frame, this action function has no opinion."
  (org-upwell-test--with-dir
   (should-not (org-upwell-display-above-bench (current-buffer) nil))))

(ert-deftest org-upwell-test-the-file-reuses-the-window-above-the-bench ()
  "The pane that was reading the entry keeps reading it; no new window."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file)))
     (delete-other-windows)
     (set-window-buffer (selected-window) (marker-buffer mk))
     (org-upwell--bench-draw (org-upwell-domain mk))
     (let* ((bench (get-buffer-window "*org-upwell*" nil))
            (above (window-in-direction 'above bench))
            (n (length (window-list nil 'nomini)))
            (win (org-upwell-display-above-bench (marker-buffer mk) nil)))
       (should (eq win above))
       (should (= n (length (window-list nil 'nomini))))))))

(ert-deftest org-upwell-test-the-agenda-is-not-where-the-file-goes ()
  "C-i from the agenda must not put the file in the agenda's window, and
must not put it in the strip either.  It is cut in between."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (agenda (get-buffer-create "*Org Agenda*")))
     (unwind-protect
         (progn
           (with-current-buffer agenda (org-agenda-mode))
           (delete-other-windows)
           (set-window-buffer (selected-window) agenda)
           (org-upwell--bench-draw (org-upwell-domain mk))
           (let* ((bench (get-buffer-window "*org-upwell*" nil))
                  (win (org-upwell-display-above-bench (marker-buffer mk) nil)))
             (should (window-live-p win))
             (should-not (eq win bench))
             (should (eq bench (window-in-direction 'below win)))
             (should (eq (window-buffer win) (marker-buffer mk)))
             (should (get-buffer-window agenda nil))))
       (kill-buffer agenda)))))

(ert-deftest org-upwell-test-bench-follows-the-agenda-on-its-own ()
  "Org's own follow opens the entry's file in another window, which is the
movement this setting exists to do without."
  (org-upwell-test--with-dir
   (let* ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (org-upwell-agenda-follow t)
          (org-agenda-follow-mode nil)
          (agenda (get-buffer-create "*Org Agenda*"))
          drawn)
     (unwind-protect
         (with-current-buffer agenda
           (org-agenda-mode)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (propertize "  Task\n" 'org-hd-marker mk)))
           (goto-char (point-min))
           (cl-letf (((symbol-function 'org-upwell--follow-draw)
                      (lambda (m) (setq drawn m))))
             (org-upwell--agenda-follow))
           (should (equal drawn mk)))
       (kill-buffer agenda)))))

(ert-deftest org-upwell-test-open-all-asks-above-the-cap ()
  "The bench is the one place that opens in bulk, so it is the one place
that has to say how many first.  `org-upwell-bench-open-max\=' is the number
above which it asks; the number is the whole warning, because what is about
to happen is that many applications starting at once."
  (org-upwell-test--with-dir
    (let* ((file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (opened 0)
           (asked nil))
      (dolist (name '("a.txt" "b.txt" "c.txt" "d.txt"))
        (let ((p (expand-file-name name dir)))
          (write-region "x" nil p)
          (org-upwell-claim
           (org-upwell-save (list :path p :name name :provenance "pin"))
           "T1" 'confirmed)))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (cl-letf (((symbol-function 'org-upwell-open)
                   (lambda (&rest _) (setq opened (1+ opened))))
                  ((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) nil)))
          ;; under the cap: no question, and it opens
          (let ((org-upwell-bench-open-max 8))
            (org-upwell-bench-open-all))
          (should-not asked)
          (should (= 4 opened))
          ;; over it: it asks, and "no" opens nothing
          (setq opened 0)
          (let ((org-upwell-bench-open-max 3))
            (should-error (org-upwell-bench-open-all) :type 'user-error))
          (should (string-match-p "4" (or asked "")))
          (should (= 0 opened)))))))

(ert-deftest org-upwell-test-r-reads-the-store-and-R-reassigns ()
  "`r\=' is the key a bench left open while the clock runs needs: the watcher
keeps sighting things and the sync keeps attributing them, so the list goes
quietly out of date.  Reassign, which used to hold `r\=', moves up a case."
  (should (eq (lookup-key org-upwell-bench-mode-map (kbd "r"))
              #'org-upwell-bench-redraw))
  (should (eq (lookup-key org-upwell-bench-mode-map (kbd "R"))
              #'org-upwell-bench-reassign)))

(ert-deftest org-upwell-test-redraw-picks-up-what-the-clock-attributed ()
  "And it really re-reads: a claim written behind the bench's back -- which
is what `org-upwell-sync\=' does every minute while the clock runs -- is on
the list after `r\=' and was not before."
  (org-upwell-test--with-dir
    (let* ((file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (p (expand-file-name "late.txt" dir)))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (goto-char (point-min))
        (should-not (search-forward "late.txt" nil t))
        (write-region "x" nil p)
        (org-upwell-claim
         (org-upwell-save (list :path p :name "late.txt" :provenance "clock"))
         "T1" 'provisional)
        (org-upwell-bench-redraw)
        (goto-char (point-min))
        (should (search-forward "late.txt" nil t))))))

(ert-deftest org-upwell-test-the-legend-names-only-live-commands ()
  "A foot that names a command nobody can run is worse than no foot."
  (should org-upwell-bench-commands)
  (pcase-dolist (`(,command ,scope ,what) org-upwell-bench-commands)
    (should (commandp command))
    (should (memq scope '(row page)))
    (should (stringp what))
    (should (<= (string-width what) 15))))

(ert-deftest org-upwell-test-the-legend-reads-the-keymap ()
  "The keys are read from the keymap rather than written down, because a
configuration is expected to move them -- this package's own dotfiles put
`g\=' back on motion -- and a printed key would then be a printed lie."
  (let ((moved (with-temp-buffer
                 (use-local-map
                  (let ((map (make-sparse-keymap)))
                    (define-key map (kbd "Z") #'org-upwell-bench-open-all)
                    map))
                 (substring-no-properties (org-upwell--bench-legend 80)))))
    (should (string-match-p "Z +open every one" moved)))
  (let ((usual (with-temp-buffer
                 (use-local-map org-upwell-bench-mode-map)
                 (substring-no-properties (org-upwell--bench-legend 80)))))
    (should (string-match-p "a +open every one" usual))
    (should (string-match-p "R +move elsewhere" usual))))

(ert-deftest org-upwell-test-the-legend-fits-a-narrow-bench ()
  "The bench is half a window wide and truncates rather than wraps, so a
line too long is not a line that wraps -- it is a line whose end nobody
ever sees.  It falls back to one column, and to shorter wording, rather
than off the edge."
  (dolist (width '(80 60 46 40 30 24))
    (with-temp-buffer
      (use-local-map org-upwell-bench-mode-map)
      (dolist (line (split-string
                     (substring-no-properties (org-upwell--bench-legend width))
                     "\n"))
        (should (<= (string-width line) width))))))

(ert-deftest org-upwell-test-the-legend-separates-row-from-page ()
  "A row command pressed on the title line says only that there is no item
there, which is a puzzle rather than an explanation.  So the foot says which
is which."
  (with-temp-buffer
    (use-local-map org-upwell-bench-mode-map)
    (let* ((text (substring-no-properties (org-upwell--bench-legend 80)))
           (split (string-search "on the bench" text)))
      (should split)
      (should (< (string-search "keep it here" text) split))
      (should (> (string-search "add a URL" text) split)))))

(ert-deftest org-upwell-test-the-old-expand-names-still-answer ()
  "The bench is the noun this package has, and `expand\=' was the verb from
when asking for a heading meant opening its files.  Renamed, not removed: a
configuration that bound the old name keeps working, and hears about it from
the byte-compiler rather than from a void-function at the keyboard."
  (dolist (pair '((org-upwell-expand . org-upwell-bench)
                  (org-upwell-expand-clock . org-upwell-bench-clock)
                  (org-upwell-expand-id . org-upwell-bench-id)))
    (should (fboundp (car pair)))
    (should (eq (indirect-function (car pair)) (indirect-function (cdr pair)))))
  (dolist (pair '((org-upwell-expand-max . org-upwell-bench-open-max)
                  (org-upwell-expand-open-location . org-upwell-bench-open-location)
                  (org-upwell-expand-clock-in . org-upwell-bench-clock-in)
                  (org-upwell-last-expanded-id . org-upwell-last-bench-id)))
    (should (eq (indirect-variable (car pair)) (cdr pair))))
  ;; and the file answers to the name a `require' may still use
  (should (featurep 'org-upwell-expand)))

(ert-deftest org-upwell-test-the-protocol-still-takes-expand ()
  "A URL already in somebody\='s bookmarks is not something a rename breaks."
  (org-upwell-test--with-dir
    (let ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (seen nil))
      (org-id-add-location "T1" file)
      (cl-letf (((symbol-function 'org-upwell-bench-id)
                 (lambda (id) (setq seen id))))
        (org-upwell-protocol (list :expand "T1"))
        (should (equal seen "T1"))
        (setq seen nil)
        (org-upwell-protocol (list :bench "T1"))
        (should (equal seen "T1"))))))

(ert-deftest org-upwell-test-the-old-directory-names-still-answer ()
  "Renamed, not removed: a configuration that called them by the old name
keeps working, and finds out from the byte-compiler rather than from a
void-function at the keyboard."
  (should (fboundp 'org-upwell-open-folder))
  (should (fboundp 'org-upwell-trace-folder-p))
  (should (eq (indirect-function 'org-upwell-open-folder)
              (indirect-function 'org-upwell-open-directory)))
  (should (eq (indirect-function 'org-upwell-trace-folder-p)
              (indirect-function 'org-upwell-trace-directory-p))))

;;;; Kind, and the round trip a property has to survive

(ert-deftest org-upwell-test-one-directory-is-one-record ()
  "A directory arrives with a separator on the end from one watcher and
without it from another, and identity is exact string equality.  Trimmed
on the way in, so pinning it and then walking through it do not leave two
rows for one place."
  (org-upwell-test--with-dir
    (let ((sub (expand-file-name "papers" dir)))
      (make-directory sub t)
      (org-upwell-save (list :path (file-name-as-directory sub)))
      (org-upwell-save (list :path sub))
      (should (= 1 (length (org-upwell-items))))
      (should (equal sub (plist-get (car (org-upwell-items)) :path))))))

(ert-deftest org-upwell-test-a-directory-says-the-place-above-it ()
  "The where column answers \"which of the two same-named things is this\".
For a file that is the directory holding it; for a directory it has to be
the one above, or the row says its own name twice."
  (should (equal "/tmp/a/b"
                 (org-upwell--item-where (list :path "/tmp/a/b/c.xlsx"))))
  (should (equal "/tmp/a"
                 (org-upwell--item-where (list :path "/tmp/a/b" :kind "dir")))))

(ert-deftest org-upwell-test-a-directory-is-stood-in-not-opened ()
  "`org-upwell-open-function\=' is a policy about which extensions belong to
the OS and which to Emacs.  A directory has no extension and no such
answer."
  (org-upwell-test--with-dir
    (let ((sub (expand-file-name "papers" dir))
          (revealed nil)
          (opened nil))
      (make-directory sub t)
      (let ((item (org-upwell-save (list :path sub))))
        (cl-letf (((symbol-function 'org-upwell--reveal-external)
                   (lambda (p) (setq revealed p)))
                  ((symbol-function 'org-upwell--open-path-external)
                   (lambda (p) (setq opened p))))
          (org-upwell-open item))
        (should (equal sub revealed))
        (should-not opened)))))


(ert-deftest org-upwell-test-kind-goes-in-and-comes-back-the-same ()
  "A property value is a string.  A field that goes in as a symbol comes
back as a string and never compares equal to itself, which makes every
save think the heading changed -- so the store is rewritten on a pass
that changed nothing."
  (org-upwell-test--with-dir
    (let* ((sub (expand-file-name "papers" dir))
           (file (expand-file-name "quote.csv" dir)))
      (make-directory sub t)
      (write-region "x" nil file)
      (should (equal "dir" (plist-get (org-upwell-save (list :path sub)) :kind)))
      (should (equal "file" (plist-get (org-upwell-save (list :path file)) :kind)))
      ;; and it reads back as the same string, not as a symbol
      (should (equal "dir" (plist-get (org-upwell-find :path sub) :kind)))
      (should (org-upwell-directory-item-p (org-upwell-find :path sub)))
      (should-not (org-upwell-directory-item-p (org-upwell-find :path file))))))

(ert-deftest org-upwell-test-a-stale-item-saved-twice-writes-once ()
  "The whole reason the round trip matters.  `:stale\=' went in as t and came
back as \"t\", so every save of a stale item called the heading changed and
sent the store to disk again."
  (org-upwell-test--with-dir
    (let ((path (expand-file-name "quote.csv" dir))
          (writes 0)
          (orig (symbol-function 'save-buffer)))
      (write-region "x" nil path)
      (org-upwell-save (list :path path :stale t))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest a) (setq writes (1+ writes)) (apply orig a))))
        (org-upwell-save (list :path path :stale t)))
      (should (= 0 writes)))))

(ert-deftest org-upwell-test-a-record-from-before-the-field-gets-one ()
  "A store written before `UPWELL_KIND\=' existed carries none.  The next
sighting has to fill it in, which it only does if the field is one of the
things a save compares."
  (org-upwell-test--with-dir
    (let ((sub (expand-file-name "papers" dir)))
      (make-directory sub t)
      (org-upwell-save (list :path sub))
      ;; make it look like a record from before the field
      (with-current-buffer (find-file-noselect (org-upwell-file))
        (goto-char (point-min))
        (should (search-forward org-upwell-prop-kind nil t))
        (org-back-to-heading t)
        (org-entry-delete (point) org-upwell-prop-kind)
        (save-buffer))
      (org-upwell--store-drop (plist-get (org-upwell-find :path sub) :id))
      (should-not (plist-get (org-upwell-find :path sub) :kind))
      (org-upwell-save (list :path sub))
      (should (equal "dir" (plist-get (org-upwell-find :path sub) :kind))))))

(ert-deftest org-upwell-test-a-record-without-a-kind-reads-as-a-file ()
  "Written before the field existed.  It answers \"file\" until the next
sighting fills it in, rather than costing a stat on every walk of the
store -- one of these paths may be on a file server."
  (should (equal "file" (org-upwell-kind (list :path "/anywhere"))))
  (should-not (org-upwell-directory-item-p (list :path "/anywhere"))))

;;;; What Emacs itself sees

(ert-deftest org-upwell-test-a-written-trace-reads-back ()
  "Emacs writes into the file the watcher writes, so the reader must not be
able to tell them apart.  Built with `json-encode\=' rather than by hand
because the path that would break a hand-built line -- a quote, a
backslash, a character outside ASCII -- is a path somebody really has."
  (org-upwell-test--with-dir
    (dolist (name (list "plain.txt"
                        "with space.txt"
                        "with \"quote\".txt"
                        "納品スケジュール.xlsx"))
      (let ((path (expand-file-name name dir)))
        (org-upwell-trace-write :path path :kind "file")))
    (let* ((traces (org-upwell-read-trace-file (org-upwell-trace-file)))
           (paths (mapcar (lambda (tr) (plist-get tr :path)) traces)))
      (should (= 4 (length traces)))
      (dolist (name (list "plain.txt" "with space.txt"
                          "with \"quote\".txt" "納品スケジュール.xlsx"))
        (should (member (expand-file-name name dir) paths)))
      ;; and the fields the watcher writes are all there
      (let ((one (car traces)))
        (should (numberp (plist-get one :ts)))
        (should (equal "Emacs" (plist-get one :app)))
        (should (eq 'file (plist-get one :kind)))))))

(ert-deftest org-upwell-test-a-written-directory-keeps-its-kind ()
  "`kind\=' is the one field that says a directory is a directory when the
path no longer exists to be asked."
  (org-upwell-test--with-dir
    (org-upwell-trace-write :path (expand-file-name "gone" dir) :kind "dir")
    (let ((tr (car (org-upwell-read-trace-file (org-upwell-trace-file)))))
      (should (eq 'dir (plist-get tr :kind))))))

(ert-deftest org-upwell-test-dired-is-written-down-once ()
  "The directory dired is showing is the thing nothing else can see.  Written
when it changes, and not again while you stay -- the rule the watcher
follows, for the same reason."
  (org-upwell-test--with-dir
    (let ((org-upwell-sight--buffer nil)
          (org-upwell-sight--payload nil)
          (here (expand-file-name "acme" dir)))
      (make-directory here t)
      (dired here)
      (unwind-protect
          (progn
            (org-upwell-sight--update)
            (should (= 1 (length (org-upwell-read-trace-file
                                  (org-upwell-trace-file)))))
            ;; staying is silent, even after the buffer pointer is forgotten
            (setq org-upwell-sight--buffer nil)
            (org-upwell-sight--update)
            (should (= 1 (length (org-upwell-read-trace-file
                                  (org-upwell-trace-file)))))
            (let ((tr (car (org-upwell-read-trace-file
                            (org-upwell-trace-file)))))
              (should (eq 'dir (plist-get tr :kind)))
              ;; no trailing separator: the shape the store keeps
              (should (equal here (plist-get tr :path)))))
        (kill-buffer (current-buffer))))))

(ert-deftest org-upwell-test-only-work-files-are-written-down ()
  "A file under the roots is work.  One outside them is a configuration
file, a library, or this package's own store, and a bench buried in those
would be worse than a bench missing a file."
  (org-upwell-test--with-dir
    (let* ((work (expand-file-name "work" dir))
           (elsewhere (expand-file-name "elsewhere" dir))
           (org-upwell-search-roots (list work)))
      (make-directory work t)
      (make-directory elsewhere t)
      (dolist (case (list (cons (expand-file-name "quote.csv" work) t)
                          (cons (expand-file-name "init.el" elsewhere) nil)
                          ;; the store, even when it sits in a root
                          (cons (org-upwell-file) nil)))
        (let ((path (car case))
              (wanted (cdr case))
              (org-upwell-sight--buffer nil)
              (org-upwell-sight--payload nil))
          (make-directory (file-name-directory path) t)
          (write-region "x" nil path)
          (let ((buf (find-file-noselect path)))
            (unwind-protect
                (with-current-buffer buf
                  (should (eq wanted
                              (and (org-upwell-sight--payload) t))))
              (kill-buffer buf))))))))

(ert-deftest org-upwell-test-what-emacs-saw-is-claimed-like-anything-else ()
  "The whole point of writing into the watcher's own file: nothing
downstream is told which process saw the thing."
  (org-upwell-test--with-dir
    (let* ((work (expand-file-name "work" dir))
           (org-upwell-search-roots (list work))
           (sheet (expand-file-name "quote.csv" work))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (now (current-time)))
      (make-directory work t)
      (write-region "x" nil sheet)
      (let ((buf (find-file-noselect sheet))
            (org-upwell-sight--buffer nil)
            (org-upwell-sight--payload nil))
        (unwind-protect
            (with-current-buffer buf (org-upwell-sight--update))
          (kill-buffer buf)))
      (org-upwell-claim-interval mk (time-subtract now 3600)
                                 (time-add now 60))
      (should (equal (list "quote.csv")
                     (mapcar (lambda (m) (plist-get m :name))
                             (org-upwell-claimed-to "T1")))))))

(ert-deftest org-upwell-test-a-dired-directory-reaches-the-heading ()
  "End to end, and the reason this half of capture exists.  The resident
watcher has no branch for Emacs, so a person who does their directory work
in dired had nothing recorded at all -- and the directory that was
recorded by the other watchers was thrown away before it could be claimed.
Both halves have to hold for this to pass."
  (org-upwell-test--with-dir
    (let* ((here (expand-file-name "acme" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (now (current-time)))
      (make-directory here t)
      (dired here)
      (unwind-protect
          (let ((org-upwell-sight--buffer nil)
                (org-upwell-sight--payload nil))
            (org-upwell-sight--update))
        (kill-buffer (current-buffer)))
      (org-upwell-claim-interval mk (time-subtract now 3600)
                                 (time-add now 60))
      (let ((claimed (org-upwell-claimed-to "T1")))
        (should (equal (list "acme") (mapcar (lambda (m) (plist-get m :name))
                                             claimed)))
        (should (org-upwell-directory-item-p (car claimed)))
        (should (equal here (plist-get (car claimed) :path)))))))

(ert-deftest org-upwell-test-sight-mode-is-on-with-the-mode ()
  "Nothing to configure: the watcher cannot see in here, so Emacs looking at
itself is part of catching the hour, not an extra."
  (let ((was org-upwell-mode))
    (unwind-protect
        (progn
          (org-upwell-mode 1)
          (should org-upwell-sight-mode)
          (should (memq #'org-upwell-sight--update post-command-hook))
          (org-upwell-mode -1)
          (should-not org-upwell-sight-mode)
          (should-not (memq #'org-upwell-sight--update post-command-hook)))
      (org-upwell-mode (if was 1 -1)))))

;;;; What a row says it is

(ert-deftest org-upwell-test-the-form-column-says-what-it-can ()
  "The extension when it is known, because that is the most anybody can
say in six columns.  A word for the kind only when there is no extension
to read: a SharePoint short link says which application opens it and
nothing else, and an .aspx is a page whatever it is called."
  (dolist (case '(("pptx"   :path "/a/b/deck.pptx")
                  ("xlsx"   :path "/a/b/quote.XLSX")
                  ("file"   :path "/a/b/notes")
                  ("dir"    :path "/a/b/papers" :kind "dir")
                  ("slides" :url "https://c.sharepoint.com/:p:/r/s/Doc.aspx?d=x")
                  ("sheet"  :url "https://c.sharepoint.com/:x:/r/s/Doc.aspx?d=x")
                  ("pptx"   :url "https://c.sharepoint.com/s/_layouts/15/Doc.aspx?d=x&file=deck.pptx")
                  ("page"   :url "https://c.sharepoint.com/sites/s/SitePages/plan.aspx")
                  ("pdf"    :url "https://example.com/vendor/manual.pdf")
                  ("page"   :url "https://example.com/news")))
    (should (equal (car case) (org-upwell-form (cdr case))))))

(ert-deftest org-upwell-test-the-protocol-and-the-column-read-one-table ()
  "Two tables of what a URL opens in would disagree the first time one of
them was added to."
  (should (equal "excel" (org-upwell-office-app "https://c/:x:/r/s/x")))
  (should (string-prefix-p "ms-excel:ofe|u|"
                           (org-upwell-mint-office "https://c/:x:/r/s/x")))
  (should-not (org-upwell-office-app "https://example.com/news"))
  (should-not (org-upwell-mint-office "https://example.com/news")))

(ert-deftest org-upwell-test-directories-are-listed-above-files ()
  "Sorted in among the files, the place the work is kept moves every time a
file is added.  Above them it is always in the same place."
  (org-upwell-test--with-dir
    (let* ((sub (expand-file-name "acme" dir))
           (sheet (expand-file-name "quote.xlsx" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (make-directory sub t)
      (write-region "x" nil sheet)
      (dolist (p (list sheet sub))
        (org-upwell-claim (org-upwell-save (list :path p)) "T1" 'confirmed))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (let ((text (substring-no-properties (buffer-string))))
          (should (< (string-search "Directories" text)
                     (string-search "Files" text)))
          (should (< (string-search "acme" text)
                     (string-search "quote.xlsx" text))))))))

(ert-deftest org-upwell-test-an-empty-section-still-says-so ()
  "A heading with nothing under it is what says the question has not been
answered yet -- there is a key for answering it."
  (org-upwell-test--with-dir
    (let* ((sheet (expand-file-name "quote.xlsx" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (write-region "x" nil sheet)
      (org-upwell-claim (org-upwell-save (list :path sheet)) "T1" 'confirmed)
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Directories\n  (none)" text))
          (should (string-search "quote.xlsx" text)))))))

(ert-deftest org-upwell-test-both-sections-share-one-set-of-columns ()
  "One width for both, computed over every item, or the columns step at the
break."
  (org-upwell-test--with-dir
    (let* ((sub (expand-file-name "a" dir))
           (sheet (expand-file-name "a-very-long-file-name-indeed.xlsx" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (make-directory sub t)
      (write-region "x" nil sheet)
      (dolist (p (list sheet sub))
        (org-upwell-claim (org-upwell-save (list :path p :provenance "pin"))
                          "T1" 'confirmed))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (goto-char (point-min))
        (let (columns)
          (while (not (eobp))
            (when (org-upwell--bench-item-at-point)
              (beginning-of-line)
              ;; where the provenance column starts
              (when (re-search-forward "  \\(pin\\|trace\\)" (line-end-position) t)
                (push (- (match-beginning 0) (line-beginning-position))
                      columns)))
            (forward-line 1))
          (should (= 2 (length columns)))
          (should (= (car columns) (cadr columns))))))))

(ert-deftest org-upwell-test-marking-steps-over-a-section-heading ()
  "Marking moves down, the way dired does.  The line below the last
directory is a heading, and stopping there would leave the next `m\=' with
nothing to mark."
  (org-upwell-test--with-dir
    (let* ((sub (expand-file-name "acme" dir))
           (sheet (expand-file-name "quote.xlsx" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (make-directory sub t)
      (write-region "x" nil sheet)
      (dolist (p (list sheet sub))
        (org-upwell-claim (org-upwell-save (list :path p)) "T1" 'confirmed))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (goto-char (point-min))
        (should (search-forward "acme" nil t))
        (beginning-of-line)
        (org-upwell-bench-toggle-mark)
        ;; the only row below it is in the other section
        (should (org-upwell--bench-item-at-point))
        (should (equal "quote.xlsx"
                       (plist-get (org-upwell--bench-item-at-point) :name)))))))

(ert-deftest org-upwell-test-a-local-copy-is-marked-and-put-under-it ()
  "Opening a document on SharePoint and downloading it leaves two things,
and they stay two things.  What is worth saying is that they are the same
document."
  (org-upwell-test--with-dir
    (let* ((local (expand-file-name "vendor comparison.pdf" dir))
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n* NEXT Other\n:PROPERTIES:\n:ID: T2\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (write-region "x" nil local)
      (org-upwell-claim (org-upwell-save
                         (list :url "https://example.com/x/vendor comparison.pdf"
                               :name "vendor comparison"))
                        "T1" 'confirmed)
      (org-upwell-claim (org-upwell-save (list :path local)) "T1" 'confirmed)
      ;; a second local file of the same name is not the original of anything:
      ;; two downloads of one document are two copies, not a copy of a copy
      (let ((other (expand-file-name "old/vendor comparison.pdf" dir)))
        (make-directory (file-name-directory other) t)
        (write-region "x" nil other)
        (org-upwell-claim (org-upwell-save (list :path other)) "T1" 'confirmed))
      (org-upwell--bench-draw (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (let* ((text (substring-no-properties (buffer-string)))
               (lines (split-string text "\n")))
          (should (seq-find (lambda (l) (and (string-search "vendor comparison.pdf" l)
                                             (string-suffix-p "copy" l)))
                            lines))
          ;; the URL is the original and is not marked; both local files are
          ;; copies of it, and sit under it
          (let ((orig (seq-position lines nil
                                    (lambda (l _)
                                      (string-match-p "pdf +vendor comparison  " l)))))
            (should orig)
            (should-not (string-suffix-p "copy" (nth orig lines)))
            (should (string-suffix-p "copy" (nth (+ 1 orig) lines)))
            (should (string-suffix-p "copy" (nth (+ 2 orig) lines)))))))))

(ert-deftest org-upwell-test-two-downloads-are-not-copies-of-each-other ()
  "A copy is a copy of something that is not on this machine.  Two
downloads of one document are two copies; neither is the original, and a
listing that made one of them the original of the other would put a row
under a row that is no more the document than it is."
  ;; named the way `org-upwell-save' names them, or the comparison has
  ;; nothing to compare and the test passes for the wrong reason
  (let* ((url (list :url "https://x/vendor" :name "vendor"))
         (a (list :path "/a/vendor.pdf" :name "vendor.pdf"))
         (b (list :path "/b/vendor.pdf" :name "vendor.pdf")))
    ;; with nothing but local files, nothing is a copy
    (should-not (org-upwell--copy-of a (list a b)))
    (should-not (org-upwell--copy-of b (list a b)))
    ;; the URL is never a copy, whatever else is there
    (should-not (org-upwell--copy-of url (list url a b)))
    ;; and both local ones are copies of it
    (should (eq url (org-upwell--copy-of a (list url a b))))
    (should (eq url (org-upwell--copy-of b (list url a b))))))

(ert-deftest org-upwell-test-tidy-offers-only-what-many-names-share ()
  "Two names sharing an opening is a coincidence; a dozen sharing one is the
site putting its own name on every page it serves."
  (let ((org-upwell-tidy-threshold 3))
    (let ((found (org-upwell--tidy-affixes
                  '("社内ポータル - 規定集" "社内ポータル - 出張申請"
                    "社内ポータル - 経費"
                    "PowerPoint - 提案書" "PowerPoint - 議事録"
                    "見積比較"))))
      (should (equal '("社内ポータル - ") (mapcar #'car found)))
      (should (eq 'head (cadr (car found))))
      (should (= 3 (cddr (car found)))))
    ;; and the same at the other end
    (let ((found (org-upwell--tidy-affixes
                  '("規定集 - 社内ポータル" "出張申請 - 社内ポータル"
                    "経費 - 社内ポータル" "見積比較"))))
      (should (equal '(" - 社内ポータル") (mapcar #'car found)))
      (should (eq 'tail (cadr (car found)))))))

(ert-deftest org-upwell-test-tidy-strips-only-what-was-chosen ()
  "Offered rather than stripped: losing real words is worse than keeping a
few noisy ones."
  (org-upwell-test--with-dir
    (let ((org-upwell-tidy-threshold 3))
      (dolist (name '("社内ポータル - 規定集" "社内ポータル - 出張申請"
                      "社内ポータル - 経費" "見積比較 - 社内用"))
        (org-upwell-save (list :name name :url (concat "https://x/" name))))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) (list "start    3  社内ポータル - "))))
        (org-upwell-tidy-names))
      (let ((names (sort (mapcar (lambda (m) (plist-get m :name))
                                 (org-upwell-items))
                         #'string<)))
        (should (member "規定集" names))
        (should (member "出張申請" names))
        (should (member "経費" names))
        ;; not chosen, not touched
        (should (member "見積比較 - 社内用" names))))))

(provide 'org-upwell-test)

;;; org-upwell-test.el ends here
