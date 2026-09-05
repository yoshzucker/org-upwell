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
(require 'org-upwell-expand)
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
        (cl-letf (((symbol-function 'org-upwell-search-roots)
                   (lambda () (list dir))))
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
      (org-upwell-bench (org-upwell-domain mk))
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
      (org-upwell-bench (org-upwell-domain mk))
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

(ert-deftest org-upwell-test-expand-does-not-open-a-hidden-bench ()
  "Expand opens files; a hidden listing stays hidden."
  (org-upwell-test--with-dir
    (let ((file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (called nil))
      (cl-letf (((symbol-function 'org-upwell-bench)
                 (lambda (&rest _) (setq called t))))
        (org-upwell-expand (org-upwell-test--heading-marker file)))
      (should-not called)
      (should-not (get-buffer "*org-upwell*")))))

(ert-deftest org-upwell-test-expand-switches-a-showing-bench ()
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
      (org-upwell-bench (org-upwell-domain drop))
      (with-current-buffer "*org-upwell*"
        (should (equal "DROP" (plist-get org-upwell-bench-domain :id)))
        (should-not (plist-get org-upwell-bench-domain :items)))
      (cl-letf (((symbol-function 'org-upwell-open) #'ignore)
                ((symbol-function 'org-upwell--open-url) #'ignore))
        (org-upwell-expand quotes))
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
      (org-upwell-bench (org-upwell-domain mk))
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
      (org-upwell-bench (org-upwell-domain
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
      (cl-letf (((symbol-function 'org-upwell-bench)
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
      (cl-letf (((symbol-function 'org-upwell-bench)
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
      (org-upwell-bench (org-upwell-domain
                         (org-upwell-test--heading-marker file)))
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

(ert-deftest org-upwell-test-demo-quotes-heading-has-the-three-files ()
  "Compare the three quotes is why expand in the demo has something to show."
  (org-upwell-test--with-dir
    (let ((org-upwell-demo-directory (expand-file-name "demo" dir)))
      (org-upwell-demo-regenerate)
      (let ((org-upwell-directory org-upwell-demo-directory)
            (file (expand-file-name "projects.org" org-upwell-demo-directory)))
        (should (equal '("brief.txt" "quote-acme.csv" "quote-beta.txt")
                       (sort (mapcar (lambda (m) (plist-get m :name))
                                     (plist-get (org-upwell-domain
                                                 (org-upwell-test--marker-at-id
                                                  file org-upwell-demo-id-quotes))
                                                :items))
                             #'string<)))))))

(ert-deftest org-upwell-test-demo-expand-switches-bench-from-drop-to-quotes ()
  "The path the demo teaches: bench opens on File the photos, C-c v on
Compare the three quotes must put the quotes on the bench."
  (org-upwell-test--with-dir
    (let ((org-upwell-demo-directory (expand-file-name "demo" dir)))
      (org-upwell-demo-regenerate)
      (let* ((org-upwell-directory org-upwell-demo-directory)
             (org-upwell-expand-max 0)
             (org-upwell-expand-open-location nil)
             (file (expand-file-name "projects.org" org-upwell-demo-directory))
             (drop (org-upwell-test--marker-at-id file org-upwell-demo-id-drop))
             (quotes (org-upwell-test--marker-at-id
                      file org-upwell-demo-id-quotes)))
        (org-upwell-bench (org-upwell-domain drop))
        (with-current-buffer "*org-upwell*"
          (should (equal org-upwell-demo-id-drop
                         (plist-get org-upwell-bench-domain :id))))
        (org-upwell-expand quotes)
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

(ert-deftest org-upwell-test-expand-does-not-take-the-bench-window ()
  "C-c v on the bench must not replace the listing it just redrew."
  (org-upwell-test--with-dir
    (let* ((org-upwell-expand-max 0)
           (org-upwell-expand-open-location nil)
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file)))
      (org-upwell-bench (org-upwell-domain mk))
      (let ((bench (get-buffer-window "*org-upwell*" nil)))
        (should (window-live-p bench))
        (select-window bench)
        (org-upwell-expand)
        (should (window-live-p bench))
        (should (eq (window-buffer bench) (get-buffer "*org-upwell*")))))))

;;;; Opening

(ert-deftest org-upwell-test-expand-does-not-take-the-agenda-window ()
  "V in the agenda expands the row.  It does not close the agenda."
  (org-upwell-test--with-dir
    (let* ((org-upwell-expand-max 0)
           (org-upwell-expand-open-location nil)
           (file (org-upwell-test--write-journal
                  "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
           (mk (org-upwell-test--heading-marker file))
           (agenda (get-buffer-create "*Org Agenda*")))
      (with-current-buffer agenda (org-agenda-mode))
      (delete-other-windows)
      (set-window-buffer (selected-window) agenda)
      (org-upwell-expand mk)
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

(ert-deftest org-upwell-test-search-roots-without-org-directory ()
  "Resolve must not die on a configuration that never set `org-directory'."
  (let ((org-directory nil))
    (should (listp (org-upwell-search-roots)))))

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

(provide 'org-upwell-test)

;;; org-upwell-test.el ends here
