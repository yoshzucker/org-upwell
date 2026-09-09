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

;;;; Folders

(ert-deftest org-upwell-test-basename-of-a-folder ()
  "A folder arrives with a separator on the end, and
`file-name-nondirectory' answers nothing at all for such a path."
  (should (equal (org-upwell-basename "/a/b/project/") "project"))
  (should (equal (org-upwell-basename "/a/b/c.txt") "c.txt"))
  (should (null (org-upwell-basename "/")))
  (should (null (org-upwell-basename nil))))

(ert-deftest org-upwell-test-a-folder-trace-is-named-after-the-folder ()
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

(ert-deftest org-upwell-test-pinning-a-folder-names-it ()
  (org-upwell-test--with-dir
   (let ((folder (file-name-as-directory (expand-file-name "papers" dir))))
     (make-directory folder t)
     (should (equal (plist-get (org-upwell--spec-from-path folder) :name)
                    "papers")))))

(ert-deftest org-upwell-test-open-folder-reveals-the-file-that-is-there ()
  "Not the folder the path was stored with: a file filed away into a `done'
folder is resolved first, and the folder to stand in is the new one."
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
           (org-upwell-open-folder (org-upwell-find :id (plist-get m :id))))))
     (should revealed)
     (should (equal (file-truename revealed) (file-truename b))))))

(ert-deftest org-upwell-test-open-folder-says-a-url-has-none ()
  (org-upwell-test--with-dir
   (let ((m (org-upwell-save (list :url "https://example.com/a"
                                   :name "a page"))))
     (should-error (org-upwell-open-folder m) :type 'user-error))))

(ert-deftest org-upwell-test-a-folder-trace-is-known-for-one ()
  "`kind' cannot answer alone: the Windows watcher calls the folder it
read a file, so the disk is asked as well."
  (org-upwell-test--with-dir
   (let ((folder (expand-file-name "papers" dir))
         (file (expand-file-name "papers/a.txt" dir)))
     (make-directory folder t)
     (write-region "x" nil file)
     (should (org-upwell-trace-folder-p (list :path folder :kind 'file)))
     (should (org-upwell-trace-folder-p (list :path "/gone/away/" :kind 'dir)))
     (should-not (org-upwell-trace-folder-p (list :path file :kind 'file))))))

(ert-deftest org-upwell-test-a-folder-walked-through-is-not-an-item ()
  "The bench lists things to open.  A folder somebody had open on the way
to a file is where the work was kept, and the file is already there."
  (org-upwell-test--with-dir
   (let* ((folder (expand-file-name "procurement" dir))
          (sheet (expand-file-name "procurement/quote.csv" dir))
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time)))
     (make-directory folder t)
     (write-region "x" nil sheet)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1800))) folder)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1500))) sheet)
     (org-upwell-claim-interval mk (time-subtract now 3600) now)
     (should (org-upwell-find :path sheet))
     (should-not (org-upwell-find :path folder))
     (should (equal (list "quote.csv")
                    (mapcar (lambda (m) (plist-get m :name))
                            (org-upwell-claimed-to "T1")))))))

(ert-deftest org-upwell-test-a-spell-of-only-folders-writes-no-id ()
  "`org-upwell-heading-id' creates an ID in the user's own file.  A spell
with nothing to claim has no business leaving a mark there."
  (org-upwell-test--with-dir
   (let* ((folder (expand-file-name "procurement" dir))
          (file (org-upwell-test--write-journal "* NEXT Task\n"))
          (mk (org-upwell-test--heading-marker file))
          (now (current-time)))
     (make-directory folder t)
     (org-upwell-test--write-trace
      (floor (float-time (time-subtract now 1800))) folder)
     (org-upwell-claim-interval mk (time-subtract now 3600) now)
     (should-not (org-with-point-at mk (org-id-get))))))

(ert-deftest org-upwell-test-sync-leaves-no-unclaimed-folder ()
  "Off the clock a trace becomes an unclaimed item, a row to deal with
later.  A folder walked through is not a row to deal with."
  (org-upwell-test--with-dir
   (let ((folder (expand-file-name "procurement" dir)))
     (make-directory folder t)
     (org-upwell-test--write-trace (floor (float-time (current-time))) folder)
     (org-upwell-sync 1)
     (should-not (org-upwell-find :path folder)))))

;;;; What the bench line says

(ert-deftest org-upwell-test-where-is-the-folder-not-the-name ()
  "The name is already the first thing on the line."
  (should (equal (org-upwell--item-where (list :path "/tmp/a/b/c.xlsx"))
                 "/tmp/a/b"))
  (should (equal (org-upwell--item-where
                  (list :url "https://contoso.sharepoint.com/sites/a/f.xlsx"))
                 "contoso.sharepoint.com"))
  (should (equal (org-upwell--item-where
                  (list :office "ms-excel:ofe|u|https://share.example/a/f.xlsx"))
                 "share.example")))

(ert-deftest org-upwell-test-a-folder-too-long-keeps-its-end ()
  "Cutting the end off leaves every deep path looking like every other.

Two copies of one file are in the same tree up to the last folder or
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
  "Two files called the same thing, kept in two folders.

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
     (org-upwell-bench (org-upwell-domain mk))
     (with-current-buffer "*org-upwell*"
       (let ((text (buffer-string)))
         (should (= 2 (cl-count-if (lambda (l) (string-match-p "quote\\.csv" l))
                                   (split-string text "\n"))))
         (should (string-match-p "/a +pin" text))
         (should (string-match-p "/b +drop" text)))))))

(ert-deftest org-upwell-test-the-folder-column-is-only-as-wide-as-it-needs ()
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
a folder, every row that came out of one browser read the same three words."
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
      (org-upwell-bench (org-upwell-domain mk))
      (with-current-buffer "*org-upwell*"
        (let ((text (buffer-string)))
          (should (string-match-p "納品スケジュール" text))
          (should (string-match-p "見積書の差し替え" text)))))))

(ert-deftest org-upwell-test-a-host-is-cut-from-the-other-end ()
  "A folder is told apart by its last name and a host by its first, so the
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
      ;; and the folder still keeps its end
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
      (org-upwell-bench (org-upwell-domain mk))
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
      (org-upwell-bench (org-upwell-domain mk))
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

(ert-deftest org-upwell-test-expand-with-a-prefix-asks-anyway ()
  "C-u C-c v is the way to the list when point is on a heading."
  (org-upwell-test--with-dir
   (let* ((org-upwell-expand-max 0)
          (org-upwell-expand-open-location nil)
          (file (org-upwell-test--write-journal
                 "* NEXT Task\n:PROPERTIES:\n:ID: T1\n:END:\n"))
          (mk (org-upwell-test--heading-marker file))
          (asked 0))
     (cl-letf (((symbol-function 'org-upwell--read-heading-marker)
                (lambda () (setq asked (1+ asked)) mk)))
       (with-current-buffer (marker-buffer mk)
         (goto-char mk)
         (let ((current-prefix-arg '(4)))
           (call-interactively #'org-upwell-expand))
         (should (= 1 asked))
         (let ((current-prefix-arg nil))
           (call-interactively #'org-upwell-expand))
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
     (org-upwell-bench (org-upwell-domain mk))
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
     (org-upwell-bench (org-upwell-domain mk))
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
     (org-upwell-bench (org-upwell-domain mk))
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
     (org-upwell-bench (org-upwell-domain mk))
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
           (org-upwell-bench (org-upwell-domain mk))
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

(provide 'org-upwell-test)

;;; org-upwell-test.el ends here
