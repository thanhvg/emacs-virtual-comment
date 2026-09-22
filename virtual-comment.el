;;; virtual-comment.el --- Virtual Comments    -*- lexical-binding: t; -*-

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-virtual-comment
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.5.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; * Intro
;;
;; This package allows adding virtual comments to files in buffers. These
;; comments don't belong to the files so they don't. They are saved in project
;; root or a global file which can be viewed and searched. The file name is
;; .evc.
;;
;; * Virtual comments
;; A virtual comment is an overlay and it is added above the line it comments on
;; and has the same indentation. The virtual comment can be single line or
;; multiline. Each line can have one comment.
;;
;; * Install
;; Spacemacs layer:
;; https://github.com/thanhvg/spacemacs-eos
;;
;; Vanilla
;; (require 'virtual-comment)
;; (add-hook 'find-file-hook 'virtual-comment-mode)
;;
;; * Commands
;; - virtual-comment-make: create or edit a comment at current line
;; - virtual-comment-next: go to next comment in buffer
;; - virtual-comment-previous: go to previous comment in buffer
;; - virtual-comment-delete: remove the current comment
;; - virtual-comment-paste: paste the last removed comment to current line
;; - virtual-comment-realign: realign the comments if they are misplaced
;; - virtual-comment-persist: manually persist project comments
;; - virtual-comment-show: show all comments of current project in a derived mode
;; - virtual-comment-show-delete-display-unit-at-point: delete comment from virtual-comment-show buffer
;; from outline-mode, press enter on a comment will call virtual-comment-go to go
;; to the location of comment.
;; Commands to link to other location (reference):
;; - virtual-comment-remember-current-location store the current location
;; - virtual-comment-add-ref add the stored location (reference) as comment
;; - virtual-comment-goto-location go to location
;;
;; There are no default bindings at all for these commands.
;;
;; * Remarks
;; It's very hard to manage overlays. So this mode should be use in a sensible way.
;; Only comments of files can be persisted.
;;
;; * Test
;; cask install
;; cask exec ert-runner
;;
;; * Other similar packages and inspirations
;; https://github.com/blue0513/phantom-inline-comment
;; https://www.emacswiki.org/emacs/InPlaceAnnotations
;;
;; Changelog
;; 2022-09-20
;;  0.5.1
;;  - bug fix: check for evc existing before backup
;;  - actually update version
;; 2022-09-16
;;  0.5.0
;;  - won't create .evc if there is no comment for the project
;;  - only save data if there is change
;;  - if can't parse the evc file copy it to .evc.error
;; 2022-09-12
;;  0.4.1 back up to .evc.bk when saving data
;; 2022-02-28:
;;  0.4 virtual-comment-show-delete-display-unit-at-point
;; 2021-11-01:
;;  0.03 virtual-comment-make create its own buffer to get input, no longer use read-from-minibuffer
;; 2021-09-27:
;;  0.02 add location/reference

;;; Code:
(require 'cl-generic)
(require 'cl-lib)
(require 'outline)
(require 'pp)
(require 'project)
(require 'seq)
(require 'simple)
(require 'subr-x)
(require 'thingatpt)

(defvar-local virtual-comment--buffer-data nil
  "Buffer comment data.
Lazily initialized value of `virtual-comment-buffer-data'")

(defvar virtual-comment--store nil
  "Global comment store.
Lazy singleton of `virtual-comment-store'")

(defvar-local virtual-comment--project nil
  "Project comment store.
Lazy value of `virtual-comment-project'")

(defvar-local virtual-comment--is-initialized nil
  "Flag to tell if `virtual-comment--init' has already run.")

(defvar-local virtual-comment--update-data-timer nil
  "Local timer for `virtual-comment--update-data-async'.
When this value is non-nil then there is a timer for
`virtual-comment--update-data' to run in future.")

(defvar virtual-comment--current-location nil
  "A string of stored (yanked) location")

(defcustom virtual-comment-idle-time 1
  "Number of seconds after Emacs is idle to run a scheduled update."
  :type 'number
  :group 'virtual-comment)

(defcustom virtual-comment-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'virtual-comment)

(defface virtual-comment-unanchored-face
  '((t :inherit warning :underline t))
  "Face for a comment whose original line couldn't be confidently found.
Used instead of `virtual-comment-face' when the buffer changed
enough that `virtual-comment--repair-overlay-maybe' had to guess
at (or gave up on) the comment's new location; see the
`unanchored' slot of `virtual-comment-unit'."
  :group 'virtual-comment)

(defcustom virtual-comment-default-file "~/.evc"
  "Default file to save comments."
  :type 'string
  :group 'virtual-comment)

(defcustom virtual-comment-backup-count 3
  "Number of rotated backup generations to keep for the .evc file.
Each time data is persisted, the previous backup becomes
FILE.bk.1, the one before that FILE.bk.2, and so on, up to this
many generations; older ones are discarded.  Set to 0 to disable
rotation and keep only the single FILE.bk as before."
  :type 'integer
  :group 'virtual-comment)

(defcustom virtual-comment-deleted-overlay-ring-size 20
  "Maximum number of deleted comments kept for `virtual-comment-paste'."
  :type 'integer
  :group 'virtual-comment)

(defvar virtual-comment-deleted-overlays nil
  "Stack of recently deleted comment overlays, most recent first.
Each `virtual-comment-delete' pushes onto this list (capped at
`virtual-comment-deleted-overlay-ring-size'); each
`virtual-comment-paste' pops the most recent one off and
restores it.  Formerly a single overlay slot
\(`virtual-comment-deleted-overlay'), which meant a second delete
silently discarded the first.")

(cl-defstruct (virtual-comment-unit
               (:constructor virtual-comment-unit-create)
               (:copier nil))
  "Comment data structure."
  (point nil
         :type integer
         :documentation
         "where the comment starts. It is the begin of the line")
  (comment nil
           :type string
           :documentation "comment string")
  (target nil
          :type string
          :documentation "line content on which the comment is.
For a multi-line region this is the whole region's text joined
by newlines; the overlay itself is still anchored to the first
line of the region.")
  (above-target nil
                :type string
                :documentation
                "line content directly above TARGET, \"\" if TARGET was the
first line of the buffer.  Used to disambiguate TARGET when it
matches more than one line elsewhere in the buffer; see
`virtual-comment--resolve-target'.  Only updated when a location
is confidently confirmed, never on a mere guess.")
  (below-target nil
                :type string
                :documentation
                "line content directly below TARGET, \"\" if TARGET was the
last line of the buffer.  See ABOVE-TARGET.  For a multi-line
region this is the line after the *last* line of the region.")
  (unanchored nil
              :type boolean
              :documentation
              "non-nil when the last repair attempt could not confidently
relocate this comment: either TARGET no longer appears anywhere
in the buffer, or it appears more than once and even the
surrounding context couldn't disambiguate it, so POINT is only a
guess (nearest occurrence to the last known location).  Surfaced
with `virtual-comment-unanchored-face' and in
`virtual-comment-show' rather than silently trusted."))

(cl-defstruct (virtual-comment-buffer-data
               (:constructor virtual-comment-buffer-data-create)
               (:copier nil))
  "Store data of current buffer."
  (filename nil
            :type string
            :documentation "file name from project root, it is not used.")
  (comments nil
            :type list
            :documentation "list of `virtual-comment-unit'"))

(cl-defstruct (virtual-comment-store
               (:constructor virtual-comment-store-create)
               (:copier nil))
  "Global store of comments."
  (default nil
           :type record
           :documentation
           "Comments on files that does not belong to a project `virtual-comment-project'")
  (projects nil
            :type hash-table
            :documentation "hash table of projet ID (MD5) vs `virtual-comment-project'"))

(cl-defstruct (virtual-comment-project
               (:constructor virtual-comment-project-create)
               (:copier nil))
  "Project store of comments."
  (files nil
         :type hash-table
         :documentation "hash table of file-name vs `virtual-comment-buffer-data'")
  (count nil
         :type integer
         :documentation "reference count"))

(cl-defgeneric virtual-comment-equal (vc1 vc2)
  "Compare if two virtual-comment structures are equal.")

(cl-defmethod virtual-comment-equal
  ((vc1 virtual-comment-unit) (vc2 virtual-comment-unit))
  (and
   (equal
    (virtual-comment-unit-point vc1)
    (virtual-comment-unit-point vc2))
   (equal
    (virtual-comment-unit-comment vc1)
    (virtual-comment-unit-comment vc2))
   (equal
    (virtual-comment-unit-target vc1)
    (virtual-comment-unit-target vc2))
   (equal
    (virtual-comment-unit-above-target vc1)
    (virtual-comment-unit-above-target vc2))
   (equal
    (virtual-comment-unit-below-target vc1)
    (virtual-comment-unit-below-target vc2))
   (equal
    (virtual-comment-unit-unanchored vc1)
    (virtual-comment-unit-unanchored vc2))))

(cl-defmethod virtual-comment-equal
  ;; ((vc1 (head virtual-comment-unit)) (vc2 (head virtual-comment-unit)))
  ((vc1 list) (vc2 list))
  (and
   (equal
    (length vc1)
    (length vc2))
   (let* ((sort-fn (lambda (a b) (< (virtual-comment-unit-point a)
                                    (virtual-comment-unit-point b))))
          (sorted-vc1 (sort vc1 sort-fn))
          (sorted-vc2 (sort vc2 sort-fn))
          (head1 (car sorted-vc1))
          (head2 (car sorted-vc2)))
     (while (and sorted-vc1
                 sorted-vc2
                 (virtual-comment-equal head1 head2))
       (setq sorted-vc1 (cdr sorted-vc1))
       (setq sorted-vc2 (cdr sorted-vc2))
       (setq head1 (car sorted-vc1))
       (setq head2 (car sorted-vc2)))
     (if sorted-vc1
         nil
       t))))

(cl-defmethod virtual-comment-equal
  ((vc1 virtual-comment-buffer-data) (vc2 (eql nil)))
  nil)

(cl-defmethod virtual-comment-equal
  ((vc1 (eql nil)) (vc2 virtual-comment-buffer-data))
  nil)

(cl-defmethod virtual-comment-equal
  ((vc1 virtual-comment-buffer-data) (vc2 virtual-comment-buffer-data))
  (and (equal
        (virtual-comment-buffer-data-filename vc1)
        (virtual-comment-buffer-data-filename vc2))
       (virtual-comment-equal
        (virtual-comment-buffer-data-comments vc1)
        (virtual-comment-buffer-data-comments vc2))))

(defun virtual-comment-unit--upgrade (unit)
  "Pad an older, shorter `virtual-comment-unit' record to the current shape.
`cl-defstruct' records are printed and read back positionally
\(`#s(virtual-comment-unit 1 \"hi\" \"line\")', not by slot name), so
a `.evc' file saved by an older version of this package, before
some slot existed, produces a record shorter than what the
current struct definition expects.  Calling an accessor for a
slot beyond the record's actual length raises `args-out-of-range'
the first time it happens, rather than failing at load time where
it would be safe to catch -- so every unit read from disk is
upgraded once, here, before it is ever used."
  (if (and (recordp unit)
           (eq (type-of unit) 'virtual-comment-unit))
      (let* ((template (virtual-comment-unit-create))
             (wanted-len (length template))
             (have-len (length unit)))
        (if (< have-len wanted-len)
            (let ((upgraded (copy-sequence template)))
              (dotimes (i have-len)
                (aset upgraded i (aref unit i)))
              upgraded)
          unit))
    unit))

(defun virtual-comment--upgrade-persisted-data (data)
  "Upgrade every `virtual-comment-unit' found in DATA in place.
DATA is a hash table of file name to `virtual-comment-buffer-data',
as returned by reading the legacy on-disk format (a single
positional-record hash-table blob).  See
`virtual-comment-unit--upgrade'.  Only the legacy reader in
`virtual-comment--load-data-from-file' calls this: the current
on-disk format (see `virtual-comment--serialize-store') stores
comments as small alists, which don't need a positional upgrade
at all -- a field missing from an alist is simply nil, exactly
like a freshly created unit, whether it's missing because the
file predates that field or the running code no longer uses it."
  (maphash
   (lambda (_filename buffer-data)
     (when buffer-data
       (setf (virtual-comment-buffer-data-comments buffer-data)
             (mapcar #'virtual-comment-unit--upgrade
                     (virtual-comment-buffer-data-comments buffer-data)))))
   data)
  data)

(defun virtual-comment--unit-to-alist (unit)
  "Convert `virtual-comment-unit' UNIT to a small, self-describing alist.
Used for on-disk storage; see `virtual-comment--serialize-store'
for why this, rather than the struct itself, is what gets
written to a `.evc' file."
  (list (cons 'point (virtual-comment-unit-point unit))
        (cons 'comment (virtual-comment-unit-comment unit))
        (cons 'target (virtual-comment-unit-target unit))
        (cons 'above-target (virtual-comment-unit-above-target unit))
        (cons 'below-target (virtual-comment-unit-below-target unit))
        (cons 'unanchored (virtual-comment-unit-unanchored unit))))

(defun virtual-comment--alist-to-unit (alist)
  "Convert an on-disk comment ALIST back into a `virtual-comment-unit'.
Any key missing from ALIST -- because it was written by an older
version of this package before that field existed, or by hand --
simply comes back as nil, exactly like a freshly created unit.
Unlike the struct's own positional on-disk format (see
`virtual-comment-unit--upgrade'), this needs no separate upgrade
step, now or for any field added in the future."
  (virtual-comment-unit-create
   :point (alist-get 'point alist)
   :comment (alist-get 'comment alist)
   :target (alist-get 'target alist)
   :above-target (alist-get 'above-target alist)
   :below-target (alist-get 'below-target alist)
   :unanchored (alist-get 'unanchored alist)))

(defun virtual-comment--buffer-data-to-entry (file-name buffer-data)
  "Convert FILE-NAME and its BUFFER-DATA into one on-disk entry.
Shape: (FILE-NAME (comments COMMENT-ALIST...)).  Comments are
sorted by point so that a file whose comments didn't change
serializes to the exact same text next time, keeping diffs
minimal; see `virtual-comment--serialize-store'."
  (list file-name
        (cons 'comments
              (sort (mapcar #'virtual-comment--unit-to-alist
                            (virtual-comment-buffer-data-comments buffer-data))
                    (lambda (a b) (< (or (alist-get 'point a) 0)
                                     (or (alist-get 'point b) 0)))))))

(defun virtual-comment--entry-to-buffer-data (entry)
  "Convert one on-disk ENTRY back to (FILE-NAME . `virtual-comment-buffer-data')."
  (let* ((file-name (car entry))
         (comments (alist-get 'comments (cdr entry))))
    (cons file-name
          (virtual-comment-buffer-data-create
           :filename file-name
           :comments (mapcar #'virtual-comment--alist-to-unit comments)))))

(defun virtual-comment--serialize-store (files-hash)
  "Convert FILES-HASH into the on-disk list format.
FILES-HASH is a hash table of file name to
`virtual-comment-buffer-data', i.e. the `files' slot of a
`virtual-comment-project'.  The result is a plain list of
entries (see `virtual-comment--buffer-data-to-entry'), sorted by
file name, meant to be written with `pp' rather than `prin1'.

This -- rather than printing FILES-HASH itself, a single opaque
hash-table/record blob -- is what makes a `.evc' file readable
and hand-editable, and lets two independent sets of new comments
in the same project usually merge with an ordinary three-way text
merge instead of colliding on one indivisible object.  Sorting
matters for that: without a deterministic order, an unrelated
change could reshuffle the whole file and manufacture spurious
conflicts.  It doesn't make every concurrent edit conflict-free --
two comments added to the very same file at adjacent sorted
positions can still collide, the same trade-off `bookmark.el's
own alist-based format has -- but it turns the common case
\(different files, or different parts of the same file's list\)
from a guaranteed conflict into a clean merge."
  (let (entries)
    (maphash (lambda (file-name buffer-data)
               (push (virtual-comment--buffer-data-to-entry file-name buffer-data)
                     entries))
             files-hash)
    (sort entries (lambda (a b) (string< (car a) (car b))))))

(defun virtual-comment--deserialize-store (entries)
  "Convert on-disk ENTRIES back into a file-name -> `virtual-comment-buffer-data'
hash table.  Inverse of `virtual-comment--serialize-store'."
  (let ((files-hash (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((pair (virtual-comment--entry-to-buffer-data entry)))
        (puthash (car pair) (cdr pair) files-hash)))
    files-hash))

(defun virtual-comment--persisted-entry-p (entry)
  "Loosely validate ENTRY as one on-disk entry: (FILE-NAME . ALIST)."
  (and (consp entry)
       (stringp (car entry))
       (listp (cdr entry))))

(defun virtual-comment--persisted-list-p (data)
  "Validate DATA as the current on-disk list format: a list of entries.
See `virtual-comment--persisted-entry-p'.  Distinguishing this
from the legacy hash-table format
\(`virtual-comment--persisted-data-p'\) is how
`virtual-comment--load-data-from-file' tells old and current
`.evc' files apart, without needing an explicit version marker:
the two are different Lisp types to begin with."
  (and (listp data)
       (seq-every-p #'virtual-comment--persisted-entry-p data)))

(cl-defmethod virtual-comment-equal
  ((ht1 hash-table) (ht2 hash-table))
  "Compare two hash tables of file-name vs `virtual-comment-buffer-data'."
  (and (= (hash-table-count ht1)
          (hash-table-count ht2))
       (catch 'flag (maphash (lambda (x y)
                               (unless (virtual-comment-equal (gethash x ht2) y)
                                 (throw 'flag nil)))
                             ht1)
              t)))

(defun virtual-comment--persisted-data-p (data)
  "Validate data load from file.
Which is a hash table of filename vs `virtual-comment-buffer-data'"
  (and (hash-table-p data)
       (catch 'flag
         (maphash (lambda (filename vcbd)
                    (unless (and (or (stringp filename) (equal filename nil))
                                 (or (virtual-comment-buffer-data-p vcbd) (equal vcbd nil)))
                      (throw 'flag nil)))
                  data)
         ;; (throw 'flag t)
         t)))

(defun virtual-comment--get-store ()
  "Get comment store.
If nil create it and create a hash table to :projects slot."
  (unless virtual-comment--store
    (setq virtual-comment--store
          (virtual-comment-store-create
           :default (virtual-comment-project-create :count 0 :files nil)
           :projects (make-hash-table :test 'equal))))
  virtual-comment--store)

(defun virtual-comment--get-project-in-store (project-id)
  "Get project data from store using PROJECT-ID as key.
When PROJECT-ID is nil return the default store."
  (let ((store (virtual-comment--get-store)))
    (if project-id
        (let ((projects (virtual-comment-store-projects store)))
          (if-let (prj (gethash project-id projects))
              prj
            (puthash project-id
                     (virtual-comment-project-create :count 0 :files nil)
                     projects)))
      ;; else get the default store
      (virtual-comment-store-default store))))

(defun virtual-comment--remove-project-in-store (project-id)
  "Remove project data from store using PROJECT-ID as key.
When PROJECT-ID is nil, the default slot of store is in action."
  (let ((store (virtual-comment--get-store)))
    (if project-id
        (let ((projects (virtual-comment-store-projects store)))
          (remhash project-id projects))
      ;; else we reset the default slot
      (setf (virtual-comment-store-default store)
            (virtual-comment-project-create :count 0 :files nil)))))

(defun virtual-comment--make-project-id (project-location)
  "Return project unique id form PROJECT-LOCATION."
  (md5 project-location))

(defun virtual-comment--get-root ()
  "Return absolute path of current project, nil if not found.

If `project-root' could deal with nil type then we wouldn't need this
helper function. We can cl-method nil type for `project-root' but
it is not our business."
  (if-let (prj (project-current))
      (project-root prj)
    nil))

(defun virtual-comment--get-project ()
  "Get project from store.
Return `virtual-comment--project'."
  (if virtual-comment--project
      virtual-comment--project
    (setq virtual-comment--project
          (let* ((root (virtual-comment--get-root))
                 (project-id (if root
                                 (virtual-comment--make-project-id root)
                               nil)))
            (virtual-comment--get-project-in-store project-id)))))

(defun virtual-comment--remove-project ()
  "Remove project from store."
  (let* ((root (virtual-comment--get-root))
         (project-id (if root
                         (virtual-comment--make-project-id root)
                       nil)))
    (virtual-comment--remove-project-in-store project-id)))

(defun virtual-comment--get-buffer-data-in-project (file-name prj)
  "Get buffer data fro FILE-NAME from PRJ.
If not found create it."
  (if-let (data (gethash file-name (virtual-comment-project-files prj)))
      data
    (puthash file-name
             (virtual-comment-buffer-data-create)
             (virtual-comment-project-files prj))))

(defun virtual-comment--get-buffer-data ()
  "Get struct `virtual-comment--buffer-data' if nil then create it."
  (if virtual-comment--buffer-data
      virtual-comment--buffer-data
    (let ((project-data (virtual-comment--get-project))
          (file-name (virtual-comment--get-buffer-file-name)))
      (setq
       virtual-comment--buffer-data
       (if file-name
           (virtual-comment--get-buffer-data-in-project
            file-name
            project-data)
         (virtual-comment-buffer-data-create))))))

(defun virtual-comment--buffer-data-empty-p (buffer-data)
  "Tell if BUFFER-DATA (`virtual-comment-buffer-data') is empty.
There are two slots but for now we only care about slot comments."
  (not (virtual-comment-buffer-data-comments buffer-data)))

(defun virtual-comment--ovs-to-cmts (ovs)
  "Repair and map overlay OVS list to list of `virtual-comment-unit'."
  (mapcar (lambda (ov)
            (virtual-comment--repair-overlay-maybe ov t))
          ovs))

(defun virtual-comment--update-data ()
  "Update buffer comment data."
  (let ((data (virtual-comment--get-buffer-data))
        (ovs (virtual-comment--get-buffer-overlays t)))
    ;; update file name
    (setf (virtual-comment-buffer-data-filename data)
          (virtual-comment--get-buffer-file-name))
    (setf (virtual-comment-buffer-data-comments data)
          (virtual-comment--ovs-to-cmts ovs))))

(defun virtual-comment--update-data-async ()
  "Update buffer comment data when idle."
  (unless virtual-comment--update-data-timer
    (setq virtual-comment--update-data-timer
          (run-with-idle-timer
           virtual-comment-idle-time
           nil ;; no repeat
           (lambda (buff)
             (with-current-buffer buff
               (virtual-comment--update-data)
               ;; done reset the flag
               (setq virtual-comment--update-data-timer nil)))
           (current-buffer)))))

(defun virtual-comment-get-evc-file ()
  "Return evc file path."
  (let ((root (virtual-comment--get-root)))
    (if root
        (concat root ".evc")
      virtual-comment-default-file)))

(defun virtual-comment--get-buffer-file-name ()
  "Return path from project root, nil when not a file."
  (when-let ((name (buffer-file-name)))
    (let ((root (virtual-comment--get-root))
          (file-abs-path (file-truename name)))
      (if root
          (substring file-abs-path (length (file-truename root)))
        file-abs-path))))

(defun virtual-comment--get-saved-file ()
  "Return path to .evc file at project root."
  (let ((root (virtual-comment--get-root)))
    (if root
        (concat root ".evc")
      virtual-comment-default-file)))

(defun virtual-comment--rotate-backups (file)
  "Rotate numbered backups of FILE.
FILE.bk.1 becomes FILE.bk.2, FILE.bk.2 becomes FILE.bk.3, and so
on up to `virtual-comment-backup-count' generations; anything
older is left to be overwritten.  Does not touch FILE.bk itself;
callers should rename/copy FILE to FILE.bk after calling this."
  (when (> virtual-comment-backup-count 0)
    (cl-loop for n from (1- virtual-comment-backup-count) downto 1
             for src = (format "%s.bk.%d" file n)
             for dst = (format "%s.bk.%d" file (1+ n))
             when (file-exists-p src)
             do (rename-file src dst t))
    (let ((current-bk (format "%s.bk" file)))
      (when (file-exists-p current-bk)
        (rename-file current-bk (format "%s.bk.1" file) t)))))

(defun virtual-comment--dump-data-to-file (data file)
  "Dump DATA to .evc FILE, keeping rotated backups of prior versions.
DATA is a file-name -> `virtual-comment-buffer-data' hash table.
Written pretty-printed (see `virtual-comment--serialize-store')
rather than as a single opaque blob, so the file stays readable
and diffable by hand."
  (when (file-exists-p file)
    (virtual-comment--rotate-backups file)
    (copy-file file (format "%s.bk" file) t))
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-length nil)
          (print-level nil))
      (pp (virtual-comment--serialize-store data)))))

(defun virtual-comment--persist ()
  "Persist project data to file."
  (let* ((new-data (virtual-comment-project-files (virtual-comment--get-project)))
         (file (virtual-comment-get-evc-file))
         (org-data (virtual-comment--load-data-from-file file)))
    (unless (virtual-comment-equal new-data org-data)
      (message "virtual-comment: Saving %s" file)
      (virtual-comment--dump-data-to-file new-data
                                          file))))

(defun virtual-comment--take-non-nil (project-files-slot)
  "Create new hash table from PROJECT-FILES-SLOT and remove empty value."
  (let ((my-hashtable (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (when (virtual-comment-buffer-data-comments v)
                             (puthash k v my-hashtable)))
             project-files-slot)
    my-hashtable))

(defun virtual-comment-persist ()
  "Persist project data to file."
  (interactive)
  (let ((data (virtual-comment--get-project))
        (file (virtual-comment-get-evc-file)))
    (virtual-comment--dump-data-to-file
     ;; need to filter nil data because this is a manual call
     (virtual-comment--take-non-nil (virtual-comment-project-files data)) file)))

(defun virtual-comment--load-data-from-file (file &optional verbose)
  "Read data from FILE.
Return the slot file of `virtual-comment-project'. If not found
or fail, return an empty hash talbe. When data doesn't pass the
`virtual-comment--persisted-data-p' rename .evc file to
.evc.error.

Understands two on-disk shapes: the current, human-readable list
format (`virtual-comment--persisted-list-p', converted with
`virtual-comment--deserialize-store'), and the older
hash-table-of-records format this package used to write
(`virtual-comment--persisted-data-p', upgraded on the fly with
`virtual-comment--upgrade-persisted-data').  A file in the older
format is rewritten in the current one the next time this
project's data is saved -- see `virtual-comment--dump-data-to-file'
-- so the migration is transparent and happens once per project."
  (if (file-exists-p file)
      (with-temp-buffer
        (condition-case err
            (progn (insert-file-contents file)
                   (let ((data (read (current-buffer))))
                     (cond
                      ((virtual-comment--persisted-list-p data)
                       (virtual-comment--deserialize-store data))
                      ((virtual-comment--persisted-data-p data)
                       (virtual-comment--upgrade-persisted-data data))
                      (t (user-error "evc unable to parse persited data")))))
          (user-error
           (let ((file-error (format "%s.error" file)))
             (rename-file file file-error t)
             (message "Could not parse .evc, renamed it to .evc.error"))
           (make-hash-table :test 'equal))
          (error
           (progn
             (message "virtual-comment error: couldn't read %s %s %s" file (car err) (cdr err))
             (make-hash-table :test 'equal)))))
    (when verbose (message "virtual-comment: %s doesn't exist" file))
    (make-hash-table :test 'equal)))

(defun virtual-comment--overlayp (ov)
  "Predicate for OV being comment."
  (overlay-get ov 'virtual-comment))

(defun virtual-comment--get-overlay-at (point)
  "Return the overlay comment of this POINT."
  (seq-find #'virtual-comment--overlayp
            (overlays-in point (1+ point))))

(defun virtual-comment--get-buffer-overlays (&optional should-sort)
  "Get all overlay comment.
When SHOULD-SORT is non-nil sort by point."
  (let ((ovs (seq-filter #'virtual-comment--overlayp
                         (overlays-in (point-min) (point-max)))))
    (if should-sort
        (sort ovs
              (lambda (first second)
                (< (overlay-start first) (overlay-start second))))
      ovs)))

(defun virtual-comment--line-at-point ()
  "Reinvent `thing-at-point line'."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun virtual-comment--line-relative (offset)
  "Return the buffer line OFFSET lines from point, or \"\" past a boundary.
OFFSET is passed to `forward-line'."
  (save-excursion
    (if (= 0 (forward-line offset))
        (virtual-comment--line-at-point)
      "")))

(defun virtual-comment--target-line-count (target)
  "Return the number of lines TARGET spans.
A single-line target (no newline) returns 1."
  (1+ (cl-count ?\n (or target ""))))

(defun virtual-comment--current-region-text (start nlines)
  "Return the buffer text of NLINES lines starting at the line of START.
The returned string has no properties and no trailing newline, so
it can be compared with a stored multi-line `virtual-comment-target'."
  (save-excursion
    (goto-char start)
    (buffer-substring-no-properties
     (line-beginning-position)
     (progn (forward-line (1- nlines)) (line-end-position)))))

(defun virtual-comment--set-context-props (ov)
  "Record the lines above/below OV's region as future disambiguation context.
Called whenever OV's location has just been freshly confirmed
correct (created, edited, pasted, or successfully repaired) --
never when only guessing -- so `virtual-comment-unit-above-target'
/`-below-target' always reflect a known-good fingerprint rather
than possibly-already-wrong text.  For a multi-line region the
below line is the line after the *last* line of the region."
  (save-excursion
    (goto-char (overlay-start ov))
    (overlay-put ov 'virtual-comment-above-target (virtual-comment--line-relative -1))
    (let ((nlines (virtual-comment--target-line-count
                   (overlay-get ov 'virtual-comment-target))))
      (overlay-put ov 'virtual-comment-below-target
                   (virtual-comment--line-relative nlines)))))

(defun virtual-comment--search-all (s)
  "Return every fuzzy match point of S in the buffer, ignoring whitespace.
Like the old `virtual-comment--search' but doesn't stop at the
first hit, so callers can disambiguate between several candidate
lines instead of always trusting whichever comes first."
  (save-excursion
    (let ((words (split-string s))
          (matches nil))
      (when words
        (let ((s-multi-re (mapconcat #'regexp-quote words "\\(?:[ \t\n]+\\)")))
          (goto-char (point-min))
          (while (re-search-forward s-multi-re nil t)
            (push (match-beginning 0) matches)
            (goto-char (match-end 0)))))
      (nreverse matches))))

(defun virtual-comment--closest-point (points hint-point)
  "Return whichever of POINTS is numerically closest to HINT-POINT."
  (car (sort (copy-sequence points)
             (lambda (a b) (< (abs (- a hint-point)) (abs (- b hint-point)))))))

(defun virtual-comment--neighbor-line-matches-p (offset expected)
  "Non-nil if the line OFFSET lines from point equals EXPECTED.
OFFSET is passed to `forward-line'; comparison ignores leading
and trailing whitespace.  EXPECTED of \"\" means \"there should be
no line there\" (a buffer boundary), so it only matches when
`forward-line' can't move that far either."
  (save-excursion
    (if (= 0 (forward-line offset))
        (string= (string-trim (virtual-comment--line-at-point))
                 (string-trim (or expected "")))
      (string-empty-p (or expected "")))))

(defun virtual-comment--resolve-target-via-text (hint-point target above-target below-target)
  "Try to find where TARGET now lives, using text and context alone.
HINT-POINT is the comment's last known position, used to break
ties.  ABOVE-TARGET/BELOW-TARGET are the lines that used to
surround TARGET, used to disambiguate when TARGET matches more
than one line.  Returns a plist (:point POINT :status STATUS):

- `unique'   exactly one line in the buffer matches TARGET
- `context'  several lines matched TARGET, but exactly one also
             had matching neighbors, so it was safe to pick
- `nearest'  still ambiguous even with context; fell back to
             whichever match sits closest to HINT-POINT -- a
             guess, callers should flag this as unanchored
- `not-found' TARGET doesn't appear anywhere in the buffer"
  (let ((matches (virtual-comment--search-all target)))
    (cond
     ((null matches) (list :point nil :status 'not-found))
     ((= (length matches) 1) (list :point (car matches) :status 'unique))
     (t
      (let ((context-matches
             (seq-filter
              (lambda (pt)
                (save-excursion
                  (goto-char pt)
                  (and (virtual-comment--neighbor-line-matches-p -1 above-target)
                       (virtual-comment--neighbor-line-matches-p 1 below-target))))
              matches)))
        (if (= (length context-matches) 1)
            (list :point (car context-matches) :status 'context)
          (list :point (virtual-comment--closest-point matches hint-point)
                :status 'nearest)))))))

(defun virtual-comment--resolve-target (hint-point target above-target below-target)
  "Resolve where a comment's TARGET line now is.
Thin wrapper around `virtual-comment--resolve-target-via-text',
kept as its own entry point in case another resolution strategy
is added later."
  (virtual-comment--resolve-target-via-text hint-point target above-target below-target))

(defun virtual-comment--repair-overlay-maybe (ov &optional make-comment-unit)
  "Re-align comment overlay OV if necessary.
When MAKE-COMMENT-UNIT is non nil return `virtual-comment-unit'.

If OV's region still reads the same as when it was last confirmed,
only its bounds/display are refreshed.  Otherwise
`virtual-comment--resolve-target' is used to try to find where
the region went; a confident match (`unique' or `context') moves
OV there and re-confirms its context fingerprint for next time.
A mere guess (`nearest') moves OV too, for display purposes, but
leaves its stored fingerprint untouched (so a later, better
positioned repair can still recover) and flags it unanchored.  If
the region can't be found at all (`not-found'), OV is left
exactly where it was rather than silently drifting to whatever
now happens to be there, and is likewise flagged."
  (save-excursion
    (goto-char (overlay-start ov))
    (let* ((org-target (overlay-get ov 'virtual-comment-target))
           (nlines (virtual-comment--target-line-count org-target))
           (current-target (virtual-comment--current-region-text
                            (overlay-start ov) nlines)))
      (if (string= org-target current-target)
          (overlay-put ov 'virtual-comment-unanchored nil)
        (let* ((above (or (overlay-get ov 'virtual-comment-above-target) ""))
               (below (or (overlay-get ov 'virtual-comment-below-target) ""))
               (resolution (virtual-comment--resolve-target
                            (overlay-start ov) org-target above below))
               (found (plist-get resolution :point))
               (status (plist-get resolution :status)))
          (cond
           ((memq status '(unique context))
            (goto-char found)
            (move-overlay ov (line-beginning-position) (line-end-position))
            (overlay-put ov 'virtual-comment-target
                         (virtual-comment--current-region-text
                          (overlay-start ov) nlines))
            (overlay-put ov 'virtual-comment-unanchored nil)
            (virtual-comment--set-context-props ov))
           ((eq status 'nearest)
            (goto-char found)
            (move-overlay ov (line-beginning-position) (line-end-position))
            (overlay-put ov 'virtual-comment-unanchored t))
           (t
            (overlay-put ov 'virtual-comment-unanchored t))))))
    (goto-char (overlay-start ov))
    (unless (bolp)
      (move-overlay ov (line-beginning-position) (line-end-position)))
    (overlay-put ov
                 'before-string
                 (virtual-comment--make-comment-for-display
                  (overlay-get ov 'virtual-comment)
                  (current-indentation)
                  (overlay-get ov 'virtual-comment-unanchored))))
  (when make-comment-unit
    (virtual-comment-unit-create
     :point (overlay-start ov)
     :comment (overlay-get ov 'virtual-comment)
     :target (overlay-get ov 'virtual-comment-target)
     :above-target (overlay-get ov 'virtual-comment-above-target)
     :below-target (overlay-get ov 'virtual-comment-below-target)
     :unanchored (overlay-get ov 'virtual-comment-unanchored))))

;;;###autoload
(defun virtual-comment-realign ()
  "Realign overlays if necessary."
  (interactive)
  (mapc #'virtual-comment--repair-overlay-maybe
        (virtual-comment--get-buffer-overlays))
  (virtual-comment--update-data-async-maybe))

(defun virtual-comment--get-comment-at (point)
  "Return comment string at POINT."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (overlay-get ov 'virtual-comment)))

(defun virtual-comment--insert-hook-handler (ov is-after-change &rest _)
  "Update ov field virtual-comment-target.
OV is overlay, IS-AFTER-CHANGE, _ are extra params.  For a
multi-line region the new target is recomputed over the same
number of lines the region spanned before the change."
  (when is-after-change
    (overlay-put ov
                 'virtual-comment-target
                 (let ((nlines (virtual-comment--target-line-count
                                (overlay-get ov 'virtual-comment-target))))
                   (virtual-comment--current-region-text
                    (overlay-start ov) nlines)))))

(defun virtual-comment--get-neighbor-cmt (point end-point getter-func)
  "Return point of the neighbor comment of POINT, nil if not found.
With GETTER-FUNC until END-POINT."
  (let ((neighbor-pos (funcall getter-func point)))
    (if (virtual-comment--get-overlay-at neighbor-pos)
        neighbor-pos
      (if (= neighbor-pos end-point)
          nil
        (virtual-comment--get-neighbor-cmt neighbor-pos
                                           end-point
                                           getter-func)))))

(defun virtual-comment--kill-buffer-hook-handler ()
  "On buffer about to be killed.
Decrease counter, check if should persist data."
  (when virtual-comment--update-data-timer
    (cancel-timer virtual-comment--update-data-timer)
    (setq virtual-comment--update-data-timer nil)
    (virtual-comment--update-data))
  (let ((data (virtual-comment--get-project))
        (buffer-data (virtual-comment--get-buffer-data)))
    ;; if buffer data is nothing then remove it from project data
    (when (virtual-comment--buffer-data-empty-p buffer-data)
      (remhash
       (virtual-comment--get-buffer-file-name)
       (virtual-comment-project-files data)))
    ;; decrease reference count
    (cl-decf (virtual-comment-project-count data))
    ;; persistence maybe
    (when (= 0 (virtual-comment-project-count data))
      ;; debug message
      ;; (message
      ;;  "virtual-comment: Leaving %s"
      ;;  (virtual-comment--get-root))
      (virtual-comment--persist)
      ;; remove project files from store
      (virtual-comment--remove-project))))

(defun virtual-comment--before-revert-buffer-hook-handler ()
  "On buffer about to revert.
Clear all overlays and act like buffer about to close."
  (virtual-comment--kill-buffer-hook-handler)
  (virtual-comment--clear)
  (setq virtual-comment--is-initialized nil))

(defun virtual-comment--after-revert-buffer-hook-handler ()
  "On buffer after revert. Restore things."
  ;; when it is a hard reload then virtual-comment-mode will
  ;; be called twice via this hook and via normal-mode actions
  ;; but it will do side effect only once thanks to the flag
  ;; virtual-comment--is-initialized
  (virtual-comment-mode))

;;;###autoload
(defun virtual-comment-next ()
  "Go to next/below comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (line-end-position)
                                                     (point-max)
                                                     #'next-overlay-change))
      (goto-char point)
    (message "No next comment found.")))

;;;###autoload
(defun virtual-comment-previous ()
  "Go to previous/above comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (line-beginning-position)
                                                     (point-min)
                                                     #'previous-overlay-change))
      (goto-char point)
    (message "No previous comment found.")))

(defun virtual-comment--make-comment-for-display (comment indent &optional unanchored)
  "Make a comment string with face and INDENT from COMMENT for display.
Uses `virtual-comment-unanchored-face' instead of the normal
`virtual-comment-face' when UNANCHORED is non-nil, to visibly
flag a comment whose location is only a guess; see
`virtual-comment-unit-unanchored'."
  (concat (make-string indent ?\s)
          (mapconcat
           (lambda (it)
             (propertize it 'face (if unanchored
                                      'virtual-comment-unanchored-face
                                    virtual-comment-face)))
           (split-string comment "\n")
           (concat "\n" (make-string indent ?\s)))
          "\n"))

(defun virtual-comment--ov-ensure (ov comment target indent)
  "Ensure overlay OV has all the necessary props from COMMENT, TARGET and INDENT."
  (overlay-put ov 'virtual-comment comment)
  (overlay-put ov 'virtual-comment-target target)
  (overlay-put ov
               'before-string
               (virtual-comment--make-comment-for-display
                comment
                indent))
  (overlay-put ov
               'modification-hooks
               '(virtual-comment--insert-hook-handler)))

(defun virtual-comment--make (unit)
  "Make a comment from a comment UNIT."
  (let ((point (virtual-comment-unit-point unit))
        (comment (virtual-comment-unit-comment unit))
        (target (virtual-comment-unit-target unit)))
    (save-excursion
      (goto-char point)
      (let* ((indent (current-indentation))
             (org-comment (virtual-comment--get-comment-at point))
             (ov (if org-comment (virtual-comment--get-overlay-at point)
                   (make-overlay point (line-end-position) nil t nil))))
        (virtual-comment--ov-ensure ov comment target indent)
        ;; carry the persisted fingerprint over as-is (don't recompute
        ;; from the buffer here): it represents the last confirmed-good
        ;; state, which `virtual-comment--repair-overlay-maybe' needs
        ;; intact to judge whether -- and where -- the file has since
        ;; moved on without us.
        (overlay-put ov 'virtual-comment-above-target
                     (virtual-comment-unit-above-target unit))
        (overlay-put ov 'virtual-comment-below-target
                     (virtual-comment-unit-below-target unit))
        (overlay-put ov 'virtual-comment-unanchored
                     (virtual-comment-unit-unanchored unit))
        (when (virtual-comment-unit-unanchored unit)
          (overlay-put ov 'before-string
                       (virtual-comment--make-comment-for-display comment indent t)))))))

(defun virtual-comment--append (str)
  "Append STR to comment.
Won't prepend new line if comment is nil"
  (let* ((point (line-beginning-position))
         (indent (current-indentation))
         (target (thing-at-point 'line t))
         (org-comment (virtual-comment--get-comment-at point))
         (comment (if org-comment
                      (concat org-comment "\n" str)
                    str))
         ;; must get existing overlay when comment is non-nil
         (ov (if org-comment (virtual-comment--get-overlay-at point)
               (make-overlay point (line-end-position) nil t nil))))
    (virtual-comment--ov-ensure ov comment target indent)
    (virtual-comment--set-context-props ov)
    (virtual-comment--update-data-async-maybe)))

;;;###autoload
(defun virtual-comment-add-location ()
  "Append `virtual-comment--current-location' to current line as comment."
  (interactive)
  (when virtual-comment--current-location
    (virtual-comment--append virtual-comment--current-location)))

(defun virtual-comment--get-locations (str)
  "Get locations from string STR."
  (seq-filter (lambda (x) (string-match-p ".*? | .*?:[0-9]+$" x)) (split-string str "\n")))

(defun virtual-comment--goto-location (str)
  "STR is 'symbol | filepath:number'."
  (when (string-match "\\`.*? | \\(.*?\\):\\([0-9]+\\)\\'" str)
    (let ((file-name (match-string-no-properties 1 str))
          (line-number (match-string-no-properties 2 str)))
      (find-file-other-window (if (file-name-absolute-p file-name)
                                  file-name
                                (expand-file-name file-name (virtual-comment--get-root))))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number))))))

;;;###autoload
(defun virtual-comment-goto-location ()
  "Open location in other window."
  (interactive)
  (when-let* ((cmt (virtual-comment--get-comment-at (line-beginning-position)))
              (candidates (virtual-comment--get-locations cmt)))
    (if (= (length candidates) 1)
        (virtual-comment--goto-location (car candidates))
      (virtual-comment--goto-location (completing-read "Select:" candidates)))))

(defvar-local virtual-comment-make--callback nil)

;;;###autoload
(defun virtual-comment-make (beg end)
  "Add or edit comment for the current line or active region.
When the region is active the comment is associated with the
whole region: the stored TARGET is the multi-line text of the
region, so `virtual-comment-show' displays the region and not
just its first line."
  (interactive
   (if (use-region-p)
       (list (save-excursion (goto-char (region-beginning))
                             (line-beginning-position))
             (save-excursion (goto-char (region-end))
                             (line-end-position)))
     (list (line-beginning-position) (line-end-position))))
  (let* ((point beg)
         (indent (current-indentation))
         (target (buffer-substring-no-properties beg end))
         (org-comment (virtual-comment--get-comment-at point))
         (ov (if org-comment (virtual-comment--get-overlay-at point)
               (make-overlay (line-beginning-position)
                             (line-end-position)
                             nil t nil)))
         (buffer (current-buffer)))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*virtual-comment-make*"))
    (text-mode)
    (virtual-comment-make-mode 1)
    (when org-comment
      (insert org-comment))
    ;; set up callback
    (setq virtual-comment-make--callback
          (lambda (comment)
            (with-current-buffer buffer
              (if (> (length comment) 0)
                  (progn
                    (virtual-comment--ov-ensure ov comment target indent)
                    (virtual-comment--set-context-props ov))
                (unless org-comment
                  (delete-overlay ov)))
              (virtual-comment--update-data-async-maybe))))
    (message
     (substitute-command-keys
      "\\[virtual-comment-make-done] to finish, \\[virtual-comment-make-abort] to abort"))))

(defvar virtual-comment-make-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-k") #'virtual-comment-make-abort)
    (define-key keymap (kbd "C-c C-c") #'virtual-comment-make-done)
    ;; (define-key keymap (kbd "C-c C-b") #'virtual-comment-make-clear)
    keymap))

(define-minor-mode virtual-comment-make-mode
  "Minor mode to get text input for virtual-comment."
  :lighter " evcmm"
  :keymap virtual-comment-make-mode-map
  (if virtual-comment-make-mode
      (add-hook 'post-command-hook 'virtual-comment-make-mode--post-command nil t)
    (remove-hook 'post-command-hook 'virtual-comment-make-mode--post-command t)))

(defun virtual-comment-make-mode--post-command ()
  (virtual-comment-make-mode--adjust-window-size-to-fit-text))

(defun virtual-comment-make-mode--adjust-window-size-to-fit-text ()
  (when (and (> (+ 2 (line-number-at-pos (point-max)))
                (window-height))
             (> (/ (frame-height) (window-height))
                1))
    (enlarge-window 1)))

(defun virtual-comment-make-abort ()
  "Abort virtual-comment-make"
  (interactive)
  (let ((callback virtual-comment-make--callback))
    (kill-buffer)
    (delete-window)
    ;; memory leak must still delete ov
    (funcall callback "")))

(defun virtual-comment-make-clear ()
  "Clear current edit buffer of virtual-comment-make."
  (interactive)
  (delete-region (point-min) (point-max)))

(defun virtual-comment-make-done ()
  "Done with virtual-comment-make, apply the change."
  (interactive)
  ;; grab the text
  ;; run the callback
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (callback virtual-comment-make--callback))
    (kill-buffer)
    (delete-window)
    (funcall callback content)))

(defun virtual-comment--get-location-at-point (&optional want-full-path)
  "Get symbol at point and its project path or full path plus line number."
  (format "%s | %s:%s"
          (thing-at-point 'symbol t)
          (if want-full-path
              (file-truename (buffer-file-name))
            (file-relative-name (file-truename (buffer-file-name)) (virtual-comment--get-root)))
          (line-number-at-pos)))

;;;###autoload
(defun virtual-comment-remember-current-location (prefix)
  "Save current location to `virtual-comment--current-location'."
  (interactive "p")
  (setq virtual-comment--current-location (virtual-comment--get-location-at-point (= 4 prefix))))

(defun virtual-comment--delete-comment-at (point)
  "Delete the comment at point POINT.
Find the overlay for this POINT, delete it, and push it onto
`virtual-comment-deleted-overlays' so it can be recovered with
`virtual-comment-paste', even after further deletes."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (delete-overlay ov)
    (push ov virtual-comment-deleted-overlays)
    (when (> (length virtual-comment-deleted-overlays)
             virtual-comment-deleted-overlay-ring-size)
      (setq virtual-comment-deleted-overlays
            (butlast virtual-comment-deleted-overlays
                     (- (length virtual-comment-deleted-overlays)
                        virtual-comment-deleted-overlay-ring-size))))))

;;;###autoload
(defun virtual-comment-delete ()
  "Delete comments of this current line.
The comment then can be pasted with `virtual-comment-paste'."
  (interactive)
  (let ((point (line-beginning-position)))
    (virtual-comment--delete-comment-at point))
  (virtual-comment--update-data-async-maybe))

(defun virtual-comment--paste-at (point indent target)
  "Paste the most recently deleted comment at POINT with INDENT and TARGET.
Pops it off `virtual-comment-deleted-overlays', so repeated calls
walk back through however many comments were deleted, most
recent first."
  (when-let (ov (pop virtual-comment-deleted-overlays))
    (let ((comment-for-display (virtual-comment--make-comment-for-display
                                (overlay-get ov 'virtual-comment)
                                indent)))
      (overlay-put ov 'before-string comment-for-display)
      (overlay-put ov 'virtual-comment-target target)
      (overlay-put ov 'virtual-comment-unanchored nil)
      (move-overlay ov point (line-end-position))
      (virtual-comment--set-context-props ov))))

;;;###autoload
(defun virtual-comment-paste ()
  "Paste the most recently deleted comment onto the current line.
Calling this repeatedly restores earlier deletes as well, most
recent first; see `virtual-comment-deleted-overlays'."
  (interactive)
  (if virtual-comment-deleted-overlays
      (progn
        (virtual-comment--paste-at (line-beginning-position)
                                   (current-indentation)
                                   (thing-at-point 'line t))
        (virtual-comment--update-data-async-maybe))
    (message "virtual-comment: nothing to paste")))

(defun virtual-comment--clear ()
  "Clear all overlays in current buffer."
  (mapc #'delete-overlay
        (virtual-comment--get-buffer-overlays)))

;;;###autoload
(define-minor-mode virtual-comment-mode
  "This mode shows virtual comments."
  :lighter " evc"
  :keymap (make-sparse-keymap)
  (if virtual-comment-mode
      (virtual-comment-mode-enable)
    (virtual-comment-mode-disable)))

(defun virtual-comment--update-data-async-maybe ()
  "Do update only when mode is active."
  (when virtual-comment-mode
    (virtual-comment--update-data-async)))

(defun virtual-comment--reload-project ()
  "Reload project data.
First remove current project from store then load it from file
and stuff to store again."
  (virtual-comment--remove-project)
  (let* ((project-data (virtual-comment--get-project)))
    (setf (virtual-comment-project-files project-data)
          (if-let (my-data
                   (virtual-comment--load-data-from-file
                    (virtual-comment--get-saved-file)))
              my-data
            (make-hash-table :test 'equal)))))

(defun virtual-comment--reload-data ()
  "Drop current data and reload from the store."
  (virtual-comment--clear)
  (mapc #'virtual-comment--make
        (virtual-comment-buffer-data-comments
         (virtual-comment--get-buffer-data))))

(defun virtual-comment--init ()
  "Get everything ready if necessary store, project and buffer.
This function should only run once when mode is active. That is
after variable `virtual-comment-mode' is enabled in buffer, if you
run (virtual-comment-mode) again this function won't do anything."
  (unless virtual-comment--is-initialized
    ;; get project
    (let* ((project-data (virtual-comment--get-project))
           (count (virtual-comment-project-count project-data)))
      ;; check if project-data needs initialization
      (when (= count 0)
        ;; not initialized yet. must update
        ;; first load project file
        (setf (virtual-comment-project-files project-data)
              (if-let (my-data
                       (virtual-comment--load-data-from-file (virtual-comment--get-saved-file)))
                  my-data
                (make-hash-table :test 'equal))))
      ;; increase ref counter
      (cl-incf (virtual-comment-project-count project-data))
      ;; get buffer data from project and make overlay
      (mapc #'virtual-comment--make
            (virtual-comment-buffer-data-comments
             (virtual-comment--get-buffer-data))))
    (setq virtual-comment--is-initialized t)))

(defun virtual-comment-mode-enable ()
  "Run when variable `virtual-comment-mode' is on."
  (add-hook 'after-save-hook #'virtual-comment--update-data-async 0 t)
  (add-hook 'before-revert-hook
            #'virtual-comment--before-revert-buffer-hook-handler
            0
            t)
  (add-hook 'after-revert-hook
            #'virtual-comment--after-revert-buffer-hook-handler
            0
            t)
  (add-hook 'kill-buffer-hook #'virtual-comment--kill-buffer-hook-handler 0 t)
  ;; (setq virtual-comment-buffer-data nil)
  (virtual-comment--init))

(defun virtual-comment-mode-disable ()
  "Run when variable `virtual-comment-mode' is off."
  (remove-hook 'after-save-hook #'virtual-comment--update-data-async t)
  (remove-hook 'before-revert-hook
               #'virtual-comment--before-revert-buffer-hook-handler
               t)
  (remove-hook 'after-revert-hook
               #'virtual-comment--after-revert-buffer-hook-handler
               t)
  (remove-hook 'kill-buffer-hook #'virtual-comment--kill-buffer-hook-handler t)
  (virtual-comment--kill-buffer-hook-handler)
  (virtual-comment--clear)
  (kill-local-variable 'virtual-comment--is-initialized)
  (kill-local-variable 'virtual-comment--buffer-data)
  (kill-local-variable 'virtual-comment--project))

;; view job
;;;###autoload
(defun virtual-comment-go ()
  "Go to location of comment at point of view buffer."
  (interactive)
  (let ((active-point (get-text-property (point) 'virtual-comment-point))
        (file-name (get-text-property (point) 'virtual-comment-full-path)))
    (when (and active-point file-name)
      (find-file file-name)
      (goto-char active-point))))

(defvar virtual-comment-show-map
  ;; (setq virtual-comment-show-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'virtual-comment-go)
    (define-key map (kbd "C-c C-d") 'virtual-comment-show-delete-display-unit-at-point)
    map)
  "Keymap for show.")

(defun virtual-comment--print-comments (unit file-name root)
  "Print out comments.
from UNIT as `virtual-comment-unit' for FILE-NAME of project
ROOT."
  ;; (message "%s" comments)
  (let* ((full-path (concat root file-name))
         (point (virtual-comment-unit-point unit))
         (comment (virtual-comment-unit-comment unit))
         (target (virtual-comment-unit-target unit))
         (unanchored (virtual-comment-unit-unanchored unit))
         (label (if unanchored (concat "[UNANCHORED] " comment) comment)))
    (insert (format "%s\n%s"
                    (propertize label
                                'font-lock-face (if unanchored
                                                    'virtual-comment-unanchored-face
                                                  'highlight)
                                'virtual-comment-point point
                                'virtual-comment-full-path full-path
                                'virtual-comment-relative-path file-name
                                'keymap virtual-comment-show-map)
                    target))))

(defun virtual-comment--print (file-comments file-name root)
  "Print out comments from FILE-COMMENTS for FILE-NAME of project ROOT.
FILE-COMMENTS is list of (point . comment).

Should produce:
* file-name
comment1
comment2.

If no comments, print nothing
Pressing enter on comment will go to comment."
  (when (> (length file-comments) 0)
    (insert (format "* %s\n" file-name))
    (mapc (lambda (it)
            (virtual-comment--print-comments it file-name root))
          file-comments)))

;; (setq virtual-comment-show-mode-map
(defvar virtual-comment-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    map))

;;;###autoload
(define-derived-mode virtual-comment-show-mode outline-mode "evcs"
  "Major mode to view `virutal-comment' comments."
  (setq buffer-read-only t))

(defun virtual-comment--show (project-data root buffer &optional file-name)
  "Print out an org buffer of project comments to BUFFER.
PROJECT-DATA is `virtual-comment-project' struct.
ROOT is project root."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (maphash
       (lambda (key val)
         (virtual-comment--print
          (virtual-comment-buffer-data-comments val)
          key
          root))
       (virtual-comment-project-files project-data))
      (virtual-comment-show-mode)
      (goto-char (point-min)))
    ;; go to node for file-name
    (when file-name
      (search-forward (concat "* " file-name "\n") nil t))
    ;; (local-set-key (kbd "q") 'quit-window)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun virtual-comment-show ()
  "Show comments for this file and its project."
  (interactive)
  (let ((file-name (virtual-comment--get-buffer-file-name))
        (root (virtual-comment--get-root)))
    (virtual-comment--show virtual-comment--project
                           root
                           (get-buffer-create
                            (format "*%s*" (if root root "default")))
                           file-name)))

(defun virtual-comment--emacs-kill-hook-handler ()
  "Handler to run when emacs about to close.
Go through buffer list and act like buffer about to close."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when virtual-comment-mode
          (virtual-comment--kill-buffer-hook-handler))))))

(defun virtual-comment-show--thing-at-point (&optional thing)
  "_"
  (pcase thing
    ('full-path (get-text-property (point) 'virtual-comment-full-path))
    ('relative-path (get-text-property (point) 'virtual-comment-relative-path))
    (_ (get-text-property (point) 'virtual-comment-point))))

(defun virtual-comment-show--begin-of-display-unit (vc-point)
  "Get the start of the comment display unit.
Display unit is the comment string and target string. Current
point is on the comment string. VC-POINT is the current
`virtual-comment-point of the current point. Find the beginning
of the comment string and return it's point."
  (save-excursion
    ;; go backward
    (while (and (not (bobp))
                (eql vc-point (virtual-comment-show--thing-at-point)))
      (backward-char 1))
    (point)))

(defun virtual-comment-show--end-of-display-unit (vc-point)
  "See `virtual-comment-show--begin-of-display-unit'."
  (save-excursion
    ;; go foward
    (while (and (not (eobp))
                (eql vc-point (if-let ((my-p (virtual-comment-show--thing-at-point)))
                                  my-p
                                vc-point)))
      (forward-char 1))
    (1- (point))))

(defun virtual-comment-show--delete-display-unit (vc-point)
  (let ((inhibit-read-only t))
    (save-excursion
      (delete-region
       (virtual-comment-show--begin-of-display-unit vc-point)
       (virtual-comment-show--end-of-display-unit vc-point)))))

(defun virtual-comment-show-delete-display-unit-at-point ()
  "_"
  (interactive)
  (when-let ((vc-point (virtual-comment-show--thing-at-point))
             (full-path (virtual-comment-show--thing-at-point 'full-path))
             (file-name (virtual-comment-show--thing-at-point 'relative-path)))
    ;; delete the display unit in show mode
    (virtual-comment-show--delete-display-unit vc-point)
    (forward-line)
    ;; update the store
    (virtual-comment--remove-comment vc-point
                                     file-name
                                     (virtual-comment--get-project))
    ;; if buffer is live remove the the comment ov from it
    (virtual-comment--remove-comment-from-file-buffer-maybe vc-point full-path)))

(defun virtual-comment--remove-comment-from-file-buffer-maybe (vc-point full-name)
  "_"
  (when-let ((buff (find-buffer-visiting full-name)))
    (with-current-buffer buff
      (when-let ((ov (virtual-comment--get-overlay-at vc-point)))
        (delete-overlay ov)))))

(defun virtual-comment--remove-comment-from-units (comment-units point)
  "Take COMMENT-UNITS list of `virtual-comment-unit' return the filtered list.
Remove the unit that has COMMENT from the list and return the
list."
  (seq-remove (lambda (cmt-unit)
                (eql (virtual-comment-unit-point cmt-unit) point))
              comment-units))

(defun virtual-comment--remove-comment (vc-point file-name project)
  "-"
  ;; get project from store
  ;; get file data from project
  (let* ((file-data  (virtual-comment--get-buffer-data-in-project
                      file-name
                      project))
         (file-comment-units (virtual-comment-buffer-data-comments file-data)))
    (setf (virtual-comment-buffer-data-comments file-data)
          (virtual-comment--remove-comment-from-units file-comment-units vc-point))))

(add-hook 'kill-emacs-hook #'virtual-comment--emacs-kill-hook-handler)

(provide 'virtual-comment)
;;; virtual-comment.el ends here
