;;; virtual-comment.el --- Virtual Comments

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-virtual-comment
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1

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
;; FIXME
;; Abbrevations:
;; cmt: comment
;; ov: overlay

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'outline)
(require 'simple)


(defvar-local virtual-comment--buffer-data nil
  "Buffer comment data.")

(defvar virtual-comment--store nil
  "Global comment store.")

(defvar-local virtual-comment--project nil
  "Project comment store.")

(defvar-local virtual-comment--is-initialized nil
  "Flag to tell if `virtual-comment--init' has already run.")

(defvar-local virtual-comment--update-data-timer nil
  "Local timer for `virtual-comment--update-data-async'.
When this value is non-nil then there is a timer for
`virtual-comment--update-data' to run in future.")

(defcustom virtual-comment-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'virtual-comment)

(defcustom virtual-comment-default-file "~/.evc"
  "Default file to save comments."
  :type 'string
  :group 'virtual-comment)

(defvar virtual-comment-deleted-overlay nil
  "Ref of the overlay deleted.")

(cl-defstruct (virtual-comment-buffer-data
               (:constructor virtual-comment-buffer-data-create)
               (:copier nil))
  "Store data of current buffer.
FILENAME is file name from project root, it is not used.
COMMENTS is list of (point . comment)."
  filename comments)

(cl-defstruct (virtual-comment-store
               (:constructor virtual-comment-store-create)
               (:copier nil))
  "Global store of comments.
Slot default is default `virtual-comment-project' for buffers
which don't belong to a project. Slot projects is a list of
`virtual-comment-project'."
  default projects)

(cl-defstruct (virtual-comment-project
               (:constructor virtual-comment-project-create)
               (:copier nil))
  "Project store of comments.
Slot files is hashtable of file-name:`virtual-comment-buffer-data'
Slot count is reference count."
  files count)


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

(defun virtual-comment--get-project ()
  "Get project from store.
Return `virtual-comment--project'."
  (if virtual-comment--project
      virtual-comment--project
    (setq virtual-comment--project
          (let* ((root (cdr (project-current)))
                 (project-id (if root
                                 (virtual-comment--make-project-id root)
                               nil)))
            (virtual-comment--get-project-in-store project-id)))))

(defun virtual-comment--remove-project ()
  "Remove project from store."
  (let* ((root (cdr (project-current)))
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
  "Maps overlay OVS list to list of (point . comment)."
  (mapcar (lambda (ov)
            (cons (overlay-start ov) (overlay-get ov 'virtual-comment)))
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
           5 ;; seconds
           nil ;; no repeat
           (lambda (buff)
             (with-current-buffer buff
               (virtual-comment--update-data)
               ;; done reset the flag
               (setq virtual-comment--update-data-timer nil)))
           (current-buffer)))))

(defun virtual-comment-get-evc-file ()
  "Return evc file path."
  (let ((root (cdr (project-current))))
    (if root
        (concat root ".evc")
      virtual-comment-default-file)))

(defun virtual-comment--get-buffer-file-name ()
  "Return path from project root, nil when not a file."
  (when-let ((name (buffer-file-name)))
    (let ((root (cdr (project-current)))
          (file-abs-path (file-truename name)))
      (if root
          (substring file-abs-path (length (file-truename root)))
        file-abs-path))))

(defun virtual-comment--get-saved-file ()
  "Return path to .ipa file at project root."
  (let ((root (cdr (project-current))))
    (if root
        (concat root ".evc")
      virtual-comment-default-file)))

(defun virtual-comment--dump-data-to-file (data file)
  "Dump DATA to .evc FILE."
  (with-temp-file file
    (let ((standard-output (current-buffer)))
      (prin1 data))))

(defun virtual-comment-dump-data ()
  "Dump data."
  (let ((data (virtual-comment--get-project))
        (file (virtual-comment-get-evc-file)))
    (virtual-comment--dump-data-to-file (virtual-comment-project-files
                                         data)
                                        file)))

(defun virtual-comment--load-data-from-file (file)
  "Read data from FILE.
If not found or fail, return an empty hash talbe."
  (with-temp-buffer
    (condition-case nil
        (progn (insert-file-contents file)
               (read (current-buffer)))
      (error (make-hash-table :test 'equal)))))

(defun virtual-comment--load ()
  "Load stuff."
  (setq virtual-comment--buffer-data (virtual-comment--load-data-from-file
                                      (virtual-comment--get-saved-file))))

;; https://stackoverflow.com/questions/16992726/how-to-prompt-the-user-for-a-block-of-text-in-elisp
(defun virtual-comment--read-string-with-multiple-line (prompt pre-string exit-keyseq clear-keyseq)
  "Read multiline from minibuffer.
PROMPT with PRE-STRING binds EXIT-KEYSEQ to submit binds
CLEAR-KEYSEQ to clear text."
  (let ((keymap (copy-keymap minibuffer-local-map))
        ;; enable evil in minibuffer
        ;; https://github.com/emacs-evil/evil/pull/1059
        (evil-want-minibuffer t))
    (define-key keymap (kbd "RET") 'newline)
    (define-key keymap exit-keyseq 'exit-minibuffer)
    (define-key keymap clear-keyseq
      (lambda () (interactive) (delete-region (minibuffer-prompt-end) (point-max))))
    (read-from-minibuffer prompt pre-string keymap)))

(defun virtual-comment--read-string (prompt &optional pre-string)
  "Prompt for multiline string and return it.
PROMPT is show in multiline, PRE-STRING is string added to the
prompt"
  (virtual-comment--read-string-with-multiple-line
   (concat prompt " C-s to submit, C-g to cancel, C-c C-k to clear:\n")
   pre-string
   (kbd "C-s")
   (kbd "C-c C-k")))

(defun virtual-comment--overlayp (ov)
  "Predicate for OV being commnent."
  (overlay-get ov 'virtual-comment))

(defun virtual-comment--get-overlay-at (point)
  "Return the overlay comment of this POINT."
  (seq-find #'virtual-comment--overlayp
            (overlays-in point point)))

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

(defun virtual-comment--repair-overlay-maybe (ov)
  "Re-align coment overlay OV if necessary."
  (save-excursion
    (goto-char (overlay-start ov))
    (unless (= (point) (point-at-bol))
      ;; repair
      (overlay-put ov
                   'before-string
                   (virtual-comment--make-comment-for-display
                    (overlay-get ov 'virtual-comment)
                    (current-indentation)))
      (move-overlay ov (point-at-bol) (point-at-bol)))))

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

(defun virtual-comment--insert-hook-handler (ov
                                             is-after-change
                                             begin
                                             end
                                             &optional pre-change)
  "Move overlay back to the front.
OV is overlay, IS-AFTER-CHANGE, BEGIN END and PRE-CHANGE are extra
params. If there is already a ov comment on the line, the moved
ov will be discarded and its comment will be added to the host
comment."
  ;; (message "yay: ov:%s begin:%s end:%s indentation:%s" ov begin end (current-indentation))
  (when is-after-change
    (let* ((point (point-at-bol))
           (comment (overlay-get ov 'virtual-comment))
           (comment-for-display (virtual-comment--make-comment-for-display
                                 comment
                                 (current-indentation))))
      (move-overlay ov point point)
      (overlay-put ov 'before-string comment-for-display)
      (overlay-put ov 'virtual-comment comment))))

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
    ;; decrease ref count
    (cl-decf (virtual-comment-project-count data))
    ;; persistence maybe
    (when (= 0 (virtual-comment-project-count data))
      (message "Persisting virtual comments...")
      (virtual-comment-dump-data)
      ;; remove project files from store
      (virtual-comment--remove-project))))

(defun virtual-comment-next ()
  "Go to next/below comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (point-at-bol)
                                                    (point-max)
                                                    #'next-overlay-change))
      (goto-char point)
    ;; (progn (goto-char point) (message "%s thanh" point))
    (message "No next comment found.")))

(defun virtual-comment-previous ()
  "Go to previous/above comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (point-at-bol)
                                                    (point-min)
                                                    #'previous-overlay-change))
      (goto-char point)
    ;; (progn (goto-char point) (message "%s thanh" point))
    (message "No previous comment found.")))

(defun virtual-comment--make-comment-for-display (comment indent)
  "Make a comment string with face and INDENT from COMMENT for display."
  (concat (make-string indent ?\s)
          (mapconcat
           (lambda (it)
             (propertize it 'face virtual-comment-face))
           (split-string comment "\n")
           (concat "\n" (make-string indent ?\s)))
          "\n"))

(defun virtual-comment--make (pair)
  "Make a comment at point from PAIR of (point . comment)."
  (let ((point (car pair))
        (comment (cdr pair)))
    (save-excursion
      (goto-char point)
      (let* ((indent (current-indentation))
             (org-comment (virtual-comment--get-comment-at point))
             (ov (if org-comment (virtual-comment--get-overlay-at point)
                   (make-overlay point point))))
        (overlay-put ov 'virtual-comment comment)
        (overlay-put ov
                     'before-string
                     (virtual-comment--make-comment-for-display
                      comment
                      indent))))))

(defun virtual-comment-make ()
  "Add or edit comment at current line."
  (interactive)
  (let* ((point (point-at-bol))
         (indent (current-indentation))
         (org-comment (virtual-comment--get-comment-at point))
         (comment (virtual-comment--read-string
                   "Insert comment:"
                   org-comment))
         ;; must get existing overlay when comment is non-nil
         (ov (if org-comment (virtual-comment--get-overlay-at point)
               (make-overlay point point))))
    (overlay-put ov 'virtual-comment comment)
    (overlay-put ov
                 'before-string
                 (virtual-comment--make-comment-for-display comment indent))

    ;; (unless org-comment
    ;;   (overlay-put ov 'insert-in-front-hooks '(virtual-comment--insert-hook-handler))
    ;; (overlay-put ov 'insert-behind-hooks '(virtual-comment--insert-hook-handler))
    ;; (overlay-put ov 'modification-hooks '(virtual-comment--insert-hook-handler))
    (virtual-comment--update-data-async-maybe)))

(defun virtual-comment--delete-comment-at (point)
  "Delete the comment at point POINT.
Find the overlay for this POINT and delete it. Update the store."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (setq virtual-comment-deleted-overlay ov)
    (delete-overlay ov)))

(defun virtual-comment-delete ()
  "Delete comments of this current line.
The comment then can be pasted with `virtual-comment-paste'."
  (interactive)
  (let ((point (point-at-bol)))
    (virtual-comment--delete-comment-at point))
  (virtual-comment--update-data-async-maybe))

(defun virtual-comment--paste-at (point indent)
  "Paste comment at POINT and with INDENT."
  (when virtual-comment-deleted-overlay
    (let ((comment-for-display (virtual-comment--make-comment-for-display
                                (overlay-get virtual-comment-deleted-overlay 'virtual-comment)
                                indent)))
      (overlay-put virtual-comment-deleted-overlay 'before-string comment-for-display)
      (move-overlay virtual-comment-deleted-overlay point point))))

(defun virtual-comment-paste ()
  "Paste comment."
  (interactive)
  (virtual-comment--paste-at (point-at-bol) (current-indentation))
  (virtual-comment--update-data-async-maybe))

(defun virtual-comment--clear ()
  "Clear all overlays in current buffer."
  (mapc #'delete-overlay
        (virtual-comment--get-buffer-overlays)))

(define-minor-mode virtual-comment-mode
  "This mode shows virtual commnents."
  :lighter "evc"
  :keymap (make-sparse-keymap)
  (if virtual-comment-mode
      (virtual-comment-mode-enable)
    (virtual-comment-mode-disable)))

(defun virtual-comment--update-data-async-maybe ()
  "Do update only when mode is active."
  (when virtual-comment-mode
    (virtual-comment--update-data-async)))

(defun virtual-comment--init ()
  "Get everything ready if necessary store, project and buffer.
This function should only run once when mode is active. That is
after `virtual-comment-mode' is enabled in buffer, if you
run (virtual-comment-mode) again this function won't do anything."
  ;; get project
  (unless virtual-comment--is-initialized
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
  "Run when `virtual-comment-mode' is on."
  (add-hook 'after-save-hook 'virtual-comment--update-data-async 0 t)
  ;; (add-hook 'before-revert-hook 'virtual-comment-clear 0 t)
  (add-hook 'kill-buffer-hook 'virtual-comment--kill-buffer-hook-handler 0 t)
  ;; (setq virtual-comment-buffer-data nil)
  (virtual-comment--init))

(defun virtual-comment-mode-disable ()
  "Run when `virtual-comment-mode' is off."
  (remove-hook 'after-save-hook 'virtual-comment--update-data-async t)
  ;; (remove-hook 'before-revert-hook 'virtual-comment-clear t)
  (remove-hook 'kill-buffer-hook 'virtual-comment--kill-buffer-hook-handler t)
  (virtual-comment--kill-buffer-hook-handler)
  (virtual-comment--clear)
  (kill-local-variable 'virtual-comment--is-initialized)
  (kill-local-variable 'virtual-comment--buffer-data)
  (kill-local-variable 'virtual-comment--project))

;; view job
(defun virtual-comment-go ()
  "Go to location of comment at point of view buffer."
  (interactive)
  (let ((active-point (get-text-property (point) 'virtual-comment-point))
        (file-name (get-text-property (point) 'virtual-comment-file)))
    (when (and active-point file-name)
      (find-file file-name)
      (goto-char active-point))))

(defvar virtual-comment-show-map
  ;; (setq virtual-comment-show-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'virtual-comment-go)
    map)
  "Keymap for show.")

(defun virtual-comment--print-comments (pair file-name root)
  "Print out comments.
from PAIR is (point . comment) for FILE-NAME of project
ROOT."
  ;; (message "%s" comments)
  (let ((full-path (concat root file-name))
        (point (car pair))
        (comment (cdr pair)))
    (insert (format "%s\n\n"
                    (propertize comment
                                ;; 'face 'highlight
                                ;; 'font-lock-face 'underline
                                'font-lock-face 'highlight
                                'virtual-comment-point point
                                'virtual-comment-file full-path
                                'keymap virtual-comment-show-map)))))

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

(defun virtual-comment-show ()
  "Show comments for this file and its project."
  (interactive)
  (let ((file-name (virtual-comment--get-buffer-file-name))
        (root (cdr (project-current))))
    (virtual-comment--show virtual-comment--project
                           root
                           (get-buffer-create
                            (format "*%s*" (if root root "default")))
                           file-name)))

(provide 'virtual-comment)
;;; virtual-comment.el ends here
