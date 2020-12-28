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
;; Abbrevation
;; cmt: comment
;; ov: overlay

;;; Code:
(require 'cl-lib)
(require 'project)

;; (defvar-local virtual-comment-buffer-data nil
;;   "Buffer comments.
;; Currently is a list of (point . comment). But could be a hash table.")

(defvar virtual-comment-yanked-overlay nil
  "Ref of the overlay yanked.")

(defvar virtual-comment-global-file "~/.evc")

(defvar-local virtual-comment--buffer-data nil
  "Buffer comment data.")

(defvar virtual-comment--store nil
  "Global comment store.")

(defvar-local virtual-comment--project nil
  "Project comment store.")

(defcustom virtual-comment-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'virtual-comment)

(cl-defstruct (virtual-comment-buffer-data
               (:constructor virtual-comment-buffer-data-create)
               (:copier nil))
  filename comments)

(cl-defstruct (virtual-comment-store
               (:constructor virtual-comment-store-create)
               (:copier nil))
  "Global store of comments.
Slot global is default store list of `virtual-comment-buffer-data'.
Slot projects is list of `virtual-comment-project'."
  global projects)

(cl-defstruct (virtual-comment-project
               (:constructor virtual-comment-project-create)
               (:copier nil))
  "Project store of comments.
Slot files is list of `virtual-comment-buffer-data'
Slot count is reference count."
  files count)

(defun virtual-comment--get-store ()
  "Get comment store.
If nil create it and create a hash table to :projects slot."
  (unless virtual-comment--store
    (setq virtual-comment--store (virtual-comment-store-create
                                  :projects (make-hash-table :test 'equal))))
  virtual-comment--store)

(defun virtual-comment--get-project-in-store (project-id)
  "Get project data from store using PROJECT-ID as key."
  (let* ((store (virtual-comment--get-store))
         (projects (virtual-comment-store-projects store)))
    (if-let (prj (gethash project-id projects))
        prj
      (puthash project-id
               ;; (virtual-comment-project-create :count 0 :files (make-hash-table :test 'equal))
               (virtual-comment-project-create :count 0 :files nil)
               ;; nil
               projects))))

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

(defun virtual-comment--get-data ()
  "Get struct `virtual-comment--buffer-data' if nil then create it."
  (unless virtual-comment--buffer-data
    (setq virtual-comment--buffer-data (virtual-comment-buffer-data-create)))
  virtual-comment--buffer-data)

(defun virtual-comment--ovs-to-cmts (ovs)
  "Maps overlay OVS list to list of (point . comment)."
  (mapcar (lambda (ov)
            (cons (overlay-start ov) (overlay-get ov 'virtual-comment)))
          ovs))

(defun virtual-comment--update-data ()
  "Update."
  (let ((data (virtual-comment--get-data))
        (ovs (virtual-comment--get-buffer-overlays)))
    ;; update file name
    (setf (virtual-comment-buffer-data-filename data)
          (virtual-comment--get-buffer-file-name))
    (setf (virtual-comment-buffer-data-comments data)
          (virtual-comment--ovs-to-cmts ovs))))

(defun virtual-comment-get-evc-file ()
  "Return evc file path."
  (let ((root (cdr (project-current))))
    (if root
        (concat root ".evc")
      virtual-comment-global-file)))

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
      virtual-comment-global-file)))

;; experimenting
(defun virtual-comment--dump-data-to-file (data file)
  "Dump DATA to .evc FILE."
  (with-temp-file file
    (let ((standard-output (current-buffer)))
      (prin1 data))))

(defun virtual-comment-dump-data ()
  "Dump data."
  (let ((data (virtual-comment--get-data))
        (file (virtual-comment-get-evc-file)))
    (virtual-comment--dump-data-to-file data file)))

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

(defun virtual-comment--get-buffer-overlays ()
  "Get all overlay comment."
  (seq-filter #'virtual-comment--overlayp
              (overlays-in (point-min) (point-max))))

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

(defun virtual-comment-repair-overlays-maybe ()
  "Re-align overlays if necessary."
  (interactive)
  (mapc #'virtual-comment--repair-overlay-maybe
        (virtual-comment--get-buffer-overlays)))

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
  (message "yay: ov:%s begin:%s end:%s indentation:%s" ov begin end (current-indentation))
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

(defun virtual-comment-next ()
  "Go to next/below comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (point-at-bol)
                                                    (point-max)
                                                    #'next-overlay-change))
      ;; (goto-char point)
      (progn (goto-char point) (message "%s thanh" point))
    (message "No next comment found.")))

(defun virtual-comment-previous ()
  "Go to previous/above comment."
  (interactive)
  (if-let (point (virtual-comment--get-neighbor-cmt (point-at-bol)
                                                    (point-min)
                                                    #'previous-overlay-change))
      (progn (goto-char point) (message "%s thanh" point))
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

(defun virtual-comment-make ()
  "Add or edit comment at current line."
  (interactive)
  (let* ((point (point-at-bol))
         (indent (current-indentation))
         (org-comment (virtual-comment--get-comment-at point))
         (comment (virtual-comment--read-string
                   "Insert comment:"
                   org-comment))
         ;; must get existing overlay when comment is not nill
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
    ))

(defun virtual-comment--yank-comment-at (point)
  "Delete the comment at point POINT.
Find the overlay for this POINT and delete it. Update the store."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (setq virtual-comment-yanked-overlay ov)
    (delete-overlay ov)))

(defun virtual-comment-yank ()
  "Delete comments of this current line."
  (interactive)
  (let ((point (point-at-bol)))
    (virtual-comment--yank-comment-at point)))

(defun virtual-comment--paste-at (point indent)
  "Paste comment at POINT and with INDENT."
  (when virtual-comment-yanked-overlay
    (let ((comment-for-display (virtual-comment--make-comment-for-display
                                (overlay-get virtual-comment-yanked-overlay 'virtual-comment)
                                indent)))
      (overlay-put virtual-comment-yanked-overlay 'before-string comment-for-display)
      (move-overlay virtual-comment-yanked-overlay point point))))

(defun virtual-comment-paste ()
  "Paste comment."
  (interactive)
  (virtual-comment--paste-at (point-at-bol) (current-indentation)))

(defun virtual-comment-load-store-maybe ()
  "Get everything ready if necessary store, project and buffer."
  ;; get project
  (let* ((project-data (virtual-comment--get-project))
         (count (virtual-comment-project-count project-data)))
    ;; check if project-data needs initialization
    (when (= count 0)
      ;; not initialized yet. must update
      ;; first load project file
      (setf (virtual-comment-project-files project-data)
            (virtual-comment--load-data-from-file (virtual-comment--get-saved-file))))
    ;; increase ref counter
    (cl-incf (virtual-comment-project-count project-data))
    ;; get buffer data from project
    ))

(define-minor-mode virtual-comment-mode
  "This mode shows virtual commnents."
  :lighter "evc"
  :keymap (make-sparse-keymap)
  (if virtual-comment-mode
      (virtual-comment-mode-enable)
    (virtual-comment-mode-disable)))

(defun virtual-comment-mode-enable ()
  "Run when `virtual-comment-mode' is on."
  (add-hook 'after-save-hook 'virtual-comment-save-in-buffer 0 t)
  (add-hook 'before-revert-hook 'virtual-comment-clear 0 t)
  ;; (setq virtual-comment-buffer-data nil)
  (virtual-comment-load-store-maybe))

(defun virtual-comment-mode-disable ()
  "Run when `virtual-comment-mode' is off."
  (remove-hook 'after-save-hook 'virtual-comment-save-in-buffer t)
  (remove-hook 'before-revert-hook 'virtual-comment-clear t)
  ;; (virtual-comment-clear)
  (kill-local-variable 'virtial-comment-in-buffer))

(provide 'virtual-comment)
;;; virtual-comment.el ends here
