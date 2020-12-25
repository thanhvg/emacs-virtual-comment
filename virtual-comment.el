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

(defvar-local virtual-comment-project-data nil
  "Project comments.")

;; (defvar-local virtual-comment-buffer-data nil
;;   "Buffer comments.
;; Currently is a list of (point . comment). But could be a hash table.")

(defvar virtual-comment-yanked-overlay nil
  "Ref of the overlay yanked.")

(defvar-local virtual-comment-buffer-overlays nil
  "Buffer overlay comments.
Currently is a list of overlays. Should be sorted.")

(cl-defstruct (virtual-comment-buffer-data
               (:constructor virtual-comment-buffer-data-create)
               (:copier nil))
  filename comments)

(defun virtual-comment-buffer-overlays--add (ov my-list)
  "MY-LIST has at least one element and its head is smaller than OV."
  (let ((start (overlay-start ov))
        (head (car my-list))
        (tail (cdr my-list)))
    (if (or (not tail)
            (<= start (overlay-start (car tail))))
        (setcdr my-list (cons ov tail))
      (virtual-comment-buffer-overlays--add ov tail))))

(defun virtual-comment-buffer-overlays-add (ov)
  "Add OV to `virtual-comment-buffer-overlays' in order."
  (if (or (not virtual-comment-buffer-overlays)
          (< (overlay-start ov) (overlay-start (car virtual-comment-buffer-overlays))))
      (push ov virtual-comment-buffer-overlays)
    (virtual-comment-buffer-overlays--add ov virtual-comment-buffer-overlays)))

(defun virtual-comment--get-data ()
  "Get struct `virtual-comment-buffer-overlays' if nil then create it."
  (unless virtual-comment-buffer-overlays
    (setq virtual-comment-buffer-overlays (virtual-comment-buffer-data-create)))
  virtual-comment-buffer-overlays)

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

(defvar virtual-comment-global-store nil
  "Global comment store.")

(defvar virtual-comment-global-file "~/.evc")

(defcustom virtual-comment-face 'highlight
  "Face for annotations."
  :type 'face
  :group 'virtual-comment)

(defun virtual-comment-get-project-path ()
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

(defun virtual-comment-read-data-from-file (file)
  (message "FIXME"))

(defun virtual-comment-init-global-store ()
  (setq virtual-comment-global-store
        (make-hash-table :test 'equal :weakness 'value)))

(defun virtual-comment-load-into-buffer ()
  "Load comments from store."
  )

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

(defun virtual-comment--repare-overlay-maybe (ov)
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
  (mapc #'virtual-comment--repare-overlay-maybe
        (virtual-comment--get-buffer-overlays)))

(defun virtual-comment--get-data-at (point)
  "Return data at POINT."
  (seq-find (lambda (it) (= point (car it)))
            virtual-comment-buffer-data))

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

(define-minor-mode virtual-comment-mode
  "FIXME."
  :lighter "evc"
  :keymap (make-sparse-keymap)
  (if virtual-comment-mode
      (virtual-comment-mode-enable)
    (virtual-comment-mode-disable)))

(defun virtual-comment-mode-enable ()
  (add-hook 'after-save-hook 'virtual-comment-save-in-buffer 0 t)
  (add-hook 'before-revert-hook 'virtual-comment-clear 0 t)
  (setq virtual-comment-buffer-data nil)
  (virtual-comment-load-into-buffer))

(defun virtual-comment-mode-disable ()
  (remove-hook 'after-save-hook 'virtual-comment-save-in-buffer t)
  (remove-hook 'before-revert-hook 'virtual-comment-clear t)
  ;; (virtual-comment-clear)
  (kill-local-variable 'virtial-comment-in-buffer))

(provide 'virtual-comment)
;;; virtual-comment.el ends here
