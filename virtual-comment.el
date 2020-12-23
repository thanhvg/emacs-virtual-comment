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

;;; Code:
(require 'project)

(defvar-local virtual-comment-project-data nil
  "Project comments.")

(defvar-local virtual-comment-buffer-data nil
  "Buffer comments.
Currently is a list of (point . comment). But could be a hash table.")

(defvar-local virtual-comment-buffer-overlays nil
  "Buffer overlay comments.
Currently is a list of overlays. Should be sorted.")

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
      virtual-commnent-global-file)))

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

(defun virtual-comment--get-overlay-at (point)
  "Return the overlay comment of this POINT."
  (seq-find (lambda (it) (overlay-get it 'before-string))
            (overlays-in point point)))

(defun virtual-comment--get-data-at (point)
  "Return data at POINT."
  (seq-find (lambda (it) (= point (car it)))
            virtual-comment-buffer-data))

(defun virtual-comment--get-comment-at (point)
  "Return comment at POINT."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (overlay-get ov 'before-string)))

(defun virtual-comment--insert-hook-handler (ov is-before begin end &optional pre-change)
  "Move overlay back to the front"
  (message "yay: %s" ov)
  (let ((point (point-at-bol)))
    (move-overlay ov point point)))

(defun virtual-comment-make-comment-here ()
  "Add or edit comment at current line."
  (interactive)
  (let* ((point (point-at-bol))
         (org-comment (virtual-comment--get-comment-at point))
         (comment (virtual-comment--read-string
                   "Insert comment:"
                   org-comment))
         ;; must get existing overlay when comment is not nill
         (ov (if org-comment (virtual-comment--get-overlay-at point)
               (make-overlay point point nil t nil))))
    (overlay-put ov 'before-string (concat comment "\n"))
    (unless org-comment
      (overlay-put ov 'insert-in-front-hooks '(virtual-comment--insert-hook-handler))
      (push ov virtual-comment-buffer-overlays))))

(defun virtual-comment--delete-comment-at (point)
  "Delete the comment at point POINT.
Find the overlay for this POINT and delete it. Update the store."
  (when-let (ov (virtual-comment--get-overlay-at point))
    (delete-overlay ov)
    (delq ov virtual-comment-buffer-overlays)))

(defun virtual-comment-delete-comment-here ()
  "Delete comments of this current line."
  (let ((point (point-at-bol)))
    (virtual-comment--delete-comment-at point)))

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
