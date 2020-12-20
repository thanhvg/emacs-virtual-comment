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


(require 'project)

(defvar-local virtual-comment-project-data nil
  "Project comments.")

(defvar-local virtual-comment-buffer-data nil
  "Buffer comments.

Currently is a list of (point . comment). But could be a hash table.")

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

(defun virtual-comment-make-comment-here ()
  (let ((point (point-at-bol)))
    (overlay-put (make-overlay point point nil t nil)
                 'before-string
                 "Yay commnent\n")
    (push (cons point "a comment") virtual-comment-buffer-data)))

(defun virtual-comment--delete-comment-here (point)
  "Delete the comment at current point.

Find the overlay for this line and delete it. Update the store."
  (when-let (found (seq-find (lambda (it) (= point (car it)))
                             virtual-comment-buffer-data))
    ;; (message "I found it")
    (dolist (o (overlays-in point point))
      (when (overlay-get o 'before-string)
        (delete-overlay o)))
    (setq virtual-comment-buffer-data
          (seq-remove (lambda (it) (= point (car it)))
                      virtual-comment-buffer-data))))

(defun virtual-comment-delete-comment-here ()
  "Delete comments of this current line."
  (let ((point (point-at-bol)))
    (virtual-comment--delete-comment-here point)))

(define-minor-mode virtual-comment-mode
  "FIXME."
  :lighter "evc"
  :keymap (make-sparse-keymap)//
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
