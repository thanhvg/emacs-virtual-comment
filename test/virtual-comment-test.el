;;; virtual-comment-test.el --- Tests for virtual-comment
(ert-deftest test/hello-world ()
  "Hello world"
  (message "Hello world")
  (should t))

(condition-case err
    (unless (virtual-comment--persited-data-p
             (with-temp-buffer
               (progn (insert-file-contents ".evc")
                      (read (current-buffer)))))
      (user-error "crap"))
  (user-error
   (message "user-error"))
  (error
   (message "error %s" (car err))))

(hash-table-p (virtual-comment--load-data-from-file ".evc"))

(hash-table-p (virtual-comment--load-data-from-file ".evc.test"))

(hash-table-p (virtual-comment--load-data-from-file ".evc.test.err"))

(equal (virtual-comment-buffer-data-create :filename "huh" :comments '(crap crap2))
       (virtual-comment-buffer-data-create :filename "huh2" :comments '(crap crap2)))


;; virtual-comment--project
;; virtual-comment--buffer-data
;;; virtual-comment-test.el ends here
