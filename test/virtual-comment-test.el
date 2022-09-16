;;; virtual-comment-test.el --- Tests for virtual-comment
(ert-deftest test/hello-world ()
  "Hello world"
  (message "Hello world")
  (should t))

(condition-case err
    (unless (virtual-comment--persisted-data-p
             (with-temp-buffer
               (progn (insert-file-contents ".evc.test2.error")
                      (read (current-buffer)))))
      (user-error "crap"))
  (user-error
   (message "user-error"))
  (error
   (message "error %s" (car err))))

(virtual-comment-equal
 (virtual-comment--load-data-from-file ".evc")
 (virtual-comment--load-data-from-file ".evc"))

(virtual-comment--load-data-from-file ".evc.test2")

(virtual-comment-equal
 (virtual-comment--load-data-from-file ".evc")
 (virtual-comment--load-data-from-file ".evc.test"))

(hash-table-p (virtual-comment--load-data-from-file ".evc"))

(hash-table-p (virtual-comment--load-data-from-file ".evc.test"))

(hash-table-p (virtual-comment--load-data-from-file ".evc.test.err"))

(equal (virtual-comment-buffer-data-create :filename "huh" :comments '(crap crap2))
       (virtual-comment-buffer-data-create :filename "huh2" :comments '(crap crap2)))
(equal '(1 2) '(1 2))

(virtual-comment-equal
 (virtual-comment-unit-create :point 1 :comment "foo" :target "bar")
 (virtual-comment-unit-create :point 1 :comment "foo" :target "bar"))

(virtual-comment-equal
 `(,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar"))
 `(,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar")))

(virtual-comment-equal
 `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
   ,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar"))
 `(,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar")
 ,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")))

(virtual-comment-equal
 `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
   ,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar"))
 `(,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar")
 ,(virtual-comment-unit-create :point 10 :comment "foofoofoo" :target "bar")))

(virtual-comment-equal
 (virtual-comment-unit-create :point 1 :comment "foo" :target "bar")
 (virtual-comment-unit-create :point 1 :comment "foofoo" :target "bar"))

(eql "foo" "foo")

(virtual-comment-equal
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar")))
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar"))))

(virtual-comment-equal
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 1 :comment "foo" :target "bar")))
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 11 :comment "foo" :target "bar"))))

(virtual-comment-equal
 nil
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 11 :comment "foo" :target "bar"))))

(virtual-comment-equal
 (virtual-comment-buffer-data-create
  :filename "foo"
  :comments
  `(,(virtual-comment-unit-create :point 10 :comment "foofoo" :target "bar")
    ,(virtual-comment-unit-create :point 11 :comment "foo" :target "bar")))
 nil)

;; virtual-comment--project
;; virtual-comment--buffer-data
;;; virtual-comment-test.el ends here
