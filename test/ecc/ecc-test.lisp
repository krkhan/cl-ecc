;;;; cl-ecc-test.lisp

(in-package #:cl-ecc.test)

(defun run-tests ()
  (dolist (test (reverse *tests-list*))
    (funcall test)))

(run-tests)

(format t "Tests passed: ~a/~a~%" (length *passed-tests*) (length *tests-list*))
