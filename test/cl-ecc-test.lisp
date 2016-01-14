;;;; cl-ecc-test.lisp

(in-package #:cl-ecc-test)

(run-tests)

(format t "Tests passed: ~a/~a~%" (length *passed-tests*) (length *tests-list*))
