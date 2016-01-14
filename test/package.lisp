;;;; package.lisp


(defpackage #:cl-ecc-test.common
  (:use #:cl
        #:cl-ecc.curve
        #:cl-ecc.ecdsa
        #:cl-ecc.ecdh
        #:cl-ecc.elgamal)
  (:export :*tests-list*
           :*passed-tests*
           :def-positive-test
           :def-negative-test
           :make-curve-tests
           :run-tests))

(defpackage #:cl-ecc-test.math-mod
  (:use #:cl
        #:cl-ecc.common
        #:cl-ecc.math-mod
        #:cl-ecc-test.common)
;;  (:import-from :cl-ecc.common :invalid-operation-error)
  )

(defpackage #:cl-ecc-test.curve-parameters
  (:use #:cl
        #:cl-ecc.common
        #:cl-ecc.math-mod
        #:cl-ecc.curve
        #:cl-ecc.ecdh
        #:cl-ecc.ecdsa
        #:cl-ecc.elgamal
        #:cl-ecc.curve-parameters
        #:cl-ecc-test.common))

(defpackage #:cl-ecc-test
  (:use #:cl
        #:cl-ecc
        #:cl-ecc-test.common))
