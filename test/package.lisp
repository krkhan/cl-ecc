;;;; package.lisp

(defpackage #:cl-ecc.test
  (:use #:cl
        #:cl-ecc
        #:iterate
        #:lisp-unit2)
  (:import-from #:alexandria :with-gensyms)
  (:export
   :define-curve-test-parameters
   :make-curve-tests

   ;; curves
   :*secp192r1*
   :*secp256k1*

   ;; types
   :octet-vector))

(defpackage #:ecc-tests
  (:use #:cl #:cl-ecc.test))
