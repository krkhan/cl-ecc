;;;; ecdsa-suite.lisp
(in-package :cl-ecc.test)


(defparameter *ecdsa-key-table* (cl-ecc::put-keys-in-hash-table
                                 (uiop:merge-pathnames*
                                   "test/keys/ECDSA-pairs.key" cl-ecc::*source-directory*)))

(define-test ecc-tests::ecdsa-key-gen-test
    (:tags ('external interface ecdsa keygen))
  (iterate (for (key value) in-hashtable *ecdsa-key-table*)
           (let ((calc-key (key (ecdsa-gen-pub *secp256k1* (make-instance 'ECDSA-Private-Key :key key)))))
             (assert-equal (integer-length value) (integer-length calc-key))
             (assert-equal value calc-key))))
