;;;; ecdsa-suite.lisp
(in-package :cl-ecc.test)


(defparameter *ecdsa-key-table* (cl-ecc::put-keys-in-hash-table
                                 (uiop:merge-pathnames*
                                   "test/keys/ECDSA-pairs.key" cl-ecc::*source-directory*)))

(define-test ecc-tests::ecdsa-key-gen-test
    (:tags '(external interface ecdsa keygen))
  (iterate (for (key value) in-hashtable *ecdsa-key-table*)
           (let ((calc-key (key (ecdsa-gen-pub *secp256k1* (make-instance 'ECDSA-Private-Key :key key)))))
             (assert-equal (integer-length value) (integer-length calc-key))
             (assert-equal value calc-key))))

(define-test ecc-tests::ber-decode
    (:tags '(external interface ecdsa asn.1))
  (let* ((privkey (ecdsa-gen-priv))
         (msg (make-instance 'ECDSA-Message-hash :hash 12345676543234567876543))
         (sig (ecdsa-gen-sig *secp256k1* msg privkey 1)))
    (assert-typep '(simple-array (unsigned-byte 8) (*))
                  (ecdsa-ber-encode sig))
    (assert-typep 'ECDSA-Signature
                  (ecdsa-ber-decode (ecdsa-ber-encode sig)))
    (assert-equality #'ecdsa-sig-equalp
                     (ecdsa-gen-sig *secp256k1* msg privkey 1)
                     (ecdsa-ber-decode (ecdsa-ber-encode sig)))))
