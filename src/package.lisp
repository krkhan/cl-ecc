;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl
        #:ironclad
        #:nibbles
        #:iterate
        #:helper-library
        #:com.gigamonkeys.macro-utilities)
  (:shadow :null)
  (:import-from #:com.gigamonkeys.binary-data
                :read-value
                :write-value
                :define-binary-type
                :define-binary-class)
  (:export :ecdh-gen-pub
           :ecdh-gen-secret
           :ElGamalMessage
           :elgamal-encrypt
           :elgamal-decrypt
           :ECDSASig
           :ecdsa-gen-sig
           :ecdsa-verify-sig
           :sig-equalp
           :ecdsa-gen-pub

           ;; curves
           :*p17*
           :*secp256k1*
           :*secp192r1*))
