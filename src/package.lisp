;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl)
  (:export :ecdh-gen-pub
           :ecdh-gen-secret
           :ElGamalMessage
           :elgamal-encrypt
           :elgamal-decrypt
           :ECDSASig
           :ecdsa-gen-sig
           :ecdsa-verify-sig
           :sig-equalp

           ;; curves
           :*p17*
           :*secp256k1*
           :*secp192r1*))
