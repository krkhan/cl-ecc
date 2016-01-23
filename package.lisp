;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl
        #:ironclad
        #:nibbles
        #:iterate
        #:com.gigamonkeys.macro-utilities)
  (:shadow :null)
  (:import-from #:com.gigamonkeys.binary-data
                :read-value
                :write-value
                :define-binary-type
                :define-binary-class)
  (:export
   ;; external functions
   :ecdh-gen-pub
   :ecdh-gen-secret
   :ElGamalMessage
   :elgamal-encrypt
   :elgamal-decrypt
   :ECDSASig
   :ecdsa-gen-sig
   :ecdsa-verify-sig
   :sig-equalp
   :ecdsa-gen-pub

   ;; internal functions

   ;; utils

   :make-byte-array

   ;; read/write
   :read-value
   :write-value

   ;; math-mod
   :add-mod
   :sub-mod
   :mul-mod
   :inv-mod
   :div-mod
   :expt-mod
   :sqrt-mod

   ;; Curve and Point functions
   :point-equalp
   :valid-curve-p
   :point-on-curve-p
   :at-x
   :point-inverse
   :add-point
   :mul-point
   :order-of-point
   :point->int

   ;; Errors
   :invalid-ecc-error
   :invalid-type-error


   ;; Classes and types
   :Curve
   :Point
   :*p17*
   :Curve-p17
   :Point-p17
   :*secp256k1*
   :Curve-secp256k1
   :Point-secp256k1
   :*secp192r1*
   :Curve-secp192r1
   :Point-secp192r1))
