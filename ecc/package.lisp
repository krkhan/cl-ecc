;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl
        #:iterate)

  (:shadow :null)

  (:export
   ;;;; external (to be exported)

   ;; ecc
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

   ;; reader functions
   :get-slot
   :get-key

   ;; classes
   :Key
   :Private-Key
   :Public-Key
   :ECDH-Private-Key
   :ECDH-Public-Key
   :ECDH-Secret
   :ElGamalMessage
   :ElGamalPlaintext
   :ElGamalCiphertext
   :ECDSA-Signature
   :ECDSA-Message-Hash
   :ECDSA-Private-Key
   :ECDSA-Public-Key

   ;; curve variables
   :*p17*
   :*secp256k1*
   :*secp192r1*
   :*inf-point*

   ;;;; internal (not to be exported)

   ;; utils
   :octet-vector

   ;; math-mod
   :add-mod :sub-mod :mul-mod :inv-mod :div-mod :expt-mod :sqrt-mod

   ;; Curve and Point functions
   :point-equalp
   :valid-curve-p
   :point-on-curve-p
   :at-x
   :point-inverse
   :add-points
   :mul-point
   :order-of-point

  ;; Errors
   :invalid-operation-error
   :invalid-type-error

   ;; Curve and Point classes

   :Curve
   :Point
   :Curve-p17
   :Point-p17
   :Curve-secp256k1
   :Point-secp256k1
   :Curve-secp192r1
   :Point-secp192r1))
