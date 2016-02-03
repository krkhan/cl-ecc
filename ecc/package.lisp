;;;; package.lisp

(defpackage #:cl-ecc
  (:use #:cl
        #:iterate
        #:alexandria)

  (:shadow :null)

  (:export
   ;;;; external (to be exported)

   ;; ecc
   :ecdh-gen-pub
   :ecdh-gen-secret
   :elgamal-encrypt
   :elgamal-decrypt
   :ecdsa-gen-sig
   :ecdsa-verify-sig
   :ecdsa-sig-equalp
   :ecdsa-gen-pub

   ;; reader functions
   :x :y :key :version-byte :r :s :a :b :p :g :n :h

   ;; reader macros
   :define-slot-to-octet-vector-parser
   :define-custom-octet-reader-functions
   :define-custom-composite-octet-reader-functions
   :define-slot-type-validator

   ;; classes
   :Key
   :Point
   :Curve
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

   ;; Types
   :octet-vector

   ;; Errors
   :invalid-operation-error
   :invalid-type-error))

  ;;  ;;;; internal (not to be exported)


  ;;  ;; math-mod
  ;;  :add-mod :sub-mod :mul-mod :inv-mod :div-mod :expt-mod :sqrt-mod

  ;;  ;; Curve and Point functions
  ;;  :point-equalp
  ;;  :valid-curve-p
  ;;  :point-on-curve-p
  ;;  :at-x
  ;;  :point-inverse
  ;;  :add-points
  ;;  :mul-point
  ;;  :order-of-point

  ;;  ;; Curve and Point classes

  ;;  :Curve
  ;;  :Curve-p17
  ;;  :Point-p17
  ;;  :Curve-secp256k1
  ;;  :Point-secp256k1
  ;;  :Curve-secp192r1
  ;;  :Point-secp192r1))
