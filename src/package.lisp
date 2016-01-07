;;;; package.lisp

(defpackage #:cl-ecc.common
  (:use :cl)
  (:export :invalid-operation-error
           :invalid-type-error
           :validate-accessor-types))

(defpackage #:cl-utils-clos
  (:use :cl)
  (:export :def-exporting-class))

(defpackage #:cl-ecc.math-mod
  (:use #:cl
        #:cl-ecc.common)
  (:export :add-mod
           :sub-mod
           :mul-mod
           :inv-mod
           :div-mod
           :expt-mod
           :sqrt-mod))

(defpackage #:cl-ecc.curve
  (:use #:cl
        #:cl-ecc.common
        #:cl-ecc.math-mod
        #:cl-utils-clos)
  (:export :Point
           :point-equalp
           :Curve
           :*inf-point*
           :valid-curve-p
           :point-on-curve-p
           :at-x
           :point-inverse
           :add-points
           :mul-point
           :order-of-point))

(defpackage #:cl-ecc.ecdh
  (:use #:cl
        #:cl-ecc.math-mod
        #:cl-ecc.curve)
  (:export
           :ecdh-gen-pub
           :ecdh-gen-secret))

(defpackage #:cl-ecc.elgamal
  (:use #:cl
        #:cl-ecc.curve
        #:cl-ecc.ecdh)
  (:export :ElGamalMessage
           :elgamal-encrypt
           :elgamal-decrypt))

(defpackage #:cl-ecc.ecdsa
  (:use #:cl
        #:cl-ecc.curve
        #:cl-ecc.math-mod)
  (:export :ECDSASig
           :ecdsa-gen-sig
           :ecdsa-verify-sig
           :sig-equalp))

(defpackage #:cl-ecc
  (:use #:cl
        #:cl-ecc.common
        #:cl-utils-clos
        #:cl-ecc.math-mod
        #:cl-ecc.curve
        #:cl-ecc.ecdh
        #:cl-ecc.elgamal
        #:cl-ecc.ecdsa))
