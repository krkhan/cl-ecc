(defpackage #:ecrypto
  (:use #:common-lisp)
  (:export
    #:invalid-operation-error
    #:invalid-type-error
    #:validate-accessor-types))

(defpackage #:mod
  (:use
    #:common-lisp
    #:ecrypto)
  (:export
    ; defined in mod.lisp
    #:add-mod
    #:sub-mod
    #:mul-mod
    #:inv-mod
    #:div-mod
    #:expt-mod
    #:sqrt-mod))

(defpackage #:ecc
  (:use
    #:common-lisp
    #:ecrypto
    #:mod)
  (:export
    ; defined in point.lisp
    #:Point
    #:point-equalp
    #:*inf-point*
    ; defined in curve.lisp
    #:Curve
    #:valid-curve-p
    #:point-on-curve-p
    #:at-x
    #:point-inverse
    #:add-points
    #:mul-point
    #:order-of-point
    ; defined in ecdh.lisp
    #:ecdh-gen-pub
    #:ecdh-gen-secret
    ; defined in elgamal.lisp
    #:ElGamalMessage
    #:elgamal-gen-pub
    #:elgamal-encrypt
    #:elgamal-decrypt
    ; defined in ecdsa.lisp
    #:ECDSASig
    #:ecdsa-gen-sig
    #:ecdsa-verify-sig
    #:sig-equalp
    ; defined in test-vectors.lisp
    #:*p17-gen*
    #:*p17-order*
    #:*p17-points*
    #:*p17-curve*
    #:*nistp192-gen*
    #:*nistp192-curve*
    #:*nistp192-mulpoints*
    #:*nistp192-ecdsa-tests*))
