;;;; mod-math-suite.lisp


(in-package #:cl-ecc.test)

(define-test ecc-tests::add-mod
    (:tags '(mod-tests))
  (assert-eql 4  (cl-ecc::add-mod 34 24 9))
  (assert-eql 10 (cl-ecc::add-mod 34 -84 15))
  (assert-eql -1 (cl-ecc::add-mod 3 -2 -2))
  (assert-error 'arithmetic-error (cl-ecc::add-mod 0 0 0)))

(define-test ecc-tests::sub-mod
    (:tags '(mod-tests))
  (assert-eql 121 (cl-ecc::sub-mod 675 3544 130 ))
  (assert-eql 27  (cl-ecc::sub-mod 13 54 34))
  (assert-eql 0   (cl-ecc::sub-mod 139 5 2)))

(define-test ecc-tests::mul-mod
    (:tags '(mod-tests))
  (assert-eql 13 (cl-ecc::mul-mod 4 7 15))
  (assert-eql 2  (cl-ecc::mul-mod 4 4 7))
  (assert-eql 0  (cl-ecc::mul-mod 10 6 15)))

(define-test ecc-tests::inv-mod
    (:tags '(mod-tests))
  (assert-eql 4  (cl-ecc::inv-mod 3 11))
  (assert-eql 5  (cl-ecc::inv-mod 6 29))
  (assert-eql 10 (cl-ecc::inv-mod 10 99)))

(define-test ecc-tests::div-mod
    (:tags '(mod-tests))
  (assert-eql 4 (cl-ecc::div-mod 6 5 7))
  (assert-eql 4 (cl-ecc::div-mod 5 3 7))
  (assert-eql 6 (cl-ecc::div-mod 1550 25 8)))

(define-test ecc-tests::legendre-symbol
    (:tags '(mod-tests))
  (assert-eql 1  (cl-ecc::legendre-symbol 16 37))
  (assert-eql -1 (cl-ecc::legendre-symbol 24 41))
  (assert-eql 0  (cl-ecc::legendre-symbol 12 3)))

(define-test ecc-tests::sqrt-mod
    (:tags '(mod-tests))
  (assert-eql 19 (cl-ecc::sqrt-mod 58 101))
  (assert-eql 5  (cl-ecc::sqrt-mod 3 11))
  (assert-eql 87  (cl-ecc::sqrt-mod 111 113))
  (assert-eql 162244492740221711333411667492080568
              (let* ((a 2)
                     (p 360027784083079948259017962255826129)
                     (sq (cl-ecc::sqrt-mod a p)))
                sq))
  (assert-error 'invalid-operation-error (cl-ecc::sqrt-mod 45 6)))
