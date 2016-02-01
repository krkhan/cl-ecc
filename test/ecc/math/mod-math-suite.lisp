;;;; mod-math-suite.lisp


(in-package #:cl-ecc.test)

(define-test ecc-tests::add-mod
    (:tags '(mod-tests))
  (assert-eql 4  (add-mod 34 24 9))
  (assert-eql 10 (add-mod 34 -84 15))
  (assert-eql -1 (add-mod 3 -2 -2)))

(define-test ecc-tests::sub-mod
    (:tags '(mod-tests))
  (assert-eql 121 (sub-mod 675 3544 130 ))
  (assert-eql 27  (sub-mod 13 54 34))
  (assert-eql 0   (sub-mod 139 5 2)))

(define-test ecc-tests::mul-mod
    (:tags '(mod-tests))
  (assert-eql 13 (mul-mod 4 7 15))
  (assert-eql 2  (mul-mod 4 4 7))
  (assert-eql 0  (mul-mod 10 6 15)))

(define-test ecc-tests::inv-mod
    (:tags '(mod-tests))
  (assert-eql 4  (inv-mod 3 11))
  (assert-eql 5  (inv-mod 6 29))
  (assert-eql 10 (inv-mod 10 99)))

(define-test ecc-tests::div-mod
    (:tags '(mod-tests))
  (assert-eql 4 (div-mod 6 5 7))
  (assert-eql 4 (div-mod 5 3 7))
  (assert-eql 6 (div-mod 1550 25 8)))

(define-test ecc-tests::legendre-symbol
    (:tags '(mod-tests))
  (assert-eql 1  (cl-ecc::legendre-symbol 16 37))
  (assert-eql -1 (cl-ecc::legendre-symbol 24 41))
  (assert-eql 0  (cl-ecc::legendre-symbol 12 3)))

(define-test ecc-tests::sqrt-mod
    (:tags '(mod-tests))
  (assert-eql 19 (sqrt-mod 58 101))
  (assert-eql 5  (sqrt-mod 3 11))
  (assert-eql 87  (sqrt-mod 111 113))
  (assert-eql 162244492740221711333411667492080568
              (let* ((a 2)
                     (p 360027784083079948259017962255826129)
                     (sq (sqrt-mod a p)))))
  (assert-error 'simple-error (make-instance )))
