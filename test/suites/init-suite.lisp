;;;; init-suite.lisp

(in-package :cl-ecc.test)



(define-test ecc-tests::key-init
    (:tags '(basic init key))
  (assert-typep 'key (make-instance 'cl-ecc::Private-Key
                                    :key 123456765432)))

(define-test ecc-tests::point-init
    (:tags '(basic init point))

  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x 20
                                      :y 30))

  (assert-error 'unbound-slot (y (make-instance 'cl-ecc::Point :x 34)))
  (assert-error 'unbound-slot (x (make-instance 'cl-ecc::Point :y 22))))


(define-test ecc-tests::curve-init
    (:tags '(basic init curve))

  (assert-typep 'cl-ecc::Curve (make-instance 'cl-ecc::Curve
                                      :a 10 :b 20 :p 55 :g (make-instance 'cl-ecc::Point
                                                                       :x 10
                                                                       :y 20) :n 40 :h 1)))
