;;;; curve-suite.lisp

(in-package #:cl-ecc.test)

(defvar p17-order 19)

(defvar p17-points '((5 1)
                     (6 3)
                     (10 6)
                     (3 1)
                     (9 16)
                     (16 13)
                     (0 6)
                     (13 7)
                     (7 6)
                     (7 11)
                     (13 10)
                     (0 11)
                     (16 4)
                     (9 1)
                     (3 16)
                     (10 11)
                     (6 14)
                     (5 16)))

;; Curve tests
(define-test ecc-tests::point-constructor
    (:tags '(curve-op-tests))
    (assert-typep  'Point (make-instance 'Point
                                  :x (octet-vector 20)
                                  :y (octet-vector 30))))

(define-test ecc-tests::curve-constructor
    (:tags '(curve-op-tests))
    (assert-typep 'Curve (make-instance 'Curve
                                        :a (integer-to-octets 10)
                                        :b (integer-to-octets 20)
                                        :p (integer-to-octets 30)
                                        :g (make-instance 'Point
                                                          :x (integer-to-octets 10)
                                                          :y (integer-to-octets 20))
                                        :n (integer-to-octets 40)
                                        :h (integer-to-octets 1))))
(define-test ecc-tests::point-equalp
    (:tags '(curve-op-tests))
  (assert-eql t (point-equalp (make-instance 'Point
                                             :x (octet-vector 10)
                                             :y (octet-vector 10))
                              (make-instance 'Point
                                             :x (octet-vector 10)
                                             :y (octet-vector 10))))
  (assert-eql nil (point-equalp (make-instance 'Point
                                               :x (octet-vector 10)
                                               :y (octet-vector 10))
                                (make-instance 'Point
                                               :x (make-array 10 :element-type '(unsigned-byte 8) :initial-element 1)
                                               :y (octet-vector 9)))))

(define-test ecc-tests::valid-curve-p
    (:tags '(curve-op-tests))
  (assert-true (valid-curve-p
                (make-instance 'Curve
                               :a (integer-to-octets 10)
                               :b (integer-to-octets 50)
                               :p (integer-to-octets 30)
                               :g (make-instance 'Point
                                                 :x (integer-to-octets 10)
                                                 :y (integer-to-octets 20))
                               :n (integer-to-octets 40)
                               :h (integer-to-octets 1))))
  (assert-true (valid-curve-p
                (make-instance 'Curve
                               :a (hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC")
                               :b (hex-string-to-byte-array "64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1")
                               :p (hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF")
                               :g (make-instance 'Point
                                                 :x (hex-string-to-byte-array "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012")
                                                 :y (hex-string-to-byte-array "07192b95ffc8da78631011ed6b24cdd573f977a11e794811"))
                               :n (hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831")
                               :h (hex-string-to-byte-array "01")))) ))
