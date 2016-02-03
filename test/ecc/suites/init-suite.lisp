;;;; init-suite.lisp

(in-package :cl-ecc.test)



(define-test ecc-tests::point-init
    (:tags '(basic init point))
  (assert-typep 'Point (make-instance 'Point
                                      :x (ironclad:integer-to-octets 20)
                                      :y (ironclad:integer-to-octets 30)))
  (assert-typep 'Point (make-instance 'Point
                                      :x 20
                                      :y 30))
  (assert-typep 'Point (make-instance 'Point
                                      :x "aa"
                                      :y "a0"))
  (assert-typep 'Point (make-instance 'Point
                                      :x 12
                                      :y "a2"))
  (assert-typep 'Point (make-instance 'Point
                                      :x (ironclad:integer-to-octets 23)
                                      :y "a2"))
  (assert-error 'invalid-type-error (make-instance 'Point
                                                   :x 20
                                                   :y 30.5))
  (assert-error 'invalid-type-error (make-instance 'Point
                                                   :x 'dds
                                                   :y 'a))
  (assert-error 'type-error (make-instance 'Point
                                           :x "a2f"
                                           :y 32))
  (assert-error 'simple-error (make-instance 'Point
                                             :x "ba4j"
                                             :y 15))
  (assert-error 'unbound-slot (make-instance 'Point :x 34))
  (assert-error 'unbound-slot (make-instance 'Point :y 22)))


(define-test ecc-tests::curve-init
    (:tags '(basic init curve))
  (assert-typep 'Curve (make-instance 'Curve
                                      :a (ironclad:integer-to-octets 10)
                                      :b (ironclad:integer-to-octets 20)
                                      :p (ironclad:integer-to-octets 30)
                                      :g (make-instance 'Point
                                                        :x (ironclad:integer-to-octets 10)
                                                        :y (ironclad:integer-to-octets 20))
                                      :n (ironclad:integer-to-octets 40)
                                      :h (ironclad:integer-to-octets 1)))
  (assert-typep 'Curve (make-instance 'Curve
                                      :a 10 :b 20 :p 55 :g (make-instance 'Point
                                                                       :x 10
                                                                       :y 20) :n 40 :h 1))
  (assert-typep 'Curve (make-instance 'Curve
                                      :a "4a2465"
                                      :b "ff"
                                      :p "a0"
                                      :g (make-instance 'Point
                                                        :x "a2"
                                                        :y "b5")
                                      :n "76"
                                      :h "c4"))
  (assert-typep 'Curve (make-instance 'Curve
                                      :a 23
                                      :b "76"
                                      :p (ironclad:integer-to-octets 23)
                                      :g (make-instance 'Point :x 23 :y 65)
                                      :n 65
                                      :h 23))
  (assert-error 'invalid-type-error (make-instance 'Curve
                                                  :a 23
                                                  :b 2
                                                  :p 23
                                                  :g 25
                                                  :n 32
                                                  :h 1))
  (assert-error 'invalid-type-error (make-instance 'Curve
                                                   :a 23
                                                   :b 2
                                                   :p 23
                                                   :g (make-instance 'Point :x 34 :y 76)
                                                   :n 32.5
                                                   :h 1))
  (assert-error 'type-error (make-instance 'Curve
                                           :a 23
                                           :b 2
                                           :p "af3"
                                           :g (make-instance 'Point :x 34 :y 76)
                                           :n 32
                                           :h 1))
  (assert-error 'simple-error (make-instance 'Curve
                                           :a 23
                                           :b 2
                                           :p "4tshgbfg"
                                           :g (make-instance 'Point :x 34 :y 76)
                                           :n 32
                                           :h 1))

  (assert-error 'invalid-type-error (make-instance 'Curve
                                                   :a 10 :b 20 :p 30 :g 0 :n 40 :h 1))
  (assert-error 'unbound-slot (make-instance 'Curve
                                                   :b 20 :p 30 :g 0 :n 40 :h 1))
  (assert-error 'unbound-slot (make-instance 'Curve
                                             :a 10 :b 20 :p 30 :g 0 :h 1)))
