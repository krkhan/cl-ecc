;;;; init-suite.lisp

(in-package :cl-ecc.test)



(define-test ecc-tests::key-init
    (:tags '(basic init key))
  (assert-typep 'key (make-instance 'cl-ecc::Private-Key
                                    :key 123456765432))
  (assert-typep 'key (make-instance 'cl-ecc::Private-Key
                                    :key "af7654"))
  (assert-typep 'key (make-instance 'cl-ecc::Private-Key
                                    :key (ironclad:integer-to-octets 2345675432))))

(define-test ecc-tests::point-init
    (:tags '(basic init point))
  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x (ironclad:integer-to-octets 20)
                                      :y (ironclad:integer-to-octets 30)))
  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x 20
                                      :y 30))
  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x "aa"
                                      :y "a0"))
  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x 12
                                      :y "a2"))
  (assert-typep 'cl-ecc::Point (make-instance 'cl-ecc::Point
                                      :x (ironclad:integer-to-octets 23)
                                      :y "a2"))

  (assert-error 'type-error (make-instance 'cl-ecc::Point
                                           :x "a2f"
                                           :y 32))
  (assert-error 'simple-error (make-instance 'cl-ecc::Point
                                             :x "ba4j"
                                             :y 15))
  (assert-error 'unbound-slot (y (make-instance 'cl-ecc::Point :x 34)))
  (assert-error 'unbound-slot (x (make-instance 'cl-ecc::Point :y 22))))


(define-test ecc-tests::curve-init
    (:tags '(basic init curve))
  (assert-typep 'cl-ecc::Curve (make-instance 'cl-ecc::Curve
                                      :a (ironclad:integer-to-octets 10)
                                      :b (ironclad:integer-to-octets 20)
                                      :p (ironclad:integer-to-octets 30)
                                      :g (make-instance 'cl-ecc::Point
                                                        :x (ironclad:integer-to-octets 10)
                                                        :y (ironclad:integer-to-octets 20))
                                      :n (ironclad:integer-to-octets 40)
                                      :h (ironclad:integer-to-octets 1)))
  (assert-typep 'cl-ecc::Curve (make-instance 'cl-ecc::Curve
                                      :a 10 :b 20 :p 55 :g (make-instance 'cl-ecc::Point
                                                                       :x 10
                                                                       :y 20) :n 40 :h 1))
  (assert-typep 'cl-ecc::Curve (make-instance 'cl-ecc::Curve
                                      :a "4a2465"
                                      :b "ff"
                                      :p "a0"
                                      :g (make-instance 'cl-ecc::Point
                                                        :x "a2"
                                                        :y "b5")
                                      :n "76"
                                      :h "c4"))
  (assert-typep 'cl-ecc::Curve (make-instance 'cl-ecc::Curve
                                      :a 23
                                      :b "76"
                                      :p (ironclad:integer-to-octets 23)
                                      :g (make-instance 'cl-ecc::Point :x 23 :y 65)
                                      :n 65
                                      :h 23))



  (assert-error 'simple-error (make-instance 'cl-ecc::Curve
                                           :a 23
                                           :b 2
                                           :p "4tshgbfg"
                                           :g (make-instance 'cl-ecc::Point :x 34 :y 76)
                                           :n 32
                                           :h 1)))
