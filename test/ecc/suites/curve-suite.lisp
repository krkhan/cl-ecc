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
(define-test ecc-tests::point-equalp
    (:tags '(internal basic point p17 methods))
  (assert-true (point-equalp (make-instance 'Point
                                             :x (ironclad:integer-to-octets 10)
                                             :y (ironclad:integer-to-octets 10))
                              (make-instance 'Point
                                             :x (ironclad:integer-to-octets 10)
                                             :y (ironclad:integer-to-octets 10))))
  (assert-true (point-equalp (make-instance 'Point
                                             :x 10
                                             :y (ironclad:integer-to-octets 10))
                              (make-instance 'Point
                                             :x (ironclad:integer-to-octets 10)
                                             :y "0a")))

  (assert-false (point-equalp (make-instance 'Point
                                               :x (ironclad:integer-to-octets 10)
                                               :y (ironclad:integer-to-octets 10))
                                (make-instance 'Point
                                               :x (make-array 10 :element-type '(unsigned-byte 8) :initial-element 1)
                                               :y (ironclad:integer-to-octets 9))))
  (assert-true (point-equalp (make-instance 'Point
                                             :x 10
                                             :y (ironclad:integer-to-octets 10))
                              (make-instance 'Point
                                             :x (ironclad:integer-to-octets 10)
                                             :y "0a"))))


(define-test ecc-tests::valid-curve-p
    (:tags '(internal basic curve p17 methods))
  (assert-error 'simple-error (valid-curve-p
                               (make-instance 'Curve
                                              :a (ironclad:integer-to-octets 10)
                                              :b (ironclad:integer-to-octets 50)
                                              :p (ironclad:integer-to-octets 30)
                                              :g (make-instance 'Point
                                                                :x (ironclad:integer-to-octets 10)
                                                                :y (ironclad:integer-to-octets 20))
                                              :n (ironclad:integer-to-octets 40)
                                              :h (ironclad:integer-to-octets 1))))
  (assert-true (valid-curve-p
                (make-instance 'Curve
                               :a (ironclad:hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC")
                               :b (ironclad:hex-string-to-byte-array "64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1")
                               :p (ironclad:hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF")
                               :g (make-instance 'Point
                                                 :x (ironclad:hex-string-to-byte-array "188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012")
                                                 :y (ironclad:hex-string-to-byte-array "07192b95ffc8da78631011ed6b24cdd573f977a11e794811"))
                               :n (ironclad:hex-string-to-byte-array "FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831")
                               :h (ironclad:hex-string-to-byte-array "01")))))
(define-test ecc-tests::at-x
    (:tags '(internal basic curve p17 methods))
  (assert-equal '(t t t t t t t t t t t t t t t t t t) (let ((result '())
                                                             (ec *p17*))
                                                         (dolist (point p17-points result)
                                                           (multiple-value-bind (y yinv) (at-x ec (first point))
                                                             (if (or (= y (second point)) (= yinv (second point)))
                                                                 (push 't result)
                                                                 (push 'nil result)))))))

(define-test ecc-tests::point-inverse
    (:tags '(internal basic curve p17 methods))
  (assert-equality #'point-equalp
                   (make-instance 'Point :x 5 :y 16)
                   (let* ((ec *p17*)
                          (p (make-instance 'Point :x 5 :y 1)))
                     (point-inverse ec p))))

(define-test ecc-tests::add-points
    (:tags '(internal basic curve p17 methods))
  (assert-equality #'point-equalp
                   (make-instance 'Point :x 5 :y 1)
                   (add-points *p17* (make-instance 'Point :x 5 :y 1) *inf-point*)
                   (add-points *p17* *inf-point* (make-instance 'Point :x 5 :y 1)))

  (assert-equality #'point-equalp
                   *inf-point*
                   (let ((p1 (make-instance 'Point :x 5 :y 1)))
                     (add-points *p17* p1 (point-inverse *p17* p1))))

  (assert-equality #'point-equalp
                   (make-instance 'Point :x 6 :y 3)
                   (let ((p1 (make-instance 'Point :x 5 :y 1))
                         (p2 (make-instance 'Point :x 5 :y 1)))
                     (add-points *p17* p1 p2)))

  (assert-equality #'point-equalp
                   (make-instance 'Point :x 3 :y 1)
                   (let ((p1 (make-instance 'Point :x 10 :y 11))
                         (p2 (make-instance 'Point :x 0 :y 6)))
                     (add-points *p17* p1 p2)))
  (assert-equality #'point-equalp
                   (make-instance 'Point :x 13 :y 10)
                   (let ((p1 (make-instance 'Point :x 9 :y 16))
                         (p2 (make-instance 'Point :x 16 :y 13)))
                     (add-points *p17* p1 p2)))

  (assert-true (let* ((ec *p17*)
                      (cur-point (get-slot :point 'cl-ecc::g ec)))
                 (dolist (ref-point-coords p17-points t)
                   (assert-equality #'point-equalp cur-point (make-instance 'Point
                                                                            :x (first ref-point-coords)
                                                                            :y (second ref-point-coords)))
                   (setf cur-point (add-points ec (get-slot :point 'cl-ecc::g ec) cur-point))
                   (assert-equality #'point-on-curve-p cur-point ec)))))

(define-test ecc-tests::mul-point
    (:tags '(internal basic curve p17 methods))
  (let* ((g (get-slot :point 'cl-ecc::g *p17*))
         (p-result (mul-point *p17* g 0)))
    (assert-equality #'point-equalp p-result *inf-point*)

    (iterate (for i from 1 to 18)
             (let ((ref-point-coords (nth (1- i) p17-points))
                   (p-result (mul-point *p17* g i)))
               (assert-equality #'point-equalp
                                p-result
                                (make-instance 'Point
                                               :x (first ref-point-coords)
                                               :y (second ref-point-coords)))))))

(define-test ecc-tests::order-of-point
    (:tags '(internal basic curve p17))
  (let* ((g (get-slot :point 'cl-ecc::g *p17*))
         (o (order-of-point *p17* g)))
    (assert-equal o p17-order)
    (assert-true (point-equalp *inf-point* (mul-point *p17* g o)))))

(define-test ecc-tests::ecdh-p17
    (:tags '(internal basic curve ecdh p17 methods))
  (let* ((alice-priv (make-instance 'ECDH-Private-Key :key 3))
         (bob-priv (make-instance 'ECDH-Private-Key :key 12))
         (alice-pub (ecdh-gen-pub *p17* alice-priv))
         (bob-pub (ecdh-gen-pub *p17* bob-priv))
         (alice-secret (ecdh-gen-secret *p17* alice-priv bob-pub))
         (bob-secret (ecdh-gen-secret *p17* bob-priv alice-pub)))
    (assert-equality #'point-equalp alice-secret (make-instance 'Point :x 6 :y 14))
    (assert-equality #'point-equalp alice-secret bob-secret)))

(define-test ecc-tests::elgamal-p17
    (:tags '(internal basic curve elgamal p17))
  (let* ((bob-priv (make-instance 'ECDH-Private-Key :key 16))
         (bob-pub (ecdh-gen-pub *p17* bob-priv))
         (plaintext (make-instance 'ElGamalPlaintext :x 3 :y (at-x *p17* 3)))
         (alice-ephemeral (make-instance 'ECDH-Private-Key :key 2))
         (encrypted (elgamal-encrypt *p17* plaintext bob-pub alice-ephemeral))
         (decrypted (elgamal-decrypt *p17* encrypted bob-priv)))
    (assert-equality #'point-equalp plaintext decrypted)))

(define-test ecc-tests::ecdsa-p17
    (:tags '(internal basic curve ecdsa p17))
  (let* ((bob-priv (make-instance 'ECDSA-Private-key :key 3))
         (bob-pub (ecdsa-gen-pub *p17* bob-priv))
         (msghash (make-instance 'ECDSA-Message-hash :hash 8))
         (k 7)
         (sig (ecdsa-gen-sig *p17* msghash bob-priv k)))
    (assert-equality #'sig-equalp sig (make-instance 'ECDSA-Signature
                                                     :r (ironclad:integer-to-octets 0)
                                                     :s (ironclad:integer-to-octets 12)))
    (assert-true (ecdsa-verify-sig *p17* msghash sig bob-pub))))
