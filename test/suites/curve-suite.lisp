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
  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point
                                                    :x 10
                                                    :y 10)
                                     (make-instance 'cl-ecc::Point
                                                    :x 10
                                                    :y 10)))
  (assert-false (cl-ecc::point-equalp (make-instance 'cl-ecc::Point
                                                     :x 10
                                                     :y 10)
                                      (make-instance 'cl-ecc::Point
                                                     :x 10
                                                     :y 9))))

(define-test ecc-tests::valid-curve-p
    (:tags '(internal basic curve p17 methods))
  (assert-error 'simple-error (cl-ecc::valid-curve-p
                               (make-instance 'cl-ecc::Curve
                                              :a 10
                                              :b 50
                                              :p 30
                                              :g (make-instance 'cl-ecc::Point
                                                                :x 10
                                                                :y 20)
                                              :n 40
                                              :h 1)))
  (assert-true (cl-ecc::valid-curve-p
                (make-instance 'cl-ecc::Curve
                               :a #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
                               :b #x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
                               :p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF
                               :g (make-instance 'cl-ecc::Point
                                                 :x #x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                                                 :y #x07192b95ffc8da78631011ed6b24cdd573f977a11e794811)
                               :n #xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831
                               :h #x01))))
(define-test ecc-tests::at-x
    (:tags '(internal basic curve p17 methods))
  (assert-equal '(t t t t t t t t t t t t t t t t t t) (let ((result '())
                                                             (ec *p17*))
                                                         (dolist (point p17-points result)
                                                           (multiple-value-bind (y yinv) (cl-ecc::at-x ec (first point))
                                                             (if (or (= y (second point)) (= yinv (second point)))
                                                                 (push 't result)
                                                                 (push 'nil result)))))))

(define-test ecc-tests::point-inverse
    (:tags '(internal basic curve p17 methods))
  (assert-true (cl-ecc::point-equalp
                   (make-instance 'cl-ecc::Point :x 5 :y 16)
                   (let* ((ec *p17*)
                          (p (make-instance 'cl-ecc::Point :x 5 :y 1)))
                     (cl-ecc::point-inverse ec p)))))

(define-test ecc-tests::add-points
    (:tags '(internal basic curve p17 methods))
  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point :x 5 :y 1)
                                     (cl-ecc::add-points *p17* (make-instance 'cl-ecc::Point :x 5 :y 1) *inf-point*)))
  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point :x 5 :y 1)
                                     (cl-ecc::add-points *p17* *inf-point* (make-instance 'cl-ecc::Point :x 5 :y 1))))

  (assert-true (cl-ecc::point-equalp *inf-point*
                                     (let ((p1 (make-instance 'cl-ecc::Point :x 5 :y 1)))
                                       (cl-ecc::add-points *p17* p1 (cl-ecc::point-inverse *p17* p1)))))

  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point :x 6 :y 3)
                                     (let ((p1 (make-instance 'cl-ecc::Point :x 5 :y 1))
                                           (p2 (make-instance 'cl-ecc::Point :x 5 :y 1)))
                                       (cl-ecc::add-points *p17* p1 p2))))

  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point :x 3 :y 1)
                                     (let ((p1 (make-instance 'cl-ecc::Point :x 10 :y 11))
                                           (p2 (make-instance 'cl-ecc::Point :x 0 :y 6)))
                                       (cl-ecc::add-points *p17* p1 p2))))
  (assert-true (cl-ecc::point-equalp (make-instance 'cl-ecc::Point :x 13 :y 10)
                                     (let ((p1 (make-instance 'cl-ecc::Point :x 9 :y 16))
                                           (p2 (make-instance 'cl-ecc::Point :x 16 :y 13)))
                                       (cl-ecc::add-points *p17* p1 p2))))

  (assert-true (let* ((ec *p17*)
                      (cur-point (cl-ecc::g ec)))
                 (dolist (ref-point-coords p17-points t)
                   (assert-true (cl-ecc::point-equalp cur-point (make-instance 'cl-ecc::Point
                                                                               :x (first ref-point-coords)
                                                                               :y (second ref-point-coords))))
                   (setf cur-point (cl-ecc::add-points ec (cl-ecc::g ec) cur-point))
                   (assert-true (cl-ecc::point-on-curve-p  ec cur-point))))))

(define-test ecc-tests::mul-point
    (:tags '(internal basic curve p17 methods))
  (let* ((g (cl-ecc::g *p17*))
         (p-result (cl-ecc::mul-point *p17* g 0)))
    (assert-true (cl-ecc::point-equalp p-result *inf-point*))

    (iterate (for i from 1 to 18)
             (let ((ref-point-coords (nth (1- i) p17-points))
                   (p-result (cl-ecc::mul-point *p17* g i)))
               (assert-true (cl-ecc::point-equalp
                                p-result
                                (make-instance 'cl-ecc::Point
                                               :x (first ref-point-coords)
                                               :y (second ref-point-coords))))))))

(define-test ecc-tests::order-of-point
    (:tags '(internal basic curve p17))
  (let* ((g (cl-ecc::g *p17*))
         (o (cl-ecc::order-of-point *p17* g)))
    (assert-equal o p17-order)
    (assert-true (cl-ecc::point-equalp *inf-point* (cl-ecc::mul-point *p17* g o)))))

(define-test ecc-tests::ecdh-p17
    (:tags '(internal basic curve ecdh p17 methods))
  (let* ((alice-priv (make-instance 'ECDH-Private-Key :key 3))
         (bob-priv (make-instance 'ECDH-Private-Key :key 12))
         (alice-pub (ecdh-gen-pub *p17* alice-priv))
         (bob-pub (ecdh-gen-pub *p17* bob-priv))
         (alice-secret (ecdh-gen-secret *p17* alice-priv bob-pub))
         (bob-secret (ecdh-gen-secret *p17* bob-priv alice-pub)))
    (assert-true (cl-ecc::point-equalp alice-secret (make-instance 'cl-ecc::Point :x 6 :y 14)))
    (assert-true (cl-ecc::point-equalp alice-secret bob-secret))))

(define-test ecc-tests::elgamal-p17
    (:tags '(internal basic curve elgamal p17))
  (let* ((bob-priv (make-instance 'ECDH-Private-Key :key 16))
         (bob-pub (ecdh-gen-pub *p17* bob-priv))
         (plaintext (make-instance 'ElGamalPlaintext :x 3 :y (cl-ecc::at-x *p17* 3)))
         (alice-ephemeral (make-instance 'ECDH-Private-Key :key 2))
         (encrypted (elgamal-encrypt *p17* plaintext bob-pub alice-ephemeral))
         (decrypted (elgamal-decrypt *p17* encrypted bob-priv)))
    (assert-true (cl-ecc::point-equalp plaintext decrypted))))

(define-test ecc-tests::ecdsa-p17
    (:tags '(internal basic curve ecdsa p17))
  (let* ((bob-priv (make-instance 'ECDSA-Private-key :key 3))
         (bob-pub (ecdsa-gen-pub *p17* bob-priv))
         (msghash (make-instance 'ECDSA-Message-hash :hash 8))
         (k 7)
         (sig (ecdsa-gen-sig *p17* msghash bob-priv k)))
    (assert-true (cl-ecc::ecdsa-sig-equalp sig (make-instance 'ECDSA-Signature
                                                :r 0
                                                :s 12)))
    (assert-true (ecdsa-verify-sig *p17* msghash sig bob-pub))))
