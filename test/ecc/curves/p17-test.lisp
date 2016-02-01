;;;; p17-test.lisp

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


(def-positive-test test-Point-constructor ()
  (make-instance 'Point :x 20 :y 30))



(def-positive-test test-Curve-constructor ()
  (make-instance 'Curve :a 10 :b 20 :p 30 :g (make-instance 'Point
                                                            :x 10
                                                            :y 20) :n 40))

(def-negative-test test-Point-constructor-error invalid-type-error ()
  (make-instance 'Point :x 20 :y 30.5))

(def-negative-test test-Curve-constructor-error invalid-type-error ()
  (make-instance 'Curve :a 10 :b 20 :p 30 :g 0 :n 40))

(def-negative-test test-Curve-validity-error simple-error ()
  (valid-curve-p (make-instance 'Curve :a 10 :b 50 :p 30
                                :g (make-instance 'Point :x 10 :y 20) :n 40)))

(def-positive-test test-Point-equal ()
  (assert
    (point-equalp
      (make-instance 'Point :x 20 :y 30)
      (make-instance 'Point :x 20 :y 30)))
  (assert
    (not
      (point-equalp
        (make-instance 'Point :x 20 :y 30)
        (make-instance 'Point :x 30 :y 30)))))

(def-positive-test test-calculate-Points-on-Curve-p17 ()
  (dolist (point p17-points)
    (let ((c *p17-curve*))
      (multiple-value-bind (y yinv) (at-x c (first point))
        (assert (or (= y (second point)) (= yinv (second point))))))))

(def-positive-test test-Point-Inverse-p17 ()
  (let* ((c *p17-curve*)
        (p (make-instance 'Point :x 5 :y 1))
        (pinv (point-inverse c p)))
    (assert (point-equalp pinv (make-instance 'Point :x 5 :y 16)))))

(def-positive-test test-Point-Add-p17 ()
  (let ((c *p17-curve*)
        (p1 (make-instance 'Point :x 5 :y 1))
        (p2 *inf-point*))
    (assert (point-equalp (add-points c p1 p2) p1))
    (assert (point-equalp (add-points c p2 p1) p1))
    (assert (point-equalp (add-points c p1 (point-inverse c p1)) *inf-point*)))

  (let ((c *p17-curve*)
        (p1 (make-instance 'Point :x 5 :y 1))
        (p2 (make-instance 'Point :x 5 :y 1))
        (p-result (make-instance 'Point :x 6 :y 3)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let ((c *p17-curve*)
        (p1 (make-instance 'Point :x 9 :y 16))
        (p2 (make-instance 'Point :x 16 :y 13))
        (p-result (make-instance 'Point :x 13 :y 10)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let ((c *p17-curve*)
        (p1 (make-instance 'Point :x 10 :y 11))
        (p2 (make-instance 'Point :x 0 :y 6))
        (p-result (make-instance 'Point :x 3 :y 1)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let ((c *p17-curve*)
        (cur-point *p17-gen*))
    (dolist (ref-point-coords p17-points)
      (assert (point-equalp cur-point (make-instance
                                       'Point :x (first ref-point-coords)
                                              :y (second ref-point-coords))))
      (setf cur-point (add-points c *p17-gen* cur-point))
      (assert (point-on-curve-p c cur-point)))))

(def-positive-test test-Point-multiply-p17 ()
  (let* ((c *p17-curve*)
         (p-result (mul-point c *p17-gen* 0)))
    (assert (point-equalp p-result *inf-point*)))

  (loop for i from 1 to 18
     do
       (let* ((c *p17-curve*)
              (ref-point-coords (nth (1- i) p17-points))
              (p-result (mul-point c *p17-gen* i)))
         (assert (point-equalp
                  p-result
                  (make-instance 'Point :x (first ref-point-coords)
                                        :y (second ref-point-coords)))))))

(def-positive-test test-Point-order-p17 ()
  (let* ((c *p17-curve*)
        (o (order-of-point c *p17-gen*)))
    (assert (= o p17-order))
    (assert (point-equalp *inf-point* (mul-point c *p17-gen* o)))))

(def-positive-test test-ecdh-p17 ()
  (let* ((c *p17-curve*)
         (alice-priv 3)
         (bob-priv 12)
         (alice-pub (ecdh-gen-pub c alice-priv))
         (bob-pub (ecdh-gen-pub c bob-priv))
         (alice-secret (ecdh-gen-secret c alice-priv bob-pub))
         (bob-secret (ecdh-gen-secret c bob-priv alice-pub)))
    (assert (point-equalp alice-secret (make-instance 'Point :x 6 :y 14)))
    (assert (point-equalp alice-secret bob-secret))))

(def-positive-test test-elgamal-p17 ()
  (let* ((c *p17-curve*)
         (bob-priv 16)
         (bob-pub (ecdh-gen-pub c bob-priv))
         (plaintext (make-instance 'Point :x 3 :y (at-x c 3)))
         (alice-ephemeral 2)
         (encrypted (elgamal-encrypt c plaintext bob-pub alice-ephemeral))
         (decrypted (elgamal-decrypt c encrypted bob-priv)))
    (assert (point-equalp plaintext decrypted))))

(def-positive-test test-ecdsa-p17 ()
  (let* ((c *p17-curve*)
         (bob-priv 3)
         (bob-pub (ecdh-gen-pub c bob-priv))
         (msghash 8)
         (k 7)
         (sig (ecdsa-gen-sig c msghash bob-priv k)))
    (assert (sig-equalp sig (make-instance 'ECDSASig :r 0 :s 12)))
    (ecdsa-verify-sig c msghash sig bob-pub)))
