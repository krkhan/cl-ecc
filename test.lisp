;;;; test.lisp

(in-package #:cl-ecc)

;; non curve dependant tests

(def-positive-test test-mod ()
  (let* ((a 2)
        (p 360027784083079948259017962255826129)
        (sq (sqrt-mod a p)))
    (assert (= sq 162244492740221711333411667492080568))))

(def-negative-test test-mod-error invalid-operation-error ()
  (let* ((a 2)
        (p 360027784083079948259017962255826120)
        (sq (sqrt-mod a p)))))

(def-positive-test test-Point-constructor ()
  (make-instance 'Point :x 20 :y 30))

(def-negative-test test-Point-constructor-error invalid-type-error ()
  (make-instance 'Point :x 20 :y 30.5))

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

(def-positive-test test-Curve-constructor ()
  (make-instance 'Curve :a 10 :b 20 :p 30 :g (make-instance 'Point
                                                            :x 10
                                                            :y 20) :n 40))

(def-negative-test test-Curve-constructor-error invalid-type-error ()
  (make-instance 'Curve :a 10 :b 20 :p 30 :g 0 :n 40))



;; Curve dependant tests

(def-positive-test test-Curve-validity-nistp192 ()
  (valid-curve-p *nistp192-curve*))

(def-positive-test test-Curve-validity-secp256k1 ()
  (valid-curve-p *secp256k1-curve*))

(def-negative-test test-Curve-validity-error simple-error ()
  (valid-curve-p (make-instance 'Curve :a 10 :b 50 :p 30
                                :g (make-instance 'Point :x 10 :y 20) :n 40)))

(def-positive-test test-Point-on-Curve-nistp192-p ()
  (let* ((c *nistp192-curve*))
    (assert (point-on-curve-p c *nistp192-gen*))
    (assert (point-on-curve-p c *inf-point*))))

(def-positive-test test-Point-on-Curve-secp256k1-p ()
  (let* ((c *secp256k1-curve*))
    (assert (point-on-curve-p c *secp256k1-gen*))
    (assert (point-on-curve-p c *inf-point*))))

(def-positive-test test-calculate-Points-on-Curve-p17 ()
  (dolist (point *p17-points*)
    (let ((c *p17-curve*))
      (multiple-value-bind (y yinv) (at-x c (first point))
        (assert (or (= y (second point)) (= yinv (second point))))))))

(def-positive-test test-Point-Inverse-p17 ()
  (let* ((c *p17-curve*)
         (p (make-instance 'Point :x 5 :y 1))
         (pinv (point-inverse c p)))
    (assert (point-equalp pinv (make-instance 'Point :x 5 :y 16)))))

(def-positive-test test-Point-Add ()
  (let* ((c *p17-curve*)
         (p1 (make-instance 'Point :x 5 :y 1))
         (p2 *inf-point*))
    (assert (point-equalp (add-points c p1 p2) p1))
    (assert (point-equalp (add-points c p2 p1) p1))
    (assert (point-equalp (add-points c p1 (point-inverse c p1)) *inf-point*)))

  (let* ((c *p17-curve*)
         (p1 (make-instance 'Point :x 5 :y 1))
         (p2 (make-instance 'Point :x 5 :y 1))
         (p-result (make-instance 'Point :x 6 :y 3)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let* ((c *p17-curve*)
         (p1 (make-instance 'Point :x 9 :y 16))
         (p2 (make-instance 'Point :x 16 :y 13))
         (p-result (make-instance 'Point :x 13 :y 10)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let* ((c *p17-curve*)
         (p1 (make-instance 'Point :x 10 :y 11))
         (p2 (make-instance 'Point :x 0 :y 6))
         (p-result (make-instance 'Point :x 3 :y 1)))
    (assert (point-equalp (add-points c p1 p2) p-result)))

  (let* ((c *p17-curve*)
         (cur-point *p17-gen*))
    (dolist (ref-point-coords *p17-points*)
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
              (ref-point-coords (nth (1- i) *p17-points*))
              (p-result (mul-point c *p17-gen* i)))
         (assert (point-equalp
                  p-result
                  (make-instance 'Point :x (first ref-point-coords)
                                        :y (second ref-point-coords)))))))

(def-positive-test test-Point-multiply-nistp192 ()
    (loop for k being the hash-keys in *nistp192-mulpoints* using (hash-value v)
       do
         (assert (point-equalp (mul-point *nistp192-curve* *nistp192-gen* k) v))))

(def-positive-test test-Point-multiply-secp256k1 ()
    (loop for k being the hash-key in *secp256k1-mulpoints* using (hash-value v)
       do
         (assert (point-equalp (mul-point *secp256k1-curve* *secp256k1-gen* k) v))))

(def-positive-test test-Point-order ()
  (let* ((c *p17-curve*)
         (o (order-of-point c *p17-gen*)))
    (assert (= o *p17-order*))
    (assert (point-equalp *inf-point* (mul-point c *p17-gen* o)))))

(def-positive-test test-ecdh ()
  (let* ((c *p17-curve*)
         (alice-priv 3)
         (bob-priv 12)
         (alice-pub (ecdh-gen-pub c alice-priv))
         (bob-pub (ecdh-gen-pub c bob-priv))
         (alice-secret (ecdh-gen-secret c alice-priv bob-pub))
         (bob-secret (ecdh-gen-secret c bob-priv alice-pub)))
    (assert (point-equalp alice-secret (make-instance 'Point :x 6 :y 14)))
    (assert (point-equalp alice-secret bob-secret))))

(def-positive-test test-elgamal ()
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

(def-positive-test test-ecdsa-nistp192 ()
  (dolist (test *nistp192-ecdsa-tests*)
    (let* ((c *nistp192-curve*)
           (priv (gethash "d" test))
           (pub (make-instance 'Point :x (gethash "pub-x" test)
                                      :y (gethash "pub-y" test)))
           (msghash (gethash "msghash" test))
           (k (gethash "k" test))
           (sig (make-instance 'ECDSASig :s (gethash "s" test)
                                         :r (gethash "r" test))))
      (assert (point-equalp pub (ecdh-gen-pub c priv)))
      (assert (sig-equalp sig (ecdsa-gen-sig c msghash priv k)))
      (ecdsa-verify-sig c msghash sig pub))))

(def-positive-test test-ecdsa-secp256k1 ()
  (dolist (test *secp256k1-ecdsa-tests*)
    (let* ((c *secp256k1-curve*)
           (priv (gethash "d" test))
           (pub (make-instance 'Point :x (gethash "pub-x" test)
                                      :y (gethash "pub-y" test)))
           (msghash (gethash "msghash" test))
           (k (gethash "k" test))
           (sig (make-instance 'ECDSASig :s (gethash "s" test)
                                         :r (gethash "r" test))))
      (assert (point-equalp pub (ecdh-gen-pub c priv)))
      (assert (sig-equalp sig (ecdsa-gen-sig c msghash priv k)))
      (ecdsa-verify-sig c msghash sig pub))))

(run-tests)

(format t "Tests passed: ~a/~a~%" (length *passed-tests*) (length *tests-list*))
