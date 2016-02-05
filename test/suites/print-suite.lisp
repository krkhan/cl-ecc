;;;; print-suite.lisp

(in-package :cl-ecc.test)

;; (define-test ecc-tests::print-test
;;     (:tags '(internal basic print))
;;   (assert-false (print-object (make-instance 'Point :x 5 :y 10) t))
;;   (assert-false (print-object (make-instance 'Curve :a 5 :b 10 :p 4 :g (make-instance 'Point :x 6 :y 10) :n 10 :h 6) t))
;;   (assert-false (print-object (make-instance 'Key   :key 5) t))
;;   (assert-false (print-object (make-instance 'ElGamalMessage
;;                                              :x (ecdh-gen-pub *p17* (make-instance 'ECDH-Private-Key :key 10))
;;                                              :y (make-instance 'ElGamalCiphertext :x 45 :y 234)) t)))
