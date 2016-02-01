;;;; math-mod-test.lisp

(in-package #:cl-ecc.test)

(def-positive-test test-mod ()
  (let* ((a 2)
        (p 360027784083079948259017962255826129)
        (sq (sqrt-mod a p)))
    (assert (= sq 162244492740221711333411667492080568))))

(def-negative-test test-mod-error cl-ecc::invalid-operation-error ()
  (let* ((a 2)
        (p 360027784083079948259017962255826120)
        (sq (sqrt-mod a p)))))
;; Curve tests

;; (define-test test-point-equalp
;;     (:tags '(curve-tests))
;;   (assert-eql t (point-equalp (make-instance 'Point
;;                                              :x (make-array 10 :element-type 'simple-octet-vector)
;;                                              :y (make-array 10 :element-type 'simple-octet-vector))
;;                               (make-instance 'Point
;;                                              :x (make-array 10 :element-type 'simple-octet-vector)
;;                                              :y (make-array 10 :element-type 'simple-octet-vector))))
;;   (assert-eql nil (point-equalp (make-instance 'Point
;;                                                :x (make-array 10 :element-type 'simple-octet-vector)
;;                                                :y (make-array 10 :element-type 'simple-octet-vector))
;;                                 :y (make-array 9 :element-type 'simple-octet-vector))))
