;;;; common.lisp

(in-package #:cl-ecc)

(defmacro with-slots-to-integers ((&rest var) (&rest slots) Curve &body body)
  (assert (= (length var) (length slots)))
  `(let ,(iterate (for n in var)
                  (for i from 0)
                  (collect `(,n (write-value 'byte-array nil (ironclad:octets-to-integer
                                                              (,(nth i slots) ,Curve))))))
       ,@body))
