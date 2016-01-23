;;;; common.lisp

(in-package #:cl-ecc)

(defmacro with-slots-to-integers ((&rest var) (&rest slots) Curve &body body)
  (assert (= (length var) (length slots)))
  `(let ,(iterate (for n in var)
                  (for i from 0)
                  (collect `(,n (write-value 'byte-array nil (ironclad:octets-to-integer
                                                              (,(nth i slots) ,Curve))))))
       ,@body))

(defun if-octet-in-list->integer (&rest args)
  (mapcar #'(lambda (x) (if (typep x 'octet-vector)
                            (ironclad:octets-to-integer x)
                            x))
          (first args)))

(defun if-octetlist->integer (&rest args)
  (iterate (for i in args)
           (when (typep i 'octet-vector)
             (collect (ironclad:integer-to-octets)))))
