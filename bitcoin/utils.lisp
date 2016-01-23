;;;; utils.lisp

(in-package :cl-ecc)

(defun flatten-vectors (list &key (element-type '(vector (unsigned-byte 8))))
  (do ((count 0 (1+ count))
       (result))
      ((> count (- (length list) 1)) result)
    (let ((el1 (elt list count)))
      (setf result (concatenate element-type result el1)))))
