;;;; common.lisp

(in-package #:cl-ecc-test.common)

(defparameter *tests-list* '())
(defparameter *passed-tests* '())

(defmacro def-positive-test (funcname args &body body)
  (let ((ex (gensym)))
    `(progn
       (push ',funcname *tests-list*)
       (defmethod ,funcname ,args
         (handler-case
           (progn
             ,@body
             (format t "--- ~a passed~%~%" ',funcname)
             (push ',funcname *passed-tests*))
           (error (,ex)
             (format t "!!! ~a failed: caught unexpected error: ~a~%~%"
                     ',funcname ,ex)))))))

(defmacro def-negative-test (funcname expected-error args &body body)
  (let ((ex (gensym)))
    `(progn
       (push ',funcname *tests-list*)
       (defmethod ,funcname ,args
         (handler-case
           (progn
             ,@body
             (format t "!!! ~a failed: did not catch expected error~%~%"
                     ',funcname))
           (,expected-error (,ex)
             (format t "--- ~a passed -- caught expected error: ~a~%~%"
                     ',funcname ,ex)
             (push ',funcname *passed-tests*)))))))

(defun run-tests ()
  (dolist (test (reverse *tests-list*))
    (funcall test)))


(defun conc-sym-name (prefix end)
  (let ((prefix-s (string prefix))
        (end-s (string end)))
    (intern (concatenate 'string (string prefix-s) (string end-s)))))

(defun conc-global-sym-name (prefix end)
  (let ((prefix-s (string prefix))
        (end-s (string end)))
    (intern (concatenate 'string "*" (string prefix-s) (string end-s) "*"))))

(defmacro make-curve-tests (curve)
  (let* ((curve-symbol (conc-global-sym-name curve '-curve))
         (gen-symbol (conc-global-sym-name curve '-gen)))
    `(progn
       (def-positive-test ,(conc-sym-name 'test-Curve-validity- curve) ()
         (valid-curve-p ,curve-symbol))

       (def-positive-test ,(conc-sym-name 'test-Point-on-Curve- curve) ()
         (let* ((c ,curve-symbol))
           (assert (point-on-curve-p ,curve-symbol ,gen-symbol))
           (assert (point-on-curve-p ,curve-symbol *inf-point*))))

       (def-positive-test ,(conc-sym-name 'test-Point-multiply- curve) ()
         (let ((mulpoints ,(conc-sym-name curve '-mulpoints)))
           (loop for k being the hash-keys in mulpoints using (hash-value v)
              do
                (assert (point-equalp (mul-point ,curve-symbol ,gen-symbol k) v)))))

       (def-positive-test ,(conc-sym-name 'test-ecdsa- curve) ()
         (dolist (test ,(conc-sym-name curve '-ecdsa-tests))
           (let* ((c ,curve-symbol)
                  (priv (gethash "d" test))
                  (pub (make-instance 'Point :x (gethash "pub-x" test)
                                             :y (gethash "pub-y" test)))
                  (msghash (gethash "msghash" test))
                  (k (gethash "k" test))
                  (sig (make-instance 'ECDSASig :s (gethash "s" test)
                                                :r (gethash "r" test))))
             (assert (point-equalp pub (ecdh-gen-pub c priv)))
             (assert (sig-equalp sig (ecdsa-gen-sig c msghash priv k)))
             (ecdsa-verify-sig c msghash sig pub)))))))
