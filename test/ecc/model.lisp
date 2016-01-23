;;;; model.lisp

(in-package #:cl-ecc.test)


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

(defmacro define-curve-test-parameters (curve-name &key mulpoint1 mulpoint2 mulpoint3 mulpoint4 mulpoint5 mulpoint6
                                                     msghash1 d1 pub-x1 pub-y1 k1 r1 s1
                                                     msghash2 d2 pub-x2 pub-y2 k2 r2 s2
                                                     msghash3 d3 pub-x3 pub-y3 k3 r3 s3)

  (let ((table-name (intern (concatenate 'string (string curve-name) (string '-mulpoints))) )
        (test-list-name (intern (concatenate 'string (string curve-name) (string '-ecdsa-tests-list))) )
        (tests-name (intern (concatenate 'string (string curve-name) (string '-ecdsa-tests))) )
        (point-class-name (conc-to-global-sym (intern (concatenate 'string (string 'Point-) (string curve-name))))))

    `(progn

       (defparameter ,table-name (make-hash-table))

       (setf (gethash ,(first mulpoint1) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint1)))))
       (setf (gethash ,(first mulpoint2) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint2)))))
       (setf (gethash ,(first mulpoint3) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint3)))))
       (setf (gethash ,(first mulpoint4) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint4)))))
       (setf (gethash ,(first mulpoint5) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint5)))))
       (setf (gethash ,(first mulpoint6) ,table-name)
             (read-value ,point-class-name (ironclad:make-octet-input-stream
                                            (ironclad:hex-string-to-byte-array ,(second mulpoint6)))))

       (defparameter ,test-list-name
         '(("msghash" ,msghash1
            "d" ,d1
            "pub-x" ,pub-x1
            "pub-y" ,pub-y1
            "k" ,k1
            "r" ,r1
            "s" ,s1)
           ("msghash" ,msghash2
            "d" ,d2
            "pub-x" ,pub-x2
            "pub-y" ,pub-y2
            "k" ,k2
            "r" ,r2
            "s" ,s2)
           ("msghash" ,msghash3
            "d" ,d3
            "pub-x" ,pub-x3
            "pub-y" ,pub-y3
            "k" ,k3
            "r" ,r3
            "s" ,s3)))


       (defparameter ,tests-name '())

       (dolist (test ,test-list-name)
         (let ((entry (make-hash-table :test 'equal)) k v)
           (do ((i 0 (+ i 2)))
               ((>= i (length test)))
             (setf k (nth i test))
             (setf v (nth (1+ i) test))
             (setf (gethash k entry) v))
           (push entry ,test-list-name)))

       (make-curve-tests ,curve-name))))
