;;;; suite.lisp

(in-package :cl-ecc.test)

(defmacro make-curve-tests (name)
  (alexandria:with-gensyms (mulpoints test c priv pub msghash k sig)

    (let ((ec (conc-to-global-sym name)))

      `(progn

         (define-test ,(conc-sym-name 'valid-curve-p- name)
             (:tags '(internal ,name parameters))
           (assert-true (valid-curve-p ,ec))
           (assert-true (point-on-curve-p ,ec (get-slot :vector 'cl-ecc::g ,ec)))
           (assert-true (point-on-curve-p ,ec *inf-point*)))

         (define-test ,(conc-sym-name 'point-multiply name)
             (:tags '(internal ,name parameters))
           (let ((,mulpoints ,(conc-sym-name name '-mulpoints)))
             (loop
                for k being the hash-keys in mulpoints using (hash-value v)
                do
                  (assert-equal (point-equalp (mul-point ,ec (cl-ecc::g ,ec) k) v)))))

         (define-test ,(conc-sym-name 'ecdsa- name)
             (:tags '(internal ,name ecdsa parameters))
           (dolist (,test ,(conc-sym-name name '-ecdsa-tests))
             (let* ((,c ,ec)
                    (,priv (gethash "d" ,test))
                    (,pub (make-instance 'Point :x (gethash "pub-x" ,test)
                                        :y (gethash "pub-y" ,test)))
                    (,msghash (gethash "msghash" ,test))
                    (,k (gethash "k" test))
                    (,sig (make-instance 'ECDSASig :s (gethash "s" ,test)
                                        :r (gethash "r" ,test))))
               (assert-true (point-equalp pub (ecdh-gen-pub ,c ,priv)))
               (assert-true (sig-equalp sig (ecdsa-gen-sig ,c ,msghash ,priv ,k)))
               (assert-true (ecdsa-verify-sig ,c ,msghash ,sig ,pub)))))))))

(defmacro define-curve-test-parameters (curve-name &key mulpoint1 mulpoint2 mulpoint3 mulpoint4 mulpoint5 mulpoint6
                                                     msghash1 d1 pub-x1 pub-y1 k1 r1 s1
                                                     msghash2 d2 pub-x2 pub-y2 k2 r2 s2
                                                     msghash3 d3 pub-x3 pub-y3 k3 r3 s3)
  (with-gensyms (table test-list tests)
    (let ((table (intern (concatenate 'string (string 'ecc-tests::) (string curve-name) (string '-mulpoints))) )
          (test-list (intern (concatenate 'string (string 'ecc-tests::) (string curve-name) (string '-ecdsa-tests-list))) )
          (tests-name (intern (concatenate 'string (string 'ecc-tests::) (string curve-name) (string '-ecdsa-tests))) )
          (ec (conc-to-global-sym curve-name)))

      `(progn

         (defparameter ,table (make-hash-table))

         (setf (gethash ,(first mulpoint1) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint1)))))
         (setf (gethash ,(first mulpoint2) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint2)))))
         (setf (gethash ,(first mulpoint3) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint3)))))
         (setf (gethash ,(first mulpoint4) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint4)))))
         (setf (gethash ,(first mulpoint5) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint5)))))
         (setf (gethash ,(first mulpoint6) ,table)
               (read-value (type-of (cl-ecc::g ,ec)) (ironclad:make-octet-input-stream
                                                             (ironclad:hex-string-to-byte-array ,(second mulpoint6)))))

         (defparameter ,test-list
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

         (dolist (test ,test-list)
           (let ((entry (make-hash-table :test 'equal)) k v)
             (do ((i 0 (+ i 2)))
                 ((>= i (length test)))
               (setf k (nth i test))
               (setf v (nth (1+ i) test))
               (setf (gethash k entry) v))
             (push entry ,tests-name)))

         (make-curve-tests ,curve-name)))))
