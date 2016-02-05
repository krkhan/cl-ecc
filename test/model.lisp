;;;; suite.lisp

(in-package :cl-ecc.test)

(defmacro make-curve-tests (name)
  (alexandria:with-gensyms (mulpoints test c priv pub msghash sig v k)

    (let ((ec (cl-ecc::conc-to-symbol '* name '*)))

      `(progn

         (define-test ,(cl-ecc::conc-to-symbol 'cl-ecc::valid-curve-p- name)
             (:tags '(internal ,name parameters))
           (assert-true (cl-ecc::valid-curve-p ,ec))
           (assert-true (cl-ecc::point-on-curve-p ,ec (cl-ecc::g ,ec)))
           (assert-true (cl-ecc::point-on-curve-p ,ec cl-ecc.test::*inf-point*)))

         (define-test ,(cl-ecc::conc-to-symbol 'point-multiply- name)
             (:tags '(internal ,name parameters))
           (let ((,mulpoints ,(cl-ecc.test::conc-sym-name name '-mulpoints)))
             (loop
                for ,k being the hash-keys in ,mulpoints using (hash-value ,v)
                do
                  (assert-true (cl-ecc::point-equalp (cl-ecc::mul-point ,ec (cl-ecc::g ,ec) ,k) ,v)))))


         (define-test ,(cl-ecc::conc-to-symbol 'ecdsa- name)
             (:tags '(internal ,name ecdsa parameters))
           (dolist (,test ,(cl-ecc::conc-to-symbol name '-ecdsa-tests))
             (let* ((,c ,ec)
                    (,priv (make-instance 'ECDSA-Private-key :key (gethash "d" ,test)))
                    (,pub (make-instance 'cl-ecc.test::ECDSA-Public-Key
                                         :x (gethash "pub-x" ,test)
                                         :y (gethash "pub-y" ,test)
                                         :version-byte 4))
                    (,msghash (make-instance 'ECDSA-Message-Hash :hash (gethash "msghash" ,test)))
                    (,k (gethash "k" ,test))
                    (,sig (make-instance 'cl-ecc.test::ECDSA-Signature
                                         :s (gethash "s" ,test)
                                         :r (gethash "r" ,test))))
               (assert-true (cl-ecc::point-equalp ,pub (ecdsa-gen-pub ,c ,priv)))
               (assert-true (ecdsa-sig-equalp ,sig (ecdsa-gen-sig ,c ,msghash ,priv ,k)))
               (assert-true (ecdsa-verify-sig ,c ,msghash ,sig ,pub)))))))))

(defmacro define-curve-test-parameters (curve &key mulpoint1 mulpoint2 mulpoint3 mulpoint4 mulpoint5 mulpoint6
                                                     msghash1 d1 pub-x1 pub-y1 k1 r1 s1
                                                msghash2 d2 pub-x2 pub-y2 k2 r2 s2
                                                msghash3 d3 pub-x3 pub-y3 k3 r3 s3)
  (with-gensyms (entry test k v i)
    (let ((table (cl-ecc::conc-to-symbol curve '-mulpoints))
          (test-list (cl-ecc::conc-to-symbol  curve '-ecdsa-tests-list))
          (tests-name (cl-ecc::conc-to-symbol curve '-ecdsa-tests)))
      `(progn

         (defparameter ,table (make-hash-table))

         (setf (gethash ,(first mulpoint1) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split  (second mulpoint1)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint1)))))

         (setf (gethash ,(first mulpoint2) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split (second mulpoint2)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint2)))))
         (setf (gethash ,(first mulpoint3) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split (second mulpoint3)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint3)))))
         (setf (gethash ,(first mulpoint4) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split (second mulpoint4)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint4)))))
         (setf (gethash ,(first mulpoint5) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split (second mulpoint5)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint5)))))
         (setf (gethash ,(first mulpoint6) ,table)
               (make-instance 'cl-ecc::Point
                              :x ,(first (cl-ecc::integer-as-sequence-split (second mulpoint6)))
                              :y ,(second (cl-ecc::integer-as-sequence-split (second mulpoint6)))))

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

         (dolist (,test ,test-list)
           (let ((,entry (make-hash-table :test 'equal))
                 (,k 0)
                 (,v 0))
             (do ((,i 0 (+ ,i 2)))
                 ((>= ,i (length ,test)))
               (setf ,k (nth ,i ,test))
               (setf ,v (nth (1+ ,i) ,test))
               (setf (gethash ,k ,entry) ,v))
             (push ,entry ,tests-name)))

         (make-curve-tests ,curve)))))
