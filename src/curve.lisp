;;;; curve.lisp

(in-package #:cl-ecc)

;; Generics

(defgeneric point-equalp (pt1 pt2)
  (:documentation "Result: Predicate. Compares two points structures"))

(defgeneric valid-curve-p (curve)
  (:documentation "Result: Predicate. Test if ec curve is valid by params"))

(defgeneric point-on-curve-p (curve pt)
  (:documentation "Result: Predicate. Test if a given point is valid on
                   the given curve"))

(defgeneric at-x (curve x)
  (:documentation "NEED TO CHECK"))

(defgeneric point-inverse (curve pt)
  (:documentation "Result: a 'Point. Y coordinates inverted over x-axis"))

(defgeneric add-points (curve pt1 pt2)
  (:documentation "Result: a 'Point. Addition of 2 Points on a Curve"))

(defgeneric mul-point (curve pt int)
  (:documentation "Result: a 'Point. Multiplication of 2 Points on a Curve"))

(defgeneric order-of-point (curve pt)
  (:documentation "Return: integer. Order of a Point on a Curve"))

(defgeneric get-value (Accessor Class)
  (:documentation "Returns the integer value of class slot."))

(defgeneric point->int (pt)
  (:documentation "Return: integer. concatenate x and y coords of point into integer"))

;; Point classes and methods
(def-exporting-class Point ()
  ((x :accessor x :initarg :x :export t)
   (y :accessor y :initarg :y :export t)))
(def-exporting-class Curve ()
  ((a :accessor a :initarg :a :export t)
   (b :accessor b :initarg :b :export t)
   (p :accessor p :initarg :p :export t)
   (g :accessor g :initarg :g :export t)
   (n :accessor n :initarg :n :export t)
   (h :accessor h :initarg :h :export t)))

;; Infinity point
(defparameter *inf-point* (make-instance
                     'Point
                     :x (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
                     :y (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))

(defmethod print-object ((object Curve) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (a b p g n h) object
      (format stream "~&a: ~d~&b: ~d~&p: ~d~&g: ~d~&n: ~d~&h: ~d~&" a b p g n h))))

(defmethod print-object ((object Point) stream)
  (when (point-equalp object *inf-point*)
    (format stream "<Point At Infinity>"))
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "~&~tx: ~d~&~ty: ~d~&" x y))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hex-string-octet-length (num)
    (assert (= (mod (length num) 2) 0))
    (/ (length (format nil "~x" num)) 2)))

(define-binary-type octet-array (format bytes)
  (:reader (in)
           (cond
             ((and (not (eq bytes nil))
                   (eq format nil))
              (let ((seq (make-array (list bytes)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
                (alexandria:rotate seq (- (read-sequence seq in)))))

             ((and (eq bytes nil)
                   (eq format nil))
              (let ((seq (make-array 500
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0
                                     :adjustable t)))
                (adjust-array seq (read-sequence seq in))
                (setf seq (copy-array seq
                                      :element-type '(unsigned-byte 8)
                                      :adjustable nil
                                      :fill-pointer nil))))
             ((and (not (eq bytes nil))
                   (eq format 'integer))
              (let ((seq (make-array (list bytes)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
                (alexandria:rotate seq (- (read-sequence seq (ironclad:make-octet-input-stream
                                                              (ironclad:integer-to-octets in)))))))
             (t (error "Reader keys not specified correctly."))))

  (:writer (out seq)
           (cond
             ((and (eq bytes nil)
                   (eq format nil))
              (do ((counter 0 (1+ counter)))
                  ((= counter (length seq)) out)
                (write-byte (elt seq counter) out)))
             ((and (not (eq bytes nil))
                   (eq format nil))
              (do ((counter 0 (1+ counter)))
                  ((= counter bytes) out)
                (write-byte (elt seq counter) out)))
             ((and (eq bytes nil)
                   (eq format 'integer))
              (ironclad:octets-to-integer seq))
             ((and (not (eq bytes nil))
                   (eq format nil))
              (ironclad:octets-to-integer (subseq seq 0 bytes)))
             (t (error "Writer keys not specified correctly")))))

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
  (with-gensyms (seq)
    (let ((coord-type (intern (concatenate 'string (string 'Coordinate-) (string name))))
          (curve-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (pname (intern (concatenate 'string (string 'Point-) (string name)))))

      `(progn
         (define-binary-type ,coord-type (format)
           (:reader (in)
                    (cond
                      ((eq format nil)
                       (let ((coordinate-octet-length ,(/ (hex-string-octet-length par-g) 2)))
                         (read-value 'octet-array in :bytes coordinate-octet-length)))

                      ((eq format 'integer)
                       (let ((coordinate-octet-length ,(/ (hex-string-octet-length par-g) 2)))
                         (read-value 'octet-array in :format 'integer :bytes coordinate-octet-length)))))

           (:writer (out ,seq)
                    (let ((coordinate-octet-length ,(/ (hex-string-octet-length par-g) 2)))
                      (write-value 'octet-array out ,seq
                                   :bytes coordinate-octet-length))))

          (define-binary-class ,pname (Point)
           ((x ,coord-type)
            (y ,coord-type)))

         (define-binary-class ,curve-name (Curve)
           ((a (octet-array :bytes ,(hex-string-octet-length par-a)))
            (b (octet-array :bytes ,(hex-string-octet-length par-b)))
            (p (octet-array :bytes ,(hex-string-octet-length par-p)))
            (g ,pname)
            (n (octet-array :bytes ,(hex-string-octet-length par-n)))
            (h (octet-array :bytes ,(hex-string-octet-length par-h)))))

         (defparameter ,sym
           (read-value ',curve-name
                       (ironclad:make-octet-input-stream
                        (ironclad:hex-string-to-byte-array
                         ,(concatenate 'string par-a par-b par-p par-g par-n par-h)))))))))


(defmethod get-value (accessor (ec Curve))
  (write-value 'octet-array nil (slot-value ec accessor) :format 'integer))



(defmethod point-equalp ((p1 Point) (p2 Point))
  (and
    (equalp (x p1) (x p2))
    (equalp (y p1) (y p2))))

(defmacro with-slots-to-integers ((&rest var) (&rest slots) Curve &body body)
  (assert (= (length var) (length slots)))
  `(let ,(iterate (for n in var)
                  (for i from 0)
                  (collect `(,n (write-value 'octet-array nil (,(nth i slots) ,Curve) :format 'integer))))
       ,@body))


(defmethod valid-curve-p ((ec Curve))
  (with-slots-to-integers (ec-a ec-b ec-p) (a b p) ec
    (assert (and
             (<= 0 ec-a)
             (< ec-a ec-p)
             (< 0 ec-b)
             (< ec-b ec-p)
             (> ec-p 2)))
    (assert (not
             (= 0 (add-mod (mul-mod 4 (expt-mod ec-a 3 ec-p) ec-p)
                           (mul-mod 27 (expt-mod ec-b 2 ec-p) ec-p)
                           ec-p))))))

(defmethod point-on-curve-p ((ec Curve) (pt Point))
  (with-slots-to-integers (ec-a ec-b ec-p) (a b p) ec
    (with-slots-to-integers (pt-x pt-y) (x y) pt
      (when (point-equalp pt *inf-point*)
        (return-from point-on-curve-p t))
      (let ((left (expt-mod pt-y 2 ec-p))
            (right (add-mod (expt-mod pt-x 3 ec-p)
                            (mul-mod ec-a pt-x ec-p)
                            ec-b
                            ec-p)))
        (equalp left right)))))

(defmethod at-x ((ec Curve) (x integer))
  (with-slots-to-integers (a-val b-val p-val) (a b p) ec
      (assert (< x p-val))
      (let* ((ysq (add-mod (expt-mod x 3 p-val) (mul-mod a-val x p-val) b-val p-val))
            (y (sqrt-mod ysq p-val)))
      (values y (- p-val y)))))

(defmethod point-inverse ((ec Curve) (pt Point))
  (with-slots-to-integers (p-val) (p) ec
    (with-slots-to-integers (y-val) (y) pt
      (make-instance (type-of pt)
                     :x (x pt)
                     :y (read-value 'octet-array (mul-mod -1 y-val p-val)
                                    :format 'integer
                                    :bytes (length (y pt)))))))

(defmethod add-points ((ec Curve) (p1 Point) (p2 Point))
  (with-slots-to-integers (ec-a ec-p) (a p) ec
    (with-slots-to-integers (p1-x p1-y) (x y) p1
      (with-slots-to-integers (p2-x p2-y) (x y) p2
        (when (point-equalp p1 *inf-point*)
          (return-from add-points p2))
        (when (point-equalp p2 *inf-point*)
          (return-from add-points p1))
        (when (and (= p1-x p2-x)
                   (or (not (= p1-y p2-y))
                       (= p1-y 0)))
          (return-from add-points *inf-point*))

        (let (s result-x result-y)
          (if (= p1-x p2-x)
              (setf s (mul-mod (add-mod (mul-mod 3 p1-x p1-x ec-p)
                                        ec-a
                                        ec-p)
                               (inv-mod (mul-mod 2 p1-y ec-p)
                                        ec-p)
                               ec-p))
              (setf s (mul-mod (sub-mod p2-y p1-y ec-p)
                               (inv-mod (sub-mod p2-x p1-x ec-p)
                                        ec-p)
                               ec-p)))
          (setf result-x (sub-mod (mul-mod s s ec-p)
                                  p1-x
                                  p2-x
                                  ec-p))
          (setf result-y (sub-mod (mul-mod s
                                           (sub-mod p1-x result-x ec-p)
                                           ec-p)
                                  p1-y
                                  ec-p))
          (make-instance (type-of p1)
                         :x (read-value 'octet-array result-x
                                        :format 'integer
                                        :bytes (length (x p1)))
                         :y (read-value 'octet-array result-y
                                        :format 'integer
                                        :bytes (length (y p1)))))))))

(defmethod mul-point ((c Curve) (pt Point) (d integer))
  (when (= 0 d)
    (return-from mul-point *inf-point*))

  (loop with result = pt
     for i from (- (integer-length d) 2) downto 0
     do
       (setf result (add-points c result result))
       (when (logbitp i d)
         (setf result (add-points c result pt)))
     finally
       (return-from mul-point result)))

(defmethod order-of-point ((c Curve) (pt Point))
  (loop for i from 1 and
        result = (mul-point c pt i)
        until (point-equalp result *inf-point*)
     finally (return-from order-of-point (1- i))))

(defmethod point->int ((pt Point))
  (ironclad:octets-to-integer (concatenate '(vector (unsigned-byte 8)) (x pt) (y pt))))



(defun test-parse-point ()
  (let ((s (ironclad:make-octet-input-stream
            (ironclad:hex-string-to-byte-array
             "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"))))
    (read-value 'Point-secp256k1 s)))


(defun test-parse-curve ()
  (let ((s (ironclad:make-octet-input-stream
            (ironclad:hex-string-to-byte-array
             "0007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD036414101"))))
    (read-value 'Curve-secp256k1 s)))
