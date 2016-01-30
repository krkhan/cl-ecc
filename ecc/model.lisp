;;;; model.lisp

(in-package #:cl-ecc)

;; Generics

;; Model

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-class-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (point-class-name (intern (concatenate 'string (string 'Point-) (string name)))))
      `(progn
          (defclass ,point-class-name (Point))
          (defclass ,curve-class-name (Curve))

          (defvar ,sym
            (make-instance ,curve-class-name
                           :a (hex-string-to-byte-array par-a)
                           :b (hex-string-to-byte-array par-b)
                           :p (hex-string-to-byte-array par-p)
                           :g (make-instance ,point-class-name
                                             :x (hex-string-to-byte-array (subseq par-g 0 (/ (length par-g) 2)))
                                             :y (hex-string-to-byte-array (subseq par-g (/ (length par-g) 2))))
                           :n (hex-string-to-byte-array par-n)
                           :h (hex-string-to-byte-array par-h))))))


;; General Classes and reader functions

(defclass Point ()
  ((x :initarg :x
      :initform (error ":x must be initialized")
      :type '(octet-vector))
   (y :initarg :y
      :initform (error ":y must be initialized")
      :type 'octet-vector)))

(define-reader-functions get-x Point x)
(define-reader-functions get-y Point y)

(defclass Curve ()
  ((a :reader get-a
      :initarg :a
      :initform (error ":a must be initialized")
      :type 'octet-vector)
   (b :initarg :b
      :initform (error ":b must be initialized")
      :type 'octet-vector)
   (p :initarg :p
      :initform (error ":p must be initialized")
      :type 'octet-vector)
   (g :initarg :g
      :initform (error ":g must be initialized")
      :type 'Point)
   (n :initarg :n
      :initform (error ":n must be initialized")
      :type 'octet-vector)
   (h :initarg :h
      :initform (error ":h must be initialized")
      :type 'octet-vector)))

(define-reader-functions get-a Curve a)
(define-reader-functions get-b Curve b)
(define-reader-functions get-p Curve p)

(defmethod get-g :before (spec (ec Curve))
  (assert (typep (slot-value ec 'g) 'Point)))
(defmethod get-g ((ec Curve) (spec (eql :point)))
  (slot-value ec 'g))

(define-reader-functions get-n Curve n)
(define-reader-functions get-h Curve h)


(defvar *inf-point* (make-instance 'Point
                                   :x (octet-vector 0)
                                   :y (octet-vector 0))
  "Zero Point")



;; Helper macros

(defmacro define-reader-functions (name class slot)
  `(progn

     (defmethod ,name :before (spec (object ,class))
                `(assert (typep object ',slot) 'octet-vector))

     (defmethod ,name ((spec (eql :int)) (object ,class))
       (ironclad:integer-to-octets (slot-value object ',slot)))

     (defmethod ,name ((spec (eql :vector)) (object ,class) )
       (slot-value object ',slot))

     (defmethod ,name ((spec (eql :hex-string)) (object ,class))
       (ironclad:byte-array-to-hex-string (slot-value object ',slot)))))


;; Printing

(defmethod print-object ((pt Point) stream)
  (when (point-equalp pt *inf-point*)
    (format out "<Point At Infinity>")
    (return-from print-object))
  (print-unreadable-object (pt stream :type t)
    (format out "~&~t x:~x ~&~t x:~d ~&~t x:~x ~&
                 ~&~t y:~x ~&~t y:~d ~&~t y:~x ~&"
            (get-x pt :vector) (get-x pt :int) (get-x pt :hex-string)
            (get-y pt :vector) (get-y pt :int) (get-y pt :hex-string))))

(defmethod print-object ((ec Curve) stream)
  (print-unreadable-object (ec stream :type t)
    (format out "~&~t a:~x ~&~t a:~d ~&~t a:~x
                 ~&~t b:~x ~&~t b:~d ~&~t b:~x
                 ~&~t p:~x ~&~t p:~d ~&~t p:~x
                 ~&~t ~a
                 ~&~t n:~x ~&~t n:~d ~&~t n:~x
                 ~&~t h:~x ~&~t h:~d ~&~t h:~x"
            (get-a ec :vector) (get-a ec :int) (get-a ec :hex-string)
            (get-b ec :vector) (get-b ec :int) (get-b ec :hex-string)
            (get-p ec :vector) (get-p ec :int) (get-p ec :hex-string)
            (get-g ec)
            (get-n ec :vector) (get-n ec :int) (get-n ec :hex-string)
            (get-h ec :vector) (get-h ec :int) (get-h ec :hex-string))))


;; Errors

(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))


(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
