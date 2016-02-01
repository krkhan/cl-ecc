;;;; model.lisp

(in-package #:cl-ecc)

(defgeneric get-slot (spec slot object)
  (:documentation
   "General reader function that gets value from object according to spec.
    Arguments; spec: :int, :vector, :hex-string, :point"))



;; Helper macros

(defmacro define-generic-reader-functions (class)

  `(progn

     (defmethod get-slot :before (slot (object ,class) spec)
                (cond
                  ((and (eq slot 'g)
                        (typep object 'Curve)) (assert (typep (slot-value object slot) 'Point)))
                  (t (assert (typep (slot-value object slot) 'octet-vector)))))

     (defmethod get-slot ((spec (eql :int)) slot (object ,class) )
       (ironclad:octets-to-integer (slot-value object slot)))

     (defmethod get-slot ((spec (eql :vector)) slot (object ,class) )
       (slot-value object slot))

     (defmethod get-slot ((spec (eql :hex-string)) slot (object ,class))
       (ironclad:byte-array-to-hex-string (slot-value object slot)))

     (defmethod get-slot ((spec (eql :point)) slot (object ,class))
       (slot-value object slot))))

;; Model

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-class-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (point-class-name (intern (concatenate 'string (string 'Point-) (string name)))))
      `(progn
          (defclass ,point-class-name (Point) ())
          (defclass ,curve-class-name (Curve) ())

          (defvar ,sym
            (make-instance ',curve-class-name
                           :a (hex-string-to-byte-array ,par-a)
                           :b (hex-string-to-byte-array ,par-b)
                           :p (hex-string-to-byte-array ,par-p)
                           :g (make-instance ',point-class-name
                                             :x (hex-string-to-byte-array (subseq ,par-g 0 (/ (length ,par-g) 2)))
                                             :y (hex-string-to-byte-array (subseq ,par-g (/ (length ,par-g) 2))))
                           :n (hex-string-to-byte-array ,par-n)
                           :h (hex-string-to-byte-array ,par-h))))))


;; Generic Classes and reader functions
(defclass Private-Key ()
  ((key :initarg :key
        :initform (error ":key must be initialized")
        :type 'octet-vector)))

(defclass Point ()
  ((x :initarg :x
      :initform (error ":x must be initialized")
      :type '(octet-vector))
   (y :initarg :y
      :initform (error ":y must be initialized")
      :type 'octet-vector)))

(defclass Curve ()
  ((a :initarg :a
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

;; Reader functions
(define-generic-reader-functions Private-Key)
(define-generic-reader-functions Point)
(define-generic-reader-functions Curve)


(defvar *inf-point* (make-instance 'Point
                                   :x (octet-vector 0)
                                   :y (octet-vector 0))
  "Zero Point")





;; Printing

(defmethod print-object ((pt Point) stream)
  (when (point-equalp pt *inf-point*)
    (format stream "<Point At Infinity>")
    (return-from print-object))
  (print-unreadable-object (pt stream :type t)
    (format stream "~&~t x:~x ~&~t x:~d ~&~t x:~x ~&
                   ~&~t y:~x ~&~t y:~d ~&~t y:~x ~&"
            (get-slot :vector 'x pt) (get-slot :int 'x pt) (get-slot :hex-string 'x pt)
            (get-slot :vector 'y pt) (get-slot :int 'y pt) (get-slot :hex-string 'y pt))))

(defmethod print-object ((ec Curve) stream)
  (print-unreadable-object (ec stream :type t)
    (format stream "~&~t a:~x ~&~t a:~d ~&~t a:~x
                   ~&~t b:~x ~&~t b:~d ~&~t b:~x
                   ~&~t p:~x ~&~t p:~d ~&~t p:~x
                   ~&~t ~a
                   ~&~t n:~x ~&~t n:~d ~&~t n:~x
                   ~&~t h:~x ~&~t h:~d ~&~t h:~x"
            (get-slot :vector 'a ec) (get-slot :int 'a ec) (get-slot :hex-string 'a ec)
            (get-slot :vector 'b ec) (get-slot :int 'b ec) (get-slot :hex-string 'b ec)
            (get-slot :vector 'p ec) (get-slot :int 'p ec) (get-slot :hex-string 'p ec)
            (get-slot :point 'g ec)
            (get-slot :vector 'n ec) (get-slot :int 'n ec) (get-slot :hex-string 'n ec)
            (get-slot :vector 'h ec) (get-slot :int 'h ec) (get-slot :hex-string 'h ec))))

(defmethod print-object ((key Private-Key) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~&~t key:~x ~&~t key:~d ~&~t key:~x"
            (get-slot :vector 'key key) (get-slot :int 'key key) (get-slot :hex-string 'key key))))

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
