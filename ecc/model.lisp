;;;; model.lisp

(in-package #:cl-ecc)


;; General

(define-binary-type byte-array (bytes)
  (:reader (in)
           (cond
             ((not (eq bytes nil))
              (let ((seq (make-byte-array bytes)))
                (iterate (for i from 0 to (- bytes 1))
                         (setf (elt seq i) (read-byte in)))
                seq))
             ((eq bytes nil)
              (let* ((slength (- (ironclad::end in) (ironclad::index in)))
                     (seq (make-byte-array slength)))
                (read-sequence seq in)
                seq))))
  (:writer (out seq)
           (cond
             ((eq bytes nil) (write-sequence seq out))
             ((not (eq bytes nil)) (write-sequence seq out :end bytes)))))


;; Curve and Point

(defclass Point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))


(defmethod print-object ((p Point) out)
  (when (point-equalp p +inf-point+)
    (format out "<Point At Infinity>")
    (return-from print-object))
  (format out "<Point~%~tx:~x~%~ty:~x>" (x p) (y p)))

(defclass Curve ()
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)
   (p :accessor p :initarg :p)
   (g :accessor g :initarg :g)
   (n :accessor n :initarg :n)
   (h :accessor h :initarg :h)))

(defmethod print-object ((c Curve) out)
  (format out "<Curve ~%a:~x~%b:~x~%p:~x~%g:~x~%n:~x~%>"
          (a c) (b c) (p c) (g c) (n c)))


(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (pname (intern (concatenate 'string (string 'Point-) (string name)))))
      `(progn
          (define-binary-class ,pname (Point)
            ((x (byte-array :bytes ,(/ (length par-g) 4)))
             (y (byte-array :bytes ,(/ (length par-g) 4)))))

         (define-binary-class ,curve-name (Curve)
           ((a (byte-array :bytes ,(/ (length par-a) 2)))
            (b (byte-array :bytes ,(/ (length par-b) 2)))
            (p (byte-array :bytes ,(/ (length par-p) 2)))
            (g ,pname)
            (n (byte-array :bytes ,(/ (length par-n) 2)))
            (h (byte-array :bytes ,(/ (length par-h) 2)))))

         (defvar ,sym
             (read-value ',curve-name
                         (ironclad:make-octet-input-stream
                          (ironclad:hex-string-to-byte-array
                           ,(concatenate 'string par-a par-b par-p par-g par-n par-h))))))))

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
