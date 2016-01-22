;;;; model.lisp

(in-package #:cl-ecc)


;; General

(define-binary-type byte-array (bytes)
  (:reader (in)
           (cond
             ((integerp bytes)
              (let ((seq (make-byte-array bytes)))
                (read-sequence seq in)))
             ((eq bytes nil)
              (let* ((slength (ironclad::end in))
                     (seq (make-byte-array slength)))
                (read-sequence seq in)))
             (t (error "byte-array reader keys not specified correctly."))))
  (:writer (out seq)
           (cond
             ((eq bytes nil) (write-sequence seq out))
             ((integerp bytes) (write-sequence seq out :end bytes))
             (t (error "byte-array writer keys not specified correctly.")))))
s

;; Curve

(defmacro define-elliptic-curve (name sym &key par-a par-b par-p par-g par-n par-h)
    (let ((curve-name (intern (concatenate 'string (string 'Curve-) (string name))))
          (pname (intern (concatenate 'string (string 'Point-) (string name)))))
      `(progn
          (define-binary-class ,pname (Point)
           ((x byte-array)
            (y byte-array)))

         (define-binary-class ,curve-name (Curve)
           ((a byte-array)
            (b byte-arry)
            (p byte-array)
            (g ,pname)
            (n byte-array)
            (h byte-array)))

         (define-constant ,sym
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
