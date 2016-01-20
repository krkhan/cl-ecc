;;;; common.lisp

(in-package #:cl-ecc)

(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))

(defmacro validate-accessor-types (classname &rest typespeclist)
  (let ((i (gensym))
        (accessor (gensym))
        (typename (gensym)))
    `(defmethod initialize-instance :after ((object ,classname) &key)
       (do ((,i 0 (+ ,i 2))
            (,accessor)
            (,typename))
           ((>= ,i (length ',typespeclist)))
           (setf ,accessor (nth ,i ',typespeclist))
           (setf ,typename (nth (1+ ,i) ',typespeclist))
           (unless (typep (funcall ,accessor object) ,typename)
             (error 'invalid-type-error
                    :msg (format nil "~a must be of type ~a instead of ~a"
                                 ,accessor
                                 ,typename
                                 (type-of (funcall ,accessor object)))))))))

(defmacro export-pack-symbols (pack-name)
  (let ((pack (gensym))
        (pack-key (gensym)))
    `(let ((,pack-key ,pack-name))
       (let ((,pack (find-package ,pack-key)))
         (do-all-symbols (sym ,pack)
           (when (eql (symbol-package sym) ,pack) (export sym)))))))

(defmacro export-all-symbols (pack-name)
  (let ((pack (gensym))
        (pack-key (gensym)))
    `(let ((,pack-key ,pack-name))
       (let ((,pack (find-package ,pack-key)))
         (do-all-symbols (sym ,pack) (export sym))))))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))



(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;; Errors

(define-ecc-error invalid-operation-error)
(define-ecc-error invalid-type-error)
