;;;; util-clos.lisp

(in-package #:cl-ecc)


;; Macro for exporting defining a class AND exporting accessor functions
(defmacro def-exporting-class (name (&rest superclasses) (&rest slot-specs)
                               &optional class-option)
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name)))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,(append
           (mapcar (lambda (spec)
                     (let ((export-pos (position :export spec)))
                       (if export-pos
                       (append (subseq spec 0 export-pos)
                           (subseq spec (+ 2 export-pos)))
                       spec)))
               slot-specs)
           (when class-option (list class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports))))

;; USAGE
;; (macroexpand-1
;;  '(def-exporting-class test1 nil
;;    ((test-1 :accessor test-1 :export t)
;;     (test-2 :initform 1 :reader test-2 :export t)
;;     (test-3 :export t))))

;; (PROGN
;;  (DEFCLASS TEST1 NIL
;;            ((TEST-1 :ACCESSOR TEST-1) (TEST-2 :INITFORM 1 :READER TEST-2)
;;             (TEST-3)))
;;  (EXPORT 'TEST-1)
;;  (EXPORT 'TEST-2))
