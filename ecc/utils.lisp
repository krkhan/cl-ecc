;;;; utils.lisp
(in-package :cl-ecc)

;; Misc
(defmacro conc-to-symbol (&rest args)
  `(intern (concatenate 'string ,@(mapcar #'(lambda (x) `(string ,x))  args))))

;; Number functions
(defun integer-byte-size (integer)
  (ceiling (integer-length integer) 8))
(defun concatenate-integers (&rest args)
  (reduce #'(lambda (y x) (+ x (* (expt 10 (length (format nil "~d" x))) y))) args))
(defun subseq-integer (integer start &optional end)
  (parse-integer (subseq (format nil "~d" integer) start end)))
(defun integer-as-sequence-split (int)
  (let* ((hex-string (integer-to-hex-string int))
        (slength (length hex-string)))
    (list (parse-integer (subseq hex-string 0 (/ slength 2)) :radix 16)
          (parse-integer (subseq hex-string (/ slength 2)) :radix 16))))

;; Hex-string functions
(defun integer-to-hex-string (int)
  "Transform integer into proper hex-string (with even number of elements)"
  (let* ((hstring (concatenate 'string (format nil "~x" int)))
        (string-length (length hstring)))
    (unless (evenp string-length)
      (setf hstring (concatenate 'string "0" hstring))
      (incf string-length))
    hstring))

;; Error macros
(defmacro define-ecc-error (error-name)
  (let ((object (gensym))
        (out (gensym)))
    `(progn
       (define-condition ,error-name (error)
         ((msg :initarg :msg :reader error-msg)))
       (defmethod print-object ((,object ,error-name) ,out)
         (format ,out "~a" (error-msg ,object))))))

(defun put-keys-in-hash-table (filename)
  (with-open-file (stream filename)
    (do ((result (make-hash-table))
         (priv (read-line stream nil) (read-line stream nil 'eof))
         (pub (read-line stream nil) (read-line stream nil 'eof)))
        ((or (eq 'eof priv) (eq 'eof pub)) result)
      (setf (gethash (parse-integer priv :radix 16) result) (parse-integer pub :radix 16)))))
