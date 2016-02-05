;;;; printing.lisp

(in-package :cl-ecc)

(defmethod print-object ((pt Point) stream)
  (when (point-equalp pt *inf-point*)
    (format stream "<Point At Infinity>")
    (return-from print-object))
  (print-unreadable-object (pt stream :type t)
    (format stream "~{~&~x:~&~}" (list 'x (x pt) 'y (y pt)))))

(defmethod print-object ((ec Curve) stream)
  (print-unreadable-object (ec stream :type t)
    (format stream "~{~&~x:~&~}"
            (list 'a (a ec) 'b (b ec) 'p (p ec) 'g (g ec) 'n (n ec) 'h (h ec)) )))

(defmethod print-object ((key Key) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~{~&~x:~&~}" (list 'key (key key)))))

;; elgamal.lisp

(defmethod print-object ((msg ElGamalmessage) stream)
  (print-unreadable-object (msg stream :type t)
    (format stream "~{~&~x:~&~}" (list 'x (x msg) 'y (y msg)))))


;; ecdsa.lisp

(defmethod print-object ((sig ECDSA-Signature) stream)
  (print-unreadable-object (sig stream :type t)
    (format stream "~{~&~x:~&~}" (list 'r (r sig) 's (s sig)))))

(defmethod print-object ((msghash ECDSA-Message-Hash) stream)
  (print-unreadable-object (msghash stream :type t)
    (format stream "~{~&~x:~&~}" (list 'hash (hash msghash)))))
