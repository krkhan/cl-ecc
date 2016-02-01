;;;; printing.lisp

(in-package :cl-ecc)

;; model.lisp

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

;; elgamal.lisp

(defmethod print-object ((msg ElGamalmessage) stream)
  (print-unreadable-object (msg stream :type t)
    (format stream "~&~t x:~a
                   ~&~t y:~a"
            (slot-value msg 'x)
            (slot-value msg 'y))))


;; ecdsa.lisp

(defmethod print-object ((sig ECDSA-Signature) stream)
  (print-unreadable-object (sig stream :type t)
    (format stream "~&~t r:~x ~&~t r:~d ~&~t r:~x
                   ~&~t s:~x ~&~t s:~d ~&~t s:~x"
            (get-slot :vector 'r sig) (get-slot :int 'r sig) (get-slot :hex-string 'r sig)
            (get-slot :vector 's sig) (get-slot :int 's sig) (get-slot :hex-string 's sig))))

(defmethod print-object ((msghash ECDSA-Message-Hash) stream)
  (print-unreadable-object (msghash stream :type t)
    (format stream "~&~t hash:~x ~&~t hash:~d ~&~t hash:~x"
            (get-slot :vector 'hash msghash) (get-slot :int 'hash msghash) (get-slot :hex-string 'hash msghash))))

(defmethod print-object ((pubkey ECDSA-Public-Key) stream)
  (print-unreadable-object (pubkey stream :type t)
    (format stream "~&~t version-byte:~x ~&~t version-byte:~d ~&~t version-byte:~x
                   ~&~t x:~x ~&~t x:~d ~&~t x:~x
                   ~&~t y:~x ~&~t y:~d ~&~t y:~x
                   ~&~t key:~x ~&~t key:~d ~&~t key:~x"
            (get-slot :vector 'version-byte pubkey) (get-slot :int 'version-byte pubkey) (get-slot :hex-string 'version-byte pubkey)
            (get-slot :vector 'x pubkey) (get-slot :int 'x pubkey) (get-slot :hex-string 'x pubkey)
            (get-slot :vector 'y pubkey) (get-slot :int 'y pubkey) (get-slot :hex-string 'y pubkey)
            (get-key :vector pubkey) (get-key :int pubkey) (get-key :hex-string pubkey))))
