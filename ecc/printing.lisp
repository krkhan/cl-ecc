;;;; printing.lisp

(in-package :cl-ecc)

;; model.lisp

(defmethod print-object ((pt Point) stream)
  (when (point-equalp pt *inf-point*)
    (format stream "<Point At Infinity>")
    (return-from print-object))
  (print-unreadable-object (pt stream :type t)
    (format stream "~&~t x:~x  ~&~t x:~x ~&
                    ~&~t y:~x  ~&~t y:~x ~&"
            (x pt) (x pt :hex-string t)
            (y pt) (y pt :hex-string t))))

(defmethod print-object ((ec Curve) stream)
  (print-unreadable-object (ec stream :type t)
    (format stream "~&~t a:~x ~&~t a:~x
                    ~&~t b:~x ~&~t b:~x
                    ~&~t p:~x ~&~t p:~x
                    ~&~t ~a
                    ~&~t n:~x ~&~t n:~x
                    ~&~t h:~x ~&~t h:~x"
            (a ec) (a ec :hex-string t)
            (b ec) (b ec :hex-string t)
            (p ec) (p ec :hex-string t)
            (g ec)
            (n ec) (n ec :hex-string t)
            (h ec) (h ec :hex-string t))))

(defmethod print-object ((key Key) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~&~t key:~x ~&~t key:~x"
            (key key) (key key))))

;; elgamal.lisp

(defmethod print-object ((msg ElGamalmessage) stream)
  (print-unreadable-object (msg stream :type t)
    (format stream "~&~t x:~a
                   ~&~t y:~a"
            (x msg)
            (y msg))))


;; ecdsa.lisp

(defmethod print-object ((sig ECDSA-Signature) stream)
  (print-unreadable-object (sig stream :type t)
    (format stream "~&~t r:~x  ~&~t r:~x
                    ~&~t s:~x  ~&~t s:~x"
            (r sig)  (r sig :hex-string t)
            (s sig)  (s sig :hex-string t))))

(defmethod print-object ((msghash ECDSA-Message-Hash) stream)
  (print-unreadable-object (msghash stream :type t)
    (format stream "~&~t hash:~x ~&~t hash:~x"
            (hash msghash)  (hash msghash :hex-string t))))

(defmethod print-object ((pubkey ECDSA-Public-Key) stream)
  (print-unreadable-object (pubkey stream :type t)
    (format stream "~&~t key:~x  ~&~t key:~x"
            (key pubkey)  (key pubkey))))
