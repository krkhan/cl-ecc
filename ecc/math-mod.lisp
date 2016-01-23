;;;; math-mod.lisp

(in-package #:cl-ecc)

(defun add-mod (&rest args)
  "Returns: arg1 + arg2 + ... + arg(n-1) (mod n)"
  (let ((lastindex (1- (length args))))
    (mod (apply '+ (subseq args 0 lastindex)) (nth lastindex args))))

(defun sub-mod (&rest args)
  "Returns: arg1 - arg2 - ... - arg(n-1) (mod n)"
  (let ((lastindex (1- (length args))))
    (mod (apply '- (subseq args 0 lastindex)) (nth lastindex args))))

(defun mul-mod (&rest args)
  "Returns: arg1 * arg2 * ... * arg(n-1) (mod n)"
  (let ((lastindex (1- (length args))))
    (mod (apply '* (subseq args 0 lastindex)) (nth lastindex args))))

(defun inv-mod (a n)
  "Returns: a^-1 (mod n)
   Signals: 'invalid-operation-error if gcd(a, n) != 1"
  (unless (= (ironclad::gcd a n) 1)
    (error 'invalid-operation-error :msg "gcd(a, n) != 1"))

  (multiple-value-bind (g b a) (ironclad:egcd a n)
    (declare (ignore g a))
    (mod b n)))

(defun div-mod (a b n)
  "Returns: a / b (mod n)"
  (mul-mod a (inv-mod b n) n))

(defun legendre-symbol (a p)
  "Returns: 1 if a is a quadratic residue (mod p),
           -1 if a is a quadratic non-residue (mod p)
            0 if a = 0 (mod p)
           >1 there appears to be a problem with the arguments (ie p not prime)"
  (if (=  (mod a p) 0) (return-from legendre-symbol 0))
  (let* ((pow (truncate (/ (1- p) 2)))
         (ls (ironclad:expt-mod a pow p)))
    (if (= ls (1- p))
        -1
        ls)))

(defun sqrt-mod (a p)
  "Returns: x where x^2 = a (mod p)"
  (let (ls s e n x b g r tt m gs)
    (setf ls (legendre-symbol a p))
      (cond
        ((not (= ls 1))
         (error 'invalid-operation-error
                :msg (format nil "sqrt of ~a does not exist mod ~a" a p)))
        ((= a 0) (return-from sqrt-mod 0))
        ((= p 2) (return-from sqrt-mod p))
        ((= (mod p 4) 3) (return-from sqrt-mod (ironclad:expt-mod a (/ (1+ p) 4) p))))

      (setf s (1- p))
      (setf e 0)

      (loop do
           (setf s (truncate (/ s 2)))
           (setf e (1+ e))
         while (= (mod s 2) 0))

      (setf n 2)
      (loop do
           (setf n (1+ n))
         until (= (legendre-symbol n p) -1))

      (setf x (ironclad:expt-mod a (truncate (/ (1+ s) 2)) p))
      (setf b (ironclad:expt-mod a s p))
      (setf g (ironclad:expt-mod n s p))
      (setf r e)

      (loop do
           (setf tt b)
           (setf m 0)

           (loop repeat r
              until (= tt 1)
              do
                (setf tt (ironclad:expt-mod tt 2 p))
                (setf m (1+ m)))

           (when (= m 0) (return-from sqrt-mod x))

           (setf gs (ironclad:expt-mod g (expt 2 (- r m 1)) p))
           (setf g (mul-mod gs gs p))
           (setf x (mul-mod x gs p))
           (setf b (mul-mod b g p))
           (setf r m))))





;; Deprecated


;; Does not return 100& correct

;; (defun legendre-symbol2 (a p)
;;   "Returns: 1 if a is a quadratic residue (mod p),
;;            -1 if a is a quadratic non-residue (mod p)
;;             >1  if p is not a prime"
;;   (let (pow ls)
;;     (setf pow (truncate (/ (1- p) 2)))
;;     (setf ls (ironclad:expt-mod a pow p))
;;     (when (= ls (1- p)) (return-from legendre-symbol2 -1))
;;     ls))



;; Ironclad expt being used

;; (defun ironclad:expt-mod (x e n)
;;   "Returns: x^e (mod n)"
;;   (loop with result = x
;;      for i from (- (integer-length e) 2) downto 0
;;      do
;;        (setf result (mul-mod result result n))
;;        (when (logbitp i e)
;;          (setf result (mul-mod result x n)))
;;      finally
;;        (return-from ironclad:expt-mod result)))


;; (defmethod egcd ((a (eql 'octet-vector)) (b (eql 'octet-vector)))
;;   (let ((a-int (ironclad:octets-to-integer a))
;;         (b-int (ironclad:octets-to-integer b)))
;;     (egcd a-int b-int)))

;; (defmethod egcd2 ((a integer) (b integer))
;;   "Returns: (gcd(a, b), s, t) where gcd(a, b) = s*a + t*b"
;;   (let (quotient)
;;     (do ((s-i 0)
;;          (s-i-minus-1 1)
;;          (t-i 1)
;;          (t-i-minus-1 0)
;;          (r-i (min a b))
;;          (r-i-minus-1 (max a b)))

;;         ((= r-i 0) (values r-i-minus-1 s-i-minus-1 t-i-minus-1))

;;         (setf quotient (truncate (/ r-i-minus-1 r-i)))
;;         (shiftf r-i-minus-1 r-i (- r-i-minus-1 (* quotient r-i)))
;;         (shiftf s-i-minus-1 s-i (- s-i-minus-1 (* quotient s-i)))
;;       (shiftf t-i-minus-1 t-i (- t-i-minus-1 (* quotient t-i))))))

;; (defun egcd2 (a b)
;;   "Returns: (s, t) where gcd(a, b) = s*a + t*b if a>b
;;    or gcd(a, b) = t*a + s*b if a<b.
;;    Is more efficient than egcd()"
;;   (if (zerop b) (return-from egcd2 (values 1 0)))
;;   (multiple-value-bind (q r) (floor a b)
;;     (multiple-value-bind (s v) (egcd2 b r)
;;       (values v (- s (* q v))))))




;; (defun ironclad:expt-mod-n2 (b e n)
;;   (when (= n 1) (return-from ironclad:expt-mod-n2 0))
;;   (loop
;;      with base = (mod b n)
;;      with result = 1
;;      for i from (??? e) downto 0 ;; need to find function
;;                                  ;; to convert decimal to binary integer
;;      do
;;        (when (oddp i)
;;          (setf result (mul-mod result base n)))
;;        (setf i (ash i -1))
;;        (setf base (mul-mod base base n))
;;      finally
;;        (return result)))



;; (define-octet-vector-version sub-mod)
;; (define-octet-vector-version mul-mod)

;; (defmethod add-mod-pair ((type (eql 'octet-array)) arg1 arg2 &key (base 256))
;;   "Return: octet-vector. arg1 + arg2 ... + arg(n-1) (mod n)"
;;   (let* ((carry-over 0)
;;          (big-vector (copy-array (largest-sequence arg1 arg2) :fill-pointer t))
;;          (small-vector (copy-array (smallest-sequence arg1 arg2) :fill-pointer t))
;;          (diff-size (- (length big-vector) (length small-vector))))
;;     ;; (declare (type (vector (unsigned-byte 8)) big-vector small-vector)
;;     ;;          (ignorable diff-size))

;;     (do ((counter 0 (1+ counter))
;;          (tresult (make-array (length big-vector)
;;                               :element-type '(unsigned-byte 8)
;;                               :fill-pointer t
;;                               :adjustable t))
;;          (smallv-size (length small-vector) (1- smallv-size))
;;          bigv-elt smallv-elt)
;;         ((eq 0 smallv-size) (do ((counter 0 (1+ counter)))
;;                                 ((eq counter diff-size)
;;                                  (nreverse (copy-array tresult
;;                                                        :element-type '(unsigned-byte 8)
;;                                                        :fill-pointer nil
;;                                                        :adjustable nil)))
;;                               (multiple-value-bind (q r) (truncate (+ (vector-pop big-vector) carry-over) base)
;;                                 (unless (eq r 0)
;;                                   (vector-push-extend r tresult))
;;                                 (setf carry-over q))))
;;       (setf bigv-elt (vector-pop big-vector))
;;       (setf smallv-elt (vector-pop small-vector))
;;       (multiple-value-bind (q r) (truncate (+ bigv-elt smallv-elt carry-over) base)
;;         (setf (elt tresult counter) r)
;;         (setf carry-over q)))))
