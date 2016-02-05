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

;; inv-mod is being phased out for ironclad::modular-inverse.
;; It might cause a bug as it returns 0 where inv-mod fails.
;; I think.
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
  (mul-mod a (ironclad::modular-inverse b n) n))

(defun legendre-symbol (a p)
  "Returns: 1 if a is a quadratic residue (mod p),
           -1 if a is a quadratic non-residue (mod p)
            0 if a = 0 (mod p)
           >1 there appears to be a problem with the arguments (ie p not prime)"
  (if (=  (mod a p) 0) (return-from legendre-symbol 0))
  (let* ((pow (truncate (/ (1- p) 2)))
         (ls (ironclad:expt-mod a pow p)))
    (when (and (> ls 1) (< ls -1))
      (error "Seems to be a problem with the arguments. (ie p is not prime)"))
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
