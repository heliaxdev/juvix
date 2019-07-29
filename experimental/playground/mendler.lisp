;;; Note some parts of my code uses my standard library
;;; This stdlib: can be loaded by the asdf file in the following link
;;; https://github.com/mariari/Misc-Lisp-Scripts

;; attempt 1 -------------------------------------------------------------------

(defun inl (x k l)
  (declare (ignore l))
  (funcall k x))

(defun inr (y k l)
  (declare (ignore k))
  (funcall l y))

(defun case% (i k l)
  (funcall i k l))

(defun zero (k l)
  (inl '() k l))

(defun one (k l)
  (inr #'zero k l))

(defun fix (f)
  (funcall f (fix f)))

;; Attempt 2--------------------------------------------------------------------

(defstruct s param)

(defun s (x)
  (make-s :param x))

(defconstant +Z+ :empty)

(defun fold-m (alg d)
  (funcall d alg))

(defun in (r f)
  (funcall f (fn:curry fold-m f) r))

(defun zero% (x)
  (in +Z+ x))

(defun succ% (n x)
  (in (S n) x))

(defun one% (x)
  (succ% #'zero% x))

(defun two% (x)
  (succ% #'one% x))

(defun hundred% (x)
  (succ%
   (reduce (lambda (x acc)
             (declare (ignore x))
             (fn:curry succ% acc))
           (list:range 0 98)
           :initial-value #'zero%
           :from-end t)
   x))

(defun six% (x)
  (succ% (fn:curry succ% (fn:curry succ% (fn:curry succ% (fn:curry succ% #'one%)))) x))

(defun is-even (rec x)
  (if (equalp +Z+ x)
      t
      (not (funcall rec (s-param x)))))

;; (time (two% #'is-even))

(defgeneric fmap (f xs))


(defmethod fmap (f (x (eql :empty)))
  (declare (ignore f))
  x)

(defmethod fmap (f (x s))
  (s (funcall f (s-param x))))

(defun out (d)
  (funcall d
           (lambda (rec fr) (fmap (lambda (r) (fn:curry in (funcall rec r))) fr))))

(defun out% (rec fr)
  (fmap (lambda (r) (fn:curry in (funcall rec r))) fr))

;; first attempt O(1)
(defun pred-alg% (n)
  (let ((var (out n)))
    (if (equalp +Z+ var)
        #'zero%
        (s-param var))))

(defun pred-alg (rec n)
  (declare (ignore rec))
  (if (equalp +Z+ n)
      #'zero%
      (s-param n)))

;; (time (pred-alg% #'hundred%))

;; (funcall (pred-alg% #'one%) #'is-even)
;; (funcall (pred-alg% #'hundred%) #'is-even)
;; (time (hundred% #'pred-alg))
