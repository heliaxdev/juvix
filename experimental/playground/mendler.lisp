;;; Note some parts of my code uses my standard library
;;; This stdlib: can be loaded by the asdf file in the following link
;;; https://github.com/mariari/Misc-Lisp-Scripts

;; No tag case------------------------------------------------------------------
(defun inl (x k l)
  (declare (ignore l))
  (funcall k x))

(defun inr (y k l)
  (declare (ignore k))
  (funcall l y))

(defun case% (i k l)
  (funcall i k l))

(defun zero-c (k l)
  (inl '() k l))

(defun one-c (k l)
  (inr #'zero-c k l))

(defun succ-c (c k l)
  (inr c k l))

(defun fix (f)
  (funcall f (fix f)))

(defun is-even (rec x)
  (case% x
         (lambda (x)
           (declare (ignore x))
           t)
         (lambda (s)
           (not (funcall rec s)))))

(defun pred (rec x)
  (declare (ignore rec))
  (case% x
         (lambda (x)
           (declare (ignore x))
           #'zero)
         (lambda (s)
           s)))

(defun zero (x)
  (in #'zero-c x))

(defun succ (n x)
  (in (fn:curry succ-c n) x))

;; (succ (fn:curry succ #'zero) #'is-even)

;; Attempt 2--------------------------------------------------------------------

(defstruct s param)

(defun s (x)
  (make-s :param x))

(defconstant +Z+ :empty)

(defun fold-m (alg d)
  (funcall d alg))

(defun in (r f)
  (funcall f (fn:curry fold-m f) r))

;; with the tag
(defun zero% (x)
  (in +Z+ x))

;; with the tag
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

(defun is-even-tag (rec x)
  (if (equalp +Z+ x)
      t
      (not (funcall rec (s-param x)))))

;; (time (two% #'is-even-tag))

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

;; (funcall (pred-alg% #'one%) #'is-even-tag)
;; (funcall (pred-alg% #'hundred%) #'is-even-tag)

;; Encoding a binary tree with and without tags --------------------------------

;; data Bin a = Leaf a
;;            | Empt
;;            | Branch (Bin a) a (Bin a)


(defun leaf-c (v k l)
  (inl v k l))

(defun empt-c (k l)
  (inr (fn:curry inl '()) k l))

(defun branch-c (left ele right k l)
  (inr (fn:curry inr (list left ele right)) k l))

(defun leaf (n x)
  (in (fn:curry leaf-c n) x))

(defun empt (x)
  (in #'empt-c x))

(defun branch (left ele right x)
  (in (fn:curry branch-c left ele right) x))


(defmacro case%% (x &rest args)
  (reduce (lambda (term ys)
            (if (equalp (car ys) 'case%)
                `(case% ,x
                        ,term
                        (lambda (,x)
                          ,ys))
                `(case% ,x
                        ,term
                        ,ys)))
          args
          :from-end t))

(defun is-branch (rec x)
  (declare (ignore rec))
  (case%% x
         ;; leaf case
         (constantly nil)
         ;; Empt
         (constantly nil)
         ;; branch case
         (lambda (x)
           ;; x has left ele right
           (print x)
           t)))

(defun is-branch% (rec x)
  (declare (ignore rec))
  (case% x
         ;; leaf case
         (constantly nil)
         ;; Empt ∨ Branch
         (lambda (x)
           (case% x
                  ;; empt case
                  (constantly nil)
                  ;; branch case
                  (lambda (x)
                    ;; x has left ele right
                    (print x)
                    t)))))

;; (branch #'empt 2 #'empt #'is-branch%)
