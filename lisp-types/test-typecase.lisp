;; Copyright (c) 2016 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



(in-package :lisp-types.test)

(define-test rte/disjoint-typecase
  (assert-true (equal (macroexpand-1 '(lisp-types::disjoint-typecase fred
				       (integer 42)
				       (bignum 43)
				       (number 44)
				       (string 45)
				       ((or string float) 46)
				       ((or string number) 47)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(NIL 43)
			((AND NUMBER (NOT INTEGER)) 44)
			(STRING 45)
			(NIL 46)
			(NIL 47))))
  (assert-true (equal (macroexpand-1 '(lisp-types::disjoint-typecase fred
				       (integer 42)
				       ((and number (not integer)) 43)
				       ((and number (not bignum)) 44)))
		      '(TYPECASE FRED
			(INTEGER 42)
			((AND NUMBER (NOT INTEGER)) 43)
			(NIL 44)))))

(define-test rte/reduced-typecase
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       (integer 42)
				       (bignum 43)
				       (number 44)
				       (string 45)
				       ((or string float) 46)
				       ((or string number) 47)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(BIGNUM 43)
			(NUMBER 44)
			(STRING 45)
			(FLOAT 46)
			(NIL 47))))
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       (integer 42)
				       ((and number (not integer)) 43)
				       ((and number (not bignum)) 44)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(NUMBER 43)
			(NIL 44)))))

(define-test rte/reduced-typecase2
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       ((or A B)
					41)
				       (A
					42)
				       (B
					43)))
		      '(TYPECASE FRED ((OR A B) 41) (NIL 42) (NIL 43))))
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       ((and A B)
					41)
				       (A
					42)
				       (B
					43)))
		      '(TYPECASE FRED ((AND A B) 41) (A 42) (B 43))))
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       ((or (not A) B)
					41)
				       (A
					42)
				       (B
					43)))
		      '(TYPECASE FRED ((OR (NOT A) B) 41) (T 42) (NIL 43))))
  (assert-true (equal (macroexpand-1 '(rte::reduced-typecase fred
				       ((or (not (and A B)) C)
					41)
				       (A
					42)
				       (B
					43)
				       (C
					44)))
		      '(TYPECASE FRED ((OR (NOT A) (NOT B) C) 41) (T 42) (T 43) (NIL 44))))
  

  )


(define-test rte/optimized-typecase
  (assert-true (equal (macroexpand-1 (macroexpand-1 '(rte::optimized-typecase fred
						      (integer 42)
						      (bignum 43)
						      (number 44)
						      (string 45)
						      ((or string float) 46)
						      ((or string number) 47))))
		      '(TYPECASE FRED
			(STRING 45)
			(INTEGER 42)
			(NUMBER 44)
			(NIL 47)
			(NIL 46)
			(NIL 43))))

  (assert-true (equal (macroexpand-1 (macroexpand-1 '(rte::optimized-typecase fred
						      (integer 42)
						      ((and number (not integer)) 43)
						      ((and number (not bignum)) 44))))
		      '(TYPECASE FRED
			(INTEGER 42)
			(NUMBER 43)
			(NIL 44)))))


(define-test rte/optimized-typecase2
  (assert-true (equal (rte:optimized-typecase 1
					      (null 2)
					      (t 3))
		      3))
  (assert-true (equal (rte:optimized-typecase nil
					      (null 2)
					      (t 3))
		      2))
  (assert-true (equal (rte:optimized-typecase 1
					      (t 2))
		      2))

  (assert-true (equal (rte:optimized-typecase 1
					      (null 2))
		      nil))
  (assert-true (equal (rte:optimized-typecase nil
					      (null 2))
		      2))
  )
  
  
  