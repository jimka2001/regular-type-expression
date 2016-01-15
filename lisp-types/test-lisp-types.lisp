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

(defpackage :lisp-types.test
  (:use :cl :lisp-types :lisp-unit))


(in-package :lisp-types.test)

(define-test type/reduce-cons
  (assert-true (equal (reduce-lisp-type '(cons (and float number) (or string (not string))))
		      '(cons float t))))

(define-test type/reduce-lisp-type
  (flet ((reduce-lisp-type (type)
	   (reduce-lisp-type type)))
    (assert-true (equal (reduce-lisp-type '(and))
			t))
    (assert-true (equal (reduce-lisp-type '(or))
			nil))
    (assert-true (equal (reduce-lisp-type '(and float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float))
			'float))
    (assert-true (equal (reduce-lisp-type '(and float t))
			'float))
    (assert-true (equal (reduce-lisp-type '(and t float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float nil))
			'float))
    (assert-true (equal (reduce-lisp-type '(or nil float))
			'float))
    (assert-true (equal (reduce-lisp-type '(not nil))
			t))
    (assert-true (equal (reduce-lisp-type '(not t))
			nil))
    (assert-true (equal (reduce-lisp-type '(not (not float)))
			'float))
    (assert-true (equal (reduce-lisp-type '(not (or float string)))
			'(and (not float) (not string))))
    (assert-true (equal (reduce-lisp-type '(not (and float string)))
			t)) ;; because (and float string) is nil
    ))


(define-test type/fixed-point
  
  (assert-true (< (rte::fixed-point #'(lambda (obj)
							    (/ obj 2.0))
							10
							:test #'(lambda (a b)
								  (< (abs (- a b)) 0.01)))
		  0.01)))


(define-test type/reduce-lisp-type2
  (assert-true (equal (reduce-lisp-type '(or A (and A B C D) E))
		      '(or A E)))
  (assert-true (equal (reduce-lisp-type '(or (and A B) (and A B C D) E F))
		      '(or E F (and A B))))
  (assert-true (equal (reduce-lisp-type '(or A (and (not A) B)))
		      '(or A B)))
  (assert-true (equal (reduce-lisp-type'(or (and (not A) B) A))
		      '(or A B)))
  (assert-true (equal (reduce-lisp-type '(or (not A) (and A B)))
		      '(or B (not A))))
  (assert-true (equal (reduce-lisp-type '(or (and A B) (not A)))
		      '(or B (not A)))))
  
		      
(define-test type/consensus-theorem
  (assert-true (equal (reduce-lisp-type '(or W (and A B) X
					  Y (and (not A) C)
					  Z (and B C)))
		      '(or W X Y Z
			(and A B)
			(and C (not A)))))
					
  (assert-true (equal (reduce-lisp-type '(or (and A B)
					       (and (not A) C)
					       (and B C)))
		      '(or (and A B)
			(and C (not A)))))
  (assert-true (equal (reduce-lisp-type '(or (and A B)
					       (and B C)
					       (and (not A) C)))
		      '(or (and A B)
			(and C (not A)))))
  (assert-true (equal (reduce-lisp-type '(or (and A B)
					       (and B C)
					       (and C (not A))))
		      '(or (and A B)
			(and C (not A)))))

  (assert-true (equal (reduce-lisp-type '(or (and A U V)
					       (and V W (not A))
					       (and V W U)))
		      '(or (and A U V)
			(and V W (not A)))))
  (assert-true (equal (reduce-lisp-type '(or (and A U V)
					       (and (not A) V W)
					       (and U V W)))
		      '(or (and A U V)
			(and V W (not A))))))


(define-test type/rule-case
  (let ((n 0))
    (assert-true (equal (lisp-types::rule-case (+ 4 5)
			  (t
			   (incf n)
			   9)
			  ((= 1 n)
			   (incf n)
			   9)
			  ((= 2 n)
			   (incf n)
			   0))
			0))
    (assert-true (equal 3 n))))
				  
(define-test type/reduce-member
  (assert-true (equal (reduce-lisp-type '(and (member 1 2 3)
					  (member 2 3 4)))
		      '(member 2 3)))
  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 3))
					  (not (member 2 3 4))))
		      '(not (member 1 2 3 4 ))))
  (assert-true (equal (reduce-lisp-type '(and (not (member 20))
					  (not (member 2 3 4))
					  (member 10 20 30)
					  (member 20 30)))
		      '(eql 30)))
  (assert-true (equal (reduce-lisp-type '(and keyword (member :y :z) (not (eql :y))))
		      '(eql :z)))
  (assert-true (equal (reduce-lisp-type '(and (member a b 2 3) symbol))
		      '(member a b)))
  (assert-true (equal (reduce-lisp-type '(and (member a 2) symbol))
		      '(eql a)))
  (assert-true (equal (reduce-lisp-type '(and (member a b) fixnum))
		      nil))

  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 a)) (not (member 2 3 4 b))))
		      '(not (member 1 2 3 4 a b))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member 1 2 a b))))
		      '(and fixnum (not (member 1 2)))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member a b))))
		      'fixnum))
  (assert-true (equal (reduce-lisp-type '(or fixnum string (member 1 2 "hello" a b)))
		      '(or fixnum string (member a b))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(and keyword (not (member :x))))
		      '(and keyword (not (eql :x)))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(or number (not (member 1 2 a b))))
		      '(not (member a b))))
  )
