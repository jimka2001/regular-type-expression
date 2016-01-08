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

(in-package :rte.test)

(define-test test/destructuring-case-1-a
  (assert-true
   (equal 3 (destructuring-case '(x y z)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3)))))
(define-test test/destructuring-case-1-b

  (assert-true
   (equal 2 (destructuring-case '(x y)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3))))  )

(define-test test/destructuring-case-1-c
  (assert-true
   (equal 1 (destructuring-case '(x)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-a
  (assert-true
   (equal 3 (destructuring-case '((x) y z)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      (((a) b c)
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-b
  
  (assert-true
   (equal 2 (destructuring-case '(x (y))
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a (b))
	       (declare (ignore a b))
	       2)
	      (((a) b)
	       (declare (ignore a b))
	       3))))  )


(define-test test/destructuring-case-2-c
  (assert-true
   (equal 3 (destructuring-case '((x) y)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a (b))
	       (declare (ignore a b))
	       2)
	      (((a) b)
	       (declare (ignore a b))
	       3))))
)
   
   
(define-test test/destructuring-case-3
  (let ((n 0))
    (dolist (x '((1)
		 (2 (3))
		 (1 ((2)) (3 4))))
      (destructuring-case x
	((a)
	 (incf n)
	 (assert-true (= a 1)))
	((a (b))
	 (incf n)
	 (assert-true (= a 2))
	 (assert-true (= b 3)))
	((a ((b)) (c d))
	 (incf n)
	 (assert-true (= a 1))
	 (assert-true (= b 2))
	 (assert-true (= c 3))
	 (assert-true (= d 4)))))

    (assert-true (= n 3))))


	  
(define-test test/destructuring-case-4
  (let ((a '(1 2 :x 3 :y 4))
	(n 0))
    (destructuring-case a
      ((u v &key x y)
       (incf n)
       (assert-true (= u 1))
       (assert-true (= v 2))
       (assert-true (= x 3))
       (assert-true (= y 4))))
    (assert-true (= n 1))))


(define-test test/destructuring-case-5
  (destructuring-bind (u v &key x ((:y (y1 y2)) '(nil nil))) '(1 2 :x 3 :y (4 5))
    (assert-true (= u 1))
    (assert-true (= v 2))
    (assert-true (= x 3))
    (assert-true (= y1 4))
    (assert-true (= y2 5)))
  (let ((a '(1 2 :x 3 :y (4 5)))
	(n 0))
    (destructuring-case a
      ((u v &key x ((:y (y1 y2)) '(nil nil)))
       (incf n)
       (assert-true (= u 1))
       (assert-true (= v 2))
       (assert-true (= x 3))
       (assert-true (= y1 4))
       (assert-true (= y2 5))))
    (assert-true (= n 1))))

(define-test test/destructuring-case-6
  (LET ((N 0)
	(A '(1 2 :x 3 :y (4 5))))
    (TYPECASE A
      ((NOT LIST) NIL)
      ((RTE
	(:CAT T
	      T
	      (:OR
	       (:CAT (:OR (:CAT (EQL :X) T)
		          :EMPTY-WORD)
		     (:OR (:CAT (EQL :Y)
				(:AND LIST
				     (RTE (:CAT T T))))
		          :EMPTY-WORD))
	       (:CAT (:OR (:CAT (EQL :Y)
				(:AND LIST
				     (RTE (:CAT T T))))
		          :EMPTY-WORD)
		     (:OR (:CAT (EQL :X) T)
		          :EMPTY-WORD)))))
       (DESTRUCTURING-BIND (U V &KEY X ((:Y (Y1 Y2)) '(NIL NIL)))
	   A
	 (INCF N)
	 (ASSERT-TRUE (= U 1))
	 (ASSERT-TRUE (= V 2))
	 (ASSERT-TRUE (= X 3))
	 (ASSERT-TRUE (= Y1 4))
	 (ASSERT-TRUE (= Y2 5)))))
    (assert-true (= n 1))))

(define-test test/destructuring-case-7
  (LET ((N 0)
	(A '( :x 3 :y (4 5))))
    (TYPECASE A
      ((NOT LIST) NIL)
      ((RTE
	(:CAT 
	      
	 (:OR
	  (:CAT (:OR (:CAT (EQL :X) T)
		 :EMPTY-WORD)
		(:OR (:CAT (EQL :Y)
			   (:AND LIST
				(RTE (:CAT T T))))
		 :EMPTY-WORD))
	  (:CAT (:OR (:CAT (EQL :Y)
			   (:AND LIST
				(RTE (:CAT T T))))
		 :EMPTY-WORD)
		(:OR (:CAT (EQL :X) T)
		 :EMPTY-WORD)))))
       (DESTRUCTURING-BIND ( &KEY X ((:Y (Y1 Y2)) '(NIL NIL)))
	   A
	 (INCF N)
	 (ASSERT-TRUE (= X 3))
	 (ASSERT-TRUE (= Y1 4))
	 (ASSERT-TRUE (= Y2 5)))))
    (assert-true (= n 1))))


(define-test test/destructuring-case-8b
  (destructuring-lambda-list-to-rte '(&whole llist a (b c) &rest keys &key x y z &allow-other-keys)
					 :type-specifiers '((a fixnum)
							    (b fixnum)
							    (c fixnum)
							    (x symbol)
							    (y string)
							    (z list))))

(define-test test/destructuring-case-8
  ;; with &allow-other-keys
  (assert-true (canonicalize-pattern (destructuring-lambda-list-to-rte '(&key x y z &allow-other-keys)))
	       (canonicalize-pattern
		'(:or
		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :x) t (:0-* (not (member :y :z)) t)) :empty-word)
		    (:or (:cat (eql :y) t (:0-* (not (member :z)) t)) :empty-word)
		    (:or (:cat (eql :z) t (:0-* t t)) :empty-word)))
 
		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :x) t (:0-* (not (member :y :z)) t)) :empty-word)
		    (:or (:cat (eql :z) t (:0-* (not (member :y)) t)) :empty-word)
		    (:or (:cat (eql :y) t (:0-* t t)) :empty-word)))
 
		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :y) t (:0-* (not (member :x :z)) t)) :empty-word)
		    (:or (:cat (eql :x) t (:0-* (not (member :z)) t)) :empty-word)
		    (:or (:cat (eql :z) t (:0-* t t)) :empty-word)))
 
		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :y) t (:0-* (not (member :x :z)) t)) :empty-word)
		    (:or (:cat (eql :z) t (:0-* (not (member :x)) t)) :empty-word)
		    (:or (:cat (eql :x) t (:0-* t t)) :empty-word)))

		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :z) t (:0-* (not (member :x :y)) t)) :empty-word)
		    (:or (:cat (eql :x) t (:0-* (not (member :y)) t)) :empty-word)
		    (:or (:cat (eql :y) t (:0-* t t)) :empty-word)))

		  (:and (:0-* keyword t)
		   (:cat (:or (:cat (eql :z) t (:0-* (not (member :x :y)) t)) :empty-word)
		    (:or (:cat (eql :y) t (:0-* (not (member :x)) t)) :empty-word)
		    (:or (:cat (eql :x) t (:0-* t t)) :empty-word))))))

  (assert-equal (canonicalize-pattern (destructuring-lambda-list-to-rte '(&key x y z)))
		(canonicalize-pattern
		 '(:or
		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :x) t (:0-* (member :x) t)) :empty-word)
		     (:or (:cat (eql :y) t (:0-* (member :x :y) t)) :empty-word)
		     (:or (:cat (eql :z) t (:0-* (member :x :y :z) t)) :empty-word)))

		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :x) t (:0-* (member :x) t)) :empty-word)
		     (:or (:cat (eql :z) t (:0-* (member :x :z) t)) :empty-word)
		     (:or (:cat (eql :y) t (:0-* (member :x :y :z) t)) :empty-word)))
		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :y) t (:0-* (member :y) t)) :empty-word)
		     (:or (:cat (eql :x) t (:0-* (member :x :y) t)) :empty-word)
		     (:or (:cat (eql :z) t (:0-* (member :x :y :z) t)) :empty-word)))
		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :y) t (:0-* (member :y) t)) :empty-word)
		     (:or (:cat (eql :z) t (:0-* (member :y :z) t)) :empty-word)
		     (:or (:cat (eql :x) t (:0-* (member :x :y :z) t)) :empty-word)))
		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :z) t (:0-* (member :z) t)) :empty-word)
		     (:or (:cat (eql :x) t (:0-* (member :x :z) t)) :empty-word)
		     (:or (:cat (eql :y) t (:0-* (member :x :y :z) t)) :empty-word)))
		   (:and (:0-* keyword t)
		    (:cat (:or (:cat (eql :z) t (:0-* (member :z) t)) :empty-word)
		     (:or (:cat (eql :y) t (:0-* (member :y :z) t)) :empty-word)
		     (:or (:cat (eql :x) t (:0-* (member :x :y :z) t)) :empty-word)))))))

(define-test test/destructuring-case-9
  (let ((n 0))
    (destructuring-case '(:x (1 2))
      ((&key ((:x (a b)) '(nil nil)))
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (incf n)))
    (assert-true (equal n 1))))

(define-test test/destructuring-case-10
  (let ((n 0))
    (destructuring-case '(:x (1 2))
      ((&key ((:x (a b c)) '(nil nil nil)))
       (declare (ignore a b c))
       nil)
      ((&key ((:x (a b)) '(nil nil)))
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (incf n)))
    (assert-true (equal n 1))))


(define-test test/destructuring-case-11
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30))
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil)))
       (declare (ignore a b c d))
       nil)
      ((&key ((:x (a b)) '(nil nil))
	     ((:y (u v w)) '(nil nil nil)))
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n)))
    (assert-true (equal n 1))))

(define-test test/destructuring-case-12
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30))
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil)))
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil)))
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n)))
    (assert-true (equal n 1))))


(define-test test/destructuring-case-13
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30) :z 100)
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil)))
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil)))
       (declare (ignore u v w a b))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     z)
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (assert-true (equal z 100))
       (incf n)))
    
    (assert-true (equal n 1))))

(define-test test/destructuring-case-13b
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30) :y -1 :z 100 :x -2)
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil)))
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil)))
       (declare (ignore a b u v w))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     z)
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (assert-true (equal z 100))
       (incf n)))
    
    (assert-true (equal n 1))))

(define-test test/destructuring-case-14
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30) :z 100)
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil))
	     &allow-other-keys)
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     &allow-other-keys)
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n))
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     z)
       (declare (ignore u v w a b z))
       nil))
    
    (assert-true (equal n 1))))

(define-test test/destructuring-case-14b
  (let ((n 0))
    (destructuring-case '(:x (1 2) :y (10 20 30) :z 100 :a 12)
      ((&key ((:x (a b c)) '(nil nil nil))
	     ((:y (d)) '(nil))
	     &allow-other-keys)
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     &allow-other-keys)
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n))
      ((&key ((:y (u v w)) '(nil nil nil))
	     ((:x (a b)) '(nil nil))
	     z)
       (declare (ignore u v w a b z))
       nil))
    
    (assert-true (equal n 1))))


(define-test test/destructuring-case-15
  (let ((n 0))
    (destructuring-case '((1 2) 3)
      ((a b c)
       (declare (type float a b) (type integer c))
       (list a b c))
      ((a b c)
       (declare (type fixnum a b) (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type float a b) (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type integer a b) (type integer c))
       (incf n)
       (assert-true (= a 1))
       (assert-true (= b 2))
       (assert-true (= c 3))))
    (assert-true (= 1 n))))

(define-test test/destructuring-case-15b
  (let ((n 0))
    (destructuring-case '((1 2) 3)
      ((a b c)
       (declare (type float a b) (type integer c))
       (list a b c))
      ((a b c)
       (declare (type fixnum a b) (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type float a b) (type integer c))
       (list a b c)))
    (assert-true (= 0 n))))

(define-test test/gather-type-declarations
  (assert-false (set-exclusive-or '((a fixnum)
				    (b fixnum)
				    (c string)
				    (d bignum)
				    (e float))
				  (gather-type-declarations '((declare (type fixnum a b))
								   (declare (type string c))
								   (declare (bignum d)
								            (float e))
								   (declare (ignore x y z))
								   ()))
				  :test #'equal)))
								 
