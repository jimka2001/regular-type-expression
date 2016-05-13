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
		'(:and (:* t)
		  (:cat (:cat) (:cat)
		   (:and (:* t)
		    (:and (:* keyword t) (:cat (:* (not (eql :x)) t) (:? (eql :x) t (:* t)))
		     (:cat (:* (not (eql :y)) t) (:? (eql :y) t (:* t)))
		     (:cat (:* (not (eql :z)) t) (:? (eql :z) t (:* t)))))))))

  (assert-equal (canonicalize-pattern (destructuring-lambda-list-to-rte '(&key x y z)))
		(canonicalize-pattern
		 '(:and (:0-* (member :x :y :z) t)
		   (:cat (:0-* (not (eql :x)) t) (:or :empty-word (:cat (eql :x) t (:0-* t))))
		   (:cat (:0-* (not (eql :y)) t) (:or :empty-word (:cat (eql :y) t (:0-* t))))
		   (:cat (:0-* (not (eql :z)) t) (:or :empty-word (:cat (eql :z) t (:0-* t)))))))

    (rte::map-permutations (lambda (perm)
			     (assert-test (equal :here
						 (destructuring-case (mapcan (lambda (key)
									       (list key 12))
									     perm)
						   ((&key (x 1) (y 1) (z 1))
						    (declare (type fixnum x y z))
						    (assert-test (equal 12 x))
						    (assert-test (equal 12 y))
						    (assert-test (equal 12 z))
						    :here)))))
		    '(:x :y :z))

  (rte::map-permutations (lambda (perm)
			   (assert-test (equal :here
					       (destructuring-case (mapcan (lambda (key)
									     (list key 12))
									   perm)
						 ((&key (x 1) (y 1) (z 1) &allow-other-keys)
						  (declare (type fixnum x y z))
						  (assert-test (equal 12 x))
						  (assert-test (equal 12 y))
						  (assert-test (equal 12 z))
						  :here)))))
		    '(:w :x :y :z)))

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

(define-test test/destructuring-case-16
  (assert-true (equal '(1 x 0)
		      (destructuring-case '(x)
			((name &key (count 0))
			 (declare (type fixnum count))
			 (list 1 name count))
			((name &key count)
			 (list 2 name count)))))

  (assert-true (equal '(2 x y)
		      (destructuring-case '(x :count y)
			((name &key (count 0))
			 (declare (type fixnum count))
			 (list 1 name count))
			((name &key (count 'z))
			 (declare (type symbol count))
			 (list 2 name count)))))

  (assert-true (equal '(1 x 0)
		      (destructuring-case '(x)
			((name &key (count 0))
			 (declare (type fixnum count))
			 (list 1 name count))
			((name &key (count 42))
			 (declare (type fixnum count))
			 (list 2 name count))))))
		    

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
								 
(define-test test/destructuring-methods
  (assert-true (= 6 (rte:destructuring-methods '(1 2 3) (:call-next-method cnm)
		      ((a b c)
		       (declare (type number a b c)
				(ignore a b c))
		       (* 2 (cnm)))
		      ((a b c)
		       (declare (type fixnum a b c)
				(ignore a b c))
		       3))))
  (assert-error 'error (rte:destructuring-methods '(1 2 3) (:call-next-method cnm)
			 ((a b)
			  (declare (ignore a b))
			  1)
			 ((a b c)
			  (declare (ignore a b c))
			  (cnm)))))


(define-test test/destructuring-case-allow-other-keys
  (let ((data '(1 (2 3) :a 12
		:x name :x "not-symbol"
		:b 13 :z nil :z 14 :y "hello" :a 15 :x 16 :y 17 :z 18)))
    (destructuring-bind (&whole llist a (b c) 
			 &rest keys
			 &key (x t) (y "") z &allow-other-keys) data
      (declare (type fixnum a b c)
	       (type symbol x)
	       (type string y)
	       (type list z))
      (assert-true (= a 1))
      (assert-true (= b 2))
      (assert-true (= c 3))
      (list a b c x y z llist keys))

    (assert-false
     (null
      (destructuring-case data
	((&whole llist a (b c) 
		 &rest keys
		 &key (x t) (y "") z &allow-other-keys)
	 (declare (type fixnum a b c)
		  (type symbol x)
		  (type string y)
		  (type list z))
	 (assert-true (= a 1))
	 (assert-true (= b 2))
	 (assert-true (= c 3))
	 (assert-true (eq x 'name))
	 (assert-true (string= "hello" y))
	 (assert-true (eq nil z))
	 (assert-false (null keys))
	 llist))))))

(define-test test/destructuring-case-allow-other-keys-2
  (let ((data '(1 (2 3)
		:x name :y 3.14 :z 14)))
    (assert-error 'error
		  (destructuring-bind (&whole llist a (b c) 
				       &rest keys
				       &key (x t) (y "") z &allow-other-keys) data
		    (declare (type fixnum a b c)
			     (type symbol x)
			     (type string y)
			     (type list z))
		    (list llist a b c keys x y z)))

    (assert-true
     (equal
      'default
      (destructuring-case data
	((&whole llist a (b c) 
		 &rest keys
		 &key (x t) (y "") z &allow-other-keys)
	 (declare (type fixnum a b c)
		  (type symbol x)
		  (type string y)
		  (type list z))
	 (list llist a b c keys x y z))
	((&rest args)
	 (declare (ignore args))
	 'default))))
))
       
(defun test-graph-2keys ()
  (let ((pattern (rte::destructuring-lambda-list-to-rte
		  '(&key (x t) (y "") &allow-other-keys)
		  :type-specifiers
		  (gather-type-declarations
		   '((declare (type symbol x)
		      (type string y)))))))
    (format t "~S~%" pattern)
    (ndfa::ndfa-to-dot
     (rte::make-state-machine pattern)
     #p"/tmp/dfa2.png" :state-legend nil)))

(defun test-graph-3keys ()
  (let ((pattern (rte::destructuring-lambda-list-to-rte
		  '(&whole llist a (b c)
		    &rest keys
		    &key (x t) (y "") (z 12) ;; &allow-other-keys
		    )
		  :type-specifiers
		  (gather-type-declarations
		   '((declare (type fixnum a b c)
		      (type symbol x)
		      (type string y)
		      (type fixnum z)))))))
    (format t "~S~%" pattern)
    (ndfa::ndfa-to-dot 
     (rte::make-state-machine pattern)
     #p"/tmp/dfa3.png"
     :transition-abrevs '((t t1)
			  (list t2)
			  (fixnum t3)
			  (symbol t4)
			  (keyword t5)
			  (string t6)
			  ((and list (rte (:cat fixnum fixnum))) t7)
			  ((eql :x) t8)
			  ((eql :y) t9)
			  ((eql :z) t10)
			  ((eql :x :y) t11)
			  ((eql :x :z) t12)
			  ((eql :y :z) t13)
			  ((eql :x :y :z) t14)
			  ((and keyword (not (eql :x))) t15)
			  ((and keyword (not (eql :y))) t16)
			  ((and keyword (not (eql :z))) t17)
			  ((and keyword (not (member :x :y))) t18)
			  ((and keyword (not (member :x :z))) t19)
			  ((and keyword (not (member :y :z))) t20)
			  ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-b ()
  ;; (&key (x t) (y "") (z 12) &allow-other-keys)
  ;; (declare (type symbol x)
  ;;          (type string y)
  ;;          (type fixnum z))
  (let ((pattern '(:and (:* keyword t)
		   ;; the first :x is followed by a symbol
		   (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))

		   ;; the first :y is followed by a string
		   (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))

		   ;; the first :z is followed by a fixnum
		   (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))))
    (format t "~S~%" pattern)
    (ndfa::ndfa-to-dot 
     (rte::make-state-machine pattern)
     #p"/tmp/dfa3.png"
     :transition-abrevs '((t t1)
			  (list t2)
			  (fixnum t3)
			  (symbol t4)
			  (keyword t5)
			  (string t6)
			  ((and list (rte (:cat fixnum fixnum))) t7)
			  ((eql :x) t8)
			  ((eql :y) t9)
			  ((eql :z) t10)
			  ((eql :x :y) t11)
			  ((eql :x :z) t12)
			  ((eql :y :z) t13)
			  ((eql :x :y :z) t14)
			  ((and keyword (not (eql :x))) t15)
			  ((and keyword (not (eql :y))) t16)
			  ((and keyword (not (eql :z))) t17)
			  ((and keyword (not (member :x :y))) t18)
			  ((and keyword (not (member :x :z))) t19)
			  ((and keyword (not (member :y :z))) t20)
			  ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-c ()
  (let ((pattern  '(:and
		    (:and (:* keyword t)
		     (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))
		     (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))
		     (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))
		    (:not (:and (:* keyword t)
			   (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))
			   (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))
			   (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))))))
    (format t "~S~%" pattern)
    (ndfa::ndfa-to-dot 
     (rte::make-state-machine pattern)
     #p"/tmp/dfa4.png"
     :transition-abrevs '((t t1)
			  (list t2)
			  (fixnum t3)
			  (symbol t4)
			  (keyword t5)
			  (string t6)
			  ((and list (rte (:cat fixnum fixnum))) t7)
			  ((eql :x) t8)
			  ((eql :y) t9)
			  ((eql :z) t10)
			  ((eql :x :y) t11)
			  ((eql :x :z) t12)
			  ((eql :y :z) t13)
			  ((eql :x :y :z) t14)
			  ((and keyword (not (eql :x))) t15)
			  ((and keyword (not (eql :y))) t16)
			  ((and keyword (not (eql :z))) t17)
			  ((and keyword (not (member :x :y))) t18)
			  ((and keyword (not (member :x :z))) t19)
			  ((and keyword (not (member :y :z))) t20)
			  ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-d (pattern)
  (format t "~S~%" pattern)
  (ndfa::ndfa-to-dot 
   (rte::prune (rte::make-state-machine pattern))
   #p"/tmp/dfa4.png"
    :transition-legend t
   :state-legend t))

