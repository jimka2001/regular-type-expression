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

;; TODO add some tests with satisfies types. (satisfies oddp) (satisfies evenp) etc.

(define-test type/match-empty
  (assert-true (typep #() '(rte (:cat))))
  (assert-true (typep () '(rte (:cat))))
  (assert-false (typep #(t) '(rte (:cat))))
  (assert-false (typep #(t) '(rte (:cat)))))

(define-test type/typep-1
  (assert-true (typep '(:x 3 :y 4)
		      '(RTE
			(:CAT
			 (:OR
			  (:CAT (:OR (:CAT (EQL :X) T)
				 :EMPTY-WORD)
			   (:OR (:CAT (EQL :Y) T)
			    :EMPTY-WORD))
			  (:CAT (:OR (:CAT (EQL :Y) T)
				 :EMPTY-WORD)
			   (:OR (:CAT (EQL :X) T)
			    :EMPTY-WORD))))))))

(define-test type/typep-2
  (assert-true (typep '(:x 3 :y 4)
		      '(RTE
			(:OR
			 (:CAT (:OR (:CAT (EQL :X) T)
				:EMPTY-WORD)
			  (:OR (:CAT (EQL :Y) T)
			   :EMPTY-WORD))
			 (:CAT (:OR (:CAT (EQL :Y) T)
				:EMPTY-WORD)
			  (:OR (:CAT (EQL :X) T)
			   :EMPTY-WORD)))))))

(define-test type/typep-2
  (assert-true (typep '(:x 3 :y 4)
		      '(RTE
			(:OR
			 (:CAT (:OR (:CAT (EQL :X) T)
				:EMPTY-WORD)
			  (:OR (:CAT (EQL :Y) T)
			   :EMPTY-WORD))
			 (:CAT (:OR (:CAT (EQL :Y) T)
				:EMPTY-WORD)
			  (:OR (:CAT (EQL :X) T)
			   :EMPTY-WORD)))))))



(define-test type/typep-13
  (assert-true (typep '(:x 3 :y 4)
		      '(RTE
			(:OR
			 (:CAT (:OR (:CAT (EQL :X) T)
				:EMPTY-WORD)
			  (:OR (:CAT (EQL :Y) T)
			   :EMPTY-WORD)))))))



(define-test type/match-sequence
  (assert-true (rte:match-sequence '(1 2 3) '(:cat number number number)))

  (let ((2d (make-array '(5 4)
			:initial-contents '((b0 b1 b2 b3)
					    (b0 1  2  b3)
					    (b0 2  3  b3)
					    (b0 4  5  b3)
					    (b0 b1 b2 b3)))))
    (assert-test (rte:match-sequence (make-instance '2d-array:row-vector :2d-array 2d :row 0)
				     '(:1-* symbol)))
    (assert-test (rte:match-sequence (make-instance '2d-array:column-vector :2d-array 2d :column 1)
				     '(:cat symbol (:0-* number) symbol)))))

  

(define-test type/2d-array
  
  (let ((2d (make-array '(5 4)
			:initial-contents '((b0 b1 b2 b3)
					    (b0 1  2  b3)
					    (b0 2  3  b3)
					    (b0 4  5  b3)
					    (b0 b1 b2 b3)))))
    (assert-test (typep (make-instance '2d-array:row-vector :2d-array 2d :row 0)
			'(rte (:1-* symbol))))
    (assert-test (typep (make-instance '2d-array:row-vector :2d-array 2d :row 1)
		       '(rte (:1 symbol (:1-* fixnum) symbol))))
    (assert-test (typep (make-instance '2d-array:column-vector :2d-array 2d :column 0)
			'(rte (:1-* (eql b0)))))

    (assert-test (typep (make-instance '2d-array:column-vector :2d-array 2d :column 1)
			'(rte (:1 (eql b1) (:1-* fixnum) (eql b1)))))
    
    (assert-test (typep (make-instance '2d-array:vector-of-rows :2d-array 2d)
			'(rte (:1 (rte (:1-* symbol))
				   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
				   (rte (:1-* symbol))))))

    (assert-test (typep (make-instance '2d-array:vector-of-columns :2d-array 2d)
			'(rte (:1 (rte (:1-* symbol))
				   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
				   (rte (:1-* symbol))))))

    (assert-false (typep (make-instance '2d-array:vector-of-columns :2d-array 2d)
			'(rte (:1 (rte (:1-* symbol))
				   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
				   (rte (:1-* symbol))
				   t))))
    ))


(defun type/declaration2 ()
  (typep nil '(rte
	       (:0-* number number)))
  (typep nil '(rte
	       (:1 (rte
		    (:0-* number number)))))
  
  (typep nil '(rte
	       (:1-* (rte
		      (:0-* number number)))))
  (funcall (lambda (x)
   	     (declare (type (rte
   			     (:1-* (rte
				    (:0-* number number))))
   			    x))
   	     x)
   	   '((1 1) (2 3) (5 6.0)))
  )

(define-test type/declaration2
  (assert-true (type/declaration2)))

(define-test type/declaration
  (assert-true (equal '(1 2 3)
		      (funcall (lambda (x)
				 (declare (type (rte (:0-* t))
						x))
				 x) '(1 2 3))))
  (assert-error 'error (funcall (lambda (x)
				  (declare (type (rte (:0-* symbol))
						 x))
				  x) '(1 2 3)))
  
  (assert-error 'error
   		(funcall (lambda (x)
   			   (declare (type (rte
   					   (:1-* (rte (:0-* symbol))))
   					  x))
			   x)
   			 '((1 1) (2 nil) (5 6.0))))
  (assert-true (funcall (lambda (x)
   			  (declare (type (rte (:1-* (rte
								       (:cat number number))))
   					 x))
			  x)
   			'((1 1) (2 3) (5 6.0))))
  (assert-error 'error
   		(funcall (lambda (x)
   			   (declare (type (rte
   					   (:1-* (rte
   						  (:cat number number)))) x))
			   x)
   			 '((1 1) (2 nil) (5 6.0)))

   		)
  )





(define-test type/remove-redundant-types-2
    ;; canot remove any redundant if one of the types is not a valid lisp type
    (assert-true (equal (rte::remove-redundant-types '(t (:cat t t)) :and)
			'(t (:cat t t))))
    (assert-true (equal (rte::remove-redundant-types '((:cat t t) t) :and)
			'((:cat t t) t)))
    (assert-true (equal (rte::remove-redundant-types '(t (:cat t t)) :or)
			'(t (:cat t t))))
    (assert-true (equal (rte::remove-redundant-types '((:cat t t) t) :or)
			'((:cat t t) t))))

(define-test type/remove-redundant-types
  (assert-true (equal '((integer 0 1))
		      (rte::remove-redundant-types '(integer (integer 0 1) (integer 0 3) number)
						   :and)))
  (assert-true (equal '(number)
		      (rte::remove-redundant-types '(integer (integer 0 1) (integer 0 3) number)
						   :or)))
  (assert-true (equal '(integer float)
		      (rte::remove-redundant-types '(integer (integer 0) (integer 0 5) float (float 0.0) (float 0.0 100.0))
						   :or))))

(define-test type/alphabetize
  (assert-true (equal '(float number string) (rte::alphabetize '(number float string))))
  (assert-true (equal (rte::alphabetize '(number (number 10 12) (number 2 5) (number 6) float))
		      '(float number (number 2 5) (number 6) (number 10 12)))))

(define-test type/canonicalize-pattern

    (assert-true (equal '(:cat symbol number) (canonicalize-pattern '(:cat (type symbol) (type number)))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:cat :empty-word :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-word :empty-set))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-set :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-set float))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:cat))))
    (assert-true (equal 'float      (canonicalize-pattern '(:cat :empty-word float))))
    (assert-true (equal '(:cat float string) (canonicalize-pattern '(:cat float :empty-word string :empty-word))))

    (assert-true (equal :empty-word (canonicalize-pattern '(:and :empty-word :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-word :empty-set))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-set :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-set float))))
    (assert-true (equal '(:0-* t)   (canonicalize-pattern '(:and))))
    (assert-true (equal 'float  (canonicalize-pattern '(:and (:0-* t) float))))
    ;; (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-word float))))
    (assert-true (equal 'float      (canonicalize-pattern '(:and float float))))

    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-word :empty-word))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-word :empty-set))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-set :empty-word))))
    (assert-true (equal 'float      (canonicalize-pattern '(:or :empty-set float))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:or))))
    (assert-true (equal 'float      (canonicalize-pattern '(:or float float))))

    (assert-true (equal '(:or float string)
			(canonicalize-pattern '(:or string float))))
    
    (assert-true (equal '(:cat (:0-* float) (:or float string))
			(canonicalize-pattern '(:cat (:0-* float) (:or string float)))))

    (assert-true (equal '(:OR (:AND A D E F) ;; currently failing, must fix
			      (:AND A D E G)
			      (:AND B D E F)
			      (:AND B D E G)
			      (:AND C D E F)
			      (:AND C D E G))
			(canonicalize-pattern '(:and (:or A B C) D E (:or F G)))))

    ;; redundant types

    (assert-true (equal 'number (canonicalize-pattern '(:or float number))))
    (assert-true (equal 'float  (canonicalize-pattern '(:and float number))))
    (assert-true (equal :empty-set (canonicalize-pattern '(:and float string))))


    ;; test to fix error
    (assert-true (equal :empty-word (canonicalize-pattern '(:AND (:0-* T) (:CAT (:CAT) (:CAT))))))
    (assert-true (equal '(:and t :empty-word)  (canonicalize-pattern '(:AND t (:CAT (:CAT) (:CAT))))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:CAT (:CAT) (:CAT)))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:AND (:0-* T) :empty-word))))
    )



(define-test type/traverse-pattern
  (assert-true (equal :empty-word (rte::traverse-pattern :empty-word :client #'identity)))
  (assert-true (equal :empty-set (rte::traverse-pattern :empty-set :client #'identity)))
  (assert-true (equal 'float (rte::traverse-pattern 'float :client #'identity)))

  (assert-true (equal '(:or float number) (rte::traverse-pattern '(:or float number) :client #'identity)))
  (assert-true (equal '(:and float number) (rte::traverse-pattern '(:and float number) :client #'identity)))
  (assert-true (equal '(:cat float number) (rte::traverse-pattern '(:cat float number) :client #'identity)))

  (assert-true (equal '(:0-* float number) (rte::traverse-pattern '(:0-* float number) :client #'identity)))

  (assert-true (equal '(:0-* float number) (rte::traverse-pattern '(:0-* float number) :f-type #'identity))))

(define-test type/first-types
  (assert-false (set-exclusive-or '(float)
				  (rte::first-types '(:cat float
						      integer
						      (:0-* symbol (:0-* number) symbol)
						      integer
						      (number 0 4)
						      (rte (:0-1 symbol))))
				  :test #'equal))
  (assert-false (set-exclusive-or '((integer 0 1) (integer 2 9))
				  (rte::first-types '(:cat (:0-* (integer 0 1))
						      (integer 2 9)))
				  :test #'equal))
  )

(define-test type/equivalent-types-p
  (assert-true (rte::equivalent-types-p '(member 1 2 3) '(or (eql 1) (eql 2) (eql 3)))))

(define-test type/nullable
  (assert-false (rte::nullable :empty-set))
  (assert-true  (rte::nullable :empty-word))
  (assert-false (rte::nullable 'float))
  (assert-false (rte::nullable '(integer 1 3)))
  (assert-true  (rte::nullable '(:cat (:0-1 number) (:0-1 string) (:0-* float) :empty-word)))
  (assert-true  (rte::nullable '(:0-* number)))
  (assert-false (rte::nullable '(:1-* number)))
  (assert-true  (rte::nullable '(:0-1 number)))
  (assert-true  (rte::nullable '(:and (:0-1 number)
				 (:0-* number))))
  (assert-false (rte::nullable '(:and (:0-1 number)
				 (:1-* number))))
  (assert-true  (rte::nullable '(:or (:0-1 number)
				 (:1-* number))))
  (assert-true  (rte::nullable '(:or (:0-1 string)
				 (:0-* number))))
  (assert-false (rte::nullable '(:or string
				 (:1-* number)))))
  

(define-test type/derivative

  ;; trivial cases
  (assert-true (rte::derivative 'float 'float))
  (assert-true (equal :empty-word (rte::derivative 'float 'float)))
  (assert-true (equal :empty-set  (rte::derivative :empty-word 'float)))
  (assert-true (equal :empty-set  (rte::derivative 'float 'string)))
  (assert-true (equal :empty-set  (rte::derivative :empty-set 'float)))

  ;; or/and

  (assert-true (equal :empty-word (rte::derivative '(:or float float) 'float)))
  (assert-true (equal :empty-word (rte::derivative '(:and float float) 'float)))
  (assert-true (equal :empty-word (rte::derivative '(:or  float string) 'float)))
  (assert-true (equal :empty-word (rte::derivative '(:or  float string) 'string)))


  (assert-true (equal :empty-word
		      (rte::derivative '(:cat float) 'float)))
  (assert-true (equal 'string
		      (rte::derivative '(:cat float string) 'float)))
  (assert-true (equal '(:0-* string)
		      (rte::derivative '(:cat float (:0-* string)) 'float)))
  (assert-true (equal '(:0-* (:or float string))
		      (rte::derivative '(:cat float (:0-* (:or string float))) 'float)))
    
  (assert-true (equal '(:0-* float)
		      (rte::derivative '(:0-* float) 'float)))

  (assert-true (equal '(:0-* float)
		      (rte::derivative '(:1-* float) 'float)))

  (assert-true (equal (rte::canonicalize-pattern '(:or (:cat (:0-* float) (:or float string))  :empty-word))
		      (rte::derivative '(:cat (:0-* float) (:or string float)) 'float)))
  (assert-true (equal (rte::canonicalize-pattern '(:or (:cat (:0-* float) (:0-* (:or float string)))
						   (:0-* (:or float string))))
		      (rte::derivative '(:cat (:0-* float) (:0-* (:or string float))) 'float)))

  )

(define-test type/derivative-2
  (assert-true (equal (rte::canonicalize-pattern (rte::derivative '(:OR
								    (:CAT (:OR (:CAT (EQL :X) T)
									   :EMPTY-WORD)
								     (:OR (:CAT (EQL :Y) T)
								      :EMPTY-WORD))
								    (:CAT (:OR (:CAT (EQL :Y) T)
									   :EMPTY-WORD)
								     (:OR (:CAT (EQL :X) T)
								      :EMPTY-WORD)))
								  '(eql :y)))
		      (rte::canonicalize-pattern `(:or ,(rte::derivative '(:CAT (:OR (:CAT (EQL :X) T)
										:EMPTY-WORD)
									  (:OR (:CAT (EQL :Y) T)
									   :EMPTY-WORD))
									'(eql :y))
						       ,(rte::derivative '(:CAT (:OR (:CAT (EQL :Y) T)
										:EMPTY-WORD)
									  (:OR (:CAT (EQL :X) T)
									   :EMPTY-WORD))
									'(eql :y)))))))


(define-test type/derivative-3
  ;; D(R.S) = D(R).S + v(R).D(S)
  ;; if R is nullable
  ;;   =  D(R).S + D(S)
  ;; else
  ;;   =  D(R).S
  (let ((R '(:OR (:CAT (EQL :X) T)
	     :EMPTY-WORD))
	(S '(:OR (:CAT (EQL :Y) T)
	     :EMPTY-WORD)))
    (assert-true (rte::nullable R))
    ;; RS/y
    (assert-true (equal (rte::canonicalize-pattern (rte::derivative `(:cat ,R ,S)
								     '(eql :y)))
			(rte::canonicalize-pattern `(:or (:cat ,(rte::derivative R '(eql :y)) ,S)
							 ,(rte::derivative S '(eql :y))))))
    (assert-true (rte::nullable R))
    (assert-true (rte::nullable S))
    (assert-true (rte::nullable `(:cat ,R ,S)))
    (assert-true (rte::nullable `(:cat ,S ,R)))
    ;; RS/x
    (assert-true (equal (rte::canonicalize-pattern (rte::derivative `(:cat ,R ,S) '(eql :x)))
			(rte::canonicalize-pattern '(:cat t (:or :empty-word
							     (:cat (eql :y) t))))))
    ;; RS/y
    (assert-true (equal (rte::canonicalize-pattern (rte::derivative `(:cat ,R ,S)
								    '(eql :y)))
			t))
    ;; SR/x
    (assert-true (equal (rte::canonicalize-pattern (rte::derivative `(:cat ,S ,R) '(eql :x)))
			t))
    
    ;; SR/y
    (assert-true (equal (rte::canonicalize-pattern (rte::derivative `(:cat ,S ,R) '(eql :y)))
			(rte::canonicalize-pattern '(:cat t (:or :empty-word
							     (:cat (eql :x) t))))))

    (assert-false (equal t
			 (rte::canonicalize-pattern (rte::derivative `(:or (:cat ,R ,S)
									   (:cat ,S ,R))
								     '(eql :x)))))

    (assert-false (equal t
			 (rte::canonicalize-pattern (rte::derivative `(:or (:cat ,R ,S)
									   (:cat ,S ,R))
								     '(eql :y)))))
    
    ))
		      


(define-test type/rte-type
  (assert-true (typep '(1 x) '(rte (:cat number symbol))))
  (assert-true (typep '(2 x) '(rte (:cat (:or symbol number)
					symbol))))
  (assert-true (typep '()      '(rte (:0-or-more number))))
  (assert-true (typep '(1 2 3) '(rte (:0-or-more number))))

  (assert-true (typep #(1 2 3) '(rte (:1 number number number))))
  (assert-true (typep #(1 2 3 5) '(rte (:0-* integer))))

  )

(define-test type/test1
  (dolist (pattern '((rte (:or (:cat (:1-* (eql 0)) (:0-* (eql 1)))
				  (:cat (:0-* (eql 0)) (:1-* (eql 1)))))
		     (rte (:and (:1-* (:or (eql 0) (eql 1)))
				  (:cat (:0-* (eql 0)) (:0-* (eql 1)))))
		     (rte (:or (:cat (:1-* (eql 0)) (:0-* (eql 1)))
				  (:1-* (eql 1))))))
    (dolist (yes '(#*0
		   #*1
		   #*01
		   #*001
		   #*0001
		   #*0111
		   #*0011))
      (assert-true (typep yes pattern)))
    (dolist (no '(#()
		  #*10
		  #*010
		  #*0110
		  #(0 1 2)))
      (assert-false (typep no pattern)))))

(define-test type/test2
  (let ((pattern '(rte (:and (:cat integer number)
			            (:cat number integer)))))
    (dolist (yes '((1 1)
		   (1 2)))
      (assert-true (typep yes pattern)))
    (dolist (no '((1.0 2.0)
		  (3 4.0)
		  (5.0 6)
		  (1 2 3)))
      (assert-false (typep no pattern)))))

					

(define-test type/rte
  (assert-true (typep '(1 x) '(rte (:1 number symbol))))
  
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:1-or-more (eql 1)) (:0-or-1 (eql 0))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:0-or-more (eql 1)) (:0-or-1 (eql 0))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-true (typep '(0)       '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-true (typep '()        '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '()       '(rte (:1 (:0-or-more (eql 1)) (:1-or-more (eql 0))))))
  (assert-true (typep '(1 1 1)   '(rte (:1-or-more (eql 1)))))
  (assert-true (typep '(1 1 1)   '(rte (:0-or-more (eql 1)))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 1)) (:0-or-more (eql 1))))))
  (assert-false (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 2)) (:0-or-more (eql 3))))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 2)) (:0-or-more (eql 1))))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 1)) (:0-or-more (eql 2))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:1-or-more (eql 1)) (:0-or-more (eql 1))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:0-or-more (eql 1)) (:1-or-more (eql 1))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:and (:1-or-more (eql 1)) (:0-or-more (eql 1)))
					(eql 0)))))
  (assert-true (typep '(1 1 1)   '(rte (:1 (:1-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '(1 1 1)   '(rte (:and (:1-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '(1 1 1)  '(rte (:and (:0-or-more (eql 1)) (:1-or-more (eql 0))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:0-or-more (eql 1)) (:1-or-more (:or (eql 1) (eql 0)))))))
  (assert-true (typep '(x 100 y 200 z 300 "hello" "world")
		      '(rte (:1 (:0-or-more symbol number) (:1-or-more string)))))

  ;; recursive use of rte
  (assert-false (typep '((1 1)
			 (2 2 x 2 2))
		       '(rte (:0-or-more (rte (:1-or-more number))))))
  (assert-true (typep '((1 1)
			(2 2 x 2 2))
		      '(rte (:0-or-more (rte (:1-or-more (:or symbol number)))))))
  (assert-true (typep '((1 1) (2 2 2 2)) '(rte (:0-or-more (rte (:1-or-more number))))))
  

  ;; zero-or-more
  (assert-true (typep '() '(rte (:0-* number))))
  (assert-true (typep '(nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1 2 3 4) '(rte (:1 (:0-* null) (:0-* number)))))
  
  ;; one-or-more
  (assert-false (typep '() '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1 2 3 4) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-false (typep '(nil nil "1" 2 3 4) '(rte (:1 (:1-* null) (:0-* number)))))

  ;; zero-or-one
  (assert-true (typep nil '(rte (:1 (:0-1 null) (:0-* t)))))
  (assert-true (typep nil '(rte (:1 (:0-1 null) (:0-* t)))))
  (assert-true (typep '(1) '(rte (:1 (:0-1 null) (:0-* number)))))
  (assert-true (typep '(nil 1) '(rte (:1 (:0-1 null) (:0-* number)))))
  (assert-false (typep '(nil nil 1) '(rte (:1 (:0-1 null) (:0-* number)))))

  (assert-true (typep '(1 "hello")                 '(rte (:1 number (:1-* string)))))
  (assert-true (typep '(1 2 "hello" "world")       '(rte (:1 number number (:1-* string)))))
  (assert-true (typep '(1 "hello" "there" "world") '(rte (:1 (:1-* number) (:1-* string)))))
  (assert-true (typep '(1 hello "there" "world")   '(rte (:1 number symbol (:1-* string)))))
  (assert-true (typep '(1 hello)                   '(rte (:1 number symbol (:0-* string)))))
  (assert-false (typep '(1 hello)                  '(rte (:1 number (:0-* string)))))

  ;; pattern-prefix
  (assert-true (typep '(x 100 y 200 z 300)       '(rte (:1-* symbol number))))
  (assert-true (typep '(x 100 y 200 z 300 400)   '(rte (:1 (:1-* symbol number) (:0-1 t)))))
  (assert-true (typep '(x 100 y 200 z 300 400)   '(rte (:1 (:1-* symbol number) (:0-* t)))))
  (assert-false (typep  '(x 100 y 200 z 300 400) '(rte (:1 (:1-* symbol number) null))))

  (assert-true (typep '(x 100 y 200 z 300)
		      '(rte (:1 (:or (:0-* string) (:0-* symbol integer))))))
  (assert-true (typep nil '(rte (:0-* string))))
  (assert-true (typep '("hello" nil 1 2 3  
			"there" nil 
			"world" nil 12.0 "foo"
			"world" nil 12.0 "bar"
			)
		      '(rte (:0-* (:cat string null (:or (:1-* integer) (:0-* number string)))))))

  (assert-false (typep '("hello" nil 1 2 3  
			"there" nil  12.0
			"world" nil 13.0 "foo"
			"world" nil 14.0 "bar"
			)
		      '(rte (:0-* (:cat string null (:or (:1-* integer) (:0-* number string)))))))
  
  (let ((pattern     '(rte (:1 float integer (:0-* symbol (:0-* number) symbol) integer))))

    (assert-true (typep '(            1.0   3      x 1 2 3 y 12)		pattern))
    (assert-true (typep '(            1.0   3 4)                                pattern))
    (assert-true (typep '(            1.0   3   x y           12) pattern))
    (assert-false (typep '(           1.0   3   x y z         12) pattern))
    (assert-true (typep '(            1.0   3   x 12 y x 12 13 y x 11.0 12.0 13 y 14) pattern))
    )


)


(define-test type/find-keyword
  (assert-true (eq :x (rte::find-keyword 'x nil)))
  (assert-true (eq :x (rte::find-keyword '(x) nil)))
  (assert-true (eq :x (rte::find-keyword '((x y)) nil)))
  (assert-true (eq :x (rte::find-keyword '((:x y)) nil))))

(define-test test/alphabetize
  (assert-true (equal (RTE::ALPHABETIZE
		       '((:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))
			 (:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))))
		      '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
			(:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD)))))

  (assert-true (equal (RTE::ALPHABETIZE
		       '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
			 (:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))))
		      '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
			(:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))))))

(define-test test/destructuring-lambda-list-to-rte
  (assert-true (equal '(:cat t t t)
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(a b c)))))
  (assert-true (equal '(:cat t t t)
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(&whole w a b c)))))
  (assert-true (equal '(:cat t t t)
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(a b c &aux x y)))))
  
  (assert-true (equal (rte::canonicalize-pattern  '(:cat t t t (:and  (:0-* keyword t) 
								(:cat (:0-1 (eql :x) t (:0-* (MEMBER :X) T))))))
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(a b c &key x)))))
  
  (assert-true (equal (rte::canonicalize-pattern '(:cat t t t (:and (:0-* keyword t) (:or (:CAT (:|0-1| (EQL :X) T (:0-* (MEMBER :X) T))
											   (:|0-1| (EQL :Y) T (:0-* (MEMBER :Y :X) T)))
										      (:CAT (:|0-1| (EQL :Y) T (:0-* (MEMBER :Y) T))
										       (:|0-1| (EQL :X) T (:0-* (MEMBER :X :Y) T)))))))
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(a b c &key x y)))))
  
  ;; assert an error because &optional cannot follow &key
  (assert-error 'error (rte::destructuring-lambda-list-to-rte '(a b c &key x y &optional r)))
  (assert-true (equal (rte::canonicalize-pattern '(:cat T T T (:|0-1| T)
						   (:and (:0-* keyword t)
						    (:OR (:CAT (:|0-1| (EQL :Y) T (:0-* (MEMBER :Y) T))
							  (:|0-1| (EQL :X) T (:0-* (MEMBER :X :Y) T)))
						     (:CAT (:|0-1| (EQL :X) T (:0-* (MEMBER :X) T))
						      (:|0-1| (EQL :Y) T (:0-* (MEMBER :X :Y) T)))))))
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '(a b c &optional r &key x y)))))

  (assert-true (equal (rte::canonicalize-pattern '(:cat (:and list (RTE (:cat T T))) T (:|0-1| T)
						   (:and (:0-* keyword t) 
						    (:OR (:CAT (:|0-1| (EQL :Y) T (:0-* (MEMBER :Y) T))
							  (:|0-1| (EQL :X) T (:0-* (MEMBER :X :Y) T)))
						     (:CAT (:|0-1| (EQL :X) T (:0-* (MEMBER :X) T))
						      (:|0-1| (EQL :Y) T (:0-* (MEMBER :X :Y) T)))))))
		      (rte::canonicalize-pattern (rte::destructuring-lambda-list-to-rte '((a b) c &optional r &key x y)))))

  ;; test &rest
  (assert-true (equal (rte::canonicalize-pattern '(:CAT (:AND LIST (RTE (:CAT T T))) T (:OR T :EMPTY-WORD)
						   (:and (:0-* keyword t) 
						    (:OR (:AND (:AND LIST (RTE (:CAT T T T T)))
							  (:CAT (:OR (:CAT (EQL :X) T (:0-* (MEMBER :X) T)) :EMPTY-WORD)
							   (:OR (:CAT (EQL :Y) T (:0-* (MEMBER :X :y) T)) :EMPTY-WORD)))
						     (:AND (:AND LIST (RTE (:CAT T T T T)))
						      (:CAT (:OR (:CAT (EQL :Y) T (:0-* (MEMBER :Y) T)) :EMPTY-WORD)
						       (:OR (:CAT (EQL :X) T (:0-* (MEMBER :X :Y) T)) :EMPTY-WORD)))))))
		      (rte::canonicalize-pattern  (rte::destructuring-lambda-list-to-rte '((a b) c &optional q &rest (d e f g) &key x y)))))

  ;; &aux
  (assert-true (equal (rte::canonicalize-pattern '(:CAT (:AND LIST (RTE (:CAT T T))) T (:OR T :EMPTY-WORD)
						   (:and (:0-* keyword t)
						    (:OR (:AND (:AND LIST (RTE (:CAT T T T T)))
							  (:CAT (:OR (:CAT (EQL :X) T (:0-* (MEMBER :X) T)) :EMPTY-WORD)
							   (:OR (:CAT (EQL :Y) T (:0-* (MEMBER :X :Y) T)) :EMPTY-WORD)))
						     (:AND (:AND LIST (RTE (:CAT T T T T)))
						      (:CAT (:OR (:CAT (EQL :Y) T (:0-* (MEMBER :Y) T)) :EMPTY-WORD)
						       (:OR (:CAT (EQL :X) T (:0-* (MEMBER :X :Y) T)) :EMPTY-WORD)))))))
		      (rte::canonicalize-pattern  (rte::destructuring-lambda-list-to-rte '((a b) c &optional q &rest (d e f g) &key x y &aux u v)))))
  ;; add &allow-other-keys
  )




