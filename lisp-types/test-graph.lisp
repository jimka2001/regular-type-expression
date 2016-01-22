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


(define-test types/graph2
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (let ((all-numbers (set-difference (remove-if-not (lambda (type)
							(subtypep type 'number))
						      all-types)
				       ;; redundante types
				       '(single-float signed-byte double-float char-int))))

      (assert-false (set-exclusive-or (decompose-types all-numbers)
				      (decompose-types-graph all-numbers)
				      :test #'equivalent-types-p)))))

(define-test type/graph
  (assert-false (set-exclusive-or (decompose-types-graph '((or (eql 10)
							    (member 1 2)
							    (member 3 4))
							   (or (eql 11)
							    (member 1 3)
							    (member 2 4))
							   (member 10 200)))
				  (decompose-types       '((or (eql 10)
							    (member 1 2)
							    (member 3 4))
							   (or (eql 11)
							    (member 1 3)
							    (member 2 4))
							   (member 10 11)))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '((eql 1) (eql 2) (member 1 2)))
				  (decompose-types       '((eql 1) (eql 2) (member 1 2)))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(CONDITION CLASS CELL-ERROR BUILT-IN-CLASS))
				  (decompose-types       '(CONDITION CLASS CELL-ERROR BUILT-IN-CLASS))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(CONDITION CONCATENATED-STREAM COMPLEX CLASS CHARACTER CHAR-INT
							   CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN BIT-VECTOR BIT
							   BIGNUM BASE-STRING BASE-CHAR ATOM ARRAY-TOTAL-SIZE ARRAY-RANK ARRAY
							   ARITHMETIC-ERROR))
				  (decompose-types       '(CONDITION CONCATENATED-STREAM COMPLEX CLASS CHARACTER CHAR-INT
							   CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN BIT-VECTOR BIT
							   BIGNUM BASE-STRING BASE-CHAR ATOM ARRAY-TOTAL-SIZE ARRAY-RANK ARRAY
							   ARITHMETIC-ERROR))
				  :test #'equivalent-types-p))

  (assert-false (set-exclusive-or (decompose-types-graph '( BIT  ATOM         ARRAY-RANK))
				  (decompose-types       '( BIT  ATOM         ARRAY-RANK))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '( BIT  ATOM         ARRAY-RANK ARRAY))
				  (decompose-types       '( BIT  ATOM         ARRAY-RANK ARRAY))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(CHAR-CODE CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN
							   BIT-VECTOR BIT BIGNUM BASE-STRING BASE-CHAR ATOM
							   ARRAY-TOTAL-SIZE ARRAY-RANK ARRAY ARITHMETIC-ERROR))
				  (decompose-types       '(CHAR-CODE CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN
							   BIT-VECTOR BIT BIGNUM BASE-STRING BASE-CHAR ATOM
							   ARRAY-TOTAL-SIZE ARRAY-RANK ARRAY ARITHMETIC-ERROR))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '(cell-error arithmetic-error ))
				  (decompose-types '(cell-error arithmetic-error ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '(cell-error BUILT-IN-CLASS ))
				  (decompose-types '(cell-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '( arithmetic-error BUILT-IN-CLASS ))
				  (decompose-types '( arithmetic-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '(cell-error arithmetic-error BUILT-IN-CLASS ))
				  (decompose-types '(cell-error arithmetic-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '(integer))
				  (decompose-types '(integer))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (decompose-types-graph '(integer fixnum))
				  (decompose-types '(integer fixnum))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(fixnum number))
				  (decompose-types '(fixnum number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(integer number))
				  (decompose-types '(integer number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(integer fixnum number))
				  (decompose-types '(integer fixnum number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(integer fixnum number bit unsigned-byte bignum))
				  (decompose-types '(integer fixnum number bit unsigned-byte bignum))
				  :test #'equivalent-types-p)))
