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

    (let ((lisp-types-test (find-package  :lisp-types.test))
          (lisp-types (find-package  :lisp-types)))
      (do-symbols (name :lisp-types)
        (when (and (eq lisp-types (symbol-package name))
                   (not (find-symbol (symbol-name name) lisp-types-test)))
          (format t "4 importing name=~A into  :lisp-types.test~%" name)
          (shadowing-import name :lisp-types.test))))
;;(shadow-package-symbols)
;;(do-symbols (name :lisp-types)
;;  (shadowing-import name :lisp-types.test))



(defun perf-decompose-types-graph (&key (max 18))
  (declare (notinline string< sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function ; see https://groups.google.com/forum/#!topic/comp.lang.lisp/S-O94JzjlFw
						char-code ; same as char-int
						)))
    (setf all-types (sort all-types #'string<))
    (let ( data)
      (flet ((test1 (types &aux sorted)
	       (format t "~A~%" (car types))
	       ;;(format t "  types=~A~%" types)
	       (dolist (algo (list (list :algorithm 'decompose-types-graph
					 :function (lambda (types)
						     (decompose-types-graph types :reduce t)))
				   (list :algorithm 'decompose-types-sat
					 :function #'decompose-types-sat)
				   (list :algorithm 'decompose-types
					 :function #'decompose-types)))
		 (let ((t1 (get-internal-run-time))
		       (t2 (progn (setf sorted (funcall (getf algo :function) types))
				  (get-internal-run-time))))
		   (unless (= t1 t2)
		     (push (list
			    :algorithm (getf algo :algorithm)
			    :time (/ (- t2 t1) internal-time-units-per-second)
			    :input-length (length types)
			    :output-length (length sorted))
			   data)
		     (format t "  ~A ~D ~D ~F~%"
			     (getf algo :algorithm)
			     (length types)
			     (length sorted)
			     (/ (- t2 t1) internal-time-units-per-second)))))))
	(dotimes (r 10)
	  (let ((rnd-all-types (shuffle-list (copy-list all-types)))
		(testing-types nil))
	    (while (and rnd-all-types
			(>= max (length testing-types)))
	      (push (pop rnd-all-types) testing-types)
	      (test1 testing-types)))))
      (values data
	      (length data)))))




(define-test types/find-duplicates
  (assert-true (equal '(a b) (lisp-types::find-duplicates '(a b a b)))))





(define-test types/graph2
  (declare (notinline set-difference))
  (let ((all-numbers (set-difference (valid-subtypes 'number)
                                     ;; redundante types
                                     '(nil single-float signed-byte double-float char-int))))
    
    (assert-false (set-exclusive-or (decompose-types all-numbers)
                                    (decompose-types-graph all-numbers)
                                    :test #'equivalent-types-p))))

(define-test type/graph-7
  (let ((types '( UNSIGNED-BYTE BIGNUM ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-6
  (let ((types '( UNSIGNED-BYTE FLOAT BIGNUM REAL ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-5
  (let ((types '(COMPLEX FLOAT-DIGITS UNSIGNED-BYTE BIGNUM ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-4
  (let ((types '((OR ARRAY-RANK (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE)))
                 (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE)))))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-3
  (let ((types '(COMPLEX
                 FLOAT
                 FLOAT-DIGITS
                 (AND BIGNUM (NOT UNSIGNED-BYTE))
                 (AND BIGNUM UNSIGNED-BYTE)
                 (AND ARRAY-RANK (NOT FLOAT-DIGITS))
                 (AND UNSIGNED-BYTE (NOT ARRAY-RANK) (NOT BIGNUM))
                 (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE))
                 )))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-2
  (let ((types '(COMPLEX
                 FLOAT
                 FLOAT-DIGITS
                 (AND BIGNUM (NOT UNSIGNED-BYTE))
                 (AND BIGNUM UNSIGNED-BYTE)
                 (AND ARRAY-RANK (NOT FLOAT-DIGITS))
                 (AND UNSIGNED-BYTE (NOT ARRAY-RANK) (NOT BIGNUM))
                 (OR ARRAY-RANK (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE))))))
    (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH types)
                                    (decompose-types types)
                                    :test #'equivalent-types-p))))

(define-test type/graph
  (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH '( COMPLEX
							   FLOAT-DIGITS
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   ARRAY-RANK
							   RATIONAL
							   ))
				  (decompose-types   '( COMPLEX
						       FLOAT-DIGITS
						       UNSIGNED-BYTE
						       FLOAT
						       BIGNUM
						       REAL
						       ARRAY-RANK
						       RATIONAL
						       ))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH '( COMPLEX
							   FIXNUM
							   FLOAT-DIGITS
							   NUMBER
							   CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL

							   ARRAY-RANK
					      
							   RATIONAL
							   RATIO

							   SHORT-FLOAT))
				  (decompose-types '( COMPLEX
						     FIXNUM
						     FLOAT-DIGITS
						     NUMBER
						     CHAR-CODE
						     UNSIGNED-BYTE
						     FLOAT
						     BIGNUM
						     REAL

						     ARRAY-RANK
					      
						     RATIONAL
						     RATIO

						     SHORT-FLOAT))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH '( COMPLEX
							   FIXNUM
							   FLOAT-DIGITS
							   NUMBER
							   CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   LONG-FLOAT
							   INTEGER
							   ARRAY-RANK
							   BIT
							   RATIONAL
							   SHORT-FLOAT))
				  (decompose-types      '( COMPLEX
							  FIXNUM
							  FLOAT-DIGITS
							  NUMBER
							  CHAR-CODE
							  UNSIGNED-BYTE
							  FLOAT
							  BIGNUM
							  REAL
							  LONG-FLOAT
							  INTEGER
							  ARRAY-RANK
							  BIT
							  RATIONAL
							  SHORT-FLOAT))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (DECOMPOSE-TYPES-GRAPH '( COMPLEX
							   FIXNUM
							   FLOAT-DIGITS
							   NUMBER
							   CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   LONG-FLOAT
							   INTEGER
							   ARRAY-RANK
							   BIT
							   RATIONAL
							   RATIO
							   SHORT-FLOAT))
				  (decompose-types      '( COMPLEX
							  FIXNUM
							  FLOAT-DIGITS
							  NUMBER
							  CHAR-CODE
							  UNSIGNED-BYTE
							  FLOAT
							  BIGNUM
							  REAL
							  LONG-FLOAT
							  INTEGER
							  ARRAY-RANK
							  BIT
							  RATIONAL
							  RATIO
							  SHORT-FLOAT))
				  :test #'equivalent-types-p))
  
  (assert-false (set-exclusive-or (decompose-types-graph '((or (eql 10)
							    (member 1 2)
							    (member 3 4))
							   (or (eql 11)
							    (member 1 3)
							    (member 2 4))
							   (member 10 11)))
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
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(FIXNUM CHAR-CODE UNSIGNED-BYTE))
                                  (decompose-types       '(FIXNUM CHAR-CODE UNSIGNED-BYTE))
                                  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (decompose-types-graph '(FIXNUM (integer 0 (1114112)) UNSIGNED-BYTE))
                        (decompose-types       '(FIXNUM (integer 0 (1114112)) UNSIGNED-BYTE))
                        :test #'equivalent-types-p))


)


