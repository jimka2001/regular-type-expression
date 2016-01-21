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



(define-test type/sat
  (let* ((A '(1 2 3 4 5 6 7 8 9 10 12 13))
	 (B '(2 3 5 6))
	 (C '(3 4 5 7))
	 (D '(5 6 7 8 9 13))
	 (E '(9))
	 (F '(10))
	 (G '(11))
	 (H '(9 12 13))
	 (types `((member ,@A)
		  (member ,@B)
		  (member ,@C)
		  (member ,@D)
		  (member ,@E)
		  (member ,@F)
		  (member ,@G)
		  (member ,@H))))
    (lisp-types::generate-constraints types)
    (assert-false (set-exclusive-or '((eql 1)
				      (eql 2)
				      (eql 3)
				      (eql 4)
				      (eql 5)
				      (eql 6)
				      (eql 7)
				      (eql 8)
				      (eql 9)
				      (eql 10)
				      (eql 11)
				      (eql 12)
				      (eql 13))
				    (decompose-types-sat types)
				    :test #'equivalent-types-p)))
  
  (let ((types '(bignum unsigned-byte fixnum)))
    (assert-false (set-exclusive-or (decompose-types-sat types)
				    (decompose-types types)
				    :test #'equivalent-types-p)))
  
  (let ((types '(rational 
		 bit
		 real 
		 bignum
		 float
		 unsigned-byte
		 number
		 )))
    (assert-false (set-exclusive-or (decompose-types-sat types)
				    (decompose-types types)
				    :test #'equivalent-types-p)))

  (let ((types '(rational bit integer long-float real floating-point-inexact
		 double-float bignum signed-byte float unsigned-byte single-float number fixnum
		 char-int complex)))
    (assert-false (set-exclusive-or (decompose-types-sat types)
		      (decompose-types types) :test #'equivalent-types-p))

  (let ((types '(short-float ratio rational bit integer long-float real floating-point-inexact
		 double-float bignum signed-byte float unsigned-byte single-float number fixnum
		 char-int complex)))
    (assert-false (set-exclusive-or (decompose-types-sat types)
				    (decompose-types types)
				    :test #'equivalent-types-p)))


  (let ((types '(short-float ratio rational bit integer long-float real floating-point-inexact
		 double-float bignum signed-byte float unsigned-byte single-float number fixnum
		 char-int complex)))
    (assert-false (set-exclusive-or (decompose-types-sat types)
				    (decompose-types types)
				    :test #'equivalent-types-p))))

  

)

(define-test types/sat2
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (let ((all-numbers (remove-if-not (lambda (type)
					(subtypep type 'number))
				      all-types)))
      (assert-false (set-exclusive-or (decompose-types all-numbers)
				      (decompose-types-sat all-numbers)
				      :test #'equivalent-types-p)))))


(defun types/cmp-perf-sat ()
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function control-error division-by-zero error)))
    (setf all-types (sort all-types #'string<))
    (let ((n 1)
	  (testing-types (list (pop all-types))))
      (flet ((test1 (types &aux sorted)
	       (format t "~A~%" (car types))
	       (let ((t1 (get-internal-run-time))
		     (t2 (progn (setf sorted (decompose-types types))
				(get-internal-run-time)))
		     (t3 (progn (decompose-types-sat types)
				(get-internal-run-time))))

		 (unless (= t3 t2)
		   (format t "   ~D ~D ~F ~F ~F~%"
			   n
			   (length sorted)
			   (/ (- t2 t1) internal-time-units-per-second)
			   (/ (- t3 t2) internal-time-units-per-second)
			   (/ (- t2 t1) (- t3 t2))))
		 (incf n))))
	(loop :while testing-types
	      :do (progn (test1 testing-types)
			 (push (pop all-types) testing-types)))))))
		  