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
				    (sat-decompose-types types)
				    :test #'equal))))

