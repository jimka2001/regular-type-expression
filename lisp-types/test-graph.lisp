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

(define-test type/graph
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