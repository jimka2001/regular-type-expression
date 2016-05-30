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
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-1-b
  (assert-true
   (equal 2 (destructuring-case '(x y)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3))))  )

(define-test test/destructuring-case-1-c
  (assert-true
   (equal 1 (destructuring-case '(x)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-a
  (assert-true
   (equal 3 (destructuring-case '((x) y z)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      (((a) b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-b  
  (assert-true
   (equal 2 (destructuring-case '(x (y))
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a (b)) ()
	       (declare (ignore a b))
	       2)
	      (((a) b) ()
	       (declare (ignore a b))
	       3))))  )


(define-test test/destructuring-case-2-c
  (assert-true
   (equal 3 (destructuring-case '((x) y)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a (b)) ()
	       (declare (ignore a b))
	       2)
	      (((a) b) ()
	       (declare (ignore a b))
	       3))))
)
   
(define-test test/destructuring-case-3
  (let ((n 0))
    (dolist (x '((1)
		 (2 (3))
		 (1 ((2)) (3 4))))
      (destructuring-case x
	((a) ()
	 (incf n)
	 (assert-true (= a 1)))
	((a (b)) ()
	 (incf n)
	 (assert-true (= a 2))
	 (assert-true (= b 3)))
	((a ((b)) (c d)) ()
	 (incf n)
	 (assert-true (= a 1))
	 (assert-true (= b 2))
	 (assert-true (= c 3))
	 (assert-true (= d 4)))))

    (assert-true (= n 3))))
