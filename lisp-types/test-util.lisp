;; Copyright (c) 2017 EPITA Research and Development Laboratory
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

(def-cache-fun cache-it call-with-cache (a b)
    "testing function"
  (+ a b))

(define-test test-util/test1
  (assert-true
   (equal
    (loop :for a :from 0 :below 2
          :for b :from 0 :below 3
          :collect (list a b (cache-it a b) (cache-it b a)))
    (call-with-cache
     (lambda ()
       (loop :for a :from 0 :below 2
             :for b :from 0 :below 3
             :collect (list a b (cache-it a b) (cache-it b a))))))))

(define-test test-util/test2
  (assert-true
   (equal
    (loop :for a :from 0 :below 5
          :for b :from 0 :below 7
          :collect (list a b (cache-it a b) (cache-it b a)))
    (call-with-cache
     (lambda ()
       (loop :for a :from 0 :below 5
             :for b :from 0 :below 7
             :collect (list a b (cache-it a b) (cache-it b a))))))))
