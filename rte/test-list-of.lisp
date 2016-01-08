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


(define-test type/list-of

  ;; type-of
  (assert-true (typep '(1 2 3 4 5) '(rte:list-of number)))


  ;; ((1 "hello") (2 "world"))
  ;; (1 "hello" 2 "world")

  (assert-true (typep '(1 2 3 4) '(or symbol (rte:list-of number))))
  (assert-true (typep '(1 2 3 4) '(rte:list-of number)))
  (assert-true (typep '(1 nil 2 nil 3 nil) '(rte:list-of (or null number))))

  ;; cons
  (assert-true (typep '(a 1 2 3 4) '(cons symbol (rte:list-of number))))
  (assert-true (typep '(nil 1 2 3 4) '(cons null (rte:list-of number))))
  (assert-false (typep '(1 2 3 4) '(cons null (rte:list-of number))))
  (assert-true (typep '(42 (1) (2) (3)) '(cons number (rte:list-of (rte:list-of number)))))
  (assert-true (typep '(42 (1) (nil) (2) (3)) '(cons number (rte:list-of (rte:list-of (or null number))))))

  ;; car*-cdr-of
  (assert-true (typep '(1 "hello") '(rte::car*-cdr-of number (rte:list-of string))))
  (assert-true (typep '(1 "hello" "world") '(rte::car*-cdr-of number (rte:list-of string))))
  (assert-true (typep '(1 "hello" "there" "world") '(rte::car*-cdr-of number (rte:list-of string))))
  (assert-true (typep '(1 hello "there" "world") '(rte::car*-cdr-of number symbol (rte:list-of string))))
  (assert-true (typep '(1 hello) '(rte::car*-cdr-of number symbol (rte:list-of string))))
  (assert-false (typep '(1 hello) '(rte::car*-cdr-of number (rte:list-of string))))


  )
