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
      (format t "3 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

(define-test bdd-17/decompose-simple
  (dolist (class-name '(sexp-graph bdd-graph))
    (assert-false (decompose-by-graph-1 (list nil) :graph-class class-name))
    (assert-false (decompose-by-graph-2 (list nil):graph-class class-name))
    (assert-true  (decompose-by-graph-1 (list t) :graph-class class-name))
    (assert-true  (decompose-by-graph-2 (list t):graph-class class-name))))

(define-test bdd-17/decompose-simple-2
  (dolist (class-name '(sexp-graph bdd-graph))
    (assert-true (decompose-by-graph-1 '(number integer) :graph-class class-name))
    (assert-true (decompose-by-graph-2 '(number integer):graph-class class-name))
    (assert-true (decompose-by-graph-1 '(number string) :graph-class class-name))
    (assert-true (decompose-by-graph-2 '(number string) :graph-class class-name))))


(define-test bdd-17/decompose-simple-3
  (dolist (class-name '(sexp-graph bdd-graph))
    (assert-true (decompose-by-graph-1 '(number integer string) :graph-class class-name))
    (assert-true (decompose-by-graph-2 '(number integer string):graph-class class-name))
    (assert-true (decompose-by-graph-1 '(number string integer vector) :graph-class class-name))
    (assert-true (decompose-by-graph-2 '(number string integer vector) :graph-class class-name))))

