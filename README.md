## Synopsis

This project contains several Common Lisp sub-projects,

### rte

definition of the RTE CL type.  A type (and supporting functions) which implement rational type expressions.
      For information about this project and related publications , see [Efficient dynamic type checking of heterogeneous sequences](https://www.lrde.epita.fr/wiki/Publications/newton.16.rte.report)

### 2d-array

Extensible sequence classes to represent vertical and horizontal "slices" of 2d arrays

### lisp-types

Utilities dealing with CL types

### ndfa

Implementation of non-deterministed finite automata


## Motivation

The implementation of rational type expression is the main result of this project.
However, several intermediate results might be useful as well, so they are made
available.

## Installation

This code loads via asdf.
rte.asd loads the RTE system and its dependencies.
However, if you do not wish to use RTE, you may also use ndfa.asd, 2d-array.asd, or lisp-types.asd
as starting points.


## API Reference

to-be-done


## Code Examples

### RTE
```lisp
(defun F4 (obj)
  (destructuring-case obj
    ((name &key count) ((symbol name)
                        (integer count))
     ...)
    ((name data &rest strings) ((name symbol)
                                (data list)
                                (strings (rte (:* string))))
     ...)))
```

```lisp
(defun F (X Y)
  (declare
     (type (rte (:* (cons number)))
           Y))
  ...)
```

### 2D-ARRAY
```lisp
(let* ((arr (make-array '(3 2) :initial-contents '((1 2)
                                                   (3 4)
                                                   (5 6))))
         (row-vector (make-instance '2d-array:row-vector
                                    :2d-array arr :row 2))
         (column-vector (make-instance '2d-array:column-vector
                                       :2d-array arr :column 1))
         (vector-of-rows (make-instance '2d-array:vector-of-rows
                                        :2d-array arr))
         (vector-of-columns (make-instance '2d-array:vector-of-columns
                                           :2d-array arr)))
         
    ;; length
    (assert (= 2 (sequence:length vector-of-columns)))
    (assert (= 3 (sequence:length vector-of-rows)))
    (assert (= 2 (sequence:length row-vector)))
    (assert (= 3 (sequence:length column-vector)))


    ;; elt
    (assert (= 5 (sequence:elt row-vector 0)))
    (assert (= 6 (sequence:elt row-vector 1)))

    )
```
### LISP-TYPES

#### DISJOINT_TYPES-P
```lisp
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p 'number '(not float)))))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p '(not float) 'number))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p '(not number) 'float))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p 'float '(not number)))))
```

#### SMARTER-SUBTYPEP
```lisp
  (assert (equal '(t t) (smarter-subtypep '(eql :x) 'keyword)))
  (assert (equal '(t t) (smarter-subtypep '(not keyword) '(not (eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep 'keyword '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not keyword) '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not (eql :x)) 'keyword))))
```
#### DECOMPOSE-TYPES
```lisp
  (decompose-types '(float integer bignum string seqeunce))
==>
  (string
   (and sequence (not string))
   bignum
   (and integer (not bignum))
   float)
```

#### REDUCE-LISP-TYPE
```lisp
  (assert (equal (REDUCE-LISP-TYPE '(array (and integer number) (3)))
                      '(array integer (3))))
  (assert (equal (REDUCE-LISP-TYPE '(array * (3)))
                      '(array * (3))))

  ;; base-string
  (assert (equal (REDUCE-LISP-TYPE '(base-string *))
                      'base-string))

  ;; bit-vector
  (assert (equal (REDUCE-LISP-TYPE '(bit-vector *))
                      'bit-vector))

  (assert (equal (REDUCE-LISP-TYPE '(bit-vector 3))
                      '(bit-vector 3)))

  ;; complex
  (assert (equal (REDUCE-LISP-TYPE '(complex (and number real)))
                      '(complex real)))
  (assert (equal (REDUCE-LISP-TYPE '(complex *))
                      'complex ))

  ;; simple-array
  (assert (equal (REDUCE-LISP-TYPE '(simple-array (and number real) (3)))
                      '(simple-array real (3))))

  ;; vector
  (assert (equal (REDUCE-LISP-TYPE '(vector (and number real)))
                      '(vector real)))



  (assert (equal (REDUCE-LISP-TYPE '(cons (and float number) (or string (not string))))
                      '(cons float t)))
  (assert (equal (REDUCE-LISP-TYPE '(cons * *))
                      'cons))
  (assert (equal (REDUCE-LISP-TYPE '(cons (and float number) *))
                      '(cons float)))
  (assert (equal (REDUCE-LISP-TYPE '(cons * (and float number)))
                      '(cons * float)))

  (assert (equal (REDUCE-LISP-TYPE '(function (integer integer) integer))
                      '(function (integer integer) integer)))
  (assert (equal (REDUCE-LISP-TYPE '(function ((and integer integer) integer) integer))
                      '(function (integer integer) integer)))

  (assert (equal (REDUCE-LISP-TYPE '(function ((and integer integer) (and integer integer)) (and integer integer)))
                      '(function (integer integer) integer)))

  ;; test some optional arguments &optional &key &rest etc

  ;; &optional
  (assert (equal (REDUCE-LISP-TYPE '(function (&optional) (and list cons)))
                      '(function (&optional) cons)))

  (assert (equal (REDUCE-LISP-TYPE '(function (&optional (and integer number)) (and list cons)))
                      '(function (&optional integer) cons)))
  
  ;; &rest
  (assert (equal (REDUCE-LISP-TYPE '(function (&rest (and integer number)) (and list cons)))
                      '(function (&rest integer) cons)))


  ;; &key
  (assert (equal (REDUCE-LISP-TYPE '(function (&key) t))
                      '(function (&key) t)))

  (assert (equal (REDUCE-LISP-TYPE '(function (&key (x (and integer number))) (and list cons)))
                      '(function (&key (x integer)) cons)))

  ;; combining &optional &key &rest
  (assert (equal (REDUCE-LISP-TYPE
                       '(function ((and integer number)
                                   &optional (and integer number) (and integer number)
                                   &rest (and integer number)
                                   &key (x (and integer number)) (y (and integer number)))
                         (and list cons)))
                      '(function (integer
                                  &optional integer integer
                                  &rest integer
                                  &key (x integer) (y integer))
                        cons)))

```


### NDFA
```lisp
(let ((sm (make-instance 'ndfa:state-machine :key #'evenp)))
    (ndfa:add-state sm :label 'a
                       :initial-p t
                       :transitions `((:next-label b :transition-label t)
                                      (:next-label c :transition-label nil)))
    (assert (ndfa::states sm))
    (ndfa:add-state sm :label 'b
                       :final-p t
                       :initial-p t
                       :transitions `((:next-label c :transition-label t)
                                      (:next-label b :transition-label nil)))
    (ndfa:add-state sm :label 'c
                       :transitions `((:next-label b :transition-label t)
                                      (:next-label c :transition-label nil)))
    (with-output-to-string (str)
      (dolist (state (ndfa::states sm))
        (dolist (transition (ndfa::transitions state))
          (princ (ndfa::next-state transition) str))))
    (mapcar #'ndfa::transitions (ndfa::states sm))
    (assert (ndfa::get-initial-states sm))
    (assert (= 2 (length (ndfa::get-initial-states sm))))

    (assert (ndfa::get-initial-states sm))
    (ndfa:perform-transitions sm '(1))
    (ndfa:perform-transitions sm '(1 2))
    (ndfa:perform-transitions sm #(1 2 3))
    )
```

## Tests

Testing is done using [LispUnit](https://github.com/OdonataResearchLLC/lisp-unit).  You may load
the system code without the tests via rte.asd, ndfa.asd, 2d-array.asd, or lisp-types.asd.
But if you wish to run the tests, the starting points are respectively rte-test.asd, ndfa-test.asd, 2d-array-test.asd, and lisp-types-test.asd.
Within each corresponding subdirectory the files contining LispUnit test cases are all prefixed by "test-".
To run the tests, you'll need to use ASDF to load the corresponding asdf system definition, e.g.,

```lisp
CL-USER> (asdf:load-system :rte-test)
CL-USER> (in-package :rte.test)
TEST> (rte.test::test)
```


## Contributors
The majority of the code development has been done by Jim Newton, doctoral candidate at [UPMC](http://www.upmc.fr) [EPITA](http://www.epita.fr) [LRDE](https://www.lrde.epita.fr).



## License

```
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
```