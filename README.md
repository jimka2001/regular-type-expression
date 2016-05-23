## Synopsis

This project contains several Common Lisp sub-projects,

rte - definition of the RTE type.  A type (and supporting functions) which implement rational type expressions.
      For information about this project and related publications , see (https://www.lrde.epita.fr/wiki/Publications/newton.16.rte.report Efficient dynamic type checking of heterogeneous sequences
)
2d-array - Extensible sequence classes to represent vertical and horizontal "slices" of 2d arrays
lisp-types - Utilities dealing with CL types
ndfa - Implementation of non-deterministed finite automata


## Code Example

### RTE
```lisp
(defun F4 (obj)
  (destructuring-case obj
    ((name &key count)
     (declare (type symbol name)
              (type integer count))
     ...)
    ((name data &rest strings)
     (declare (type name symbol)
              (type data list)
              (type strings
                    (rte (:* string))))
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

### NDFA
```lisp
(let ((sm (make-instance 'ndfa:state-machine :key #'evenp)))
    (ndfa:add-state sm :label 'a
                       :initial-p t
                       :transitions `((:next-label b :transition-label t)
                                      (:next-label c :transition-label nil)))
    (assert-true (ndfa::states sm))
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

## Motivation

The implementation of rational type expression is the main result of this project.
However, several intermediate results might be useful as well, so they are made
available.

## Installation

This code loads via asdf.
(./rte.asd) loads the RTE system and its dependencies.
However, if you do not wish to use RTE, you may also use (./ndfa.asd), (./2d-array.asd), or (./lisp-types.asd)
as starting points.


## API Reference

Depending on the size of the project, if it is small and simple enough the reference docs can be added to the README. For medium size to larger projects it is important to at least provide a link to where the API reference docs live.

## Tests

Testing is done using (https://github.com/OdonataResearchLLC/lisp-unit LispUnit).  You may load
the system code without the tests via (./rte.asd), (./ndfa.asd), (./2d-array.asd), or (./lisp-types.asd).
But if you wish to run the tests, the starting points are respectively (./rte-test.asd), (./ndfa-test.asd), (./2d-array-test.asd), and (./lisp-types-test.asd).
Within each corresponding subdirectory the files contining LispUnit test cases are all prefixed by "test-".
To run the tests, you'll need to use ASDF to load the corresponding asdf system definition, e.g.,

```lisp
CL-USER> (asdf:load-system :rte-test)
CL-USER> (in-package :rte.test)
TEST> (rte.test::test)
```


## Contributors
The majority of the code development has been done by Jim Newton (doctoral candidate at (http://www.upmc.fr UPMC) (http://www.epita.fr EPITA) (https://www.lrde.epita.fr LRDE).



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
````