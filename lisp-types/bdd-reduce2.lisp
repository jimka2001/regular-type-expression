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

(defpackage :bdd-17
  (:use :cl :lisp-types)
  (:export))

(in-package :bdd-17)

(defvar *node-num* 0)
(defclass node ()
  ((id :type unsigned-byte :reader id :initform (incf *node-num*))
   (touches :initform nil :type list :accessor touches)
   (subsets :initform nil :type list :accessor subsets)
   (supersets :initform nil :type list :accessor supersets)))

(defgeneric label (node))
(defgeneric (setf label) (new-type node))
(defgeneric add-node (graph node))
(defgeneric node-and (node1 node2))
(defgeneric node-and-not (node1 node2))

(defclass graph ()
  ((nodes :type list :accessor nodes :initarg :nodes)
   (blue :type list :accessor blue
         :initform nil
         :documentation "List of blue arrows in order (origin destination)")
   (green :type list :accessor green
          :initform nil
          :documentation "List of green lines connecting nodes, order of pair (x y) is semantically unimportant, but for ease of access (id x) < (id y)")
   (disjoint :type list :accessor disjoint
             :initform nil)))

(defun bdd-decompose-by-graph-1 (u)
  (declare (type list u))
  (let ((g (construct-graph u)))
    (loop :while (or (blue g) (green g))
          :do (dolist (x->y (blue g))
                (destructuring-bind (x y) x->y
                  (break-relaxed-subset g x y)))
          :do (dolist (xy (green g))
                (destructuring-bind (x y) xy
                  (break-touching g x y))))
    (disjoint g)))

(defun bdd-decompose-by-graph-2 (u)
  (declare (type list u))
  (let ((g (construct-graph u)))
    (loop :while (or (blue g) (green g))
          :do (dolist (x->y (blue g))
                (destructuring-bind (x y) x->y
                  (break-strict-subset g x y)))
          :do (dolist (xy (green g))
                (destructuring-bind (x y) xy
                  (break-touching g x y)))
          :do (dolist (x->y (blue g))
                (destructuring-bind (x y) x->y
                  (break-loop g x y))))
    (disjoint g)))

(defun construct-graph (graph-class u )
  (declare (type list u))
  (let ((g (make-instance graph-class)))
    (dolist (label u)
      (add-node g label))
    (mapl (lambda (tail)
            (let ((x (car tail)))
              (mapc (lambda (y)
                      (cond
                        ((subtypep x y)
                         (add-blue-arrow g x y))
                        ((subtypep y x)
                         (add-blue-arrow g y x))
                        (t
                         (multiple-value-bind (disjoint trust) (disjoint-types-p x y)
                           (cond
                             ((null trust)
                              (add-green-line g x y))
                             (disjoint
                              nil)
                             (t ;; intersecting types
                              (add-green-line g x y)))))))
                    (cdr tail)))) (nodes g))
    (dolist (node (nodes g))
      (maybe-disjoint-node g node))
    g))
  
(defun maybe-disjoint-node (g node)
  (declare (type graph g) (type node node))
  (cond
    ((null (label node))
     (setf (nodes g) (remove node (nodes g) :test #'eq)))
    ((null (or (touches node)
               (supersets node)
               (subsets node)))
     (setf (nodes g) (remove node (nodes g) :test #'eq))
     (pushnew node (disjoint g) :test #'eq))))

(defun sort-nodes (n1 n2)
  (declare (type node n1 n2))
  (if (< (id n1) (id n2))
      (list n1 n2)
      (list n2 n1)))

(defun add-green-line (g x y)
  (declare (type graph g) (type node x y))
  (pushnew (sort-nodes x y) (green g) :test #'equal)
  (pushnew x (touches y) :test #'eq)
  (pushnew y (touches x) :test #'eq))

(defun delete-green-line (g x y)
  (declare (type graph g) (type node x y))
  (setf (green g)   (remove (sort-nodes x y) (green g) :test #'equal)
        (touches y) (remove x (touches y) :test #'eq)
        (touches x) (remove y (touches x) :test #'eq))
  (maybe-disjoint-node g x)
  (maybe-disjoint-node g y))

(defun add-blue-arrow (g x y)
  (pushnew (list x y) (blue g) :test #'equal)
  (pushnew x (subsets y) :test #'eq)
  (pushnew y (supersets x) :test #'eq))

(defun delete-blue-arrow (g x y)
  (declare (type graph g) (type node x y))
  (setf (blue g)      (remove (list x y) (blue g) :test #'equal)
        (subsets y)   (remove x (subsets y) :test #'eq)
        (supersets x) (remove y (supersets x) :test #'eq))
  (maybe-disjoint-node g x)
  (maybe-disjoint-node g y))

(defun break-strict-subset (g x y)
  (declare (type graph g) (type node x y))
  (cond
    ((null (member y (subsets x) :test #'eq))
     nil)
    ((null (subsets x))
     nil)
    ((null (touches x))
     nil)
    (t
     (setf (label y) (node-and-not y x))
     (delete-blue-arrow g x y)))
  g)

(defun break-relaxed-subset (g x y)
  (declare (type graph g) (type node x y))
  (cond ((null (member y (supersets x) :test #'eq))
         nil)
        ((null (subsets x))
         nil)
        (t
         (setf (label y) (node-and-not y x))
         (dolist (alpha (intersection (touches x) (subsets y) :test #'eq))
           (add-green-line g alpha y)
           (delete-blue-arrow g alpha y))
         (delete-blue-arrow g x y)))
  g)

(defun break-touching (g x y)
  (declare (type graph g) (type node x y))
  (cond
    ((null (member y (touches x) :test #'eq))
     nil)
    ((null (subsets x))
     nil)
    ((null (subsets y))
     nil)
    (t
     (let ((z (add-node g (node-and x y))))
       (psetf (label x) (node-and-not x y)
              (label y) (node-and-not y x))
       (dolist (alpha (union (supersets x) (supersets y) :test #'eq))
         (add-blue-arrow g z alpha))
       (dolist (alpha (intersection (touches x) (touches y) :test #'eq))
         (add-green-line g z alpha)))
     (delete-green-line g x y)))
  g)
       
(defun break-loop  (g x y)
  (declare (type graph g) (type node x y))
  (cond
    ((null (member y (touches x) :test #'eq))
     nil)
    ((null (subsets x))
     nil)
    ((null (subsets y))
     nil)
    (t
     (let ((z (add-node g (node-and x y))))
       (setf (label x) (node-and-not x y))
       (dolist (alpha (touches x))
         (add-green-line g z alpha))
       (dolist (alpha (union (supersets x) (supersets y) :test #'eq))
         (add-blue-arrow g z alpha))
       (add-blue-arrow g z y)
       (add-blue-arrow g z x)
       (delete-blue-arrow g x y))))
  g)

;; implemention of sexp based types

(defclass sexp-node (node)
  ((sexp :initarg :sexp :accessor sexp)))

(defmethod label ((node sexp-node))
  (sexp node))

(defmethod (setf label) (new-type (node sexp-node))
  (setf (sexp  node) new-type))

(defmethod node-and-not ((x sexp-node) (y sexp-node))
  `(and ,(label x) (not ,(label y))))
  
(defmethod node-and  ((x sexp-node) (y sexp-node))
  `(and ,(label x) ,(label y)))

(defclass sexp-graph (graph)
  ())

(defmethod add-node ((g sexp-graph) type-specifier)
  (let ((z (make-instance 'sexp-node :sexp type-specifier)))
    (push z
          (nodes g))
    z))


;; implemention of bdd based types

(defclass bdd-node (node)
  ((bdd :initarg :bdd :accessor bdd :type lisp-types::bdd-node)))

(defmethod label ((node bdd-node))
  (bdd node))

(defmethod (setf label) ((new-type lisp-types::bdd-node) (node bdd-17::bdd-node))
  (setf (bdd node) new-type))

(defmethod node-and-not ((x bdd-node) (y bdd-node))
  (lisp-types::bdd-and-not x y))

(defmethod node-and ((x bdd-node) (y bdd-node))
  (lisp-types::bdd-and x y))

(defclass bdd-graph (graph)
  ())

(defmethod add-node ((g bdd-graph) type-specifier)
  (let ((z (make-instance 'bdd-17::sexp-node :bdd (bdd type-specifier))))
    (push z
          (nodes g))
    z))
