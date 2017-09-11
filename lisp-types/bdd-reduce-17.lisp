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

(in-package :lisp-types)

(defgeneric label (node))
(defgeneric (setf label) (new-type node))

(defvar *node-num* 0)

(defclass node ()
  ((id :type unsigned-byte :reader id :initform (incf *node-num*))
   (label :initarg :label :accessor label)
   (touches :initform nil :type list :accessor touches)
   (subsets :initform nil :type list :accessor subsets)
   (supersets :initform nil :type list :accessor supersets)))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t :identity nil)
    (format stream "~D: ~A" (id n) (label n))))

(defgeneric add-node (graph node))
(defgeneric node-and (node1 node2))
(defgeneric node-and-not (node1 node2))
(defgeneric node-subtypep (node1 node2))
(defgeneric node-empty-type (node))
(defgeneric node-disjoint-types-p (node1 node2))

(defclass graph ()
  ((nodes :type list :accessor nodes :initarg :nodes
          :initform nil)
   (blue :type list :accessor blue
         :initform nil
         :documentation "List of blue arrows in order (origin destination)")
   (green :type list :accessor green
          :initform nil
          :documentation "List of green lines connecting nodes, order of pair (x y) is semantically unimportant, but for ease of access (id x) < (id y)")
   (disjoint :type list :accessor disjoint
             :initform nil)))

(defgeneric extract-disjoint (graph))
(defgeneric decompose-graph-1 (g))
(defgeneric decompose-graph-2 (g))

(defmethod decompose-graph-1 ((g graph))
  (loop :while (or (blue g) (green g))
        :do (dolist (x->y (blue g))
              (destructuring-bind (x y) x->y
                (break-relaxed-subset g x y)))
        :do (dolist (xy (green g))
              (destructuring-bind (x y) xy
                (break-touching g x y))))
  (extract-disjoint g))

(defun decompose-by-graph-1 (u &key (graph-class 'sexp-graph))
  (declare (type list u))
  (decompose-graph-1 (construct-graph graph-class u)))

(defmethod decompose-graph-2 ((g graph))
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
  (extract-disjoint g))

(defun decompose-by-graph-2 (u &key (graph-class 'sexp-graph))
  (declare (type list u))
  (decompose-graph-2 (construct-graph graph-class u)))

(defun construct-graph (graph-class u)
  (declare (type list u))
  (let ((g (make-instance graph-class)))
    (dolist (label u)
      (add-node g label))
    (mapl (lambda (tail)
            (let ((x (car tail)))
              (mapc (lambda (y)
                      (cond
                        ((node-subtypep x y)
                         (add-blue-arrow g x y))
                        ((node-subtypep y x)
                         (add-blue-arrow g y x))
                        (t
                         (multiple-value-bind (disjoint trust) (node-disjoint-types-p x y)
                           (cond
                             ((null trust) ;; maybe intersection types, not sure
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
    ((node-empty-type node)
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

(defun break-strict-subset (g sub super)
  (declare (type graph g) (type node sub super))
  (cond 
    ((null (member super (supersets sub) :test #'eq))
     nil)
    ((subsets sub)
     nil)
    ((touches sub)
     nil)
    (t
     (setf (label super) (node-and-not super sub))
     (delete-blue-arrow g sub super)))
  g)

(defun break-relaxed-subset (g sub super)
  (declare (type graph g) (type node sub super))
  (cond ((null (member super (supersets sub) :test #'eq))
         nil)
        ((subsets sub)
         nil)
        (t
         (setf (label super) (node-and-not super sub))
         (dolist (alpha (intersection (touches sub) (subsets super) :test #'eq))
           (add-green-line g alpha super)
           (delete-blue-arrow g alpha super))
         (delete-blue-arrow g sub super)))
  g)

(defun break-touching (g x y)
  (declare (type graph g) (type node x y))
  (cond
    ((null (member y (touches x) :test #'eq))
     nil)
    ((subsets x)
     nil)
    ((subsets y)
     nil)
    (t
     (let ((z (add-node g (node-and x y))))
       (psetf (label x) (node-and-not x y)
              (label y) (node-and-not y x))
       (dolist (alpha (union (supersets x) (supersets y) :test #'eq))
         (add-blue-arrow g z alpha))
       (dolist (alpha (intersection (touches x) (touches y) :test #'eq))
         (add-green-line g z alpha))
       (maybe-disjoint-node g z))
     (delete-green-line g x y)))
  g)
       
(defun break-loop  (g x y)
  (declare (type graph g) (type node x y))
  (cond
    ((null (member y (touches x) :test #'eq))
     nil)
    ((subsets x)
     nil)
    ((subsets y)
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
  ((label :type (or list symbol))))

(defmethod node-and-not ((x sexp-node) (y sexp-node))
  (reduce-lisp-type `(and ,(label x) (not ,(label y)))))
  
(defmethod node-and  ((x sexp-node) (y sexp-node))
  (reduce-lisp-type `(and ,(label x) ,(label y))))

(defmethod node-empty-type ((node sexp-node))
  (null (label node)))

(defmethod node-subtypep ((x sexp-node) (y sexp-node))
  (subtypep (label x) (label y)))

(defmethod node-disjoint-types-p ((x sexp-node) (y sexp-node))
  (disjoint-types-p (label x) (label y)))

(defclass sexp-graph (graph)
  ())

(defmethod add-node ((g sexp-graph) type-specifier)
  (let ((z (make-instance 'sexp-node :label type-specifier)))
    (push z
          (nodes g))
    z))

(defmethod extract-disjoint ((g sexp-graph))
  (mapcar #'label (disjoint g)))

(defmethod decompose-graph-1 ((g sexp-graph))
  (call-with-equiv-hash
   (lambda ()
     (call-with-disjoint-hash
      (lambda ()
        (call-with-subtype-hash
         (lambda ()
           (call-next-method))))))))

(defmethod decompose-graph-2 ((g sexp-graph))
  (call-with-equiv-hash
   (lambda (x)
     (call-with-disjoint-hash
      (lambda ()
        (call-with-subtype-hash
         (lambda ()
           (call-next-method))))))))


;; implemention of bdd based types

(defclass node-of-bdd (node)
  ((label :type bdd)))

(defmethod node-and-not ((x node-of-bdd) (y node-of-bdd))
  (bdd-and-not (label x) (label y)))

(defmethod node-and ((x node-of-bdd) (y node-of-bdd))
  (bdd-and (label x) (label y)))

(defmethod node-empty-type ((node node-of-bdd))
  (eq *bdd-false* (label node)))

(defmethod node-subtypep ((x node-of-bdd) (y node-of-bdd))
  (bdd-subtypep (label x) (label y)))

(defmethod node-disjoint-types-p ((x node-of-bdd) (y node-of-bdd))
  (values (bdd-disjoint-types-p (label x) (label y))
          t))

(defclass bdd-graph (graph)
  ())

(defmethod add-node ((g bdd-graph) type-specifier)
  (let ((z (make-instance 'node-of-bdd :label (bdd type-specifier))))
    (push z
          (nodes g))
    z))

(defmethod extract-disjoint ((g bdd-graph))
  (mapcar #'bdd-to-dnf (mapcar #'label (disjoint g))))

(defmethod decompose-graph-1 ((g bdd-graph))
  (bdd-call-with-new-hash
   (lambda ()
     (call-next-method))))

(defmethod decompose-graph-2 ((g bdd-graph))
  (bdd-call-with-new-hash
   (lambda ()
     (call-next-method))))
