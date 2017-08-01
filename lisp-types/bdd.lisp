;; Copyright (c) 2016,2017 EPITA Research and Development Laboratory
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

(in-package   :lisp-types)

(defgeneric bdd-serialize (bdd))

(defgeneric bdd (obj))
(defgeneric bdd-leaf (value))
(defgeneric bdd-node (label left right))
(defgeneric bdd-or (b1 b2))
(defgeneric bdd-and (b1 b2))
(defgeneric bdd-and-not (b1 b2))


(defvar *bdd-count* 1)
(defclass bdd ()
  ((ident ;; :reader bdd-ident
          :initarg :ident :initform (incf *bdd-count*))
   (label ;; :reader bdd-label
    :initarg :label)
   (dnf)
   (expr)))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-label (bdd)
  (slot-value bdd 'label))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-ident (bdd)
  (slot-value bdd 'ident))

(defun bdd-bfs (bdd action)
  (let* ((buf (tconc nil bdd))
         ;; TODO -- don't really need nodes, we could
         ;; actually just pop (car buf), but this way
         ;; is a bit less obscure.
         (nodes (car buf)))
    (while nodes
      (funcall action (car nodes))
      (typecase (car nodes)
        (bdd-node
         (unless (member (bdd-left (car nodes)) (car buf) :test #'eq)
           (tconc buf (bdd-left (car nodes))))
         (unless (member (bdd-right (car nodes)) (car buf) :test #'eq)
           (tconc buf (bdd-right (car nodes))))))
      (pop nodes))))

(defun bdd-count-nodes (bdd)
  (let ((c 0))
    (bdd-bfs bdd (lambda (node)
                   (declare (ignore node))
                   (incf c)))
    c))

(defun bdd-new-hash ()
  (make-hash-table :test #'equal))

(defvar *bdd-hash* (bdd-new-hash))
(defvar *bdd-verbose* nil)

(defun bdd-with-new-hash (thunk &key (verbose *bdd-verbose*))
  (let ((*bdd-verbose* verbose)
        (*bdd-hash* (bdd-new-hash))
        (*disjoint-hash* (new-disjoint-hash))
        (*subtype-hash* (new-subtype-hash)))
    ;; (setf (gethash :type-system *bdd-hash*)
    ;;       (bdd '(not (or
    ;;                   (and (not integer) (not ratio) rational)
    ;;                   (and rational float)
    ;;                   (and array sequence (not vector))
    ;;                   (and (not float) (not integer) (not ratio) real)
    ;;                   (and (not bignum) (not fixnum) unsigned-byte)))))
    (prog1 (funcall thunk)
      (when verbose
        (format t "finished with ~A and ~A~%" *bdd-hash* *subtype-hash*)))))
  
(defun bdd-make-key (label left right)
  (list left right label))

(defun bdd-find-int-int (hash label left right)
  (declare (type fixnum left right)
           (optimize (speed 3) (safety 0)))
  (gethash (bdd-make-key label left right) hash))

(defun bdd-find (hash label left-bdd right-bdd)
  (declare (type bdd left-bdd right-bdd))
  (bdd-find-int-int hash label (bdd-ident left-bdd) (bdd-ident right-bdd)))

(defmethod print-object ((bdd bdd) stream)
  (print-unreadable-object (bdd stream :type t :identity nil)
    (when (slot-boundp bdd 'ident)
      (format stream "[~D]" (slot-value bdd 'ident)))
    (format stream "~S=~S" (bdd-serialize bdd) (bdd-to-dnf bdd))))

(defmethod bdd ((bdd bdd))
  bdd)

(defclass bdd-node (bdd)
  ((left :type bdd :initarg :left ;;:reader bdd-left
         )
   (right :type bdd :initarg :right ;; :reader bdd-right
          )))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-right (bdd)
  (slot-value bdd 'right))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-left (bdd)
  (slot-value bdd 'left))

(defclass bdd-leaf (bdd) ())

(defmethod bdd-serialize ((leaf bdd-leaf))
  (bdd-label leaf))

(defclass bdd-true (bdd-leaf)
  ((ident :initform 1)
   (label :initform t)
   (dnf :initform t)
   (expr :initform t)))

(defclass bdd-false (bdd-leaf)
  ((ident :initform 0)
   (label :initform nil)
   (dnf :initform nil)
   (expr :initform nil)))

(defvar *bdd-true* (make-instance 'bdd-true))
(defvar *bdd-false* (make-instance 'bdd-false))

(defmethod bdd ((label symbol))
  (if (valid-type-p label)
      (%bdd-node label *bdd-true* *bdd-false*)
      (error "invalid type specifier: ~A" label)))

(defmethod bdd ((expr list))
  (declare (optimize (speed 3) (safety 0) (debug 3) (compilation-speed 0) (space 0)))
  (destructuring-bind (head &rest tail) expr
    (flet ((bdd-tail ()
             (mapcar #'bdd tail)))
      (case head
        ((and)
         (reduce #'bdd-and (bdd-tail) :initial-value *bdd-true* ))
        ((or)
         (reduce #'bdd-or (bdd-tail) :initial-value *bdd-false*))
        ((not)
         ;; (assert (null (cdr tail)) ()
         ;;         "NOT takes exactly one argument: cannot convert ~A to a BDD" expr)
         (bdd-and-not *bdd-true* (bdd (car tail))))
        ((and-not)
         ;; (assert (<= 2 (length tail)) ()
         ;;         "AND-NOT takes at least two arguments: cannot convert ~A to a BDD" expr)
         (destructuring-bind (bdd-head &rest bdd-tail) (bdd-tail)
           (reduce #'bdd-and-not bdd-tail :initial-value bdd-head)))
        (t
         (if (valid-type-p expr)
             (%bdd-node expr *bdd-true* *bdd-false*)
             (error "invalid type specifier: ~A" expr)))))))

(defmethod bdd ((label (eql nil)))
  *bdd-false*)

(defmethod bdd ((label (eql t)))
  *bdd-true*)

(defmethod bdd-leaf ((value (eql t)))
  *bdd-true*)
(defmethod bdd-leaf ((value (eql nil)))
  *bdd-false*)

(defmethod bdd-serialize ((b bdd-node))
  (list (bdd-label b)
        (bdd-serialize (bdd-left b))
        (bdd-serialize (bdd-right b))))

(defmethod bdd-node (label left right)
  (error "cannot create bdd-node from arguments ~A" (list label left right)))

(defmethod bdd-node :around (label (left (eql t)) right)
  (bdd-node label *bdd-true* right))

(defmethod bdd-node :around (label (left (eql nil)) right)
  (bdd-node label *bdd-false* right))

(defmethod bdd-node :around (label left (right (eql t)))
  (bdd-node label left *bdd-true*))

(defmethod bdd-node :around (label left (right (eql nil)))
  (bdd-node label left *bdd-false*))


(defvar *bdd-hash-access-count* 0)



(defmethod bdd-node (label (left bdd) (right bdd))
  (%bdd-node label left right))

(defmethod bdd-or ((true bdd-true) (b bdd))
  *bdd-true*)
(defmethod bdd-or :around ((b bdd) (true bdd-true))
  *bdd-true*)
(defmethod bdd-or ((false bdd-false) (b bdd))
  b)
(defmethod bdd-or :around ((b bdd) (false bdd-false))
  b)

(defmethod bdd-and ((true bdd-true) (b bdd))
  b)
(defmethod bdd-and :around ((b bdd) (true bdd-true))
  b)
(defmethod bdd-and ((false bdd-false) (b bdd))
  *bdd-false*)
(defmethod bdd-and :around ((b bdd) (false bdd-false))
  *bdd-false*)


(defmethod bdd-and-not :around ((b bdd) (true bdd-true))
  *bdd-false*)
(defmethod bdd-and-not ((false bdd-false) (b bdd))
  *bdd-false*)
(defmethod bdd-and-not :around ((b bdd) (false bdd-false))
  b)
(defmethod bdd-and-not ((true bdd-true) (b bdd))
  (%bdd-node (bdd-label b)
            (bdd-and-not *bdd-true* (bdd-left b))
            (bdd-and-not *bdd-true* (bdd-right b))))

(defun bdd-cmp (t1 t2)
  (cond
    ((equal t1 t2)
     '=)
    ((null t1)
     '<)
    ((null t2)
     '>)
    ((not (eql (class-of t1) (class-of t2))) 
     (bdd-cmp (class-name (class-of t1)) (class-name (class-of t2))))
    (t
     ;; thus they are the same type, but they are not equal
     (typecase t1
       (list
        (let (value)
          (while (and t1
                      t2
                      (eq '= (setf value (bdd-cmp (car t1) (car t2)))))
            (pop t1)
            (pop t2))
          (cond
            ((and t1 t2)
             value)
            (t1    '>)
            (t2    '<)
            (t     '=))))
       (symbol
        (cond
          ((not (eql (symbol-package t1) (symbol-package t2)))
           ;; call bdd-cmp because symbol-package might return nil
           ;;  don't call string= directly
           (bdd-cmp (symbol-package t1) (symbol-package t2)))
          ((string< t1 t2) ;; same package
           '<)
          (t
           '>)))
       (package
        (bdd-cmp (package-name t1) (package-name t2)))
       (string
        ;; know they're not equal, thus not string=
        (cond
          ((string< t1 t2)
           '<)
          (t
           '>)))
       (number
        (cond ((< t1 t2)
               '<)
              (t
               '>)))
       (t
        (error "cannot compare a ~A with a ~A" (class-of t1) (class-of t2)))))))

(flet ((bdd-op (op b1 b2)
         (declare (type bdd b1 b2))
         (let ((a1 (bdd-label b1))
               (c1 (bdd-left b1))
               (d1 (bdd-right b1))
               (a2 (bdd-label b2))
               (c2 (bdd-left b2))
               (d2 (bdd-right b2)))
           (declare (type bdd c1 c2 d1 d2))
           (ecase (bdd-cmp a1 a2)
             ((=)
              (%bdd-node a1 (funcall op c1 c2) (funcall op d1 d2)))
             ((<)
              (%bdd-node a1 (funcall op c1 b2) (funcall op d1 b2)))
             ((>)
              (%bdd-node a2 (funcall op b1 c2) (funcall op b1 d2)))))))

  (defmethod bdd-or ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        b1
        (bdd-op #'bdd-or b1 b2)))

  (defmethod bdd-and ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        b1
        (bdd-op #'bdd-and b1 b2)))

  (defmethod bdd-and-not ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        *bdd-false*
        (bdd-op #'bdd-and-not b1 b2))))

(defmethod bdd-or (b1 b2)
  (error "bdd-or not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and (b1 b2)
  (error "bdd-and not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and-not (b1 b2)
  (error "bdd-and-not not implemented for ~A and ~A" b1 b2))

(defun bdd-to-dnf (bdd)
  (slot-value bdd 'dnf))

(defun bdd-to-expr (bdd)
  (slot-value bdd 'expr))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'dnf)))
  (setf (slot-value bdd 'dnf)
        (%bdd-to-dnf bdd)))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'expr)))
  (setf (slot-value bdd 'expr)
        (cond
          ((and (eq *bdd-false* (bdd-left bdd))
                (eq *bdd-true* (bdd-right bdd)))
           `(not ,(bdd-label bdd)))
          ((and (eq *bdd-false* (bdd-right bdd))
                (eq *bdd-true* (bdd-left bdd)))
           (bdd-label bdd))
          ((eq *bdd-false* (bdd-left bdd))
           `(and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-right bdd))))
          ((eq *bdd-false* (bdd-right bdd))
           `(and ,(bdd-label bdd) ,(bdd-to-expr (bdd-left bdd))))
          ((eq *bdd-true* (bdd-left bdd))
           `(or ,(bdd-label bdd)
                (and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-right bdd)))))
          ((eq *bdd-true* (bdd-right bdd))
           `(or (and ,(bdd-label bdd) ,(bdd-to-expr (bdd-left bdd)))
                (not ,(bdd-label bdd))))
          (t
           `(or (and ,(bdd-label bdd) ,(bdd-to-expr (bdd-left bdd)))
                (and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-right bdd))))))))


