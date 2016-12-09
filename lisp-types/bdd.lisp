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

(in-package   :lisp-types)

(defgeneric bdd-label (obj))
(defgeneric bdd-serialize (bdd))

(defgeneric bdd (obj))
(defgeneric bdd-leaf (value))
(defgeneric bdd-node (label left right))
(defgeneric bdd-or (b1 b2))
(defgeneric bdd-and (b1 b2))
(defgeneric bdd-and-not (b1 b2))

(defvar *bdd-count* 1)
(defclass bdd ()
  ((ident :reader bdd-ident :initarg :ident :initform (incf *bdd-count*))
   (label :reader bdd-label :initarg :label)))

(defun bdd-new-hash ()
  (make-hash-table :test #'equal))

(defvar *bdd-hash* (bdd-new-hash))

(defun bdd-with-new-hash (thunk)
  (let ((*bdd-hash* (bdd-new-hash)))
    ;; (setf (gethash :type-system *bdd-hash*)
    ;;       (bdd '(not (or
    ;;                   (and (not integer) (not ratio) rational)
    ;;                   (and rational float)
    ;;                   (and array sequence (not vector))
    ;;                   (and (not float) (not integer) (not ratio) real)
    ;;                   (and (not bignum) (not fixnum) unsigned-byte)))))
    (funcall thunk)))
  
(defun bdd-find-int-int (hash label left right)
  (declare (type fixnum left right)
           (optimize (speed 3) (safety 0)))
  ((lambda (&rest key)
     (gethash key hash)) label left right))

(defun bdd-find (hash label left-bdd right-bdd)
  (declare (type bdd left-bdd right-bdd))
  (bdd-find-int-int hash label (bdd-ident left-bdd) (bdd-ident right-bdd)))

(defmethod print-object ((bdd bdd) stream)
  (print-unreadable-object (bdd stream :type t :identity nil)
    (when (slot-boundp bdd 'ident)
      (format stream "[~D]" (slot-value bdd 'ident)))
    (format stream "~A=~A" (bdd-serialize bdd) (bdd-to-dnf bdd))))

(defmethod bdd ((bdd bdd))
  bdd)

(defclass bdd-node (bdd)
  ((left :type bdd :initarg :left :reader bdd-left)
   (right :type bdd :initarg :right :reader bdd-right)))

(defclass bdd-leaf (bdd) ())

(defmethod bdd-serialize ((leaf bdd-leaf))
  (bdd-label leaf))

(defclass bdd-true (bdd-leaf)
  ((ident :initform 1)
   (label :initform t)))

(defclass bdd-false (bdd-leaf)
  ((ident :initform 0)
   (label :initform nil)))

(defvar *bdd-true* (make-instance 'bdd-true))
(defvar *bdd-false* (make-instance 'bdd-false))

(defmethod bdd ((label symbol))
  (if (valid-type-p label)
      (%bdd-node label *bdd-true* *bdd-false*)
      (error "invalid type specifier: ~A" label)))

(defmethod bdd ((expr list))
  (destructuring-bind (head &rest tail) expr
    (flet ((bdd-tail ()
             (mapcar #'bdd tail)))
      (case head
        ((and)
         (reduce #'bdd-and (bdd-tail) :initial-value *bdd-true* ))
        ((or)
         (reduce #'bdd-or (bdd-tail) :initial-value *bdd-false*))
        ((not)
         (assert (null (cdr tail)) ()
                 "NOT takes exactly one argument: cannot convert ~A to a BDD" expr)
         (bdd-and-not *bdd-true* (bdd (car tail))))
        ((and-not)
         (assert (<= 2 (length tail)) ()
                 "AND-NOT takes at least two arguments: cannot convert ~A to a BDD" expr)
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
(labels ((relation (r x-parity y-parity)
           #'(lambda (x y)
               (funcall r
                        (if x-parity
                            x
                            `(not ,x))
                        (if y-parity
                            y
                            `(not ,y)))))
         (super (x-parity y-parity)
           (relation #'(lambda (a b)
                         (smarter-subtypep b a)) x-parity y-parity))
         (sub (x-parity y-parity)
           (relation #'smarter-subtypep x-parity y-parity))
         (disjoint (x-parity y-parity)
           (relation #'disjoint-types-p x-parity y-parity)))
  (let* ((reductions `((:case  1 :child :left  :relation ,(disjoint t t)     :reduction ,#'bdd-right)
                       (:case  2 :child :left  :relation ,(disjoint t nil)   :reduction ,#'bdd-left)
                       (:case  3 :child :right :relation ,(disjoint nil t)   :reduction ,#'bdd-right)
                       (:case  4 :child :right :relation ,(disjoint nil nil) :reduction ,#'bdd-left)

                       (:case  5 :child :right :relation ,(super t t)        :reduction ,#'bdd-right)
                       (:case  6 :child :right :relation ,(super t nil)      :reduction ,#'bdd-left)
                       (:case  7 :child :left  :relation ,(super nil t)      :reduction ,#'bdd-right)
                       (:case  8 :child :left  :relation ,(super nil nil)    :reduction ,#'bdd-left)

                       (:case  9 :child :left  :relation ,(sub t t)          :reduction ,#'bdd-left)
                       (:case 10 :child :left  :relation ,(sub t nil)        :reduction ,#'bdd-right)
                       (:case 11 :child :right :relation ,(sub nil t)        :reduction ,#'bdd-left)
                       (:case 12 :child :right :relation ,(sub nil nil)      :reduction ,#'bdd-right)))
         (left-reductions  (setof r reductions
                             (eq :left (getf r :child))))
         (right-reductions (setof r reductions
                             (eq :right (getf r :child)))))

    (defun %bdd-node (label left-bdd right-bdd)
      (cond
        ((eq left-bdd right-bdd)
         left-bdd)
        ((bdd-find *bdd-hash* label left-bdd right-bdd))
        (t
         (let ((new-left  (bdd-reduce label left-bdd  left-reductions))
               (new-right (bdd-reduce label right-bdd right-reductions)))
           (cond
             ((eq new-left new-right)
              new-left)
             ((bdd-find *bdd-hash* label new-left new-right))
             (t
              (let* ((bdd (make-instance 'bdd-node
                                        :label label
                                        :left  new-left
                                        :right new-right))
                     (key (list label (bdd-ident new-left) (bdd-ident new-right))))
                ;;(format t "hash=~A~%" *bdd-hash*)
                ;;(format t "   new ~A ~A ~A~%" label new-left new-right)
                ;;(maphash (lambda (key value)
                ;; (format t "    ~A --> ~A~%" key value)) *bdd-hash*)
                (incf *bdd-hash-access-count*)
                (when (= 0 (mod *bdd-hash-access-count* 10000))
                  (format t "bdd hash = ~A~%" *bdd-hash*))
                (setf (gethash key *bdd-hash*) bdd)
                (setf (gethash key *bdd-hash*)
                      (cond
                        ;; check (bdd-and-not bdd new-left)
                        ;;   vs  (bdd-and-not new-left bdd)
                        ((bdd-type-equal bdd new-left)
                         new-left)
                        ;; check (bdd-and-not bdd new-right)
                        ;;   vs  (bdd-and-not new-right bdd)
                        ((bdd-type-equal bdd new-right)
                         new-right)
                        ;; the next two clauses, which use CL:SUBTYPEP are necessary
                        ;; because the CL type system contains lots of identities
                        ;; which are difficult to encode.  such as
                        ;;   (nil = (and (not integer) (not ration) rational))
                        ;;  and several more.  Even if we could encode all these relationships,
                        ;;  there are more potential relationships every time a user's deftype
                        ;;  is evaluated.  For example, it might be that
                        ;;  (nil = (and user-type (not (cons string))))
                        ;;  but we can't find that out as there's no way to iterate
                        ;;  through all the user's type definitions and their expansions.
                        ((subtypep (bdd-to-dnf bdd) nil)
                         *bdd-false*)
                        ((subtypep t (bdd-to-dnf bdd))
                         *bdd-true*)
                        (t
                         bdd))))))))))))

(defun bdd-find-reduction (label bdd reduction-rules)
  (declare (type bdd bdd)
           (type list reduction-rules))
  "Apply each of the REDUCTION-RULES to BDD.  Some of the reduction rules may
result in reducing the BDD to a simpler form.   If no reduction rule applies
then NIL is returned, otherwise the reduced BDD is returned.
Each element of REDUCTION-RULES is a plist having at least the keys
  :RELATION - a relation between two type specifiers, eg., #'SMARTER-SUBTYPEP
  :REDUCTION - a function from BDD->BDD, which normally returns either 
             the left or right child E.g., #'BDD-LEFT or #'BDD-RIGHT"
  (let ((reduced (reduce (lambda (bdd reduction-rule)
                           (destructuring-bind (&key
                                                  (relation (lambda (a b)
                                                              (declare (ignore a b))
                                                              nil))
                                                  (reduction #'identity) &allow-other-keys) reduction-rule
                             (declare (type (function (t t) t) relation)
                                      (type (function (bdd) bdd) reduction))
                             (cond
                               ((typep bdd 'bdd-leaf)
                                bdd)
                               ((funcall relation label (bdd-label bdd))
                                (funcall reduction bdd))
                               (t
                                 bdd))))
                         reduction-rules
                         :initial-value bdd)))
    (if (eq bdd reduced)
        nil
        reduced)))
      
(defun bdd-reduce (label bdd search)
  "This recursive function starts at a BDD whose bdd-label is LABEL, but during the
recursive descent, LEVEL remains fixed, while BDD walks the BDD dag.  At each step,
until we reach a leaf, we call BDD-FIND-REDUCTION to see if the BDD can be reduced
according to the LABEL which is now the label of some parent in its lineage."
  (declare (type bdd bdd)
           (type list search))
  (labels ((recure (bdd parents)
             (cond
               ((typep bdd 'bdd-leaf)
                bdd)
               ((bdd-find-reduction label bdd search))
               (t
                (%bdd-node (bdd-label bdd)
                           (recure (bdd-left bdd) (cons bdd parents))
                           (recure (bdd-right bdd) (cons bdd parents)))))))
    (recure bdd nil)))

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

(defgeneric bdd-cmp (t1 t1))
(defmethod bdd-cmp :around (t1 t2)
  (cond
    ((equal t1 t2)
     '=)
    ((eql (type-of t1) (type-of t2))
     (call-next-method))
    ((listp t2)
     '<)
    ((listp t1)
     '>)
    (t
     (bdd-cmp (type-of t1) (type-of t2)))))

(defmethod bdd-cmp ((l1 list) (l2 list))
  "Compare two lists by finding the first corresponding elements
which are not = (according to BDD-CMP) and comparing them with BDD-CMP."
  (mapcar (lambda (e1 e2 &aux (c (bdd-cmp e1 e2)))
            (case c
              ((=)
               nil)
              (t
               (return-from bdd-cmp c))))
          l1 l2)
  '=)

(defmethod bdd-cmp ((s1 symbol) (s2 symbol))
  (cond
    ((eq s1 s2)
     '=)
    ((string< s1 s2)
     '<)
    (t
     '>)))

(defmethod bdd-cmp (t1 t2)
  (error "cannot compare a ~A with a ~A" (class-of t1) (class-of t2)))

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
    (bdd-op #'bdd-or b1 b2))

  (defmethod bdd-and ((b1 bdd-node) (b2 bdd-node))
    (bdd-op #'bdd-and b1 b2))

  (defmethod bdd-and-not ((b1 bdd-node) (b2 bdd-node))
    (bdd-op #'bdd-and-not b1 b2)))

(defmethod bdd-or (b1 b2)
  (error "bdd-or not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and (b1 b2)
  (error "bdd-and not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and-not (b1 b2)
  (error "bdd-and-not not implemented for ~A and ~A" b1 b2))

(defun bdd-to-dnf (bdd)
  "Convert a BDD to logical expression in DNF (disjunctive normal form), i.e. an OR of ANDs."
  (let (disjunctions)
    (labels ((wrap (op zero forms)
               (cond ((cdr forms)
                      `(,op ,@forms))
                     (forms
                      (car forms))
                     (t
                      zero)))
             (remove-supers (type-specs &optional done &aux (t1 (car type-specs)))
               (if type-specs
                   (remove-supers (cdr type-specs)
                                  (if (exists type done
                                        (smarter-subtypep type t1))
                                      done
                                      (cons t1 done)))
                   done))

             (recure (bdd stack)
               (etypecase bdd
                 (bdd-true
                  (push (wrap 'and t (remove-supers stack)) disjunctions))
                 (bdd-false
                  nil)
                 (bdd-node
                  (recure (bdd-left bdd) (cons (bdd-label bdd) stack))
                  (recure (bdd-right bdd) (cons `(not ,(bdd-label bdd)) stack))))))
      (recure bdd nil)
      (wrap 'or nil disjunctions))))

(defun bdd-subtypep (t-sub t-super)
  (declare (type bdd t-super t-sub))
  ;; TODO--isn't there a quicker way to find out whether (bdd-and-not A B)
  ;;   is false other than calculating the whole thing?
  (eq *bdd-false* (bdd-and-not t-sub t-super)))

(defun bdd-empty-type (bdd)
  (bdd-subtypep bdd *bdd-false*))

(defun bdd-disjoint-types-p (bdd1 bdd2)
  (bdd-empty-type (bdd-and bdd1 bdd2)))

(defun bdd-type-equal (t1 t2)
  (declare (type bdd t1 t2))
  (and (bdd-subtypep t1 t2)
       (bdd-subtypep t2 t1)))

(defun bdd-reduce-lisp-type (type)
    "Given a common lisp type designator such as (AND A (or (not B) C)), 
convert it to DNF (disjunctive-normal-form)"
  (bdd-to-dnf (bdd type)))

(defvar *bdd-slicers* (list #'bdd-and
                            #'bdd-and-not
                            #'(lambda (a b) (bdd-and-not b a))))
(defun %bdd-decompose-types (type-specifiers)
  (bdd-with-new-hash
   (lambda ()
     (labels ((option-bdd (bdd)
                (if (bdd-empty-type bdd)
                    nil
                    (list bdd)))
              (slice-set (bdd-a bdd-b)
                ;; given two types expressed as bdds, return a list of at most three
                ;; bdds. a&b a&!b b&!a.  If any of the three corresponds to the nil
                ;; type, it is omitted from the returned list.
                (mapcan (lambda (f)
                          (option-bdd (funcall f bdd-b bdd-a)))
                        *bdd-slicers*))
              (slice (bdds bdd1)
                (let ((sliced (mapcan (lambda (bdd2)
                                        (slice-set bdd1 bdd2))
                                      bdds)))
                  ;; We remove the more complicated type.
                  ;;  e.g., we could count the atomic types and remove the bdd with more.
                  ;;  e.g., prefer to keep INTEGER and discard (AND RATIONAL (NOT RATIO))
                  ;; this works because remove-duplicates removes the element closer to the
                  ;; beginning of the list when choosing which of two elements to remove.
                  (remove-duplicates (sort sliced #'>
                                           :key #'(lambda (bdd)
                                                    (length (bdd-collect-atomic-types bdd))))
                                     :test #'bdd-type-equal)))
              (remove-supers (bdds)
                (remove-if (lambda (bdd1)
                             (exists bdd2 bdds
                               (and (not (eq bdd1 bdd2))
                                    (bdd-subtypep bdd2 bdd1)))) bdds)))
       (let* ((bdds (mapcan (lambda (type-specifier)
                             (option-bdd (bdd type-specifier)))
                            type-specifiers))
              (U (reduce #'bdd-or bdds :initial-value *bdd-false*)))
         (remove-supers
          (reduce #'slice (cdr bdds)
                  :initial-value (list (bdd-and U (car bdds))
                                       (bdd-and-not U (car bdds))))))))))

(defun bdd-collect-terms (bdd)
  (declare (type bdd bdd))
  "Return a list of bdds each of which has one path from the top to a t leaf.
The union, bdd-or, of the element of the return list represents the equivalent
type as the given bdd.  Otherwise stated, if the given bdd is assumed to be a sum
of min-terms, this function returns a list of the min-terms."
  (labels ((recure (term)
             (etypecase term
               (bdd-true
                (list term))
               (bdd-false
                nil)
               (bdd-node
                (nconc (mapcar (lambda (left)
                                 (%bdd-node (bdd-label term) left *bdd-false*))
                               (recure (bdd-left term)))
                       (mapcar (lambda (right)
                                 (%bdd-node (bdd-label term) *bdd-false* right))
                               (recure (bdd-right term))))))))
    (recure bdd)))

(defun bdd-decompose-types (type-specifiers)
  (mapcar #'bdd-to-dnf
          (%bdd-decompose-types type-specifiers)))

(defun bdd-find-dup-bdds (bdds)
  "A debugging function.  It can be used to find whether two (or more) bdds
in the given list have the same dnf form."
  (let ((hash (make-hash-table :test #'equal))
        dups)
    (dolist (bdd bdds)
      (push bdd (gethash (bdd-to-dnf bdd) hash nil)))
    (maphash (lambda (dnf bdds)
               (declare (ignore dnf))
               (when (cdr bdds)
                 (push (cons (bdd-to-dnf (car bdds)) bdds) dups)))
             hash)
    dups))

(defun bdd-collect-atomic-types (bdd)
  (let (labels)
    (labels ((recure (bdd)
               (etypecase bdd
                 (bdd-leaf
                  nil)
                 (bdd-node
                  (pushnew (bdd-label bdd) labels :test #'equal)
                  (recure (bdd-left bdd))
                  (recure (bdd-right bdd))))))
      (recure bdd)
      labels)))

(defun check-decomposition (given calculated)
  "debugging function to assure that a given list of types GIVEN corresponds correctly
to a set of types returned from %bdd-decompose-types."
  (bdd-with-new-hash
   (lambda ()
     (let ((bdd-given (bdd `(or ,@given)))
           (bdd-calculated (bdd `(or ,@calculated))))
       (unless (bdd-subtypep bdd-given bdd-calculated)
         (error "union of given types ~A is not a subset of union of~%    calculated types ~A~%difference is ~A"
                given calculated (bdd-to-dnf (bdd-and-not bdd-given bdd-calculated))))
       (unless (bdd-subtypep bdd-calculated bdd-given)
         (error "union of calculated types ~A is not a subset of~%    union of given types ~A~%difference is ~A"
                calculated given (bdd-to-dnf (bdd-and-not bdd-calculated bdd-given))))
       (dolist (c calculated)
         (when (bdd-empty-type (bdd c))
           (error "calculated empty type ~A" c))
         (unless (exists g given
                   (bdd-subtypep (bdd c) (bdd g)))
           (error "calculated type ~A is not a subset of any given type ~A"
                  c given))
         (dolist (c2 (remove c calculated))
           (when (bdd-type-equal (bdd c2) (bdd c))
             (error "calculated two equal types ~A = ~A" c c2))))))))
