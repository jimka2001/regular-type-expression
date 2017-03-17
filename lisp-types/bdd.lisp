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

(defun bdd-new-hash ()
  (make-hash-table :test #'equal))

(defvar *bdd-hash* (bdd-new-hash))

(defun bdd-with-new-hash (thunk)
  (let ((*bdd-hash* (bdd-new-hash))
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
      (format t "finished with ~A and ~A~%" *bdd-hash* *subtype-hash*))))
  
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

(defun check-table ()
  nil
  ;; (maphash (lambda (key1 bdd1)
  ;;            (maphash (lambda (key2 bdd2)
  ;;                       (cond ((eq bdd1 bdd2))
  ;;                             ((not (equal (bdd-to-dnf bdd1)
  ;;                                          (bdd-to-dnf bdd2))))
  ;;                             (t
  ;;                              (assert nil (bdd1 bdd2 key1 key2)
  ;;                                      "two bdds have some DNF"))))
  ;;                     *bdd-hash*))
  ;;          *bdd-hash*)
  )
                               


(defvar *bdd-hash-access-count* 0)
(labels ((incr-hash ()
           (incf *bdd-hash-access-count*)
           (when (= 0 (mod *bdd-hash-access-count* 10000))
             (format t "bdd hash = ~A wall-time=~A cpu-time=~A~%"
                     *bdd-hash*
                     (truncate (get-internal-run-time) internal-time-units-per-second)
                     (truncate (get-universal-time) internal-time-units-per-second))))
         (relation (r x-parity y-parity)
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
         ;; :relation ... :reduction 
         (left-reductions  (mapcar #'cddddr (setof r reductions
                                              (eq :left (getf r :child)))))
         (right-reductions (mapcar #'cddddr (setof r reductions
                                              (eq :right (getf r :child))))))

    (defun %bdd-node (label left-bdd right-bdd)
      (cond
        ((eq left-bdd right-bdd) ;; 26%
         left-bdd)
        ((bdd-find *bdd-hash* label left-bdd right-bdd)) ;; 63%
        (t ;; 11%
         (let ((new-left  (bdd-reduce label left-bdd  left-reductions))
               (new-right (bdd-reduce label right-bdd right-reductions)))
           (cond
             ((eq new-left new-right) ;; 2.5%
              new-left)
             ((bdd-find *bdd-hash* label new-left new-right)) ;;7%
             (t
              (let* ((bdd (make-instance 'bdd-node
                                         :label label
                                         :left  new-left
                                         :right new-right))
                     (key (bdd-make-key label (bdd-ident new-left) (bdd-ident new-right))))
                (incr-hash)
                (setf (gethash key *bdd-hash*) bdd)
                (setf (gethash key *bdd-hash*)
                      (cond
                        ;; check (bdd-and-not bdd new-left)
                        ;;   vs  (bdd-and-not new-left bdd)
                        ((bdd-type-equal bdd new-left) ;; 0.006%   ;; TODO perhaps it is more interesting to check equivalance first to the 'smaller' of new-left and new-right, not sure because checking for smaller might be slow, and making a new slot to store the size might expand memory enough to also make the program slower?
                         new-left)
                        ;; check (bdd-and-not bdd new-right)
                        ;;   vs  (bdd-and-not new-right bdd)
                        ((bdd-type-equal bdd new-right) ;; 0.5%
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
                        ((smarter-subtypep (bdd-to-expr bdd) nil) ;; 0.03%
                         *bdd-false*)
                        ((smarter-subtypep t (bdd-to-expr bdd))
                         *bdd-true*)
                        (t ;; 1.7%
                         bdd))))))))))))

(defun bdd-find-reduction (label bdd reduction-rules)
  (declare (type bdd bdd)
           (type list reduction-rules)
           (optimize (speed 3) (safety 0)))
  "Apply each of the REDUCTION-RULES to BDD.  Some of the reduction rules may
result in reducing the BDD to a simpler form.   If no reduction rule applies
then NIL is returned, otherwise the reduced BDD is returned.
Each element of REDUCTION-RULES is a plist having at least the keys
  :RELATION - a relation between two type specifiers, eg., #'SMARTER-SUBTYPEP
  :REDUCTION - a function from BDD->BDD, which normally returns either 
             the left or right child E.g., #'BDD-LEFT or #'BDD-RIGHT"
  (let ((reduced (reduce (lambda (bdd reduction-rule-plist)
                           (cond
                             ((typep bdd 'bdd-leaf)
                              bdd)
                             ((funcall (the function (getf reduction-rule-plist :relation)) label (bdd-label bdd))
                              (funcall (the function (getf reduction-rule-plist :reduction)) bdd))
                             (t
                              bdd)))
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
  (labels ((recure (bdd)
             (cond
               ((typep bdd 'bdd-leaf)
                bdd)
               ((bdd-find-reduction label bdd search))
               (t
                (%bdd-node (bdd-label bdd)
                           (recure (bdd-left bdd))
                           (recure (bdd-right bdd)))))))
    (recure bdd)))

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

(defun x%bdd-to-dnf (bdd)
  "Convert a BDD to logical expression in DNF (disjunctive normal form), i.e. an OR of ANDs."
  (declare (type bdd bdd))
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
      (wrap 'or nil disjunctions)
      ;; (flet ((make-member (objects)
      ;;          (cond ((cdr objects)
      ;;                 (cons 'member objects))
      ;;                (objects
      ;;                 (cons 'eql objects))
      ;;                (t
      ;;                 'null))))
                      
      ;;   (let ((disjunctions (loop for type in disjunctions
      ;;                             collect (typecase type
      ;;                                       ((cons (eql and))
      ;;                                        (let ((mem (find-if (lambda (obj)
      ;;                                                              (typep obj '(cons (member member eql))))
      ;;                                                            (cdr type))))
      ;;                                          (if mem
      ;;                                              (make-member (setof obj (cdr mem)
      ;;                                                             (forall t2 (cdr type)
      ;;                                                               (typep obj t2))))
      ;;                                              type)))
      ;;                                       (t
      ;;                                        type)))))
      ;;     (declare (notinline set-difference))
      ;;     (let ((disjunctions (let* ((mems (setof type disjunctions
      ;;                                        (typep type '(cons (member member eql)))))
      ;;                                (non-mems (set-difference disjunctions mems)))
      ;;                           (if mems
      ;;                               (cons (make-member (reduce #'union (mapcar #'cdr mems) :initial-value nil))
      ;;                                     non-mems)
      ;;                               disjunctions))))
      ;;       (wrap 'or nil disjunctions))))
      )))


(defun remove-super-types (type-specs)
  (cond
    ((null type-specs)
     nil)
    ((null (cdr type-specs))
     type-specs)
    (t
     (let ((new (reduce (lambda (t1 specs)
                          (cond
                            ((exists t2 specs
                               (subtypep t2 t1))
                             specs)
                            (t
                             (cons t1 (setof t2 specs
                                        (not (subtypep t1 t2)))))))
                        type-specs :initial-value nil :from-end t)))
       ;; if there were no supers to remove, then return the original list
       ;; allowing the newly allocated one to be GC-ed.
       (if (equal new type-specs)
           type-specs
           new)))))

(defun %bdd-to-dnf (bdd)
  "Convert a BDD to logical expression in DNF (disjunctive normal form), i.e. an OR of ANDs.
The construction attempts re-use cons cells in order to reduce the memory footprint of a large
set of BDDs."
  (declare (type bdd bdd))
  (labels (
           (wrap (op zero forms)
             (cond ((cdr forms)
                    (cons op forms))
                   (forms
                    (car forms))
                   (t
                    zero)))
           (prepend (head dnf)
             (typecase dnf
               ((cons (eql or))
                (wrap
                 'or nil
                 (mapcar (lambda (tail)
                           (prepend head tail))
                         (cdr dnf))))
               ((cons (eql and))
                (wrap 'and t (remove-super-types (cons head (cdr dnf)))))
               ((eql t)
                head)
               ((eql nil)
                nil)
               (t
                (wrap 'and t (remove-supers (list head dnf))))))
           (disjunction (left right)
             (cond
               ((null left)
                right)
               ((null right)
                left)
               ((and (typep left '(cons (eql or)))
                     (typep right '(cons (eql or))))
                (cons 'or (nconc (copy-list (cdr left)) (cdr right))))
               ((typep left '(cons (eql or)))
                (wrap 'or nil (cons right (cdr left))))
               ((typep right '(cons (eql or)))
                (wrap 'or nil (cons left (cdr right))))
               (t
                (wrap 'or nil (list left right))))))
    
    (let ((left-terms  (prepend (bdd-label bdd) (bdd-to-dnf (bdd-left bdd))))
          (right-terms (prepend `(not ,(bdd-label bdd)) (bdd-to-dnf (bdd-right bdd)))))
      (disjunction left-terms
                   right-terms))))

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

(defun bdd-to-if-then-else-1 (bdd obj)
  "expand into worse-case exponentially large code as IF-THEN-ELSE, whose run-time is logrithmic."
  (labels ((expand (bdd)
             (typecase bdd
               (bdd-false nil)
               (bdd-true t)
               (bdd-node `(if (typep ,obj ',(bdd-label bdd))
                              ,(expand (bdd-left bdd))
                              ,(expand (bdd-right bdd)))))))
    (typecase bdd
      (bdd-false
       `(lambda (,obj)
          (declare (ignore ,obj))
          nil))
      (bdd-true
       `(lambda (,obj)
          (declare (ignore ,obj))
          t))
      (bdd-node
       `(lambda (,obj)
          ,(expand bdd))))))

(defun topological-sort (graph &key (test 'eql))
  ;; this function was taking verbatim from rosettacode.org
  ;; https://rosettacode.org/wiki/Topological_sort#Common_Lisp
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (vertex)
             "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
             (multiple-value-bind (entry presentp) (gethash vertex entries)
               (if presentp entry
                 (setf (gethash vertex entries) (cons 0 '()))))))
      ;; populate entries initially
      (dolist (vertex graph)
        (destructuring-bind (vertex &rest dependencies) vertex
          (let ((ventry (entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (entry dependency)))
                (unless (funcall test dependency vertex)
                  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (entry dependant))))
                (push dependant S)))))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))

;; (bdd-to-if-then-else-2 (bdd '(or (and sequence (not array))
;;                                      number
;;                               (and (not sequence) array))) 'X)

(defun bdd-to-if-then-else-2 (bdd obj)
  "expand into linear size code as LET*, whose runtime is linear"
  (let ((constraints (make-hash-table :test #'eq)))
    (labels ((calc-constraints (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (nth-value 1 (gethash bdd constraints))
                    (setf (gethash bdd constraints) nil))
                  (pushnew bdd (gethash (bdd-left bdd) constraints nil) :test #'eq)
                  (pushnew bdd (gethash (bdd-right bdd) constraints nil) :test #'eq)
                  (calc-constraints (bdd-left bdd))
                  (calc-constraints (bdd-right bdd))))))
      (calc-constraints bdd)
      ;; constraints is a hash table mapping BEFORE to a list of BDDS which which BEFORE must preceed in the sorted list.

      (let* ((nodes (topological-sort (let (collected)
                                        (maphash (lambda (key value)
                                                   (push (cons key value) collected)) constraints)
                                        collected) :test #'eq))
             (name-map (mapcar (lambda (node)
                                 (list node (gensym "N")))
                               nodes))
             (vars (mapcar (lambda (node)
                             (typecase node
                               (bdd-false
                                (list (cadr (assoc node name-map)) nil))
                               (bdd-true
                                (list (cadr (assoc node name-map)) t))
                               (bdd-node
                                (list (cadr (assoc node name-map))
                                      `(if (typep ,obj ',(bdd-label node))
                                           ,(cadr (assoc (bdd-left node) name-map))
                                           ,(cadr (assoc (bdd-right node) name-map)))))))
                           (reverse nodes))))
        `(lambda (,obj)
           (let* ,vars
             ,(caar (last vars))))))))



(defun bdd-to-if-then-else-3 (bdd obj)
  "expand into linear size code as LABELS, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping)
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (gensym)) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false nil)
                 (bdd-true t)
                 (bdd-node
                  (list (cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  `(,(cadr (assoc bdd bdd->name-mapping)) ()
                    (if (typep ,obj ',(bdd-label bdd))
                        ,(branch (bdd-left bdd))
                        ,(branch (bdd-right bdd))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (labels ,(mapcar #'label-function (mapcar #'car bdd->name-mapping))
           ,(branch bdd))))))

;; (bdd-to-if-then-else-3 (bdd '(or (and sequence (not array))
;;                                      number
;;                               (and (not sequence) array))) 'X)
      
      

(defun bdd-typep (obj type-specifier)
  "This function has the same syntax as CL:TYPEP, but using a BDD based algorithm " 
  (bdd-type-p obj (bdd type-specifier)))

(define-compiler-macro bdd-typep (obj type-specifier)
  (typecase type-specifier
    ((cons (eql quote))
     (bdd-with-new-hash
      (lambda (&aux (bdd (bdd (cadr type-specifier))))
        `(funcall ,(bdd-to-if-then-else-3 bdd (gensym)) ,obj))))
    (t
     `(typep ,obj ,type-specifier))))


;; (funcall (compiler-macro-function 'bdd-typep) '(bdd-typep X '(or (and sequence (not array))
;;                                       number
;;                                (and (not sequence) array))) nil)


(defun bdd-type-p (obj bdd)
  "Similar semantics to TYPEP but takes a bdd rather than a type-specifier.
If a CL type specifier is given as 2nd argument, it is interpreted as
the corresponding BDD object, via a call to the function BDD.
Returns T if the OBJ of an element of the specified type,
Returns NIL otherwise."
  (etypecase bdd
    (bdd-false
     nil)
    (bdd-true
     t)
    (bdd-node
     (bdd-type-p obj 
                 (if (typep obj (bdd-label bdd))
                     (bdd-left bdd)
                     (bdd-right bdd))))
    (t
     (bdd-type-p obj (the bdd (bdd bdd))))))

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
       (let ((bdds (mapcan (lambda (type-specifier)
                             (option-bdd (bdd type-specifier)))
                           type-specifiers)))
         (when bdds
           (let* ((U (reduce #'bdd-or bdds :initial-value *bdd-false*))
                  (init (list (bdd-and U (car bdds))
                              (bdd-and-not U (car bdds)))))
             (remove-supers
              (reduce #'slice (cdr bdds)
                      :initial-value (remove *bdd-false* init))))))))))

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
  (when type-specifiers
    (with-disjoint-hash
        (lambda ()
          (with-subtype-hash
              (lambda ()
                (mapcar #'bdd-to-dnf
                        (%bdd-decompose-types type-specifiers))))))))

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

