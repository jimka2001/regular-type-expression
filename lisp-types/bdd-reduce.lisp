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

(in-package   :lisp-types)

(labels ((incr-hash ()
           (incf *bdd-hash-access-count*)
           (when *bdd-verbose*
             (when (= 0 (mod *bdd-hash-access-count* 10000))
               (format t "bdd hash = ~A wall-time=~A cpu-time=~A~%"
                       *bdd-hash*
                       (truncate (get-internal-run-time) internal-time-units-per-second)
                       (truncate (get-universal-time) internal-time-units-per-second)))))
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
      
      

(defun bdd-to-if-then-else-4 (bdd obj)
  "expand into linear size code as TAGBODY/GO, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping (num 0))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (incf num)) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false `(return nil))
                 (bdd-true `(return t))
                 (bdd-node
                  `(go ,(cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  `(,(cadr (assoc bdd bdd->name-mapping))
                    (if (typep ,obj ',(bdd-label bdd))
                        ,(branch (bdd-left bdd))
                        ,(branch (bdd-right bdd))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (block nil
           (tagbody 
              ,@(mapcan #'label-function
                        (mapcar #'car (reverse bdd->name-mapping)))))))))


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
   (lambda (&aux (bdds (remove-if #'bdd-empty-type (mapcar #'bdd type-specifiers))))
     (labels ((try (bdds disjoint-bdds)
                (cond
                  ((null bdds)
                   disjoint-bdds)
                  (t
                   (let ((bdd-a (car bdds)))
                     (destructuring-bind (all-disjoint? bdd-set)
                         (reduce (lambda (acc bdd-b &aux (bdd-ab (bdd-and bdd-a bdd-b)))
                                   (destructuring-bind (all-disjoint? bdd-set) acc
                                     (cond
                                       ((bdd-empty-type bdd-ab)
                                        ;; If the intersection of A and B is the empty type,
                                        ;; then we don't need to calculate A\B and B\A because
                                        ;; we know that A\B = A and B\A = B.
                                        ;; Thus we simply add B to the bdd-set being accumulated.
                                        (list all-disjoint? (adjoin bdd-b bdd-set)))
                                       (t
                                        ;; If the interesction of A and B is non empty,
                                        ;; then we augment bdd-set with at most 3 types.  Looking at
                                        ;; {AB, A\B, B\A} \ {{}}, some of which might be equal, so we
                                        ;; remove duplicates, and accumulate also all-disjoint?=nil because
                                        ;; we've found something A is not disjoint with.
                                        (list nil (union (remove-duplicates
                                                          (remove-if #'bdd-empty-type
                                                                     (list bdd-ab
                                                                           (bdd-and-not bdd-a bdd-ab)
                                                                           (bdd-and-not bdd-b bdd-ab))))
                                                         bdd-set))))))
                                 (cdr bdds)
                                 :initial-value '(t nil))
                       (try bdd-set
                            (if all-disjoint?
                                (pushnew bdd-a disjoint-bdds)
                                disjoint-bdds))))))))
       (try bdds nil)))))

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

(defun boolean-expr-to-latex (expr &optional (stream t))
  (etypecase expr
    ((eql nil)
     (format stream "\\bot"))
    ((eql t)
     (format stream "\\top"))
    ((not list)
     (format stream "~A" expr))
    ((cons (eql and))
          (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\wedge ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql or))
     (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\vee ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql not))
     (format stream "\\neg ")
     (boolean-expr-to-latex (cadr expr) stream))))

