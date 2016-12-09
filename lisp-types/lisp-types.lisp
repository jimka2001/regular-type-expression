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

(cl:defpackage :lisp-types
  (:use :cl)
  (:export
   "DECOMPOSE-TYPES"
   "DECOMPOSE-TYPES-GRAPH"
   "DECOMPOSE-TYPES-SAT"
   "VALID-TYPE-P"
   "REDUCE-LISP-TYPE"
   "REDUCED-TYPECASE"
   "OPTIMIZED-TYPECASE"
   "DISJOINT-TYPES-P"
   "SMARTER-SUBTYPEP"
   "EQUIVALENT-TYPES-P"
   "AMBIGUOUS-SUBTYPE"
   "AUTO-PERMUTE-TYPECASE"
   ))

(in-package   :lisp-types)

(define-condition ambiguous-subtype (style-warning)
  ((sub   :type (or symbol nil cons) :initarg :sub :initform :UNINITIALIZED)
   (super :type (or symbol nil cons) :initarg :super :initform :UNINITIALIZED)
   (consequence :type (or string nil) :initarg :consequence :initform nil))
  (:documentation "Warning raised when unable to determine the subtype relationship.")
  (:report (lambda (condition stream)
	     (format stream "Cannot determine whether ~S is a subtype of ~S"
		     (slot-value condition 'sub)
		     (slot-value condition 'super))
	     (when (slot-value condition 'consequence)
	       (format stream ", ~A" (slot-value condition 'consequence))))))
		     
(defun hash-to-list (hash &aux list)
  "HASH is a hashtable with test=EQUAL which has been used with ENTER-CONSES.
  HASH-TABLE-returns the list of lists which ENTER-CONSES has accumulated."
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (push key list)) hash)
  list)

(defun enter-conses (hash object)
  "HASH is a hash-tabel which uses EQUAL as test.
   OBJECT is any lisp object.
   ENTER-CONSES returns an object which is EQUAL to the given OBJECT,
   but subsequent times ENTER-CONSES is called with another such EQUAL
   object, the value returned is EQ.
   E.g., If (EQUAL A B), then (ENTER-CONSES hash A) and (ENTER-CONSES hash B)
   returns values which are EQ to each other.
   In addition, The elements of the given list (if OBJECT is indeed a list)
   share the same EQ property.  I.e., if A is a list, and B is a list and A and B
   contain elements which are EQUAL to each other, they the corresponding values
   in the return value will be EQ."
  (cond
    ((atom object)
     object)
    ((gethash object hash))
    (t
     (setf object (cons (enter-conses hash (car object))
			(enter-conses hash (cdr object))))
     (setf (gethash object hash) object))))


(defmacro multiple-value-destructuring-bind (destructuring-lambda-list form &body body)
  `(destructuring-bind ,destructuring-lambda-list (multiple-value-list ,form)
     ,@body))

(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defun shuffle-list (data)
  (labels ((split (data)
	     (let (l1 l2)
	       (while data
		 (if (zerop (random 2))
			     (push (pop data) l1)
			     (push (pop data) l2)))
	       (values l1 l2)))
	   (fold (l1 l2 &aux data)
	     (loop :while (or l1 l2)
		   :do (cond
			 ((null l1)
			  (push (pop l2) data))
			 ((null l2)
			  (push (pop l1) data))
			 ((zerop (random 2))
			  (push (pop l2) data))
			 (t
			  (push (pop l1) data))))
	     data))
    (dotimes (n 15)
      (multiple-value-bind (l1 l2) (split data)
	(setq data (fold l1 l2))))
    data))
    
(defun valid-type-p (type-designator)
  #+sbcl (and (SB-EXT:VALID-TYPE-SPECIFIER-P type-designator)
	      (not (eq type-designator 'cl:*)))
  #+(or clisp  allegro) (ignore-errors (subtypep type-designator t))
  #-(or sbcl clisp allegro) (error "VALID-TYEP-P not implemented for ~A" (lisp-implementation-type))
)

(assert (not (valid-type-p (gensym))))
(assert (valid-type-p 'bignum))

;; TODO need to update some calls to subtypep to use smarter-subtypep instead.
(defun smarter-subtypep (t1 t2)
  "The sbcl subtypep function does not know that (eql :x) is a subtype of keyword,
this function SMARTER-SUBTYPEP understands this."
  (declare (optimize (speed 3) (compilation-speed 0)))
  (multiple-value-bind (T1<=T2 OK) (subtypep t1 t2)
    (cond
      (OK
       (values T1<=T2 t))
      ((typep t1 '(cons (member eql member))) ; (eql obj) or (member obj1 ...)
       (values (every #'(lambda (obj)
			  (declare (notinline typep))
			  (typep obj t2))
		      (cdr t1))
	       t))
      ;; T1 <: T2 ==> not(T2) <: not(T1)
      ((and (typep t1 '(cons (eql not)))
	    (typep t2 '(cons (eql not))))
       (smarter-subtypep (cadr t2) (cadr t1)))
      ;; T1 <: T2 ==> not( T1 <= not(T2))
      ((and (typep t2 '(cons (eql not)))
	    (smarter-subtypep t1 (cadr t2)))
       (values nil t))
      ;; T1 <: T2 ==> not( not(T1) <= T2)
      ((and (typep t1 '(cons (eql not)))
	    (smarter-subtypep (cadr t1) t2))
       (values nil t))
      (t
       (values nil nil)))))

(defun xor (a b)
  (or (and a (not b))
      (and (not a) b)))

(defun void-type-p (type)
  (subtypep type nil))

(defun universal-type-p (type)
  (subtypep t type))

(defun disjoint-types-p (T1 T2 &aux X Y)
  "Two types are considered disjoint, if their interseciton is empty,
i.e., is a subtype of nil."
  (declare (optimize (speed 3) (compilation-speed 0))
	   (notinline subsetp))
  (multiple-value-bind (disjointp OK) (subtypep `(and ,T1 ,T2) nil)
    (cond
      (OK
       (values disjointp t))
      ((subsetp '((t t) (nil t))
		(list (setq X (multiple-value-list (smarter-subtypep T1 T2)))
		      (multiple-value-list (smarter-subtypep T2 T1)))
                :test #'equal)
       ;; Is either  T1<:T2 and not T2<:T1
       ;;    or      T2<:T1 and not T1<:T2 ?
       ;; if so, then one is a propert subtype of the other.
       ;; thus they are not disjoin.t
       (values nil t))
      ;;  T1 ^ T2 = 0 ==> !T1 ^ T2 != 0 if T1!=1 and T2 !=0
      ;; !T1 ^ T2 = 0 ==>  T1 ^ T2 != 0 if T1!=0 and T2 !=0
      ((and (typep T1 '(cons (eql not)))
            (not (void-type-p (cadr T1)))
            (not (void-type-p T2))
	    (disjoint-types-p (cadr T1) T2))
       (values nil t))
      ;; T1 ^  T2 = 0 ==> T1 ^ !T2 != 0  if T1!=0 and T2!=1
      ;; T1 ^ !T2 = 0 ==> T1 ^  T2 != 0  if T1!=0 and T2!=0
      ((and (typep T2 '(cons (eql not))) 
            (not (void-type-p T1))
            (not (void-type-p (cadr T2)))
	    (disjoint-types-p T1 (cadr T2)))
       (values nil t))
      ;; e.g., (disjoint-types-p (not float) number) ==> (nil t)
      ;;       (disjoint-types-p (not number) float) ==> (t t)
      ((and (typep T1 '(cons (eql not)))
	    (setq Y (multiple-value-list (smarter-subtypep (cadr T1) T2)))
	    (setq X (multiple-value-list (smarter-subtypep T2 (cadr T1))))
	    (subsetp '((t t) (nil t)) (list X Y) :test #'equal))
       (values (car X) t))
      ;; e.g., (disjoint-types-p float (not number)) ==> (t t)
      ;;       (disjoint-types-p number (not float)) ==> (nil t)
      ((and (typep T2 '(cons (eql not)))
	    (setq Y (multiple-value-list (smarter-subtypep T1 (cadr T2))))
	    (setq X (multiple-value-list (smarter-subtypep (cadr T2) T1)))
	    (subsetp '((t t) (nil t)) (list X Y) :test #'equal))
       (values (car Y) t))
      ((or (smarter-subtypep T1 T2)
	   (smarter-subtypep T2 T1))
       (values nil t))
      (t
       (values nil nil)))))

(defun equivalent-types-p (T1 T2)
  "Two types are considered equivalent if each is a subtype of the other."
  (multiple-value-bind (T1<=T2 okT1T2) (smarter-subtypep T1 T2)
    (multiple-value-bind (T2<=T1 okT2T2) (smarter-subtypep T2 T1)
      (values (and T1<=T2 T2<=T1) (and okT1T2 okT2T2)))))

(defun set-equalp (set-a set-b &key (test #'equal))
  (declare (notinline set-exclusive-or))
  (not (set-exclusive-or set-a set-b :test test)))

(defun set-subsetp (set-sub set-super &key (test #'equal))
  (every (lambda (s)
	   (member s set-super :test test))
	 set-sub))

(defmacro pattern-bind (destructuring-lambda-list object &rest body)
  "Like destructuring-bind, but if you use a variable named _ (one or more times)
an automatic (declare (ignore _)) will be generated at the head of the body. 
WARNING: this function is not robust enough to destinguish _ used a variable declaration
vs in an evaluation position; it simply checks whether the lambda-list contains the
symbol _ somewhere (recursively)."
  (labels ((find_ (obj)
	     (cond ((eq '_ obj)
		    t)
		   ((listp obj)
		    (some #'find_ obj)))))
    (if (find_ destructuring-lambda-list)
	`(destructuring-bind ,destructuring-lambda-list ,object
	   (declare (ignore _))
	   ,@body)
	`(destructuring-bind ,destructuring-lambda-list ,object
	   ,@body))))

(defun partition-by-predicate (predicate data)
  (let (true-elements false-elements)
    (dolist (element data)
      (if (funcall predicate element)
	  (push element true-elements)
	  (push element false-elements)))
    (values true-elements false-elements)))

(defun cmp-objects (a b)
  (cond ((equal a b)
	 t)
	((null a)
	 t)
	((null b)
	 nil)
	((not (eql (class-of a) (class-of b)))
	 (cond 
	   ((and (atom a) (listp b))
	    t)
	   ((and (listp a) (atom b))
	    nil)
	   (t
	    (cmp-objects (class-name (class-of a)) (class-name (class-of b))))))
	((and (symbolp a) (symbolp b))
	 (if (equal (symbol-package a) (symbol-package b))
	     (string<= a b)
	     (string<= (package-name (symbol-package a))
		       (package-name (symbol-package b)))))
	((and (stringp a) (stringp b))
	 (string<= a b))
	((and (numberp a) (numberp b))
	 (<= a b))
	((and (characterp a) (characterp b))
	 (char<= a b))
	((not (and (listp a) (listp b)))
	 (error "cannot compare ~A ~A with ~A ~A" (class-of a) a (class-of b) b))
	((equal (car a) (car b))
	 (cmp-objects (cdr a) (cdr b)))
	(t
	 (cmp-objects (car a) (car b)))))

(defun alphabetize (patterns)
  "non-descructively sort a list of patterns into a canonical order."
  (declare (type list patterns))
  (sort (copy-list patterns) #'cmp-objects))

(defun alphabetize-type (type)
  (declare (optimize (speed 3) (compilation-speed 0)))
  (cond
    ((atom type)
     type)
    ((member (car type) '(and or not))
     (cons (car type) (alphabetize
		       (mapcar #'alphabetize-type (cdr type)))))
    ((eq 'cons (car type))
     (cons 'cons (mapcar #'alphabetize-type (cdr type))))
    ((eq 'member (car type))
     (cons 'member (alphabetize (cdr type))))
    (t
     type)))

(defmacro rule-case (object &body clauses)
"return the value of the first clause-value not EQUAL to OBJECT of the first clause
whose test is true, otherwise return OBJECT."
  (let ((new (gensym "new"))
        (old (gensym "old")))
    (labels ((expand-clause (clause)
               (destructuring-bind (test &body body) clause
                 (assert body () "invalid test/body used in RULE-CASE: ~A" clause)
                 `(when (and (equal ,new ,old)
                             ,test)
                   (setf ,new (progn ,@body)))))
             (expand-clauses ()
               (mapcar #'expand-clause clauses)))
      `(let* ((,new ,object)
              (,old ,new))
         ,@(expand-clauses)
         ,new))))

(proclaim '(inline sub-super))

(defun sub-super (types)
  (declare (optimize (speed 3) (compilation-speed 0)))
  (loop :for tail :on types
	:do (when (cdr types)
	      (loop :for t1 :in (cdr tail)
		    :with t2 = (car tail)
		    :do
		       (cond ((subtypep t1 t2)
			      (return-from sub-super (values t t1 t2)))
			     ((subtypep t2 t1)
			      (return-from sub-super (values t t2 t1)))))))
  (values nil))

(defun remove-subs (types)
  (declare (type list types)
	   (inline sub-super)
	   (optimize (speed 3) (compilation-speed 0)))
  (multiple-value-bind (match? sub super) (sub-super types)
    (declare (ignore super))
    (if match?
	(remove-subs (remove sub types :test #'eq)) ; tail call
	types)))

(defun remove-supers (types)
  (declare (type list types)
	   (inline sub-super)
	   (optimize (speed 3) (compilation-speed 0)))
  (declare (type list types))
  (multiple-value-bind (match? sub super) (sub-super types)
    (declare (ignore sub))
    (if match?
	(remove-supers (remove super types :test #'eq)) ; tail call
	types)))

(defun fixed-point (function arg &key (test #'equal))
  "Find the fixed point of a FUNCTION, starting with the given ARG."
  (declare (type (function (t) t) function))
  (let ((result (funcall function arg)))
	
    (loop :while (not (funcall test result arg))
	  :do (progn (setf arg result)
		     (setf result (funcall function arg))))
    result))

(defun perf-reduce ()
  (let ((EOF (list nil))
	data)
  (with-open-file (stream #p"/tmp/jnewton/types/expressions-12.data" :direction :input)
    (while (not (eq EOF (setf data (read stream nil EOF))))
      (reduce-lisp-type data)))))

(defmacro forall (var data &body body)
  `(every #'(lambda (,var) ,@body) ,data))

(defmacro setof (var data &body body)
  `(remove-if-not (lambda (,var) ,@body) ,data))


(defmacro prog1-let ((var expr) &body body)
  `(let ((,var ,expr))
     ,@body
     ,var))

