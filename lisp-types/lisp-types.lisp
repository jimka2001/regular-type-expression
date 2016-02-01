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
   "EQUIVALENT-TYPES-P"
   ))

(in-package   :lisp-types)


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

(defun disjoint-types-p (T1 T2)
  "Two types are considered disjoint, if their interseciton is empty, i.e., is a subtype of nil."
  (subtypep `(and ,T1 ,T2) nil))

(defun equivalent-types-p (T1 T2)
  "Two types are considered equivalent if each is a subtype of the other."
  (multiple-value-bind (T1<=T2 okT1T2) (subtypep T1 T2)
    (multiple-value-bind (T2<=T1 okT2T2) (subtypep T2 T1)
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

;; A + A!B = A + B
(defun reduce-redundancy (operands)
  (declare (type list operands)
	   (optimize (speed 3) (compilation-speed 0)))
  (labels ((and? (obj)
	     (and (consp obj)
		  (eq 'and (car obj))))
	   (not?  (obj)
	     (and (consp obj)
		  (eq 'not (car obj)))))
    (mapcar (lambda (op2)
	      (block reduce
		(cond
		  ((not (and? op2))
		   op2)
		  ((some (lambda (op1)
			   (cond
			     ((and (atom op1)
				   (member `(not ,op1) (cdr op2) :test #'equal))
			      ;; A + A!B -> A + B
			      (return-from reduce
				(cons 'and (remove `(not ,op1) (cdr op2) :test #'equal))))
			     ((and (not? op1)
				   (member (cadr op1) (cdr op2) :test #'equal))
			      ;; !A + AB --> !A + B
			      (return-from reduce
				(cons 'and (remove (cadr op1) (cdr op2) :test #'equal))))
			     (t
			      nil)))
			 operands)
					; line not reachable
		   )
		  (t
		   op2))))
	    operands)))

;; (or A (and A B C D) E)
;;   --> (or A E)
;; (or (and A B) (and A B C D) E F)
;;   --> (or (and A B) E F)
(defun reduce-absorption (operands)
  (declare (type list operands)
	   (optimize (speed 3) (compilation-speed 0)))
  (dolist (operand operands)
    (setf operands (remove-if (lambda (op)
				(cond ((eq op operand)
				       nil)
				      ((not (and (consp op)
						 (eq 'and (car op))))
				       nil)
				      ((member operand (cdr op))
				       t)
				      ((not (and (consp operand)
						 (eq 'and (car operand))))
				       nil)
				      (t
				       (set-subsetp (cdr operand) (cdr op)))))
			      operands)))
  operands)

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

(defun reduce-lisp-type-once (type)
  "Given a lisp type designator, make one pass at reducing it, removing redundant information such as
repeated or contradictory type designators."
  (declare (optimize (speed 3) (compilation-speed 0))
	   (inline sub-super reduce-absorption reduce-redundancy remove-supers remove-subs))
  (labels ((substitute-tail (list search replace)
	     (cons (car list)
		   (mapcar (lambda (e)
			     (if (equal e search)
				 replace
				 e))
			   (cdr list))))
	   (or? (obj)
	     (and (consp obj)
		  (eq 'or (car obj))))
	   (and? (obj)
	     (and (consp obj)
		  (eq 'and (car obj))))
	   (not? (obj)
	     (and (consp obj)
		  (eq 'not (car obj))))
	   (eql? (obj)
	     (and (consp obj)
		  (eq 'eql (car obj))))
	   (member? (obj)
	     (and (consp obj)
		  (eq 'member (car obj))))
	   (cons? (obj)
	     (and (consp obj)
		  (eq 'cons (car obj))))
	   (eql-or-member? (obj)
	     (and (consp obj)
		  (member (car obj) '(eql member) :test #'eq)))
	   (not-eql-or-member? (obj) ; (not (eql ...)) or (not (member ...))
	     (and (not? obj)
		  (consp (cadr obj))
		  (member (car (cadr obj)) '(eql member) :test #'eq))))

    (cond
      ((atom type)
       type)
      ((member? type)
       (if (cddr type)
	   type
	   (cons 'eql (cdr type))))
      ((and (member? type)
	    (not (cddr type))) ;; (member A) --> (eql A)
       (cons 'eql (cdr type)))
      ;; TODO - extend to understand other type specifiers which reference type specifiers such as
      ;;   (function (float float) number)
      ;;   (vector number)
      ;;   (complex float)
      ;;   etc
      ((cons? type) ; (cons (and float number) (or string (not string))) --> (cons float t)
       (cons 'cons (mapcar #'reduce-lisp-type-once (cdr type))))

      ((not (or (or? type)	       ; (number 1 8) --> (number 1 8)
		(and? type)
		(not? type)))
       type)
      (t ;; (or ...) (and ...) (not ...)
       (setf type (cons (car type)
			(remove-duplicates (cdr type) :test #'equal)))
       (setf type (cons (car type)
			(mapcar #'reduce-lisp-type-once (cdr type))))
       (destructuring-bind (operator &rest operands) type
	 (declare (type (member and or not) operator)
		  (type list operands))
	 (ecase operator
	   ((and)			; REDUCE AND
	    (setf operands (remove-supers operands)) ; (and float number) --> (and float)
	    (while (some #'and? operands)
					; (and (and A B) X Y) --> (and A B X Y)
	      (setf operands (mapcan #'(lambda (operand)
					 (if (and? operand)
					     (copy-list (cdr operand))
					     (list operand)))
				     operands)))
	    (when (member t operands)
					; (and A t B) --> (and A B)
	      (setf operands (remove t operands)))
	    (rule-case type
	      ((null operands)		; (and) --> t
	       t)
	      ((null (cdr operands))	; (and A) --> A
	       (car operands))
	      ((member nil operands)	; (and A nil B) --> nil
	       nil)
	      ((some #'or? operands) 
	       (let* ((match (find-if #'or? operands))
		      (and-operands (remove match operands :test #'eq)))
		 (cons 'or
		       (loop :for or-operand :in (cdr match)
			     :collect (cons 'and (cons or-operand and-operands))))))
	      ((some #'eql-or-member? operands)
	       ;; (and (member a b 2 3) symbol) --> (member a b)
	       ;; (and (member a 2) symbol) --> (eql a)
	       ;; (and (member a b) fixnum) --> nil
	       (let ((objects (remove-if-not (lambda (e)
					       (typep e type)) (cdr (find-if #'eql-or-member? operands)))))
		 (cond ((null objects)
			nil)
		       ((null (cdr objects))
			(cons 'eql objects))
		       (t
			(cons 'member objects)))))		     
	      ((< 1 (count-if #'not-eql-or-member? operands))
	       ;; (and A B (not (member 1 2 a)) (not (member 2 3 4 b)))
	       ;;   --> (and A B (not (member 1 2 3 4 a b)))
	       (multiple-value-bind (not-matches others) (partition-by-predicate #'not-eql-or-member? operands)
		 (let ((not-common (cdr (cadr (car not-matches)))))
		   (declare (notinline union)
			    (notinline set-difference))
		   (dolist (not-match (cdr not-matches))
		     (setq not-common (union not-common (cdr (cadr not-match)))))
		   `(and (not (member ,@not-common))
			 ,@others))))
	      ;;      (and fixnum (not (member 1 2 a b)))
	      ;;   --> (and (not (member 1 2)) fixnum)
	      ;;      (and fixnum (not (member a b)))
	      ;;   --> (and fixnum)
	      ((and (some #'not-eql-or-member? operands)
		    (cdr operands))
	       (multiple-value-destructuring-bind (((_not (_member &rest old-elements))) others)
		   (partition-by-predicate #'not-eql-or-member? operands)
		 (declare (ignore _not _member))
		 (let ((new-elements (remove-if-not (lambda (e)
						      (every (lambda (o)
							       (typep e o))
							     others))
						    old-elements)))
		   (cond
		     ((cdr new-elements)
		      (substitute-tail type `(not (member ,@old-elements))
				       `(not (member ,@new-elements))))
		     (new-elements
		      (substitute-tail type `(not (member ,@old-elements))
				       `(not (eql ,@new-elements))))
		     (t
		      (cons 'and others))))))
	      ((subtypep (cons 'and operands) nil)		; (and float string) --> nil
	       nil)
	      (t
	       (cons 'and operands))))
	   ((or)					  ; REDUCE OR
	    (setf operands (remove-subs operands)) ; (or float number) --> (or number)
	    (setf operands (reduce-absorption operands))
	    (when (some #'and? operands)
	      (setf operands (reduce-redundancy operands)))

	    ;; consensus theorem
	    ;; AB + A!C + BC = AB + A!C
	    ;; ABU + A!CU + BCU = ABU + A!CU
	    (labels ((find-potential-consensus-tail (and1 and2)
		       (declare (type (cons symbol list) and1 and2))
		       ;; exists? x in and1 where x! is in and2?
		       (let ((t1 (find-if (lambda (t1)
					    (member `(not ,t1) (cdr and2) :test #'equal))
					  (cdr and1))))
			 (when t1
			   (union (remove t1 (cdr and1) :test #'equal)
				  (remove `(not ,t1) (cdr and2) :test #'equal)
				  :test #'equal))))
		     (pair-consensus (t1 t2 &aux (tail (find-potential-consensus-tail t1 t2)))
		       (find-if #'(lambda (t4) (and (and? t4)
						    (set-equalp tail (cdr t4))))
				operands))
		     (find-consensus-term (&aux consensus)
		       (dolist (t1 operands)
			 (when (and? t1)
			   (dolist (t2 operands)
			     (cond
			       ((null (and? t2)))
			       ((equal t1 t2))
			       (t
				(setf consensus (or (pair-consensus t1 t2)
						    (pair-consensus t2 t1)))
				(when consensus
				  (return-from find-consensus-term consensus)))))))))
	      (let (consensus-term)
		(loop :while (setf consensus-term (find-consensus-term))
		      :do (setf operands (remove consensus-term operands :test #'equal)))))
	    (while (some #'or? operands) ; (or A (or U V) (or X Y) B C) --> (or A U V X Y B C)
	      (setf operands (mapcan #'(lambda (operand)
					 (if (or? operand)
					     (copy-list (cdr operand))
					     (list operand)))
				     operands)))
	    (rule-case type
	      ((null operands)		; (or) --> nil
	       nil)
	      ((null (cdr operands))	; (or A) --> A
	       (car operands))
	      ((member t operands)	; (or A t B) --> t
	       t)
	      ((member nil operands)	; (or A nil B) --> (or A B)
	       (cons 'or (remove nil operands)))
	      ((< 1 (count-if #'eql-or-member? operands))
	       ;; (or string (member 1 2 3) (eql 4) (member 2 5 6))
	       ;;  --> (or string (member 1 2 3 4 5 6))
	       (multiple-value-bind (matches other) (partition-by-predicate #'eql-or-member? operands)
		 (cons 'or (cons (cons 'member (mapcan (lambda (match)
							 (copy-list (cdr match))) matches))
				 other))))
	      ((and (some #'eql-or-member? operands)
		    (cdr operands))
	       ;; (or fixnum string (member 1 2 "hello" a b)))
	       ;; --> (or fixnum string (member a b))
	       (multiple-value-destructuring-bind (((_member &rest old-elements)) others)
		   (partition-by-predicate #'eql-or-member? operands)
		 (declare (ignore _member))
		 (let ((new-elements (remove-if (lambda (e)
						  (some (lambda (o)
							  (typep e o))
							others))
						old-elements)))
		   (cond ((cdr new-elements)
			  (substitute-tail type `(member ,@old-elements)
					   `(member ,@new-elements)))
			 (new-elements
			  (substitute-tail type `(member ,@old-elements)
					   `(eql ,@new-elements)))
			 (t
			  `(or ,@others))))))
	      ((and (some #'not-eql-or-member? operands)
		    (cdr operands))
	       ;; (or number (not (member 1 2 a b)))
	       ;; --> (or number (not (member a b)))
	       ;; --> (not (member a b))
	       (let ((new-operands (mapcar (lambda (op)
					     (if (not? op)
						 (cadr op)
						 `(not ,op)))
					   operands)))
		 (reduce-lisp-type-once `(not (and ,@new-operands)))))
	      ((subtypep t (cons 'or operands))	        ; (or number (not number)) --> t
	       t)
	      (t
	       (cons 'or operands))))
	   ((not)
	    (assert (null (cdr operands)) nil "invalid type ~A, not requires exactly one operand" type)
	    (cond ((equal '(nil) operands) ; (not nil) --> t
		   t)
		  ((equal '(t) operands) ; (not t) --> nil
		   nil)
		  ((atom (car operands)) ; (not atom) --> (not atom) 
		   type)
		  ((not? (car operands)) ; (not (not A)) --> A
		   (cadr (car operands)))
		  ((or? (car operands)) ; (not (or A B C)) --> (and (not A) (not B) (not C))
		   (pattern-bind ((_ &rest args)) operands
				 (cons 'and
				       (loop :for operand :in args
					     :collect `(not ,operand)))))
		  ((and? (car operands)) ; (not (and A B C)) --> (or (not A) (not B) (not C))
		   (pattern-bind ((_ &rest args)) operands
				 (cons 'or
				       (loop :for operand :in args
					     :collect `(not ,operand)))))
		  (t
		   type)))))))))


(defun fixed-point (function arg &key (test #'equal))
  "Find the fixed point of a FUNCTION, starting with the given ARG."
  (declare (type (function (t) t) function))
  (let ((result (funcall function arg)))
	
    (loop :while (not (funcall test result arg))
	  :do (progn (setf arg result)
		     (setf result (funcall function arg))))
    result))

(defun reduce-lisp-type (type)
    "Given a common lisp type designator such as (AND A (or (not B) C)), apply some
algebraic manipulations to reduce the expression to a cannonical form.  The general
cannonical form is an OR of ANDs such as (OR A (not B) (AND C (not D))), but may
be even simpler in cases such as (OR A B), or (AND A B).  A few restrictions apply:
1) OR never appears with an AND block
2) neither AND nor OR appear inside a NOT block
3) OR never has fewer than 2 operands
4) AND never has fewer than 2 operands"
  (alphabetize-type
   (fixed-point #'reduce-lisp-type-once
		type :test #'equal)))


(defun perf-reduce ()
  (let ((EOF (list nil))
	data)
  (with-open-file (stream #p"/tmp/jnewton/types/expressions-12.data" :direction :input)
    (while (not (eq EOF (setf data (read stream nil EOF))))
      (reduce-lisp-type data)))))
      


(defun decompose-types (type-specifiers)
  (declare (type list type-specifiers))
  "Given a list TYPE-SPECIFIERS of lisp type names, return a list of disjoint
type-specifiers comprising the same union, with each of the resulting type-specifiers
being a sub-type of one of the given type-specifiers."
  (let (decomposition) ;; the list of disjoint type-specifiers
    (labels ((remove-disjoint ()
	       (dolist (T1 type-specifiers)
		 (when (every (lambda (T2)
				(disjoint-types-p T1 T2)) ;; are T1 and T2 disjoint?
			      (cdr type-specifiers))
		   (let ((new (reduce-lisp-type T1)))
		     (when new ; don't remember the nil type
		       (pushnew new decomposition :test #'equivalent-types-p)))
		   (setf type-specifiers (remove T1 type-specifiers :test #'equal)))))
	     (find-intersecting ()
	       (mapl (lambda (T1-tail &aux (T1 (car T1-tail)) (tail (cdr T1-tail)))
		       (dolist (T2 tail)
			 (unless (disjoint-types-p T1 T2)
			   (return-from find-intersecting (values t T1 T2)))))
		     type-specifiers)
	       nil)
	     (forget (type)
	       (setf type-specifiers (remove type type-specifiers :test #'equal)))
	     (remember (type)
	       (pushnew type type-specifiers :test #'equivalent-types-p)))
      (loop :while type-specifiers
	    :do
	       (progn
		 (remove-disjoint)
		 (multiple-value-bind (foundp T1 T2) (find-intersecting)
		   (when foundp
		     (forget T1)
		     (forget T2)
		     (remember `(and ,T1 ,T2))
		     (remember `(and ,T1 (not ,T2)))
		     (remember `(and (not ,T1) ,T2))))))
      decomposition)))

