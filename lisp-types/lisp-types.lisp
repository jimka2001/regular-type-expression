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
   "DISJOINT-IZE"
   "VALID-TYPE-P"
   "REDUCE-LISP-TYPE"
   "REDUCED-TYPECASE"
   "OPTIMIZED-TYPECASE"
   "DISJOINT-TYPES-P"
   "EQUIVALENT-TYPES-P"
   ))

(in-package   :lisp-types)

(defun valid-type-p (type-designator)
  ;; TODO need to make this work for other lisps
  ;;  current only works for sbcl
  (SB-EXT:VALID-TYPE-SPECIFIER-P type-designator))

(defun disjoint-types-p (T1 T2)
  "Two types are considered disjoint, if their interseciton is empty, i.e., is a subtype of nil."
  (subtypep `(and ,T1 ,T2) nil))

(defun types-equivalent-p (T1 T2)
)

(defun equivalent-types-p (T1 T2)
  "Two types are considered equivalent if each is a subtype of the other."
  (and (subtypep T1 T2)
       (subtypep T2 T1))
  ;; TODO -- the following is the correct implementation
  ;; TODO -- enable this code and verify the tests
  ;; (multiple-value-bind (T1<=T2 okT1T2) (subtypep T1 T2)
  ;; (multiple-value-bind (T2<=T1 okT2T2) (subtypep T2 T1)
  ;; (values (and T1<=T2 T2<=T1) (and okT1T2 okT2T2))))
)



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

(defun reduce-lisp-type-once (type)
  "Given a lisp type designator, make one pass at reducing it, removing redundant information such as
repeated or contradictory type designators."
  (labels ((or? (obj)
	     (and (listp obj)
		  (eq 'or (car obj))))
	   (and? (obj)
	     (and (listp obj)
		  (eq 'and (car obj))))
	   (not? (obj)
	     (and (listp obj)
		  (eq 'not (car obj))))
	   (eql? (obj)
	     (and (listp obj)
		  (eq 'eql (car obj))))
	   (member? (obj)
	     (and (listp obj)
		  (eq 'member (car obj))))
	   (cons? (obj)
	     (and (listp obj)
		  (eq 'cons (car obj))))
	   (eql-or-member? (obj)
	     (or (eql? obj)
		 (member? obj)))
	   (sub-super (types)
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
	   (remove-subs (types)
	     (multiple-value-bind (match? sub super) (sub-super types)
	       (declare (ignore super))
	       (if match?
		   (remove-subs (remove sub types :test #'eq)) ; tail call
		   types)))
	   (remove-supers (types)
	     (multiple-value-bind (match? sub super) (sub-super types)
	       (declare (ignore sub))
	       (if match?
		   (remove-supers (remove super types :test #'eq)) ; tail call
		   types))))
    (cond
      ((atom type)
       type)
      ((subtypep type nil)		; (and float string) --> nil
       nil)
      ((subtypep t type)	      ; (or number (not number)) --> t
       t)
      ;; TODO - extend to understand other type specifiers which reference type specifiers such as
      ;;   (function (float float) number)
      ;;   (vector number)
      ;;   (complex float)
      ;;   etc
      ((cons? type) ; (cons (and float number) (or string (not string))) --> (cons float t)
       `(cons ,@(mapcar #'reduce-lisp-type-once (cdr type))))
      ((not (or (or? type)	       ; (number 1 8) --> (number 1 8)
		(and? type)
		(not? type)))
       type)
      (t
       (setf type (cons (car type)
			(mapcar #'reduce-lisp-type-once (cdr type))))
       (setf type (cons (car type)
			(remove-duplicates (cdr type) :test #'equal)))
       (destructuring-bind (operator &rest operands) type
	 (ecase operator
	   ((and)				     ; REDUCE AND
	    (setf operands (remove-supers operands)) ; (and float number) --> (and float)

	    (cond ((null operands)	; (and) --> t
		   t)
		  ((null (cdr operands)) ; (and A) --> A
		   (car operands))
		  ((member nil operands) ; (and A nil B) --> nil
		   nil)
		  ((member t operands)	; (and A t B) --> (and A B)
		   (cons 'and (remove t operands)))
		  ((some #'and? operands) ; (and A (and U V) (and X Y) B C) --> (and A U V X Y B C)
		   (cons `and
			 (mapcan #'(lambda (operand)
				     (if (and? operand)
					 (copy-list (cdr operand))
					 (list operand)))
				 operands)))
		  ((some #'or? operands) ; (and (or A B) X Y) --> (or (and A X Y) (and B X Y))
		   (let* ((match (find-if #'or? operands))
			  (and-operands (remove match operands :test #'eq)))
		     (cons 'or
			   (loop :for or-operand :in (cdr match)
				 :collect (cons 'and (cons or-operand and-operands))))))
		  ((< 1 (count-if #'eql-or-member? operands))
		   (multiple-value-bind (matches other) (partition-by-predicate #'eql-or-member? operands)
		     (let ((common (cdr (car matches))))
		       (declare (notinline intersection))
		       (dolist (match (cdr matches))
			 (setq common (intersection common (cdr match))))
		       (cons 'and (cons (cons 'member common)
					other)))))
		  (t
		   (cons 'and operands))))
	   ((or)				   ; REDUCE AND
	    (setf operands (remove-subs operands)) ; (or float number) --> (or number)
	    ;; (or A (and A B C D) E)
	    ;;   --> (or A E)
	    ;; (or (and A B) (and A B C D) E F)
	    ;;   --> (or E F)
	    (dolist (operand operands)
	      (setf operands (remove-if (lambda (op)
					  (cond ((eq op operand)
						 nil)
						((not (and? op))
						 nil)
						((member operand (cdr op))
						 t)
						((not (and? operand))
						 nil)
						(t
						 (set-subsetp (cdr operand) (cdr op)))))
					operands)))
	    ;; A + A!B = A + B
	    (setf operands (mapcar (lambda (op2)
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
				   operands))

	    ;; consensus theorem
	    ;; AB + A!C + BC = AB + A!C
	    ;; ABU + A!CU + BCU = ABU + A!CU
	    (labels ((find-potential-consensus-tail (and1 and2)
		       ;; exists? x in and1 where x! is in and2?
		       (let ((t1 (find-if (lambda (t1)
					    (member `(not ,t1) (cdr and2) :test #'equal))
					  (cdr and1))))
			 (when t1
			   (union (remove t1 (cdr and1) :test #'equal)
				  (remove `(not ,t1) (cdr and2) :test #'equal)
				  :test #'equal))))
		     (pair-consensus (t1 t2 &aux (tail (find-potential-consensus-tail t1 t2)))
		       (find-if #'(lambda (t4) (set-equalp tail (cdr t4))) operands))
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

	    (cond ((null operands)	; (or) --> nil
		   nil)
		  ((null (cdr operands)) ; (or A) --> A
		   (car operands))
		  ((member t operands)	; (or A t B) --> t
		   t)
		  ((member nil operands) ; (or A nil B) --> (or A B)
		   (cons 'or (remove nil operands)))
		  ((some #'or? operands) ; (or A (or U V) (or X Y) B C) --> (or A U V X Y B C)
		   (cons 'or
			 (mapcan #'(lambda (operand)
				     (if (or? operand)
					 (copy-list (cdr operand))
					 (list operand)))
				 operands)))
		  ((< 1 (count-if #'eql-or-member? operands))
		   (multiple-value-bind (matches other) (partition-by-predicate #'eql-or-member? operands)
		     (cons 'or (cons (cons 'member (mapcan (lambda (match)
							     (copy-list (cdr match))) matches))
				     other))))
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
		   (pattern-bind ((_ A)) operands
				 A))
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
  (fixed-point #'reduce-lisp-type-once
	       type :test #'equal))

(defun disjoint-ize (type-designators)
  (declare (type list type-designators))
  "Given a list TYPE-DESIGNATORS of lisp type names, return a list of disjoint
type-designators comprising the same union, with each of the resulting type-designators
being a sub-type of one of the given type-designators."
  (let (decomposition) ;; the list of disjoint type-designators
    (labels ((remove-disjoint ()
	       (dolist (T1 type-designators)
		 (when (every (lambda (T2)
				(disjoint-types-p T1 T2)) ;; are T1 and T2 disjoint?
			      (cdr type-designators))
		   (let ((new (reduce-lisp-type T1)))
		     (when new ; don't remember the nil type
		       (pushnew new decomposition :test #'equivalent-types-p)))
		   (setf type-designators (remove T1 type-designators :test #'equal)))))
	     (find-intersecting ()
	       (mapl (lambda (T1-tail &aux (T1 (car T1-tail)) (tail (cdr T1-tail)))
		       (dolist (T2 tail)
			 (unless (disjoint-types-p T1 T2)
			   (return-from find-intersecting (values t T1 T2)))))
		     type-designators)
	       nil)
	     (forget (type)
	       (setf type-designators (remove type type-designators :test #'equal)))
	     (remember (type)
	       (pushnew type type-designators :test #'equivalent-types-p)))
      (loop :while type-designators
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
