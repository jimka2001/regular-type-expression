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
	   (notinline member)
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

(defvar *compound-type-specifier-names*
  '(
    (array :type dimension-spec)
    (base-string size)
    (bit-vector size)
    (complex :type)
    (cons :type :type)
    (double-float  lower-limit upper-limit)
    (float lower-limit upper-limit)
    (integer)
    (long-float lower-limit upper-limit)
    (rational)
    (real)
    (short-float lower-limit upper-limit)
    (signed-byte)
    (simple-array :type dimension-spec)
    (simple-base-string )
    (simple-bit-vector ) 
    (simple-string    )  
    (simple-vector   )   
    (single-float   )    
    (string        )     
    (unsigned-byte) 
    (vector  :type size)
    )
  "Each element of this list of sublists, each describin how to parse a CL compound type.
The CAR of each sublist the name of a type.
The CDR of each sublist is a list of symbols which may or may not contain the symbol
:type one or more times.  Any symbol not EQ to :type has no distinguished meaning.
If a type is found in this list that it treated as a declaration of to things:
1) That trailing * characters in a type specifiers can be removed, and if a singleton
   list remains it may be replaced by just the name.
   E.g., (short-float * *), (short-float *) (short-float) and short-float
   are equivalent, and the REDUCE-TYPE function will reduce any of them to short-float.
2) If :type appears in the list, then the corresponding argument in a type specifier
   is treated as a type specifier and is subject to reduction by REDUCE-LISP-TYPE.
   E.g., (vector :type size) ==> that a type specifier such as (vector (and integer number) 3)
   can be reduced to (vector integer 3).
Applications are free to push entries onto this list to notify REDUCE-LISP-TYPE of how
to reduce a type defined by DEFTYPE.")

(defun reduce-lisp-type-once (type &aux it)
  "Given a lisp type designator, make one pass at reducing it, removing redundant information such as
repeated or contradictory type designators."
  (declare (optimize (speed 3) (compilation-speed 0))
	   (inline sub-super reduce-absorption reduce-redundancy remove-supers remove-subs))
  (labels ((build (op zero args)
             (cond ((null args)
                    zero)
                   ((null (cdr args))
                    (car args))
                   (t
                    (cons op args))))
           (make-and (args)
             (build 'and t args))
           (make-or (args)
             (build 'or nil args))
           (make-member (args)
             (cond ((null args)
                    nil)
                   ((null (cdr args))
                    (cons 'eql args))
                   (t
                    (cons 'member args))))
           (substitute-tail (list search replace)
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
	   (member? (obj)
	     (and (consp obj)
		  (eq 'member (car obj))))
	   (function? (obj)
	     (and (consp obj)
		  (eq 'function (car obj))))
	   (values? (obj)
	     (and (consp obj)
		  (eq 'values (car obj))))
	   (reducable-compound? (obj)
	     (declare (notinline assoc))
	     (and (consp obj)
		  (assoc (car obj) *compound-type-specifier-names*)))
	   ;; (cons? (obj)
	   ;;   (and (consp obj)
	   ;; 	  (eq 'cons (car obj))))
	   (eql-or-member? (obj)
	     (and (consp obj)
		  (member (car obj) '(eql member) :test #'eq)))
	   (not-eql-or-member? (obj) ; (not (eql ...)) or (not (member ...))
	     (and (not? obj)
		  (consp (cadr obj))
		  (member (car (cadr obj)) '(eql member) :test #'eq)))
	   (remove-trailing-*s (data &aux (trailing (exists-tail tail data
						      (forall e tail (eq '* e)))))
	     (if trailing
		 (ldiff data trailing)
		 data)))

    (cond
      ((atom type)
       type)
      ((member? type)
       (cond ((cddr type);; (member A B) --> (member A B)
	      type)
	     ((cdr type)  ;; (member A) --> (eql A)
	      (cons 'eql (cdr type)))
	     (t ;; (member) --> nil
	      nil)))
      ((setq it (reducable-compound? type))
       (setf type (remove-trailing-*s type))
       ;; reduce 'type' arguments
       (let ((tail type))
	 (setf type
	       (mapcar #'(lambda (arg param)
			   (pop tail)
			   (cond
			     ((eq '* arg)
			      '*)
			     ((eq :type param)
			      (reduce-lisp-type-once arg))
			     (t
			      arg)))
		       type
		       it))
	 (when tail
	   (setf type (append type tail)))
	 ;; after removing trailing *'s, if the result is a singleton list, return the car of the list, i.e., (car it)
	 (if (cdr type)
	     type
	     (car type))))

      ((function? type)
	 (flet ((reduce-spec (arg-typespec)
		  ;;arg-typespec::= (typespec*  
		  ;;                  [&optional typespec*]  
		  ;;                  [&rest typespec]  
		  ;;                  [&key (keyword typespec)*])
		  (let* ((type-conc (list nil))
			 (required-tail arg-typespec)
			 (optional-tail (member '&optional required-tail))
			 (rest-tail     (member '&rest (or optional-tail
							   required-tail)))
			 (key-tail      (member '&key  (cond
							 (rest-tail
							  (cddr rest-tail))
							 (t
							  (or optional-tail
							      required-tail)))))
			 (rest-subseq (ldiff rest-tail
					     key-tail))
			 (optional-subseq (ldiff optional-tail
						 (or rest-tail
						     key-tail)))
			 (required-subseq (ldiff required-tail
						 (or optional-tail
						     rest-tail
						     key-tail))))
		    (dolist (spec required-subseq)
		      (tconc type-conc (reduce-lisp-type-once spec)))
		    (when optional-subseq
		      (tconc type-conc (pop optional-subseq))
		      (dolist (spec optional-subseq)
			(tconc type-conc (reduce-lisp-type-once spec))))
		    (when rest-subseq
		      (assert (= 2 (length rest-subseq)) ()
			      "Invalid &rest portion of ~A" arg-typespec)
		      (tconc type-conc (car rest-subseq))
		      (tconc type-conc (reduce-lisp-type-once (cadr rest-subseq))))
		    (when key-tail
		      (tconc type-conc (car key-tail))
		      (dolist (keyword-spec (cdr key-tail))
			(declare (notinline length))
			(assert (= 2 (length keyword-spec)) ()
				"Invalid &key ~S portion of ~S" keyword-spec arg-typespec)
			(destructuring-bind (keyword spec) keyword-spec
			  (tconc type-conc (list keyword
						 (reduce-lisp-type-once spec))))))
		    (car type-conc))))

	   (declare (notinline length))
	   (case (length type)
	     ((1)			; (function)
	      'function)
	     ((2)			; (function arg-typespec)
	      (list 'function
		    (reduce-spec (cadr type))))
	     ((3)	      ; (function arg-typespec value-typespec)
	      (list 'function
		    (reduce-spec (cadr type))
		    (reduce-lisp-type-once (caddr type)))))))
      ((values? type)
       ;; value-typespec::= typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys] 
       (let* ((type-conc (list nil))
	      (required-tail (cdr type))
	      (optional-tail (member '&optional required-tail))
	      (rest-tail     (member '&rest (or optional-tail
						required-tail)))
	      (allow-other-keys-tail (member '&allow-other-keys (cond
								  (rest-tail
								   (cddr rest-tail))
								  (t
								   (or optional-tail
								       required-tail)))))
	      (rest-subseq (ldiff rest-tail
				  (cddr rest-tail)))
	      (optional-subseq (ldiff optional-tail
				      (or rest-tail
					  allow-other-keys-tail)))
	      (required-subseq (ldiff required-tail
				      (or optional-tail
					  rest-tail
					  allow-other-keys-tail))))

	 (tconc type-conc (car type)) ; values
	 (dolist (spec required-subseq)
	   (tconc type-conc (reduce-lisp-type-once spec)))
	 (when optional-subseq
	   (tconc type-conc (pop optional-subseq))
	   (dolist (spec optional-subseq)
	     (tconc type-conc (reduce-lisp-type-once spec))))
	 (when rest-subseq
	   (assert (= 2 (length rest-subseq)) ()
		   "Invalid &rest portion of ~A" type)
	   (tconc type-conc (car rest-subseq))
	   (tconc type-conc (reduce-lisp-type-once (cadr rest-subseq))))
	 (when allow-other-keys-tail
	   (tconc type-conc '&allow-other-keys))
	 (car type-conc)))
      ((not (or (or? type)
		(and? type)
		(not? type)))
       type)
      (t ;; (or ...) (and ...) (not ...)
       (setf type (cons (car type)
			(remove-duplicates (cdr type) :test #'equal)))
       (setf type (cons (car type)
			(mapcar #'reduce-lisp-type-once (cdr type))))
       ;; we might now how duplicates, in particular we might have (and nil nil), if
       ;;  so then removing supers would result in (and) which would make t.  That woudl
       ;;  be very bad.  So let's remove duplicates again.
       (setf type (cons (car type)
			(remove-duplicates (cdr type) :test #'equal)))

       (destructuring-bind (operator &rest operands &aux remove-me) type
	 (declare (type (member and or not) operator)
		  (notinline remove-supers remove-subs reduce-absorption reduce-redundancy)
		  (type list operands))
	 (ecase operator
	   ((and)			; REDUCE AND
	    (setf operands (remove-supers operands)) ; (and float number) --> (and float)
	    (while (some #'and? operands)
					; (and (and A B) X Y) --> (and A B X Y)
              ;; we remove duplicates because flattening might have created some
              ;; duplicates
	      (setf operands (remove-duplicates (mapcan #'(lambda (operand)
                                                            (if (and? operand)
                                                                (copy-list (cdr operand))
                                                                (list operand)))
                                                        operands) :test #'equal)))
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
	      ((some #'or? operands)    ; (and A (or x y) B) --> (or (and A B x) (and A B y))
	       (let* ((match (find-if #'or? operands))
		      (and-operands (remove match operands :test #'eq)))
		 (make-or
		       (loop :for or-operand :in (cdr match)
			     :collect (make-and (cons or-operand and-operands))))))
	      ((some #'eql-or-member? operands)
	       ;; (and (member a b 2 3) symbol) --> (member a b)
	       ;; (and (member a 2) symbol) --> (eql a)
	       ;; (and (member a b) fixnum) --> nil
	       (let ((objects (remove-if-not (lambda (e)
					       (declare (notinline typep))
					       (typep e type)) (cdr (find-if #'eql-or-member? operands)))))
		 (make-member objects)))		     
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
							       (declare (notinline typep))
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
		      (make-and others))))))
	      ((subtypep (make-and operands) nil)		; (and float string) --> nil
	       nil)
              ((exists t1 operands
                 (exists t2 operands
                   (and (not (eq t1 t2))
                        (disjoint-types-p t1 t2))))
               nil)
              ;; (AND ARITHMETIC-ERROR (NOT CELL-ERROR))) --> ARITHMETIC-ERROR
              ((exists t1 operands
                 (exists t2 operands
                   (and (setq remove-me t2)
                        (not (eq t1 t2))
                        (smarter-subtypep t1 t2))))
               ;; we have already removed duplicates (EQUAL) so removing supertypes is safe
               (make-and (remove remove-me operands :test #'equal)))
	      (t
	       (make-and operands))))
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
			 (declare (notinline union))
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
	      (setf operands (remove-duplicates (mapcan #'(lambda (operand)
                                                            (if (or? operand)
                                                                (copy-list (cdr operand))
                                                                (list operand)))
                                                        operands)
                                                :test #'equal)))
	    (rule-case type
	      ((null operands)		; (or) --> nil
	       nil)
	      ((null (cdr operands))	; (or A) --> A
	       (car operands))
	      ((member t operands)	; (or A t B) --> t
	       t)
	      ((member nil operands)	; (or A nil B) --> (or A B)
	       (make-or (remove nil operands)))
	      ((< 1 (count-if #'eql-or-member? operands))
	       ;; (or string (member 1 2 3) (eql 4) (member 2 5 6))
	       ;;  --> (or string (member 1 2 3 4 5 6))
	       (multiple-value-bind (matches other) (partition-by-predicate #'eql-or-member? operands)
		 (make-or (cons (make-member (mapcan (lambda (match)
							 (copy-list (cdr match))) matches))
				 other))))
	      ((and (some #'eql-or-member? operands)
		    (cdr operands))
	       ;; (or fixnum string (member 1 2 "hello" a b))
	       ;; --> (or fixnum string (member a b))
	       (multiple-value-destructuring-bind (((_member &rest old-elements)) others)
		   (partition-by-predicate #'eql-or-member? operands)
		 (declare (ignore _member))
		 (let ((new-elements (remove-if (lambda (e)
						  (some (lambda (o)
							  (declare (notinline typep))
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
	      ((subtypep t (make-or operands))	        ; (or number (not number)) --> t
	       t)
              ((exists t1 operands
                 (exists t2 operands
                   (and (setq remove-me t1)
                        (not (eq t1 t2))
                        (smarter-subtypep t1 t2))))
               ;; we have already removed duplicates (EQUAL) so removing subtypes is safe
               (make-or (remove remove-me operands :test #'equal)))
	      (t
	       (make-or operands))))
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
				 (make-and
				       (loop :for operand :in args
					     :collect `(not ,operand)))))
		  ((and? (car operands)) ; (not (and A B C)) --> (or (not A) (not B) (not C))
		   (pattern-bind ((_ &rest args)) operands
				 (make-or
				       (loop :for operand :in args
					     :collect `(not ,operand)))))
		  (t
		   type)))))))))

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

(defun derive-constraints (types)
  (loop for tail on types
        nconc (loop for t2 in (cdr tail)
                 with t1 = (car tail)
                 when (subtypep t1 t2)
                   collect (list :subtype t1 t2)
                 when (subtypep t2 t1)
                   collect (list :subtype t2 t1)
                 when (and (subtypep t1 t2)
                           (subtypep t2 t1))
                   collect (list :equal t1 t2)
                 when (null (nth-value 1 (subtypep t1 t2)))
                   collect (list :unknown-subtype t1 t2)
                 when (null (nth-value 1 (subtypep t2 t1)))
                   collect (list :unknown-subtype t2 t1)
                 when (subtypep `(and ,t1 t2) nil)
                   collect (list :disjoint t1 t2)
                 when (null (nth-value 1 (subtypep `(and ,t1 t2) nil)))
                   collect (list :unknown-disjoint t1 t2))))
