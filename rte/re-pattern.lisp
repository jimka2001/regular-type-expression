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


(in-package   :rte)

(defun map-subsets (visitor data)
  "call the given VISITOR function once for each subset of the list DATA"
  (dotimes (n (expt 2 (length data)))
    (let ((n n)
	  (data data)
	  subset)
      (loop :while (and data (plusp n))
	    :do (progn (when (oddp n)
			 (push (car data) subset))
		       (pop data)
		       (setf n (truncate n 2))))
      (funcall visitor subset))))

(defun map-permutations (visit data)
  "call the given VISITOR function once for each permutation of the given list DATA"
  (declare (type list data)
	   (type (function (list) t)))
  (let ((N (length data)))
    (labels ((next (so-far k)
               (cond
                 ((eql k N)
                  (funcall visit (mapcar #'car so-far)))
                 (t
                  (incf k)
                  (loop :for items :on data
			:do (unless (member items so-far :test #'eq)
			      (next (cons items so-far) k)))))))
      (next nil 0))))

(defun traverse-pattern (pattern &rest functions
			 &key
			   (client #'(lambda (pattern)
				       (apply #'traverse-pattern pattern functions)))
			   (f-0-* #'(lambda (patterns)
				      (cons :0-* (mapcar client patterns))))
			   (f-1-* #'(lambda (patterns)
				      (apply #'traverse-pattern `(:cat ,@patterns (:0-* ,@patterns)) functions)))
			   (f-0-1 #'(lambda (patterns)
				      (apply #'traverse-pattern `(:or :empty-word (:cat ,@patterns)) functions)))
			   (f-and #'(lambda (patterns)
				      (cons :and (mapcar client patterns))))
			   (f-or  #'(lambda (patterns)
				      (cons :or (mapcar client patterns))))
			   (f-not #'(lambda (patterns)
				      (cons :not (mapcar client patterns))))
			   (f-cat #'(lambda (patterns)
				      (cons :cat (mapcar client patterns))))
			   (f-permutations #'(lambda (patterns)
					       (let (permutations)
						 (map-permutations #'(lambda (permutation)
								       (push (cons :cat permutation) permutations))
								   patterns)
						 (cons :or permutations))))
			   (f-empty-word client)
			   (f-empty-set client)
			   (f-type client))
  "Walk a regular type expression, stopping at nodes which designate lisp types.
The given functions F-0-*, F-1-*, etc are called on each node to continue the traversal.
The default behavior is to walk to list and copy it, however the behavior and return value actuall
depend on the choice of F-... function given."
  (cond ((atom pattern)
	 (case pattern
	   ((:empty-word)
	    (funcall f-empty-word pattern))
	   ((:empty-set)
	    (funcall f-empty-set pattern))
	   (t
	    (funcall f-type pattern))))
	(t				; list
	 (case (car pattern)
	   ((:or)
	    (funcall f-or (cdr pattern)))
	   ((:and)
	    (funcall f-and (cdr pattern)))
	   ((:not)
	    (funcall f-not (cdr pattern)))
	   ((:0-* :0-or-more :*)
	    (funcall f-0-* (cdr pattern)))
	   ((:1-* :1-or-more :+)
	    (funcall f-1-* (cdr pattern)))
	   ((:0-1 :0-or-1 :?)
	    (funcall f-0-1 (cdr pattern)))
	   ((:cat :1)
	    (funcall f-cat (cdr pattern)))
	   ((:permute)
	    (funcall f-permutations (cdr pattern)))
	   ((type)
	    (assert (null (cddr pattern)) nil "Invalid type: ~A" pattern)
	    (funcall f-type (cadr pattern)))
	   (t
	    (unless (valid-type-p pattern)
	      (warn "Invalid type specifier ~A" pattern))
	    (funcall f-type pattern))))))

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

(defun remove-redundant-types (patterns operator)
  "Given the operand list of either :or or :and, return a new operand list understand some basic logical reductions."
  (declare (type (member :and :or) operator))
  (if (and (eql :and operator)
	   (exists p1 patterns
		   (and (valid-type-p p1)
			(exists p2 (cdr patterns)
				(and (valid-type-p p2)
				     ;; (subtype (and T1 T2) nil) means T1 and T2 are mutually exclusive such as string number
				     (subtypep (list 'and p1 p2) nil))))))
      (list :empty-set)
      (remove-if (lambda (p1)
		   (and (valid-type-p p1) ;; can only remove redundant types if they are valid lisp types
			(exists p2 patterns
				(and (not (equal p1 p2))
				     (valid-type-p p2)
				     (case operator
				       ((:or)
					(subtypep p1 p2))
				       (t
					(subtypep p2 p1)))))))
		 patterns)))


(defun partition-by-predicate (predicate data)
  (let (true-elements false-elements)
    (dolist (element data)
      (if (funcall predicate element)
	  (push element true-elements)
	  (push element false-elements)))
    (values true-elements false-elements)))


(defun canonicalize-pattern-once (re)
  "Given a regular-type-expression, return a more canonical form.  This attempts to create an (:or ..) of (:and ...)'s, and
removing or resolving redundant or trivial type designators. CANONICALIZE-PATTERN calls this function multiple times until
a fixed point is found."
  (flet ((like-multipy (operator patterns &key idempotent)
	   (setf patterns (remove :empty-word patterns))
	   (let ((new (remove :empty-word (mapcar #'canonicalize-pattern patterns))))
	     (cond
	       ((member :empty-set patterns)
		:empty-set)
	       ((cdr new) ; at least 2 args
		(cons operator new))
	       (new ; exactly 1 arg
		(if idempotent
		    (car new)
		    (cons operator new)))
	       (t ; empty art list
		:empty-word)))))
  (traverse-pattern re
		    :f-type #'(lambda (pattern)
				(cond ((atom pattern)
				       pattern)
				      ((eql 'member (car pattern)) ; alphabetize the arguments of (member ...)
				       (cons 'member (alphabetize (cdr pattern))))
				      ((eql 'rte (car pattern))
				       (cons 'rte (mapcar #'canonicalize-pattern (cdr pattern))))
				      (t
				       pattern)))
	       :f-empty-set #'identity
	       :f-empty-word #'identity
		    :f-0-* #'(lambda (patterns)
			       (like-multipy :0-* patterns :idempotent nil))
	       :f-cat #'(lambda (patterns)
			  ;; (:cat A B (:cat C D) E F) --> (:cat A B C D E F)
			  (setf patterns
				(mapcan (lambda (term)
					  (cond ((and (listp term)
						      (eql :cat (car term)))
						 (copy-list (cdr term)))
						(t
						 (list term))))
					patterns))
			  (like-multipy :cat patterns :idempotent t))
	       :f-not #'(lambda (patterns &aux (pattern (car patterns)))
			  (typecase pattern
			    ((cons (eql :not))
			     (canonicalize-pattern (cadr pattern)))
			    ;;   (:not (:and A B)) --> (:or (:not A) (:not B))
			    ((cons (eql :and))
			     (cons :or (mapcar #'(lambda (p)
						   (canonicalize-pattern (list :not p))) (cdr pattern))))
			    ;;   (:not (:or A B)) --> (:and (:not A) (:not B))
			    ((cons (eql :or))
			     (cons :and (mapcar #'(lambda (p)
						    (canonicalize-pattern (list :not p))) (cdr pattern))))
			    ((cons (eql :0-*) (cons (eql t) null)) ;; (:not (:0-* t)) --> :empty-set
			     :empty-set)
			    ((eql :empty-word) ;; (:not :empty-word) --> (:+ t)
			      '(:+ t))
			    ((eql :empty-set) ;; (:not :empty-set) --> (:* t)
			     '(:* t))
			    (t
			     (cons :not (mapcar #'canonicalize-pattern patterns)))))
	       :f-or #'(lambda (patterns)
			 (let ((sub-or (setof s patterns
					      (and (listp s)
						   (eql :or (car s))))))
			   ;; (:or (:or A B ) C D) --> (:or A B C D)
			   (dolist (s sub-or)
			     (dolist (p (cdr s))
			       (push p patterns)))
			   (setf patterns (set-difference patterns sub-or :test #'equal)))
			 ;; (:or (member 1 2 3) (member 10 20 30))
			 ;;  --> (:or (member 1 2 3 10 20 30))   ;; in some order, unspecified
			 (when (< 1 (count-if #'(lambda (obj)
						  (and (listp obj)
						       (member (car obj) '(eql member)))) patterns))
			   (multiple-value-bind (matches other) (partition-by-predicate #'(lambda (obj)
											    (and (listp obj)
												 (member (car obj) '(eql member))))
											patterns)
			     (setq patterns (cons (cons 'member (mapcan (lambda (match)
									  (copy-list (cdr match))) matches))
						  other))))
			 (setf patterns (uniquify patterns)
			       patterns (remove :empty-set patterns)
			       patterns (mapcar #'canonicalize-pattern patterns)
			       patterns (remove :empty-set patterns)
			       patterns (uniquify patterns)
			       patterns (remove-redundant-types patterns :or))
			 (cond
			   ((cdr patterns)
			    ;; TODO, should not alphabetize patterns because it will not work in the case
			    ;; the types have side effect or if they are order dependents such as
			    ;; (:or (not list) (rte ...))
			    ;; this will break some tests, which will need to be fixed. and it will be harder
			    ;; to make assertions about complicated types.
			    (cons :or (alphabetize patterns)))
			   (patterns
			    (car patterns))
			   (t
			    :empty-set)))
		    :f-and  #'(lambda (patterns)
				(let ((sub-and (setof s patterns
						      (and (listp s)
							   (eql :and (car s))))))
				  ;; (:and (:and A B ) C D) --> (:and A B C C)
				  (dolist (s sub-and)
				    (dolist (p (cdr s))
				      (push p patterns)))
				  (setf patterns (set-difference patterns sub-and :test #'equal)))
				
				;; (:and A B (:0-* t))
				;;  --> (:and A B)
				(setf patterns (remove '(:0-* t) patterns :test #'equal))
				
				;; (:and (member 1 2 3) (member 2 3 4) ...)
				;;  --> (:and (member 2 3) ...)   ;; in some order, unspecified
				(when (< 1 (count-if #'(lambda (obj)
							 (and (listp obj)
							      (member (car obj) '(eql member)))) patterns))
				  (multiple-value-bind (matches other) (partition-by-predicate #'(lambda (obj)
												   (and (listp obj)
													(member (car obj) '(eql member))))
											       patterns)
				    (declare (notinline intersection))
				    (let ((common (cdr (car matches))))
				      (dolist (match (cdr matches))
					(setf common (intersection common (cdr match))))
				      (setq patterns (cons (cons 'member common)
							   other)))))
				;; (:and (:or A B) C D) --> (:or (:and A C D) (:and B C D))
				(let ((sub-or (find-if (lambda (s)
							 (and (listp s)
							      (eql :or (car s))))
						       patterns)))
				  (setf patterns (remove sub-or patterns :test #'equal))
				  
				  (cond
				    (sub-or
				     (canonicalize-pattern (cons :or (loop :for p :in (cdr sub-or)
									   :collect `(:and ,p ,@patterns)))))
				    ((member :empty-set patterns)
				     :empty-set)
				    (t
				     (setf patterns (uniquify patterns)
					   patterns (mapcar #'canonicalize-pattern patterns)
					   patterns (remove '(:0-* t) patterns :test #'equal)
					   patterns (uniquify patterns)
					   patterns (remove-redundant-types patterns :and))
				     (cond
				       ((member :empty-set patterns)
					:empty-set)
				       ((cdr patterns)
					;; TODO, should not alphabetize patterns because it will not work in the case
					;; the types have side effect or if they are order dependents such as
					;; (:or (not list) (rte ...))
					;; this will break some tests, which will need to be fixed. and it will be harder
					;; to make assertions about complicated types.
					(cons :and (alphabetize patterns)))
				       (patterns
					(car patterns))
				       (t
					'(:0-* t))))))))))

(defun fixed-point (function arg &key (test #'equal))
  "Find the fixed point of a FUNCTION, starting with the given ARG."
  (declare (type (function (t) t) function))
  (let ((result (funcall function arg)))
	
    (loop :while (not (funcall test result arg))
	  :do (progn (setf arg result)
		     (setf result (funcall function arg))))
    result))

(defun canonicalize-pattern (re)
  "Given a regular-type-expression, return a canonical form."
  (fixed-point #'canonicalize-pattern-once re :test #'equal))

(defun nullable (re)
  (traverse-pattern re
	       :f-empty-set (constantly nil)
	       :f-empty-word (constantly t)
	       :f-type (constantly nil)
	       :f-0-* (constantly t)
	       :f-cat #'(lambda (patterns)
			  (every #'nullable patterns))
	       :f-not #'(lambda (patterns)
			  (assert (null (cdr patterns)) () "Invalid :not rte, ~S" (cons :not patterns))
			  (not (nullable (car patterns))))
	       :f-or #'(lambda (patterns)
			 (some #'nullable patterns))
	       :f-and #'(lambda (patterns)
			  (every #'nullable patterns))))

(defun uniquify (objects)
  (cond
    ((null (cdr objects))
     objects)
    ((member (car objects) (cdr objects) :test #'equal)
     (uniquify (cdr objects)))
    (t
     (cons (car objects)
	   (uniquify (cdr objects))))))

(defun derivative (pattern wrt-type)
  (flet ((walk (patterns)
	   (mapcar (lambda (p)
		     (derivative (canonicalize-pattern p) wrt-type))
		   patterns)))
    (canonicalize-pattern
     (traverse-pattern pattern
		  :f-empty-word (constantly :empty-set)
		  :f-empty-set  (constantly :empty-set)
		  :f-type  #'(lambda (single-type-pattern)
			       (cond
				 ((equal wrt-type single-type-pattern)
				  ;; the check for equivalence is not strictly necessary because if T1 and T2 are equivalent types
				  ;; then they are NOT mutually exclusive, thus the 3rd clause of this cond would be taken.
				  ;; Nevertheless, equivalence check is probably common, and fast.
				  :empty-word)
				 ((smarter-subtypep wrt-type single-type-pattern)
				  :empty-word)
				 ((disjoint-types-p wrt-type single-type-pattern)
				  ;; are the types mutually exclusive, e.g., string vs number
				  ;; (warn "~A and ~A are mutually exclusive~%" wrt-type single-type-pattern)
				  :empty-set)
				 ((null (nth-value 1 (smarter-subtypep wrt-type single-type-pattern)))
				  (warn 'ambiguous-subtype :sub wrt-type :super single-type-pattern
							   :consequence "assuming :empty-word")
				  :empty-word)
				 ((null (nth-value 1 (smarter-subtypep single-type-pattern wrt-type)))
				  (warn 'ambiguous-subtype :sub single-type-pattern :super wrt-type
							   :consequence "assuming :empty-word")
				  :empty-word)
				 ((smarter-subtypep single-type-pattern wrt-type)
				  (warn "cannot calculate the derivative of ~S~%    w.r.t. ~S beause ~S is a subtype of ~S--assuming :empty-word"
					single-type-pattern wrt-type single-type-pattern wrt-type)
				  :empty-word)
				 (t
				  (warn "cannot calculate the derivative of ~S~%    w.r.t. ~S--assuming :empty-word"
					single-type-pattern wrt-type)
				  :empty-word)))
		  :f-or    #'(lambda (patterns)
			       (cons :or (walk patterns)))
		  :f-and   #'(lambda (patterns)
			       (cons :and (walk patterns)))
		  :f-not   #'(lambda (patterns)
			       (cons :not (walk patterns)))
		  :f-cat #'(lambda (patterns)
			     (flet ((term1 ()
				      `(:cat
					,(derivative (car patterns) wrt-type)
					,@(cdr patterns)))
				    (term2 ()
				      (derivative `(:cat ,@(cdr patterns)) wrt-type)))
			       (cond
				 ((null (cdr patterns))
				  ;; if :cat has single argument, (derivative (:cat X) Y) --> (derivate X Y)
				  (derivative (car patterns) wrt-type))
				 ((nullable (car patterns))
				  `(:or ,(term1) ,(term2)))
				 (t
				  (term1)))))
		  :f-0-* #'(lambda (patterns)
			     (let ((deriv (derivative `(:cat ,@patterns) wrt-type)))
			       `(:cat ,deriv (:0-* ,@patterns))))))))

(defun first-types (pattern)
  (traverse-pattern pattern
	       :f-empty-word (constantly nil)
	       :f-empty-set  (constantly nil)
	       :f-type #'list
	       :f-or   #'(lambda (patterns)
			   (mapcan #'first-types patterns))
	       :f-and #'(lambda (patterns)
			  (mapcan #'first-types patterns))
	       :f-not #'(lambda (patterns)
			  (mapcan #'first-types patterns))
	       :f-cat #'(lambda (patterns)
			  (cond ((null (cdr patterns))
				 (first-types (car patterns)))
				((nullable (car patterns))
				 (append (first-types (car patterns))
					 (first-types (cons ':cat (cdr patterns)))))
				(t
				 (first-types (car patterns)))))
	       :f-0-* #'(lambda (patterns)
			  (first-types (cons ':cat patterns)))))

(defclass rte-state-machine (ndfa:state-machine)
  ((ndfa::test :initform #'typep)
   (deterministicp :initform t)))

(defmethod print-object ((rte rte-state-machine) stream)
  (print-unreadable-object (rte stream :type t :identity nil)
    (dolist (state (get-initial-states rte))
      (format stream "~A" state))))


(defgeneric dump-code (object))

(defmethod dump-code ((pattern list))
  (dump-code (make-state-machine pattern)))

(defmethod dump-code ((ndfa rte-state-machine))
  (let* ((states (append (ndfa:get-initial-states ndfa)
			 (set-difference (ndfa:states ndfa)
					 (ndfa:get-initial-states ndfa) :test #'eq)))
	 (state-assoc (let ((n 0))
			(mapcar (lambda (state)
				  (list state (incf n)))
				states)))
	 (list-end `(null seq))
	 (list-next `(pop seq))
	 
	 (simple-vector-end `(>= i len))
	 (simple-vector-next `(prog1 (svref seq i)
				(incf i)))

	 (vector-end `(>= i len))
	 (vector-next `(prog1 (aref seq i)
			 (incf i)))

	 (sequence-end `(or (sequence:emptyp seq)
			    (>= i len)))
	 (sequence-next `(prog1 (sequence:elt seq i)
			   (incf i))))
	 
    (labels ((state-name (state)
	       (cadr (assoc state state-assoc :test #'eq)))
	     (dump-typecase-transition (transition)
	       `(,(transition-label transition)
		 (go ,(state-name (ndfa:next-state transition)))))
	     (dump-case-transition (transition)
	       `(,(cdr (transition-label transition))
		 (go ,(state-name (ndfa:next-state transition)))))
	     (dump-end (state end)
	       (cond ((null (state-final-p state))
		      `(when ,end
			 (return-from check nil)))
		     ((state-sticky-p state)
		      `(return-from check t))
		     (t
		      `(when ,end
			 (return-from check t)))))
	     (dump-case (state next)
	       (cond
		 ((every #'(lambda (trans)
			     (and (listp (transition-label trans))
				  (member (car (transition-label trans)) '(eql member))))
			 (transitions state))
		  `(case ,next
		     ,@(mapcar #'dump-case-transition (transitions state))
		     (t (return-from check nil))))
		 (t
		  `(optimized-typecase ,next
				       ,@(mapcar #'dump-typecase-transition (transitions state))
				       (t (return-from check nil))))))
	     (dump-state (state end next)
	       (copy-list `(,(state-name state)
			    ,(dump-end state end)
			    ,(dump-case state next))))
	     (dump-tagbody (end final-next)
	       `(tagbody 
		   (go ,(state-name (car (get-initial-states ndfa))))
		   ,@(mapcan #'(lambda (state) (dump-state state end final-next)) states))))

      `(lambda (seq)
	 ;; Don't declare seq a sequence! because if this function gets called with
	 ;; a non-sequence, we want to simply return nil, rather than signaling
	 ;; an error.
	 (declare (optimize (speed 3) (debug 0) (safety 0))
		  ;; (optimize (speed 0) (debug 3) (safety 3))
		  )
	 (block check
	   (typecase seq
	     (list
	      ,(dump-tagbody list-end list-next))
	     (simple-vector
	      (let ((i 0)
		    (len (length seq)))
		(declare (type fixnum i len) (ignorable len))
		,(dump-tagbody simple-vector-end simple-vector-next)))
	     (vector
	      (let ((i 0)
		    (len (length seq)))
		(declare (type fixnum i len) (ignorable len))
		,(dump-tagbody vector-end vector-next)))
	     (sequence		 ; case to handle extensible sequences
	      (let ((i 0)
		    (len (sequence:length seq))) ; sequence (such as infinite sequence) might not support length
		(declare (type fixnum i len) (ignorable len))
		,(dump-tagbody sequence-end sequence-next)))
	     (t
	      nil)))))))

(defmethod ndfa:perform-some-transitions ((ndfa rte-state-machine) starting-states input-sequence)
  (declare (type list starting-states)
	   (type sequence input-sequence))
  (let ((deterministicp (ndfa:deterministicp ndfa))
	(current-states starting-states)
	(sticky-final-states (intersection (ndfa:get-sticky-states ndfa) (ndfa:get-final-states ndfa) :test #'eq)))
    (every (if sticky-final-states
	       (lambda (input)
		 (setf current-states
		       (block do-states
			 (mapcan (lambda (state)
				   (mapcan (lambda (transition)
					     (when (typep input (transition-label transition))
					       (let ((next-state (ndfa:next-state transition)))
						 (cond
						   ((member next-state sticky-final-states :test #'eq)
						    (return-from ndfa:perform-some-transitions (list next-state)))
						   (deterministicp
						    (return-from do-states (list next-state)))
						   (t
						    (list next-state))))))
					   (transitions state)))
				 current-states))))
    	       (lambda (input)
		 (setf current-states
		       (block do-states
			 (mapcan (lambda (state)
				   (mapcan (lambda (transition)
					     (when (typep input (transition-label transition))
					       (cond
						 (deterministicp
						  (return-from do-states (list (ndfa:next-state transition))))
						 (t
						  (list (ndfa:next-state transition))))))
					   (transitions state)))
				 current-states)))))
	   input-sequence)
    current-states))

(defun make-state-machine (pattern)
  "Create and return a finite state machine (ndfa) which can be used to determine if a given list
consists of values whose types match PATTERN."

  ;; TODO need to sort the transitions of each state such that transitions labeled with an atomic
  ;;   type come before transistions with parameterized types.  I.e., list comes before (rte ...)
  ;;   I.e., we want to avoid testing (rte...) type if the object is not a list, in the case that
  ;;   that is required.

  (let ((sm (make-instance 'rte-state-machine))
	done ; list of patterns for which a state in the state machine has already been created
	pending ; list of paterns (derivatives) pending to examine, some are in the done list, some not.
	(pattern (canonicalize-pattern pattern)))
    (flet ((create-state (re &key initial-p)
	     (cond ((member re done :test #'equal)
		    nil)
		   ((eql :empty-set re)
		    nil)
		   (t
		    (push re done)
		    (let (transitions)
		      (dolist (type (decompose-types (uniquify (first-types re))))
			(let ((deriv (derivative re type)))
			  (case deriv
			    ((:empty-set)
			     nil)
			    (t
			     (pushnew deriv pending :test #'equal)
			     (push (list :next-label deriv
					 :transition-label type)
				   transitions)))))
		      (ndfa:add-state sm
				      :label re
				      :initial-p initial-p
				      :final-p (nullable re)
				      :transitions transitions)))))
	   (calc-sticky ()
	     ;; if a state only has transitions which are t (or some supertype of t such as (or number (not number))
	     ;; then mark it as not escapable.
	     (dolist (state (states sm))
	       (setf (ndfa:state-sticky-p state)
		     ;; a state is sticky, or non-escapable, if evert
		     ;; transition has a type=t transition to the
		     ;; state itself.  NOTE that this test is somewhat
		     ;; dangerous because it is being run using
		     ;; transition-label and state-label, i.e., before
		     ;; next (the next state of the transition) has
		     ;; been lazily calculated.
		     (and (transitions state)
			  (every (lambda (transition)
				   (and (subtypep t (transition-label transition))
					(equal (state-label state) (next-label transition))))
				 (transitions state))))))
	   (sort-transitions ()
	     (dolist (state (ndfa:states sm))
	       (setf (ndfa:transitions state)
		     (sort (ndfa:transitions state)
			   #'cmp-objects :key #'ndfa:transition-label))))
	   (parallel-transitions (&aux (hash (make-hash-table :test #'equal)))
	     ;; if two (or more) transitions from A lead to B, we can
	     ;; replace with one transition using lisp type (or label-AB1 label-AB2 ...)
	     (dolist (s1 (ndfa:states sm))
	       (dolist (tr1 (ndfa:transitions s1))
		 (push tr1 (gethash (list s1 (ndfa:next-label tr1)) hash nil))))
	     (maphash (lambda (key transitions &aux (s1 (car key)) (tr1 (car transitions)))
			(when (cdr transitions)
			  (let ((transition-labels (mapcar #'ndfa:transition-label transitions)))
			    (setf (ndfa:transition-label tr1) (reduce-lisp-type `(or ,@transition-labels)))
			    (setf (ndfa:transitions s1)
				  (set-difference (ndfa:transitions s1)
						  (cdr transitions))))))
		      hash)))
      
      (create-state pattern :initial-p t)
      (loop :while pending
	    :do (create-state (pop pending)))
      (parallel-transitions)
      (sort-transitions)
      (calc-sticky))
    sm))
      
(defun remember-state-machine (sm pattern)
  (setf (gethash pattern *state-machines*) sm)
  (register-dependents sm)
  sm)

(defun find-state-machine (pattern)
  (gethash pattern *state-machines*))

(defgeneric match-sequence (input-sequence pattern))

(defmethod match-sequence (input-sequence pattern)
  (declare (ignore input-sequence pattern))
  nil)

(defmethod match-sequence (input-sequence (sm ndfa:state-machine))
  (some #'ndfa:state-final-p (ndfa:perform-transitions sm input-sequence)))

(defmethod match-sequence (input-sequence (pattern list))
  (match-sequence input-sequence (or (find-state-machine pattern)
				     (remember-state-machine (make-state-machine pattern) pattern))))


(defun make-rte-function-name (pattern)
  (when (and (consp pattern)
	     (symbolp (car pattern))
	     (eql 'rte (car pattern))) 
    (error "cannot make pattern function of pattern string with ~S: ~S" (car pattern) pattern))
  (intern (with-output-to-string (str)
	    (write " " :stream str)
	    (write pattern
		   :stream str
		   :pretty nil
		   :escape t))
	  (symbol-package 'rte)))

(defun define-rte (pattern)
  (setf (gethash pattern *rte-types*)
	(let ((dfa (make-state-machine pattern))
	      (function-name (make-rte-function-name pattern)))
	  (register-dependents dfa)
	  (remember-state-machine dfa pattern)
	  (setf (symbol-function function-name) (eval (dump-code dfa)))
	  `(and sequence (satisfies ,function-name)))))

(deftype rte (pattern)
  "Matches a list whose types constitute 'words' in a rational
language described by the given rational expression. The PATTERN must
either be a valid lisp type specifier or a list whose (car PATTERN) is
a keyword of type RTE-KEYWORD, and each element of (cdr PATTERN) is
a valid regular type expression.

:0-* -- matches the types of zero or more successive list elements, E.g,
:1-* -- matches the types of one or more successive list elements
:0-1 -- matches the type optionally (zero or one) list element
:or  -- specifies a logical disjunction of rational expressions
:and -- specifies a logical conjunction of rational expressions
:cat -- in order concatenation of rational expressions
:permute -- specifies any-order concatenation of rational expressions

        (typep '(nil x 1 y 2 z 3 (x) nil (y) nil)
                '(rte (:0-1 null)
                      (:1-* symbol number)
                      (:0-* list null)))

        (typep '(nil x 1 11 y 2 22 z 3 33 (x) nil (y) nil)
                '(rte (:0-1 null)
                      (:or (:1-* symbol number)
                           (:1-* symbol number number))
                      (:0-* list null)))
"
  (or (gethash pattern *rte-types* nil)
      (define-rte pattern)))

(defmacro defrte (pattern)
  (let* ((dfa (make-state-machine pattern))
	 (name (make-rte-function-name pattern))
	 (code (dump-code dfa)))
    `(unless (and (fboundp ',name )
		  (symbol-function ',name))
       (setf (getf (symbol-plist ',name) :rte-pattern) ',pattern)
       (defun ,name ,@(cdr code)))))

(defun rte-reset ()
  (maphash (lambda (pattern type)
	     (declare (ignore type))
	     (let ((name (make-rte-function-name pattern)))
	       (setf (symbol-function name)
		     (lambda (&rest args)
		       (warn "function ~A no longer defined, the cache has been reset! redefining function" name)
		       (define-rte pattern)
		       (apply name args)))))
	   *rte-types*)
  (maphash (lambda (rte-pattern lisp-type)
	     (warn "Removing ~A/~A from *rte-types* hash table" rte-pattern lisp-type))
	   *rte-types*)
  (maphash (lambda (parameterized-type-specifier function-name)
	     (warn "Removing ~A/~A from *type-functions* hash table" parameterized-type-specifier function-name))
	   rte::*type-functions*)
  (setf *rte-types* (make-hash-table :test #'equal))
  (setf *type-functions* (make-hash-table)))
