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

(in-package :lisp-types)


(defun substitute-type (type substitutions)
  "Walk the given type specifier, making the given substitutions.
TYPE is a common lisp type specifier.
SUBSTITUTIONS is an alist (car/cadr) to used with :test #'equal."
  (let ((match (assoc type substitutions :test #'equal)))
    (cond
      (match
	  (cadr match))
      ((atom type)
       type)
      ((member (car type) '(and or not))
       (cons (car type)
	     (mapcar #'(lambda (t2)
			 (substitute-type t2 substitutions)) (cdr type))))
      (t
       type))))

(defun expand-reduced-typecase (obj clauses &aux reductions)
  "Returns a TYPECASE form given its first argument, OBJ, and list of CLAUSES.
The clauses appear in the same order, but the tests of each may have been
(non-destructively) modified to remove redundant logic.  This function
is the work-horse for REDUCED-TYPECASE."
  (labels ((not? (type)
	     (and (consp type)
		  (eq 'not (car type))))
	   (and? (type)
	     (and (consp type)
		  (eq 'and (car type))))
	   (or? (type)
	     (and (consp type)
		  (eq 'or (car type))))
	   (remember (type value)
	     (pushnew (list type value) reductions :test #'equal)
	     (cond
	       ((not? type)
		(remember (cadr type) (not value)))
	       ((and value
		     (and? type))
		(dolist (term (cdr type))
		  (remember term t)))
	       ((and (not value)
		     (or? type))
		(dolist (term (cdr type))
		  (remember term nil)))))
	   (transform-clauses (clauses)
	     (loop :for clause :in clauses
		   :collect (let* ((t2 (substitute-type (car clause) reductions))
				   (t3 (reduce-lisp-type t2)))
			      (remember (car clause) nil)
			      (remember t2 nil)
			      (remember t3 nil)
			      (cons t3 (cdr clause))))))
    (let ((clauses (transform-clauses clauses)))
      (cond
	((null clauses)
	 obj)
	(t
	 (destructuring-bind ((type &rest expressions) &rest other-clauses) clauses
	   (cond
	     ;;
	     ((eq t type)
	      ;; (typecase obj (t 42))
	      ;; --> (progn obj 42)
	      `(progn ,obj ,@expressions))
	     ((and (eq 'null type)
		   (null other-clauses))
	      ;; (typecase obj (null 42))
	      ;; --> (unless obj 42)
	      `(unless ,obj ,@expressions))
	     (t
	      `(typecase ,obj ,@clauses)))))))))

(defmacro reduced-typecase (obj &rest clauses)
  "Syntactically similar to TYPECASE. Expands to a call to TYPECASE but
with cases reduced if possible.  In particular latter cases assume that previous
cases have failed.  This macro preserves the order of the clauses, but is
likely to change the logic of the test of each clause.
E.g.,
(reduced-typecase obj
   (float 41)
   ((and number (not float)) 42))

Because if clause 2 is reached, it is known that obj is not a FLOAT, so this expands to

(typecase obj
   (float 41)
   (number 42))

There may be cases when a type specifier reduces to nil, in which case the
compiler may issue warnings about removing unreachable code."
  (expand-reduced-typecase obj clauses))

(defun cost (type-specifier)
  (cond
    ((eql t type-specifier)
     1)
    ((null type-specifier)
     1)
    ((symbolp type-specifier)
     (cond ((find-class type-specifier nil)
	    1)
	   ((eql (find-package type-specifier)
		 (find-package :cl))
	    1)
	   (t
	    4)))
    ((listp type-specifier)
     (case (car type-specifier)
       ((or)
	(let* ((t-tail (member t type-specifier))
	       (head (if t-tail
			 (ldiff type-specifier t-tail)
			 type-specifier)))
	  (apply #'+ (mapcar #'cost head))))
       ((and)
	(let* ((nil-tail (member nil type-specifier))
	       (head (if nil-tail
			 (ldiff type-specifier nil-tail)
			 type-specifier)))
	  (apply #'+ (mapcar #'cost head))))
       ((not cons)
	(apply #'+ (mapcar #'cost type-specifier)))
       ((member eql)
	(length (cdr type-specifier)))
       ((satisfies)
	10)
       (t
	4)))
    (t
     (error "invalid type specifier ~A" type-specifier))))

(defun expand-disjoint-typecase (obj clauses &key (reorder nil))
  "Returns a TYPECASE form given its first argument, OBJ, and list of CLAUSES.
The clauses appear in the same order, but the tests of each may have been
(non-destructively) modified so that the caller is free to re-order the clauses."
  (labels ((transform-clauses (clauses)
	     (let (complements)
	       (loop :for clause :in clauses
		     :collect (let* ((t2 (car clause))
				     (t3 (reduce-lisp-type `(and ,t2 ,@complements))))
				(push `(not ,t2) complements)
				(cons t3 (cdr clause))))))
	   (complexity (obj)
	     (cond ((listp obj) ; nil==>0 non-nil list ==> >1
		    (apply #'+ (length obj) (mapcar #'complexity obj)))
		   (t 1)))
	   (cmp (a b)
	     (cond ((equal a b)
		    t)
		   ((null a)		; nil moves to the end
		    nil)
		   ((null b)		; keep nil at end
		    t)
		   ((and (symbolp a)
			 (symbolp b)) ; leave symbols in the same order for stability
		    t)
		   ((symbolp a) ; keep symbols at beginning
		    t)
		   ((symbolp b) ; move symbol to beginning
		    nil)
		   ((and (listp a)
			 (listp b)) ; more complex goes to end
		    (<= (complexity a)
			(complexity b)))
		   (t
		    (error "cannot compare objects ~A vs ~A" (class-of a) (class-of b))))))
		    
    (let ((new-clauses (transform-clauses clauses)))
      `(typecase ,obj ,@(if reorder
			    (stable-sort new-clauses #'cmp :key #'car)
			    new-clauses)))))

(defmacro disjoint-typecase (obj &rest clauses)
  "Syntactically similar to TYPECASE. Expands to a call to TYPECASE but
with cases made exclusive so they can be re-ordered.  The expansions contains
the cases in the given order, but the caller may safely chose to reorder them.

E.g.,
(disjoint-typecase obj
   ((satisfies F) 41)
   ((satisfies G) 42))

exands to

(typecase obj
   ((satisifies F) 41)
   ((and (satisfies G) 
         (not (satisifies F))) 42))

There may be cases when a type specifier reduces to nil, in which case the
compiler may issue warnings about removing unreachable code."
  (expand-disjoint-typecase obj clauses))

(defun expand-optimized-typecase (obj clauses)
  `(reduced-typecase ,@(cdr (expand-disjoint-typecase obj clauses :reorder t))))

(defmacro optimized-typecase (obj &rest clauses)
  "Syntactically similar to TYPECASE. Expands to a call to TYPECASE but
with the types reduced as if by DISJOINT-TYPECASE, then sorting the clauses
by increasing complexity, and finally REDUCED-TYPECASE."
  (expand-optimized-typecase obj clauses))
  
(defun map-permutations (visit data)
  "call the given unary VISITOR function once for each permutation of the given list DATA"
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

(defun lconc (buf items)
  (cond
    ((null buf)
     (cons items (last items)))
    ((null (car buf))
     (setf (car buf) items)
     (setf (cdr buf) (last items))
     buf)
    (t
     (setf (cdr (cdr buf)) items)
     (setf (cdr buf) (last items))
     buf)))

(defmacro auto-permute-typecase (obj &body clauses)
  "Syntactically similar to TYPECASE. Expands to a call to TYPECASE but
with the types reduced and re-ordered to minimize a projected cost
heuristic function."
  (flet ((clauses-cost (clauses)
	   (let ((buf (list nil)))
	     (dotimes (len (length clauses))
	       (lconc buf (subseq clauses 0 len)))
	     (cost `(or ,@(mapcar #'car (car buf)))))))
    (let ((min-metric (clauses-cost clauses))
	  (min-clauses clauses))
      (destructuring-bind (_typecase obj &rest disjoint-clauses) (expand-disjoint-typecase obj clauses)
	(declare (ignore _typecase))
	(map-permutations (lambda (permuted-clauses)
			    (destructuring-bind (_typecase _obj &rest clauses)
				(expand-reduced-typecase obj permuted-clauses)
			      (declare (ignore _typecase _obj))
			      (let ((metric (clauses-cost clauses)))
				(when (< metric min-metric)
				  (setf min-metric metric)
				  (setf min-clauses clauses)))))
			  disjoint-clauses)
	`(typecase ,obj ,@min-clauses)))))


		       
