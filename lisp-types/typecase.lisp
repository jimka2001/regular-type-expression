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
cases have failed.
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

(defun expand-disjoint-typecase (obj clauses &key (reorder nil))
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
with cases made exclusive so they can be re-ordered.
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
  
