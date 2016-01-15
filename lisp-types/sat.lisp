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

(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defun visit-min-terms (client given-types &aux min-term)
  "Call the given client iteratively on all possible min-terms, except the all-zero term."
  (declare (type (function (cons) t) client)
	   (type (cons (or symbol cons) list) given-types))
  (labels ((not? (obj)
	     (and (consp obj)
		  (eq 'not (car obj))))
	   (t? (obj)
	     (not (not? obj)))
	   (invert! (obj)
	     (if (not? obj)
		 (cadr obj)
		 (list 'not obj)))
	   (next-min-term (&aux (ptr min-term))
	     "destructively modify min-term to be the 'next' minterm"
	     ;; first convert leading non-nots to not
	     (while (and ptr
			 (t? (car ptr)))
	       (setf (car ptr)
		     (invert! (car ptr)))
	       (pop ptr))
	     ;; if at the end, we're finished
	     (unless ptr
	       (return-from visit-min-terms nil))
	     ;; then change the first not to non-not
	     (setf (car ptr)
		   (invert! (car ptr)))))
    (setf min-term
	  (remove-duplicates (mapcar (lambda (type)
				       (while (not? type)
					 (setf type (invert! type)))
				       (invert! type))
				     given-types)
			     :test #'equal))
    ;; skip the first one which is ((not a) (not b) (not c) ...)
    (next-min-term)
    (while t
      (funcall client min-term)
      ;; exits if there are no more min-terms
      (next-min-term))))

(defun generate-constraints (types)
  "Given a list of types, some of which might completely or partially contained
in others, return list of 'constraints' to be used by SAT-DECOMPOSE-TYPES.
Each of these constraints is the positive term, which must be eliminated from
the SAT search.
There are three cases of constraints generated.
  1) If X is a subset of Y:   (X (not Y)) ==> the SAT constraint is (not (and X (not Y)))
  2) If Y is a subset of X:   ((not X) Y) ==> the SAT constraint is (not (and (not X) Y))
  3) If X and Y are disjoint: (X Y)       ==> the SAT constraint is (not (and X Y))"
  (mapcon (lambda (tail &aux (t1 (car tail)) (t2-tn (cdr tail)))
	    (mapcan (lambda (t2)
		      (cond
			((null (and (nth-value 1 (subtypep t1 t2))
				    (nth-value 1 (subtypep t2 t1))))
			 (warn "cannot determine relationship of ~A vs ~A, assuming disjoint" t1 t2)
			 (list (list t1 t2)))
			((subtypep t1 t2)
			 (list `(,t1 (not ,t2))))
			((subtypep t2 t1)
			 (list `((not ,t1) ,t2)))
			((disjoint-types-p t1 t2)
			 (list (list t1 t2)))))
		    t2-tn))
	  types))

(defun sat-decompose-types (types)
  "Return a list of decomposed types"
  (let (disjoint
	(constraints (generate-constraints types)))
    (visit-min-terms (lambda (min-term)
		       (unless (some (lambda (constraint)
				       (every (lambda (term)
						(member term min-term :test #'equal))
					      constraint))
				     constraints)
			 (let ((new-type (cons 'and (copy-list min-term))))
			   (unless (subtypep new-type nil)
			     (push  new-type
				    disjoint)))))
		     types)
     disjoint))
