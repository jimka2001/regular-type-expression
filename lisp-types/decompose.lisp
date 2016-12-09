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

(defun decompose-types (type-specifiers)
  (declare (type list type-specifiers))
  "Given a list TYPE-SPECIFIERS of lisp type names, return a list of disjoint, 
non-nil type-specifiers comprising the same union, with each of the resulting
type-specifiers being a sub-type of one of the given type-specifiers."
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
