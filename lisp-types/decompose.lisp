;; Copyright (c) 2016,2017 EPITA Research and Development Laboratory
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

(defun slow-decompose-types (type-specifiers)
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0))
           (notinline union))
  ;;  (declare (optimize (debug 3))  (notinline union))
  (let ((known-intersecting (make-hash-table :test #'equal)) decomposition) ;; the list of disjoint type-specifiers
    (labels ((disjoint? (T1 T2 &aux (key (list T1 T2)))
               (multiple-value-bind (hit found?) (gethash key known-intersecting)
                 (cond
                   (found? hit)
                   (t
                    (setf (gethash key known-intersecting) (disjoint-types-p T1 T2))))))
             (remove-disjoint (&aux (disjoint (setof T1 type-specifiers
                                                (forall T2 type-specifiers
                                                  (or (eq T2 T1)
                                                      (disjoint? T1 T2))))))
               (setf type-specifiers (set-difference type-specifiers disjoint :test #'eq))
               (setf decomposition (union decomposition
                                          (remove nil ; don't remember the nil type
                                                  (mapcar #'reduce-lisp-type disjoint))
                                          :test #'equivalent-types-p)))
	     (find-intersecting ()
	       (mapl (lambda (T1-tail &aux (T1 (car T1-tail)) (tail (cdr T1-tail)))
		       (dolist (T2 tail)
			 (unless (disjoint? T1 T2)
			   (return-from find-intersecting (values t T1 T2)))))
		     type-specifiers)
	       nil)
	     (forget (type)
	       (setf type-specifiers (remove type type-specifiers :test #'eq)))
	     (remember (type)
	       (pushnew type type-specifiers :test #'equivalent-types-p)))
      (while type-specifiers
        (remove-disjoint)
        (multiple-value-bind (foundp T1 T2) (find-intersecting)
          (when foundp
            (forget T1)
            (forget T2)
            (remember `(and ,T1 ,T2))
            (remember `(and ,T1 (not ,T2)))
            (remember `(and (not ,T1) ,T2)))))
      decomposition)))

;; (compile 'slow-decompose-types)
;; (trace ((labels slow-decompose-types find-intersecting)))
;; (trace ((labels slow-decompose-types forget)))
;; (trace ((labels slow-decompose-types remember)))
;; (trace ((labels slow-decompose-types remove-disjoint)))
;; (load "/Users/jnewton/sw/regular-type-expression/lisp-types/decompose.lisp")
;; (load "/Users/jnewton/sw/regular-type-expression/lisp-types/lisp-types.lisp")


(defun decompose-types (type-specifiers)
  (declare (type list type-specifiers))
  "Given a list TYPE-SPECIFIERS of lisp type names, return a list of disjoint, 
non-nil type-specifiers comprising the same union, with each of the resulting
type-specifiers being a sub-type of one of the given type-specifiers."
  (call-with-equiv-hash
   (lambda ()
     (call-with-disjoint-hash
      (lambda ()
        (call-with-subtype-hash
         (lambda ()
           (call-with-subtypep-cache
            (lambda ()
              (slow-decompose-types type-specifiers))))))))))

