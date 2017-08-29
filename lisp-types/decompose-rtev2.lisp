;; Copyright (c) 2017 EPITA Research and Development Laboratory
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

(defun %decompose-types-rtev2 (type-specifiers)
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0))
           (notinline union))
  (let ((type-specifiers (mapcar #'reduce-lisp-type-simple type-specifiers))
        (known-intersecting (make-hash-table :test #'equal)) decomposition) ;; the list of disjoint type-specifiers
    (labels ((disjoint? (T1 T2 &aux (key (list T1 T2)))
               (multiple-value-bind (hit found?) (gethash key known-intersecting)
                 (cond
                   (found? hit)
                   (t
                    (setf (gethash (reverse key) known-intersecting)
                          (setf (gethash key known-intersecting) (disjoint-types-p T1 T2)))))))
	     (forget (type)
	       (setf type-specifiers (remove type type-specifiers :test #'eq)))
	     (remember (type)
	       (pushnew type type-specifiers :test #'equivalent-types-p)))
      (while type-specifiers
        (let* ((A (car type-specifiers))
               (intersecting (setof B (cdr type-specifiers)
                               (not (disjoint? A B)))))
          (forget A)
          (cond
            ((null intersecting)
             (unless (subtypep A nil)
               (pushnew A decomposition :test #'equivalent-types-p)))
            (t
             (dolist (B intersecting)
               (forget B)
               (remember (reduce-lisp-type-simple `(and ,A ,B)))
               (remember (reduce-lisp-type-simple `(and ,A (not ,B))))
               (remember (reduce-lisp-type-simple `(and (not ,A) ,B))))))))
      (mapcar 'reduce-lisp-type-full decomposition))))


(defun decompose-types-rtev2 (type-specifiers)
  (declare (type list type-specifiers))
  "Given a list TYPE-SPECIFIERS of lisp type names, return a list of disjoint, 
non-nil type-specifiers comprising the same union, with each of the resulting
type-specifiers being a sub-type of one of the given type-specifiers."
  (with-disjoint-hash
      (lambda ()
        (with-subtype-hash
            (lambda ()
              (%decompose-types-rtev2 type-specifiers))))))
