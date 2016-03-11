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

(defun complexity (object)
  "Returns the number of cons cells in the given hierarchical list"
  (cond ((atom object)
	 0)
	(t
	 (+ (length object)
	    (reduce #'+ (mapcar #'complexity object) :initial-value 0)))))

(defun get-patterns ()
  (let (patterns)
    (maphash (lambda (pattern dfa)
	       (declare (ignore dfa))
	       (push pattern patterns))
	     *state-machines*)
    (sort-patterns patterns)))

(defun sort-patterns (patterns)
  "Returns a sorted list of regular type expression patterns.  The sorted order ensures
that if pattern-B references pattern-A, then pattern-A appears earlier in the list than
pattern-B.  This sorting is done naively: If pattern-B references pattern-A, then
the total number of cons cells of pattern-B must be larger than that of pattern-A, thus
the list is sorted by complexity, otherwise the sort order is non-deterministic."
  (declare (type list patterns)
	   (notinline <))
  (sort patterns #'< :key #'complexity))

(defun serialize-functions (stream &optional (patterns (get-patterns)))
  (dolist (pattern patterns)
    (let* ((dfa (gethash pattern *state-machines*))
	   (name (make-rte-function-name pattern))
	   (code (dump-code dfa)))
      (format stream ";; ")
      (write pattern
	     :stream stream
	     :escape t
	     :pretty nil)
      (terpri stream)
      (format stream ";; complexity = ~D~%" (complexity pattern))
      (write `(unless (fboundp ',name) (defun ,name ,@(cdr code))) ; cdr discards lambda
	     :stream stream
	     :escape t
	     :case :downcase
	     :pretty t)
      (terpri stream)
      (terpri stream))))

(defun compile-and-serialize (pathname compile)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (let ((patterns (get-patterns)))
      (prog1 (funcall compile)
	(serialize-functions stream (remove-if-not (get-patterns) patterns))))))
