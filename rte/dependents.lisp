; Copyright (c) 2016 EPITA Research and Development Laboratory
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

(defclass dependent ()
  ((ndfa :reader ndfa :initarg :ndfa)))

(defmethod print-object ((dep dependent) stream)
  (print-unreadable-object (dep stream :identity nil :type t)
    (print-object (ndfa dep) stream)))

(defun register-dependents (sm)
  (declare (type ndfa:state-machine sm))
  (labels ((register-class-names (pattern dependent)
	     (traverse-pattern pattern
			       :f-empty-word (constantly nil)
			       :f-empty-set (constantly nil)
			       :f-type #'(lambda (type-name)
					   (when (and (symbolp type-name)
						      (find-class type-name nil))
					     (sb-mop:add-dependent (find-class type-name) dependent))))))
    (dolist (state (ndfa:states sm))
      (register-class-names (ndfa:state-label state)
			    (make-instance 'dependent :ndfa sm)))))

(defmethod sb-mop:update-dependent ((class standard-class) (dep dependent) &rest init-args)
  (declare (ignore init-args))
  (let (to-remove)
    (maphash (lambda (pattern sm)
	       (when (eql sm (ndfa dep))
		 (pushnew pattern to-remove :test #'equal)))
	     *state-machines*)
    (dolist (pattern to-remove)
      (format t "removing state machine ~A because ~A changed~%" pattern class)
      (remhash pattern *state-machines*)
      (let (to-remove)
	(maphash (lambda (pattern2 type-specifier)
		   (declare (ignore type-specifier))
		   (when (equal pattern pattern2)
		     (pushnew pattern to-remove :test #'equal)))
	     *rte-types*)
	(dolist (pattern to-remove)
	  (format t "removing type cache ~A because ~A changed~%" pattern class)
	  (let ((function-name (make-rte-function-name pattern)))
	    (when (symbol-function function-name)
	      (fmakunbound function-name))
	    (remhash pattern *rte-types*)))))))
