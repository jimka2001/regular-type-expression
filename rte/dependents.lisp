
;; Copyright (C) 2015 EPITA Research and Development Laboratory

;; This file is part of Climb.

;; Climb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Climb is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


(in-package   :fr.epita.lrde.rte)

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
