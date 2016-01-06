
;; Copyright (C) 2012 EPITA Research and Development Laboratory

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

(defun expand-my-typedef (type args unary-function &key (function-name (gensym (format nil "~A" type))))
  (declare (type symbol function-name))
  (let* ((type (cons type args))
	 (sat  (or (gethash type *type-functions*)
		   (setf (gethash type *type-functions*)
			 (progn
			   (setf (symbol-function function-name) unary-function)
			   function-name)))))
    `(and sequence
	  (satisfies ,sat))))
