;;; list-of.lisp --- some types helpful for debugging

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


(in-package :fr.epita.lrde.rte)


(deftype list-of (element-type)
  "Matches possibly empty list such that each element matches ELEMENT-TYPE"
  (expand-my-typedef 'list-of (list element-type)
		     (lambda (value)
		       (every (lambda (v) (typep v element-type)) value))))

;; car-cdr-of is the same as the CL:CONS type
;; (deftype car-cdr-of (car-type cdr-type)
;;   "Matches non-empty list whose CAR matches CAR-TYPE, and whose possibly nil CDR matches CDR-TYPE"
;;   (expand-my-typedef 'car-cdr-of (list car-type cdr-type)
;; 		     (lambda (value)
;; 		       (and value
;; 			    (typep (car value) car-type)
;; 			    (typep (cdr value) cdr-type)))))

(deftype car*-cdr-of (&rest types)
  "Matches non-empty list whose leading elements respectively match the leading elements of TYPES, and thereafter the list of
remaining elements matches the final element of types.  E.g., 
(typep '(1 hello nil 1 2 3 4 5) '(car*-cdr-of number symbol null (list-of number))) 
 => TRUE"
  (cond ((null (cdr types))
	 (error "invalid type ~A" (cons 'car*-cdr-of types)))
	((null (cddr types))
	 `(cons ,@types))
	(t
	 `(cons ,(car types) (car*-cdr-of ,@(cdr types))))))
