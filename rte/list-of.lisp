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


(in-package :rte)


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
