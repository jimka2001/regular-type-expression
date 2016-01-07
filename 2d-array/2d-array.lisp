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

(defpackage :2d-array
  (:nicknames "2D-ARRAY")
  (:export
   "ROW-VECTOR"
   "COLUMN-VECTOR"
   "VECTOR-OF-ROWS"
   "VECTOR-OF-COLUMNS"
   "2D-ARRAY-GET")
  (:use :cl))

(in-package :2d-array)

(defgeneric 2d-array-get (seq row col))
(defgeneric (setf 2d-array-get) (value seq row col))

(defclass 2d-array-as-sequence (sequence standard-object)
  ((2d-array :initarg :2d-array
	     :type (array * (* *)) ;; 2 dimensional array holding any type
	     :reader 2d-array)))

(defmethod 2d-array-get ((seq 2d-array-as-sequence) (row fixnum) (col fixnum))
  (aref (2d-array seq) row col))

(defmethod (setf 2d-array-get) (value (seq 2d-array-as-sequence) (row fixnum) (col fixnum))
  (setf (aref (2d-array seq) row col)
	value))

(defclass row-vector (2d-array-as-sequence)
  ((row :type fixnum :initarg :row :accessor row)))

(defmethod print-object ((self row-vector) stream)
  (if (and (slot-boundp self 'row)
	   (slot-boundp self '2d-array))
      (print-unreadable-object (self stream :identity nil :type t)
	(format stream "[")
	(dotimes (col (sequence:length self))
	  (format stream "~A " (sequence:elt self col)))
	(format stream "]"))
      (call-next-method)))

(defmethod sequence:length ((seq row-vector))
  "Returns the number of columns in the given row-vector"
  (array-dimension (2d-array seq) 1))

(defmethod 2d-array-get ((arr array) (row fixnum) (column fixnum))
  (aref arr row column))

(defmethod (setf 2d-array-get) (value (arr array) (row fixnum) (column fixnum))
  (setf (aref arr row column)
	value))

(defmethod sequence:elt ((seq row-vector) column)
  (2d-array-get (2d-array seq) (row seq) column))

(defmethod (setf sequence:elt) (value (seq row-vector) column)
  (setf (2d-array-get (2d-array seq) (row seq) column) value))

(defclass column-vector (2d-array-as-sequence)
  ((column :type fixnum :initarg :column :accessor column)))

(defmethod print-object ((self column-vector) stream)
  (if (and (slot-boundp self 'column)
	   (slot-boundp self '2d-array))
      (print-unreadable-object (self stream :identity nil :type t)
	(format stream "[")
	(dotimes (col (sequence:length self))
	  (format stream "~A " (sequence:elt self col)))
	(format stream "]"))
      (call-next-method)))

(defmethod sequence:length ((seq column-vector))
  "Returns the number of rows in the given column-vector"
  (array-dimension (2d-array seq) 0))

(defmethod sequence:elt ((seq column-vector) row)
  (2d-array-get (2d-array seq) row (column seq)))

(defmethod (setf sequence:elt) (value (seq column-vector) row)
  (setf (2d-array-get (2d-array seq) row (column seq))
	value))

(defclass vector-of-rows (2d-array-as-sequence)
  ())

(defmethod sequence:length ((seq vector-of-rows))
  (array-dimension (2d-array seq) 0))

(defmethod sequence:elt ((seq vector-of-rows) row)
  (make-instance 'row-vector :2d-array (2d-array seq) :row row))

(defmethod (setf sequence:elt) ((row-of-values sequence) (seq vector-of-rows) row)
  "row-of-values is any SEQUENCE <= number of columns of (2d-array seq), iteratively set the values of the designated row vector
to the values from ROW-OF-VALUES."
  (let ((column 0))
    (every (lambda (value)
	     (setf (2d-array-get (2d-array seq) row column) value)
	     (incf column))
	  row-of-values))
  row-of-values)


(defclass vector-of-columns (2d-array-as-sequence)
  ())

(defmethod sequence:length ((seq vector-of-columns))
  (array-dimension (2d-array seq) 1))

(defmethod sequence:elt ((seq vector-of-columns) column)
  (make-instance 'column-vector :2d-array (2d-array seq) :column column))

(defmethod (setf sequence:elt) ((column-of-values sequence) (seq vector-of-columns) column)
  "column-of-values is any SEQUENCE <= number of rows of (2d-array seq), iteratively set the values of the designated column vector
to the values from COLUMN-OF-VALUES."
  (let ((row 0))
    (every (lambda (value)
	     (setf (2d-array-get (2d-array seq) row column) value)
	     (incf row))
	   column-of-values))
  column-of-values)
