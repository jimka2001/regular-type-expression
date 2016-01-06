
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

(defpackage :fr.epita.lrde.rte
  (:use :cl :fr.epita.lrde.ndfa)
  (:nicknames "RTE")
  (:export
   "LIST-OF"
   "CAR-CDR-OF"
   "CAR*-CDR-OF"
   "RTE"
   "MATCH-SEQUENCE"
   "RTE-RESET"
   "REGEXP-TO-RTE"
   "DESTRUCTURING-CASE"
   "REDUCED-TYPECASE"
   "OPTIMIZED-TYPECASE"
   ))

(in-package :fr.epita.lrde.rte)


(defvar *type-functions* (make-hash-table))
(defvar *state-machines* (make-hash-table :test #'equal))
(defvar *rte-types* (make-hash-table :test #'equal))


(defmacro exists (var domain &rest body)
  `(member-if #'(lambda (,var) ,@body) ,domain))

(defmacro setof (var domain &rest body)
  `(remove-if-not #'(lambda (,var) ,@body)
		  ,domain))
