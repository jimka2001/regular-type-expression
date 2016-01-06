
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

(deftype var-designator ()
  '(and symbol
    ;; not really true, but we'll use it to make the state machine smaller
       (not (member t nil &optional &key &rest &whole &allow-other-keys &aux))
    ;; (not (satisfies keywordp)) ; for the moment, don't use satisfies, it is untested with rte
    
    ))

(deftype optional-var ()
  `(rte var-designator (:0-1 t (:0-1 var-designator))))

(deftype keyword-pair ()
  `(rte symbol var-designator))

(deftype key-var ()
  `(rte (:or var-designator
		    ;;(rte (satisfies keywordp) var-designator) ; don't use SATISFIES, it is untested
		    keyword-pair
		    )
	       (:0-1 t (:0-1 var-designator))))

(deftype var-t-pair ()
  `(rte var-designator t))

(deftype destructuring-lambda-list ()
  `(rte
    ;; wholevar::= [&whole var] 
    (:0-1 (eql &whole) var-designator)
    ;; reqvars::= var* 
    (:0-* var-designator)
    ;; [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
    (:0-1 (eql &optional) (:0-* (:or var-designator
				     optional-var))) 
    ;; restvar::= [{&rest | &body} var] 
    (:0-1 (member &rest &body) var-designator)
    ;; keyvars::=
    ;;   [&key {var | ( {var | (keyword-name var)
    ;;                  }
    ;;                  [init-form [supplied-p-parameter]
    ;;                  ]
    ;;                )
    ;;         }*
    ;;         [&allow-other-keys]
    ;;   ]
    (:0-1 (eql &key)
	  (:0-* (:or var-designator
		     key-var))
	  (:0-1 (eql &allow-other-keys)))
    ;; auxvars::= [&aux {var | (var [init-form])}*] 
    (:0-1 (eql &aux)
	  (:0-* (:or var-designator
		     var-t-pair)))))

(defun destructuring-lambda-list-to-rte (dsl)
  (declare (type destructuring-lambda-list dsl))
  dsl)

(typep nil 'destructuring-lambda-list)
