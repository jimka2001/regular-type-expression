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
