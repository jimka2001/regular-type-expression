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


(defun lconc (buf items)
  (cond
    ((null buf)
     (cons items (last items)))
    ((null (car buf))
     (setf (car buf) items)
     (setf (cdr buf) (last items))
     buf)
    ((null items)
     buf)
    (t
     (setf (cdr (cdr buf)) items)
     (setf (cdr buf) (last items))
     buf)))

(defun tconc (buf &rest items)
  (lconc buf items))

(defun find-keyword (keyvar lambda-list)
  "an &key argument in a lambda list can be declared in several different ways.
FIND-KEYWORD finds and returns the keyword (symbol from keyword package) associated
with the given KEYVAR form"
  ;; {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}
  (typecase keyvar
    (symbol
     (intern (symbol-name keyvar) (find-package "KEYWORD")))
    ((cons symbol t) ; (var [init-form [supplied-p-parameter]])
     (intern (symbol-name (car keyvar)) (find-package "KEYWORD")))
    ((cons (cons symbol t) t); ((keyword-name var) [init-form [supplied-p-parameter]])
     (intern (symbol-name (car (car keyvar))) (find-package "KEYWORD")))
    (t
     (error "invalid &key syntax ~A in lambda-list ~A" keyvar lambda-list))))

(defun find-key-variable (keyvar lambda-list)
  "an &key argument in a lambda list can be declared in several different ways.
FIND-KEY-VARIABLE finds and returns the variable (symbol or destructuring-list) associated
with the given KEYVAR form"
  ;; {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}
  (typecase keyvar
    (symbol
     keyvar)
    ((cons symbol t) ; (var [init-form [supplied-p-parameter]])
     (car keyvar))
    ((cons (cons symbol t) t) ; ((keyword-name var) [init-form [supplied-p-parameter]])
     (cadr (car keyvar)))
    (t
     (error "invalid &key syntax ~A in lambda-list ~A" keyvar lambda-list))))


(defun gather-type-declarations (body)
  "BODY is the body of some destructuring-bind form.  This function, gather-type-declaration,
examines zero, one, or more declarations in the leading position of the body to find
type declarations.  An assoc list (car/cadr) is returned of the form
((var1 typespec1) (var2 typespec2)...)"
  (let (var-declarations)
    (loop :while (and body
		      (car body)
		      (listp (car body))
		      (eq 'declare (car (car body))))
	  :do (progn
		(dolist (decl (cdr (car body)))
		  (unless (member (car decl) '(dynamic-extent ignore optimize ftype inline special ignorable notinline type)
				  :test #'eq)
		    (push 'type decl))
		  (when (eq 'type (car decl))
		    (destructuring-bind (_ typespec &rest vars) decl
		      (declare (ignore _))
                       ;; need to handle the case that the same
                       ;; variable appears twice with two different
                       ;; declarations.  Need to check with the spec
                       ;; to see what the defined semantics are.
		      (dolist (var vars)
			(push (list var typespec) var-declarations))))
		  (pop body))))
    var-declarations))

(defun destructuring-lambda-list-to-rte (lambda-list &key type-specifiers)
  "Generate an RTE (regular type expression) given a destructuring-lambda-list.
If TYPE-SPECIFIERS is given it should be an alist (car/cadr) mapping variables
in the lambda list to cl type specifiers.  Such a list can be computed by calling
the function GATHER-TYPE-DECLARATIONS given a list whose leading elements are
zero or more (declare ...) forms some of which contain type declarations.

A destructuring lambda list has the following syntax:

reqvars::= var* 
optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
restvar::= [{&rest | &body} var] 
keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
            [&allow-other-keys]] 
auxvars::= [&aux {var | (var [init-form])}*] 
wholevar::= [&whole var] 
lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars)

Not supporting this syntax -> (wholevar reqvars optvars . var) "
  (declare (type list lambda-list type-specifiers))
  (let ((copy-lambda-list lambda-list)
	(ll-keywords '(&whole &optional &rest &body &key &allow-other-keys &aux))
	(whole-pattern '(:0-* t))
	req-pattern
	(rest-pattern '(:0-* t))
	(key-pattern '(:cat))
	(optional-pattern '(:cat)))

    (labels ((get-var-type (var)
	       (cadr (assoc var type-specifiers)))
	     (recursive-pattern-var (var &key (symbol-pattern t))
	       (typecase var
		 (symbol
		  (or (get-var-type var)
		      symbol-pattern))
		 (null
		  'null)
		 (list
		  `(:and list (rte ,(canonicalize-pattern
				     (destructuring-lambda-list-to-rte var
								       :type-specifiers type-specifiers)))))
		 (t
		  (error "invalid object ~A in var position in lambda list ~A" var copy-lambda-list))))
	     (recursive-pattern-var-val (var-val)
	       (typecase var-val
		 (null
		  'null)
		 (list
		  (recursive-pattern-var (car var-val)))
		 (symbol
		  (recursive-pattern-var var-val))
		 (t
		  (error "invalid object ~A in var position of lambea list ~A" var-val copy-lambda-list)))))
      ;; wholevar
      ;; wholevar::= [&whole var]
      (pop ll-keywords) ;; pop &whole off ll-keywords
      (when (eq '&whole (car lambda-list))
	(pop lambda-list)		; pop off &whole
	(setf whole-pattern (recursive-pattern-var (car lambda-list)
						   :symbol-pattern '(:0-* t)))
	(pop lambda-list)		; pop off the variable
	)
    
      ;; reqvars
      (setf req-pattern (cons :cat
			      (loop :while (and lambda-list
						(not (member (car lambda-list) ll-keywords)))
				    :collect (prog1 (recursive-pattern-var (car lambda-list))
					       (pop lambda-list)))))
    
      ;; optvars
      ;; optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
      (when (eql '&optional (car lambda-list))
	(pop ll-keywords)	    ; pop &optional off of ll-keywords
	(pop lambda-list)           ; pop &optional off the lambda-list
	(setf optional-pattern (cons :cat
				     (loop :while (and lambda-list
						       (not (member (car lambda-list) ll-keywords)))
					   :collect (prog1 (list :0-1 (recursive-pattern-var-val (car lambda-list)))
						      (pop lambda-list))))))

      ;; restvar
      ;; restvar::= [{&rest | &body} var] 
      (when (member (car lambda-list) '(&rest &body))
	(pop lambda-list)		; pop off the &rest | &body
	(when (listp (car lambda-list))
	  (setf rest-pattern (recursive-pattern-var (car lambda-list) :symbol-pattern '(:0-* t))))
	(pop lambda-list)		; pop off the var,
	(pop ll-keywords)		; pop &rest
	(pop ll-keywords)		; pop &body
	)
    
      ;; keyvars
      ;; keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
      ;;             [&allow-other-keys]]

      ;; the following (WHEN ...) calculates a regular type expression to represent the &key portion
      ;; of the lambda list, also taking into account the &allow-other-keys if given.
      ;; The result is left in KEY-PATTERN via SETF.
      ;; The format of KEY-PATTERN is one of two things depending on whether &allow-other-keys is given
      ;; or not.  
      ;;
      ;; In each case the expression has (:and (:0-* keyword t) ...) at the top
      ;; level to insist that they &key section contains only key/value pairs.
      ;; There is an (:or (:cat ...) (:cat ...)) sequence of (:cat ...) expressions.
      ;; Each (:cat ...) expression specifies one key/value pair,
      ;; and the (:or ...) indicates that they keys may be in any order.
      ;; However, if a key variable in the lambda list has a structuring element
      ;; denoting that the value will be destructured by the destructuring-case/destructuring-bind
      ;; then the destructuring element corresponds to the FIRST occurance of that keyword
      ;; in the given argument list.  If the same keyword is given multiple times in the argument
      ;; list, no structuring element is applied to occurances other than the first.
      ;; E.g., (&key x ((:y (y0 y1 y2)) '(nil nil nil)))
      ;;  The value associated with the first :y in the given argument list must match the
      ;;  destructuring element (y0 y1 y2), i.e., Fy=(:and list (rte (:cat t t t)))
      ;;  Thus each element of the (:cat ...) is of the form
      ;;  (:0-1 (eql :y) Fy                      ...)
      ;;  (:0-1 (eql :y) (:and list (rte t t t)) ...)
      ;; Here are two examples:
      ;;  the values of Fx, Fy, and Fz depend on the structuring elements of
      ;;    :x :y and :z respectively.

      #|
      (&key x y z) ; without &allow-other-keys
       ==>
          (:and (:0-* keyword t)
		(:or
		 (:cat (:0-1 (eql :x) Fx (:0-* (member :x) t))
		       (:0-1 (eql :y) Fy (:0-* (member :y :x) t))
		       (:0-1 (eql :z) Fz (:0-* (member :z :y :x) t)))

		 (:cat (:0-1 (eql :y) Fy (:0-* (member :y) t))
		       (:0-1 (eql :x) Fx (:0-* (member :x :y) t))
		       (:0-1 (eql :z) Fz (:0-* (member :z :x :y) t)))

		 (:cat (:0-1 (eql :x) Fx (:0-* (member :x) t))
		       (:0-1 (eql :z) Fz (:0-* (member :z :x) t))
		       (:0-1 (eql :y) Fy (:0-* (member :y :z :x) t)))

		 (:cat (:0-1 (eql :z) Fz (:0-* (member :z) t))
		       (:0-1 (eql :x) Fx (:0-* (member :x :z) t))
		       (:0-1 (eql :y) Fy (:0-* (member :y :x :z) t)))

		 (:cat (:0-1 (eql :y) Fy (:0-* (member :y) t))
		       (:0-1 (eql :z) Fz (:0-* (member :z :y) t))
		       (:0-1 (eql :x) Fx (:0-* (member :x :z :y) t)))

		 (:cat (:0-1 (eql :z) Fz (:0-* (member :z) t))
		       (:0-1 (eql :y) Fy (:0-* (member :y :z) t))
		       (:0-1 (eql :x) Fx (:0-* (member :x :y :z) t)))))

      (&key x y z &allow-other-keys)
       ==>
         (:and (:0-* keyword t)
	       (:or
		(:cat (:0-1 (eql :x) Fx (:0-* (not (member :y :z)) t))
		      (:0-1 (eql :y) Fy (:0-* (not (member :z)) t))
		      (:0-1 (eql :z) Fz (:0-* t t)))

		(:cat (:0-1 (eql :y) Fy (:0-* (not (member :x :z)) t))
		      (:0-1 (eql :x) Fx (:0-* (not (member :z)) t))
		      (:0-1 (eql :z) Fz (:0-* t t)))

		(:cat (:0-1 (eql :x) Fx (:0-* (not (member :y :z)) t))
		      (:0-1 (eql :z) Fz (:0-* (not (member :y)) t))
		      (:0-1 (eql :y) Fy (:0-* t t)))

		(:cat (:0-1 (eql :z) Fz (:0-* (not (member :x :y)) t))
		      (:0-1 (eql :x) Fx (:0-* (not (member :y)) t))
		      (:0-1 (eql :y) Fy (:0-* t t)))

		(:cat (:0-1 (eql :y) Fy (:0-* (not (member :x :z)) t))
		      (:0-1 (eql :z) Fz (:0-* (not (member :x)) t))
		      (:0-1 (eql :x) Fx (:0-* t t)))

		(:cat (:0-1 (eql :z) Fz (:0-* (not (member :x :y)) t))
		      (:0-1 (eql :y) Fy (:0-* (not (member :x)) t))
		      (:0-1 (eql :x) Fx (:0-* t t)))))
      |#
      
      (when (eql '&key (car lambda-list))
	(pop lambda-list)		; pop off &key
	(let* (key-vars
	       used-keywords
	       (key-formats (loop :while (and lambda-list
					      (not (member (car lambda-list) ll-keywords)))
				  :for ll-var = (pop lambda-list)
				  :for keyword = (find-keyword ll-var copy-lambda-list)
				  :for key-var = (find-key-variable ll-var copy-lambda-list)
				  :for pattern = (recursive-pattern-var key-var)
				  :collect (list keyword pattern)
				  :do (push keyword used-keywords)
				  :do (push key-var key-vars)))
	      (allow-other-keys (when (eql '&allow-other-keys (car lambda-list))
				  (pop lambda-list)
				  t))
	      or-terms)
	  (map-permutations (lambda (permutation)
			      (let ((done nil)
				    (remaining (alphabetize used-keywords))
				    (buf (list nil)))
				(dolist (item permutation)
				  (destructuring-bind (keyword pattern) item
				    (push keyword done)
				    (setf remaining (remove keyword remaining))
				    (let ((key-suffix (cond
							((not allow-other-keys)
							 `(member ,@done))
							(remaining
							 `(not (member ,@remaining)))
							(t
							 t))))
				    (tconc buf `(:0-1 (eql ,keyword) ,pattern (:0-* ,key-suffix t))))))
				(push `(:cat ,@(car buf)) or-terms)))
			    key-formats)

	  (setf key-pattern `(:and (:0-* keyword t)
				   (:or ,@or-terms)))))
	
      (pop ll-keywords)			; pop off &key
      (pop ll-keywords)			; pop off &allow-other-keys
    
      ;; auxvars
      ;; auxvars::= [&aux {var | (var [init-form])}*]     
      (when (eql '&aux (car lambda-list))
	(pop lambda-list)	   ; pop off the &aux from lambda-list
	(loop :while lambda-list
	      :for var = (pop lambda-list) ;; update every time through the loop
	      :do (typecase var
		    (null
		     (error "destructuring not supported in &aux variable ~A of lambda-list ~A" var copy-lambda-list))
		    (symbol
		     t)
		    ((cons symbol t)
		     t)
		    (t
		     (error "destructuring not supported in &aux variable ~A of lambda-list ~A" var copy-lambda-list)))))
      
      (pop ll-keywords)

      (if lambda-list
	  (error "lambda list ~A not parsed correctly: suspicious tail is ~A" copy-lambda-list lambda-list)
	  `(:and ,whole-pattern
		 (:cat ,req-pattern
		       ,optional-pattern
		       (:and ,rest-pattern
			     ,key-pattern)))))))

(defun expand-destructuring-case (object-form clauses)
  (let ((object (gensym)))
    (flet ((transform-clause (clause)
	     (destructuring-bind (lambda-list &rest body) clause
	       (let ((pattern (destructuring-lambda-list-to-rte lambda-list
								:type-specifiers (gather-type-declarations body))))
		 `((rte  ,(canonicalize-pattern pattern))
		   (destructuring-bind ,lambda-list ,object
		     ,@body))))))
      `(let ((,object ,object-form))
	 (typecase ,object
	   ((not list) nil)
	   ,@(mapcar #'transform-clause clauses))))))

(defmacro destructuring-case (object-form &rest clauses)
  (expand-destructuring-case object-form clauses))


(defun expand-destructuring-methods (object-form clauses call-next-method)
  (declare (type symbol call-next-method))
  (let ((object (gensym))
	(objects (gensym)))
    (flet ((transform-clause (clauses)
	     (destructuring-bind (lambda-list &rest body) (car clauses)
	       (let ((pattern (destructuring-lambda-list-to-rte lambda-list
								:type-specifiers (gather-type-declarations body))))

		 (if (cdr clauses)
		     `((rte ,(canonicalize-pattern pattern))
		       (flet ((,call-next-method (&rest ,objects)
				(destructuring-methods (if ,objects
							   (car ,objects)
							   ,object)
				    (:call-next-method ,call-next-method)
				  ,@(cdr clauses))))
			 (declare (ignorable (function ,call-next-method)))
			 (destructuring-bind ,lambda-list ,object
			   ,@body)))
		     `((rte ,(canonicalize-pattern pattern))
		       (flet ((,call-next-method (&rest ,objects)
				(declare (ignore ,objects))
				(error "cannot call ~A from final clause" ',call-next-method)))
			 (declare (ignorable (function ,call-next-method)))
			 (destructuring-bind ,lambda-list ,object
			   ,@body))))))))
	   `(let ((,object ,object-form))
	      (typecase ,object
		((not list) nil)
		,@(maplist #'transform-clause clauses))))))

(defmacro destructuring-methods (object-form (&key (call-next-method 'call-next-method)) &body clauses)
  (expand-destructuring-methods object-form clauses call-next-method))

