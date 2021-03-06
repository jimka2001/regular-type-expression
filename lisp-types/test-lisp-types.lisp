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

(defpackage :lisp-types.test
  (:shadowing-import-from :lisp-types "TEST" "A")
  ;;(:shadowing-import-from :closer-mop "STANDARD-GENERIC-FUNCTION" "DEFMETHOD" "DEFGENERIC")
  (:use :cl :lisp-types :lisp-unit ;;:closer-mop
   #+sbcl :sb-pcl
   #+allegro :aclmop
        ))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun shadow-package-symbols ()
    (let ((lisp-types-test (find-package  :lisp-types.test))
          (lisp-types (find-package  :lisp-types)))
      (do-symbols (name :lisp-types)
        (when (and (eq lisp-types (symbol-package name))
                   (not (find-symbol (symbol-name name) lisp-types-test)))
          (format t "5 importing name=~A into  :lisp-types.test~%" name)
          (shadowing-import name :lisp-types.test))))))

(shadow-package-symbols)


(in-package :lisp-types.test)

(defun test ()
  (let ((*print-summary* t)
	(*print-failures* t)
	(*summarize-results* t)
	(*print-errors* t))
    (run-tests :all (list :lisp-types.test))))

(define-test type/reduce-b
  (assert-true (equal (reduce-lisp-type '(AND ARITHMETIC-ERROR (NOT CELL-ERROR)))

                      'arithmetic-error))
    (assert-true (equal (reduce-lisp-type '(OR ARITHMETIC-ERROR (NOT CELL-ERROR)))

                        '(NOT CELL-ERROR))))


(define-test type/reduce-a
  (assert-true (equal (reduce-lisp-type '(and (and (integer 0) (not (integer 0 3)))
                                          (and fixnum (not (integer 0 3)))))
                      '(and fixnum (integer 0) (not (integer 0 3)))))
  (assert-true (equal (reduce-lisp-type '(and ARITHMETIC-ERROR CELL-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                          ATOM
                          CONDITION
                          CELL-ERROR
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM
                                           CONDITION CELL-ERROR
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM
                                           (AND CONDITION CELL-ERROR)
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (AND (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM)
                                           (AND CONDITION CELL-ERROR)
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (AND (AND (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM)
                                           (AND CONDITION CELL-ERROR))
                                          ARITHMETIC-ERROR))
                      nil)))

(define-test type/reduce-compound
  ;; array
  (assert-true (equal (reduce-lisp-type '(array (and integer number) (3)))
		      '(array integer (3))))
  (assert-true (equal (reduce-lisp-type '(array * (3)))
		      '(array * (3))))

  ;; base-string
  (assert-true (equal (reduce-lisp-type '(base-string *))
		      'base-string))

  ;; bit-vector
  (assert-true (equal (reduce-lisp-type '(bit-vector *))
		      'bit-vector))

  (assert-true (equal (reduce-lisp-type '(bit-vector 3))
		      '(bit-vector 3)))

  ;; complex
  (assert-true (equal (reduce-lisp-type '(complex (and number real)))
		      '(complex real)))
  (assert-true (equal (reduce-lisp-type '(complex *))
		      'complex ))

  ;; simple-array
  (assert-true (equal (reduce-lisp-type '(simple-array (and number real) (3)))
		      '(simple-array real (3))))

  ;; vector
  (assert-true (equal (reduce-lisp-type '(vector (and number real)))
		      '(vector real)))

  )


(define-test type/reduce-cons
  (assert-true (equal (reduce-lisp-type '(cons (and float number) (or string (not string))))
		      '(cons float t)))
  (assert-true (equal (reduce-lisp-type '(cons * *))
		      'cons))
  (assert-true (equal (reduce-lisp-type '(cons (and float number) *))
		      '(cons float)))
  (assert-true (equal (reduce-lisp-type '(cons * (and float number)))
		      '(cons * float))))

(define-test type/reduce-function
  (assert-true (equal (reduce-lisp-type '(function (integer integer) integer))
		      '(function (integer integer) integer)))
  (assert-true (equal (reduce-lisp-type '(function ((and integer integer) integer) integer))
		      '(function (integer integer) integer)))

  (assert-true (equal (reduce-lisp-type '(function ((and integer integer) (and integer integer)) (and integer integer)))
		      '(function (integer integer) integer)))
  ;; test some optional arguments &optional &key &rest etc

  ;; &optional
  (assert-true (equal (reduce-lisp-type '(function (&optional) (and list cons)))
		      '(function (&optional) cons)))

  (assert-true (equal (reduce-lisp-type '(function (&optional (and integer number)) (and list cons)))
		      '(function (&optional integer) cons)))
  
  ;; &rest
  (assert-true (equal (reduce-lisp-type '(function (&rest (and integer number)) (and list cons)))
		      '(function (&rest integer) cons)))

  (assert-error 'error (reduce-lisp-type '(function (&rest t t))))


  ;; &key
  (assert-true (equal (reduce-lisp-type '(function (&key) t))
		      '(function (&key) t)))

  (assert-true (equal (reduce-lisp-type '(function (&key (x (and integer number))) (and list cons)))
		      '(function (&key (x integer)) cons)))

  ;; combining &optional &key &rest
  (assert-true (equal (reduce-lisp-type
		       '(function ((and integer number)
				   &optional (and integer number) (and integer number)
				   &rest (and integer number)
				   &key (x (and integer number)) (y (and integer number)))
			 (and list cons)))
		      '(function (integer
				  &optional integer integer
				  &rest integer
				  &key (x integer) (y integer))
			cons)))

  )

;; test function subtypes
;; (define-test type/function-subtypes
;;   ;; If   T1 <: S1  and S2 <: T2
;;   ;; then S1->S2 <: T1->T2
;;
;;   ;; If  in-sub <: in-super   and out-sub <: out-super
;;   ;; then in-super -> out-sub <: in-sub -> out-super
;;
;;   (let ((types '(number real integer))) 
;;     (dolist (T1 types)
;;       (dolist (T2 types)
;; 	(dolist (S1 types)
;; 	  (dolist (S2 types)
;; 	    (when (and (subtypep T1 S1)
;; 		       (subtypep S2 T2))
;; 	      (assert-true (subtypep `(function (,S1) ,S2)
;; 				     `(function (,T1) ,T2))))))))))


(define-test type/reduce-lisp-type
  (flet ((reduce-lisp-type (type)
	   (reduce-lisp-type type)))
    (assert-true (equal (reduce-lisp-type '(and))
			t))
    (assert-true (equal (reduce-lisp-type '(or))
			nil))
    (assert-true (equal (reduce-lisp-type '(and float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float))
			'float))
    (assert-true (equal (reduce-lisp-type '(and float t))
			'float))
    (assert-true (equal (reduce-lisp-type '(and t float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float nil))
			'float))
    (assert-true (equal (reduce-lisp-type '(or nil float))
			'float))
    (assert-true (equal (reduce-lisp-type '(not nil))
			t))
    (assert-true (equal (reduce-lisp-type '(not t))
			nil))
    (assert-true (equal (reduce-lisp-type '(not (not float)))
			'float))
    (assert-true (equal (reduce-lisp-type '(not (or float string)))
			'(and (not float) (not string))))
    (assert-true (equal (reduce-lisp-type '(not (and float string)))
			t)) ;; because (and float string) is nil
    ))

(define-test type/reduce-type-nil
  (assert-true (eql nil
		    (reduce-lisp-type 
		     '(and (not t)
		           (and (not keyword) (eql :a)))))))


(define-test type/fixed-point
  
  (assert-true (< (lisp-types::fixed-point #'(lambda (obj)
							    (/ obj 2.0))
							10
							:test #'(lambda (a b)
								  (< (abs (- a b)) 0.01)))
		  0.01)))

(defclass AB-247 () ())
(defclass A1-247 (AB-247) ())
(defclass B1-247 (AB-247) ())
(defclass AB1-247 (A1-247 B1-247) ())
(defclass C-247 () ())
(defclass D-247 () ())
(defclass E-247 () ())
(defclass :F-247 () ())
(define-test type/reduce-lisp-type2

  (assert-false (class-direct-subclasses (find-class 'C-247)))
  (assert-false (class-direct-subclasses (find-class 'D-247)))
  (assert-false (class-direct-subclasses (find-class 'E-247)))
  (assert-false (class-direct-subclasses (find-class ':F-247)))
  (assert-true (intersection (class-direct-subclasses (find-class 'A1-247))
                             (class-direct-subclasses (find-class 'B1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or A1-247 (and A1-247 B1-247 C-247 D-247) E-247))
                                   '(or E-247 A1-247)))
  (let ((un-interned (gensym)))
    (assert-true (equivalent-types-p (reduce-lisp-type `(or (and A1-247 B1-247)
                                                            (and A1-247 B1-247 C-247 D-247)
                                                            E-247 ,un-interned))
                                     `(or ,un-interned E-247 (and B1-247 A1-247)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A1-247 B1-247) (and A1-247 B1-247 C-247 D-247) E-247 :F-247))
                                   '(or :f-247 E-247 (and B1-247 A1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or A1-247 (and (not A1-247) B1-247)))
                                   '(or B1-247 A1-247)))
  (assert-true (equivalent-types-p (reduce-lisp-type'(or (and (not A1-247) B1-247) A1-247))
                                   '(or B1-247 A1-247)))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (not A1-247) (and A1-247 B1-247)))
                                   '(or B1-247 (not A1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A1-247 B1-247) (not A1-247)))
                                   '(or B1-247 (not A1-247)))))

(defclass W-282 () ())
(defclass A-282 () ())
(defclass B-282 () ())
(defclass C-282 () ())
(defclass U-282 () ())
(defclass V-282 () ())
(defclass X-282 () ())
(defclass Y-282 () ())
(defclass Z-282 () ())
(defclass join-282 (W-282 A-282 B-282 C-282 U-282 V-282 X-282 Y-282 Z-282) ())
(define-test type/consensus-theorem
  (assert-true (reduce (lambda (classes class-name)
                         (intersection classes
                                       (class-direct-subclasses (find-class class-name))))
                       '(W-282 A-282 B-282 C-282 U-282 V-282 X-282 Y-282 Z-282)
                       :initial-value (class-direct-subclasses (find-class 'W-282))))
          
  (assert-true (equivalent-types-p (reduce-lisp-type '(or W-282 (and A-282 B-282) X-282
                                                       Y-282 (and (not A-282) C-282)
                                                       Z-282 (and B-282 C-282)))
                                   '(OR X-282 Y-282 W-282 Z-282 (AND A-282 B-282) (AND C-282 (NOT A-282)))))
					
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and (not A-282) C-282)
                                                       (and B-282 C-282)))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and B-282 C-282)
                                                       (and (not A-282) C-282)))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and B-282 C-282)
                                                       (and C-282 (not A-282))))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))

  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 U-282 V-282)
                                                       (and V-282 W-282 (not A-282))
                                                       (and V-282 W-282 U-282)))
                                   '(OR (AND A-282 U-282 V-282) (AND V-282 W-282 (NOT A-282)))))
  (assert-true (member (reduce-lisp-type '(or (and A-282 U-282 V-282)
                                           (and (not A-282) V-282 W-282)
                                           (and U-282 V-282 W-282)))
                       
                       '((OR (AND A-282 U-282 V-282) (AND V-282 W-282 (NOT A-282))))
                       :test #'equivalent-types-p)))


(define-test type/rule-case
  (let ((n 0))
    (assert-true (equal (lisp-types::rule-case (+ 4 5)
			  (t
			   (incf n)
			   9)
			  ((= 1 n)
			   (incf n)
			   9)
			  ((= 2 n)
			   (incf n)
			   0))
			0))
    (assert-true (equal 3 n))))
				  
(define-test type/reduce-member
  (assert-true (equal (reduce-lisp-type '(member))
		      nil))
  (assert-true (equal (reduce-lisp-type '(and (member 1 2 3)
					  (member 2 3 4)))
		      '(member 2 3)))
  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 3))
					  (not (member 2 3 4))))
		      '(not (member 1 2 3 4 ))))
  (assert-true (equal (reduce-lisp-type '(and (not (member 20))
					  (not (member 2 3 4))
					  (member 10 20 30)
					  (member 20 30)))
		      '(eql 30)))
  (assert-true (equal (reduce-lisp-type '(and keyword (member :y :z) (not (eql :y))))
		      '(eql :z)))
  (assert-true (equal (reduce-lisp-type '(and (member a b 2 3) symbol))
		      '(member a b)))
  (assert-true (equal (reduce-lisp-type '(and (member a 2) symbol))
		      '(eql a)))
  (assert-true (equal (reduce-lisp-type '(and (member a b) fixnum))
		      nil))

  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 a)) (not (member 2 3 4 b))))
		      '(not (member 1 2 3 4 a b))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member 1 2 a b))))
		      '(and fixnum (not (member 1 2)))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member a b))))
		      'fixnum))
  (assert-true (equal (reduce-lisp-type '(or fixnum string (member 1 2 "hello" a b)))
		      '(or fixnum string (member a b))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(and keyword (not (member :x))))
		      '(and keyword (not (eql :x)))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(or number (not (member 1 2 a b))))
		      '(not (member a b))))
  )

(define-test type/decompose-types-t
  (assert-false (member t (decompose-types '(KEYWORD (MEMBER :A :B) T          (NOT (EQL :A)) (EQL :A) (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (decompose-types '(        (MEMBER :A :B) T          (NOT (EQL :A)) (EQL :A) (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (decompose-types '(KEYWORD (MEMBER :A :B) T (EQL :A)                         (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (decompose-types '(KEYWORD (MEMBER :A :B) T                                  (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (decompose-types '(KEYWORD (MEMBER :A :B) T                                                 (EQL :B)))))
  (assert-false (member t (decompose-types '(KEYWORD (MEMBER :A :B) T))))
  )

(define-test type/decompose-types-member
  (assert-true (equal '(t) (decompose-types '(nil t))))
  (assert-true (equal '(t) (decompose-types '((member) t)))))

(define-test type/decompose-types
  (let ((A '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	(B '(2 3 5 6))
	(C '(3 4 5 7))
	(D '(5 6 7 8 9 10 11 12 13))
	(E '(12 13 14))
	(F '(9 10))
	(G '(10 11 12))
	(H '(15))
	(I '(16)))
    (let ((disjoint (decompose-types `((member ,@A)
					 (member ,@B)
					 (member ,@C)
					 (member ,@D)
					 (member ,@E)
					 (member ,@F)
					 (member ,@G)
					 (member ,@H)
					 (member ,@I)))))
      (assert-true (equal 16 (length disjoint)))
      (assert-true (notany (lambda (s)
			     (subtypep s nil)) disjoint))
      (dotimes (e 16)
	(let ((e (+ e 1)))
	  (assert-true (equal 1 (count-if (lambda (s)
					    (typep e s))
					  disjoint))))))))

(define-test type/decompose-types2 ()
  (let ((A '(1 2 3 4 5 6 7 8 9 10 11 12 13))
	(B '(2 3 5 6))
	(C '(3 4 5 7))
	(D '(5 6 7 8 9 13))
	(E '(9))
	(F '(10))
	(G '(11))
	(H '(9 12 13)))
    (let ((disjoint (decompose-types `((member ,@A)
					 (member ,@B)
					 (member ,@C)
					 (member ,@D)
					 (member ,@E)
					 (member ,@F)
					 (member ,@G)
					 (member ,@H)))))
      (assert-true (equal 13 (length disjoint)))
      (assert-true (notany (lambda (s)
			     (subtypep s nil)) disjoint))
      (dotimes (e 13)
	(let ((e (+ e 1)))
	  (assert-true (equal 1 (count-if (lambda (s)
					    (typep e s))
					  disjoint))))))))

(define-test type/enter-conses ()
  (let ((hash (make-hash-table :test #'equal)))
    (assert-true (eq (lisp-types::enter-conses hash '(a (1 2 3) b))
		     (lisp-types::enter-conses hash '(a (1 2 3) b))))
    (assert-true (eq (cadr (lisp-types::enter-conses hash '(a (1 2 3) b)))
		     (cadr (lisp-types::enter-conses hash '(c (1 2 3) d)))))
    (assert-true (eq (cdr (lisp-types::enter-conses hash '(a 1 2 3 4)))
		     (cdr (lisp-types::enter-conses hash '(b 1 2 3 4)))))))

(define-test type/subtype
  (assert-true (equal '(t t) (multiple-value-list (smarter-subtypep '(eql :x) 'keyword))))
  (assert-true (equal '(t t) (multiple-value-list (smarter-subtypep '(not keyword) '(not (eql :x))))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep 'keyword '(eql :x)))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep '(not keyword) '(eql :x)))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep '(not (eql :x)) 'keyword)))))

;; disjoint-types-p
(define-test type/disjoint-types-p
  (assert-false (disjoint-types-p 'fixnum '(member 1 2)))
  (assert-false (disjoint-types-p 'fixnum '(not (member 1 2))))
  (assert-false (disjoint-types-p 'keyword '(member :a :b)))
  (assert-false (disjoint-types-p 'keyword '(eql :a)))
  (assert-false (disjoint-types-p '(AND KEYWORD (NOT (MEMBER :A :B))) T))
  (assert-false (disjoint-types-p '(and symbol (not (eql a))) t))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p 'number '(not float)))))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p '(not float) 'number))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p '(not number) 'float))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p 'float '(not number)))))
  (assert-false (disjoint-types-p '(not float) '(not integer))))
