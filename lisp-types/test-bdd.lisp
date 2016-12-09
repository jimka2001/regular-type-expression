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

(in-package :lisp-types.test)

(do-symbols (name :lisp-types)
  (shadowing-import name :lisp-types.test))


(define-test test/bdd-to-dnf
  (assert-true (equal 'integer
                      (bdd-to-dnf (bdd 'integer))))
  (assert-true (bdd-to-dnf (bdd '(or string integer))))
  (assert-true (bdd-to-dnf (bdd '(or (and integer (not string)) (and string (not integer)))))))

(define-test test/bdd-create
  (assert-true (bdd 'integer))
  (assert-true (bdd '(or integer float)))
  (assert-true (bdd '(or (and integer (not string)) (and string (not integer)))))
  (assert-true (eq (bdd '(or integer string))
                   (bdd '(or string integer))))
  (assert-true (eq (bdd '(or (and integer (not string)) (and string (not integer))))
                   (bdd '(or (and (not integer) string) (and integer (not string))))))

  )

(define-test types/bdd-collect-atomic-types
  (assert-false (set-exclusive-or (bdd-collect-atomic-types (bdd '(or (and integer (not string)) (and string (not integer)))))
                                  
                                  '(integer string))))

(define-test types/bdd-equal
  (assert-true (bdd-equal *bdd-true*
                          *bdd-true*))
  (assert-true (bdd-equal *bdd-false*
                          *bdd-false*))
  (assert-false (bdd-equal *bdd-true*
                          *bdd-false*))
  (assert-false (bdd-equal *bdd-false*
                           *bdd-true*))
  (assert-true (bdd-equal (bdd '(or integer float))
                          (bdd '(or float integer)))))
  
(define-test test/certain-reductions
  (assert-true (bdd '(or (and integer (not string)) (and string (not integer)))))
  (assert-false (bdd-to-dnf (bdd-and-not (bdd 'integer) (bdd 'number)))))

(define-test type/3-types
  (let ((decomp (bdd-decompose-types '(CHAR-CODE DOUBLE-FLOAT UNSIGNED-BYTE))))
    (dolist (t1 decomp)
      (dolist (t2 (remove t1 decomp))
        (assert-false (subtypep t1 t2))
        (assert-false (subtypep t2 t1))
        (assert-false (smarter-subtypep t1 t2))
        (assert-false (smarter-subtypep t2 t1))))))

(define-test type/bdd-subtypep
  (assert-true (bdd-subtypep (bdd 'float) (bdd 'number)))
  (assert-true (bdd-subtypep (bdd '(eql :x)) (bdd 'keyword)))
  (assert-true (bdd-subtypep (bdd '(not keyword)) (bdd '(not (eql :x)))))
  (assert-false (bdd-subtypep (bdd 'keyword) (bdd '(eql :x))))
  (assert-false (bdd-subtypep (bdd '(not keyword)) (bdd '(eql :x))))
  (assert-false (bdd-subtypep (bdd '(not (eql :x))) (bdd 'keyword)))

  (assert-true (bdd-type-equal (bdd '(and (member :a :b) keyword))
                               (bdd '(member :a :b))))

  (assert-true (equal (bdd-to-dnf (bdd '(and (member :a :b) keyword)))
                      '(member :a :b)))
  )


(define-test type/bdd-performance-test
  (let* ((types '(ARITHMETIC-ERROR
                  ARRAY
                  ARRAY-RANK
                  ARRAY-TOTAL-SIZE
                  ATOM
                  BASE-CHAR
                  BASE-STRING
                  BIGNUM
                  BIT
                  BIT-VECTOR
                  BOOLEAN
                  BROADCAST-STREAM
                  BUILT-IN-CLASS
                  CELL-ERROR
                  CHAR-INT
                  CHARACTER
                  CLASS
                  COMPLEX))
         (decomp '((AND (NOT BIT) ARRAY-RANK) BIT (AND (NOT CHAR-INT) ARRAY-TOTAL-SIZE)
                   (AND CHAR-INT (NOT ARRAY-RANK)) BASE-CHAR (AND CHARACTER (NOT BASE-CHAR))
                   (AND (NOT CELL-ERROR) BUILT-IN-CLASS ARITHMETIC-ERROR)
                   (AND (NOT CLASS) (NOT CELL-ERROR) ARITHMETIC-ERROR)
                   (AND CELL-ERROR BUILT-IN-CLASS ARITHMETIC-ERROR)
                   (AND CLASS CELL-ERROR (NOT BUILT-IN-CLASS) (NOT ARITHMETIC-ERROR))
                   (AND (NOT CELL-ERROR) BUILT-IN-CLASS (NOT ARITHMETIC-ERROR))
                   (AND CLASS (NOT CELL-ERROR) (NOT BUILT-IN-CLASS) (NOT ARITHMETIC-ERROR))
                   (AND (NOT COMPLEX) (NOT CLASS) (NOT CHARACTER) (NOT CELL-ERROR)
                    (NOT BROADCAST-STREAM) (NOT BOOLEAN) (NOT BIGNUM) ATOM
                    (NOT ARRAY-TOTAL-SIZE) (NOT ARRAY) (NOT ARITHMETIC-ERROR))
                   COMPLEX (AND (NOT CLASS) CELL-ERROR (NOT ARITHMETIC-ERROR))
                   (AND CELL-ERROR BUILT-IN-CLASS (NOT ARITHMETIC-ERROR))
                   (AND CLASS CELL-ERROR (NOT BUILT-IN-CLASS) ARITHMETIC-ERROR)
                   (AND CLASS (NOT CELL-ERROR) (NOT BUILT-IN-CLASS) ARITHMETIC-ERROR)
                   (AND (NOT CLASS) CELL-ERROR ARITHMETIC-ERROR) BROADCAST-STREAM BOOLEAN BIGNUM
                   (AND (NOT BIT-VECTOR) (NOT BASE-STRING) ARRAY) BIT-VECTOR BASE-STRING))
         (t3 (bdd 'CONCATENATED-STREAM))
         (t2 (bdd `(or ,@decomp)))
         (t4 (bdd-and-not t3 t2)))

    (dolist (t1 decomp)
      (let ((bdd1 (bdd t1)))
        (format t "0: ~A~%" t1)
        (dolist (f (list #'bdd-and #'bdd-and-not #'(lambda (a b) (bdd-and-not b a))))
          (let ((t5 (funcall f bdd1 t4)))
            (format t "  ~A~%" t5)
            (format t "    ~A~%" (if (bdd-empty-type t5) nil 'not-nil))))))))

(defun types/perf-bdd ()
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function control-error division-by-zero error)))
    (setf all-types (sort all-types #'string<))
    (bdd-with-new-hash
     (lambda ()
       
       (let ((n 1)
             (testing-types (list (pop all-types))))
         (flet ((test1 (types &aux sorted)
                  (format t "~A~%" (car types))
                  (let ((t1 (get-internal-run-time))
                        (t2 (progn (setf sorted (bdd-decompose-types types))
                                   (get-internal-run-time))))
                    (format t "   ~D ~D ~F~%"
                            n
                            (length sorted)
                            (/ (- t2 t1) internal-time-units-per-second))
                    (incf n))))
           (loop :while testing-types
                 :do (progn (test1 testing-types)
                            (push (pop all-types) testing-types)))))))))

(defclass A () ())
(defclass B () ())
(defclass C () ())
(defclass D () ())
(defclass E () ())
(defclass F () ())
(defclass G () ())

(deftype non-number () `(not number))
(deftype non-integer () `(not integer))
(define-test type/bdd-reduce
  ;; there are six cases to test

  ;; 1) disjoint on left
  ;;  (number (string nil t) nil)
  ;;  --> (number t nil)
  (assert-true (equal (bdd-serialize
                       (bdd-node 'number
                                 (bdd-node 'string nil t)
                                 nil))
                      '(number t nil)))
        
  ;; 2) disjoint on right of negative type
  (assert-true (equal (bdd-serialize
                       (bdd-node 'non-number
                                 nil
                                 (bdd-node 'string nil t)))
                      '(non-number nil t)))
                      
  ;; 3) subtype on right
  (assert-true (equal (bdd-serialize
                       (bdd-node 'integer
                                 (bdd-node 'number t nil)
                                 nil))
                      '(integer t nil)))

  ;; 4) subtype on left of negative type
  (assert-true (equal (bdd-serialize
                       (bdd-node 'non-number
                                 (bdd-node 'integer nil t)
                                 nil))
                      '(non-number t nil)))
                      

  ;; 5) supertype on left
  (assert-true (equal (bdd-serialize
                       (bdd-node 'integer
                                 (bdd-node 'number t nil)
                                 nil))
                      '(integer t nil)))

  ;; 6) supertype on right of negative type
  (assert-true (equal (bdd-serialize
                       (bdd-node 'non-integer
                                 nil
                                 (bdd-node 'number t nil)))
                      '(non-integer nil t))))


(defun find-decomposition-discrepancy (&optional (type-specs '(array-rank array-total-size bignum bit
                                                               complex fixnum float float-digits
                                                               float-radix integer number ratio rational real
                                                               char-code ;; char-int
                                                               double-float ;; long-float
                                                               unsigned-byte)))
  (labels ((recure ( type-specs)
             (when (cdr type-specs)
               (recure (cdr type-specs)))
             (format t "~%~%~%n = ~D~%~%~%~%" (length type-specs))
             (let* ((bdd-types (bdd-decompose-types type-specs))
                    (def-types (decompose-types type-specs))
                    (common (intersection bdd-types def-types :test #'equivalent-types-p))
                    (bdd-left-over (set-difference bdd-types common :test #'equivalent-types-p))
                    (def-left-over (set-difference def-types common :test #'equivalent-types-p)))
               (unless (= (length def-types)
                          (length bdd-types))
                 (format t "n=~D bdd=~D  def=~D~%" (length type-specs) (length bdd-types) (length def-types))
                 (format t " given  :~A~%" type-specs)
                 (format t " common :~A~%" common)
                 (format t "    bdd :~A~%" bdd-left-over)
                 (format t "    def :~A~%" def-left-over)
                 (dolist (com common)
                   (dolist (types (list bdd-left-over def-left-over))
                     (dolist (spec types)
                       (when (subtypep spec com)
                         (format t " ~A <: ~A~%" spec com))
                       (when (subtypep com spec)
                         (format t " ~A <: ~A~%" com spec)))))
                 (format t "checking calculated bdd types~%")
                 (lisp-types::check-decomposition type-specs bdd-types)
                 (format t "checking calculated def types~%")
                 (lisp-types::check-decomposition type-specs def-types)
                 (return-from find-decomposition-discrepancy nil)
                 ))))
    (recure type-specs)))
