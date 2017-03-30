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

(let ((lisp-types-test (find-package  :lisp-types.test))
      (lisp-types (find-package  :lisp-types)))
  (do-symbols (name :lisp-types)
    (when (and (eq lisp-types (symbol-package name))
               (not (find-symbol (symbol-name name) lisp-types-test)))
      (format t "3 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

;(shadow-package-symbols)
;;(do-symbols (name :lisp-types)
;;  (shadowing-import name :lisp-types.test))


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

  
(define-test test/certain-reductions
  (assert-true (bdd '(or (and integer (not string)) (and string (not integer)))))
  (assert-false (bdd-to-dnf (bdd-and-not (bdd 'integer) (bdd 'number)))))

(define-test type/bdd-sample-a
  (assert-false (set-exclusive-or (bdd-decompose-types '(unsigned-byte bit fixnum rational number float))
                                  (decompose-types     '(unsigned-byte bit fixnum rational number float))
                                  :test #'equivalent-types-p)))

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
  (let* ((decomp '((AND (NOT BIT) ARRAY-RANK) BIT (AND (NOT CHAR-INT) ARRAY-TOTAL-SIZE)
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
        (dolist (f (list #'bdd-and #'bdd-and-not #'(lambda (a b) (bdd-and-not b a))))
          (let ((t5 (funcall f bdd1 t4)))
            (if (bdd-empty-type t5) nil 'not-nil)))))))

(defun types/perf-bdd ()
  (declare (notinline sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function control-error division-by-zero error
                                                char-code base-char)))
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
;; (defclass ABCDE (A B C D E) ())

(define-test type/reduce-c
  (assert-true (equal (reduce-lisp-type '(OR (NOT A) B))
                      '(not a))))

(deftype non-number () `(not number))
(deftype non-integer () `(not integer))
(define-test type/bdd-reduce
  (bdd-with-new-hash
   (lambda ()

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
                      '(non-integer nil t))))))

(define-test test/bdd-numbers
  (bdd-with-new-hash
   (lambda ()

  (assert-true (types/cmp-perfs :limit 15 :decompose 'lisp-types::bdd-decompose-types :types (valid-subtypes 'number))))))


(define-test test/bdd-cmp
  (bdd-with-new-hash
   (lambda ()

  ;; =
  (assert-true (eq '= (bdd-cmp 'a 'a)))
  (assert-true (eq '= (bdd-cmp "a" "a")))
  (assert-true (eq '= (bdd-cmp 1 1)))
  (assert-true (eq '= (bdd-cmp 1.0 1.0)))
  (assert-true (eq '= (bdd-cmp 1/2 1/2)))
  (assert-true (eq '= (bdd-cmp nil nil)))
  (assert-true (eq '= (bdd-cmp '(a 1 1.0) '(a 1 1.0))))

  ;; <
  (assert-true (eq '< (bdd-cmp "CL-USER" "KEYWORD")))
  (assert-true (eq '< (bdd-cmp 'CL-USER::x :x)))
  (assert-true (eq '< (bdd-cmp '(a b c) '(a b c d))))
  (assert-true (eq '< (bdd-cmp '(a 1 c) '(a 2 c d))))
  (assert-true (eq '< (bdd-cmp '(a 1 c d) '(a 2 c))))
  (assert-true (eq '< (bdd-cmp 'string 'symbol)))
  ;; (assert-true (eq '< (bdd-cmp "string" 'symbol)))
  (assert-true (eq '< (bdd-cmp 'cons 'null)))
  (assert-true (eq '< (bdd-cmp nil '(a))))
  (assert-true (eq '< (bdd-cmp 1/3 1/2)))

  ;; >
  (assert-true (eq '> (bdd-cmp "KEYWORD" "CL-USER")))
  (assert-true (eq '> (bdd-cmp :x 'CL-USER::x)))
  (assert-true (eq '> (bdd-cmp '(a b c d) '(a b c))))
  (assert-true (eq '> (bdd-cmp '(a 2 c d) '(a 1 c))))
  (assert-true (eq '> (bdd-cmp '(a 2 c) '(a 1 c d))))
  (assert-true (eq '> (bdd-cmp 'symbol 'string)))
  ;; (assert-true (eq '> (bdd-cmp 'symbol "string")))
  (assert-true (eq '> (bdd-cmp 'null 'cons)))
  (assert-true (eq '> (bdd-cmp '(a) nil)))
  (assert-true (eq '> (bdd-cmp 1/2 1/3)))
  )  ))
                   
(define-test test/bdd-type-p
  (bdd-with-new-hash
   (lambda ()
     (assert-false (bdd-type-p  t (bdd '(or (and sequence (not array))
                                         number
                                         (and (not sequence) array)))))
     (assert-true (bdd-type-p  3 (bdd '(or (and sequence (not array))
                                        number
                                        (and (not sequence) array))))))))


(define-test test/bdd-dnf
  (bdd-with-new-hash
   (lambda ()
     (assert-true (member 'number (bdd-to-dnf (bdd '(or (and sequence (not array))
                                                     number
                                                     (and (not sequence) array))))))
     (assert-false (member '(and number) (bdd-to-dnf (bdd '(or (and sequence (not array))
                                                            number
                                                            (and (not sequence) array)))) :test #'equal)))))

(defclass Z1 () ())
(defclass Z2 () ())
(defclass Z3 () ())
(defclass Z4 () ())
(defclass Z5 () ())
(defclass Z12345 (Z1 Z2 Z3 Z4 Z5) ())
(defclass Z54321 (Z5 Z4 Z3 Z2 Z1) ())


(defun int-to-boolean-expression (n vars)
  (let ((num-vars (length vars)))
    (let ((max-n (expt 2 (expt 2 num-vars))))
      (assert (< n max-n) (n vars)
              "N=~D must be less than ~D for ~D variables=~A"
              n max-n num-vars vars))
    (flet ((gen-min-term (i)
             ;; interpret the given I as a bit-mask
             ;; and generate an (AND ...) expression
             ;; the arguments of AND are the symbols in order in VAR
             ;; either as is or wrapped in (NOT ...)
             ;; e.g. if VAR='(a b), then 2 with bitmask 10 -> (and A (not B))
             ;; bits from right to left correspond to variables from left to rith
             (prog1
                 (when (oddp n)
                   (list (cons 'and (mapcar (lambda (var)
                                              (prog1 (if (oddp i)
                                                         var
                                                         `(not ,var))
                                                (setf i (ash i -1)))) vars))))
               (setf n (ash n -1)))))
      (cons 'or (loop for i from 0 to (expt 2 num-vars)
                      nconc (gen-min-term i))))))
    

(defun random-boolean-combination (vars)
  ;; vars is a list of symbols
  (int-to-boolean-expression (random (expt 2 (expt 2 (length vars))))
                             vars))

(defun measure-bdd-size (vars num-samples)
  (let ((hash (make-hash-table)))
    (dotimes (_ num-samples)
      (bdd-with-new-hash (lambda ()
                           (incf (gethash
                                  (bdd-count-nodes
                                   (bdd (random-boolean-combination vars)))
                                  hash
                                  0)))
                         :verbose nil))
    (let (a-list sum)
      (declare (notinline sort))
      (maphash (lambda (&rest args)
                 (push args a-list))
               hash)
      (setf sum (reduce #'+ (cdr a-list) :initial-value (cadr (car a-list)) :key #'cadr))
      ;;
      (mapcar (lambda (pair)
                (declare (type (cons integer (cons integer)) pair))
                (list (car pair)
                      (float (/ (cadr pair) sum))
                      ))
              (sort a-list #'< :key #'car)))))
  
(defun measure-bdd-sizes (vars num-samples)
  (maplist (lambda (vars)
             (list (length vars)
                   (measure-bdd-size vars
                                     (min (expt 2 (expt 2 (length vars)))
                                          num-samples))))
           vars))

(defun latex-measure-bdd-sizes (stream vars num-samples)
  (cond
    ((null stream)
     (with-output-to-string (str)
       (latex-measure-bdd-sizes str vars num-samples)))
    (t
     (let (legend
           (colors '("red" "blue" "goldenrod" "lavender" "greeny" "dark-cyan"))
           (data (measure-bdd-sizes vars num-samples)))
       (format stream "\\begin{tikzpicture}~%")
       (format stream "\\begin{axis}[xlabel=BDD Size, xmajorgrids, xminorgrids, ylabel=Probability, legend style={font=\\tiny}, label style={font=\\tiny}]~%")

       (dolist (pair data)
         (destructuring-bind (num-vars coordinates) pair
           (push (format nil "Size with ~D variables" num-vars) legend)
           (format stream "\\addplot[color=~A] coordinates {~%"
                   (or (pop colors) "black"))
           (dolist (xy coordinates)
             (format stream "  (~D,~A)~%" (car xy) (cadr xy)))
           (format stream "};~%")))
       (format stream "\\legend{")
       (let ((first t))
         (dolist (label (reverse legend))
           (unless first
             (format stream ","))
           (format stream "~S" label)
           (setf first nil)))
       (format stream "}~%")
       (format stream "\\end{axis}~%")
       (format stream "\\end{tikzpicture}~%")))))

;; (with-open-file (stream "/Users/jnewton/newton.16.edtchs/src/bdd-distribution.ltxdat"
;;                         :direction :output :if-exists :supersede)
;;   (sb-ext::gc :full t)
;;   (latex-measure-bdd-sizes stream '(Z1 Z2 Z3 Z4 Z5) 4000))
