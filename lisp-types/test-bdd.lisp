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
(defclass Z6 () ())
(defclass Z7 () ())
(defclass Z1234567 (Z1 Z2 Z3 Z4 Z5 Z6 Z7) ())
(defclass Z7654321 (Z7 Z6 Z5 Z4 Z3 Z2 Z1) ())


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
  (let ((hash (make-hash-table))
        (n-vars (length vars))
        (worst-case '(0)))
    (dotimes (try num-samples)
      (sb-ext::gc :full t)
      (bdd-with-new-hash (lambda (&aux (boolean-combo (random-boolean-combination vars))
                                    (bdd (bdd boolean-combo))
                                    (node-count (bdd-count-nodes bdd)))
                           (when (> node-count (car worst-case))
                             (setf worst-case (list node-count (bdd-to-dnf bdd)))
                             (bdd-view bdd)
                             (format t "~D worse case after ~D: ~A~%" n-vars try worst-case))
                           (incf (gethash
                                  node-count
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


(defun map-pairs (f objs)
  (dolist (o1 objs)
    (dolist (o2 objs)
      (unless (eq o1 o2)
        (funcall f o1 o2)))))

(defun bdd-row-sizes (n-vars)
  ;; n-vars is a positive integer
  (let ((data (list (list n-vars 2)))
        (sum 2)
        q)
    (while (> n-vars 1)
      (decf n-vars)
      (setf q (* sum (1- sum)))
      (push (list n-vars (expt 2 n-vars) q
                  (min (expt 2 n-vars)
                              q)) data)
      (incf sum q))
    (push (list 0 1 1) data)
    data))

(defun visit-pairs (f m objects )
  "Visit M pairs of adjacent objects such every element object is visited at least once,
   and no pair is visited twice."
  (declare (type (function (t t) t) f)
           (type integer m)
           (type list objects))
  (let ((need-pairs m)
        (N (length objects)))
    (let ((ptr objects))
      (loop for i from 1 to (min need-pairs (truncate N 2))
            do (progn (decf need-pairs)
                      (funcall f (pop ptr) (pop ptr)))))

    (let ((ptr objects))
      (loop for i from 1 to (min need-pairs (truncate N 2))
            do (progn (decf need-pairs)
                      (apply f (reverse (list (pop ptr) (pop ptr)))))))))


(defun bdd-make-worst-case (vars)
  (let* ((leaves (list *bdd-true* *bdd-false*))
         (size 2)
         (row-num (1- (length vars)))
         (rows (list leaves)))

    ;; build up the bottom
    (while (< (* size (1- size)) (expt 2 row-num))
      (format t "case 1 ~A~%" vars)
      (let (bdds)
        (map-pairs (lambda (o1 o2)
                     (push (bdd-node (car vars) o1 o2) bdds))
                   (reduce #'append rows :initial-value ()))
        (push bdds rows)
        (assert (= (length bdds) (* size (1- size))) (size bdds))
        (incf size (* size (1- size)))
        (pop vars)
        (decf row-num)))

    ;; build the belt with exactly (expt 2 row-num) elements,
    ;; and 2* (expt 2 row-num) arrows.
    ;; so two cases, does the previous row below have enough elements
    ;; to support the arrows?
    (let* ((n row-num)
           (m (expt 2 n))
           (p (length (car rows)))
           (needed m)
           (remaining (- m (* p (1- p))))
           bdds)

      (assert (<= p (* 2 m)) (row-num m p)
              "expecting to create BDD row of ~D elements with ~D connecting to less than ~D elements"
              m (* 2 m) (* 2 m))

      (block create-links-to-n+1
        ;; First construct as many as possible, but not too many nodes pointing to row n+1.
        ;; This will create a maximum of p(p-1) nodes.   If p*(p-1) <= 2^n then this is
        ;; sufficient, otherwise, remaining denotes how many additional need to be created
        ;; in BLOCK  create-remaining.
        (map-pairs (lambda (left right)
                     (cond
                       ((plusp needed)
                        (push (bdd-node (car vars) left right) bdds)
                        (decf needed))
                       (t
                        (return-from create-links-to-n+1))))
                   (car rows)))
      (block create-remaining
        ;; Next we create any remaining nodes that are needed.  This has some effect
        ;; only in the case that p*(p-1) > 2^n, which means that the previous block
        ;; create-links-to-n+1 failed to create 2^n nodes, because row n+1 doesn't
        ;; have enough elements.  So the strategy is to create links to as many
        ;; of the existing nodes row n+2, n+3 ... as necessary.
        (map-pairs (lambda (right left)
                     (cond
                       ((and (member left (car rows))
                             (member right (car rows))))
                       ((plusp remaining)
                        (push (bdd-node (car vars) left right) bdds)
                        (decf remaining))
                       (t
                        (return-from create-remaining))))
                   (reduce #'append rows :initial-value ())))
      (assert (= m (length bdds)) (m n p)
              "failed to create exactly ~D=2^~D nodes for row ~d, created ~D instead"
              m n n (length bdds))
      (push bdds rows)
      (pop vars))
      
    ;; build the top
    (while vars
      (format t "case 3 ~A ~A~%" vars (mapcar #'length rows))
      (let (bdds
            (ptr (car rows)))
        (assert (or (= 1 (length ptr))
                    (evenp (length ptr))) (ptr)
                    "expecting either 1 or even number as length, not ~D" (length ptr))
        (while ptr
          (push (bdd-node (car vars) (pop ptr) (pop ptr)) bdds))
        (push bdds rows))
      (pop vars))
    (cl-user::print-vals (mapcar #'length rows))
    ;; the top row has one item, that element is the worst case bdd for the given variables
    (bdd-view (car (car rows)))
    (car (car rows))
    nil))


(defvar *bdd-6-worst-case*
  (let* ((leafs (list *bdd-true* *bdd-false*))
         (row-6 (let (pairs)
                  (map-pairs (lambda (o1 o2)
                              (push (bdd-node 'Z6 o1 o2) pairs))
                             leafs)
                  pairs))
         (row-5 (let (pairs)
                  (map-pairs (lambda (o1 o2)
                              (push (bdd-node 'Z5 o1 o2) pairs))
                             (append row-6 leafs))
                  pairs))
         ;; build 8 nodes
         (row-4 (let (pairs)
                  (visit-pairs (lambda (right left)
                                 (push (bdd-node 'Z4 right left) pairs))
                               8 row-5)
                  pairs))
         (row-3 (let (pairs (ptr row-4))
                  (assert (evenp (length row-4)))
                  (while ptr
                    (push (bdd-node 'Z3 (pop ptr) (pop ptr)) pairs))
                  pairs))
         (row-2 (let (pairs (ptr row-3))
                  (assert (evenp (length row-3)))
                  (while ptr
                    (push (bdd-node 'Z2 (pop ptr) (pop ptr)) pairs))
                  pairs))
         (row-1 (let (pairs (ptr row-2))
                  (assert (evenp (length row-2)))
                  (while ptr
                    (push (bdd-node 'Z1 (pop ptr) (pop ptr)) pairs))
                  pairs))
         (bdd (car row-1)))
    (bdd-view bdd)
    (bdd-to-dnf bdd)
    bdd))

         
         
                    
                      
                        
                    

                    
         


 (with-open-file (stream "/Users/jnewton/newton.16.edtchs/src/bdd-distribution.ltxdat"
                         :direction :output :if-exists :supersede)
   (sb-ext::gc :full t)
   (latex-measure-bdd-sizes stream '(Z1 Z2 Z3 Z4 Z5 Z6) 4000))
