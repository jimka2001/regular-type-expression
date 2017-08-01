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
          (format t "6 importing name=~A into  :lisp-types.test~%" name)
          (shadowing-import name :lisp-types.test))))

;;(shadow-package-symbols)
;;(do-symbols (name :lisp-types)
;;  (shadowing-import name :lisp-types.test))


(define-test disjoint-cmp-1
  (setf *perf-results* nil)
  (types/cmp-perfs :types '(sb-pcl::SYSTEM-CLASS
                            sb-pcl::SLOT-DEFINITION
                            sb-pcl::EQL-SPECIALIZER) :time-out nil))


(define-test disjoint-cmp-2
  (setf *perf-results* nil)
  (types/cmp-perfs :types '(sb-pcl::SYSTEM-CLASS
                            sb-pcl::STANDARD-SLOT-DEFINITION
                            sb-pcl::EFFECTIVE-SLOT-DEFINITION
                            sb-pcl::FUNCALLABLE-STANDARD-OBJECT
                            sb-pcl::SPECIALIZER
                            sb-pcl::EQL-SPECIALIZER
                            sb-pcl::DIRECT-SLOT-DEFINITION
                            sb-pcl::SLOT-DEFINITION)
                   :time-out 5))

(define-test disjoint-cmp-3
  (setf *perf-results* nil)
  (types/cmp-perfs :types '(SB-PCL:SYSTEM-CLASS
                            SB-MOP:STANDARD-WRITER-METHOD
                            SB-MOP:DIRECT-SLOT-DEFINITION)))


(define-test disjoint-cmp-4
  (setf *perf-results* nil)
  (types/cmp-perfs :types '(SB-PCL:SYSTEM-CLASS
                            SB-MOP:DIRECT-SLOT-DEFINITION
                            SB-MOP:FORWARD-REFERENCED-CLASS
                            SB-MOP:EFFECTIVE-SLOT-DEFINITION
                            SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION
                            SB-MOP:STANDARD-ACCESSOR-METHOD
                            SB-MOP:STANDARD-READER-METHOD
                            SB-MOP:FUNCALLABLE-STANDARD-CLASS
                            SB-MOP:FUNCALLABLE-STANDARD-OBJECT)
                   :time-out 8))

(define-test disjoint-cmp-5
  (setf *perf-results* nil)
  ;; decompose-types-bdd-graph
  (types/cmp-perfs :types '(SB-PCL:SYSTEM-CLASS
                            SB-MOP:STANDARD-ACCESSOR-METHOD
                            SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION)))

(define-test disjoint-cmp-6
  (setf *perf-results* nil)
  (types/cmp-perfs :types '((MEMBER 2 4 5 6 8)
                            (MEMBER 0 5 6 9 10)
                            (MEMBER 0 1 3 4 10)
                            (MEMBER 0 3 5 8 9 10))))

(define-test disjoint-cmp-7
  (setf *perf-results* nil)
  (types/cmp-perfs :types '((MEMBER 1 3 4 5 6 9)
                            (MEMBER 1 5 7 8)
                            (MEMBER 4 7 8 9 10)
                            (MEMBER 3 4 5 7 9)
                            (MEMBER 0 2 3 7 8 10)) ))

(define-test disjoint-cmp-8
  (setf *perf-results* nil)
  (types/cmp-perfs :types '((MEMBER 0 2)
                            (MEMBER 0 1 2)
                            (MEMBER 0 2 4))))

(define-test disjoint-cmp-9
  (bdd-with-new-hash
   (lambda ()
     (assert-test (= 3 (length (bdd-decompose-types '((MEMBER 0 2)
                                                      (MEMBER 0 1 2)
                                                      (MEMBER 0 2 4)))))))))

(define-test disjoint-cmp-a
  (bdd-with-new-hash
   (lambda ()
     (bdd-with-new-hash
      (lambda ()
        (let* ((t1 (bdd '(member 0 2)))
               (t2 (bdd '(member 0 1 2)))
               (t3 (bdd '(member 0 2 4)))
               (bdds (list t1 t2 t3))
               (U (reduce #'bdd-or bdds :initial-value *bdd-false*)))
          (assert-false (eq '= (bdd-cmp '(member  0 2 4) '(member 0 2))))
          (forall x '(0 1 2 4)
            (assert-true (bdd-type-p x U)))
          (assert-false (bdd-type-p 3 U))
          (assert-true (bdd-type-p 0 (bdd-and U t1)))
          (assert-true (bdd-type-p 2 (bdd-and U t1)))
          (assert-true (bdd-type-p 4 (bdd-and-not U t1)))))))))

(define-test disjoint-cmp-b
  (assert-true (= 9 (length (decompose-types-graph
                             '((MEMBER 1 3 4 5 6 9)
                               (MEMBER 1 5 7 8)
                               (MEMBER 4 7 8 9 10)
                               (MEMBER 3 4 5 7 9)
                               (MEMBER 0 2 3 7 8 10)))))))
    
(define-test disjoint-cmp-c
  (assert-true (decompose-types-graph '((MEMBER 0 3 4)
                                        (EQL 2)
                                        (MEMBER 2 3)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 2 4)
                                        (EQL 4)
                                        (MEMBER 0 1 3 4)
                                        (MEMBER 1 2 3)
                                        (MEMBER 1 4)
                                        (EQL 3)))))


(define-test disjoint-cmp-d
  (assert-true (decompose-types-graph '((EQL 2)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 2 4)
                                        (MEMBER 0 1 3)
                                        (EQL 0)
                                        (MEMBER 1 3 4)))
               ))


(define-test disjoing-cmp-e
  (assert-true (decompose-types-graph '((MEMBER 1 4)
                                        (EQL 2)
                                        (MEMBER 0 3)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 3 4)
                                        (MEMBER 0 1)
                                        (MEMBER 0 1 4)
                                        (MEMBER 0 1 3 4)))))

(define-test disjoing-cmp-f
  (assert-true (decompose-types-graph '((MEMBER 1 4)
                                        (MEMBER 1 2 4)
                                        (MEMBER 0 1 3 4)
                                        (MEMBER 0 3)
                                        (MEMBER 0 1 4)
                                        (MEMBER 1 3)
                                        (EQL 4)
                                        (MEMBER 0 3 4)
                                        (MEMBER 1 3 4)
                                        (EQL 2)
                                        (MEMBER 2 3)
                                        (MEMBER 0 1 3)
                                        (MEMBER 0 1 2 4)
                                        (MEMBER 2 3 4)
                                        (MEMBER 0 2 4)
                                        ))))

(define-test disjoint-cmp-g
  (setf *perf-results* nil)
  (types/cmp-perfs :types '((MEMBER 0 3 4)
                            (EQL 3)
                            (MEMBER 1 4)
                            (MEMBER 1 2 3)
                            (MEMBER 0 2)
                            NULL
                            (MEMBER 0 2 3)
                            (MEMBER 0 1 2 3))))

(define-test disjoint-cmp-h
  (setf *perf-results* nil)
  (types/cmp-perfs :limit 5
                   :types '((COMMON-LISP:EQL 3)
                            (COMMON-LISP:MEMBER 1 2 3)
                            (COMMON-LISP:MEMBER 0 2) COMMON-LISP:NULL
                            (COMMON-LISP:MEMBER 0 2 3)
                            (COMMON-LISP:MEMBER 0 1 2 3))))

(define-test disjoint-cmp-i
  (setf *perf-results* nil)
  (types/cmp-perfs :limit 5
                   :types '(STRING STANDARD-GENERIC-FUNCTION ATOM METHOD SIMPLE-BASE-STRING
                            SEQUENCE COMPLEX STANDARD-OBJECT STANDARD-METHOD)))

(define-test disjoint-cmp-j
  (setf *perf-results* nil)
  (types/cmp-perf :types '((MEMBER 0 1 2 4 5 6 8 9 10) (MEMBER 1 2 4 6 8)
                           (MEMBER 1 2 3 5 6 7 8 10) (MEMBER 1 5 6 7 9 10) (MEMBER 0 1 6 7 8 10)
                           (MEMBER 0 1 2 3 5 6 10) (MEMBER 0 1 3 4 5 6 8 9 10)
                           (MEMBER 0 3 5 6 8) (MEMBER 0 1 2 3 6 7 9) (MEMBER 0 2 4 8 10)
                           (MEMBER 0 1 5 9) (MEMBER 0 1 2 4 8) (MEMBER 1 3 5 6 8 9 10)
                           (MEMBER 3 5 7 9) (MEMBER 5 6 7 8 9 10) (MEMBER 0 4 6 7 8 9)
                           (MEMBER 1 4 7 9) (MEMBER 0 3 4 7 8 10) (MEMBER 0 1 4 5 7 8)
                           (MEMBER 0 2 4 5 7 9 10) (MEMBER 0 9 10))))

(define-test disjoint-cmp-k
  (let ((type-specifiers
          '((and arithmetic-error reader-error structure-class (not style-warning))
            (and arithmetic-error reader-error (not structure-class) (not style-warning))
            (and arithmetic-error (not reader-error) (not structure-class) style-warning)
            (and arithmetic-error (not reader-error) structure-class style-warning)
            (and (not arithmetic-error) condition (not reader-error) structure-class (not style-warning))
            (and (not arithmetic-error) reader-error structure-class (not style-warning))
            (and arithmetic-error (not reader-error) structure-class (not style-warning))
            (and (not arithmetic-error) (not reader-error) structure-class style-warning)
            (or (and condition (not reader-error)) (and reader-error (not style-warning)))
            (and (not condition) structure-class (not style-warning))
            (and arithmetic-error (not reader-error) (not structure-class) (not style-warning))
            (or (and (not reader-error) warning) (and reader-error (not style-warning) warning))
            (or (and (not reader-error) stream-error) (and reader-error (not style-warning)))
            (and (not arithmetic-error) reader-error (not structure-class) (not style-warning))
            (and (not arithmetic-error) (not reader-error) (not structure-class) style-warning))))
    (%decompose-types-bdd-graph type-specifiers 
                                :sort-nodes #'(lambda (graph)
                                                (declare (notinline sort))
                                                (sort graph #'< :key #'count-parents-per-node))
                                :sort-strategy "TOP-TO-BOTTOM"
                                :inner-loop :operation
                                :do-break-sub :strict
                                :do-break-loop t)))



(define-test disjoint-cmp-l
  (let ((type-specifiers
          '(CONDITION RESTART RATIONAL CONS RATIO READER-ERROR STRUCTURE-CLASS
            SYNONYM-STREAM ARITHMETIC-ERROR CHAR-CODE WARNING FLOAT-RADIX
            SIMPLE-BIT-VECTOR STREAM-ERROR ARRAY STYLE-WARNING)))
    (%decompose-types-bdd-graph type-specifiers 
                                :sort-nodes #'(lambda (graph)
                                                (declare (notinline sort))
                                                (sort graph #'< :key #'count-parents-per-node))
                                :sort-strategy "TOP-TO-BOTTOM"
                                :inner-loop :operation
                                :do-break-sub :strict
                                :do-break-loop t)))
                                



















;; (lisp-types.test::sort-results "/Users/jnewton/newton.16.edtchs/src/member.sexp" nil)
















(defun perf-test-1 (&key (size 11))
  (bdd-with-new-hash
   (lambda (&aux (type-specifiers (lisp-types::choose-randomly (loop :for name being the external-symbols in "SB-PCL"
                                                                     :when (find-class name nil)
                                                                       :collect name) size)))
     (%decompose-types-bdd-graph type-specifiers
                                 :sort-nodes (lambda (graph)
                                               (declare (notinline sort))
                                               (sort graph #'< :key
                                                     #'count-connections-per-node))
                                 :sort-strategy  "INCREASING-CONNECTIONS"
                                 :inner-loop :node
                                 :do-break-sub :relaxed
                                 :do-break-loop nil))))

(defun read-trace (stream)
  (let (pending)
    (labels ((next-token ()
               (if pending
                   (pop pending)
                   (let (chars)
                     (do ((c (read-char stream) (read-char stream nil 'the-end)))
                         ((char= #\: c) chars)
                       (when (digit-char-p c)
                         (push c chars))))))
             (un-read (token)
               (push token pending))
             (next-expr ()
               (read stream nil nil))
             (read-suffix (prefix)
               (list :prefix prefix
                     :fname (next-expr)
                     :returned (next-expr)
                     :output (next-expr)))
             (read-one-node ()
               (let* ((prefix (next-token))
                      (call (next-expr))
                      (token (next-token))
                      sub-nodes)
                 (while (not (equal token prefix))
                   (un-read token)
                   (push (read-one-node) sub-nodes)
                   (setf token (next-token)))
                 
                 `(:sub-nodes ,(reverse sub-nodes)
                   :input ,(cadr call)
                   ,@(read-suffix prefix)))))
      (read-one-node))))

(defun filter-trace (filename)
  "read the printed output traced functions, throw away function calls and returns
if the function returned the same as it was passed as input (according to EQUAL)"
  (labels ((print-trace (node)
             (when node
               (destructuring-bind (&key prefix input fname returned output sub-nodes) node
                 (cond
                   ((equal output input)
                    (mapc #'print-trace sub-nodes))
                   (t
                    (format t "~A ~A~%" prefix (list fname input))
                    (mapc #'print-trace sub-nodes)
                    (format t "~A ~A ~A ~A~%"
                            prefix fname returned output)))))))
    (print-trace (with-open-file (str filename :direction :input)
                   (read-trace str)))))
    
