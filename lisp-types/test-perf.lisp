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


(defun compare/results (all-results &aux (good-results (setof res all-results
                                                         (and res
                                                              (null (getf res :time-out)))))
                                      (good-decomp (mapcar (lambda (plist) (getf plist :decompose)) good-results)))
  ;; results is a list of plists
  ;; each plist has one of two forms
  ;;  keys: (:types :given :decompose :time-out)
  ;;        or
  ;;        (:types :given :calculated :decompose :value :time)
  ;;   :value designates a list of calculated types according to the algorithm :decompose
  ;;   This function compare/results assures that the list of types is the same for each algorithm
  ;;   Ignoring ones which timed out, i.e., :time-out exists in the plist.
  ;;  If a difference is found, an attempt is made to find a smaller input list which also results
  ;;  in different types being calculated
  (labels ((equiv-type-sets (set1 set2)
             (and (= (length set1) (length set2))
                  (bdd-with-new-hash
                   (lambda () (or (null (set-exclusive-or set1 set2 :test #'%equal))
                                  (let ((bdd-set1 (bdd `(or ,@set1)))
                                        (bdd-set2 (bdd `(or ,@set2))))
                                  (and (eq *bdd-false* (bdd-and-not bdd-set1 bdd-set2))
                                       (eq *bdd-false* (bdd-and-not bdd-set2 bdd-set1)))))))))
           (%equal (t1 t2)
             (or (bdd-type-equal (bdd t1) (bdd t2))
                 (equivalent-types-p t1 t2)))
           (compare (res1 res2)
             (cond ((equiv-type-sets (getf res1 :value) (getf res2 :value)))
                   (t
                    (find-small-difference res1 res2))))
           (touching-pairs (types)
             (loop for tail on types
                   nconc (loop for type2 in (cdr types)
                               with type1 = (car types)
                               if (not (subtypep `(and ,type1 ,type2) nil))
                                 collect (list type1 type2))))
           (find-small-difference (res1 res2)
             (let ((*package* (find-package "KEYWORD")))
               (format t "found difference given=~D ~A=~D ~A=~D~%"
                       (length (getf res1 :types))
                       (getf res1 :decompose)
                       (length (getf res1 :value))
                       (getf res2 :decompose)
                       (length (getf res2 :value)))
               (let* ((smaller (find-smaller (getf res1 :types) (getf res1 :decompose) (getf res2 :decompose)))
                      (v1 (funcall (getf res1 :decompose) smaller))
                      (v2 (funcall (getf res2 :decompose) smaller))
                      (o1 (touching-pairs v1))
                      (o2 (touching-pairs v2))
                      (v1-v2 (bdd-and-not (bdd `(or ,@v1)) (bdd `(or ,@v2))))
                      (v2-v1 (bdd-and-not (bdd `(or ,@v2)) (bdd `(or ,@v2)))))

                 (dolist (pair o1)
                   (warn "~A touching pair: ~A~%" (getf res1 :decompose) pair))
                 (dolist (pair o2)
                   (warn "~A touching pairs: ~A~%" (getf res2 :decompose) pair))

                 (let ((msg (format nil "given=~A calculated~%   ~A=[~D]~A~%   vs ~A=[~D]~A~%  a\\b=~A~%  b\\a=~A~%   common=~A~%  a-b=~A~%  b-a=~A"
                                    smaller
                                    (getf res1 :decompose) (length v1) v1 
                                    (getf res2 :decompose) (length v2) v2
                                    (set-difference v1 v2 :test #'%equal)
                                    (set-difference v2 v1 :test #'%equal)
                                    (intersection v1 v2 :test #'%equal)
                                    v1-v2
                                    v2-v1)))
                   (warn msg)
                   (error msg)))))
           (find-smaller (given f1 f2 &aux v1 v2)
             (format t "searching for smaller error than ~S~%" given)
             (let ((ts (exists t1 given
                         (not (equiv-type-sets (setf v1 (funcall f1 (remove t1 given)))
                                               (setf v2 (funcall f2 (remove t1 given))))))))
               (cond
                 (ts
                  (format t "   found smaller difference given=~D~%" (1- (length given)))
                  (find-smaller (remove (car ts) given) f1 f2))
                 (t
                  given))))
           (check-1 (given-types calculated-types decompose-function)
             (when given-types
               (loop :for types :on calculated-types
                     :do (loop :for t2 :in (cdr types)
                               :with t1 = (car types)
                               :with bdd1 = (bdd (car types))
                               :do (let ((*package* (find-package "KEYWORD"))
                                         (bdd2 (bdd t2)))
                                     (unless (bdd-disjoint-types-p bdd1 bdd2)
                                       (dolist (type given-types)
                                         (let ((fewer (remove type given-types :test #'eq)))
                                           (check-1 fewer
                                                    (funcall decompose-function fewer)
                                                    decompose-function)))
                                       (error "Calculated touching types: ~S touches ~S~%Given types ~S~%Calculated: ~S~% Decomp ~S"
                                              t1 t2 given-types calculated-types good-decomp)))))
               (let* ((bdd-given (bdd `(or ,@given-types)))
                      (bdd-calc  (bdd `(or ,@calculated-types))))
                 (let ((*package* (find-package "KEYWORD")))
                   (unless (bdd-type-equal bdd-given bdd-calc)
                     (warn "found problem with ~S" given-types)
                     (dolist (type given-types)
                       (let ((fewer (remove type given-types)))
                         (warn "  checking with ~S" fewer)
                         (check-1 fewer (funcall decompose-function fewer) decompose-function)))
                     (error "Calculated types not equivalent to given types~% given: ~S~% calculated: ~S~% decompose: ~S~% calculated - given: ~S~% given - calculated: ~S"
                            given-types
                            calculated-types
                            decompose-function
                            (bdd-to-dnf (bdd-and-not bdd-calc bdd-given))
                            (bdd-to-dnf (bdd-and-not bdd-given bdd-calc)))))))))
    (when good-results
      (let ((res1 (car good-results)))
        (bdd-with-new-hash
         (lambda ()
           (check-1 (getf res1 :types) (getf res1 :value) (getf res1 :decompose)))))
      
      (dolist (res (cdr good-results))
        (compare (car good-results) res)))))


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
  (types/cmp-perfs :types '((COMMON-LISP:EQL 3)
                                  (COMMON-LISP:MEMBER 1 2 3)
                                  (COMMON-LISP:MEMBER 0 2) COMMON-LISP:NULL
                                  (COMMON-LISP:MEMBER 0 2 3)
                                  (COMMON-LISP:MEMBER 0 1 2 3))))

(define-test disjoint-cmp-i
  (setf *perf-results* nil)
  (types/cmp-perfs :types '(STRING STANDARD-GENERIC-FUNCTION ATOM METHOD SIMPLE-BASE-STRING
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




    




