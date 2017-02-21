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

(defvar *cl-types* '(
                     arithmetic-error                  function            simple-condition           
                     array                             generic-function    simple-error               
                     atom                              hash-table          simple-string              
                     base-char                         integer             simple-type-error          
                     base-string                       keyword             simple-vector              
                     bignum                            list                simple-warning             
                     bit                               logical-pathname    single-float               
                     bit-vector                        long-float          standard-char              
                     broadcast-stream                  method              standard-class             
                     built-in-class                    method-combination  standard-generic-function  
                     cell-error                        nil                 standard-method            
                     character                         null                standard-object            
                     class                             number              storage-condition          
                     compiled-function                 package             stream                     
                     complex                           package-error       stream-error               
                     concatenated-stream               parse-error         string                     
                     condition                         pathname            string-stream              
                     cons                              print-not-readable  structure-class            
                     control-error                     program-error       structure-object           
                     division-by-zero                  random-state        style-warning              
                     double-float                      ratio               symbol                     
                     echo-stream                       rational            synonym-stream             
                     end-of-file                       reader-error        t                          
                     error                             readtable           two-way-stream             
                     extended-char                     real                type-error                 
                     file-error                        restart             unbound-slot               
                     file-stream                       sequence            unbound-variable           
                     fixnum                            serious-condition   undefined-function         
                     float                             short-float         unsigned-byte              
                     floating-point-inexact            signed-byte         vector                     
                     floating-point-invalid-operation  simple-array        warning                    
                     floating-point-overflow           simple-base-string                             
                     floating-point-underflow          simple-bit-vector    ))

(defvar *cl-type-combos*
  (loop for types on *cl-types*
        nconc (loop for t2 in (cdr types)
                 with t1 = (car types)
                 nconc (list t1 `(and ,t1 ,t2) `(or ,t1, t2)))))

(defun call-with-timeout (time-out thunk)
  "TIME-OUT, integer, the wall-time allowed to cal the function THUNK.
THUNK is a 0-ary function returning some type X.
Call the function THUNK, in one thread, and start a 2nd observer thread.  The 2nd thread
is responsible for monitoring the wall time and killing the 1st thread if the TIME-OUT has
passed.
returns a plist, one of the following:
(:wall-time rational :run-time rational :time-out integer) or
(:wall-time rational :run-time rational :value X)"
  (let (th-worker th-observer th-worker-join-failed th-observer-join-failed th-worker-destroyed-observer time-it-error result1 result2
                  (start-run-time (get-internal-run-time))
                  (start-real-time (get-internal-real-time)))
    (flet ((time-it ()
             (handler-bind ((error (lambda (e)
                                     ;; this handler explicitly declines to handle the error
                                     ;; thus the variable TIME-IT-ERROR will be set as a side
                                     ;; effect, and th-observer will be destroyed, and the system will
                                     ;; continue to search for another handler, probably the
                                     ;; debugger.
                                     (setf time-it-error e)
                                     (when th-observer
                                       (warn "killing thread ~A because of error ~A" th-observer e)
                                       (ignore-errors (bordeaux-threads:destroy-thread th-observer))))))
               (let* ((run-time-t1 (get-internal-run-time))
                      (s2 (funcall thunk))
                      (run-time-t2 (get-internal-run-time)))
                 (setf result1
                       (list :wall-time (/ (- (get-internal-real-time) start-real-time) internal-time-units-per-second)
                             :run-time (/ (- run-time-t2 run-time-t1) internal-time-units-per-second)
                             :value s2))
                 (when th-observer
                   (setf th-worker-destroyed-observer
                         (bordeaux-threads:destroy-thread th-observer)))))))
      (cond
        (time-out
         (setf th-observer
               (bordeaux-threads:make-thread
                (lambda (&aux elapsed (real-time (get-internal-real-time)) (run-time (get-internal-run-time)))
                  (block waiting
                    (dotimes (i time-out)
                      (setf run-time (get-internal-run-time))
                      (setf real-time (get-internal-real-time))
                      (when (plusp (setf elapsed (/ (- real-time start-real-time) internal-time-units-per-second)))
                        (when (> elapsed time-out)
                          (return-from waiting)))
                      (sleep 2)))
                  (setf result2 (list :wall-time (/ (- real-time start-real-time) internal-time-units-per-second)
                                      :run-time  (/ (- run-time start-run-time) internal-time-units-per-second)
                                      :time-out time-out))
                  (format t "killing thread ~A~%" th-worker)
                  (bordeaux-threads:destroy-thread th-worker))
                :name "th-observer stop-watch"))
         (setf th-worker (bordeaux-threads:make-thread #'time-it :name "th-worker handle thunk"))
         (handler-case (bordeaux-threads:join-thread th-worker)
           (SB-THREAD:JOIN-THREAD-ERROR (e)
             (setf th-worker-join-failed e)
             nil))
         (handler-case (bordeaux-threads:join-thread th-observer)
           (SB-THREAD:JOIN-THREAD-ERROR (e)
             (setf th-observer-join-failed e)
             nil)))
      (t
       (time-it))))
    (assert (typep (or result1 result2) 'cons)
            (th-worker th-observer th-worker-destroyed-observer time-it-error th-worker-join-failed th-observer-join-failed result1 result2 time-out))
    (the cons (or result1 result2))))

(defvar *perf-results* nil)
(defun types/cmp-perf (&key types (decompose 'bdd-decompose-types) (time-out 15) &aux (f (symbol-function decompose)))
  (declare (type list types)
           (type symbol decompose)
           (type function f))
  (setf types (remove nil types))
  (cond
    ((null types)
     nil)
    ((exists plist *perf-results*
       (and (eq decompose (getf plist :decompose))
            (equal types (getf plist :types))))
     (format t "skipping duplicate ~A ~A~%" decompose types)
     nil)
    (t
     (sb-ext:gc :full t)
     (let ((result (call-with-timeout time-out
                                      (lambda ()
                                        (funcall f types)))))
       (assert (or (typep (getf result :run-time) 'number )
                   (getf result :time-out)) (result))
       (destructuring-bind (&key time-out (run-time 0) (wall-time 0) value) result
         (declare (type (or null fixnum) time-out)
                  (type list value)
                  (type number run-time wall-time))
         (push
          (cond
            (time-out
             (format t "timed-out: ~D~%" time-out)
             (format t "given: ~D~%~%" (length types))
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :time-out time-out))
            (t
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :time (/ run-time 1.0)
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :calculated (length value)
                   ;; :value value
                   )))
          *perf-results*))
       (car *perf-results*)))))

(defun get-all-types ()
  (set-difference (valid-subtypes t) '(t nil class built-in-class )))


(defun valid-subtypes (super)
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (and (valid-type-p sym)
                 (subtypep sym super))
	(push sym all-types)))
    all-types))


(defvar *decomposition-functions*
  `(decompose-types
    decompose-types-sat
    decompose-types-graph
    bdd-decompose-types
    decompose-types-bdd-graph
    ,@*decompose-fun-names*))

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

(defun types/cmp-perfs (&key
                          (suite-time-out (* 60 10))
                          (randomize nil)
                          (summary nil)
                          (types (valid-subtypes 'number))
                          (limit 15)
                          (time-out nil)
                          tag
                          (decompose *decomposition-functions*)
                          (sorted-name "/dev/null")
                          (sexp-name "/dev/null")
                          (json-name "/dev/null")
                          (gnuplot-name "/dev/null")
                          (png-name "/dev/null")
                          (dat-name "/dev/null"))
  (declare (type (or list (and symbol (satisfies symbol-function))) decompose))
  (let ((*package* (find-package "KEYWORD"))
        (time-out-time (+ (get-universal-time) suite-time-out))
        delayed)
    (labels ((handle (thunk)
               (if randomize
                   (push thunk delayed)
                   (funcall thunk)))
             (log-data ()
               (print-latex dat-name
                            :json-name json-name
                            :sexp-name sexp-name
                            :gnuplot-name gnuplot-name
                            :png-name png-name
                            :sorted-name sorted-name
                            :summary summary
                            :time-out time-out))
             (run (types)
               (cond
                 ((null types))
                 ((> (length types) limit)
                  (run (cdr types)))
                 (types
                  (run (cdr types))
                  (dolist (f (if (listp decompose)
                                 decompose
                                 (list decompose)))
                    (declare (type symbol f))
                    (handle 
                     (lambda (&aux (f f) results)
                       (when (> (get-universal-time) time-out-time)
                         (log-data)
                         (return-from types/cmp-perfs 'timed-out))
                       (when (or (not (eq f 'decompose-types))
                                 (> 10 (length types)))
                         (format t "    date:  ~A~%" (multiple-value-list (get-decoded-time)))
                         (format t "function:  ~A~%" f)
                         (format t "   tag:    ~A~%" tag)
                         (format t "   limit:  ~D~%" (min limit (length types)))
                         (push (types/cmp-perf :types types :decompose f :time-out time-out)
                               results)
                         ;;(compare/results results)
                         ))))))))
      (run types)
      (when randomize
        (let* ((c (length delayed))
               (len c))
          (setf delayed (shuffle-list delayed))
          (while delayed
            (format t "countdown ~D/~D  ~D~%" (decf c) len (- time-out-time (get-universal-time)))
            (funcall (pop delayed)))))
      (log-data)))
  t)

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


(defun count-pairs (data predicate)
  (let ((c 0))
    (loop :for tail :on data
          :do (loop :for d2 :in tail
                    :with d1 = (car tail)
                    :do (when (funcall predicate d1 d2)
                          (incf c))))
    c))

(defun group-by (data &key key (test #'eql))
  (declare (type list data)
           (type (function (t) t) key)
           (type (function (t t) t) test))
  (let ((hash (make-hash-table :test test)))
    (dolist (item data)
      (push item (gethash (funcall key item) hash nil)))
    (loop for key being the hash-keys of hash
          collect (list key (gethash key hash)))))

;; (create-gnuplot "/Users/jnewton/newton.16.edtchs/src/cl-types.sexp"
;;                 "/Users/jnewton/newton.16.edtchs/src/cl-types.gnu"
;;                 "/Users/jnewton/newton.16.edtchs/src/cl-types.png")

(defun create-gnuplot (sexp-file gnuplot-file png-filename)
  (let ((content (with-open-file (stream sexp-file :direction :input)
                   (read stream nil nil))))
    (with-open-file (stream gnuplot-file :direction :output :if-exists :supersede)
      (destructuring-bind (&key summary data) content
        (format stream "# summary ~A~%" summary)
        (format stream "set term png~%")
        (format stream "set logscale xy~%")
        (format stream "set output ~S~%" png-filename)
        (format stream "plot \"-\" using 1:2 with lines~%")
        (dolist (item data)
          (destructuring-bind (&key decompose given calculated time) item
            (declare (notinline sort))
            (format stream "# ~A~%" decompose)
            (dolist (pair (sort (mapcar (lambda (given calculated time)
                                          (list (* given calculated) time))
                                        given calculated time)
                                #'< :key #'car))
              (destructuring-bind (x y) pair
                (format stream "~A ~A~%" x y)))
            (format stream "~%~%")))
            
        (format stream "end~%")))
    (sb-ext:run-program "gnuplot" (list gnuplot-file)
                        :search t)))

(defun append-tail (l1 l2 value)
  (append l1 (mapcar (constantly value) (nthcdr (length l1) l2))))

(append-tail '(a b c d) '(0 0 0 0 1 2 3 4 5 6 7) -1)
(append-tail '(0 0 0 0 1 2 3 4 5 6 7) '(a b c d) -1)

(defun integral (xys)
  "given a list of xy pairs (car cadr) calculate the area under the curve formed by the trapizoids.
the list of xys need not be already ordered."
  (let ((acc 0.0))
    (mapl (lambda (xys-tail)
            (destructuring-bind ((x1 y1) &optional ((x2 y2) '(nil nil)) &rest tail) xys-tail
              (when tail
                ;; increment by trapizoid area
                (incf acc (* (- x2 x1) (- y2 y1))))))
          (sort (copy-list xys) (lambda (p1 p2 &aux (x1 (car p1)) (x2 (car p2)) (y1 (cadr p1)) (y2 (cadr p2)))
                                  (cond ((= x1 x2)
                                         (< y1 y2))
                                        (t
                                         (< x1 x2))))))
    acc))

(defun getter (field)
  (lambda (obj) (getf obj field)))

;; (reduce (lambda (string num) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")
(defun statistics (data)
  (destructuring-bind (&key summary sorted) data
    (let ((integrals (mapcar (getter :integral) sorted)))
      (destructuring-bind (&key count sum) (reduce (lambda (accum this)
                                                     (destructuring-bind (&key count sum) accum
                                                       (list :count (1+ count) :sum (+ this sum)))) integrals
                                                       :initial-value '(:count 0 :sum 0))
        (labels ((sqr (x) (* x x))
                 (stdev (count mean data)
                   (sqrt (/ (reduce (lambda (sum this)
                                      (+ sum (sqr (- this mean))))
                                    data :initial-value 0.0)
                            count))))
          (let* ((mean (/ sum count))
                 (sigma (stdev count mean integrals)))
            (list :summary summary
                  :sigma sigma
                  :data (mapcar (lambda (plist)
                                  (destructuring-bind (&key integral samples decompose) plist
                                    (list :integral integral
                                          :score (/ (- integral mean) sigma)
                                          :samples samples
                                          :decompose decompose)))
                                sorted))))))))
  (destructuring-bind (&key summary sorted) data
    (list :summary summary
          :sorted (mapcar (lambda (plist)
                            (destructuring-bind (&key integral samples decompose) plist
                              ()))
                          sorted))))

(defun sort-results (in out &rest options)
  (declare (notinline sort))
  (cond
    ((typep in '(or pathname string))
     (with-open-file (stream in :direction :input)
       (apply #'sort-results stream out options)))
    ((typep out '(or pathname string))
     (with-open-file (stream out :direction :output :if-exists :supersede)
       (apply #'sort-results in stream options)))
    ((eq out :return)
     ;; (reduce (lambda (num string) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")
     (destructuring-bind (&key summary data) (read in nil nil)
       (let (observed-max-time observed-min-time observed-min-prod observed-max-prod)
         (dolist (plist data)
           (mapc (lambda (given calculated time)
                   (setf observed-max-prod (if observed-max-prod
                                               (max observed-max-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-min-prod (if observed-min-prod
                                               (min observed-min-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-max-time (if observed-max-time
                                               (max observed-max-time time)
                                               time))
                   (setf observed-min-time (if observed-min-time
                                               (min observed-min-time time)
                                               time)))
                 (getf plist :given) (getf plist :calculated) (getf plist :time)))
         (statistics
          (list :summary summary
                :sorted (sort 
                         (loop for plist in data
                               collect (destructuring-bind (&key decompose given calculated time &allow-other-keys) plist
                                         (let ((xys (mapcar (lambda (given calculated time)
                                                              (list (* given calculated) time)) given calculated time)))
                                           (unless (find observed-min-prod xys :key #'car)
                                             (push (list observed-min-prod observed-min-time) xys))
                                           (unless (find observed-max-prod xys :key #'car)
                                             (push (list observed-max-prod observed-max-time) xys))
                                           (list :integral (integral xys)
                                                 :samples (length xys)
                                                 :decompose decompose))))
                         #'< :key (lambda (obj) (getf obj :integral))))))))
    (t
     (let ((*package* (find-package "KEYWORD")))
       (format out "~S~%" (apply #'sort-results in :return options))))))

;; (lisp-types.test::sort-results "/Users/jnewton/newton.16.edtchs/src/member.sexp" nil)



(defun print-json (stream summary)
  (let ((groups (group-by (cdr *perf-results*) :key (lambda (item) (getf item :decompose)))))
    (format stream "{")
    (when summary
      (format stream " \"SUMMARY\" : ~S~%" summary))
    (format stream ", \"DATA\" : [")
    (flet ((print-group (group)
             (destructuring-bind (decompose data) group
               (format stream "{ \"DECOMPOSE\" : ~S" (symbol-name decompose))
               (let ((no-time-out (setof item data
                                    (null (getf item :time-out)))))
                 (dolist (tag `(:given :calculated :time))
                   (format stream "~%,~S : [" (symbol-name tag))
                   (when no-time-out
                     (format stream "~S" (getf (car no-time-out) tag)))
                   (dolist (item (cdr no-time-out))
                     (format stream ", ~A" (getf item tag)))
                   (format stream "]")))
               (format stream "}~%"))))
      (when (car groups)
        (print-group (car groups)))
      (dolist (group (cdr groups))
        (format stream ", ")
        (print-group group)))
    (format stream "]}~%"))
  nil)

(defun print-sexp (stream summary)
  (let ((groups (group-by (cdr *perf-results*) :key (lambda (item) (getf item :decompose)))))
    (format stream "(")
    (when summary
      (format stream " :SUMMARY ~S~%" summary))
    (format stream " :DATA  (")
    (flet ((print-group (group)
             (destructuring-bind (decompose data) group
               (format stream "( :DECOMPOSE ~S" (symbol-name decompose))
               (let ((no-time-out (setof item data
                                    (null (getf item :time-out)))))
                 (dolist (tag `(:given :calculated :time))
                   (format stream "~%~S (" tag)
                   (when no-time-out
                     (format stream "~S" (getf (car no-time-out) tag)))
                   (dolist (item (cdr no-time-out))
                     (format stream " ~A" (getf item tag)))
                   (format stream ")")))
               (format stream ")~%"))))
      (when (car groups)
        (print-group (car groups)))
      (dolist (group (cdr groups))
        (format stream " ")
        (print-group group)))
    (format stream "))~%"))
  nil)

(defun print-latex (stream &rest args &key (summary nil) (sorted-name "/dev/null") (sexp-name "/dev/null") (json-name "/dev/null") (png-name "/dev/null") (gnuplot-name "/dev/null") (include-decompose *decomposition-functions*) &allow-other-keys)
  (etypecase stream
    ((or stream
         (eql t)
         (eql nil))
     (with-open-file (stream json-name :direction :output :if-exists :supersede)
       (print-json stream summary))
     (with-open-file (stream sexp-name :direction :output :if-exists :supersede)
       (print-sexp stream summary))
     (sort-results sexp-name sorted-name)
     (create-gnuplot sexp-name gnuplot-name png-name)
     (format stream "given calculated sum product touching disjoint disjoint*given touching*given new time decompose~%")
     (let ((domain (remove-duplicates *perf-results*
                                      :test #'equal
                                      :key #'(lambda (plist)
                                               (list (getf plist :decompose)
                                                     (getf plist :types))))))
       (dolist (plist domain)
         (destructuring-bind (&key given calculated decompose value types time time-out &allow-other-keys) plist
           (when time-out
             (setf time time-out))
           (unless calculated
             (let ((hit (find-if (lambda (plist)
                                   (equal given (getf plist :given)))
                                 domain)))
               (setf calculated (getf hit :calculated))))
           
           (cond
             ((or (not given) (not calculated)))
             ((< time 0.0011))
             ((= 0 time))
             ((member decompose include-decompose)
              (let ((*print-case* :downcase)
                    (num-disjoint (count-pairs types #'disjoint-types-p))
                    (num-touching (count-pairs types #'(lambda (x y)
                                                         (not (disjoint-types-p x y))))))
                (format stream "~D ~D ~D ~D ~D ~D ~D ~D ~D ~S ~A~%"
                        given calculated
                        (+ given calculated) ;; sum
                        (* given calculated) ;; product
                        num-touching         ;; touching
                        num-disjoint         ;; disjoint
                        (* num-disjoint given) ;; disjoint * given
                        (* num-touching given) ;; touching * given
                        (length (set-difference value types :test #'equivalent-types-p)) ;; new
                        time decompose))))))))
    ((or pathname string)
     (with-open-file (str stream :direction :output :if-exists :supersede)
       (apply #'print-latex str args)))))


(defun test-report (&key types file-name (limit 15) tag (time-out 15) (suite-time-out (* 60 10)))
  "TIME-OUT is the number of seconds to allow for one call to a single decompose function.
SUITE-TIME-OUT is the number of time per call to TYPES/CMP-PERFS."
  (setf *perf-results* nil)
  (let ((type-specifiers (shuffle-list types)))
    (apply #'types/cmp-perfs :types type-specifiers
                             :suite-time-out suite-time-out
                             :randomize t
                             :limit limit
                             :tag tag
                             :summary file-name
                             :time-out time-out
                             :decompose  *decomposition-functions*
                             (when file-name
                               (list
                                :dat-name (format nil "/Users/jnewton/newton.16.edtchs/src/~A.dat" file-name)
                                :png-name  (format nil "/Users/jnewton/newton.16.edtchs/src/~A.png" file-name)
                                :gnuplot-name (format nil "/Users/jnewton/newton.16.edtchs/src/~A.gnu" file-name)
                                :json-name (format nil "/Users/jnewton/newton.16.edtchs/src/~A.json" file-name)
                                :sexp-name (format nil "/Users/jnewton/newton.16.edtchs/src/~A.sexp" file-name)
                                :sorted-name (format nil "/Users/jnewton/newton.16.edtchs/src/~A.sorted" file-name))))))



(defun random-subset-of-range (min max)
  (loop for i from min to max
        when (zerop (random 2))
          collect i))

(defvar *member-types* (remove-duplicates
                        (loop for i from 1 to 25
                              collect (let ((s (random-subset-of-range 0 10)))
                                        (cond ((cdr s)
                                               (cons 'member s))
                                              (s
                                               (cons 'eql s))
                                              (t
                                               'null))))
                        :test #'equal))


(defun big-test-report (&key (suite-time-out (* 60 40)) (time-out 45)  )
  (test-report :limit 22
               :suite-time-out suite-time-out
               :tag "member" :time-out time-out
               :types *member-types*
               :file-name "member")
  (test-report :limit 12 :tag "conditions"
               :time-out time-out
               :suite-time-out suite-time-out
               :types (valid-subtypes 'condition) :file-name "subtypes-of-condition")

  (test-report :limit 23 :tag "numbers" :time-out time-out
               :suite-time-out suite-time-out
               :types (valid-subtypes 'number) :file-name "subtypes-of-number")

  (test-report :limit 13 :tag "numbers and conditions" :time-out time-out
               :suite-time-out suite-time-out
               :types (union (valid-subtypes 'number) (valid-subtypes 'condition))
               :file-name "subtypes-of-number-or-condition")

  (test-report :limit 22 :tag "subtypes of t" :time-out time-out
               :suite-time-out suite-time-out
               :types (valid-subtypes t) :file-name "subtypes-of-t")

  (test-report :limit 18 :tag "SB-PCL types" :time-out time-out
               :suite-time-out suite-time-out
               :types (loop :for name being the external-symbols in "SB-PCL"
                            :when (find-class name nil)
                              :collect name)
               :file-name "pcl-types")
  (test-report :limit 18 :tag "cl-types" :time-out time-out
               :suite-time-out suite-time-out
               :types *cl-types* :file-name "cl-types")
  (test-report :limit 23
               :suite-time-out suite-time-out
               :tag "cl-combos"
               :time-out time-out
               :types (subseq (shuffle-list *cl-type-combos*) 13850) :file-name "cl-combos")
  )
