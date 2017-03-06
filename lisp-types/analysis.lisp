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

(defvar *decomposition-function-descriptors*
  `((:names (decompose-types) :max-num-types 12 :color "blue")
    (:names (decompose-types-sat) :color "dark-cyan")
    (:names (decompose-types-graph) :color "green")
    (:names (bdd-decompose-types) :color "orange")
    (:names ,*decompose-fun-names* :color "violet")
    (:names (decompose-types-bdd-graph) :color "dark-green")))

(defvar *decomposition-functions*
  (set-difference (mapcan (lambda (plist)
                            (copy-list (getf plist :names))) *decomposition-function-descriptors*)
                  *decompose-fun-names*))


(defun types/cmp-perfs (&key
                          (re-run t)
                          (verify nil)
                          (suite-time-out (* 60 10))
                          (randomize nil)
                          (summary nil)
                          (limit 15)
                          (types (choose-randomly (valid-subtypes 'number) limit))
                          (time-out nil)
                          tag
                          (decompose *decomposition-functions*)
                          normalize
                          (sorted-name "/dev/null")
                          (sexp-name "/dev/null")
                          (gnuplot-name "/dev/null")
                          (png-name "/dev/null")
                          (png-normalized-name "/dev/null")
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
               (print-report dat-name
                            :sexp-name sexp-name
                            :gnuplot-name gnuplot-name
                            :png-name png-name
                            :png-normalized-name png-normalized-name
                            :sorted-name sorted-name
                            :summary summary
                            :normalize normalize
                            :time-out time-out
                            :limit limit))
             (run (types)
               (dolist (f (if (listp decompose)
                              decompose
                              (list decompose)))
                 (declare (type symbol f))
                 (let ((descr (find-decomposition-function-descriptor f)))
                   (loop :for len :from 2 :to limit
                         :do (when (or (null (getf descr :max-num-types))
                                       (>= (getf descr :max-num-types) len))
                               (handle
                                (let ((f f)
                       
                                      (len len))
                                  (lambda (&aux results)
                                    (when (> (get-universal-time) time-out-time)
                                      (log-data)
                                      (return-from types/cmp-perfs 'timed-out))
                                    (format t "    date:  ~A~%" (multiple-value-list (get-decoded-time)))
                                    (format t "function:  ~A~%" f)
                                    (format t "   tag:    ~A~%" tag)
                                    (format t "   length:  ~D~%" len)
                                    (let ((result (types/cmp-perf :types (choose-randomly types len)
                                                                  :decompose f
                                                                  :time-out time-out)))
                                      (when verify
                                        (push result results)
                                        (compare/results results))))))))))))
      (when re-run
        (run (choose-randomly types limit))
        (when randomize
          (let* ((c (length delayed))
                 (len c))
            (setf delayed (shuffle-list delayed))
            (while delayed
              (format t "countdown ~D/~D  ~D/~D~%" (decf c) len (- time-out-time (get-universal-time)) suite-time-out)
              (funcall (pop delayed))))))
      (log-data)))
  t)


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
                                        (funcall f types))))
           (num-unknown 0)
           (num-known 0))
       (declare (type (and unsigned-byte fixnum) num-known num-unknown))
       (dolist (t1 types)
         (dolist (t2 types)
           (dolist (t3 (list t1 `(not t1)))
             (dolist (t4 (list t2 `(not t2)))
               (if (nth-value 1 (subtypep t3 t4))
                   (incf num-unknown)
                   (incf num-known))))))
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
             (format t "given: ~D~%" (length types))
             (let ((*package* (find-package "KEYWORD")))
               (format t "given: ~S~%" types))
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :known num-known
                   :unknown num-unknown
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :time-out time-out))
            (t
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :known num-known
                   :unknown num-unknown
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








(defun find-decomposition-function-descriptor (name)
  (typecase name
    (symbol
     (find-if (lambda (plist)
                (member name (getf plist :names))) 
              *decomposition-function-descriptors*))
    (string
     (find-if (lambda (plist)
                (exists f (getf plist :names)
                  ;; case independent search
                  (string-equal name (symbol-name f))))
              *decomposition-function-descriptors*))))

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

(defun build-string (delimiter data)
  (cond
    ((null data)
     "")
    (t
     (with-output-to-string (str)
       (format str "~A" (car data))
       (dolist (next (cdr data))
         (format str "~A~A" delimiter next))))))

(defun create-gnuplot (sorted-file gnuplot-file png-filename normalize)
  (let ((content (with-open-file (stream sorted-file :direction :input)
                   (read stream nil nil))))
    (with-open-file (stream gnuplot-file :direction :output :if-exists :supersede)
      (destructuring-bind (&key summary sorted &allow-other-keys &aux min-curve) content
        (cl-user::print-vals summary sorted content)
        (assert (typep sorted 'cons))
        ;; sort DATA so that the order agrees with *decomposition-function-descriptors*
        (setf sorted (mapcan (lambda (desc &aux (names (getf desc :names)))
                             (setof plist sorted
                               (exists name names
                                 (string= (getf plist :decompose) (symbol-name name)))))
                           *decomposition-function-descriptors*))
        ;; if we are trying to normalize, we need at least two points in the graph
        ;;    so in this case remove DATA which has less that 2 points to plot
        (when normalize
          (setf sorted (setof plist sorted
                         (cdr (getf plist :xys)))))
        (setf min-curve (reduce (lambda (curve1 curve2)
                                  (if (< (getf curve1 :integral)
                                         (getf curve2 :integral))
                                      curve1
                                      curve2)) (cdr sorted) :initial-value (car sorted)))
        (assert (typep min-curve 'cons))
        (format stream "# summary ~A~%" summary)
        (format stream "set term png~%")
        (unless normalize
          (format stream "set logscale xy~%"))
        (labels ((xys (curve)
                   (declare (notinline sort))
                   (destructuring-bind (&key xys &allow-other-keys) curve
                     (remove-duplicates (sort (copy-list xys)
                                              (lambda (a b)
                                                (if (= (car a) (car b))
                                                    (< (cadr a) (cadr b))
                                                    (< (car a) (car b)))))
                                        :test #'equal))))
          (let* ((line-style 0)
                 (mapping (mapcar (lambda (descr)
                                    (incf line-style)
                                    (format stream "set style line ~D linecolor rgb ~S~%" line-style
                                            (getf descr :color))
                                    ;; collect
                                    `(:line-style ,line-style ,@descr))
                                  *decomposition-function-descriptors*))
                 (normalize-to-xys (when normalize
                                     (xys (find (symbol-name normalize) sorted
                                                :key (getter :decompose) :test #'string=)))))
            (assert (or (not normalize) normalize-to-xys) (normalize normalize-to-xys sorted))
            (incf line-style)
            (format stream "set style line ~D linecolor rgb ~S~%" line-style "dark-turquoise")
            (labels ((interpolate-y (x0 x1 y1 x2 y2)
                       (+ y1 (* (- y2 y1)
                                (/ (float (- x0 x1)) (- x2 x1)))))
                     (interpolate (x)
                       (declare (type integer x))
                       (mapl (lambda (tail &aux (xy1 (car tail)) (xy2 (cadr tail)) (xy-tail (cddr tail)))
                               ;; normalize-to-xys is a list of x-y pairs already ordered by x value, with no duplicate x's
                               ;; for the given x, interpolate the value of y
                               ;; 1) if x < the min x from normalize-to-xys
                               ;; 2) if x is found exactly in normalize-to-xys
                               ;; 3) if x is strictly between two x's from normalize-to-xys
                               ;; 4) if x > the max x from normalize-to-xys
                               (assert xy2 (x normalize tail normalize-to-xys)
                                       "interpolation failed to find x=~A" x)
                               (cond
                                 ((or (< x (car xy1)) ; case 1
                                      (and xy2        ; case 3
                                           (> x (car xy1))
                                           (< x (car xy2)))
                                      (and xy2 ; case 4
                                           (null xy-tail)
                                           (> x (car xy2))))
                                  (return-from interpolate (interpolate-y x (car xy1) (cadr xy1) (car xy2) (cadr xy2))))
                                 ((= x (car xy1)) ; case 2
                                  (return-from interpolate (cadr xy1)))
                                 ((and xy2
                                       (= x (car xy2))) ; case 2
                                  (return-from interpolate (cadr xy2)))))
                             normalize-to-xys))
                     (plot-curve (curve)
                       (destructuring-bind (&key decompose
                                            &allow-other-keys
                                            &aux (color (getf (find-decomposition-function-descriptor decompose) :color)))
                           curve
                         (format stream "# ~A ~A~%" color decompose)
                         (dolist (pair (xys curve))
                           (destructuring-bind (x y) pair
                             (cond
                               (normalize
                                (format stream "~A ~A~%" x (- y (the number (interpolate x)))))
                               ((zerop y))
                               (t
                                (format stream "~A ~A~%" x y)))))
                         (format stream "end~%"))))
              (format stream "plot ~A"
                      (build-string (format nil ",\\~%")
                                    (mapcar (lambda (data-plist &aux (decompose (getf data-plist :decompose)))
                                              (let ((mapping-plist (find-if (lambda (mapping-plist)
                                                                              (exists name (getf mapping-plist :names)
                                                                                (string= decompose
                                                                                         (symbol-name name))))
                                                                            mapping)))
                                                (format nil "   \"-\" using 1:2 notitle with lines ls ~D"
                                                        (getf mapping-plist :line-style))))
                                            sorted)))
              (format stream ",\\~%\   \"-\" using 1:2 notitle with lines ls ~D~%" line-style)
              (mapc #'plot-curve sorted)
              (plot-curve min-curve))))))

    (sb-ext:run-program "gnuplot" (list gnuplot-file)
                        :search t
                        :output png-filename
                        :if-output-exists :supersede)))

(defun integral (xys)
  "given a list of xy pairs (car cadr) calculate the area under the curve formed by the trapizoids.
the list of xys need not be already ordered."
  (let ((acc 0.0)
        (sorted (sort (copy-list xys) (lambda (p1 p2 &aux (x1 (car p1)) (x2 (car p2)) (y1 (cadr p1)) (y2 (cadr p2)))
                                        (cond ((= x1 x2)
                                               (< y1 y2))
                                              (t
                                               (< x1 x2)))))))
    (mapl (lambda (xys-tail)
            (destructuring-bind ((x1 y1) &optional ((x2 y2) '(nil nil)) &rest tail) xys-tail
              (declare (ignore x1 x2))
              (when tail
                ;; increment by trapizoid area
                (incf acc (* 1.0 ;;(- x2 x1)
                             (/ (+ y2 y1) 2))))))
          sorted)
    (when (minusp acc)
      (warn "negative integral ~A: ~A~%" acc sorted))
    (/ acc (length xys))))

(defun statistics (data)
  (destructuring-bind (&key summary
                         time-out-count
                         run-count
                         time-out-run-time
                         run-time
                         wall-time
                         limit
                         unknown
                         known
                         sorted &allow-other-keys) data
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
          (let* ((mean (unless (zerop count)
                         (/ sum count)))
                 (sigma (when mean
                          (stdev count mean integrals)))
                 previous)
            (list :summary summary
                  :time-out-count time-out-count
                  :run-count run-count
                  :time-out-run-time time-out-run-time
                  :run-time run-time
                  :wall-time wall-time
                  :limit limit
                  :unknown unknown
                  :known known
                  :sigma sigma
                  :mean mean
                  :sorted (mapcar (lambda (plist)
                                    (destructuring-bind (&key integral samples decompose arguments xys &allow-other-keys
                                                         &aux (score (unless (member sigma '(0 0.0 nil))
                                                                       (/ (- integral mean) sigma)))) plist
                                    (list :integral integral
                                          :score score
                                          :delta (when score
                                                   (prog1 (when previous
                                                            (- previous score))
                                                     (setf previous score)))
                                          :samples samples
                                          :decompose decompose
                                          :arguments arguments
                                          :xys xys)))
                                sorted))))))))

(defun append-tail (l1 l2 value)
  (append l1 (mapcar (constantly value) (nthcdr (length l1) l2))))

(append-tail '(a b c d) '(0 0 0 0 1 2 3 4 5 6 7) -1)
(append-tail '(0 0 0 0 1 2 3 4 5 6 7) '(a b c d) -1)



;; (reduce (lambda (string num) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")



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
     (destructuring-bind (&key summary limit data time-out-count run-count time-out-run-time run-time wall-time unknown known) (read in nil nil)
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
                 (getf plist :given) (getf plist :calculated) (getf plist :run-time)))
         (statistics
          (list :summary summary
                :time-out-count time-out-count
                :run-count run-count
                :time-out-run-time time-out-run-time
                :run-time run-time
                :wall-time wall-time
                :limit limit
                :unknown unknown
                :known known
                :sorted (sort 
                         (loop for plist in data
                               collect (destructuring-bind (&key decompose given calculated run-time &allow-other-keys) plist
                                         (let ((xys (mapcar (lambda (given calculated run-time)
                                                              (declare (type fixnum given calculated)
                                                                       (type number run-time))
                                                              (list (* given calculated) run-time)) given calculated run-time)))
                                           (unless (find observed-min-prod xys :key #'car)
                                             (push (list observed-min-prod observed-min-time) xys))
                                           (unless (find observed-max-prod xys :key #'car)
                                             (push (list observed-max-prod observed-max-time) xys))
                                           (list :integral (integral xys) ;; TODO using the simple integral to sort the curves is dubious because it give more weight to 
                                                 :xys xys
                                                 :samples (length xys)
                                                 :decompose decompose
                                                 :arguments (get (intern decompose (find-package :lisp-types))
                                                                 'decompose-properties)))))
                         #'< :key (lambda (obj) (getf obj :integral))))))))
    (t
     (let ((*package* (find-package "CL")))
       (format out "~S~%" (apply #'sort-results in :return options))))))

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

(defun print-sexp (stream summary limit)
  (let ((groups (group-by (cdr *perf-results*) :key (getter :decompose))))
    (format stream "(")
    (when summary
      (format stream " :SUMMARY ~S~%" summary))
    (format stream "  :LIMIT ~D~%" limit)
    (format stream "  :TIME-OUT-COUNT ~D~%" (count-if (getter :time-out) *perf-results*))
    (format stream "  :RUN-COUNT ~D~%" (count-if (getter :calculated) *perf-results*))
    (format stream "  :TIME-OUT-RUN-TIME ~A~%" (reduce (lambda (total next)
                                                         (if (getf next :time-out)
                                                             (+ total (getf next :run-time))
                                                             total)) *perf-results*
                                                             :initial-value 0))
    (format stream "  :RUN-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :run-time))
                                                total))
                                              *perf-results*
                                              :initial-value 0))
    (format stream "  :WALL-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :wall-time))
                                                total))
                                               *perf-results*
                                               :initial-value 0))
    (format stream " :DATA  (")
    (flet ((print-group (group)
             (destructuring-bind (decompose data) group
               (format stream "( :DECOMPOSE ~S" (symbol-name decompose))
               (when (get decompose 'lisp-types::decompose-properties)
                 (let ((*package* (find-package "KEYWORD")))
                   (format stream "~%:OPTIONS ~S" (get decompose 'lisp-types::decompose-properties))))
               (let ((time-outs (setof item data
                                  (getf item :time-out)))
                     (no-time-out (setof item data
                                    (null (getf item :time-out)))))
                 (when time-outs
                   (format stream "~%:TIME-OUT (:TIME ~D :COUNT ~D~%"
                           (getf (car time-outs) :time-out) 
                           (length time-outs))
                   (format stream ":GIVEN ~A)"
                           (mapcar (getter :given) time-outs)))
               (dolist (tag `(:given :calculated :run-time :wall-time :known :unknown))
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

(defun print-report (stream &rest args &key limit (summary nil) normalize (sorted-name "/dev/null") (sexp-name "/dev/null") (png-name "/dev/null") (png-normalized-name "/dev/null") (gnuplot-name "/dev/null") (include-decompose *decomposition-functions*) &allow-other-keys)
  (etypecase stream
    ((or stream
         (eql t)
         (eql nil))
     (format t "report ~A~%" summary)
     (with-open-file (stream sexp-name :direction :output :if-exists :supersede)
       (print-sexp stream summary limit))
     (sort-results sexp-name sorted-name)
     (create-gnuplot sorted-name gnuplot-name png-name nil)
     (when normalize
       (create-gnuplot sorted-name gnuplot-name png-normalized-name normalize))
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
       (apply #'print-report str args)))))

(defun test-report (&key (prefix "") (re-run t) (suite-time-out (* 60 60 4))  (time-out (* 3 60)) normalize (destination-dir "/Users/jnewton/newton.16.edtchs/src")
                      types file-name (limit 15) tag
                    &allow-other-keys)
  "TIME-OUT is the number of seconds to allow for one call to a single decompose function.
SUITE-TIME-OUT is the number of time per call to TYPES/CMP-PERFS."
  (when re-run
    (setf *perf-results* nil))
  (let ((type-specifiers (shuffle-list types)))
    (apply #'types/cmp-perfs :re-run re-run
                             :types type-specifiers
                             :suite-time-out suite-time-out
                             :randomize t
                             :limit limit
                             :tag tag
                             :summary file-name
                             :time-out time-out
                             :decompose  *decomposition-functions*
                             :normalize normalize
                             (when file-name
                               (list
                                :dat-name (format nil "~A/~A~A.dat" destination-dir prefix file-name)
                                :png-name (format nil "~A/~A~A.png" destination-dir prefix file-name)
                                :png-normalized-name (format nil "~A/~A~A-normalized.png" destination-dir prefix file-name)
                                :gnuplot-name (format nil "~A/~A~A.gnu" destination-dir prefix file-name)
                                :sexp-name (format nil "~A/~A~A.sexp" destination-dir prefix file-name)
                                :sorted-name (format nil "~A/~A~A.sorted" destination-dir prefix file-name))))))

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

(defun parameterization-report (&key (re-run t))
  (big-test-report :re-run re-run
                   :prefix "bdd-graph-"
                   :normalize 'decompose-types-bdd-graph
                   :decomposition-functions (cons 'decompose-types-bdd-graph
                                                  *decompose-fun-names*)))

(defun analysis (file-names)
  (declare (notinline sort))
  (let* ((measures '(:recursive :inner-loop :sort-strategy :do-break-sub :do-break-loop))
        (table (make-hash-table :test #'eq))
        (data (sort (mapcan (lambda (file-name)
                              (with-open-file (stream file-name)
                                (destructuring-bind (&key summary sorted &allow-other-keys) (read stream)
                                  (mapcar (lambda (sorted-plist)
                                            (destructuring-bind (&key score integral arguments &allow-other-keys) sorted-plist
                                              (dolist (measure measures)
                                                (pushnew (getf arguments measure) (gethash measure table) :test #'equal))
                                              (destructuring-bind (&key sort-strategy inner-loop recursive do-break-sub do-break-loop &allow-other-keys) arguments
                                                (list :score score
                                                      :recursive recursive
                                                      :inner-loop inner-loop
                                                      :sort-strategy sort-strategy
                                                      :do-break-sub do-break-sub
                                                      :do-break-loop do-break-loop
                                                      :integral integral
                                                      :summary summary))))
                                          sorted))))
                            file-names)
                    #'< :key (getter :score))))
    (flet ((mean (numbers &aux (c 0))
             (/ (reduce (lambda (acc next)
                          (incf c)
                          (+ acc next)) numbers :initial-value 0.0) c)))
      (list :attributes
            (sort (mapcan (lambda (measure)
                      (mapcar (lambda (value)
                                (list measure value (mean (mapcar (getter :score) (setof plist data
                                                                                    (equal value (getf plist measure)))))))
                              (gethash measure table)))
                    measures)
                  #'< :key #'caddr)
            :data data))))

(defun do-analysis ()
  (analysis '("/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-combos.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-member.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-pcl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number-or-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-t.sorted")))

(defun big-test-report (&rest options &key (prefix "") (re-run t) (suite-time-out (* 60 60 4)) (time-out (* 3 60)) normalize (decomposition-functions *decomposition-functions*) (destination-dir "/Users/jnewton/newton.16.edtchs/src"))
  (declare (ignore prefix re-run suite-time-out time-out normalize destination-dir))
  (let ((*decomposition-functions*  decomposition-functions))
    (apply #'test-report :limit 27
                         :tag "cl-types"
                         :types *cl-types*
                         :file-name "cl-types"
                         options)
    (apply #'test-report :limit 26
                         :tag "cl-combos"
                         :types (choose-randomly  *cl-type-combos* 13850)
                         :file-name "cl-combos"
                         options)
    
    (apply #'test-report :limit 26
                         :tag "member"
                         :types *member-types*
                         :file-name "member"
                         options)
    
    (apply #'test-report :limit 15
                         :tag "conditions"
                         :types (valid-subtypes 'condition)
                         :file-name "subtypes-of-condition"
                         options)

    (apply #'test-report :limit 23
                         :tag "numbers"
                         :types (valid-subtypes 'number)
                         :file-name "subtypes-of-number"
                         options)

    (apply #'test-report :limit 13
                         :tag "numbers and conditions"
                         :types (union (valid-subtypes 'number) (valid-subtypes 'condition))
                         :file-name "subtypes-of-number-or-condition"
                         options)

    (apply #'test-report :limit 22
                         :tag "subtypes of t"
                         :types (valid-subtypes t)
                         :file-name "subtypes-of-t"
                         options)

    (apply #'test-report :limit 15
                         :tag "SB-PCL types"
                         :types (loop :for name being the external-symbols in "SB-PCL"
                                      :when (find-class name nil)
                                        :collect name)
                         :file-name "pcl-types"
                         options)
    ))

