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

(in-package :lisp-types)


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

(defun median-a-list (a-list)
  (let ((a-list (copy-list a-list)))
    (loop while (cdr a-list)
          do (let ((couple (cons (car a-list) (last a-list))))
               (destructuring-bind ((low-index low-count)
                                    (high-index high-count)) couple
                 (setf a-list (if (and (null (cddr a-list))
                                       (= low-count high-count))
                                  (list (list (/ (+ low-index high-index) 2) 1))
                                  (merge 'list (cond ((= low-count high-count)
                                                      nil)
                                                     ((< low-count high-count)
                                                      (list (list high-index (- high-count low-count))))
                                                     (t
                                                      (list (list low-index (- low-count high-count)))))
                                         (set-difference a-list couple :test #'eq)
                                         #'<
                                         :key #'car))))))
    (values (caar a-list) a-list)))

(assert (= 1 (median-a-list '((1 3)))))
(assert (= 1 (median-a-list '((0 1) (1 3) (2 1)))))
(assert (= 1 (median-a-list '((0 2) (1 3) (2 1)))))
(assert (= 1/2 (median-a-list '((0 2) (1 2)))))

(defun make-announcement-timer (min max interval announce)
  (declare (type (function (integer number) t))
           (type integer min max)
           (type real interval))
  (let* ((start-time (get-internal-real-time))
         (internal-interval (* interval internal-time-units-per-second))
         (previous-announcement start-time))
    (lambda (iteration &aux (now (get-internal-real-time)))
      (cond
        ((equal min iteration))
        ((< (+ previous-announcement internal-interval)
            now)
         (setf previous-announcement now)
         (let* ((finish (+ start-time (/ (* (- max min) (- now start-time))
                                         (- iteration min))))
                (remaining-seconds (/ (- finish now) internal-time-units-per-second)))
           (funcall announce iteration (coerce remaining-seconds 'double-float))))))))
        

(defun measure-bdd-size (vars num-samples)
  (let* ((hash (make-hash-table))
         (ffff (1- (expt 2 (expt 2 (length vars)))))
         (density (/ num-samples (1+ ffff)))
         (randomp (< num-samples (1+ ffff))))
    (format t "generating ~D " num-samples)
    (when randomp (format t "randomly chosen "))
    (format t "BDDs of possible ~D with ~D variables ~A~%"  (1+ ffff) (length vars) vars)
    (flet ((measure (try randomp)
             (sb-ext::gc :full t)
             (bdd-with-new-hash (lambda (&aux (boolean-combo (if randomp
                                                                 (random-boolean-combination vars)
                                                                 (int-to-boolean-expression try vars)))
                                           (bdd (bdd boolean-combo))
                                           (node-count (bdd-count-nodes bdd)))
                                  (incf (gethash node-count hash 0)))
                                :verbose nil)))
      (measure 0 nil)
      (measure 1 nil)
      (measure ffff nil)
      (let ((announcer (make-announcement-timer 2 (1- num-samples) 2
                                                (lambda (try remaining-seconds)
                                                  (format t "~D ~D: seconds remaining ~D = ~D minutes = ~D hours~%"
                                                          (length vars)
                                                          try remaining-seconds
                                                          (/ remaining-seconds 60)
                                                          (/ remaining-seconds (* 60 60)))))))
        (loop for try from 2 below num-samples
              do (progn (measure try randomp)
                        (funcall announcer try)))))
    (let (a-list sum average median)
      (declare (notinline sort))
      (maphash (lambda (&rest args)
                 (push args a-list))
               hash)
      (setf a-list (sort a-list #'< :key #'car))
      (setf sum (reduce #'+ a-list :initial-value 0 :key #'cadr))
      (setf average (/ (reduce (lambda (old item)
                                 (destructuring-bind (size occurances) item
                                   (+ old (* size occurances)))) a-list :initial-value 0) sum))
      (setf median (median-a-list a-list))      
      ;;
      (list :sum sum
            :num-samples num-samples
            :randomp randomp
            :num-vars (length vars)
            :density density
            :average-size average
            :median median
            :counts (mapcar (lambda (pair)
                              (declare (type (cons integer (cons integer)) pair))
                              (list (car pair)
                                    (float (/ (cadr pair) sum))
                                    (cadr pair)
                                    (truncate (cadr pair) density)
                                    ))
                            a-list)))))

(defun measure-bdd-sizes (vars num-samples min max)
  (mapcon (lambda (vars)
            (cond
              ((> min (length vars))
               nil)
              ((> (length vars) max)
               nil)
              (t
               (list (measure-bdd-size vars
                                       (min (expt 2 (expt 2 (length vars)))
                                            num-samples))))))
          vars))

(defun latex-measure-bdd-sizes (prefix vars num-samples &key (min 1) (max (length vars)))
  (declare (type string prefix)
           (type list vars)
           (type fixnum num-samples)
           (notinline sort))
  (ensure-directories-exist prefix)
  ;; prefix = "/Users/jnewton/newton.16.edtchs/src"
  (let (legend
        (colors '("red" "goldenrod" "blue" "lavender" "greeny" "dark-cyan" "color-7" "color-8"))
        (data (sort (measure-bdd-sizes vars num-samples min max) #'< :key (getter :num-vars))))

    (flet ((individual-plot (stream num-vars &aux (plist (find num-vars data :key (getter :num-vars))))
             (format stream "\\begin{tikzpicture}~%")
             (format stream "\\begin{axis}[xlabel=ROBDD node count for ~D variables,ymajorgrids,yminorgrids,xmajorgrids,xminorgrids,ylabel=Number of Boolean functions,legend style={font=\tiny}]~%" num-vars)
             (format stream "\\addplot[color=blue] coordinates {~%")
             (dolist (item (getf plist :counts))
               (format stream "(~D,~D)~%" (car item) (coerce (nth 3 item) 'double-float)))
             (format stream "};~%")
             (format stream "\\legend{}~%")
             (format stream "\\end{axis}~%")

             (format stream "\\end{tikzpicture}~%"))
           (average-plot (stream)
             (format stream "\\begin{tikzpicture}~%")
             (format stream "\\begin{axis}[ymajorgrids,yminorgrids,xmajorgrids,xlabel=Number of variables,ylabel=ROBDD size,legend style={anchor=west,font=\tiny},")
             (format stream "xtick={1")
             (loop for xtick from 2
                     to (reduce (lambda (max item)
                                  (max max (getf item :num-vars)))
                                (cdr data)
                                :initial-value (getf (car data) :num-vars))
                   do (format stream ",~D" xtick))
             (format stream "}]~%")
             ;; worst case size
             (format stream "\\addplot[color=red] coordinates {~%")
             (dolist (plist data)
               (destructuring-bind (&key num-vars counts &allow-other-keys) plist
                 (format stream "(~D,~D)~%"
                         num-vars
                         (reduce #'max counts :key #'car :initial-value 0))))
             (format stream "};~%")
             ;; average size
             (format stream "\\addplot[color=green] coordinates {~%")
             (dolist (plist data)
               (destructuring-bind (&key num-vars average-size &allow-other-keys) plist
                 (format stream "(~D,~D)~%"
                         num-vars
                         (coerce average-size 'double-float))))
             (format stream "};~%")
             (format stream "\\addplot[color=blue] coordinates {~%")
             (dolist (plist data)
               (destructuring-bind (&key num-vars median &allow-other-keys) plist
                 (format stream "(~D,~D)~%"
                         num-vars
                         median)))
             (format stream "};~%")
             (format stream "\\legend{Worst case, Average, Median}~%")
             (format stream "\\end{axis}~%")
             (format stream "\\end{tikzpicture}~%"))
           (size-plots (stream)
             (format stream "\\begin{tikzpicture}~%")
             (format stream "\\begin{axis}[xlabel=BDD Size, ymajorgrids,yminorgrids,xmajorgrids, xminorgrids, ylabel=Probability, legend style={font=\\tiny}, label style={font=\\tiny}]~%")
                
             (dolist (datum data)
               (destructuring-bind (&key num-vars counts &allow-other-keys) datum
                 (when (> num-vars 1)
                   (push (format nil "Size with ~D variables" num-vars) legend)
                   (format stream "\\addplot[color=~A] coordinates {~%"
                           (or (pop colors) "black"))
                   (dolist (xy counts)
                     (format stream "  (~D,~A)~%" (car xy) (cadr xy)))
                   (format stream "};~%"))))
                
             (format stream "\\legend{")
             (let ((first t))
               (dolist (label (reverse legend))
                 (unless first
                   (format stream ","))
                 (format stream "~S" label)
                 (setf first nil)))
             (format stream "}~%")
             (format stream "\\end{axis}~%")
             (format stream "\\end{tikzpicture}~%")))


      (with-open-file (stream (format nil "~A/bdd-distribution.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)

        (size-plots stream))
      (with-open-file (stream (format nil "~A/bdd-distribution-expected.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (average-plot stream))
      (loop for num-vars from min to max
            do (with-open-file (stream (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars)
                                       :direction :output :if-does-not-exist :create :if-exists :supersede)
                 (individual-plot stream num-vars)))
      )
    data))

(defun all-possible-bdds (prefix vars &aux (num-vars (length vars)))
  (declare (notinline sort)
           (type string prefix)
           (type list vars)
          )
  (let ((bdd-data (bdd-with-new-hash (lambda ()
                         (loop for try from 0 below (expt 2 (expt 2 num-vars))
                               collect (let* ((expr (int-to-boolean-expression try vars))
                                         (bdd (bdd expr)))
                                         (list :bdd bdd
                                               :node-count (bdd-count-nodes bdd)
                                               :expr (bdd-to-dnf bdd)))))
                       :verbose nil))
        (uniq 1000))
    
    (sort (loop for data in (sort bdd-data #'< :key (getter :node-count))
          collect (destructuring-bind (&key bdd expr node-count &allow-other-keys) data
                    (list :node-count node-count
                          :num-vars num-vars
                          :path (bdd-to-png bdd :reduced t
                                                :basename (format nil "~A/vars=~D-~D-~D"
                                                                  prefix num-vars node-count (incf uniq)))
                          :expr expr)))
          #'< :key (getter :node-count))))

(defun all-possible-bdds-latex (prefix vars)
  (with-open-file (latex (format nil "~A/all-robdds-~A.ltx" prefix (length vars))
                         :direction :output :if-exists :supersede)
    (format latex "\\begin{table}~%")
    (format latex "\\begin{center}~%")
    (format latex "\\begin{tabular}{c|c|l}~%")
    (format latex "No. Nodes & ROBDD & Boolean Expression\\\\~%")
    (format latex "\\hline~%")
    (dolist (data (all-possible-bdds prefix vars))
      (destructuring-bind (&key path node-count expr &allow-other-keys) data
        (format latex "~D " node-count)
        (format latex "& \\includegraphics[width=0.5in]{~A}~%" (pathname-name (pathname path)))
        (format latex "&~%")
        (format latex "\\begin{minipage}{2in}")
        (format latex "\\begin{verbatim}~%")
        (format latex "~A~%" expr)
        (format latex "\\end{verbatim}~%")
        (format latex "\\end{minipage}\\\\~%")))
    (format latex "\\hline~%")
    (format latex "\\end{tabular}~%")
    (format latex "\\end{center}~%")
    (format latex "\\caption{All ROBDDs of ~r Variable~:p }~%" (length vars))
    (format latex "\\label{fig.robdds.of.size.~D}~%" (length vars))
    (format latex "\\end{table}~%")))
