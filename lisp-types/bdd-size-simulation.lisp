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
      (loop for try from 2 below num-samples
            do (measure try randomp)))
    (let (a-list sum average)
      (declare (notinline sort))
      (maphash (lambda (&rest args)
                 (push args a-list))
               hash)
      (setf sum (reduce #'+ a-list :initial-value 0 :key #'cadr))
      (setf average (/ (reduce (lambda (old item)
                                 (destructuring-bind (size occurances) item
                                   (+ old (* size occurances)))) a-list :initial-value 0) sum))
      ;;
      (list :sum sum
            :num-samples num-samples
            :randomp randomp
            :num-vars (length vars)
            :density density
            :average-size average
            :counts (mapcar (lambda (pair)
                              (declare (type (cons integer (cons integer)) pair))
                              (list (car pair)
                                    (float (/ (cadr pair) sum))
                                    (cadr pair)
                                    (truncate (cadr pair) density)
                                    ))
                            (sort a-list #'< :key #'car))))))

(defun measure-bdd-sizes (vars num-samples)
  (maplist (lambda (vars)
             (measure-bdd-size vars
                               (min (expt 2 (expt 2 (length vars)))
                                    num-samples)))
           vars))

(defun latex-measure-bdd-sizes (prefix vars num-samples)
  (declare (type string prefix)
           (type list vars)
           (type fixnum num-samples)
           (notinline sort))
  ;; prefix = "/Users/jnewton/newton.16.edtchs/src"
  (let (legend
        (colors '("red" "goldenrod" "blue" "lavender" "greeny" "dark-cyan"))
        (data (sort (measure-bdd-sizes vars num-samples) #'< :key (getter :num-vars))))

    (flet ((individual-plot (stream num-vars &aux (plist (find num-vars data :key (getter :num-vars))))
             (format stream "\\begin{tikzpicture}~%")
             (format stream "\\begin{axis}[xlabel=BDD node count for ~D variables,ymajorgrids,yminorgrids,xmajorgrids,xminorgrids,ylabel=Number of Boolean functions,legend style={font=\tiny}]~%" num-vars)
             (format stream "\\addplot[color=blue] coordinates {~%")
             (dolist (item (getf plist :counts))
               (format stream "(~D,~D)~%" (car item) (coerce (nth 3 item) 'float)))
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
      (loop for num-vars from 1 to (length vars)
            do (with-open-file (stream (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars)
                                       :direction :output :if-does-not-exist :create :if-exists :supersede)
                 (individual-plot stream num-vars)))
      )
         
    data))
