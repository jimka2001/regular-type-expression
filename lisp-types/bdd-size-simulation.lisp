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
