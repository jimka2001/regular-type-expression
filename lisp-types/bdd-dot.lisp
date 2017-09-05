;; Copyright (c) 2017 EPITA Research and Development Laboratory
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

(in-package   :lisp-types)

(defun bdd-to-dot (bdd stream &key (reduced t))
  (cond
    ((null stream)
     (with-output-to-string (str)
       (bdd-to-dot bdd str :reduced reduced)))
    (t
     ;; header
     (format stream "digraph G {~%")
     
     (labels ((dot-node (bdd node-num)
                (format stream "~D [shape=~A,label=~S]~%"
                        node-num
                        (bdd-shape bdd)
                        (format nil "~A" (bdd-label bdd))))
              (bdd-shape (bdd)
                (typecase bdd
                  (bdd-node "ellipse")
                  (bdd-leaf "box")))

              )

       (cond
         (reduced
          (let* ((num 0)
                 (buf (tconc nil (list :bdd bdd :node-num (incf num))))
                 labels
                 (nodes (car buf)))
            ;; BFS: first print the node delcarations and remember the node list, and remember the labels
            (while nodes
              (destructuring-bind (&key node-num bdd) (car nodes)
                (pushnew (bdd-label bdd) labels :test #'equal)
                (dot-node bdd node-num)
                (typecase bdd
                  (bdd-node
                   (unless (find (bdd-left bdd) (car buf) :key (getter :bdd))
                     (tconc buf (list :bdd (bdd-left bdd)  :node-num (incf num))))
                   (unless (find (bdd-right bdd) (car buf) :key (getter :bdd))
                     (tconc buf (list :bdd (bdd-right bdd) :node-num (incf num)))))))
              (pop nodes))
            ;; now print the rank=same lines
            (dolist (label labels)
              (let ((common-labels (setof node (car buf)
                                     (equal label (bdd-label (getf node :bdd))))))
                (when (cdr common-labels)
                  (format stream "{rank=same")
                  (dolist (common common-labels)
                    (format stream " ~D" (getf common :node-num)))
                  (format stream "}~%"))))
            ;; now print the connections
            (dolist (node (car buf))
              (destructuring-bind (&key node-num bdd) node
                (typecase bdd
                  (bdd-node
                   (let* ((left-num  (getf (find (bdd-left  bdd) (car buf)
                                                 :key (getter :bdd))
                                           :node-num))
                          (right-num (getf (find (bdd-right bdd) (car buf)
                                                 :key (getter :bdd))
                                           :node-num)))
                     (format stream "~D -> ~D [style=~A,color=~A]~%" node-num left-num  "solid" "green")
                     (format stream "~D -> ~D [style=~A,color=~A]~%" node-num right-num "dotted" "red"))))))))
         (t
          (let (nodes
                (num 0))
            (labels ((visit (bdd action path)
                       (funcall action bdd path)
                       (typecase bdd
                         (bdd-node
                          (visit (bdd-left bdd) action (cons :L path))
                          (visit (bdd-right bdd) action (cons :R path)))))
                     (name-node (bdd path)
                       (push (list :bdd bdd :node-num (incf num) :path path) nodes))
                     (print-node (bdd path &aux
                                             (node (find-node bdd path))
                                             (node-num (getf node :node-num)))
                       (dot-node bdd node-num))
                     (find-node (bdd path)
                       (find-if #'(lambda (node)
                                    (and (eq bdd (getf node :bdd))
                                         (equal path (getf node :path))))
                                nodes))
                     (print-connections (bdd path)
                       (typecase bdd
                         (bdd-node
                          (let* ((node-num  (getf (find-node bdd             path)           :node-num))
                                 (left-num  (getf (find-node (bdd-left  bdd) (cons :L path)) :node-num))
                                 (right-num (getf (find-node (bdd-right bdd) (cons :R path)) :node-num)))
                            (declare (type fixnum left-num right-num))
                            (format stream "~D -> ~D [style=~A]~%" node-num left-num  "solid")
                            (format stream "~D -> ~D [style=~A]~%" node-num right-num "dotted"))))))
              (visit bdd #'name-node ())
              (visit bdd #'print-node ())
              (visit bdd #'print-connections ())))))
         
       ;; footer
       (format stream "}~%")))))
     
(defun bdd-to-png (bdd &key (reduced t) (basename (format nil "/tmp/jnewton/graph/~A" (bdd-ident bdd))))
  (let ((dot-path (format nil "~A.dot" basename))
        (png-path (format nil "~A.png" basename)))
    (ensure-directories-exist dot-path)
    (with-open-file (stream dot-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (bdd-to-dot bdd stream :reduced reduced))
    (format t "~A~%" png-path)
    (run-program "dot"
                 (list "-Tpng" dot-path
                       "-o" png-path)
                 :search t)
    png-path))
  
(defun bdd-view (bdd &key (reduced t) (basename (format nil "/tmp/jnewton/graph/~A" (bdd-ident bdd))))
  (run-program "open" (list (bdd-to-png bdd :reduced reduced
                                            :basename basename))
               :search t))
