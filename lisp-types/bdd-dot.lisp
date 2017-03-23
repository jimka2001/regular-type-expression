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

     (let* ((buf (tconc nil (list :bdd bdd :node-name (format nil "~A" (bdd-label bdd)))))
            done
            (nodes (car buf)))

       (labels ((descend (&rest args &key bdd &allow-other-keys)
                  (declare (type bdd bdd))
                  (cond
                    ((not reduced)
                     (tconc buf args)                     
                     (print-node args)
                     args)
                    ((car (member bdd (car buf) :key (getter :bdd))))
                    (t
                     (tconc buf args)
                     (print-node args)
                     args)))
                (print-node (args)
                  (destructuring-bind (&key bdd node-name &allow-other-keys) args
                    (unless (and reduced
                                 (find bdd done :key (getter :bdd)))
                      (format stream "~S [shape=~A,label=~S]~%"
                              node-name
                              (typecase bdd
                                (bdd-node "ellipse")
                                (bdd-leaf "box"))
                              (format nil "~A" (bdd-label bdd))))))
                (connect (bdd1 name2 parity &aux
                                             (name1 (getf (find bdd1 (car buf) :key (getter :bdd)) :node-name))
                                    )
                  (format stream "~S -> ~S [style=~A]~%" name1 name2 (ecase parity
                                                                       ((:L) "solid")
                                                                       ((:R) "dotted")))))
         (print-node (car nodes))
         (while nodes
           (destructuring-bind (&key bdd node-name &allow-other-keys &aux name-R name-L) (car nodes)
             (typecase bdd
               (bdd-node
                (setf name-L (getf (descend :bdd (bdd-left bdd)  :node-name (format nil "~A:L" node-name) :parity :L) :node-name))
                (setf name-R (getf (descend :bdd (bdd-right bdd) :node-name (format nil "~A:R" node-name) :parity :R) :node-name))))
             (unless (and reduced
                          (find bdd done :key (getter :bdd)))
               (push (car nodes) done)
               (typecase bdd
                 (bdd-node
                  (connect bdd name-L  :L)
                  (connect bdd name-R :R))))
             (pop nodes)))))
         
     ;; footer
     (format stream "}~%"))))
     
(defun bdd-view (bdd)
  (let ((dot-path (format nil "/tmp/jnewton/graph/~A.dot" (bdd-ident bdd)))
        (png-path (format nil "/tmp/jnewton/graph/~A.png" (bdd-ident bdd))))
  (with-open-file (stream dot-path :direction :output :if-exists :supersede)
    (bdd-to-dot bdd stream :reduced nil))
    (sb-ext:run-program "dot"
                        (list "-Tpng" dot-path
                              "-o" png-path)
                        :search t)
    (sb-ext:run-program "open" (list png-path) :search t)))
