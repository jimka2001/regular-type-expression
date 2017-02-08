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

(in-package   :lisp-types)
(defmacro remfq (obj place)
  `(setf ,place (remove ,obj ,place :test #'eq)))

;;(pushnew :bdd-debug *features*)
(setf *features* (remove :bdd-debug *features*))

(defun bdd-graph-to-dot (graph out)
  (typecase out
    (string
     (let ((pathname (pathname out)))
       (cond ((string= "dot" (pathname-type pathname))
              (with-open-file (stream out :direction :output :if-exists :supersede)
                (bdd-graph-to-dot graph stream)))
             ((string= "png" (pathname-type pathname))
              (let ((dot-path (merge-pathnames (make-pathname :type "dot") pathname)))
                (bdd-graph-to-dot graph (namestring dot-path))
                (sb-ext:run-program "dot" (list "-Tpng" (namestring dot-path)
                                                "-o" out)
                                    :search t)))))
     out)
    ((eql t)
     (bdd-graph-to-dot graph *standard-output*))
    (null
     (with-output-to-string (str)
       (bdd-graph-to-dot graph str)))
    (stream
     (labels ((dnf (node)
                (bdd-to-dnf (getf node :bdd)))
              (print-head ()
                (print-comments)
                (format out "digraph G {~%")
                (format out "  rankdir=BT ;~%"))
              (print-foot ()
                (format out "}~%"))
              (print-comments ()
                (dolist (node graph)
                  (format out "// ~D " (getf node :id))
                  (write (dnf node) :stream out :pretty nil)
                  (format out "~%")))
              (print-node-defs ()
                (dolist (node graph)
                  (format out "  ~D ; // " (getf node :id))
                  (write (dnf node) :stream out :pretty nil)
                  (terpri out)))
              (print-touching ()
                (format out "  subgraph Rel1 {~%")
                (format out "    edge [dir=none, color=green]~%")
                (dolist (node graph)
                  (dolist (touch (getf node :touches))
                    ;; avoid connecting A->B and B-> A
                    (when (< (getf node :id) (getf touch :id))
                      (format out "    ~D -> ~D~%" (getf node :id) (getf touch :id)))))
                (format out "}~%"))
              (print-sub-super ()
                (format out "  subgraph Rel2 {~%")
                (format out "    edge [color=blue]~%")
                (dolist (node graph)
                  (dolist (super (getf node :super-types))
                    (format out "    ~D -> ~D~%" (getf node :id) (getf super :id))))
                (format out "}~%")))
       (print-head)
       (print-node-defs)
       (print-touching)
       (print-sub-super)
       (print-foot)))))

(defun %decompose-types-bdd-graph (type-specifiers)
  (declare (notinline sort)
           (optimize (speed 3) (compilation-speed 0) (debug 0) (space 0)))
  (let* ((node-id 0)
         (bdds (remove-duplicates
                (sort (mapcar #'bdd type-specifiers)
                      #'>
                      :key #'(lambda (bdd)
                               (length (bdd-collect-atomic-types bdd))))
                :test #'bdd-type-equal))
         #+:bdd-debug (bdd-type-orig (bdd `(or ,@type-specifiers)))
         (graph (loop :for bdd :in bdds
                      :collect (list :bdd bdd
                                     :super-types nil
                                     :sub-types nil
                                     :touches nil
                                     :id (incf node-id)))))



    (let ((changed nil)
          #+:bdd-debug (c 1000)
          #+:bdd-debug (html-file "/tmp/jnewton/graph.html")
          disjoint-bdds)
      (labels ((disjoint! (node)
                 (setf changed t)
                 (setf graph (remove node graph :test #'eq))
                 (unless (eq (getf node :bdd) *bdd-false*)
                   (pushnew (getf node :bdd) disjoint-bdds :test #'bdd-type-equal)))
               (node-to-dnf (node)
                 (declare (type cons node))
                 (bdd-to-dnf (getf node :bdd)))
               #+:bdd-debug (getter (prop)
                              (lambda (plist)
                                (getf plist prop)))
               #+:bdd-debug (dot (comment &rest nodes)
                              (let ((*print-case* :downcase)
                                    (png-file (format nil "/tmp/jnewton/graph-~D.png" (incf c)))
                                    (graph-bdd (reduce #'bdd-or (mapcar (getter :bdd) graph)
                                                       :initial-value (reduce #'bdd-or disjoint-bdds
                                                                              :initial-value *bdd-false*))))
                                (with-open-file (html html-file :direction :output
                                                                :if-exists :append
                                                                :if-does-not-exist :create)
                                  (format html "<hr><br>~A<br>~%" comment)
                                  (format html "<ul>~%")
                                  (dolist (node nodes)
                                    (format html "<li> ~D ~A~%" (getf node :id) (node-to-dnf node)))
                                  (format html "</ul>~%")
                                  (format html "<p><img src=~S><p>~%" png-file)
                                  (bdd-graph-to-dot graph png-file)
                                  (format html "expression: <pre>~A</pre>~%<p>" (bdd-to-dnf graph-bdd))
                                  (when graph
                                    (format html "Graph Nodes:<br><ul>~%")
                                    (dolist (node graph)
                                      (format html "<li>~D ~A<br>~%" (getf node :id) (node-to-dnf node)))
                                    (format html "</ul>~%"))
                                  (when disjoint-bdds
                                    (format html "Disjoint types:<br><ul>~%")
                                    (dolist (bdd disjoint-bdds)
                                      (format html "<li>~A<br>~%" (bdd-to-dnf bdd)))
                                    (format html "</ul>~%"))
                                  (cond
                                    ((eq bdd-type-orig graph-bdd)
                                     (format html "graph is consistent:<br>~%"))
                                    (t
                                     (let ((a-not-b (bdd-and-not bdd-type-orig graph-bdd))
                                           (b-not-a (bdd-and-not graph-bdd bdd-type-orig)))
                                       (cond
                                         ((and (eq a-not-b *bdd-false*)
                                               (eq b-not-a *bdd-false*))
                                          (format html "equal BDDs which are not EQ:<br>~%")
                                          (if (equal (bdd-to-dnf bdd-type-orig)
                                                     (bdd-to-dnf graph-bdd))
                                              (format html "same DNF:<br>~%")
                                              (format html "different DNF:<br>~%"))
                                          (format html "<ul>~%")
                                          (format html "<li> <pre>original = ~S</pre>~%" (bdd-to-dnf bdd-type-orig))
                                          (format html "<li> <pre>graph    = ~S</pre>~%" (bdd-to-dnf graph-bdd))
                                          (format html "<li> <pre>original = ~S</pre>~%" (bdd-serialize bdd-type-orig))
                                          (format html "<li> <pre>graph    = ~S</pre>~%" (bdd-serialize graph-bdd))
                                          (format html "</ul>~%"))
                                         (t
                                          (format html "ERROR in graph:<br>~%")
                                          (format html "<ul>~%")
                                          (format html "<li>original - graph = ~S<br>~%" (bdd-to-dnf a-not-b))
                                          (format html "<li>original - graph = ~S<br>~%" (reduce-lisp-type (bdd-to-dnf a-not-b)))
                                          (format html "<li>graph - original = ~S<br>~%" (bdd-to-dnf b-not-a))
                                          (format html "<li>graph - original = ~S<br>~%" (reduce-lisp-type (bdd-to-dnf b-not-a)))
                                          (format html "</ul>~%")
                                          )))))
                                  )))
               (print-graph ()
                 (format t "the graph~%")
                 (dolist (node graph)
                   (format t "graph node=~A~%" (node-to-dnf node))
                   (format t "  sub-types=~A~%" (mapcar #'node-to-dnf (getf node :sub-types)))
                   (format t "  super-types=~A~%" (mapcar #'node-to-dnf (getf node :super-types)))
                   (format t "  touches=~A~%" (mapcar #'node-to-dnf (getf node :touches))))
                 (format t "---~%")
                 (dolist (node disjoint-bdds)
                   (format t "disjoint ~A~%" (bdd-to-dnf node)))
                 (format t "---~%"))
               (disjoint-condition (node)
                 (null (or (getf node :touches)
                           (getf node :sub-types)
                           (getf node :super-types))))
               (subset-condition (sub)
                 (null (getf sub :sub-types)))
               (break-sub! (sub super)
                 (setf (getf super :bdd) (bdd-and-not (getf super :bdd)
                                                      (getf sub :bdd))
                       changed t)
                 (no-sub-super! sub super)
                 (remove-nil! super))
               (touching-condition (node)
                 (null (getf node :sub-types)))
               (touch! (n1 n2)
                 (unless (eq n1 n2)
                   (pushnew n1 (getf n2 :touches) :test #'eq)
                   (pushnew n2 (getf n1 :touches) :test #'eq)))
               (no-touch! (n1 n2)
                 (remfq n2 (getf n1 :touches))
                 (remfq n1 (getf n2 :touches)))
               (sub-super! (sub super)
                 (no-touch! sub super)
                 (pushnew sub (getf super :sub-types) :test #'eq)
                 (pushnew super (getf sub :super-types) :test #'eq))
               (no-sub-super! (sub super)
                 (remfq sub (getf super :sub-types))
                 (remfq super (getf sub :super-types)))
               (remove-nil! (node)
                 (when (eq *bdd-false* (getf node :bdd))
                   (dolist (touch (getf node :touches))
                     (no-touch! node touch))
                   ;; (dolist (sub (getf node :sub-types))
                   ;;   (warn "child of nil ~A assuming it is nil~%"
                   ;;         (node-to-dnf sub))
                   ;;   (setf (getf sub :bdd) *bdd-false*))
                   (dolist (super (getf node :super-types))
                     (no-sub-super! node super))
                   (setf graph (remove node graph :test #'eq))
                   #+:bdd-debug (dot "remove-nil!" node)
                   ;;(mapc #'remove-nil! (getf node :sub-types))
                   ))
               (break-touch! (node-x node-y)
                 (setf changed t)
                 (no-touch! node-x node-y)
                 (let* ((bdd-x (getf node-x :bdd))
                        (bdd-y (getf node-y :bdd))
                        (supers-xy (union (getf node-x :super-types)
                                          (getf node-y :super-types) :test #'eq))
                        (touches-xy (intersection (getf node-x :touches)
                                                  (getf node-y :touches) :test #'eq))
                        (node-xy (list :bdd (bdd-and bdd-x bdd-y)
                                       :super-types nil
                                       :sub-types nil
                                       :touches nil
                                       :id (incf node-id))))
                   (unless (eq *bdd-false* (getf node-xy :bdd))
                     (push node-xy graph)
                     (dolist (super supers-xy)
                       (sub-super! node-xy super))
                     (dolist (touch (set-difference touches-xy supers-xy :test #'eq))
                       (touch! node-xy touch)))
                   (let ((new-bdd-x (bdd-and-not bdd-x bdd-y))
                         (new-bdd-y (bdd-and-not bdd-y bdd-x)))
                     (setf (getf node-x :bdd) new-bdd-x 
                           (getf node-y :bdd) new-bdd-y)
                     (remove-nil! node-x)
                     (remove-nil! node-y))))
               (break-loop! (D)
                 (when D
                   (let* ((B (car (getf D :touches)))
                          (D-bdd (getf D :bdd))
                          (B-bdd (getf B :bdd))
                          (BD (list :bdd (bdd-and B-bdd D-bdd)
                                    :super-types nil ;; to fill in below
                                    :sub-types nil
                                    :touches nil ;; to fill in below
                                    :id (incf node-id))))
                     (no-touch! B D)
                     (unless (eq (getf BD :bdd) *bdd-false*)
                       (dolist (touch (getf D :touches))
                         (touch! BD touch))
                       (dolist (super (getf D :super-types))
                         (sub-super! BD super))
                       (sub-super! BD B)
                       (sub-super! BD D)
                       (push BD graph))
                     (setf (getf D :bdd) (bdd-and-not D-bdd B-bdd))
                     (remove-nil! D))
                   (setf changed t))))

        ;; setup all the :super-types, :sub-types, and :touches lists
        (loop :for tail :on graph
              :do (loop :for node2 :in (cdr tail)
                        :with node1 = (car tail)
                        :do
                           (let ((bdd1 (getf node1 :bdd))
                                 (bdd2 (getf node2 :bdd)))
                             (cond
                               ((bdd-disjoint-types-p bdd1 bdd2)) ;skip
                               ((bdd-subtypep bdd1 bdd2)
                                (sub-super! node1 node2))
                               ((bdd-subtypep bdd2 bdd1)
                                (sub-super! node2 node1))
                               (t ;; touching
                                (touch! node1 node2))))))

        (setf changed t)
        #+:bdd-debug (dot "given")
        (while changed
          (setf changed nil)
          (dolist (node graph)
            (when (disjoint-condition node)
              (disjoint! node)
              #+:bdd-debug (dot "disjoint!" node)))

          ;; ;; first break touches which don't have super-types nor sub-types
          ;; (while (exists node graph
          ;;          (and (null (getf node :sub-types))
          ;;               (null (getf node :super-types))
          ;;               (exists touch (getf node :touches)
          ;;                 (and (null (getf touch :sub-types))
          ;;                      (null (getf touch :super-types))))))
          ;;   (dolist (node graph)
          ;;     (unless (or (getf node :sub-types)
          ;;                 (getf node :super-types))
          ;;       (dolist (touch (getf node :touches))
          ;;         (unless (or (getf touch :sub-types)
          ;;                     (getf touch :super-types))
          ;;           (break-touch! node touch)
          ;;           #+:bdd-debug (dot "break-touch! no hierarchy" node touch))))))

          ;; next break child -> parent if child touches nothing and has no sub-types
          (dolist (node graph)
            ;; subset condition
            (when (subset-condition node)
              (dolist (super (getf node :super-types))
                (break-sub! node super)
                #+:bdd-debug (dot "break-sub!" node super)
                (dolist (sibling (getf super :sub-types))
                  (cond ((eq node sibling))
                        ((member sibling (getf node :touches) :test #'eq)
                         (no-sub-super! sibling super)
                         (touch! sibling super)))))))
          (dolist (node graph)
            ;; touching connections
            (when (touching-condition node)
              (dolist (neighbor (getf node :touches))
                (when (touching-condition neighbor)
                  (break-touch! node neighbor)
                  #+:bdd-debug (dot "break-touch!" node neighbor)))))
          (unless changed
            (let ((problematic (find-if (lambda (n) (and (null (getf n :sub-types))
                                                         (getf n :touches)))
                                        graph)))
              (break-loop! problematic)
              #+:bdd-debug (when problematic
                             (dot "break-loop!" problematic)))))
        
        (when graph
          (print-graph)
          (error "graph nodes remain~%"))

        (mapcar #'bdd-to-dnf disjoint-bdds)))))

(defun decompose-types-bdd-graph (type-specifiers)
  (bdd-with-new-hash (lambda ()
                       (%decompose-types-bdd-graph type-specifiers))))
