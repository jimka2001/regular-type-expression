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
              (with-open-file (stream out :direction :output :if-exists :supersede :if-does-not-exist :create)
                (bdd-graph-to-dot graph stream)))
             ((string= "png" (pathname-type pathname))
              (let ((dot-path (merge-pathnames (make-pathname :type "dot") pathname)))
                (bdd-graph-to-dot graph (namestring dot-path))
                (run-program "dot" (list "-Tpng" (namestring dot-path)
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
 
(eval-when (:compile-toplevel :load-toplevel :execute)
   (defvar *sort-strategies*
     `((:sort-nodes ,#'(lambda (graph)
                         (shuffle-list graph))
        :sort-strategy "SHUFFLE")
       (:sort-nodes ,(lambda (graph)
                       (declare (notinline sort))
                       (sort graph #'< :key #'count-connections-per-node))
        :sort-strategy "INCREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare (notinline sort))
                       (sort graph #'> :key #'count-connections-per-node))
        :sort-strategy "DECREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare (notinline sort))
                       (sort graph #'> :key #'count-parents-per-node))
        :sort-strategy "BOTTOM-TO-TOP")
       (:sort-nodes ,(lambda (graph)
                       (declare (notinline sort))
                       (sort graph #'< :key #'count-parents-per-node))
        :sort-strategy "TOP-TO-BOTTOM"))))

(defun find-sort-strategy-function (name)
  (getf (find name *sort-strategies* :test #'string= :key (getter :sort-strategy))
        :sort-nodes))


(defun slow-decompose-types-bdd-graph (type-specifiers
                                   &key
                                     (sort-strategy "DECREASING-CONNECTIONS") ;; "BOTTOM-TO-TOP" or "DECREASING-CONNECTIONS"
                                     (recursive t)
                                     (inner-loop :operation)
                                     (do-break-sub :relaxed)
                                     (do-break-loop nil) ;; t or nil
                                     (sort-nodes (find-sort-strategy-function sort-strategy))
                                     (do-disjoint t)
                                     (do-break-touch t))
  (declare (notinline sort +)
           ;;(ignore sort-strategy)
           (type (member :node :operation) inner-loop)
           (type (member :strict :relaxed) do-break-sub)
           (type (function (list) list) sort-nodes)
           (type list type-specifiers)
           (optimize (speed 3) (compilation-speed 0) (debug 0) (space 0))
           ;;(optimize (speed 0) (compilation-speed 0) (debug 3) )
           )

  (when (eq :strict do-break-sub)
    (assert do-break-loop (do-break-sub do-break-loop)
            "Unsupported combination do-break-loop=~A do-break-sub=~A" do-break-loop do-break-loop))
  (when (eq :node inner-loop)
    (assert (not recursive) (recursive inner-loop)
            "Unsupported combination recursive=~A inner-loop=~A" recursive inner-loop))
  (let* ((node-id 0)
         (bdds (remove-duplicates
                (sort (mapcar #'bdd type-specifiers)
                      #'>
                      :key #'(lambda (bdd)
                               (declare (notinline length))
                               (length (bdd-collect-atomic-types bdd))))
                :test #'bdd-type-equal))
         (bdd-type-orig (bdd `(or ,@type-specifiers)))
         (graph (loop :for bdd :in bdds
                      :collect (list :bdd bdd
                                     :super-types nil
                                     :sub-types nil
                                     :touches nil
                                     :id (incf node-id)
                                     :removed nil))))

    (let ((changed 0)
          (c 1000)
          (html-file "/tmp/jnewton/graph.html")
          disjoint-bdds)
      (declare (type (and unsigned-byte fixnum) changed))
      (labels ((while-changed (thunk)
                 (while (plusp changed)
                   (setf changed 0)
                   (funcall thunk)))
               (disjoint! (node)
                 (when (member node graph :test #'eq)
                   (incf changed)
                   (setf graph (remove node graph :test #'eq))
                   (setf (getf node :removed) t)
                   (unless (eq (getf node :bdd) *bdd-false*)
                     (pushnew (getf node :bdd) disjoint-bdds :test #'bdd-type-equal))
                   t))
               (node-to-dnf (node)
                 (declare (type cons node))
                 (bdd-to-dnf (getf node :bdd)))
               (getter (prop)
                 (lambda (plist)
                   (getf plist prop)))
               (dot (comment &rest nodes)
                 (let ((*print-case* :downcase)
                       (png-file (format nil "/tmp/jnewton/graph-~D.png" (incf c)))
                       (graph-bdd (reduce #'bdd-or (mapcar (getter :bdd) graph)
                                          :initial-value (reduce #'bdd-or disjoint-bdds
                                                                 :initial-value *bdd-false*))))
                   (with-open-file (html html-file :direction :output
                                                   :if-exists :append
                                                   :if-does-not-exist :create )
                     (unless html
                       (format t "cannot write graph to ~A~%" html-file)
                       (return-from dot))
                     (format t "writing graph to ~A~%" html-file)
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
                             (format html "<li> <pre>DNF of original = ~S</pre>~%" (bdd-to-dnf bdd-type-orig))
                             (format html "<li> <pre>DNF of graph    = ~S</pre>~%" (bdd-to-dnf graph-bdd))
                             (format html "<li> <pre>BDD of original = ~S</pre>~%" (bdd-serialize bdd-type-orig))
                             (format html "<li> <pre>BDD of graph    = ~S</pre>~%" (bdd-serialize graph-bdd))
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
               (node-id (node)
                 (getf node :id))
               (print-graph (comment)
                 (dot comment)
                 (format t "the graph  ~D nodes~%" (length graph))
                 (format t "graph nodes: ~A~%"
                         (mapcar #'node-id graph))
                 (dolist (node graph)
                   (format t "graph node[~A]=~A~%" (node-id node) (node-to-dnf node))
                   (format t "  sub-types=~A~%  ~A~%"
                           (mapcar #'node-id (getf node :sub-types))
                           (mapcar #'node-to-dnf (getf node :sub-types)))
                   (format t "  super-types=~A~%  ~A~%"
                           (mapcar #'node-id (getf node :super-types))
                           (mapcar #'node-to-dnf (getf node :super-types)))
                   (format t "  touches=~A~%  ~A~%"
                           (mapcar #'node-id (getf node :touches))
                           (mapcar #'node-to-dnf (getf node :touches))))
                 (format t "---~%")
                 (dolist (node disjoint-bdds)
                   (format t "disjoint ~A~%" (bdd-to-dnf node)))
                 (format t "---~%"))
               (disjoint-condition (node)
                 (null (or (getf node :touches)
                           (getf node :sub-types)
                           (getf node :super-types))))
               (break-sub! (sub super)
                 (setf (getf super :bdd) (bdd-and-not (getf super :bdd)
                                                      (getf sub :bdd)))
                 (incf changed)
                 (no-sub-super! sub super)
                 (remove-nil! super))
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
                 (declare (notinline union +))
                 (incf changed)
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
                   (let* ((neighbors (sort (setof touch (getf D :touches)
                                             (getf touch :sub-types))
                                           #'< :key (lambda (node)
                                                      (length (getf node :super-types)))))
                          ;; of all the neighbors of D with sub-types, select the one with the
                          ;; least number of super-types, hopefully with 0 supertypes.
                          (B (car neighbors)) 
                          (D-bdd (getf D :bdd))
                          (B-bdd (getf B :bdd))
                          (BD (list :bdd (bdd-and B-bdd D-bdd)
                                    :super-types nil ;; to fill in below
                                    :sub-types nil
                                    :touches nil ;; to fill in below
                                    :id (incf node-id))))
                     (assert B () "break-loop! called to break a loop which does not exist")
                     (no-touch! B D)
                     (unless (eq (getf BD :bdd) *bdd-false*)
                       (dolist (touch (getf D :touches))
                         (touch! BD touch))
                       (sub-super! BD B)
                       (sub-super! BD D)
                       (dolist (super (getf BD :super-types))
                         (dolist (super-super (getf super :super-types))
                           (sub-super! BD super-super)))
                       (push BD graph))
                     (setf (getf D :bdd) (bdd-and-not D-bdd B-bdd))
                     (remove-nil! D))
                   (incf changed)))
               (do-disjoint (node)
                 (when (disjoint-condition node)
                   (disjoint! node)
                   #+:bdd-debug (dot "disjoint!" node)))
               (do-break-sub-strict (node)
                 (when (and (null (getf node :sub-types))
                            (null (getf node :touches)))
                   (dolist (super (getf node :super-types))
                     (break-sub! node super)
                     #+:bdd-debug (dot "break-sub!" node super))
                   (do-disjoint node)))
               (do-break-sub-relaxed (node)
                 ;; break child -> parent if child touches nothing and has no sub-types
                 ;; -- subset condition
                 (when (null (getf node :sub-types))
                   (dolist (super (getf node :super-types))
                     (break-sub! node super)
                     #+:bdd-debug (dot "break-sub!" node super)
                     (dolist (sibling (getf super :sub-types))
                       (cond ((eq node sibling))
                             ((member sibling (getf node :touches) :test #'eq)
                              (no-sub-super! sibling super)
                              (touch! sibling super)))))
                   (do-disjoint node)))
               
               (do-break-touch (node)
                 ;; touching connections
                 (when (null (getf node :sub-types))
                   (dolist (neighbor (getf node :touches))
                     (when (null (getf neighbor :sub-types))
                       (break-touch! node neighbor)
                       #+:bdd-debug (dot "break-touch!" node neighbor)))
                   (do-disjoint node)))
               (do-break-loop (node)
                 (when (null (getf node :sub-types))
                   (while (exists touch (getf node :touches)
                            (getf touch :sub-types))
                     (break-loop! node))
                   (do-disjoint node))))

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
        
        (setq graph (funcall sort-nodes graph))
        
        (incf changed)
        #+:bdd-debug (dot "given")
        (let ((operations (nconc (when do-disjoint
                                   (list #'do-disjoint))
                                 (ecase do-break-sub
                                   ((:strict)
                                    (list #'do-break-sub-strict))
                                   ((:relaxed)
                                    (list #'do-break-sub-relaxed)))
                                 (when do-break-touch
                                   (list #'do-break-touch))
                                 (when do-break-loop
                                   (list #'do-break-loop)))))

          (ecase inner-loop
            ((:node)
             (while-changed (lambda ()
                              (dolist (operation operations)
                                (declare (type function operation))
                                (dolist (node graph)
                                  (funcall operation node))))))
            ((:operation)
             (labels ((do-operations (node &key (level 0) lineage)
                        (unless (getf node :removed)
                          (when recursive
                            (dolist (sub (getf node :sub-types))
                              (do-operations sub :level (1+ level) :lineage (cons node lineage))))
                          (dolist (operation operations)
                            (declare (type function operation))
                            (funcall operation node)))))
               (while-changed (lambda ()
                                (mapc #'do-operations graph)))))))

        (when graph
          (print-graph "nodes remain" )
          (error "~D graph nodes remain~%" (length graph)))

        (mapcar #'bdd-to-dnf disjoint-bdds)))))


(defun count-connections-per-node (node)
  (+ (length (getf node :touches))
     (length (getf node :sub-types))
     (length (getf node :super-types))))

(defun count-parents-per-node (node)
  (length (getf node :super-types)))

(defun decompose-types-bdd-graph (type-specifiers)
  (decompose-by-graph-1 type-specifiers :graph-class 'bdd-graph))


(defun decompose-types-bdd-graph-recursive-increasing-connections (type-specifiers)
  (bdd-call-with-new-hash (lambda ()
                       (slow-decompose-types-bdd-graph type-specifiers
                                                   :sort-strategy "INCREASING-CONNECTIONS"
                                                   :inner-loop :recursive))))

(defmacro make-decompose-fun-combos ()
  (let (fun-defs
        prop-defs
        fun-names
        ( operation-combos '((:do-break-sub :strict  :do-break-loop t)
                             (:do-break-sub :relaxed :do-break-loop nil)
                             (:do-break-sub :relaxed :do-break-loop t)))
        ( inner-loops '((:inner-loop :node      :recursive nil)
                        (:inner-loop :operation :recursive t)
                        (:inner-loop :operation :recursive nil)))
        ( sort-nodes (mapcar (getter :sort-strategy) *sort-strategies*))

        )
    (dolist (sort-nodes-arg sort-nodes)
      (dolist (inner-loop-args inner-loops)
        (destructuring-bind (&key inner-loop recursive) inner-loop-args
          (dolist (operation-combo-args operation-combos)
            (destructuring-bind (&key do-break-sub do-break-loop) operation-combo-args
              (let* ((symbol (concatenate 'string
                                          "DECOMPOSE-TYPES-BDD-GRAPH-"
                                          (symbol-name do-break-sub)
                                          "/"
                                          "BREAK-LOOP="
                                          (if do-break-loop "YES" "NO")
                                          "/"
                                          (symbol-name inner-loop)
                                          "/"
                                          "RECURSIVE="
                                          (if recursive "YES" "NO")
                                          "/"
                                          sort-nodes-arg))
                     (fun-name (intern symbol (find-package "LISP-TYPES")))
                     (props `(:sort-strategy ,sort-nodes-arg
                              ,@inner-loop-args
                              ,@operation-combo-args)))

                (push `(setf (get ',fun-name 'decompose-properties) ',props) prop-defs)
                (push `(defun ,fun-name (type-specifiers)
                         (bdd-call-with-new-hash (lambda ()
                                              (slow-decompose-types-bdd-graph type-specifiers ,@props))))
                      fun-defs)))))))
    (setf fun-names (mapcar #'cadr fun-defs))
    `(progn
       (defvar *decompose-fun-names* ',fun-names)
       ,@prop-defs
       ,@fun-defs)))

(make-decompose-fun-combos)
