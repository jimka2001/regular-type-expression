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
;;(pushnew :decompose-graph *features*)
(setf *features* (remove :decompose-graph *features*))


(defun all-type-specifiers (package)
  (let (all-types)
    (do-external-symbols (sym package)
      (when (valid-type-p sym)
	(push sym all-types)))
    all-types))

(defun find-duplicates (data &key (test #'eql) (key #'identity))
  (remove-duplicates
   (mapcon (lambda (tail &aux (item (car tail)))
	      (when (member item (cdr tail) :test test :key key)
		(list item)))
	    data)
   :test test
   :key key))

(deftype type-option ()
  '(cons t null))

(defgeneric graph-node-id (node))
(defgeneric type-option (node))
(defgeneric (setf type-option) (value node))
(defclass graph-node ()
  ((id :initarg :id :reader graph-node-id)
   (type-option :accessor type-option)
   (original)
   (super-type-options :initarg :super-type-options :accessor super-type-options :initform nil)
   (sub-type-options :initarg :sub-type-options :accessor sub-type-options :initform nil)
   (touch-options :initarg :touch-options :accessor touch-options :initform nil)))

(defgeneric type-specifier (node))
(defgeneric (setf type-specifier) (value node))

(defmethod type-specifier ((node graph-node))
  (car (type-option node)))
(defmethod (setf type-specifier) (value (node graph-node))
  "keep the same cons cell containing the type-option"
  #+:decompose-graph (assert (typep (type-option node) '(cons t null)))
  (setf (car (type-option node)) value))

(defmethod initialize-instance :after ((node graph-node) &key type &allow-other-keys)
  (setf (slot-value node 'original) type)
  (setf (type-option node) (list type)))

(defmethod print-object ((node graph-node) stream)
  (print-unreadable-object (node stream :type t :identity nil)
    (format stream "~D: ~A" (graph-node-id node) (type-specifier node))))


(defgeneric graph-to-dot (graph stream))

(defmethod graph-to-dot (graph (filename string))
  (graph-to-dot graph (pathname filename)))

(defmethod graph-to-dot (graph (pathname pathname))
  (with-open-file (stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (graph-to-dot graph stream)))

(defmethod graph-to-dot (graph (stream (eql nil)))
  (with-output-to-string (str)
    (graph-to-dot graph str)))

(defmethod graph-to-dot (graph (stream (eql t)))
  (graph-to-dot graph *standard-output*))

(defmethod graph-to-dot (graph (stream stream))
  (declare (notinline sort)
           (type list graph))
  (let ((graph (sort (copy-list graph) #'< :key (lambda (node)
						  (graph-node-id node)))))
    (labels ((find-name (type-option)
               (declare (type type-option type-option))
	       (graph-node-id (find-node type-option graph)))
	     (print-comments ()
	       (dolist (node graph)
                 (declare (type graph-node node))
		 (format stream "// ~D " (graph-node-id node))
		 (write (type-specifier node) :stream stream :pretty nil)
		 (format stream "~%")))
	     (connect (a b)
               (declare (type type-option a b))
	       (let ((a (find-name a))
		     (b (find-name b)))
		 (format stream "    ~D -> ~D~%" a b)))
	     (print-node-defs ()
	       (dolist (node graph)
                 (declare (type graph-node node))
		 (format stream "  ~D ; // " (graph-node-id node))
		 (write (reduce-lisp-type (type-specifier node)) :stream stream :pretty nil)
		 (terpri stream)))
	     (print-touching ()
	       (format stream "  subgraph Rel1 { // touches~%")
	       (format stream "    edge [dir=none, color=green]~%")
	       (dolist (node graph)
                 (declare (type graph-node node))
		 (dolist (touch (touch-options node))
                   (when (< (graph-node-id node) (find-name touch))
                     (connect (type-option node) touch))))
	       (format stream "  }~%"))
	     (print-sub-super ()
	       (format stream "  subgraph Rel2 { // supersets~%")
	       (format stream "    edge [color=blue]~%")
	       (dolist (node graph)
                 (declare (type graph-node node))
		 (dolist (super (super-type-options node))
		   (connect (type-option node) super)))
	       (format stream "  }~%")))
      (print-comments)
      (format stream "digraph G {~%")
      (format stream "  rankdir=BT ;~%")
      (print-node-defs)
      (print-touching)
      (print-sub-super)
      (format stream "}~%"))))

(defun find-node (type-option graph)
  (declare (type type-option type-option)
           (type list graph))
  (find type-option graph :test #'eq :key #'type-option))




(defun eventual-super (sub-node super-node graph)
  (declare (type graph-node sub-node super-node)
           (type list graph))
  ;; #+:decompose-graph (format t "searching ~D <: ~D~%" (graph-node-id sub-node) (graph-node-id super-node))
  (labels ((recure (super-node chain)
             (declare (type graph-node super-node))
             ;; #+:decompose-graph (format t "   searching? ~D  ~A~%" (graph-node-id super-node) chain)
             (when (or (member (type-option sub-node) (sub-type-options super-node) :test #'eq)
                       (exists ss (sub-type-options super-node)
                         (declare (type type-option ss))
                         ;;#+:decompose-graph (format t "      find ~A = ~A~%" ss (find-node ss graph))
                         (let ((node (find-node ss graph)))
                           (and (not (member (graph-node-id node) chain))
                                (recure (find-node ss graph) (cons (graph-node-id node) chain))))))
               ;; #+:decompose-graph (format t "found sub-super: ~A~%" chain)
               t)))
    (recure super-node (list (graph-node-id super-node)))))

(defun verify-graph (graph &rest notes)
  "Used for debugging, this function traverses the given GRAPH and checks the structure and redundancies."
  (labels ((memq (a b)
	     (member a b :test #'eq))
	   (disjoint? (t1 t2)
	     (declare (type type-option t1 t2))
	     (disjoint-types-p (car t1) (car t2))))

    (assert (null (find-duplicates graph :test #'eq :key #'type-option)) (notes graph)
	    "duplicates in graph nodes: ~A" (find-duplicates graph :key #'type-option :test #'eq))
    (dolist (node graph)
      (declare (type graph-node node)
               (notinline typep))
      (assert (typep node 'graph-node)
	      (notes node)
	      "corrupt car of node")
      (assert (typep (type-option node) 'cons)
	      (notes graph node)
	      "expecting a cons as type-option: not ~A" (type-option node))
      (assert (not (member (type-option node) (touch-options node) :test #'eq)) (notes node)
	      "node touches itself")
      (assert (not (member (type-option node) (super-type-options node) :test #'eq)) (notes node)
	      "node is super-type of itself")
      (assert (not (member (type-option node) (sub-type-options node) :test #'eq)) (notes node)
	      "node is sub-type of itself")
	      
      (assert (null (find-duplicates (sub-type-options node))) (notes node)
	      "duplicates in sub-types: ~A" (find-duplicates (sub-type-options node)))
      (mapc (lambda (sub-type &aux (sub-type-node (find-node sub-type graph)))
	      (assert (typep sub-type 'cons)
		      (notes node) "corrupted sub-types")
	      (assert sub-type-node
		      (notes node sub-type graph) "node ~S references sub-type ~A which does not exist:~%~A"
                      (graph-node-id node) sub-type node)
	      (assert (memq (type-option node) (super-type-options sub-type-node)) (notes node)
		      "sub-type is missing this node as super-type"))
	    (sub-type-options node))
      (assert (null (find-duplicates (super-type-options node) :test #'eq)) (notes node)
	      "duplicates in super-types: ~A" (find-duplicates (super-type-options node) :test #'eq))
      (mapc (lambda (super-type &aux (super-type-node (find-node super-type graph)))
	      (assert (typep super-type 'cons)
		      (notes node) "corrupted super-types")
	      (assert super-type-node
		      (notes node super-type) "node ~D references super-type ~A which does not exist"
                      (graph-node-id node) super-type)
	      (assert (memq (type-option node) (sub-type-options super-type-node)) (notes node)
		      "super-type is missing this node as sub-type ~%~A~%~A" node super-type-node))
	    (super-type-options node))
      (assert (null (find-duplicates (touch-options node) :test #'eq)) (notes node)
	      "duplicates in touches: ~A" (find-duplicates (touch-options node) :test #'eq))
      (mapc (lambda (touch &aux (touch-node (find-node touch graph)))
	      (assert (typep touch 'cons)
		      (notes node) "corrupted touches")
	      (assert touch-node (notes node touch) "node ~A references touch ~A which does not exist" node touch)
	      (assert (memq (type-option node) (touch-options touch-node)) (notes node) "touch node mismatch"))
	    (touch-options node))
      (dolist (n2 graph)
        (declare (type graph-node n2))
        (cond
          ((eq node n2))
          ((< (graph-node-id node) (graph-node-id n2)))
          ((disjoint? (type-option node) (type-option n2)))
          ((member (type-option n2) (touch-options node) :test #'eq))
          ((eventual-super n2 node graph))
          ((eventual-super node n2 graph))
          (t
           (format t "missing touch ~A -> ~A~%" (graph-node-id node) (graph-node-id n2))
           #+:decompose-graph (create-png graph :message 221))))
        )))


(defun diff-files (file1 file2)
  (with-open-file (s1 file1 :direction :input :if-does-not-exist :error)
    (with-open-file (s2 file2 :direction :input :if-does-not-exist :error)
      (while t
        (let ((c1 (read-char s1 nil s1))
              (c2 (read-char s2 nil s2)))
          (cond
            ((and (eql c1 s1)
                  (eql c2 s2))
             ;; reached eof at same time
             (return-from diff-files nil))
            ((or (eql c1 s1)
                 (eql c2 s2))
             ;; one reached eof but other didnt
             (return-from diff-files t))
            ((not (eql c1 c2))
             ;; read different character
             (return-from diff-files t))))))))

(defvar *iteration* 0)
(defvar *previous-dot* "/dev/null")
(defun create-png (graph &key (disjoint-types nil) (message "")
                   &aux (dot-file (format nil "/tmp/jnewton/graph/graph-~2,'0D.dot" (incf *iteration*)))
                     (out (format nil "/tmp/jnewton/graph/graph-~2,'0D.png" *iteration*)))
  (declare (notinline sort))
  (flet ((count-touches ()
           (/ (loop :for node :in graph
                    :summing (length (touch-options node)))
              2))
         (count-supers ()
           (loop :for node :in graph
                 :summing (length (super-type-options node)))))

    (format t "============= ~A =============~%" message)
    (let ((graph (sort (copy-list graph) #'< :key #'graph-node-id)))
      (dolist (node graph)
        (declare (type graph-node node))
        (format t "~A: " (graph-node-id node))
        (write (type-specifier node) :stream t :pretty nil)
        (terpri t)))
    (dolist (type disjoint-types)
      (format t "disjoint: ")
      (write type :stream t :pretty nil)
      (terpri t))
    (graph-to-dot graph dot-file)
    (sb-ext:run-program "dot" (list "-Tpng" dot-file
                                    "-o" out)
                        :search t)
    (sb-ext:run-program "cp" (list "-f" "/tmp/jnewton/graph/graph.png" "/tmp/jnewton/graph/graph-previous.png")
                        :search t)
    (sb-ext:run-program "cp" (list "-f" out "/tmp/jnewton/graph/graph.png")
                        :search t)
    (format t "created ~A~%" out)
    (when (diff-files *previous-dot* dot-file)
      (y-or-n-p "continue nodes=~D, touches=~D, supers=~D?"
                (length graph) (count-touches) (count-supers)))
    (setf *previous-dot* dot-file)))


(defun decompose-types-graph (type-specifiers &key (reduce t))
  (decompose-by-graph-1 type-specifiers :graph-class 'sexp-graph))


