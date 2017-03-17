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
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
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
  (declare (type list type-specifiers)
	   (notinline assoc sort)
	   ;;(optimize (speed 3) (debug 0) (compilation-speed 0))
           )
  (setf *iteration* 0)
  (let* (disjoint-types
	 (node-id 0)
	 (graph (loop :for type-specifier :in (remove-duplicates type-specifiers
                                                                 :test #'equivalent-types-p)
		      :collect (make-instance 'graph-node
                                              :type type-specifier
                                              :id (incf node-id)))))
    (declare (type list graph))
    (labels (
             #+:decompose-graph (dot (&key (message ""))
                                  (create-png graph :message message :disjoint-types disjoint-types))

	     (unionq (a b)
	       (declare (type list a b)
			(notinline union))
	       (union a b :test #'eq))
	     (removeq (a b)
	       (remove a b :test #'eq))
	     (set-differenceq (a b)
	       (declare (notinline set-difference))
	       (set-difference a b :test #'eq))
	     (intersectionq (a b)
	       (intersection a b :test #'eq))

	     (subtype? (type-option-1 type-option-2)
	       (declare (notinline typep)
			(type cons type-option-1 type-option-2))
	       (smarter-subtypep (car type-option-1) (car type-option-2)))
	     (disjoint? (t1 t2)
	       (declare (type type-option t1 t2))
	       (disjoint-types-p (car t1) (car t2)))
	     (empty? (type)
	       (subtypep type nil))
             (remove-node (node &aux (type-option (type-option node)))
               (declare (type graph-node node))
               (setf graph (removeq node graph))
               (dolist (n graph)
                 (declare (type graph-node n))
                 (when (member type-option (touch-options n) :test #'eq)
                   (warn "while deleting ~A, found node ~A touching it, repairing." node n)
                   (setf (touch-options n) (removeq type-option (touch-options n))))
                 (when (member type-option (sub-type-options n))
                   (warn "while deleting ~A, found node ~A who has it as subtype, repairing." node n)
                   (setf (sub-type-options n) (removeq type-option (sub-type-options n))))
                 (when (member type-option (super-type-options n))
                   (warn "while deleting ~A, found node ~A who has it as supertype, repairing." node n)
                   (setf (super-type-options n) (removeq type-option (super-type-options n))))))
             (nodes-touch! (n1 n2 &aux (t1 (type-option n1)) (t2 (type-option n2)))
               (declare (type graph-node n1 n2))
               (unless (eq t1 t2)
                 (pushnew t1 (touch-options n2) :test #'eq)
                 (pushnew t2 (touch-options n1) :test #'eq)))
             (nodes-no-touch! (n1 n2 &aux (t1 (type-option n1)) (t2 (type-option n2)))
               (declare (type graph-node n1 n2))
               (setf (touch-options n2) (removeq t1 (touch-options n2)))
               (setf (touch-options n1) (removeq t2 (touch-options n1))))
             (nodes-sub-super! (n-sub n-super &aux (t-sub (type-option n-sub)) (t-super (type-option n-super)))
               (declare (type graph-node n-sub n-super))
               (nodes-no-touch! n-sub n-super)
               (pushnew t-sub (sub-type-options n-super) :test #'eq)
               (pushnew t-super (super-type-options n-sub) :test #'eq))
             (nodes-no-sub-super! (n-sub n-super &aux (t-sub (type-option n-sub)) (t-super (type-option n-super)))
               (declare (type graph-node n-sub n-super))
               (setf (sub-type-options n-super)
                     (removeq t-sub (sub-type-options n-super)))
               (setf (super-type-options n-sub)
                     (removeq t-super (super-type-options n-sub))))
	     (type-intersection (t1 t2)
               ;; t1 and t1 are lisp type specifiers
	       (let ((t3 `(and ,t1 ,t2)))
		 (cond
		   ((empty? t3)
		    nil)
		   (reduce
		    (reduce-lisp-type t3))
		   (t
		    t3))))
	     (new-super-type (new-type-option node &aux (super-type-options (super-type-options node)))
	       (declare (type type-option new-type-option)
			(type graph-node node))
	       (cond
		 ((null super-type-options)
                  (nodes-sub-super! node (find-node new-type-option graph))
                  #+:decompose-graph (format t "case 1: ~A~%" (super-type-options node))
                  )
		 ((exists type-option super-type-options
		    ;; new=(number) super-types=((fixnum))
		    (subtype? type-option new-type-option))
                  #+:decompose-graph (format t "case2 ignoring because ~A already contains a subtype of ~A~%"
                                             super-type-options new-type-option)
		  nil)
		 (t
                  #+:decompose-graph (format t "case 3~%")
		  ;; new=(fixnum) super-types='((number) (A) (B))
                  ;; remove transitive super-class relations
                  (dolist (super-type-option (super-type-options node))
                    (declare (type type-option super-type-option))
                    (when (subtype? new-type-option super-type-option)
                      (nodes-no-sub-super! node (find-node super-type-option graph))))
                  (nodes-sub-super! node (find-node new-type-option graph))))))
      (mapl (lambda (tail &aux (t1-node (car tail)) (t2-tn (cdr tail)) (t1 (type-option t1-node)))
              (declare (type graph-node t1-node))
	      (mapc (lambda (t2-node &aux (t2 (type-option t2-node)))
                      (declare (type graph-node t2-node))
                      (cond
                        ((subtype? t1 t2)
                         #+:decompose-graph (format t "~A <: ~A~%" t1 t2)
                         (if (subtype? t2 t1)
                             (error "equivalent types ~A and ~A" t1 t2)
                             (new-super-type t2 t1-node)))
                        ((subtype? t2 t1)
                         #+:decompose-graph (format t "~A <: ~A~%" t2 t1)
                         (new-super-type t1 t2-node))
                        ((disjoint? t1 t2)
                         #+:decompose-graph (format t "~A disjoint ~A~%" t1 t2)
                         nil)
                        (t		; touches
                         #+:decompose-graph (format t "~A touches ~A~%" t2 t1)
                         ;; this includes the case that subtypep returns NIL;NIL in both directions
                         (cond
                           ((null (nth-value 1 (subtype? t1 t2)))
                            (warn 'ambiguous-subtype :sub t1 :super t2))
                           ((null (nth-value 1 (subtype? t2 t1)))
                            (warn 'ambiguous-subtype :sub t2 :super t1)))
                         (nodes-touch! t1-node t2-node))))
		      t2-tn))
	    graph)

      #+:decompose-graph (dot :message 364)
      
      (setf graph (sort graph #'> :key (lambda (node)
					 (length (super-type-options node)))))

      #+:decompose-graph (dot :message 369)

      (let ((status 'initial))
	(labels (
                 (%copy-list (obj)
                   (if (atom obj)
                       obj
                       (copy-list obj)))
	
		 ;; everything which has super-types but no sub-types and no touches
		 (disjoin-subtypes! ()
		   "Find nodes which have super-types but no subtypes, break the super-type links,
                    being careful not to strand touching neighbors."
                   (dolist (sub-node graph)
                     (declare (type graph-node sub-node))
                     (dolist (super-type (super-type-options sub-node))
                       (let ((super-node (find-node super-type graph)))
                         (declare (type graph-node super-node)) ;; implicit assertion that super-node is non-nil
                         (when (eventual-super super-node sub-node graph)
                           (setf status 'changed)
                           (break-equivalent! sub-node super-node)
                           (return-from disjoin-subtypes!)))))
		   (dolist (node graph)
                     (declare (type graph-node node))
		     (when (and (member node graph :test #'eq) ;; TODO not sure if this is necessary
                                (super-type-options node)
				(not (sub-type-options node)))
                       #+:decompose-graph (dot :message 425)
                       #+:decompose-graph (format t "        disjoin-subtype! ~A~%" (graph-node-id node))
		       (disjoin-subtype! node)
		       #+:decompose-graph (dot :message 375)
		       )))
                 (break-equivalent! (discard-node keep-node)
                   (nodes-no-sub-super! discard-node keep-node)
                   (nodes-no-sub-super! keep-node discard-node)
                   (nodes-no-touch! discard-node keep-node)
                   ;; update the touches of the super-type node
                   (mapc (lambda (nt &aux (nn (find-node nt graph)))
                           (declare (type type-option nt)
                                    (type graph-node nn))
                           (nodes-no-touch! nn discard-node)
                           (nodes-touch! nn keep-node))
                         (touch-options discard-node))
                   ;; update the sub-types of all of the super-types of discard-node
                   (mapc (lambda (st &aux (sn (find-node st graph)))
                           (declare (type type-option st)
                                    (type graph-node sn))
                           (nodes-no-sub-super! discard-node sn)
                           (nodes-sub-super! keep-node sn))
                         (super-type-options discard-node))
                   ;; update the super-types of all of the sub-types of discard-node
                   (mapc (lambda (st &aux (sn (find-node st graph)))
                           (declare (type type-option st)
                                    (type graph-node sn))
                           (nodes-no-sub-super! sn discard-node)
                           (nodes-sub-super! sn keep-node))
                         (sub-type-options discard-node))
                   (remove-node discard-node))
                 
		 ;; one thing which has super-types but no sub-types
		 (disjoin-subtype! (subtype-node)
		   (declare (type graph-node subtype-node))
		   (setf status 'changed)
                   #+:decompose-graph (verify-graph graph)
		   (dolist (super-type (super-type-options subtype-node))
		     (declare (type type-option super-type))
                     #+:decompose-graph (verify-graph graph)
		     (let ((super-node (find-node super-type graph)))
		       (declare (type graph-node super-node))
                       #+:decompose-graph (format t "  disjoin-subtype! ~D->~D~%" (graph-node-id subtype-node)
                                                  (graph-node-id super-node))
                       (nodes-no-sub-super! subtype-node super-node)
		       (dolist (super-super-type (super-type-options super-node))
                         (declare (type type-option super-super-type))
                         (nodes-sub-super! subtype-node (find-node super-super-type graph)))
                       ;; we are "subtracting" some of the superclass,
                       ;;   therefore it could be that the sub-super relation becomes
                       ;;   incorrect for other subclasses of the superclass in the
                       ;;   case that subtype-node touches them.
                       ;;   if so change that sub->super relation to touch
                       (dolist (sibling-option (sub-type-options super-node))
                         (cond ((eq sibling-option (type-option subtype-node)))
                               ((member sibling-option (touch-options subtype-node) :test #'eq)
                                (let ((sibling-node (find-node sibling-option graph)))
                                  (nodes-no-sub-super! sibling-node super-node)
                                  (nodes-touch! sibling-node super-node)
                                  ;; if sibling-node has sub-classes, then those subclasses
                                  ;; are actually subclasses of super-node, yet there is no link
                                  ;; because this algorithm is depending on transitivity of the subtype relation.
                                  ;; thus we need to explicitly make those subtypes of sibling-node touch super-node.
                                  (dolist (sub-sibling-option (sub-type-options sibling-node))
                                    (let ((sub-sibling-node (find-node sub-sibling-option graph)))
                                      ;; we know sub-sibling-node IS NOT a subtype of subtype-node
                                      ;; because subtype-node HAS NO subtypes,
                                      ;; but they might touch.
                                      (cond
                                        ((member sub-sibling-option (touch-options subtype-node) :test #'eq)
                                         (nodes-no-sub-super! sub-sibling-node super-node)
                                         (nodes-touch! sub-sibling-node super-node))
                                        (t
                                         (nodes-no-touch! sub-sibling-node super-node)
                                         (nodes-sub-super! sub-sibling-node super-node))))
                                  )))))
                       
		       (let ((new-type (type-intersection (type-specifier super-node)
							  (list 'not (type-specifier subtype-node)))))
			 (cond
			   (new-type
                            ;; keep the same cons cell containing the type-option
                            (setf (type-specifier super-node)
                                  new-type))
			   (t ;; then the sub-type and super-type are equivalent
			    ;; eliminate the sub-type node from graph
                            (break-equivalent! subtype-node super-node)
                            
                            ;; we must now abort disjoin-subtype! because the node whose
                            ;; super-types we are iterating over is no longer in the graph.
                            (return-from disjoin-subtype!)))))

                     #+:decompose-graph (verify-graph graph)
                     )
                   #+:decompose-graph (format t "  disjoin-subtype! finished~%")
                   #+:decompose-graph (dot :message 503)
                   )
		 ;; everything which touches something but no sub-types
		 (untouch-leaves! ()
		   "Find nodes which have touching neighbors, but no subtypes.  
                    Break the touching links."
		   (dolist (node graph)
                     (declare (type graph-node node))
		     (when (and (touch-options node)
				(not (sub-type-options node)))
		       (untouch-leaf! node)
		       #+:decompose-graph (dot :message 425))))
		 ;; one thing which touches something but no sub-types
		 (untouch-leaf! (node &aux
                                        new-nodes)
		   (declare (type graph-node node))
		   #+:decompose-graph (format t "        untouch-leaf! ~A~%" (graph-node-id node))
		   (dolist (neighbor (touch-options node))
                     (declare (type type-option neighbor))
		     (let ((neighbor-node (find-node neighbor graph)))
                       (declare (type graph-node neighbor-node))
		       ;; make sure neighbor has no sub-types
		       #+:decompose-graph (format t "        neighbor of ~D : ~A~%" (graph-node-id node)
                                                  (graph-node-id neighbor-node))
		       (unless (sub-type-options neighbor-node)
			 (setf status 'changed)
			 (let ((new-type (type-intersection (type-specifier node)
							    (type-specifier neighbor-node))))
			   (cond
			     (new-type
			      (let ((new-node (make-instance 'graph-node
                                                             :type new-type
                                                             :id (incf node-id)
                                                             :original (%copy-list new-type))))
                                #+:decompose-graph (progn (format t "new node: ~D: intersection of~%" (graph-node-id new-node))
                                                          (format t " ~D=~A~%" (graph-node-id node) (type-specifier node))
                                                          (format t " ~D=~A~%  intersection="
                                                                  (graph-node-id neighbor-node) (type-specifier neighbor-node))
                                                          (write (type-specifier new-node) :stream t :pretty nil)
                                                          (terpri t))
                                (dolist (recent new-nodes)
                                  (declare (type graph-node recent))
                                  (unless (disjoint-types-p (type-specifier recent) new-type)
                                    (nodes-touch! recent new-node)))
                                (push new-node new-nodes)
				(push new-node graph)
				;; we must update every N this new-node touches, so that N touches new-node
                                (dolist (touch (intersectionq (touch-options node)
                                                              (touch-options neighbor-node)))
                                  (declare (type type-option touch))
                                  (nodes-touch! (find-node touch graph) new-node))
				;;  update sub-types of each super-type-node to reference this new type
                                (dolist (super-type (unionq (super-type-options node)
                                                            (super-type-options neighbor-node)))
                                  (declare (type type-option super-type))
                                  (nodes-sub-super! new-node (find-node super-type graph))))

			      (let* ((A (type-specifier node))
				     (B (type-specifier neighbor-node))
				     (new-A  (type-intersection A `(not ,B)))
				     (new-B  (type-intersection `(not ,A) B)))
				(mutate new-A node neighbor-node)
				(mutate new-B neighbor-node node)
                                #+:decompose-graph (verify-graph graph)))
			     (t
			      ;; if they are marked as touching but their intersection is void,
			      ;;  then simply update the touches fields
                              (nodes-no-touch! node neighbor-node))))))))
		 (mutate (new-type node neighbor-node)
		   (declare (type graph-node node neighbor-node))
                   #+:decompose-graph (progn (format t "mutate ~A: from " (graph-node-id node))
                                             (write (type-specifier node) :stream t :pretty nil)
                                             (format t " to ")
                                             (write new-type :stream t :pretty nil)
                                             (terpri t))
		   (cond (new-type
                          #+:decompose-graph (format t "  mutate case 1 ~%")
                          ;; descructively change the type-specifier, and all type-options pointing to
                          ;; it will effectively change by consequence
                          (setf (car (type-option node)) new-type)
                          (nodes-no-touch! node neighbor-node))
			 (t
                          #+:decompose-graph (format t "  mutate case 2~%")
			  (assert (null (sub-type-options node)) (node)
				  "we should not be mutating a node with sub-types, the sub-types should be elimianted first")
                          #+:decompose-graph (format t "supers: ~A~%" (super-type-options node))
                          #+:decompose-graph (format t "touches: ~A~%" (touch-options node))
			  (setf graph (removeq node graph))
			  (mapc (lambda (super &aux (super-node (find-node super graph)))
                                  (declare (type type-option super)
                                           (type graph-node super-node))
                                  (nodes-no-sub-super! node super-node))
				(super-type-options node))
			  (mapc (lambda (touch &aux (touch-node (find-node touch graph)))
                                  (declare (type type-option touch)
                                           (type graph-node touch-node))
                                  (nodes-no-touch! node touch-node))
				(touch-options node)))))
		 (disjoint! (&aux (disjoint-nodes (remove-if #'(lambda (node)
                                                                 (declare (type graph-node node))
                                                                 (or (touch-options node)
                                                                     (sub-type-options node)
                                                                     (super-type-options node)))
							     graph)))
		   "Find nodes which have no sub-types, no super-types, and no touching neighbors.
                    Collect the types associated with such nodes into DISJOINT-NODES, and remove
                    from GRAPH."
		   (when disjoint-nodes
		     #+:decompose-graph (format t "        ~D disjoint nodes~%" (length disjoint-nodes))
		     ;(setf status 'changed)
		     (setf graph (set-differenceq graph disjoint-nodes))
		     (dolist (disjoint-node disjoint-nodes)
		       (pushnew (type-specifier disjoint-node)
				disjoint-types
				:test #'equivalent-types-p)) 
		     #+:decompose-graph (dot :message 520)
		     )))
          
          #+:decompose-graph (dot :message 522)

	  (while (and graph
		      (not (eq 'unchanged status)))
	    (setf status 'unchanged)

	    #+:decompose-graph (format t "      disjoint!~%")
            (disjoint!)
            #+:decompose-graph (dot :message 535)
            #+:decompose-graph (format t "      untouch-leaves!~%")
            (untouch-leaves!)
            #+:decompose-graph (dot :message 538)
            #+:decompose-graph (format t "      disjoin-subtypes!~%")
            (disjoin-subtypes!)
            #+:decompose-graph (dot :message 541)
            ))

	(when graph
          (let ((stranded (setof node graph
                            (not (subtypep (type-specifier node) nil)))))
            (assert (not stranded) (stranded graph disjoint-types)
                    "stranded types: ~A~%" stranded))))

      (remove nil disjoint-types))))
