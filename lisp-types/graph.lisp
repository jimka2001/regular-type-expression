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

(defmacro exists (obj data &body body)
  `(member-if (lambda (,obj) ,@body) ,data))

(defun all-type-specifiers (package)
  (let (all-types)
    (do-external-symbols (sym package)
      (when (valid-type-p sym)
	(push sym all-types)))
    all-types))

(deftype node ()
  '(cons cons cons))

(defun find-duplicates (data)
  (remove-duplicates
   (mapcon (lambda (tail &aux (item (car tail)))
	      (when (member item (cdr tail) :test #'eq)
		(list item)))
	    data)
   :test #'eq))

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
  (let ((graph (sort (copy-list graph) #'< :key (lambda (node)
						  (getf (cdr node) :id))))
	done)
    (labels ((find-name (type-spec)
	       (getf (cdr (assoc type-spec graph :test #'eq)) :id))
	     (print-comments ()
	       (dolist (node graph)
		 (format stream "// ~D " (getf (cdr node) :id))
		 (write (caar node) :stream stream :pretty nil)
		 (format stream "~%")))
	     (connect (a b)
	       (let ((a (find-name a))
		     (b (find-name b)))
		 (cond
		   ((member (list a b) done :test #'equal))
		   (t
		    (push (list a b) done)
		    (push (list b a) done)
		    (format stream "    ~D -> ~D~%" a b)))))
	     (print-node-defs ()
	       (dolist (node graph)
		 (format stream "  ~D ; // " (getf (cdr node) :id))
		 (write (reduce-lisp-type (caar node)) :stream stream :pretty nil)
		 (terpri stream)))
	     (print-touching ()
	       (format stream "  subgraph Rel1 {~%")
	       (format stream "    edge [dir=none, color=green]~%")
	       (dolist (node graph)
		 (dolist (touch (getf (cdr node) :touches))
		   (connect (car node) touch)))
	       (format stream "  }~%"))
	     (print-sub-super ()
	       (format stream "  subgraph Rel2 {~%")
	       (format stream "    edge [color=blue]~%")
	       (dolist (node graph)
		 (dolist (super (getf (cdr node) :super-types))
		   (connect (car node) super)))
	       (format stream "  }~%")))
      (print-comments)
      (format stream "digraph G {~%")
      (format stream "  rankdir=BT ;~%")
      (print-node-defs)
      (print-touching)
      (print-sub-super)
      (format stream "}~%"))))
	  

(defun verify-graph (graph &rest notes)
  "Used for debugging, this function traverses the given GRAPH and checks the structure and redundancies."
  (labels ((memq (a b)
	     (member a b :test #'eq))
	   (disjoint? (t1 t2)
	     (declare (type cons t1 t2))
	     (disjoint-types-p (car t1) (car t2)))
	   (touches (node)
	     (getf (cdr node) :touches))
	   (sub-types (node)
	     (declare (type node node))
	     (getf (cdr node) :sub-types))
	   (super-types (node)
	     (declare (type node node))
	     (getf (cdr node) :super-types))
	   (find-node (a)
	     (assoc a graph :test #'eq)))

    (assert (null (find-duplicates (mapcar #'car graph))) (notes graph)
	    "duplicates in graph nodes: ~A" (find-duplicates (mapcar #'car graph)))
    (dolist (node graph)
      (assert (typep node 'node)
	      (notes node)
	      "corrupt car of node")
      (assert (car (car node))
	      (notes graph node)
	      "not expecting nil type")
      (assert (not (member (car node) (touches node) :test #'eq)) (notes node)
	      "node touches itself")
      (assert (not (member (car node) (super-types node) :test #'eq)) (notes node)
	      "node is super-type of itself")
      (assert (not (member (car node) (sub-types node) :test #'eq)) (notes node)
	      "node is sub-type of itself")
	      
      (assert (null (find-duplicates (sub-types node))) (notes node)
	      "duplicates in sub-types: ~A" (find-duplicates (sub-types node)))
      (mapc (lambda (sub-type &aux (sub-type-node (find-node sub-type)))
	      (assert (typep sub-type 'cons)
		      (notes node) "corrupted sub-types")
	      (assert sub-type-node
		      (notes node sub-type graph) "node references sub-type which does not exist:~%~A~%~A" sub-type node)
	      (assert (memq (car node) (super-types sub-type-node)) (notes node)
		      "sub-type is missing this node as super-type"))
	    (sub-types node))
      (assert (null (find-duplicates (super-types node))) (notes node)
	      "duplicates in super-types: ~A" (find-duplicates (super-types node)))
      (mapc (lambda (super-type &aux (super-type-node (find-node super-type)))
	      (assert (typep super-type 'cons)
		      (notes node) "corrupted super-types")
	      (assert super-type-node
		      (notes node super-type) "node references super-type which does not exist")
	      (assert (memq (car node) (sub-types super-type-node)) (notes node)
		      "super-type is missing this node as sub-type ~%~A~%~A" node super-type-node))
	    (super-types node))
      (assert (null (find-duplicates (touches node))) (notes node)
	      "duplicates in touches: ~A" (find-duplicates (touches node)))
      (mapc (lambda (touch &aux (touch-node (find-node touch)))
	      (assert (typep touch 'cons)
		      (notes node) "corrupted touches")
	      (when (disjoint? touch (car node))
		(warn "touches contains ~A which is disjoint from ~A"
		      touch (car node)))
	      (assert touch-node (notes node touch) "node references touch which does not exist")
	      (assert (memq (car node) (touches touch-node)) (notes node) "touch node mismatch"))
	    (touches node)))))

(defun decompose-types-graph (type-specifiers &key verbose reduce)
  (declare (type list type-specifiers)
	   (notinline assoc sort)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (let* (disjoint-types
	 (node-id 0)
	 (graph (loop :for type-specifier :in type-specifiers
		      :collect (list (list type-specifier)
				     :super-types nil
				     :sub-types nil
				     :touches nil
				     :original type-specifier
				     :id (incf node-id)))))
    (declare (type list graph))
    (labels ((count-touches ()
	       (/ (loop :for node :in graph
			:count (length (touches  node)))
		  2))
	     (count-supers ()
	       (loop :for node :in graph
		     :count (length(super-types  node))))
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
	     (find-node (a)
	       (assoc a graph :test #'eq))
	     (subtype? (t1 t2)
	       (declare (notinline typep)
			(type cons t1 t2))
	       (smarter-subtyepp (car t1) (car t2)))
	     (disjoint? (t1 t2)
	       (declare (type cons t1 t2))
	       (disjoint-types-p (car t1) (car t2)))
	     (super-types (node)
	       (declare (type node node))
	       (getf (cdr node) :super-types))
	     ((setf super-types) (new node)
	       (declare (type list new)
			(type node node))
	       (setf (getf (cdr node) :super-types) new))
	     (sub-types (node)
	       (declare (type node node))
	       (getf (cdr node) :sub-types))
	     ((setf sub-types) (new node)
	       (declare (type list new) ; could be nil if there are no sub-types
			(type node node))
	       (setf (getf (cdr node) :sub-types) new))
	     (touches (node)
	       (getf (cdr node) :touches))
	     ((setf touches) (new node)
	       (declare (type list new) ; could be nil if there are no touches
			(type node node))
	       (setf (getf (cdr node) :touches) new))
	     (type-specifier (node)
	       (declare (type node node))
	       (car (car node)))
	     ((setf type-specifier) (new-type node)
	       (declare (type t new-type)
			(type node node))
	       (setf (car (car node)) new-type))
	     (empty? (type)
	       (subtypep type nil))
	     (type-intersection (t1 t2)
	       (let ((t3 `(and ,t1 ,t2)))
		 (cond
		   ((empty? t3)
		    nil)
		   (reduce
		    (reduce-lisp-type t3))
		   (t
		    t3))))
	     (new-super-type (new node &aux (super-types (super-types node)))
	       (declare (type cons new)
			(type node node))
	       (cond
		 ((null super-types)
		  (setf (super-types node) (list new)))
		 ((exists type super-types
		    ;; new=(number) super-types=((fixnum))
		    (subtype? type new))
		  nil)
		 (t
		  ;; new=(fixnum) super-types='((number) (A) (B))
		  (setf (super-types node)
			(cons new (remove-if (lambda (type)
					       (subtype? new type))
					     (super-types node))))))))
      (mapl (lambda (tail &aux (t1-node (car tail)) (t2-tn (cdr tail)) (t1 (car t1-node)))
	      (mapcar (lambda (t2-node &aux (t2 (car t2-node)))
			(cond
			  ((subtype? t1 t2)
			   (when (subtype? t2 t1)
			     (warn "equivalent types ~A and ~A" t1 t2))
			   (new-super-type t2 t1-node))
			  ((subtype? t2 t1)
			   (new-super-type t1 t2-node))
			  ((disjoint? t1 t2)
			   nil)
			  (t		; touches
			   ;; this includes the case that subtypep returns NIL;NIL in both directions
			   (cond
			     ((null (nth-value 1 (subtype? t1 t2)))
			      (warn 'ambiguous-subtype :sub t1 :super t2))
			     ((null (nth-value 1 (subtype? t2 t1)))
			      (warn 'ambiguous-subtype :sub t2 :super t1)))
			   (push t2 (touches t1-node))
			   (push t1 (touches t2-node)))))
		      t2-tn))
	    graph)

      (dolist (node graph)
	(dolist (super-type (super-types node))
	  (push (car node) (sub-types (assoc super-type graph)))))
      
      (setf graph (sort graph #'> :key (lambda (node)
					 (length (getf (cdr node) :super-types)))))

      (let ((graph-size (length graph))
	    (iteration 0)
	    (status 'initial))
	(labels (
		 (dot ()
		   (verify-graph graph)
		   (graph-to-dot graph (format nil "/tmp/jnewton/graph/graph-~2,'0D.dot" (incf iteration)))
		   (y-or-n-p "continue nodes=~D, touches=~D, supers=~D?" (length graph) (count-touches) (count-supers))
		   )
		 ;; everything which has super-types but no sub-types and no touches
		 (disjoin-subtypes! (&aux (n (length graph)))
		   "Find nodes which have super-types but no subtypes, break the super-type links,
                    being careful not to strand touching neighbors."
		   (dolist (node graph)
		     (when (and (super-types node)
				(not (sub-types node)))
		       (when verbose
			 (format t "        disjoin-subtype! ~D~%" (decf n)))
		       (disjoin-subtype! node)
		       ;;(dot)
		       )))
		 ;; one thing which has super-types but no sub-types
		 (disjoin-subtype! (subtype-node &aux (subtype (car subtype-node)))
		   (declare (type node subtype-node))
		   (setf status 'changed)
		   (dolist (super-type (super-types subtype-node))
		     (declare (type cons super-type))
		     (let ((super-node (find-node super-type)))
		       (declare (type node super-node))
		       (setf (sub-types super-node)
			     (removeq subtype (sub-types super-node))
			     
			     (super-types subtype-node)
			     (removeq super-type (super-types subtype-node)))
		       (dolist (super-super-type (super-types super-node))
			 (pushnew subtype (sub-types (find-node super-super-type)) :test #'eq)
			 (pushnew super-super-type (super-types subtype-node)      :test #'eq))
		       (let ((new-type (type-intersection (type-specifier super-node)
							  (list 'not (type-specifier subtype-node)))))
			 (cond
			   (new-type
			    (setf (type-specifier super-node)
				  new-type))
			   (t ;; then the sub-type and super-type are equivalent
			    ;; eliminate the sub-type node from graph
			    (setf graph (removeq subtype-node graph))

			    (assert (null (sub-types subtype-node)))
			    ;; update the sub-types of the super-type node
			    (mapc (lambda (super-type &aux (super-node (find-node super-type)))
				    (setf (sub-types super-node)
					  (removeq subtype (sub-types super-node)))
				    (setf (super-types subtype-node)
					  (removeq super-type (super-types subtype-node)))

				    (setf (touches super-node)
					  (unionq (touches subtype-node)
						  (touches super-node)))
				    (setf (super-types super-node)
					  (unionq (removeq super-type (super-types subtype-node))
						  (super-types super-node)))
				    (mapc (lambda (neighbor-type &aux (neighbor-node (find-node neighbor-type)))
					    (setf (touches neighbor-node)
						  (removeq subtype (touches neighbor-node)))
					    (pushnew super-type (touches neighbor-node) :test #'eq))
					  (touches subtype-node)))
				  (super-types subtype-node))
			    (setf (touches subtype-node) nil)
			    (setf (super-types subtype-node) nil)))))))
		 ;; everything which touches something but no sub-types
		 (untouch-leaves! ()
		   "Find nodes which have touching neighbors, but no subtypes.  
                    Break the touching links."
		   (dolist (node graph)
		     (when (and (touches node)
				(not (sub-types node)))
		       (untouch-leaf! node)
		       ;;(dot)
		       )))
		 ;; one thing which touches something but no sub-types
		 (untouch-leaf! (node &aux (node-type (car node)) (num-neighbors (length (touches node))))
		   (declare (type node node))
		   (when verbose
		     (format t "        untouch-leaf! ~D~%" (length graph)))
		   (dolist (neighbor (touches node))
		     (let ((neighbor-node (find-node neighbor)))
		       ;; make sure neighbor has no sub-types
		       (when verbose
			 (format t "        neighbor ~D~%" (decf num-neighbors)))
		       (when (and neighbor-node
				  (not (sub-types neighbor-node)))
			 (setf status 'changed)
			 (let ((new-type (type-intersection (type-specifier node)
							    (type-specifier neighbor-node))))
			   (cond
			     (new-type
			      (let ((new-node (list (list new-type)
						    :super-types (unionq (super-types node)
									 (super-types neighbor-node))
						    :touches     (intersectionq (touches node)
										(touches neighbor-node))
						    :original (copy-list new-type)
						    :id (incf node-id))))
				(push new-node graph)
				;; we must update every N this new-node touches, so that N touches new-node
				(dolist (touch (touches new-node))
				  (push (car new-node) (touches (find-node touch))))
				;;  update sub-types of each super-type-node to reference this new type
				(dolist (super-type (super-types new-node))
				  (push (car new-node) (sub-types (find-node super-type)))))

			      (let* ((A (type-specifier node))
				     (B (type-specifier neighbor-node))
				     (new-A  (type-intersection A `(not ,B)))
				     (new-B  (type-intersection `(not ,A) B)))
				(mutate new-A node neighbor-node)
				(mutate new-B neighbor-node node)))
			     (t
			      ;; if they are marked as touching but their intersection is void,
			      ;;  then simply update the touches fields
			      (setf (touches node)
				    (removeq neighbor (touches node))

				    (touches neighbor-node)
				    (removeq node-type (touches neighbor-node))))))))))
		 (mutate (new-type node neighbor-node &aux (node-type (car node)))
		   (declare (type node node neighbor-node))
		   (cond (new-type
			  (setf (type-specifier node) new-type
				(touches node)        (remove (car neighbor-node) (touches node))))
			 (t
			  (assert (null (sub-types node)) (node)
				  "we should not be mutating a node with sub-types, the sub-types should be elimianted first")
			  (setf graph (removeq node graph))
			  (mapc (lambda (super &aux (super-node (find-node super)))
				  (setf (sub-types super-node) (removeq node-type (sub-types super-node))))
				(super-types node))
			  (mapc (lambda (touch &aux (touch-node (find-node touch)))
				  (assert touch-node)
				  (setf (touches touch-node) (removeq node-type (touches touch-node))))
				(touches node))
			  ;; explictly updating touches of neighbor-node, because mutate is called twice;
			  ;;   neighbor-node might no longer be a neighbor of node, if its the second call.
			  ;;   it might have already gotten disconnected.
			  (setf (touches neighbor-node) (removeq node-type (touches neighbor-node))))))
		 (disjoint! (&aux (disjoint-nodes (remove-if #'(lambda (node)
								(or (touches node)
								    (sub-types node)
								    (super-types node)))
							     graph)))
		   "Find nodes which have no sub-types, no super-types, and no touching neighbors.
                    Collect the types associated with such nodes into DISJOINT-NODES, and remove
                    from GRAPH."
		   (when disjoint-nodes
		     (when verbose
		       (format t "        ~D disjoint nodes~%" (length disjoint-nodes)))
		     ;(setf status 'changed)
		     (setf graph (set-differenceq graph disjoint-nodes))
		     (dolist (disjoint-node disjoint-nodes)
		       (pushnew (caar disjoint-node)
				disjoint-types
				:test #'equal))
		     ;;(dot)
		     )))


	  (while (and graph
		      (not (eq 'unchanged status)))

	    (setf status 'unchanged)
	    (when verbose
	      (unless (= (length graph)
			 graph-size)
		(setf graph-size (length graph))
		(when verbose
		  (format t "graph-size = ~D~%" graph-size))))
	    (when verbose
	      (format t "      disjoint!~%")) (disjoint!)
	    (when verbose
	      (format t "      untouch-leaves!~%")) (untouch-leaves!)
	    (when verbose
	      (format t "      disjoin-subtypes!~%")) (disjoin-subtypes!)))

	(when graph
	  (dolist (node graph)
	    (assert (subtypep (type-specifier node) nil) (node graph disjoint-types)
		    "stranded type: ~A~%" (reduce-lisp-type (type-specifier node))))))

      (remove nil disjoint-types))))

(defun get-all-types ()
  (let (types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym types)))
    (setf types (set-difference types '(t nil class built-in-class )))))

(defun perf-decompose-types-graph (&key (max 18))
  (declare (notinline string< sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function ; see https://groups.google.com/forum/#!topic/comp.lang.lisp/S-O94JzjlFw
						char-code ; same as char-int
						)))
    (setf all-types (sort all-types #'string<))
    (let ( data)
      (flet ((test1 (types &aux sorted)
	       (format t "~A~%" (car types))
	       ;;(format t "  types=~A~%" types)
	       (dolist (algo (list (list :algorithm 'decompose-types-graph
					 :function (lambda (types)
						     (decompose-types-graph types :verbose nil :reduce t)))
				   (list :algorithm 'decompose-types-sat
					 :function #'decompose-types-sat)
				   (list :algorithm 'decompose-types
					 :function #'decompose-types)))
		 (let ((t1 (get-internal-run-time))
		       (t2 (progn (setf sorted (funcall (getf algo :function) types))
				  (get-internal-run-time))))
		   (unless (= t1 t2)
		     (push (list
			    :algorithm (getf algo :algorithm)
			    :time (/ (- t2 t1) internal-time-units-per-second)
			    :input-length (length types)
			    :output-length (length sorted))
			   data)
		     (format t "  ~A ~D ~D ~F~%"
			     (getf algo :algorithm)
			     (length types)
			     (length sorted)
			     (/ (- t2 t1) internal-time-units-per-second)))))))
	(dotimes (r 10)
	  (let ((rnd-all-types (shuffle-list (copy-list all-types)))
		(testing-types nil))
	    (while (and rnd-all-types
			(>= max (length testing-types)))
	      (push (pop rnd-all-types) testing-types)
	      (test1 testing-types)))))
      (values data
	      (length data)))))
