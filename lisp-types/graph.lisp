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

(defun decompose-types-graph (type-specifiers)
  (declare (type list type-specifiers))
  (let (disjoint-types
	(graph (loop :for type-specifier :in type-specifiers
		     :collect (list (list type-specifier)
				    :super-types nil
				    :sub-types nil
				    :touches nil
				    :original type-specifier))))
    (labels ((verify ()
	       (dolist (node graph)
		 (assert (typep node 'node) (node) "corrupt car of node")
		 (assert (car (car node)) (graph node) "not expecting nil type")
		 (dolist (sub-type (sub-types node))
		   (assert (typep sub-type 'cons) (node (sub-types node)) "corrupted sub-types"))
		 (dolist (super-type (super-types node))
		   (assert (typep super-type 'cons) (node (super-types node)) "corrupted super-types"))
		 (dolist (touch (touches node))
		   (assert (typep touch 'cons) (node (touches node)) "corrupted touches")
		   (when (disjoint? touch (car node))
		     (warn "touches contains ~A which is disjoint from ~A"
			   touch (car node))))))
	     (unionq (a b)
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
	       (declare (type cons t1 t2))
	       (subtypep (car t1) (car t2)))
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
		 (if (empty? t3)
		     nil
		     t3)))
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
			  ((null (and (nth-value 1 (subtype? t1 t2))
				      (nth-value 1 (subtype? t2 t1))))
			   (warn "cannot determine relationship of ~A vs ~A, assuming disjoint" t1 t2)
			   nil)
			  ((subtype? t1 t2)
			   (when (subtype? t2 t1)
			     (warn "equivalent types ~A and ~A" t1 t2))
			   (new-super-type t2 t1-node))
			  ((subtype? t2 t1)
			   (new-super-type t1 t2-node))
			  ((disjoint? t1 t2)
			   nil)
			  (t		; touches
			   (push t2 (touches t1-node))
			   (push t1 (touches t2-node)))))
		      t2-tn))
	    graph)

      (dolist (node graph)
	(dolist (super-type (super-types node))
	  (push (car node) (sub-types (assoc super-type graph)))))
      
      (setf graph (sort graph #'> :key (lambda (node)
					 (length (getf (cdr node) :super-types)))))

      (let ((status 'initial))
	(labels (
		 ;; everything which has super-types but no sub-types and no touches
		 (disjoin-subtypes! ()
		   (dolist (node graph)
		     (when (and (super-types node)
				(not (sub-types node))
				;;(not (touches node))
				)
		       (disjoin-subtype! node))))
		 ;; one thing which has super-types but no sub-types and no touches
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
							  `(not ,(type-specifier subtype-node)))))
			 (cond
			   (new-type
			    (setf (type-specifier super-node)
				  new-type))
			   (t ;; then the sub-type and super-type are equivalent
			    ;; eliminate the super-type node from graph
			    (setf graph (removeq super-node graph))
			    ;; update the sub-type's super-types
			    (setf (super-types subtype-node)
				  (unionq (removeq super-type (super-types subtype-node))
					  (super-types super-node)))
			    ;; update the sub-type's sub-types
			    (setf (sub-types subtype-node)
				  (unionq (sub-types subtype-node)
					  (removeq subtype (sub-types super-node))))
			    ;; updates the subtype's touches
			    (dolist (neighbor-of-super (touches super-node))
			      (push neighbor-of-super (touches subtype-node))
			      (let ((neighbor-node-of-super (find-node neighbor-of-super)))
				(setf (touches neighbor-node-of-super)
				      (remove neighbor-of-super (touches neighbor-node-of-super) ))
				(pushnew subtype (touches neighbor-node-of-super))))))))))
		 ;; everything which touches something but no sub-types
		 (untouch-leaves! ()
		   (dolist (node graph)
		     (when (and (touches node)
				(not (sub-types node)))
		       (untouch-leaf! node))))
		 ;; one thing which touches something but no sub-types
		 (untouch-leaf! (node &aux (node-type (car node)))
		   (declare (type node node))
		   (dolist (neighbor (touches node))
		     (let ((neighbor-node (find-node neighbor)))
		       ;; make sure neighbor has no sub-types
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
						    :original (copy-list new-type))))
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
				(setf (type-specifier node)          new-A
				      (type-specifier neighbor-node) new-B
				      (touches node)          (remove neighbor (touches node))
				      (touches neighbor-node) (remove node-type (touches neighbor-node)))))
			     (t
			      ;; if they are marked as touching but their intersection is void,
			      ;;  then simply update the touches fields
			      (setf (touches node)
				    (removeq neighbor (touches node))

				    (touches neighbor-node)
				    (removeq node-type (touches neighbor-node))))))))))
		 (disjoint! (&aux (disjoint-nodes (remove-if #'(lambda (node)
								(or (touches node)
								    (sub-types node)
								    (super-types node)))
							     graph)))
		   (when disjoint-nodes
		     ;(setf status 'changed)
		     (setf graph (set-differenceq graph disjoint-nodes))
		     (dolist (disjoint-node disjoint-nodes)
		       (pushnew (reduce-lisp-type (caar disjoint-node))
				disjoint-types
				:test #'equal)))))
	  (while (and graph
		      (not (eq 'unchanged status)))
	    (setf status 'unchanged)
	    (disjoint!)
	    (untouch-leaves!)
	    (disjoin-subtypes!)))

	(when graph
	  (dolist (node graph)
	    (unless (subtypep (type-specifier node) nil)
	      (error "stranded type: ~A~%" (reduce-lisp-type (type-specifier node)))))))

      (remove nil disjoint-types))))

(defun perf-decompose-types-graph ()
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function ; see https://groups.google.com/forum/#!topic/comp.lang.lisp/S-O94JzjlFw
						char-code ; same as char-int
						)))
    (setf all-types (sort all-types #'string<))
    (let ((n 1)
	  (testing-types (list (pop all-types))))
      (flet ((test1 (types &aux sorted)
	       (format t "~A~%" (car types))
	       (format t "  types=~A~%" types)
	       (let ((t1 (get-internal-run-time))
		     (t2 (progn (setf sorted (decompose-types-graph types))
				(get-internal-run-time))))
		 (unless (= t1 t2)
		   (format t "   ~D ~D ~F~%"
			   n
			   (length sorted)
			   (/ (- t2 t1) internal-time-units-per-second)))
		 (incf n))))
	(loop :while testing-types
	      :do (progn (test1 testing-types)
			 (push (pop all-types) testing-types)))))))
