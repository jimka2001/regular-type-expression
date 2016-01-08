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

(defpackage :non-deterministic-finite-automata
  (:use :cl)
  (:nicknames "NDFA")
  (:export "MAKE-NDFA"
	   "STATE-MACHINE"
	   "DETERMINISTICP"
	   "STATE-NAME"
	   "STATE-FINAL-P"
	   "STATE-LABEL"
	   "STATE-STICKY-P"
	   "NEXT-LABEL"
	   "NEXT-STATE"
	   "ADD-STATE"
	   "NDFA-TO-DOT"
	   "TRANSITIONS"
	   "TRANSITION-LABEL"
	   "STATES"
	   "GET-INITIAL-STATES"
	   "GET-FINAL-STATES"
	   "GET-STICKY-STATES"
	   "PERFORM-TRANSITIONS"
	   "PERFORM-SOME-TRANSITIONS"))

(in-package :ndfa)

(defclass state-machine ()
  ((states :initarg :states :initform nil :accessor states
	   :documentation "List of elements of class STATE")
   (deterministicp :initform nil :initarg :deterministicp :accessor deterministicp)
   (final-states :accessor get-final-states)
   (sticky-states :accessor get-sticky-states)
   (initial-states :accessor get-initial-states
		   :documentation "Subset of the STATES slot indicating which of the states are initial,
 ie. which of the states answer TRUE to the STATE-INITIAL-P predicate.")
   (test :initform #'eql :initarg :test :reader test :type (function (t t) t)
	 :documentation ":KEY :TEST are the idiomatic key/test pair common to many
lisp functions. The function PERFORM-SOME-TRANSITIONS uses these two function to
iteratively determine whether each element of its INPUT sequence is
accepted or rejected. 
:TEST designates a binary predicate to be called two arguments:
   input-data - Each element of the INPUT-SEQUENCE is passed to the :KEY
                function and that value is passed as the 1st argument of
                the :TEST function.
   transition-label - the value returned from (TRANSITION-LABEL transition)
                is passed as the 2nd argument of the :TEST function")
   (key  :initform #'identity :initarg :key :reader key :type (function (t) t)))
  (:documentation "A finite state machine.  An application program is expected to maintain
a list of states, each an element of (STATES ...), and use either the function
PERFORM-TRANSITIONS or PERFORM-SOME-TRANSITIONS to compute the list of next states
given an INPUT-SEQUENCE.  Use the factory function, MAKE-NDFA, to create an instance
of STATE-MACHINE. The list of states of the machine may be specified as the
:STATES argument to MAKE-NDFA, by subsequent calls to ADD-STATE."))

(defvar *state-number* 0)

(defclass state ()
  ((label :initarg :label :reader state-label
	  :documentation "An object, usually a number, string, symbol, list indentifying this state.
Code which manipulate state transitions, use this label to identify intended states before the states
have yet been created as part of the initialization process.  The label is also used within PRINT-OBJECT.
It is not allowed to have two different states in the same state-machine which have the same label
according to the EQUAL function.")
   (state-number :initform (incf *state-number*))
   (ndfa :initarg :ndfa :reader ndfa :type state-machine
	 :documentation "The instance of STATE-MACHINE for which this instance of STATE is state of.  I.e.,
this STATE instance is a member of (STATES (STATE-MACHINE state))")
   (transitions :type list :initform nil :accessor transitions
		:documentation "List of instances of class TRANSITION")
   (initial-p :initarg :initial-p :initform nil :reader state-initial-p
	      :documentation "Indicates whether the state is an initial state of the state machine.")
   (sticky-p :initarg :sticky-p :initform nil :accessor state-sticky-p
	     :documentation "A state is sticky if once the NDFA gets into this state, it cannot leave.")
   (final-p :initarg :final-p :initform nil :reader state-final-p
	    :documentation "Indicates whether the state is a final state of the state machine."))
  (:documentation "Instances of this class comprise the values of the STATES slot of
an instance of class STATE-MACHINE."))

(defgeneric state-name (state))

(defmethod state-name ((state state))
  ;; (intern (with-output-to-string (str)
  ;; 	    (write (state-label state)
  ;; 		   :stream str
  ;; 		   :pretty nil
  ;; 		   :escape t))
  ;; 	  (symbol-package 'state-name))
  (slot-value state 'state-number)
  )

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'initial-states)))
  "Calculate the list of initial states and setf it as the value of the INITIAL-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-initial-p state)
						  (list state)))
					    (states ndfa))))

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'sticky-states)))
  "Calculate the list of initial states and setf it as the value of the STICKY-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-sticky-p state)
						  (list state)))
					    (states ndfa))))

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'final-states)))
  "Calculate the list of initial states and setf it as the value of the FINAL-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-final-p state)
						  (list state)))
					    (states ndfa))))

(defmethod print-object ((self state) stream)
  (if (slot-boundp self 'label)
      (print-unreadable-object (self stream :type t :identity nil)
	(format stream "~A" (state-label self))
	(when (state-initial-p self)
	  (format stream "[I]"))
	(when (state-final-p self)
	  (format stream "[F]")))
	
      (call-next-method)))

(defclass transition ()
  ((state :initarg :state :type state :accessor state)
   (next :initarg :next :type (or nil state) :reader next-state) ; initialized lazily based on value of next-label
   (transition-label :initarg :transition-label :accessor transition-label
		     :documentation "An object such as number, string, symbol, list which designates the
test controlling a transition to the state indicated by NEXT-LABEL.  For more details see
the documentation of the TEST and KEY slots of the STATE-MACHINE class.")
   (next-label :initarg :next-label :reader next-label
	       :documentation "A state label, indicating that this transition object represents a 
state machine transition from the state STATE to the state whose LABEL is NEXT-LABEL. NEXT-LABEL
is used in the initialization process.  Its value might indicate a state which has not yet been created.
After all the transitions to all the states have been added, it is expected that each NEXT-LABEL is EQUAL
to the LABEL of some state in the state-machine.  However, care should be taken during initialization not
to assume that such a state already exists.  After initialization is finished, the NEXT slot will be the
state object whose STATE-LABEL is EQUAL to this NEXT-LABEL"))
  (:documentation "Instances of this class comprise the list in the TRANSITIONS slot of the class STATE."))

(defmethod initialize-instance :after ((self transition) &rest initargs)
  (unless (slot-boundp self 'transition-label)
    (error "~A was created with no transition-label.  initargs=~A" self initargs)))

(defmethod ndfa ((self transition))
  (ndfa (state self)))

(defmethod print-object ((self transition) stream)
  (if (and (slot-boundp self 'state)
	   (slot-boundp (state self) 'label)
	   (slot-boundp self 'next-label))
      (print-unreadable-object (self stream :type t :identity nil)
	(format stream "~A->~A" (state-label (state self)) (next-label self)))
      (call-next-method)))

(defmethod slot-unbound ((class standard-class) (self transition) (slot-name (eql 'next)))
  "It is expected that NEXT-LABEL indicates the LABEL of some state in the state machine.
This method lazily sets the NEXT slot of SELF to the STATE slot to that state."
  (let ((state (find (next-label self) (states (ndfa self)) :key #'state-label :test #'equal)))
    (if state
	(setf (slot-value self slot-name) state)
	(error "transition has next-label=~A indicating non-existing state: available labels are: ~A"
	       (next-label self)
	       (mapcar #'state-label (states (ndfa self)))))))

(defgeneric perform-some-transitions (sm starting-states input-sequence))
(defmethod perform-some-transitions ((ndfa state-machine) starting-states input-sequence)
  "Given a list of STARTING-STATES, each of which is an element of (states NDFA), 
perform the transitions indicated by INPUT-SEQUENCE, i.e., iterate through the
STARTING-STATES, and on each element generate a list of next-states, as a function of
the existing transistions on the state.  Some of the transitions will lead to a next state
and some won't; collect a list of such 'successful' next-states.  On this new list of states,
perform the same algorithm on the next element of the INPUT-SEQUENCE, and repeat until the
INPUT-SEQUENCE is depleted.  Return the list of resulting states.
There is a notable exception, if there is every a transition into a state which is both final
and sticky, then a singleton list of that state is returned, and no further transition, 
nor no further element fo the input-sequence is considered."
  (declare (type list starting-states)
	   (type sequence input-sequence))
  (let ((current-states starting-states)
	(deterministicp (deterministicp ndfa)))
    ;; We specifically use MAP here becasue it works on sequences,
    ;; rather than simply on lists.
    (map nil
	 (lambda (input &aux new-states)
	   (dolist (state current-states)
	     (block do-transitions
	       (dolist (transition (transitions state))
		 (when (funcall (test ndfa) (funcall (key ndfa) input) (transition-label transition))
		   (let ((next-state (next-state transition)))
		     (if (and (state-sticky-p next-state)
			      (state-final-p next-state))
			 (return-from perform-some-transitions (list next-state))
			 (progn (pushnew next-state new-states)
				(when deterministicp
				  ;; if ndfa is deterministic, we need only find one transition
				  (return-from do-transitions)))))))))
	   ;; if current-states is nil, EVERY will return
	   (setf current-states new-states))
	   input-sequence)
    (the list current-states)))

(defgeneric perform-transitions (sm input-sequence))

(defmethod perform-transitions ((ndfa state-machine) input-sequence)
  "Returns a list of states which are reached by the following process:
Start with the list/set of all initial states of the state-maching NDFA.
Iterate through the INPUT-SEQUENCE, performing all the applicable transistions.
If the list of states becomes empty, return NIL.
Otherwise the set of states reached is returned.
None, some, or all of these states might be final states of the state machine."
  (declare (type sequence input-sequence))
  (perform-some-transitions ndfa (get-initial-states ndfa) input-sequence))

(defgeneric add-transition (state &key next-label transition-label))

(defmethod add-transition ((state state) &key next-label transition-label)
  "Create and return an instance of TRANSITION from STATE to the state designated by NEXT-LABEL.
Note, that the state indicated by NEXT-LABEL might not yet exist."
  (car (push (make-instance 'transition :state state :next-label next-label :transition-label transition-label)
	     (transitions state))))

(defgeneric add-state (object &key label initial-p final-p transitions))

(defmethod add-state ((ndfa state-machine) &key label initial-p final-p transitions)
  "Add or update a state designated by the given LABEL.  If the state already exists
in the state-machine NDFA, (whose STATE-LABEL is EQUAL to LABEL) its INITIAL-P and 
FINAL-P are updated to TRUE if :INITIAL-P or :FINAL-P are given as such
(but not updated to NIL).  If the state does not yet exists, it one is created
and added to the state machine."
  ;; TRANSITIONS is a list of sublists, each sublist is of the form
  ;; (unary-test-function destination-label)
  (let ((existing-state (find label (states ndfa) :key #'state-label :test #'equal)))
    (cond
      (existing-state
       (when initial-p
	 (setf (slot-value existing-state 'initial-p) t))
       (when final-p
	 (setf (slot-value existing-state 'final-p) t))
       existing-state)
      (t
       (let ((new-state (make-instance 'state
				       :ndfa ndfa
				       :label label
				       :initial-p initial-p
				       :final-p final-p)))
	 (dolist (transition transitions)
	   (apply #'add-transition new-state transition))
	 (push new-state (states ndfa))
	 new-state)))))

(defun make-ndfa (state-designators &rest initargs)
  "Factory function for generating an instance of STATE-MACHINE.  STATE-DESIGNATORS is
a list of state-designators.  Each state-designator is a valid initarg list for 
the ADD-STATE function."
  (let ((ndfa (apply #'make-instance 'state-machine :states nil initargs)))
    (dolist (state-designator state-designators)
      (apply #'add-state ndfa state-designator))
    ndfa))

