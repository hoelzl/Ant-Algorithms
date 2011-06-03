(in-package :ants)

(defparameter *current-node-number* 0)
(defparameter *node-name-prefix* :node-)

(defun make-node-name (&key (prefix *node-name-prefix*)
  		            (number (incf *current-node-number*)))
  (intern (format nil "~A~A" prefix number) :keyword))

(defclass node ()
  ((node-name :initarg :node-name :accessor node-name)
   (outgoing-transitions :initform '()
			 :initarg :outgoing-transitions
			 :accessor outgoing-transitions)
   (incoming-transitions :initform '()
			 :initarg :incoming-transitions
			 :accessor incoming-transitions)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A (in: ~A, out: ~A)"
	    (if (slot-boundp node 'node-name)
		(node-name node)
		:unnamed-node)
	    (if (slot-boundp node 'incoming-transitions)
		(length (incoming-transitions node))
		:unbound)
	    (if (slot-boundp node 'outgoing-transitions)
		(length (outgoing-transitions node))
		:unbound))))

(defvar *all-nodes* '())
(defvar *node-table* (make-hash-table))

(defmethod shared-initialize :after ((node node) slot-names &key node-name)
  (pushnew node *all-nodes*)
  (when (or (eql slot-names t) (member 'node-name slot-names))
    (let ((node-name (or node-name (make-node-name))))
      (setf (node-name node) node-name
	    (gethash node-name *node-table*) node))))

(defmethod find-node ((node node))
  node)

(defmethod find-node ((node symbol))
  (gethash node *node-table*))

(defparameter *current-transition-number* 0)
(defparameter *transition-prefix* :t)

(defun make-transition-name (&key (prefix *transition-prefix*)
			          (number (incf *current-transition-number*)))
  (intern (format nil "~A~A" prefix number) :keyword))

(defclass transition ()
  ((transition-name :initarg :transition-name
		    :accessor transition-name)
   (start-node :initarg :start-node
	       :accessor start-node)
   (end-node :initarg :end-node
	     :accessor end-node)))

(defmethod print-object ((transition transition) stream)
  (print-unreadable-object (transition stream :type t :identity t)
    (format stream "~A => ~A"
	    (if (slot-boundp transition 'start-node)
		(node-name (start-node transition))
		:unknown-node)
	    (if (slot-boundp transition 'end-node)
		(node-name (end-node transition))
		:unknown-node))))

(defvar *all-transitions* '())
(defvar *transition-table* (make-hash-table))

(defmethod shared-initialize :after ((transition transition) slot-names 
				     &key transition-name start-node end-node)
  (pushnew transition *all-transitions*)
  (when (or (eql t slot-names) (member 'transition-name slot-names))
    (let ((transition-name (or transition-name (make-transition-name))))
      (setf (transition-name transition) transition-name
	    (gethash transition-name *transition-table*) transition)))
  (when (or (eql t slot-names) (member 'start-node slot-names))
    (let ((start-node (find-node start-node)))
      (assert start-node (start-node) "Transitions must have a start node.")
      (setf (start-node transition) start-node)
      (pushnew transition (outgoing-transitions start-node))
      (setf (start-node transition) start-node)))
  (when (or (eql t slot-names) (member 'end-node slot-names))
    (let ((end-node (find-node end-node)))
      (assert end-node (end-node) "Transitions must have an end node.")
      (setf (end-node transition) end-node)
      (pushnew transition (incoming-transitions end-node))
      (setf (end-node transition) end-node))))

(defun clear-graph ()
  (setf *all-transitions* '()
	*all-nodes* '())
  (clrhash *transition-table*)
  (clrhash *node-table*)
  (setf *current-node-number* 0
	*current-transition-number* 0)
  :graph-cleared)