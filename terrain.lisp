;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Extensions of the basic graph structure for terrain modelling.

(in-package :ants)

(defclass road-segment (undirected-transition)
  ((road-length :accessor road-length :initarg :road-length :initform 1)
   (pheromone-level :accessor pheromone-level :initarg :pheromone-level :initform 0.0)))

(defmethod transition-info ((road-segment road-segment))
  (format nil "length: ~A, pheromone-level: ~A"
	  (if (slot-boundp road-segment 'road-length)
	      (road-length road-segment)
	      :unknown)
	  (if (slot-boundp road-segment 'pheromone-level)
	      (pheromone-level road-segment)
	      :unknown)))

(defmethod build-simple-graph ()
  (clear-graph)
  (let ((source (make-instance 'node :node-name :source))
	 (target (make-instance 'node :node-name :target)))
    (make-instance 'road-segment
		   :transition-nodes (list source target))
    (make-instance 'road-segment 
		   :transition-nodes (list source target)
		   :road-length 2)
    (list source target)))
