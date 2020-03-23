(in-package :PROVER)

;;;--------------------------------------------------------------------------
;;; TREE OPERATIONS
;;;--------------------------------------------------------------------------

;;; These are the low level tree operations used in building the proof tree.

;;; This function creates a node whose value is 'expr'.

(defun MAKE-NODE (name expr &key (class 'NODE))
  (make-instance class :name name :value expr))

;;;--------------------------------------------------------------------------

(defun MAKE-AND-NODE (name expr)
  (make-node name expr :class 'AND-NODE))

;;;--------------------------------------------------------------------------

(defun MAKE-OR-NODE (name expr)
  (make-node name expr :class 'OR-NODE))

;;;--------------------------------------------------------------------------

;;; This function makes 'child' the child of 'parent'.

(defun MAKE-CHILD (child parent)
  (setf (node-children parent)(nconc (node-children parent)`(,child)))
  (setf (node-parent child) parent)
  child)

;;;--------------------------------------------------------------------------

;;; This function makes 'sibb' a sibbling of 'node'

(defun MAKE-SIBBLING (sibb node)
  (make-child sibb (node-parent node)))

;;;--------------------------------------------------------------------------

;;; This function kills the children of parent,

(defun KILL-CHILDREN (parent)
  (mapc #'(lambda (child)
	    (setf (node-parent child) nil))
	(node-children parent))
  (setf (node-children parent) nil))

;;;--------------------------------------------------------------------------

;;; This function returns the value of node.

(defun VAL (node)
  (node-value node))

;;;--------------------------------------------------------------------------

;;; This function returns the parent of node.

(defun PARENT (child)
  (cond ((null child)
	 nil)
        (t
         (node-parent child))))

;;;--------------------------------------------------------------------------

;;; This function returns the child of parent.

(defun CHILD (parent)
  (cond ((null parent)
	 nil)
        (t
         (node-children parent))))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
