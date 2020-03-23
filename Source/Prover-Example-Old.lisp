(in-package :PROVER)

;;;----------------------------------------------------------------------------
;;; EXAMPLE CLAUSES
;;;----------------------------------------------------------------------------

(defparameter c1 '(P ?x ?y))

(defparameter c2 '(P A B))

(defparameter c3 '(P A ?y))

(defparameter c4 '(P ?x B))

;;;----------------------------------------------------------------------------

(defparameter t1 '(TEMPORAL I1 (<) I2))

(defparameter t2 '(TEMPORAL I2 (<) I3))

(defparameter t3 '(TEMPORAL I3 (< m) I4))

(defparameter t2-a '(TEMPORAL I2 (< m o) I3))

(defparameter t2-b '(TEMPORAL I2 (< m) I3))

;;;----------------------------------------------------------------------------
;;; Examples for the prover.
;;;--------------------------------------------------------------------------


;;; This provides some simple examples to test the prover out.

(defvar *prover-db* nil)

(setf *prover-db* '(
   ((under ?x1 ?y1 ?t1)(above ?y1 ?x1 ?t1))
   ((above ?x2 ?y2 ?t2)(on ?x2 ?y2 ?t2))
   ((above ?x3 ?y3 ?t3)(on ?x3 ?z3 ?t3)(above ?z3 ?y3 ?t3))
   ((on A B I0))
   ((on B C I0))
   ((on C D I0))
   ((R ?a ?b)(P ?a ?b)(Q ?a ?b))
   ((W ?x))
   ((P A B))
   ((Q A B))
   ((P B C))
   ((Q B C))))
  
(defvar db)
(setf db *prover-db*)


;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
