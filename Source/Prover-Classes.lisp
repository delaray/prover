(in-package :prover)

;;;**************************************************************************
;;; PROVER CLASSES
;;;**************************************************************************
;;;
;;; ENTITY
;;;   ATOMIC-ENTITY
;;;     CONSTANT-ENTITY
;;;       PREDICATE-ENTITY
;;;         BUILTIN-PREDICATE
;;;       TEMPORAL-INTERVAL
;;;       TEMPORAL-RELATION
;;;       SKOLEM-CONSTANT
;;;     VARIABLE-ENTITY
;;;     NODE
;;;
;;;**************************************************************************


;;;--------------------------------------------------------------------------
;;; Classes & Structures
;;;--------------------------------------------------------------------------

;;; This controls the display of entities.

(defvar *PRINT-ENTITY-READABLY* nil)
(defvar *PRINT-VAR-NAMES-IF-BOUND* t)


;;;**************************************************************************
;;; Part 1: Composite Entities: Constants, Variables, Predicates
;;;**************************************************************************

(defclass ENTITY-MIXIN ()
  ())

;;;--------------------------------------------------------------------------
;;;  ATOMIC-ENTITY
;;;--------------------------------------------------------------------------

(defclass ATOMIC-ENTITY (ENTITY-MIXIN UTIL::VALUE)
  ((util::name :initform nil
	       :accessor entity-name
	       :accessor object-name)
   (util::value :initform nil 
		:accessor entity-value
   		:accessor object-value)))
   
;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj ATOMIC-ENTITY) stream)
  (cond (*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<Atomic: ~a>" (entity-name obj)))))

;;;--------------------------------------------------------------------------
;;; CONSTANT-ENTITY
;;;--------------------------------------------------------------------------

(defclass CONSTANT-ENTITY (ATOMIC-ENTITY)
  ())

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj CONSTANT-ENTITY) stream)
  (cond (*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<Const: ~a>" (entity-name obj)))))

;;;--------------------------------------------------------------------------
;;; SKOLEM-CONSTANT
;;;--------------------------------------------------------------------------

(defclass SKOLEM-CONSTANT (CONSTANT-ENTITY)
  ())

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj SKOLEM-CONSTANT) stream)
  (cond (*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<Skolem: ~a>" (entity-name obj)))))
	  
;;;--------------------------------------------------------------------------
;;; VARIABLE-ENTITY
;;;--------------------------------------------------------------------------

(defclass VARIABLE-ENTITY (ATOMIC-ENTITY)
  ((util::value :accessor variable-entity-value)
   (count :initarg :count
	  :initform 0
	  :accessor variable-entity-count
	  :accessor object-count)
   (boundp :initarg :boundp
	   :initform nil
	   :accessor variable-entity-boundp
	   :accessor object-bound-p)))
  
  
;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj VARIABLE-ENTITY) stream)
   (cond (*print-entity-readably*
	  (call-next-method))
	 (t
	  (format stream "#<Var: ~a~a>" 
		  (object-name obj)
		  (object-count obj)))))

 ;;;--------------------------------------------------------------------------

#+IGNORE
(defmethod PRINT-OBJECT ((obj VARIABLE-ENTITY) stream)
  (cond (*print-entity-readably*
	 (cond (*print-var-names-if-bound*
		(if (variable-bound-p obj)
		    (format stream "~a~a/~a" 
			    (entity-name obj)
			    (variable-entity-count obj)
			    (variable-value obj))
		  (format stream "~a" (entity-name obj))))
	       (t
		(format stream "~a"
			(if (variable-bound-p obj)
			    (variable-value obj)
			  (entity-name obj))))))
	(t
	 (cond ((variable-bound-p obj)
		(format stream "#<Var: ~a~a = ~a>" 
			(entity-name obj)
			(variable-entity-count obj)
			(entity-name (variable-value obj))))
	       (t
		(format stream "#<Var: ~a~a>" (entity-name obj) (variable-entity-count obj)))))))

;;;--------------------------------------------------------------------------
;;; PREDICATE-ENTITY
;;;--------------------------------------------------------------------------

(defclass PREDICATE-ENTITY (CONSTANT-ENTITY)
  ())

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj PREDICATE-ENTITY) stream)
  (cond (*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<Predicate: ~a>" (entity-name obj)))))

;;;--------------------------------------------------------------------------
;;; BUILTIN-PREDICATE-ENTITY
;;;--------------------------------------------------------------------------

(defclass BUILTIN-PREDICATE (PREDICATE-ENTITY)
  ())

;;;--------------------------------------------------------------------------
;;; INTERVAL-ENTITY
;;;--------------------------------------------------------------------------

(defclass TEMPORAL-INTERVAL (CONSTANT-ENTITY)
  ())

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj TEMPORAL-INTERVAL) stream)
  (cond (*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<Interval: ~a>" (entity-name obj)))))

;;;----------------------------------------------------------------------------
;;; TEMPORAL RELATION CLASS
;;;----------------------------------------------------------------------------

(defclass TEMPORAL-RELATION (CONSTANT-ENTITY)
  (;; The inverse of a temporal relation
   (inverse-relation :initform nil
		     :initarg :inverse-relation
		     :accessor inverse-temporal-relation)
   ;; This is an index into the transitivity table
   (index :initform nil :initarg :index
	  :accessor temporal-relation-index)
   ;; This is a prime number used in clause product
   (prime-index :initform nil :initarg :prime-index
		:accessor temporal-relation-prime-index)))

;;;--------------------------------------------------------------------------

(defun MAKE-TEMPORAL-RELATION (name &rest rest)
  (apply #'make-instance 'TEMPORAL-RELATION :name name rest))

;;;--------------------------------------------------------------------------
;;; NODES
;;;--------------------------------------------------------------------------

(defclass NODE (UTIL::TREE-NODE)
  ((util::value :accessor node-value)
   (util::parent :accessor node-parent)
   (util::children :accessor node-children)))

;;;--------------------------------------------------------------------------

(defclass AND-NODE (NODE)
  ())

;;;--------------------------------------------------------------------------

(defclass OR-NODE (NODE)
  ())

;;;**************************************************************************
;;; Part 2: Composite Entities: CLAUSES
;;;**************************************************************************

;;; This is the root class for all composite entities.

(defclass COMPOSITE-ENTITY (ENTITY-MIXIN UTIL::VALUE)
  ((util::name :initform nil
	       :accessor entity-name
	       :accessor object-name)
   (util::value :initform nil 
		:accessor entity-value
   		:accessor object-value)))

;;;--------------------------------------------------------------------------
;;; CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass CLAUSE-ENTITY (COMPOSITE-ENTITY)
  ((predicate :initarg :predicate
	      :accessor clause-entity-predicate)
   (arguments :initarg :arguments
	      :accessor clause-entity-arguments)
   (constraint :initarg :constraint :initform nil
	       :accessor clause-entity-constraint)
   (variables :initarg :variables
	      :accessor clause-entity-variables)
   (description :initarg :description  :initform nil
		:accessor clause-entity-description)
   (bindings :initarg :bindings :initform nil
		:accessor clause-entity-bindings)
   (arity  :initarg :arity :initform 0
	   :accessor clause-entity-arity)))

;;;--------------------------------------------------------------------------

(defmethod INITIALIZE-INSTANCE :after ((clause clause-entity) &rest initargs)
  (declare (ignore initargs))
  ;; Cache the number of arguments.
  (setf (clause-entity-arity clause)
    (length (clause-entity-arguments clause))))

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj CLAUSE-ENTITY) stream)
  (unless *print-entity-readably*
    (format stream "#<Clause: "))    
  (let ((*print-entity-readably* t))
    (format stream "(~a" (clause-entity-predicate obj))
    (dolist (arg (clause-entity-arguments obj))
      (when arg
	(format stream " ~a" arg)))
    (format stream ")"))
  (unless *print-entity-readably*
    (format stream ">")))

;;;--------------------------------------------------------------------------
;;; SIMPLE-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass SIMPLE-CLAUSE-ENTITY (CLAUSE-ENTITY)
  ())
  
;;;--------------------------------------------------------------------------
;;; TEMPORAL-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass TEMPORAL-CLAUSE-ENTITY (SIMPLE-CLAUSE-ENTITY)
  (;; This is a product of indices of the temporal relations in
   ;; in clause. Used to efficiently compare temporal clauses.
   (product :initform 1 :initarg :product
	    :accessor temporal-clause-product)
   (complement :initform nil 
	       :accessor temporal-clause-complement)))

;;;--------------------------------------------------------------------------

(defmethod INITIALIZE-INSTANCE :after ((clause TEMPORAL-CLAUSE-ENTITY) &rest initargs)
  ;; Compute the temporal product.
  (setf (temporal-clause-product clause)
    (compute-relations-product (temporal-clause-relations clause))))

;;;--------------------------------------------------------------------------
;;; COMPOSITE-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass COMPOSITE-CLAUSE-ENTITY (CLAUSE-ENTITY)
  ())
  
;;;--------------------------------------------------------------------------
;;; CONJUNCTIVE-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass CONJUNCTIVE-CLAUSE-ENTITY (COMPOSITE-CLAUSE-ENTITY)
  ())

;;;--------------------------------------------------------------------------
;;; DISJUNCTIVE-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass DISJUNCTIVE-CLAUSE-ENTITY (COMPOSITE-CLAUSE-ENTITY)
  ())

;;;--------------------------------------------------------------------------
;;;  NEGATED-CLAUSE-ENTITY
;;;--------------------------------------------------------------------------

(defclass NEGATED-CLAUSE-ENTITY (COMPOSITE-CLAUSE-ENTITY)
  ())

;;;--------------------------------------------------------------------------
;;;  HOLDS CLAUSE
;;;--------------------------------------------------------------------------

(defclass HOLDS-CLAUSE-ENTITY (COMPOSITE-CLAUSE-ENTITY)
  ((time-of :initarg :time-of :initform nil
	    :accessor time-of)))

;;;--------------------------------------------------------------------------
;;;  HORN CLAUSE
;;;--------------------------------------------------------------------------

(defclass HORN-CLAUSE-ENTITY (COMPOSITE-CLAUSE-ENTITY)
  ())

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj HORN-CLAUSE-ENTITY) stream)
  (cond (*print-readably*
	 (call-next-method))
	(*print-entity-readably*
	 (format stream "~a" (entity-name obj)))
	(t
	 (format stream "#<HORN: ~a <-- ~a>"
		 (clause-description (horn-clause-lhs obj))
		 (clause-description (horn-clause-rhs obj))))))
		 

;;;--------------------------------------------------------------------------

#+IGNORE
(defmethod PRINT-OBJECT ((obj HORN-CLAUSE-ENTITY) stream)
  (unless *print-entity-readably*
    (format stream "#<Horn: "))    
  #+IGNORE
  (let ((*print-entity-readably* t))
    (format stream "(~a" (clause-entity-predicate obj)))
  (cond (*print-entity-readably*
	 (let ((arg (horn-clause-lhs obj)))
	   (if (variable-p arg)
	       (format stream " ~a" arg)
	     (format stream " ~a..." arg))))
	(t
	 (dolist (arg (clause-entity-arguments obj))
	   (format stream " ~a" arg))))
  #+IGNORE
  (format stream ")")
  (unless *print-entity-readably*
    (format stream ">")))

;;;--------------------------------------------------------------------------
;;; DATABASE-ENTITY
;;;--------------------------------------------------------------------------

(defclass DATABASE-ENTITY (COMPOSITE-ENTITY)
  ((predicates :initarg :predicates
	       :initform  (make-hash-table :test #'equalp)
	       :accessor database-predicates)
   (builtin-predicates :initarg :predicates
	       :initform  nil
	       :accessor database-builtin-predicates)
   (clauses    :initarg :clauses
	       :initform (make-hash-table :test #'equalp)
	       :accessor database-clauses)
   (constants  :initarg :constants
	       :initform (make-hash-table :test #'equalp)
	       :accessor database-constants)
   (variable-counter :initarg :variable-counter :initform 0
		     :accessor database-variable-counter)
   (proof-trees :initarg :proof-trees :initform nil
		:accessor database-proof-trees)))

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj DATABASE-ENTITY) stream)
  (format stream "#<Database: ~a>" (object-name obj)))

;;;--------------------------------------------------------------------------
;;; Default Database
;;;--------------------------------------------------------------------------

;;; This is the default system database.

(defvar *DATABASE* (make-instance 'database-entity :name "Default"))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
