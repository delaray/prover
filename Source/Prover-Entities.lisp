(in-package :PROVER)

;;;**************************************************************************
;;;
;;; ATOMIC ENTITIES
;;; ---------------
;;;
;;; Constants
;;; Predicates
;;; Variables
;;;
;;; Builtin Predicates
;;; Skolem Constants
;;;
;;;**************************************************************************
;;;
;;; Part 1: Constants
;;; -----------------
;;;
;;; MAKE-CONSTANT
;;; ADD-CONSTANT
;;; DELETE-CONSTANT
;;; FIND-CONSTANT
;;; ENSURE-CONSTANT
;;;
;;; MAP-CONSTANTS
;;; PRINT-CONSTANTS
;;; RESET-CONSTANTS
;;;
;;;
;;; Part 2: Predicates
;;; ------------------
;;;
;;; MAKE-PREDICATE
;;; ADD-PREDICATE
;;; FIND-PREDICATE
;;; DELETE-PREDICATE
;;; ENSURE-PREDICATE
;;;
;;; MAP-PREDICATES
;;; RESET-PREDICATES
;;; PRINT-PREDICATES
;;;
;;;
;;; Part 3: Variables
;;; -----------------
;;;
;;; MAKE-VARIABLE
;;; FIND-VARIABLE
;;; VARIABLE-P
;;;
;;; BIND-VARIABLE
;;; UNBIND-VARIABLE
;;; VARIABLE-BOUND-P
;;;
;;; SYMBOL-IS-VARIABLE
;;;
;;; MAKE-BINDING (variable value)
;;; BINDING-VARIABLE (binding)    
;;; BINDING-VALUE (binding)
;;; FIND-BINDING
;;; REPLACE-BINDINGS-VARIABLES
;;;
;;;
;;; Part 4: Advanced Constants
;;; --------------------------
;;;
;;; MAKE-BUILTIN-PREDICATE
;;; BUILTIN-PREDICATE-P
;;;
;;; MAKE-SKOLEM-CONSTANT
;;;
;;;--------------------------------------------------------------------------



;;;**************************************************************************
;;; PART 1: CONSTANT LITERALS
;;;**************************************************************************

(defun INTEGER-NAME (integer)(format nil "~s" integer))

;;;--------------------------------------------------------------------------
;;; MAKE-CONSTANT
;;;--------------------------------------------------------------------------

(defmethod MAKE-CONSTANT ((name STRING)
			  &key
			  (class 'CONSTANT-ENTITY)
			  (database *database*)
			  (value name)
			  (initargs nil))
  (let ((constant (apply #'make-instance class :name name :value value initargs)))
    (add-constant constant :database database)
    constant))

;;;--------------------------------------------------------------------------

(defmethod MAKE-CONSTANT ((constant SYMBOL) &rest rest)
  (apply #'make-constant (symbol-name constant) :value constant rest))

;;;--------------------------------------------------------------------------

(defmethod MAKE-CONSTANT ((constant INTEGER) &rest rest)
  (apply #'make-constant (integer-name constant) :value constant rest))

;;;--------------------------------------------------------------------------
;;; ADD-CONSTANT
;;;--------------------------------------------------------------------------

(defun ADD-CONSTANT (constant &key (database *database*))
  (setf (gethash (entity-name constant) (database-constants database))
    constant))

;;;--------------------------------------------------------------------------
;;; DELETE-CONSTANT
;;;--------------------------------------------------------------------------

(defmethod DELETE-CONSTANT ((constant-name STRING)
			    &key
			    (database *database*))
  (remhash constant-name (database-constants database)))

;;;--------------------------------------------------------------------------

(defmethod DELETE-CONSTANT ((constant CONSTANT-ENTITY)
			    &key
			    (database *database*))
  (delete-constant (object-name constant) :database database))
 
;;;--------------------------------------------------------------------------

(defmethod DELETE-CONSTANT ((constant SYMBOL)
			    &key
			    (database *database*))
  (delete-constant (symbol-name constant) :database database))
 
;;;--------------------------------------------------------------------------
;;; FIND-CONSTANT
;;;--------------------------------------------------------------------------

(defmethod FIND-CONSTANT ((constant-name STRING)
			  &key 
			  (database *database*)
			  (create-p nil)
			  (class 'CONSTANT-ENTITY))
  (let ((constant (gethash constant-name (database-constants database))))
    (unless constant
      (when create-p
	(setf constant (make-constant constant-name :class class :database database))))
    constant))

;;;--------------------------------------------------------------------------

(defmethod FIND-CONSTANT ((constant SYMBOL) &rest rest &key (database *database*))
  (declare (ignore rest))
  (find-constant (symbol-name constant) :database database))

;;;--------------------------------------------------------------------------

(defmethod FIND-CONSTANT ((constant INTEGER) &rest rest &key (database *database*))
  (declare (ignore rest))
  (find-constant (symbol-name constant) :database database))

;;;--------------------------------------------------------------------------
;;; ENSURE-CONSTANT
;;;--------------------------------------------------------------------------

(defmethod ENSURE-CONSTANT ((constant-name STRING)
			    &key
			    (database *database*)
			    (class 'CONSTANT-ENTITY))
  (or (find-constant constant-name :database database)
      (make-constant constant-name :class class :database database)))

;;;--------------------------------------------------------------------------

(defmethod ENSURE-CONSTANT ((constant SYMBOL) &rest rest)
  (apply #'ensure-constant (symbol-name constant) rest))
			    
;;;--------------------------------------------------------------------------

(defmethod ENSURE-CONSTANT ((constant INTEGER) &rest rest)
  (apply #'ensure-constant (integer-name constant) rest))

;;;--------------------------------------------------------------------------

#+IGNORE
(defun FIND-OR-MAKE-CONSTANT (constant-name
			      &key
			      (database *database*)
			      (class 'CONSTANT-ENTITY))
  (or (find-constant constant-name :database database)
      (make-constant constant-name :class class :database database)))


;;;--------------------------------------------------------------------------
;;; MAP-CONSTANTS
;;;--------------------------------------------------------------------------

(defun MAP-CONSTANTS (function &key (database *database*))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (funcall function v))
	   (database-constants database))
  t)

;;;--------------------------------------------------------------------------

(defun PRINT-CONSTANTS (&key (database *database*))
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (print value))
	   (database-constants database))
  t)

;;;--------------------------------------------------------------------------

(defun RESET-CONSTANTS (&key (database *database*))
  (clrhash (database-constants database))
  t)


;;;**************************************************************************
;;; Part 2: Predicates
;;;**************************************************************************


;;;--------------------------------------------------------------------------
;;; MAKE-PREDICATE
;;;--------------------------------------------------------------------------

(defmethod MAKE-PREDICATE ((predicate-name STRING)
			   &key 
			   (class 'PREDICATE-ENTITY)
			   (database *database*)
			   (value predicate-name)
			   (initargs nil))
  (let ((predicate (apply #'make-instance class :name predicate-name :value value initargs)))
    ;; Register predicate in the database
    (add-predicate predicate :database database)
    ;; Return the new predicate
    predicate))  

;;;--------------------------------------------------------------------------

(defmethod MAKE-PREDICATE ((predicate SYMBOL) &rest rest)
  (apply #'make-predicate (symbol-name predicate) :value predicate rest))

;;;--------------------------------------------------------------------------
;;; ADD-PREDICATE
;;;--------------------------------------------------------------------------

(defmethod ADD-PREDICATE ((predicate PREDICATE-ENTITY) &key (database *database*))
  (setf (gethash (object-name predicate) (database-predicates database)) predicate)
  predicate)

;;;--------------------------------------------------------------------------
;;; FIND-PREDICATE
;;;--------------------------------------------------------------------------

(defmethod FIND-PREDICATE ((predicate-name STRING)
			   &key
			   (database *database*)
			   (create-p nil))
  (let ((predicate (gethash predicate-name (database-predicates database))))
    (unless predicate
      (when create-p
	(setf predicate (make-predicate predicate-name :database database))))
    predicate))

;;;--------------------------------------------------------------------------

(defmethod FIND-PREDICATE ((predicate SYMBOL) &rest rest &key (database *database*))
  (declare (ignore rest))
  (find-predicate (symbol-name predicate) :database database))

;;;--------------------------------------------------------------------------
;;; DELETE-PREDICATE
;;;--------------------------------------------------------------------------

(defmethod DELETE-PREDICATE ((predicate-name STRING)
			     &key
			     (database *database*))
  (remhash predicate-name (database-predicates database)))
 
;;;--------------------------------------------------------------------------

(defmethod DELETE-PREDICATE ((predicate SYMBOL)
			     &key
			     (database *database*))
  (delete-predicate (symbol-name predicate) :database database))

;;;--------------------------------------------------------------------------

(defmethod DELETE-PREDICATE ((predicate PREDICATE-ENTITY)
			     &key
			     (database *database*))
  (delete-predicate (object-name predicate) :database database))
 
;;;--------------------------------------------------------------------------
;;; ENSURE-PREDICATE
;;;--------------------------------------------------------------------------

(defmethod ENSURE-PREDICATE ((predicate-name STRING)
			     &key
			     (database *database*)
			     (class 'PREDICATE-ENTITY))
  (or (find-predicate predicate-name :database database)
      (make-predicate predicate-name :class class :database database)))

;;;--------------------------------------------------------------------------

(defmethod ENSURE-PREDICATE ((predicate SYMBOL)
			     &key
			     (database *database*)
			     (class 'PREDICATE-ENTITY))
  (ensure-predicate (symbol-name predicate) :database database :class class))

;;;--------------------------------------------------------------------------

#+IGNORE
(defun FIND-OR-MAKE-PREDICATE (predicate-name &key (database *database*))
  (or (find-predicate predicate-name :database database)
      (make-predicate predicate-name :database database)))


;;;--------------------------------------------------------------------------
;;; MAP-PREDICATES
;;;--------------------------------------------------------------------------

(defun MAP-PREDICATES (function &key (database *database*))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (funcall function v))
	   (database-predicates database))
  t)

;;;--------------------------------------------------------------------------
;;; FIND-ALL-PREDICATES
;;;--------------------------------------------------------------------------

(defun FIND-ALL-PREDICATES (&key (database *database*)(sort t))
  (let ((predicates nil))
    (map-predicates #'(lambda (p)(push p predicates)) :database database)
    (when sort
      (setf predicates (sort predicates #'string<= :key #'object-name)))
    predicates))

;;;--------------------------------------------------------------------------
;;; RESET-PREDICATES
;;;--------------------------------------------------------------------------

(defun RESET-PREDICATES (&key (database *database*))
  (clrhash (database-predicates database))
  t)

;;;--------------------------------------------------------------------------
;;; PRINT-PREDICATES
;;;--------------------------------------------------------------------------

(defun PRINT-PREDICATES (&key (stream t)(database *database*))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (format stream "~%~a" v))
	   (database-predicates database)))


;;;**************************************************************************
;;; VARIABLE ENTITES
;;;**************************************************************************

;;;--------------------------------------------------------------------------
;;; MAKE-VARIABLE
;;;--------------------------------------------------------------------------

(defmethod MAKE-VARIABLE ((variable-name STRING)
			  &key
			  (database *database*))
  (make-instance 'variable-entity
                 :name variable-name
		 :count (database-variable-counter database)))

;;;--------------------------------------------------------------------------

(defmethod MAKE-VARIABLE :around ((name STRING) 
				  &key
				  (database *database*))
  ;; Ensure variable name starts with a "?"
  (unless (char= (aref name 0) #\?)
    (setf name (concatenate 'STRING "?" name)))
  ;; Now create the variavble entity
  (call-next-method name :database database))

;;;--------------------------------------------------------------------------

(defmethod MAKE-VARIABLE :before ((name STRING)
				  &key
				  (database *database*))
  ;; Increment the DB variable counter
  (incf (database-variable-counter database)))

;;;--------------------------------------------------------------------------

(defmethod MAKE-VARIABLE ((variable SYMBOL)
			  &key
			  (database *database*))
  (make-variable (symbol-name variable) :database database))
 
;;;--------------------------------------------------------------------------
;;; FIND-VARIABLE
;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLE ((variable STRING) (variables LIST))
  (find variable variables :test #'string-equal :key #'object-name))

;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLE ((variable SYMBOL) (variables LIST))
  (find-variable (symbol-name variable) variables))

;;;--------------------------------------------------------------------------
;;; VARIABLE-P
;;;--------------------------------------------------------------------------

(defmethod VARIABLE-P ((variable VARIABLE-ENTITY))
  t)

;;;--------------------------------------------------------------------------

(defmethod VARIABLE-P ((entity t))
  nil)

;;;--------------------------------------------------------------------------

(defmethod VARIABLE-P ((variable-name STRING))
  (char= (aref variable-name 0) #\?)) 

;;;--------------------------------------------------------------------------

(defmethod VARIABLE-P ((variable SYMBOL))
  (variable-p (symbol-name variable)))

;;;--------------------------------------------------------------------------
;;;  SYMBOL-IS-VARIABLE
;;;--------------------------------------------------------------------------

;;; These are to be deprecated.

(defmethod SYMBOL-IS-VARIABLE ((symbol symbol))
  (variable-p (symbol-name symbol)))

;;;--------------------------------------------------------------------------

(defmethod SYMBOL-IS-VARIABLE ((symbol t))
  nil)

;;;--------------------------------------------------------------------------
;;; VARIABLE-BOUND-P
;;;--------------------------------------------------------------------------

(defmethod VARIABLE-BOUND-P ((variable VARIABLE-ENTITY))
  (object-bound-p variable))

;;;--------------------------------------------------------------------------
;;; BIND-VARIABLE 
;;;--------------------------------------------------------------------------

(defmethod BIND-VARIABLE ((variable VARIABLE-ENTITY) value)
  (setf (object-bound-p variable) t)
  (setf (object-value variable) value))

;;;--------------------------------------------------------------------------
;;; UNBIND-VARIABLE
;;;--------------------------------------------------------------------------

;;; This unbinds a variable.

(defmethod UNBIND-VARIABLE ((variable VARIABLE-ENTITY))
  (setf (object-bound-p variable) nil)
  (setf (object-value variable) nil))

;;;--------------------------------------------------------------------------
;;; VARIABLE-VALUE
;;;--------------------------------------------------------------------------

;;; Recursively retrieve a variable's deepest or shallowest binding dependending
;;; on the value of <deepest-p>.

(defmethod VARIABLE-VALUE ((variable VARIABLE-ENTITY) &key (deepest-p t))
  (cond ((and deepest-p (variable-p (object-value variable)))
	 (variable-value (object-value variable)))
	(t
	 (object-value variable))))

;;;--------------------------------------------------------------------------
;;; BINDINGS
;;;--------------------------------------------------------------------------

;;; Currently a binding is simply a list of the form:
;;;
;;; (<variable-entity> <value>)

(defun MAKE-BINDING (variable-entity value)
  `(,variable-entity ,value))

;;;--------------------------------------------------------------------------

(defun BINDING-VARIABLE (binding)
  (first binding))

;;;--------------------------------------------------------------------------

(defun SET-BINDING-VARIABLE (binding variable)
  (setf (first binding) variable))

;;;--------------------------------------------------------------------------

(defun BINDING-VALUE (binding)
  (second binding))

;;;--------------------------------------------------------------------------

(defun FIND-BINDING (variable bindings)
  (find variable bindings :test #'eq :key #'binding-variable))

;;;------------------------------------------------------------------------

(defun REPLACE-BINDINGS-VARIABLES (bindings substitutions)
  (dolist (binding bindings)
    (when (find (binding-variable binding) substitutions :test #'eq :key #'first)
      (set-binding-variable binding
	(second (find (binding-variable binding) substitutions :test #'eq :key #'first))))))

;;;--------------------------------------------------------------------------
;;; VARIABLE VALUES
;;;--------------------------------------------------------------------------

(defun VALUE-VAR (var bindings)
  (if bindings (cadr (assoc var bindings)) nil))

;;;--------------------------------------------------------------------------

(defun VALUE-VARS (var-list bindings)
  (mapcar #'(lambda (var)
              (value-var var bindings))
          var-list))


;;;**************************************************************************
;;; Part 4: SPECIAIZED CONSTANTS
;;;**************************************************************************

;;;--------------------------------------------------------------------------
;;; MAKE-BUILTIN-PREDICATE
;;;--------------------------------------------------------------------------

(defun MAKE-BUILTIN-PREDICATE (name &key (database *database*)(class 'BUILTIN-PREDICATE))
  (let ((predicate (make-predicate name :database database :class class)))
    (push predicate (database-builtin-predicates database))
    predicate))

;;;--------------------------------------------------------------------------
;;; BUILTIN-PREDICATE-P
;;;--------------------------------------------------------------------------

(defmethod BUILTIN-PREDICATE-P ((predicate BUILTIN-PREDICATE) &key (database *database*))
  (declare (ignore database))
  t)

;;;--------------------------------------------------------------------------

(defmethod BUILTIN-PREDICATE-P ((predicate t) &key (database *database*))
  (declare (ignore database))
  nil)

;;;--------------------------------------------------------------------------

(defmethod BUILTIN-PREDICATE-P ((predicate SYMBOL) &key (database *database*))
  (builtin-predicate-p (find-predicate predicate :database database)))

;;;--------------------------------------------------------------------------
;;; MAKE-SKOLEM-CONSTANT 
;;;--------------------------------------------------------------------------

(defvar *skolem-count* 0)

;;; This creates a skolem constant for 'interval'.

(defun MAKE-SKOLEM-CONSTANT (interval &key (class 'TEMPORAL-INTERVAL)
					   (database *database*))
  (incf *skolem-count*)
  (let ((name (read-from-string (format nil "SK~a-~a"
					*skolem-count*
					(entity-name interval)))))
    (make-constant name :class class :database database)))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
