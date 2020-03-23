(in-package :PROVER)

;;;**************************************************************************
;;; Clause Entities
;;;**************************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; Part 1: CLAUSE CONSTUCTORS
;;; --------------------------
;;;
;;; MAKE-CLAUSE
;;; MAKE-CONJUNCTIVE-CLAUSE
;;; MAKE-DISJUNCTIVE-CLAUSE 
;;; MAKE-HORN-CLAUSE 
;;; MAKE-HOLDS-CLAUSE 
;;;
;;;
;;;**************************************************************************

;;;**************************************************************************
;;; Part 1: CLAUSE CONSTUCTORS
;;;**************************************************************************

;;;----------------------------------------------------------------------------
;;; MAKE-CLAUSE
;;;----------------------------------------------------------------------------

;;; This is mostly an internal function intented to be clled by parse-clause
;;; which takes a descriptive declarative clause description and creates and
;;; internal clause object.

(defmethod MAKE-CLAUSE ((predicate PREDICATE-ENTITY)(args LIST) 
			&rest rest
			&key 
			(class 'SIMPLE-CLAUSE-ENTITY)
			(description nil)
			(variables nil)
			(bindings nil))
  ;; Create the clause object
  (apply #'make-instance class
	 :predicate predicate
	 :description description
	 :variables variables
	 :bindings bindings
	 :arguments args
	 :arity (length args)
	 rest))

;;;----------------------------------------------------------------------------

(defmethod MAKE-CLAUSE ((predicate STRING)(args LIST) &rest rest)
  (apply #'make-clause (ensure-predicate predicate) args rest))
 
;;;----------------------------------------------------------------------------
;;; MAKE-CONJUNCTIVE-CLAUSE
;;;----------------------------------------------------------------------------

(defmethod MAKE-CONJUNCTIVE-CLAUSE ((arguments LIST) &rest rest)
  (apply #'make-clause (find-predicate 'AND) arguments
	 :class  'CONJUNCTIVE-CLAUSE-ENTITY
	 rest))

;;;----------------------------------------------------------------------------
;;; MAKE-DISJUNCTIVE-CLAUSE 
;;;----------------------------------------------------------------------------

(defmethod MAKE-DISJUNCTIVE-CLAUSE ((clauses LIST) &rest rest)
  (let ((disjunctive-clause 
	 (apply #'make-clause (find-predicate 'OR) clauses
		:class 'DISJUNCTIVE-CLAUSE-ENTITY 
		rest)))
    (dolist (clause clauses)
      (setf (clause-bindings disjunctive-clause)
	(append (clause-bindings clause)(clause-bindings disjunctive-clause)))
      (setf (clause-variables disjunctive-clause)
	(append (clause-variables clause)(clause-variables disjunctive-clause))))
    disjunctive-clause))

;;;----------------------------------------------------------------------------
;;; MAKE-HORN-CLAUSE
;;;----------------------------------------------------------------------------

(defun MAKE-HORN-CLAUSE (lhs rhs &rest rest)
  (apply #'make-clause (find-predicate 'HORN) `(,lhs ,rhs)
	 :class 'HORN-CLAUSE-ENTITY
	 rest))

;;;--------------------------------------------------------------------------
;;; MAKE-HOLDS-CLAUSE
;;;--------------------------------------------------------------------------

(defun MAKE-HOLDS-CLAUSE (clause time-of constraint 
			  &key (class 'HOLDS-CLAUSE-ENTITY)
			       (variables nil)
			       (description nil))
  (make-clause (find-predicate 'HOLDS) `(,clause ,time-of ,constraint)
	       :class class
	       :time-of time-of
	       :constraints `(,constraint)
	       :variables variables
	       :description description))

;;;**************************************************************************
;;; CLAUSE ACCESSORS
;;;**************************************************************************

;;;----------------------------------------------------------------------------
;;; CLAUSE-PREDICATE
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-PREDICATE ((clause CLAUSE-ENTITY))
  (clause-entity-predicate clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-PREDICATE) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-predicate clause) nv))

;;;----------------------------------------------------------------------------
;;; CLAUSE-ARGUMENTS
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-ARGUMENTS ((clause CLAUSE-ENTITY))
  (clause-entity-arguments clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-ARGUMENTS) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-arguments clause) nv))

;;;----------------------------------------------------------------------------
;;; CLAUSE-CONSTRAINT
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-CONSTRAINT ((clause CLAUSE-ENTITY))
  (clause-entity-constraint clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-CONSTRAINT) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-constraint clause) nv))

;;;----------------------------------------------------------------------------
;;; CLAUSE-VARIABLES 
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-VARIABLES ((clause CLAUSE-ENTITY))
  (clause-entity-variables clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-VARIABLES) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-variables clause) nv))

;;;----------------------------------------------------------------------------
;;; CLAUSE-BINDINGS 
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-BINDINGS ((clause CLAUSE-ENTITY))
  (clause-entity-bindings clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-BINDINGS) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-bindings clause) nv))

;;;----------------------------------------------------------------------------
;;; CLAUSE-DESCRIPTION
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-DESCRIPTION ((clause CLAUSE-ENTITY))
  (clause-entity-description clause))

;;;----------------------------------------------------------------------------

(defmethod (SETF CLAUSE-DESCRIPTION) (nv (clause CLAUSE-ENTITY))
  (setf (clause-entity-description clause) nv))

;;;**************************************************************************
;;; CLAUSE OPERATIONS
;;;**************************************************************************

;;; This function adds '*database*' to *DATABASE*.

(defmethod ADD-CLAUSE ((clause CLAUSE-ENTITY) &key (database *DATABASE*))
  (setf (gethash (clause-entity-predicate clause) (database-clauses database))
    (cons clause (gethash (clause-entity-predicate clause) (database-clauses database))))
  clause)

;;;----------------------------------------------------------------------------

;;; This function adds '*database*' to *DATABASE*.

(defmethod ADD-CLAUSE ((clause-description LIST) &key (database *DATABASE*))
  (add-clause (parse-clause clause-description) :database database))

;;;----------------------------------------------------------------------------
;;; DELETE-CLAUSE
;;;----------------------------------------------------------------------------

;;; This function deletes '*database*' from *DATABASE*.

(defmethod DELETE-CLAUSE ((clause clause-entity) &key (database *DATABASE*))
  (setf (gethash (clause-entity-predicate clause) (database-clauses database))
    (delete clause (gethash (clause-entity-predicate clause)
			    (database-clauses database))
	    :test #'eq)))

;;;--------------------------------------------------------------------------
;;; FINDING CLAUSES
;;;--------------------------------------------------------------------------

(defmethod FIND-CLAUSES-FOR-PREDICATE ((predicate-name symbol) &key (database *DATABASE*))
  (let ((predicate (find-predicate predicate-name :database database)))
    (when predicate
      (find-clauses-for-predicate predicate :database database))))

;;;--------------------------------------------------------------------------

(defmethod FIND-CLAUSES-FOR-PREDICATE ((predicate predicate-entity) &key (database *DATABASE*))
  (gethash predicate (database-clauses database)))

;;;--------------------------------------------------------------------------

(defmethod FIND-CLAUSE-FOR-PREDICATE ((predicate-name symbol) &key (database *DATABASE*))
  (let ((predicate (find-predicate predicate-name :database database)))
    (when predicate
      (find-clause-for-predicate predicate :database database))))

;;;--------------------------------------------------------------------------

(defmethod FIND-CLAUSE-FOR-PREDICATE ((predicate predicate-entity) &key (database *DATABASE*))
  (first (gethash predicate (database-clauses database))))

;;;--------------------------------------------------------------------------
;;; GROUND-P
;;;--------------------------------------------------------------------------

;;; A clause is fully ground if either it contains no variables, or if it
;;; does contain variables, they are all bound.

(defmethod GROUNDED-P ((clause clause-entity))
  (and (grounded-p (clause-entity-predicate clause))
       (every #'grounded-p (clause-entity-arguments clause))))

;;;--------------------------------------------------------------------------

(defmethod GROUNDED-P ((entity cons))
  (every #'grounded-p entity))

;;;--------------------------------------------------------------------------

(defmethod GROUNDED-P ((entity variable-entity))
  nil)

;;;--------------------------------------------------------------------------

(defmethod GROUNDED-P ((entity constant-entity))
  t)

;;;--------------------------------------------------------------------------
;;; CLAUSE-CONTAINS-VARIABLES-P
;;;--------------------------------------------------------------------------

(defmethod CLAUSE-CONTAINS-VARIABLES-P ((clause CLAUSE-ENTITY))
  (not (grounded-p clause)))

;;;------------------------------------------------------------------------
;;; INSTANTIATE
;;;------------------------------------------------------------------------

(defmethod INSTANTIATE ((clause CLAUSE-ENTITY)(bindings t))
  (instantiate-clause clause :bindings bindings))

;;;--------------------------------------------------------------------------
;;; INSTANTIATE-CLAUSE
;;;--------------------------------------------------------------------------

(defmethod INSTANTIATE-CLAUSE ((clause CLAUSE-ENTITY) &key (bindings (clause-bindings clause)))
  (instantiate-bindings bindings)
  clause)

;;;--------------------------------------------------------------------------
;;; MAP-CLAUSES
;;;--------------------------------------------------------------------------

(defun MAP-CLAUSES (function &key (database *DATABASE*))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (funcall function v))
	   (database-clauses database))
  t)

;;;--------------------------------------------------------------------------
;;; ADD-CONJUNCTION
;;;--------------------------------------------------------------------------

;;;--------------------------------------------------------------------------
;;; RESET-CLAUSES
;;;--------------------------------------------------------------------------

(defun RESET-CLAUSES (&key (database *DATABASE*))
  (clrhash (database-clauses database))
  t)

;;;--------------------------------------------------------------------------
;;; COPY-CLAUSE
;;;--------------------------------------------------------------------------

;;; This method creates a new clause object from the specified clause
;;; by re-parsing the original clause description. This is not however
;;; sufficient as the current clause bindings also need to be copied.

(defmethod COPY-CLAUSE ((clause CLAUSE-ENTITY) &key (database *database*))
  (let* ((new-clause (parse-clause (clause-description clause) :database database)))
    (setf (clause-bindings new-clause)
      (copy-clause-bindings clause new-clause))
    new-clause))

;;;--------------------------------------------------------------------------

(defmethod COPY-CLAUSE-BINDINGS ((clause1 CLAUSE-ENTITY)(clause2 CLAUSE-ENTITY))
  (let* ((bindings (clause-bindings clause1))
	 (new-bindings nil))
    (dolist (binding bindings)
      (let ((var (find (object-name (binding-variable binding))
		       (clause-entity-variables clause2)
		       :key #'object-name :test #'equalp)))
	(when var
	  (push (make-binding var (binding-value binding)) new-bindings))))
    (nreverse new-bindings)))
									   
;;;--------------------------------------------------------------------------
;;; HORN CLAUSES
;;;--------------------------------------------------------------------------

(defmethod HORN-CLAUSE-LHS ((clause horn-clause-entity))
  (first (clause-entity-arguments clause)))

;;;--------------------------------------------------------------------------

(defmethod HORN-CLAUSE-RHS ((clause horn-clause-entity))
  (second (clause-entity-arguments clause)))

;;;--------------------------------------------------------------------------

(defun ALL-HORN-CLAUSES (&key (database *DATABASE*))
  (find-clauses-for-predicate 'HORN :database database))

;;;**************************************************************************
;;; Part 2: CLAUSE PARSER FUNCTIONS
;;;**************************************************************************

;;;--------------------------------------------------------------------------
;;; PROCESS-CLAUSE-DESCRIPTIONS
;;;--------------------------------------------------------------------------

(defun PROCESS-CLAUSE-DESCRIPTIONS (clause-descriptions &key (database *database*))
  (dolist (clause-description clause-descriptions)
    (process-clause-description clause-description :database database)))

;;;--------------------------------------------------------------------------
;;; PROCESS-CLAUSE-DESCRIPTION
;;;--------------------------------------------------------------------------

(defun PROCESS-CLAUSE-DESCRIPTION (clause-description &key (database *database*))
  (let ((predicate-name  (first clause-description))
	(clause nil))
    (cond ((eq predicate-name 'TEMPORAL)
	   (process-temporal-clause clause-description :database database))
	  (t
	   (setf clause (parse-clause clause-description :database database))
	   (add-clause clause :database database)
	   clause))))


;;;--------------------------------------------------------------------------
;;; PROCESS-TEMPORAL-CLAUSE
;;;--------------------------------------------------------------------------

;;; This function returns two values. The first is either a temporal clause or
;;; nil if no new clause was created.

(defun PROCESS-TEMPORAL-CLAUSE (temporal-clause-description &key
							    (database *DATABASE*))
  (let* ((temporal-clause (parse-temporal-clause temporal-clause-description
						 :database database))
	 (relations (temporal-clause-relations temporal-clause))
	 (i1 (temporal-clause-interval-1 temporal-clause))
	 (i2 (temporal-clause-interval-2 temporal-clause))
	 (existing-clause (find-temporal-clause i1 i2 :database database)))
    (cond ((not existing-clause)
	   ;; There is no clause relating i1 & i2 so add the clause
	   ;; and propagate the implications.
	   (add-clause temporal-clause :database database)
	   #+IGNORE
	   (propagate-clause temporal-clause database)
	   ;; Return the new clause
	   (values temporal-clause nil))
	  (t
	   ;; There already exists a clause relating i1 & i2
	   (cond ((relations-equivalent-in-clause-p relations existing-clause)
		  ;; Just return the existing clause
		  (values existing-clause nil))
		 ((relations-constrain-clause-p relations existing-clause)
		  (setf (temporal-clause-relations existing-clause) relations)
		  (setf (temporal-clause-relations
			 (temporal-clause-complement existing-clause))
		    (inverse-temporal-relations relations))
		  (values existing-clause nil))
		 (t
		  ;; relations must conflict with existing clause
		  (values nil t)))))))


;;;--------------------------------------------------------------------------
;;; PARSE-CLAUSE
;;;--------------------------------------------------------------------------

;;; This creates a simple-clause entity as well as its atomic components.

(defmethod PARSE-CLAUSE ((clause-description LIST) &key variables (database *database*))
  (cond ((typep clause-description 'clause-entity)
	  clause-description)
	((atom clause-description)
	 (parse-atomic-argument clause-description
				:database database
				:variables variables))
	((atom (first clause-description))
	 (let* ((predicate-name  (first clause-description)))
	   (cond ((eq predicate-name 'HOLDS)
		  (parse-holds-clause clause-description 
				      :database database :variables variables))
		 ((eq predicate-name 'TEMPORAL)
		  (parse-temporal-clause clause-description
					 :database database :variables variables))
		 ((eq predicate-name 'HORN)
		  (parse-horn-clause clause-description
				     :database database :variables variables))
		 #+IGNORE
		 ((eq predicate-name 'OPERATOR)
		  (parse-operator-clause clause-description
					 :database database :variables variables))
		 #+IGNORE
		 ((eq predicate-name 'GOAL)
		  (parse-goal-clause clause-description
				     :database database :variables variables))
		 (t
		  (parse-generic-clause clause-description
					:database database :variables variables)))))
	 (t
	  ;; clause description is a list of clauses
	  (break)
	  (cons (parse-clause (first clause-description))
		(parse-clause (rest clause-description))))))

;;;--------------------------------------------------------------------------

(defmethod PARSE-CLAUSE ((clause-description NULL) &rest rest)
  nil)

;;;--------------------------------------------------------------------------
;;; PARSE-GENERIC-CLAUSE
;;;--------------------------------------------------------------------------

(defmethod PARSE-GENERIC-CLAUSE ((clause-description LIST)
				 &key
				 (class 'CLAUSE-ENTITY)
				 (variables nil)
				 (database *database*))
  (let* ((predicate-name  (first clause-description))
    	 (predicate (ensure-predicate predicate-name :database database))
	 (args nil)
	 (clause nil)
	 new-arg)

    ;; Process the arguments
    (dolist (arg (rest clause-description))
      (cond ((typep arg 'clause-entity)
	     (push (parse-clause (clause-description arg)) args)
	     #+IGNORE
	     (push arg args))
	    ((atom arg)
	     (or (find arg args :test #'eq :key #'entity-name)
		 (progn
		   (setf new-arg
		     (parse-atomic-argument arg :database database :variables variables))
		   (when (variable-p new-arg)
		     (pushnew new-arg variables))
		   (push new-arg args))))
	    (t
	     (multiple-value-setq (new-arg variables)
	       (parse-clause arg :database database :variables variables))
	     (push new-arg args))))

    ;; Restore the order of arguments
    (setf args (nreverse args))
    
    (setf clause (make-clause predicate args
			 :class (or class (compute-clause-class clause-description))
			 :description clause-description
			 :variables variables))
    (setf (object-name clause) (util::make-description-string clause-description))
    
    ;; Create the clause object & add to clause database
    (values clause variables)))
  
;;;--------------------------------------------------------------------------
;;; PARSE-HOLDS-CLAUSE
;;;--------------------------------------------------------------------------

(defun PARSE-HOLDS-CLAUSE (clause-description &key (class 'HOLDS-CLAUSE-ENTITY)
						   (database *database*) 
						   (variables nil))
  (let* ((predicate-name  (first clause-description))
    	 (predicate (ensure-predicate predicate-name :database database))
	 (clause-part (second clause-description))
	 (time-of-clause (third clause-description))
	 (constraint (fourth clause-description)))

    (when clause-part
      (multiple-value-setq (clause-part variables)
	(parse-clause clause-part :variables variables :database database)))
    
    (when time-of-clause 
      (multiple-value-setq (time-of-clause variables)
	(parse-atomic-argument time-of-clause 
			       :database database :variables variables)))
    
    (when constraint
      (multiple-value-setq (constraint variables)
	(parse-clause constraint :database database :variables variables)))
    
    ;; Create the clause object & add to clause database
    (values (make-holds-clause predicate clause-part time-of-clause constraint
			       :variables variables
			       :description clause-description
			       :class class)
	    variables)))

;;;--------------------------------------------------------------------------

(defun PARSE-HORN-CLAUSE (clause-description &key variables (database *database*))
  (let* ((lhs-description (second clause-description))
	 (rhs-description (nthcdr 2 clause-description))
	 new-clause lhs rhs)

    (multiple-value-setq (lhs variables)
      (parse-clause lhs-description :database database :variables variables))
    
    (dolist (x rhs-description)
      (multiple-value-setq (new-clause variables)
	(parse-clause x :database database :variables variables))
      (push new-clause rhs))
    
    (setf rhs (make-conjunctive-clause (nreverse rhs)
				       :description (cons 'AND rhs-description)))
    
    ;; Create the clause object & add to clause database
    (values (make-horn-clause lhs rhs
			      :description clause-description
			      :variables variables)
	    variables)))

  
;;;--------------------------------------------------------------------------

;;; Parse the temporal clause description and return as multiple values
;;; the component tokens.

(defun PARSE-TEMPORAL-CLAUSE (clause-description &key (variables nil)
						      (database *database*))
  (let* ((arguments (rest clause-description))
	 (temporal-relations (mapcar #'find-temporal-relation (second arguments)))

	 (interval-1 nil)
	 ;; Parse The Temporal Relations
	 (interval-2 nil)
	 new-clause)
    (multiple-value-setq (interval-1 variables)
      (parse-atomic-argument (first arguments)
			     :class 'TEMPORAL-INTERVAL
			     :database database
			     :variables variables))
    (multiple-value-setq (interval-2 variables)
      (parse-atomic-argument (third arguments)
			     :class 'TEMPORAL-INTERVAL
			     :database database
			     :variables variables))
  
    (setf new-clause 
      (make-temporal-clause interval-1 temporal-relations interval-2
			    :description clause-description))

    ;; Return the new clause and updated variables
    (values new-clause variables)))

;;;--------------------------------------------------------------------------
;;; PARSE-ATOMIC-ENTITY
;;;--------------------------------------------------------------------------

;;; This creates either a constant or variable entity. Variable names start
;;; wth a "?".

(defun PARSE-ATOMIC-ARGUMENT (argument &key (class 'CONSTANT-ENTITY)
					    (variables nil)
					    (database *database*))
  (cond ((variable-p argument)
	 (let ((variable (find-variable argument variables)))
	   (cond (variable
		  (values variable variables))
		 (t
		  (setf variable (make-variable argument))
		  (values variable (cons variable variables))))))
	(t
	 (values (ensure-constant argument :class class :database database)
		 variables))))

;;;--------------------------------------------------------------------------

(defun COMPUTE-CLAUSE-CLASS (clause-description)
  (let ((predicate-name  (first clause-description)))
    (cond ((eq predicate-name 'AND)
	   'conjunctive-clause-entity)
	  ((eq predicate-name 'OR)
	   'disjunctive-clause-entity)
	  ((eq predicate-name 'NOT)
	   'negated-clause-entity)
	  ((eq predicate-name 'TEMPORAL)
	   'temporal-clause-entity)
	  (t
	   'simple-clause-entity))))


;;;--------------------------------------------------------------------------
;;; same-entity-p
;;;--------------------------------------------------------------------------

(defmethod SAME-ENTITY-P ((entity-1 ATOMIC-ENTITY)(entity-2 ATOMIC-ENTITY))
  (eq entity-1 entity-2))

;;;--------------------------------------------------------------------------
;;; ATOMIC-ENTITY-P
;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity ATOMIC-ENTITY))
  t)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity COMPOSITE-ENTITY))
  nil)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity STRING))
  t)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity SYMBOL))
  t)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity INTEGER))
  t)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity LIST))
  nil)

;;;--------------------------------------------------------------------------

(defmethod ATOMIC-ENTITY-P ((entity t))
  nil)

;;;--------------------------------------------------------------------------
;;; CLAUSE PRODUCT
;;;--------------------------------------------------------------------------

(defun COMPUTE-CLAUSE-PRODUCT (temporal-clause)
  (setf (temporal-clause-product temporal-clause) 
    (compute-relations-product (temporal-clause-relations temporal-clause))))

;;;--------------------------------------------------------------------------
;;; RELATIONS PRODUCT
;;;--------------------------------------------------------------------------

(defun COMPUTE-RELATIONS-PRODUCT (relations)
  (let ((product 1))
    (dolist (relation relations)
      (setf product (* product (temporal-relation-prime-index relation))))
    product))

;;;--------------------------------------------------------------------------
;;; FIND-VARIABLES
;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLES ((clause CLAUSE-ENTITY) &key (variables))
  (setf variables (find-variables (clause-predicate clause) :variables variables))
  (find-variables (clause-arguments clause) :variables variables))

;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLES ((clauses LIST) &key (variables))
  (dolist (clause clauses)
    (setf variables (find-variables clause :variables variables)))
  variables)


;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLES ((variable VARIABLE-ENTITY) &key (variables))
  (if (find variable variables :test #'eq)
      variables
    (cons variable variables)))

;;;--------------------------------------------------------------------------

(defmethod FIND-VARIABLES ((entity t) &key (variables))
   variables)

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------







