(in-package :PROVER)

;;;----------------------------------------------------------------------------
;;; The Temporal Reasoner
;;;----------------------------------------------------------------------------
;;;
;;; The following is an implemtation of a temporal system as described in
;;; [Allen&Koomen83b], i.e. "Maintaining Knowledge About Temporal Intervals".
;;; This implementation assumes knowledge of the work described there. In
;;; particular we have implemented both algorithms described in the paper
;;; as well as the transitivity table for pairs of temporal relations.
;;;
;;; This implements a TEMPORAL REASONER. We refer to any one of the following
;;; as a tempral relation: <, >, d, di, o, oi, m, mi, s, si, f, fi, =.
;;; These are all the possible primitive temporal relations between two
;;; temporal intervals. We define a temporal clause to be a list of the form:
;;;
;;; (<interval1> <list-of-temporal-relations> <interval2>)
;;;
;;;----------------------------------------------------------------------------
;;; 
;;; MAKE-TEMPORAL-DATABASE
;;; INCREMENT-TEMPORAL-DATABASE
;;;
;;; TEMPORAL-CLAUSE-INTERVAL-1
;;; TEMPORAL-CLAUSE-INTERVAL-2
;;; TEMPORAL-CLAUSE-INTERVAL-RELATIONS (+ setf)
;;; TEMPORAL-CLAUSE-COMPONENTS
;;;
;;; INVERSE-TEMPORAL-RELATIONS
;;; MAKE-SYMETRIC-TEMPORAL-CLAUSE
;;;
;;; EQUIVALENT-CLAUSE-EXISTS-P (<temporal-clause> &key <database>)
;;; RELATIONS-EQUIVALENT-P (relations-1 relations-2) 
;;; RELATIONS-EQUIVALENT-IN-CLAUSE-P (relations temporal-clause)
;;; RELATIONS-CONSTRAIN-RELATIONS-P (relations-1 relations-2) 
;;; RELATIONS-CONSTRAIN-CLAUSE-P (relations temporal-clause)
;;;
;;; CLAUSE-CONSTRAINS-P (<temporal-clause> &key <database>)
;;; CLAUSE-CONFLICTS-P  (<temporal-clause> &key <database>)
;;;
;;; PROPAGATE-TEMPORAL-CLAUSE (<temporal-clause>)
;;; ADD-CLAUSE (<temporal-clause> &key <database>)
;;;
;;; CONSTRAINTS  (<r1> <r2>)
;;; TRANSITIVITY-RELATION (<r1> <r2>)
;;;
;;; MATCH-TEMPORAL-CLAUSE (<temporal-clause> &key <database>)
;;; FIND-TEMPORAL-MATCH  (<temporal-clause> <TEMPORAL-CLAUSES>)
;;; FIND-TEMPORAL-MATCH1  (<temporal-clause> <TEMPORAL-CLAUSES>)
;;;
;;;----------------------------------------------------------------------------

;;;--------------------------------------------------------------------------
;;; MAKE-TEMPORAL-CLAUSE
;;;--------------------------------------------------------------------------

(defun MAKE-TEMPORAL-CLAUSE (interval-1 temporal-relations interval-2 &key description)
  ;; Create the temporal clause
  (make-clause 'TEMPORAL-CLAUSE-ENTITY
	       #+IGNORE
	       (find-predicate 'TEMPORAL)
	       `(,interval-1 ,temporal-relations ,interval-2)
	       :description description))

;;;------------------------------------------------------------------------

;;; Defined for convenience as a higher-level interface to make-temporal-clause.

(defmethod MAKE-TEMPORAL-CONSTRAINT ((I1 atomic-entity)
				     relations
				     (I2 atomic-entity)
				     &key (database *database*))
  (parse-clause `(TEMPORAL ,(entity-name I1) ,relations ,(entity-name I2))
		:database database))

;;;----------------------------------------------------------------------------

(defmethod MAKE-TEMPORAL-CONSTRAINT ((I1 symbol) relations (I2 symbol)
				     &key (database *database*))
  (make-temporal-constraint (find-constant I1 :database database)
			    relations
			    (find-constant I2 :database database)
	       	    :database database))

;;;----------------------------------------------------------------------------

(defmethod MAKE-TEMPORAL-CONSTRAINT ((I1 symbol) relations (I2 atomic-entity) &key (database *database*))
  (make-temporal-constraint (find-constant I1 :database database) relations I2
			    :database database))
			    
;;;----------------------------------------------------------------------------

(defmethod MAKE-TEMPORAL-CONSTRAINT ((I1 atomic-entity) relations (I2 symbol) &key (database *database*))
  (make-temporal-constraint I1 relations (find-constant I2 :database database)
			    :database database))
  
;;;----------------------------------------------------------------------------
;;; ADD-CLAUSE
;;;----------------------------------------------------------------------------

;;; This method ensures that the clause is not already in the database.

(defmethod ADD-CLAUSE :around ((clause TEMPORAL-CLAUSE-ENTITY) &key (database *database*))
  (declare (ignore database))
  #+IGNORE
  (let* ((interval-1 (temporal-clause-interval-1 clause))
	 (interval-2 (temporal-clause-interval-2 clause)))
    (unless (find-temporal-clause interval-1 interval-2 :database database)
      (call-next-method)))
  (call-next-method))

;;;----------------------------------------------------------------------------

;;; This :after method adds the symmetric of clause to the database.

(defmethod ADD-CLAUSE :after ((clause TEMPORAL-CLAUSE-ENTITY) &key (database *database*))
  (let* ((interval-1 (temporal-clause-interval-1 clause))
	 (relations (temporal-clause-relations clause))
	 (interval-2 (temporal-clause-interval-2 clause))
	 (complement (find-temporal-clause interval-2 interval-1 :database database)))
    
    ;; Add the complement temporal clause
    (unless complement
      (setf complement
	(make-temporal-clause interval-2
			      (inverse-temporal-relations relations)
			      interval-1))
      ;; Add clause to the database
      (add-clause complement :database database)
      ;; Setup bi-directional link
      (setf (temporal-clause-complement clause) complement)
      (setf (temporal-clause-complement complement) clause))
    
    ;; Return the original clause
    clause))

;;;----------------------------------------------------------------------------
;;; DELETE-CLAUSE
;;;----------------------------------------------------------------------------

;;; This function deletes the symmetric of 'clause' from database.

(defmethod DELETE-CLAUSE :after ((clause TEMPORAL-CLAUSE-ENTITY) &key (database *database*))
  (let ((symmetric-clause (find-temporal-clause (temporal-clause-interval-2 clause)
						(temporal-clause-interval-1 clause)
						:database database)))
    ;; Delete the symmetric equivalent.
    (when symmetric-clause
      (delete-clause symmetric-clause :database database))))

;;;--------------------------------------------------------------------------

(defmacro DEFINE-TEMPORAL-INTERVAL (interval-name &key (database *database*))
  `(make-constant ,interval-name :class 'temporal-interval :database ,database))


;;;--------------------------------------------------------------------------
;;; FIND-TEMPORAL-CLAUSE
;;;--------------------------------------------------------------------------


(defmethod FIND-TEMPORAL-CLAUSE ((interval-1 atomic-entity)(interval-2 atomic-entity)
				 &key (database *database*))
  (let ((clauses (find-clauses-for-predicate 'TEMPORAL :database database)))
    (dolist (clause clauses)
      (when (and (eq (temporal-clause-interval-1 clause) interval-1)
		 (eq (temporal-clause-interval-2 clause) interval-2))
	(return clause)))))

;;;--------------------------------------------------------------------------

(defmethod FIND-TEMPORAL-CLAUSE ((interval-1 symbol)(interval-2 symbol)
				 &key (database *database*))
  (let ((i1 (find-constant interval-1 :database database))
	(i2 (find-constant interval-2 :database database)))
    (when (and i1 i2)
      (find-temporal-clause i1 i2 :database database))))

;;;--------------------------------------------------------------------------

(defmethod FIND-TEMPORAL-CLAUSE ((interval-1 variable-entity)(interval-2 temporal-interval)
				 &key (database *database*))
  (let* ((possible-matches
	  (find-temporal-clauses interval-2 :database database))
	 (matching-clause nil)
	 unifies-p bindings )
    (dolist (clause possible-matches)
      (multiple-value-setq (unifies-p bindings)
	(unify interval-1 (temporal-clause-interval-2 clause)))
      (when unifies-p
	(setf matching-clause clause)
	(return t)))
    (when unifies-p
      (values matching-clause bindings))))


;;;--------------------------------------------------------------------------

(defmethod FIND-TEMPORAL-CLAUSE ((interval-1 temporal-interval)(interval-2 variable-entity)
				 &key (database *database*))
  (find-temporal-clause interval-2 interval-1 :database database))

;;;--------------------------------------------------------------------------
;;; ALL-TEMPORAL-CLAUSES
;;;--------------------------------------------------------------------------

(defmethod ALL-TEMPORAL-CLAUSES (&key (database *database*))
  (find-clauses-for-predicate 'TEMPORAL :database database))

;;;--------------------------------------------------------------------------

(defmethod FIND-TEMPORAL-CLAUSES ((interval atomic-entity) &key (database *database*))
  (let ((clauses (find-clauses-for-predicate 'TEMPORAL :database database))
	(temporal-clauses nil))
    (dolist (clause clauses)
      (when (eq (temporal-clause-interval-1 clause) interval)
	(push clause temporal-clauses)))
    temporal-clauses))

;;;--------------------------------------------------------------------------
;;; TEMPORAL-CLAUSE-P
;;;--------------------------------------------------------------------------

(defun TEMPORAL-CLAUSE-P (clause)
  (typep clause 'temporal-clause-entity))

;;;--------------------------------------------------------------------------
;;; Reset-Temporal-Clauses
;;;--------------------------------------------------------------------------

(defun RESET-TEMPORAL-CLAUSES (&key (database *database*))
  (setf (gethash (find-predicate 'TEMPORAL) (database-clauses database)) nil)
  (clrhash database)
  t)

;;;--------------------------------------------------------------------------

(defun PRINT-TEMPORAL-CLAUSES (&key (stream t)(database *database*))
  (dolist (clause (gethash (find-predicate 'TEMPORAL) (database-clauses database)))
    (format stream "~%~a" clause)))	

;;;----------------------------------------------------------------------------
;;; Temporal Clause Intervals
;;;----------------------------------------------------------------------------

;;; This returns the first interval in a tempral clause.

(defmethod TEMPORAL-CLAUSE-INTERVAL-1 ((temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (first (clause-entity-arguments temporal-clause)))

;;;----------------------------------------------------------------------------

;;; This returns the second interval in a temporal clause.

(defmethod TEMPORAL-CLAUSE-INTERVAL-2 ((temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (third (clause-entity-arguments temporal-clause)))

;;;----------------------------------------------------------------------------
;;; Temporal Clause Relations
;;;----------------------------------------------------------------------------

;;; This returns the list of relations in a temporal clause.

(defmethod TEMPORAL-CLAUSE-RELATIONS ((temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (second (clause-entity-arguments temporal-clause)))

;;;----------------------------------------------------------------------------

;;; This sets the list of relations in a temporal clause.

(defmethod (setf TEMPORAL-CLAUSE-RELATIONS) (nv (temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (setf (second (clause-entity-arguments temporal-clause)) nv))

;;;----------------------------------------------------------------------------

;;; This updates the clause product.

(defmethod (setf TEMPORAL-CLAUSE-RELATIONS) :after (nv (temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (compute-clause-product temporal-clause)
  nv)

;;;----------------------------------------------------------------------------
;;; Temporal clause components
;;;----------------------------------------------------------------------------

;; Extract the components of temporal-clause.

(defmethod TEMPORAL-CLAUSE-COMPONENTS ((temporal-clause TEMPORAL-CLAUSE-ENTITY))
  (values (temporal-clause-interval-1 temporal-clause)
	  (temporal-clause-relations temporal-clause)
	  (temporal-clause-interval-2 temporal-clause)))

;;;----------------------------------------------------------------------------
;;; TEMPRAL RELATIONS
;;;----------------------------------------------------------------------------

(defun FIND-TEMPORAL-RELATION (name)
  (find name *temporal-relations* :test #'eq :key #'entity-name))

;;;----------------------------------------------------------------------------
;;; Inverse Temporal Relations
;;;----------------------------------------------------------------------------

;;; This returns the a list of the inverse relations of 'list-of-relations'.

(defun INVERSE-TEMPORAL-RELATIONS (list-of-relations)
  (mapcar #'inverse-temporal-relation list-of-relations))

;;;----------------------------------------------------------------------------

;;; This returns the symmetric equivalent of a temporal clause. I.e. if we
;;; had that I1 meets I2, then this would return I2 is met by I1.

(defun MAKE-SYMETRIC-TEMPORAL-CLAUSE (temp-clause)
  (make-temporal-clause 
   (temporal-clause-interval-2 temp-clause)
   (inverse-temporal-relations
    (temporal-clause-relations temp-clause))
   (temporal-clause-interval-1 temp-clause)))

;;;----------------------------------------------------------------------------
;;; TEMPORAL PREDICATES
;;;----------------------------------------------------------------------------

(defmethod EQUIVALENT-CLAUSE-EXISTS-P ((temporal-clause TEMPORAL-CLAUSE-ENTITY)
				       &key (database *database*))
  (let* ((i1 (temporal-clause-interval-1 temporal-clause))
	 (i2 (temporal-clause-interval-2 temporal-clause))
	 (existing-clause (find-temporal-clause i1 i2 :database database)))
    (and existing-clause
	 (= (temporal-clause-product temporal-clause)
	    (temporal-clause-product existing-clause)))))

;;;----------------------------------------------------------------------------

(defun RELATIONS-EQUIVALENT-P (relations-1 relations-2) 
  (= (compute-relations-product relations-1)
     (compute-relations-product relations-2)))

;;;----------------------------------------------------------------------------

(defun RELATIONS-EQUIVALENT-IN-CLAUSE-P (relations temporal-clause)
  (= (compute-relations-product relations)
     (temporal-clause-product temporal-clause)))

;;;----------------------------------------------------------------------------

(defun RELATIONS-CONSTRAIN-RELATIONS-P (relations-1 relations-2) 
  (divides-p (compute-relations-product relations-1)
	     (compute-relations-product relations-2)))

;;;----------------------------------------------------------------------------

(defun RELATIONS-CONSTRAIN-CLAUSE-P (relations temporal-clause)
  (divides-p (compute-relations-product relations)
	     (temporal-clause-product temporal-clause)))

;;;----------------------------------------------------------------------------

#+IGNORE
(defmethod CLAUSES-EQUIVALENT-P ((temporal-clause-1 temporal-clause-entity)
				 (temporal-clause-2 temporal-clause-entity))
  (= (temporal-clause-product temporal-clause-1)
     (temporal-clause-product temporal-clause-2)))

;;;----------------------------------------------------------------------------

#+IGNORE
(defmethod CLAUSES-CONSTRAINED-P ((temporal-clause-1 temporal-clause-entity)
				  (temporal-clause-2 temporal-clause-entity))
  (divides-p (temporal-clause-product temporal-clause-1)
	     (temporal-clause-product temporal-clause-2)))

;;;----------------------------------------------------------------------------

#+IGNORE
(defmethod CLAUSES-CONFLICT-P ((temporal-clause-1 temporal-clause-entity)
			       (temporal-clause-2 temporal-clause-entity))
  (unless (or (clauses-equivalent-p temporal-clause-1 temporal-clause-2)
	      (clauses-constrained-p temporal-clause-1 temporal-clause-2)
	      (clauses-constrained-p temporal-clause-2 temporal-clause-1))))
  
;;;----------------------------------------------------------------------------

;;; Badly named: Returns t if temporal-clause further constrains the
;;; database without conflicting.

(defmethod CLAUSE-CONSTRAINS-P ((temporal-clause temporal-clause-entity)
						 &key (database *database*))
  (let* ((i1 (temporal-clause-interval-1 temporal-clause))
	 (i2 (temporal-clause-interval-2 temporal-clause))
	 (existing-clause (find-temporal-clause i1 i2 :database database)))
    (cond ((not existing-clause)
	   t)
	  (t
	   (or (divides-p (temporal-clause-product temporal-clause)
			  (temporal-clause-product existing-clause))
	       (divides-p (temporal-clause-product existing-clause)
			  (temporal-clause-product temporal-clause)))))))
  
;;;----------------------------------------------------------------------------

(defmethod CLAUSE-CONFLICTS-P  ((temporal-clause temporal-clause-entity)
				&key (database *database*))
  (let* ((i1 (temporal-clause-interval-1 temporal-clause))
	 (i2 (temporal-clause-interval-2 temporal-clause))
	 (existing-clause (find-temporal-clause i1 i2 :database database)))
    (when existing-clause
      (unless (or (equivalent-clause-exists-p temporal-clause :database database)
		  (divides-p (temporal-clause-product temporal-clause)
			     (temporal-clause-product existing-clause))
		  (divides-p (temporal-clause-product existing-clause)
			     (temporal-clause-product temporal-clause)))))))

;;;----------------------------------------------------------------------------
;;; TEMPORAL DATABASE OPERATIONS
;;;----------------------------------------------------------------------------

(defun MAKE-TEMPORAL-DATABASE (temporal-clauses-description &key (database *database*))
  (reset-temporal-clauses)
  (mapc #'(lambda (temporal-clause)
	    (add-clause temporal-clause :database database))
          temporal-clauses-description))

;;;----------------------------------------------------------------------------

(defun INCREMENT-TEMPORAL-DATABASE (temporal-clause-description &key (database *database*))
  (add-clause temporal-clause-description :database database))

;;;----------------------------------------------------------------------------
;;; TEMPORAL CLAUSE PROPAGATION
;;;----------------------------------------------------------------------------

;;; This function adds (with propagation) 'temp-clause' to *TEMPORAL-RELATIONS*.
;;; First it calls 'add-temp-rel' with 'temp-clause'. Then it tries to
;;; deduce any new relations via the knowledge about the transivity of
;;; the temporal relations. I.e. if I1 is before I2,  and I2 is before I3,
;;; it would deduce that I1 is before I3 and add it to *TEMPORAL-RELATIONS*.

(defmethod ADD-CLAUSE ((temporal-clause TEMPORAL-CLAUSE-ENTITY) &key (database *database*))
    (let ((conflict-p nil))
      ;; Determine if there is a conflict & propagate
      (unless conflict-p
	(call-next-method))
      ;; Actualy add the clause to the database unless it conflicts.
      (setf conflict-p 
	(propagate-temporal-clause temporal-clause :database database))
      ;; Return the clause and conflict info.
      (values temporal-clause conflict-p)))

;;;----------------------------------------------------------------------------

;;; For temporal-clause-1 of the form (TEMPORAL i <i-j-relations> j), this method
;;; examines clauses of the form (TEMPORAL j <j-k-relations> k) for all k, in order
;;; to infer clauses of the form (TEMPORAL i <i-k-relations> k). 
;;;
;;; NOTE: This function may further constrain an existing relation between i & k.

(defmethod PROPAGATE-TEMPORAL-CLAUSE ((temporal-clause-1 TEMPORAL-CLAUSE-ENTITY)
				      &key (database *database*))
  ;; Extract the components of temporal-clause-1
  (multiple-value-bind (i relations-1 j)
      (temporal-clause-components temporal-clause-1)
    (let (;; Retrieve temporal clauses involving j.
	  (clauses (find-temporal-clauses j :database database))
	  ;; local vars
	  relations-2 temporal-clause-3 relations-3 k)

      ;; Iterate over the clauses involving j.
      (dolist (temporal-clause-2 clauses) 
	(setf k  (temporal-clause-interval-2 temporal-clause-2))
	
	;; Only continue if there is a clause and k is not the interval i.
	(when (and temporal-clause-2 (not (same-entity-p i k)))
	  (setf relations-2
	    ;; Look up constraints in the temporal relations transitivty table
	    (constraints relations-1 (temporal-clause-relations temporal-clause-2)))
	  ;;(format t "~%i = ~a, relations-2 = ~a, k = ~a" i relations-2 k)
	  (setf temporal-clause-3 (find-temporal-clause i k :database database))

	  ;; If there is an existing clause, constrain it if necessary. If the
	  ;; intersection is nil, then there must be a CONFLICT!
	  (when temporal-clause-3
	    (setf relations-3 (temporal-clause-relations temporal-clause-3))
	    (setf relations-2 (intersection relations-2 relations-3)))
	  
	  (cond ((null relations-2)
		 ;; Adding the clauses must create a conflict
		 nil)
		((and temporal-clause-3
		      (relations-equivalent-in-clause-p relations-2 temporal-clause-3))
		 ;; The temporal clause already exists
		 nil)
		(t
		 ;; The clause does not exist yet
		 (unless (= (length relations-2) 13)
		   (setf temporal-clause-3 (make-temporal-clause i relations-2 k))
		   (format t "~%Inferring temporal clause ~a~%" temporal-clause-3)
		   (add-clause temporal-clause-3)
		   (propagate-temporal-clause temporal-clause-3)))))))))
	 
;;;----------------------------------------------------------------------------   
;;; CONSTRAINTS
;;;----------------------------------------------------------------------------   

;;; This function computes the transitivity function for lists of relations.
;;; I.e. r1 & r2 are lists of relations.
;;; Note: This is an n-squared algorithm!

;;; First we iterate over the relations in r1:

(defun CONSTRAINTS (r1 r2)
  (cond ((null r1)
	 nil)
        (t
	 (let ((result nil))
	   (dolist (r r1)
	     (setf result (union result (constraints1 r r2))))
	   result))))

;;;----------------------------------------------------------------------------

;;; Here r1 is a single relation and r2 is still a list of relations:

(defun constraints1 (r1 r2)
  (cond ((null r2)
	 nil)
        (t
	 (let (result)
	   (dolist (r r2)
	     (setf result 
	       (union result (transitivity-relation r1 r))))
	   result))))

;;;----------------------------------------------------------------------------
;;; TEMPORAL MATCHING & UNIFICATION
;;;----------------------------------------------------------------------------

;;; This returns the temporal-relations (with bindings) that match 'temp-clauses'
;;; Note that matching here is a little trickier because we have to test a
;;; temporal relation or the symmetric relation. e.g. Furthermore the match may
;;; be a subset of the (rels temp-clause) since if for instance we want to prove
;;; that (I1 (< >) ?x), and we have in the database that (I1 (<) I2) we should
;;; succeed with the match (I1 (<) I2), with ?x bound to I2.
;;;
;;; The temporal relation (?x (m o) ?y) actually matches either
;;;
;;; (I1 (o) I2)  or its symmetric twin (l2 (oi) I1)
;;;
;;; The work of testing for either is done by the function 'find-temporal-match'.

(defmethod FIND-MATCHES ((temporal-clause TEMPORAL-CLAUSE-ENTITY)
			 &key (database *database*) (bindings nil))
  (multiple-value-bind (i1 relations i2)
      (temporal-clause-components temporal-clause)
    (declare (ignore relations))
    (let ((relevant-clauses nil))
      (cond ((and (variable-p i1)
		  (variable-p i2))
	     (setf relevant-clauses (all-temporal-clauses)))
	    ((variable-p i1)
	     (setf relevant-clauses
	       (find-temporal-clauses i1 :database database)))
	    ((variable-p i2)
	     (setf relevant-clauses
	       (find-temporal-clauses i2 :database database)))
	    (t nil))
      (when relevant-clauses
	(find-temporal-matches temporal-clause 
			       :clauses relevant-clauses
			       :bindings bindings)))))

;;;----------------------------------------------------------------------------

(defmethod FIND-TEMPORAL-MATCHES (temporal-clause-1 &key (clauses nil)(bindings nil))
  (declare (ignore bindings))
  (let ((new-bindings nil)
	(result nil))
    (dolist (temporal-clause-2 clauses)
      (multiple-value-setq (result new-bindings)
	(unify temporal-clause-1 temporal-clause-2))
      (when result
	;; Old code instantiated clause-2
	(push `(,temporal-clause-2 ,new-bindings) result)))
    result))

;;;----------------------------------------------------------------------------

;;; This returns nil, t or bindings, depending on whether temporal-clauses-1
;;; 'unifies' (note this is not strict unification, since the list of relations
;;; is not ordered) with temporal-clause-2

#+IGNORE
(defun FIND-TEMPORAL-MATCH (temporal-clauses-1 temporal-clause-2)
  (let ((i1 (temporal-clause-interval-1 temporal-clauses-1))
        (j1 (temporal-clause-interval-2 temporal-clauses-1))
        (r1 (temporal-clause-relations temporal-clauses-1))
        (i2 (temporal-clause-interval-1 temporal-clause-2))
        (j2 (temporal-clause-interval-2 temporal-clause-2))
        (r2 (temporal-clause-relations temporal-clause-2))
        b1 b2)
    (cond ((relations-equivalent-p temporal-clauses-1 temporal-clause-2)
           '(nil nil))
          ((and (setf b1 (unify i1 i2))
                (setf b2 (unify (instantiate j1 b1)(instantiate j2 b1)))
                (subsetp r2 r1))
           (append b1 b2))
          ((and (setf b1 (unify i1 j2))
		(instantiate-bindings b1)
                (setf b2 (unify (instantiate j1 b1)(instantiate i2 b1)))
                (subsetp (inverse-temporal-relations r2) r1))
           (append b1 b2))
            (t nil))))


;;;--------------------------------------------------------------------------
;;; The Transitivity Table
;;;--------------------------------------------------------------------------

(defun INITIALIZE-TRANSITIVITY-TABLE ()
  (dotimes (i 13)
    (dotimes (j 13)
      (let ((entry (aref *TEMPORAL-RELATIONS-TRANSITIVITY-TABLE* i j)))
	(when (symbolp (car entry))
	  (setf (aref *TEMPORAL-RELATIONS-TRANSITIVITY-TABLE* i j)
	    (mapcar #'find-temporal-relation (aref *TEMPORAL-RELATIONS-TRANSITIVITY-TABLE* i j))))))))


;;;----------------------------------------------------------------------------
;;; TRANSITITY-RELATION
;;;----------------------------------------------------------------------------

;;; This returns a list of relations that is the entry in the transitivity
;;; table for the relations r1 and r2. I.e. let I1, I2 and I3 be intervals
;;; such that (I1 r1 I2) and (I2 r2 I3). Then this function returns the
;;; relation between I1 and I3. NOTE : here r1 and r2 are single relations.
;;; The functions 'constraints' returns the transitive relation between lists
;;; of  relations.

(defun TRANSITIVITY-RELATION (r1 r2)
  (aref *TEMPORAL-RELATIONS-TRANSITIVITY-TABLE*
	(temporal-relation-index r1)
	(temporal-relation-index r2)))

;;;------------------------------------------------------------------------
;;;  TEMP-SK-CL 
;;;------------------------------------------------------------------------

;;; This instantiates the time-of clause with a skolem constant.

(defun TEMP-SK-CL (clause)
  (let ((bindings
	 `(,(make-binding (time-of (caar clause))
		       (make-skolem-constant (time-of (caar clause)))))))
    (instantiate-clause clause bindings)))

;;;----------------------------------------------------------------------------
;;; End of File
;;; ;;;----------------------------------------------------------------------------
