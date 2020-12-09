(in-package :PROVER)

;;;--------------------------------------------------------------------------
;;; The Theorem Prover
;;;--------------------------------------------------------------------------
;;;
;;; PROVE-CLAUSE
;;;
;;; PROVE
;;; PROVE-OR
;;;
;;; MATCH-HORN-CLAUSES
;;; FIND-MATCHES
;;; FIND-MATCH
;;;
;;; PRINT-PROOF-TREE
;;; CLEAR-PROOF-TREE
;;;
;;;--------------------------------------------------------------------------

(defvar *GENERATE-PROOF-TREE* nil)

(defvar *PROOF-TREE* (make-and-node 'ROOT 'ROOT))
(defvar *CURRENT-NODE* *proof-tree*)

;;;--------------------------------------------------------------------------
;;; PROVE-CLAUSE
;;;--------------------------------------------------------------------------

;;; This is the entry-point into the deductive reasoner.

(defmethod PROVE-CLAUSE ((clause CLAUSE-ENTITY) &key (database *DATABASE*)
						     (bindings nil))
  (clear-proof-tree)
  (prove clause :database database :bindings bindings))

;;;--------------------------------------------------------------------------

;;; This allows the generic function to be invoked with a clause description.

(defmethod PROVE-CLAUSE ((clause LIST) &rest rest)
  (apply #'prove-clause (parse-clause clause) rest))

;;;--------------------------------------------------------------------------
;;; PROVE
;;;--------------------------------------------------------------------------

;;; This function processes a single node. It tests to see whether this node
;;; is a leaf node (i.e. a fact), an OR-node or an AND-node and responds
;;; accordingly. It returns the latest bindings. 

(defmethod PROVE ((clause CLAUSE-ENTITY) &key (database *DATABASE*)
					      (bindings nil))
  (let ((fact-p nil)
	(new-bindings nil)
	(matches nil))
    
    ;; Try to find a matching fact
    (multiple-value-setq (fact-p new-bindings)
      (find-match clause :database database :bindings bindings  ))  
    ;; Is <clause> a fact?
    (cond (fact-p
	   ;; Add any new bindings
	   (when new-bindings
	     (setf bindings new-bindings))
	   (values clause bindings))
	  (t
	   ;; Match is of the form (<horn-clause> <bindings>)
	   (setf  matches (match-horn-clause clause :database database :bindings bindings))
	   (prove matches :bindings bindings :database database)))))

;;;--------------------------------------------------------------------------
;;; Proving Conjunctions
;;;--------------------------------------------------------------------------

;;; This function succeeds only if all the conjuncts are provable. Note that
;;; this function propagates binding information to successive sibblings.

(defmethod PROVE ((clause CONJUNCTIVE-CLAUSE-ENTITY) &key (database *DATABASE*) bindings)
  (let (result)
    (dolist (child (clause-entity-arguments clause))
      ;; Prove each child
      (multiple-value-setq (result bindings)
	(prove child :bindings (append (clause-bindings child) bindings) :database database))
      (unless result
	(return nil)))
    ;; Return result and the bindings
    ;; (print bindings)
    (values result bindings)))

;;;--------------------------------------------------------------------------
;;; Proving Disjunctions
;;;--------------------------------------------------------------------------

;;; This returns the first clause in the disjunction that succeeds along with
;;; the accumulated bindings.

(defmethod PROVE ((clause DISJUNCTIVE-CLAUSE-ENTITY)
		  &key
		  (database *DATABASE*)
		  (bindings nil))
  (let* ((matches (clause-entity-arguments clause))
	 result)
    ;; Match is of the form (<horn-clause> <bindings>)
    (dolist (horn-clause matches)
      (let ((new-bindings (clause-bindings horn-clause)))
	#+IGNORE
       (format t "~%~%Proving: ~%  Horn Clause = ~a  ~%  Bindings = ~a~%  New Bindings = ~a~%"  
	       horn-clause bindings new-bindings)
       #+IGNORE
       (format t "~%--------------------------------------------------")
       ;; (break)
       (multiple-value-setq (result bindings)
	 (prove (horn-clause-rhs horn-clause)
		:bindings new-bindings
		:database database))
       (when result
	 (setf result horn-clause)
	 (return t))))
    ;; (print bindings)
    (values result bindings)))

;;;--------------------------------------------------------------------------

#+IGNORE
(defmethod PROVE ((clause DISJUNCTIVE-CLAUSE-ENTITY) &key (database *DATABASE*) bindings)
  (let (result)
    (dolist (child (clause-entity-arguments clause))
      ;; Prove each child
      (multiple-value-setq (result bindings)
	(prove child :bindings bindings :database database))
      (when result
	(return t)))
    ;; Return result and the bindings
    (values result bindings)))


;;;--------------------------------------------------------------------------
;;; Proving Negations
;;;--------------------------------------------------------------------------

;;; This returns the first clause in the disjunction that succeeds along with
;;; the accumulated bindings.

(defmethod PROVE ((clause NEGATED-CLAUSE-ENTITY) &key (database *DATABASE*) bindings)
  (or (call-next-method)
      (let* ((negation (first (clause-arguments clause))))
	(multiple-value-bind (result new-bindings)
	    (prove negation :bindings bindings :database database)
	  (cond (result
		 ;; The negation is true so fail
		 (values nil new-bindings))
		(t
		 (values :undecidable bindings)))))))

;;;--------------------------------------------------------------------------
;;; PROVE Temporal Clause
;;;--------------------------------------------------------------------------

(defmethod PROVE ((clause TEMPORAL-CLAUSE-ENTITY) &key (database *DATABASE*) bindings)
  (let ((matches nil)
	(fact nil))
    (setf fact (find-temporal-clause (temporal-clause-interval-1 clause)
				     (temporal-clause-interval-2 clause)
				     :database database))
    (cond (fact
	   (values fact bindings))
	  (t
	   (setf matches (find-matches clause :database database :bindings bindings))
	   (prove-or matches)))))

;;;--------------------------------------------------------------------------
;;; PROVE List
;;;--------------------------------------------------------------------------

;;; Assume list is a clause-description.

(defmethod PROVE ((clause LIST) &key (database *DATABASE*) bindings)
  (prove (parse-clause clause :database database) :database database :bindings bindings))

;;;--------------------------------------------------------------------------

(defmethod PROVE ((clause NULL) &rest rest)
  (declare (ignore rest))
  nil)

;;;--------------------------------------------------------------------------

;;; This function succeeds if at least one of its children is provable.
;;; Note that no binding information needs to be propagated across the 
;;; children of an or-node. Furthermore, for the time being this function 
;;; terminates as soon as one child has been proved, it does not go on to
;;; see if there are other provable ones as well.

;;; Each child is a list of the form (<clause> <bindings>)

(defun PROVE-OR (children &key (database *database*))
  (let (result bindings)
    (dolist (child children)
      (let ((clause (first child))
	    (bindings (second child)))
	(multiple-value-setq (result bindings)
	  (prove clause :bindings bindings :database database))
	(when result
	  (return t))))
    (values result bindings)))
	
;;;--------------------------------------------------------------------------
;;; MATCH-HORN-CLAUSE
;;;--------------------------------------------------------------------------

;;; This function returns all the RHS's of the relevant clause that unify
;;; with '*database*'. It instantiates these RHS with the bindings returned by
;;; the unifier. In the special case that the RHS is nil, i.e. the
;;; instantiated LHS is a fact, it returns the instantiated LHS. So this
;;; function returns a list each element of the form:
;;;
;;; (<instantiated terms><bindings>)

(defmethod MATCH-HORN-CLAUSE ((clause CLAUSE-ENTITY) 
			      &key 
			      (database *DATABASE*)
			      (bindings nil))
  (let ((relevant-clauses (relevant-horn-clauses clause :database database))
	(matches nil))
    ;; Find the clauses that unify with the possible candidates
    (dolist (relevant-clause relevant-clauses)
      (setf relevant-clause (copy-clause relevant-clause))
      (multiple-value-bind (unifies-p disjunct-bindings)
	  (unify clause (horn-clause-lhs relevant-clause) :bindings bindings)
	(when unifies-p
	  (setf (clause-bindings relevant-clause)disjunct-bindings)
	  (push relevant-clause matches))))
    ;; Return the matches
    (when matches
      (make-disjunctive-clause matches :description (cons 'OR matches)))))
	
;;;--------------------------------------------------------------------------
;;; RELEVANT-HORN-CLAUSES
;;;--------------------------------------------------------------------------

;;; This function iterates through 'clauses' returning a list of those
;;; clauses whose LHS has the same predicate as 'clause'.

(defmethod RELEVANT-HORN-CLAUSES ((clause CLAUSE-ENTITY) &key (database *DATABASE*))
  (let ((horn-clauses (all-horn-clauses :database database))
	(relevant-clauses nil)
	(predicate (clause-predicate clause)))
    (dolist (horn-clause horn-clauses)
      (when (eq predicate (clause-predicate (horn-clause-lhs horn-clause)))
	(push horn-clause relevant-clauses)))
    ;; Return the relevanr clauses
    relevant-clauses))

;;;--------------------------------------------------------------------------
;;; FIND-MATCHES
;;;--------------------------------------------------------------------------

;;; This function returns t if 'clause' is a fact.

(defmethod FIND-MATCHES ((clause-template CLAUSE-ENTITY) &key (database *DATABASE*)(bindings nil))
  (let* ((predicate (clause-entity-predicate clause-template))
	 (clauses (find-clauses-for-predicate predicate :database database))
	 (result nil))
    ;; Find the matching clauses.
    (dolist (clause clauses)
      (multiple-value-bind (unifies-p current-bindings)
	  (unify clause-template clause :bindings bindings)
	(when unifies-p
	  (push `(,clause ,current-bindings) result))))
    result))

;;;--------------------------------------------------------------------------
;;; FIND-MATCH
;;;--------------------------------------------------------------------------

;;; This function returns t if 'clause' is a fact.

(defmethod FIND-MATCH ((clause-template clause-entity) 
		       &key (bindings nil)
			    (database *DATABASE*))
  (let* ((predicate (clause-entity-predicate clause-template))
	 (clauses (find-clauses-for-predicate predicate :database database))
	 (fact-p nil)
	 unifies-p current-bindings)
    ;; Find the matching clauses.
    (dolist (clause clauses)
      (multiple-value-setq (unifies-p current-bindings)
	  (unify clause-template clause :bindings bindings))
      (when unifies-p
	(setf fact-p clause)	  
	(when current-bindings
	  (setf bindings current-bindings))
	(return t)))
    ;; Return bindings
    (values fact-p bindings)))

;;;--------------------------------------------------------------------------
;;; FIND-LHS-MATCH
;;;--------------------------------------------------------------------------

(defmethod FIND-LHS-MATCH ((clause-template clause-entity) &key (database *DATABASE*)(bindings nil))
  (let	((unifies-p nil)
	 (fact-p nil)
	 (current-bindings nil))
    ;; Find the matching clauses.
    (dolist (horn-clause (all-horn-clauses :database database))
      (multiple-value-setq (unifies-p current-bindings)
	  (unify clause-template (horn-clause-lhs horn-clause)))
      (when unifies-p
	(setf fact-p horn-clause)
	(when current-bindings
	  (setf bindings current-bindings))
	(return t)))
    ;; Return bindings
    (values fact-p bindings)))

;;;--------------------------------------------------------------------------
;;; FIND-LHS-MATCHES
;;;--------------------------------------------------------------------------

#+IGNORE
(defmethod FIND-LHS-MATCHES ((clause-template clause-entity) 
			     &key (database *DATABASE*)(bindings bindings))
  (let ((result nil))
    ;; Find the matching clauses.
    (dolist (horn-clause (all-horn-clauses :database database))
      (multiple-value-bind (unifies-p current-bindings)
	  (unify clause-template (horn-clause-lhs horn-clause) :bindings bindings)
	(when unifies-p
	  (push `(,horn-clause ,current-bindings) result))))
    ;; Return bindings
    result))
	   
;;;--------------------------------------------------------------------------
;;; PROOF TREE
;;;--------------------------------------------------------------------------

;;; Most of the proof tree construction is handled by auxilliary methods of
;;; generic function PROVE.

(defmethod PROVE :before ((clause CLAUSE-ENTITY) 
			  &key (database *DATABASE*)
			       bindings
			       (generate-proof-tree *generate-proof-tree*))
  (declare (ignore database bindings))
  (when generate-proof-tree
    (let ((new-node (make-node 'NODE clause)))
      (cond ((typep clause 'CONJUNCTIVE-CLAUSE-ENTITY) 
	     (setf new-node (make-and-node 'AND clause))
	     (make-child new-node *current-node*))
	    ((typep clause  'DISJUNCTIVE-CLAUSE-ENTITY)
	     (setf new-node (make-or-node 'OR clause))
	     (make-child new-node *current-node*))
	    (t     
	     (cond ((or (typep *current-node* 'AND-NODE)
			(typep *current-node* 'OR-NODE))
		    (make-child new-node *current-node*))
		   (t
		    (make-sibbling new-node *current-node*)))))
      (setf *current-node* new-node))))

;;;--------------------------------------------------------------------------

(defmethod PROVE :around ((clause CLAUSE-ENTITY) 
			  &key 
			  (database *DATABASE*)
			  (bindings nil)
			  (generate-proof-tree *generate-proof-tree*))
  ;; Copy-clause does not transfer the bindings.
  (let ((new-clause clause #+IGNORE (copy-clause clause :database database)))
    (multiple-value-bind (result bindings)
	(call-next-method new-clause :database database
			  :bindings bindings
			  :generate-proof-tree generate-proof-tree)
      (when generate-proof-tree
	(when (not result)
	  (setf *current-node* (parent *current-node*))))
      #+IGNORE
      (when bindings
	(instantiate-clause new-clause :bindings bindings))
      (values result bindings))))

;;;--------------------------------------------------------------------------
;;; CLEAR-PROOF-TREE
;;;--------------------------------------------------------------------------

(defun CLEAR-PROOF-TREE ()
  (setf (node-children *proof-tree*) nil)
  (setf *current-node* *proof-tree*))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
