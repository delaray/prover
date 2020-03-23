(in-package :PROVER)

;;;****************************************************************************
;;; PROVER DATABASE OPERATIONS
;;;****************************************************************************


;;;----------------------------------------------------------------------------
;;; INITIALIZE-DATABASE
;;;----------------------------------------------------------------------------

(defun INITIALIZE-DATABASE (&key (database *database*)(clause-descriptions nil))
  
  ;; Create System Predicates
  (initialize-builtin-predicates :database database)
  
  ;; Initialize Temporal Reasoner
  (initialize-temporal-relations :database database)
  (initialize-transitivity-table)
  
  ;; Create the initial clauses
  (when clause-descriptions
    (process-clause-descriptions clause-descriptions :database database))

  ;; Create the Plan Maintenance OPerator
  ;; Note: This really belongs in the Planner module.
  #+IGNORE
  (process-clause-description *PLAN-MAINTENANCE-OPERATOR* :database database)
  
  t)

;;;----------------------------------------------------------------------------
;;; CLEAR-DATABASE
;;;----------------------------------------------------------------------------

(defun CLEAR-DATABASE (&key (database *database*))
  (reset-clauses :database database)
  (reset-constants :database database)
  (reset-predicates :database database)
  (setf (database-variable-counter database) 0)
  (setf (database-builtin-predicates database) 0)
  (setf (database-proof-trees database) 0)
  t)

;;;----------------------------------------------------------------------------
;;; RESET-DATABASE
;;;----------------------------------------------------------------------------

(defun RESET-DATABASE (&key (database *database*)(clause-descriptions nil))
  (clear-database :database database)
  (initialize-database :database database :clause-descriptions clause-descriptions))

;;;--------------------------------------------------------------------------
;;; INITIALIZE-BUILTIN-PREDICATES
;;;--------------------------------------------------------------------------

(defun INITIALIZE-BUILTIN-PREDICATES (&key (database *database*))
  (setf *AND-PREDICATE*  (make-builtin-predicate 'AND :database database))
  (setf *OR-PREDICATE* (make-builtin-predicate 'OR :database database))
  (setf *NOT-PREDICATE* (make-builtin-predicate 'NOT :database database))
  (setf *IMPLIES-PREDICATE* (make-builtin-predicate 'IMPLIES :database database))
  (setf *TEMPORAL-PREDICATE* (make-builtin-predicate 'TEMPORAL :database database))
  (setf *SPATIAL-PREDICATE* (make-builtin-predicate 'SPATIAL :database database))
  (setf *HORN-PREDICATE* (make-builtin-predicate 'HORN :database database))
  (setf *HOLDS-PREDICATE* (make-builtin-predicate 'HOLDS :database database))
  #+IGNORE
  (setf *MAINTAIN-PREDICATE* (make-builtin-predicate 'MAINTAIN :database database))
  #+IGNORE
  (setf *OPERATOR-PREDICATE* (make-builtin-predicate 'OPERATOR :database database))
  #+IGNORE
  (setf *GOAL-PREDICATE* (make-builtin-predicate 'GOAL :database database))
  (setf (database-builtin-predicates database)
    `(,*AND-PREDICATE*
      ,*OR-PREDICATE*
      ,*NOT-PREDICATE*
      ,*IMPLIES-PREDICATE*
      ,*TEMPORAL-PREDICATE*
      ,*SPATIAL-PREDICATE*
      ,*HORN-PREDICATE*
      ,*HOLDS-PREDICATE*
      ;;,*MAINTAIN-PREDICATE*
      ;;,*OPERATOR-PREDICATE*
      ;;,*GOAL-PREDICATE*
      ))
  t)

;;;----------------------------------------------------------------------------
;;; INITIALIZE-TEMPORAL-RELATIONS
;;;----------------------------------------------------------------------------

(defun INITIALIZE-TEMPORAL-RELATIONS (&key (database *database*))
  (declare (ignore database))
  ;; Create the 13 temporal relations
  (setf *BEFORE* (make-temporal-relation '< :index 0 :prime-index 2))
  (setf *DURING* (make-temporal-relation 'd :index 1 :prime-index 3))
  (setf *OVERLAPS* (make-temporal-relation 'o :index 2 :prime-index 5))
  (setf *MEETS* (make-temporal-relation 'm :index 3 :prime-index 7))
  (setf *STARTS* (make-temporal-relation 's :index 4 :prime-index 11))
  (setf *FINISHES* (make-temporal-relation 'f :index 5 :prime-index 13))
  (setf *EQUALS* (make-temporal-relation '= :index 6 :prime-index 17))
  (setf *FINISHED-BY* (make-temporal-relation 'fi :index 7 :prime-index 19))
  (setf *STARTED-BY* (make-temporal-relation 'si :index 8 :prime-index 23))
  (setf *MET-BY* (make-temporal-relation 'mi :index 9 :prime-index 29))
  (setf *OVERLAPPED-BY* (make-temporal-relation 'oi :index 10 :prime-index 31))
  (setf *CONTAINS* (make-temporal-relation 'di :index 11 :prime-index 37))
  (setf *AFTER* (make-temporal-relation '> :index 12 :prime-index 43))
  
  ;; Setup inverse relations
  (setf (inverse-temporal-relation *before*) *after*)
  (setf (inverse-temporal-relation *after*) *before*)
  (setf (inverse-temporal-relation *during*) *contains*)
  (setf (inverse-temporal-relation *contains*) *during*)
  (setf (inverse-temporal-relation *overlaps*) *overlapped-by*)
  (setf (inverse-temporal-relation *overlapped-by*) *overlaps*)
  (setf (inverse-temporal-relation *meets*) *met-by*)
  (setf (inverse-temporal-relation  *met-by*) *meets*)
  (setf (inverse-temporal-relation *starts*) *started-by*)
  (setf (inverse-temporal-relation  *started-by*) *starts*)
  (setf (inverse-temporal-relation  *finishes*) *finished-by*)
  (setf (inverse-temporal-relation  *finished-by*) *finishes*)
  (setf (inverse-temporal-relation  *equals*) *equals*)
    
  (setf *TEMPORAL-RELATIONS* `(,*BEFORE* ,*AFTER* ,*DURING* ,*CONTAINS* ,*OVERLAPS*
			       ,*OVERLAPPED-BY* ,*MEETS* ,*MET-BY* ,*FINISHES*
			       ,*FINISHED-BY* ,*STARTS* ,*STARTED-BY* ,*EQUALS*))
  
  t)

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
