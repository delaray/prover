(in-package :PROVER)

;;;**************************************************************************
;;;
;;; The Unification Algorithm
;;;
;;;**************************************************************************

;;;--------------------------------------------------------------------------
;;; UNIFY
;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 CLAUSE-ENTITY)(l2 CLAUSE-ENTITY) &key (bindings nil))
  (let* (result new-bindings)
    (cond ((not (= (clause-entity-arity l1)
		   (clause-entity-arity l2)))
	   (values nil bindings))
	  ((eq (clause-entity-predicate l1)
	       (clause-entity-predicate l2))
	   (mapc #'(lambda (a1 a2)
		     (multiple-value-setq (result new-bindings)
		       (unify a1 a2 :bindings bindings))
		     (if result
			 (when new-bindings
			   (setf bindings new-bindings))
		       (return-from unify (values nil bindings))))
		 (clause-entity-arguments l1)
		 (clause-entity-arguments l2))
	   (values result bindings))
	  (t 
	   (values nil bindings)))))

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 HOLDS-CLAUSE-ENTITY)(l2 HOLDS-CLAUSE-ENTITY) &key (bindings nil))
  (multiple-value-bind (unifies-p bindings)
      (unify (first (clause-entity-arguments l1))
	     (first (clause-entity-arguments l2))
	     :bindings bindings)
    (if unifies-p
	(unify (second (clause-entity-arguments l1))
	       (second (clause-entity-arguments l2))
	       :bindings bindings)
      (values nil bindings))))
	  

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 NULL)(l2 NULL) &key (bindings nil))
  (values t bindings))

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 ATOMIC-ENTITY)(l2 ATOMIC-ENTITY) &key (bindings nil))
  (values (eq l1 l2) bindings))

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 ENTITY-MIXIN)(l2 VARIABLE-ENTITY) &key (bindings nil))
  (unify l2 l1 :bindings bindings))

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 ATOMIC-ENTITY)(l2 VARIABLE-ENTITY) &key (bindings nil))
  (unify l2 l1 :bindings bindings))

;;;--------------------------------------------------------------------------

(defmethod UNIFY ((l1 VARIABLE-ENTITY)(l2 ENTITY-MIXIN) &key (bindings nil))
  (cond ((eq l1 l2)
	 (values t bindings))
	((variable-entity-boundp l1)
	 (unify (variable-entity-value l1) l2 
		:bindings bindings))
	((find-binding l1 bindings)
	 (unify (binding-value (find-binding l1 bindings)) l2
		:bindings bindings))
	(t
	 (values t (cons (make-binding l1 l2) bindings)))))

;;;--------------------------------------------------------------------------

;;; These are defined for convenience for toplevel interaction.

(defmethod UNIFY ((s1 SYMBOL)(s2 SYMBOL) &key (bindings nil))
  (setf s1 (parse-atomic-argument s1))
  (setf s2 (parse-atomic-argument s2))
  (unify s1 s2 :bindings bindings))

;;;--------------------------------------------------------------------------
;;; INSTANTIATE-BINDINGS
;;;--------------------------------------------------------------------------

;;; This function instantiates the variables in 'bindings' with the values
;;; specified in 'bindings'.

(defun INSTANTIATE-BINDINGS (bindings)
  (mapc #'(lambda (binding)
	    (bind-variable (binding-variable binding)(binding-value binding)))
	bindings)
  bindings)

;;;--------------------------------------------------------------------------
;;; Finding Bindings
;;;--------------------------------------------------------------------------

(defun FIND-ALL-BINDINGS-IN-TREE (simple-clause)
  (let ((vars (clause-entity-variables simple-clause)))
    (mapcar #'(lambda (var)
                (list var (find-binding-in-tree var (list *TREE*))))
            vars)))

;;;--------------------------------------------------------------------------

(defun FIND-BINDING-IN-TREE (var nodes)
  (let (binding result)
    (setf binding (value-var var (get (car nodes) 'bindings)))
    (when nodes
      (cond (binding
             (if (not (variable-p binding))
               binding
               (find-binding-in-tree binding (list (car nodes)))))
            ((setf result (find-binding-in-tree var (child (car nodes))))
             result)
            (t
             (find-binding-in-tree var (cdr nodes)))))))

;;;--------------------------------------------------------------------------

#+IGNORE
(defun CLAUSE-ENTITY-VARIABLES (clause)
  (cond ((null clause) nil)
        ((listp (car clause))
         (union  (clause-entity-variables (car clause))
                (clause-entity-variables (cdr clause))))
        ((variable-p (car clause))
         (cons (car clause)(clause-entity-variables (cdr clause))))
        (t
         (clause-entity-variables (cdr clause)))))

;;;--------------------------------------------------------------------------
;;; TYPED VARIABLES
;;;--------------------------------------------------------------------------

;;; This function returns the type of a variable, i.e the name is stripped
;;; off.

#+IGNORE
(defun var-type (var)
  (let ((type-name (var-type1 (explodec var))))
    (if (null type-name)
      'ANYTHING
      (readlist type-name))))

;;;--------------------------------------------------------------------------

#+IGNORE
(defun var-type1 (list-of-chars)
  (cond ((null list-of-chars) nil)
        ((equal (car list-of-chars) #\.)
         (cdr list-of-chars))
        (t
         (var-type1 (cdr list-of-chars)))))

;;;--------------------------------------------------------------------------

;;; This function defines 'object' to be of type type-name.

#+IGNORE
(defun def-type (object type-name)
  (setf (get object 'TYPE) type-name)
  (setf (get type-name 'MEMBERS)
        (cons object (get type-name 'MEMBERS))))

;;;--------------------------------------------------------------------------

;;; This returns the type of an object.

#+IGNORE
(defun get-type (object)
  (let (type)
    (setf type (cond ((variable-entity-p object)
                   (var-type object))
                  (t
                   (get object 'TYPE))))
    (if (null type) 'ANYTHING type)))

;;;--------------------------------------------------------------------------

;;; This returns the members of a given type.

#+IGNORE
(defun get-members (type)
  (get type 'MEMBERS))

;;;--------------------------------------------------------------------------

;;; This returns t if type1 is a supertype of type2.

#+IGNORE
(defun is-supertype (type1 type2)
  (cond ((null type2) nil)
        ((equal type1 type2) t)
        (t
         (is-supertype type1 (parent type2)))))

;;;--------------------------------------------------------------------------

;;; this returns t if type1 is a subtype of type2.

#+IGNORE
(defun is-subtype (type1 type2)
  (is-supertype type2 type1))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------
