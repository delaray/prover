(in-package :PROVER)

;;;--------------------------------------------------------------------------
;;; Prover
;;;--------------------------------------------------------------------------

(defvar *prover-bindings* nil)
(defvar *temp-rels* nil)
(defvar *curnode* nil)

;;;--------------------------------------------------------------------------
;;; Builtin Predicates
;;;--------------------------------------------------------------------------

(defvar *AND-PREDICATE* nil)
(defvar *OR-PREDICATE* nil)
(defvar *NOT-PREDICATE*  nil)
(defvar *IMPLIES-PREDICATE* nil)
(defvar *TEMPORAL-PREDICATE* nil)
(defvar *SPATIAL-PREDICATE* nil)
(defvar *HORN-PREDICATE* nil)
(defvar *MAINTAIN-PREDICATE* nil)
(defvar *HOLDS-PREDICATE* nil)
(defvar *OPERATOR-PREDICATE* nil)
(defvar *GOAL-PREDICATE* nil)

;;;--------------------------------------------------------------------------
;;; TEMPORAL RELATIONS
;;;--------------------------------------------------------------------------

(defvar *BEFORE* nil)
(defvar *DURING* nil)
(defvar *OVERLAPS* nil)
(defvar *MEETS* nil)
(defvar *STARTS* nil)
(defvar *FINISHES* nil)
(defvar *EQUALS* nil)
(defvar *FINISHED-BY* nil)
(defvar *STARTED-BY* nil)
(defvar *MET-BY* nil)
(defvar *OVERLAPPED-BY* nil)
(defvar *CONTAINS* nil)
(defvar *AFTER* nil)

(defvar *TEMPORAL-RELATIONS* nil)

;;;--------------------------------------------------------------------------

;;; This is a list of all the intervals in the database.

(defvar *TEMPORAL-INTERVALS* nil)

;;;--------------------------------------------------------------------------

;;; This initializes the transitivity table:

(defvar *TEMPORAL-RELATIONS-TRANSITIVITY-TABLE* 
    (make-array '(13 13) :initial-contents
	'(((<) (< o m d s) (<) (<) (<) (< o m d s) (<) (<)  
	    (<)(< o m d s)(< o m d s)  (<)(< > m mi f fi d di o oi = s si))
	  ((<)(d) (< o m d s)(<)
           (d)(d)(d)(< o m d s)(> oi mi d f)(>)(> oi mi d f) (< > m mi f fi d di o oi = s si)(>))
          ((<)(o d s)(< o m)(<)(o)(d s o)(o)(< o m)
           (di fi o)(oi di si)(s o oi si d di f fi =)(< o m di fi)(> oi di mi si))
          ((<)(o d s)(<)(<)(m)(d < o)(m)(<)(m)(f fi =)(o d s)(<)(> oi mi di si))
          ((<)(d)(< o m)(<)(s)(d)(s)(< m o)(s si =)(mi)(oi d f)(< o m di fi)(>))
          ((<)(d)(o s d)(m)(d)(f)(f)(f fi =)(> oi mi)(>)(> oi mi)(> oi mi di si)(>))
          ((<)(d)(o)(m)(s)(f)(=)(fi)(si)(mi)(oi)(di)(>))
          ((<)(o si d)(o)(m)(o)(f fi =)(fi)(fi)(di)(si oi di)(oi di si)(di)(> oi mi di si))
          ((< o m di fi)(oi d f)(o di fi)(o di fi)(s si =)(oi)(si)(di)
           (si)(mi)(oi)(di)(>))
          ((< o m di fi)(oi d f)(oi d f)(s si =)(d f oi)(mi)(mi)(mi)(>)(>)(>)(>)(>))
          ((< o m di fi)(oi d f)(o oi s si d di f fi =)
           (o di fi)(oi d f)(oi)(oi)(oi di si)(oi > mi)(>)(> oi mi)(> oi mi di si)(>))
          ((< o m di fi)(o oi d di s =)(o di fi)
           (o di fi)(di fi o)(di si oi)(di)(di)(di)(oi di si)(oi di si)(di)(> oi di mi si))
	  ((< > m mi f fi d di o oi = s si) (> oi mi d f) (> oi mi d f)
           (> oi mi d f)(> oi mi d f)(>)(>)(>) (>)(>)(>) (>) (>)))))

;;;--------------------------------------------------------------------------
;;; TREE
;;;--------------------------------------------------------------------------

(defvar *TREE* nil)

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
