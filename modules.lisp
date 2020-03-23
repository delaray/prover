(in-package :module-manager)

;;; ===========================================================================
;;;
;;; Author: Raymond de Lacaze
;;;
;;; This file defines  basic Theorem Prover using GBBopen's mini-module system.
;;;
;;; ===========================================================================

;;;----------------------------------------------------------------------------
;;; Knowledge-Base Package
;;;----------------------------------------------------------------------------

(eval-when (eval compile load)
  (unless (find-package :prover)
    (make-package "PROVER"
		  :use '(#-SBCL :user #+SBCL :cl-user :common-lisp #-SBCL
			 :clos #-SBCL :excl))))

;;;----------------------------------------------------------------------------

(define-root-directory '(:prover-root) *load-truename*)

;;;----------------------------------------------------------------------------
;;; Global Prover Pathname Related Variables
;;;----------------------------------------------------------------------------

(defparameter *PROVER-DEVICE*
    (pathname-device *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *PROVER-DIRECTORY*
   (pathname-directory *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *PROVER-ROOT*
    (make-pathname :name nil :type nil
                   :directory (pathname-directory *load-truename*)
                   :defaults  *load-truename*))

;;;----------------------------------------------------------------------------
;;; PROVER
;;;----------------------------------------------------------------------------

;;; This is the main prover module.

(define-module :prover
  (:directory :prover-root)
  (:requires :utilities)
  (:files "Prover-Variables"
	  "Prover-Classes"
	  "Prover-Entities"
	  "Prover-Utilities"
	  "Prover-And-Or-Tree"
	  "Prover-Clauses"
	  "Prover-Temporal"
	  "Prover-Database"
	  "Prover-Unify"
	  "Prover-Reasoner"
	  "Prover-Example"))

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
