(in-package :common-lisp-user)

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          Prover REPL Commands
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;;  Preparations:
;;;    1. Create gbbopen-modules directory in your (user-homedir-pathname) 
;;;       directory
;;;    2. Create a symbolic link to this Meperia source tree in this
;;;       gbbopen-modules directory (Windows users must create a 
;;;       "pseudo-symbolic-link" file, a text file of type .sym that contains 
;;;       the target directory path as the sole line in the file)
;;;
;;;  Then load GBBopen's initiate.lisp from whatever GBBopen installation is
;;;  desired (which will load this command file).
;;;
;;;  Recommended: Set up your CL environment according to the "Enhancing Your
;;;               Development Environment" exercise in the GBBopen Tutorial.
;;;
;;;----------------------------------------------------------------------------


;;; ===========================================================================
;;;   Supporting Entitiy

;; Remember this file:

(defparameter *prover-commands-file* *load-truename*)


;;; ===========================================================================
;;;   Useful Prover Commands

(with-system-name (:Prover)
  
  ;;---------------------------------------------------------------------------

  (define-repl-command :prover (&rest options)
    "Load (compile if needed) :Prover module."
    (format t "~&;; ***** Compiling/Loading Theorem Prover~%")
    ;; Load/compile code.
    (startup-module :prover options :Prover))
  
;;; -------------------------------------------------------------------------

  )

;;;------------------------------------------------------------------------
;;; End of File
;;;------------------------------------------------------------------------
