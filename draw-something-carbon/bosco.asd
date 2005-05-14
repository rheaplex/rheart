;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bosco.asd
;;;; Project:       bosco - Cocoa application template for OpenMCL
;;;; Purpose:       asdf system definition
;;;; Programmer:    mikel evins
;;;; Date Started:  02/04/2004
;;;;
;;;; $Id: bosco.asd,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(require :asdf)

(in-package #:asdf)

;;; Carbon application
(asdf:defsystem #:bosco-carbon
    :version "0.6"
    :serial t
    :depends-on ()
    :components
    ((:module src
	      :serial t
	      :components
			((:module lib
				  :serial t
				  :components
				  ((:module slime
					    :serial t
					    :components
					    ((:file "swank-backend")
					     (:file "nregex")
					     (:file "swank-openmcl")
					     (:file "swank-gray")
					     (:file "swank")))))
			 (:file "package")
			 (:file "application")
			 (:file "carbon")
			 (:file "main")))))

;;; Cocoa application
(asdf:defsystem #:bosco-cocoa
    :version "0.6"
    :serial t
    :depends-on ()
  :components
  ((:module src
	    :serial t
	    :components
	    ((:module lib
		      :serial t
		      :components
		      ((:module slime
				:serial t
				:components
				((:file "swank-backend")
				 (:file "nregex")
				 (:file "swank-openmcl")
				 (:file "swank-gray")
				 (:file "swank")))))
	     (:file "package")
	     (:file "application")
	     (:file "cocoa")
	     (:file "main")))))

;;; ----------------------------------------------------------------------
;;; load the application
;;; ----------------------------------------------------------------------

(defun cl-user::load-bosco-carbon ()
  (format t "~%loading Bosco carbon framework...~%")
  (operate 'load-op :bosco-carbon))

(defun cl-user::load-bosco-cocoa ()
  (format t "~%loading Bosco cocoa framework...~%")
  (operate 'load-op :bosco-cocoa))

(defun cl-user::make (&key
		      (app-name "Bosco")
		      swank-port
		      (application-class "BOSCO-CARBON-APPLICATION")
		      (application-name "Bosco")
		      framework)
  (cond
    ((eq framework :carbon)(cl-user::load-bosco-carbon))
	((eq framework :cocoa)(cl-user::load-bosco-cocoa))
	(t (error "Unknown application framework!")))
  (cl-user::make-bosco :application-class application-class
		       :application-name application-name
		       :swank-port swank-port))

