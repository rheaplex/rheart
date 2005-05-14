;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: bosco -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cocoa.lisp
;;;; Project:       Bosco - Application framework for OSX and OpenMCL
;;;; Purpose:       Main application for Cocoa builds
;;;; Programmer:    mikel evins
;;;; Date Started:  05/12/2004
;;;;
;;;; $Id: cocoa.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

;;; ABOUT

;;; The purpose of application.lisp is to provide a base class
;;; for Bosco applications so that clients of the Bosco framework
;;; can extend annd customize applications to create their own
;;; Bosco-based projects.

(in-package "BOSCO")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-support)
  (use-interface-dir :cocoa)
  )

(defparameter *pool* nil)
(defparameter *nsapp* nil)

;;; ----------------------------------------------------------------------
;;; Cocoa application
;;; ----------------------------------------------------------------------

(defclass bosco-cocoa-application (bosco-application)
  ())

(defmethod initialize-instance :after ((app bosco-cocoa-application) &key &allow-other-keys)
  (open-shared-library "/System/Library/Frameworks/Cocoa.framework/Cocoa"))

(defmethod run-application ((app bosco-cocoa-application) &key
							&allow-other-keys)
  ;; Start the main event loop
  (when *bosco-swank-port*
	(swank:create-server :port *bosco-swank-port*))
  (setf *pool* (ccl::send (ccl::send (ccl::@class ccl::ns-autorelease-pool) 'ccl::alloc) 'ccl::init))
  (setf *nsapp* (ccl::send (ccl::@class ccl::ns-application) 'ccl::shared-application))
  (ccl::send *nsapp* 'ccl::run)
  (when (not (%null-ptr-p *pool*))
	(ccl::send *pool* 'ccl::release))
  (quit))

(defmethod toplevel-function ((app bosco-cocoa-application) init-file)
  (declare (ignore init-file))
  (run-application app))

