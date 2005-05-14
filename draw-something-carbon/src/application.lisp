;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: bosco -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          application.lisp
;;;; Project:       Bosco - Application framework for OSX and OpenMCL
;;;; Purpose:       Common prelude for Bosco applications
;;;; Programmer:    mikel evins
;;;; Date Started:  05/12/2004
;;;;
;;;; $Id: application.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

;;; ABOUT

;;; The purpose of application.lisp is to provide a base class
;;; for Bosco applications so that clients of the Bosco framework
;;; can extend annd customize applications to create their own
;;; Bosco-based projects.

(in-package "BOSCO")

(defun bosco-ccl-directory ()
  (make-pathname
   :directory
     (pathname-directory (car ccl::*command-line-argument-list*))))

(defparameter *bosco-swank-port* nil)
(defparameter *bosco-initialization-functions* nil)

(defclass bosco-application (ccl::lisp-development-system)
  ())

(defparameter *bosco-application-class* (find-class 'bosco-application))

(defmethod initialize-instance :after ((app bosco-application) &key &allow-other-keys)
  (dolist (init *bosco-initialization-functions*)
    (funcall init)))

