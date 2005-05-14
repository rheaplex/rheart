;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       bosco - Cocoa application template for OpenMCL
;;;; Purpose:       package definitions for bosco
;;;; Programmer:    mikel evins
;;;; Date Started:  02/04/2004
;;;;
;;;; $Id: package.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(in-package "CL-USER")

;;; --------------------------------------------------
;;; Package: BOSCO
;;; --------------------------------------------------

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage BOSCO
	(:documentation
	 "The package in which most Bosco features are defined.")
	(:use common-lisp ccl)
	(:import-from :ccl
				  )))
