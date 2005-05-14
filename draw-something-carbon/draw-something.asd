;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          carbon-example.asd
;;;; Project:       bosco - Cocoa application template for OpenMCL
;;;; Purpose:       hello world in carbon
;;;; Programmer:    mikel evins
;;;; Date Started:  02/04/2004
;;;;
;;;; $Id: draw-something.asd,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(require :asdf)

(in-package #:asdf)

;;; Carbon application
(asdf:defsystem #:draw-something
  :serial t
  :depends-on (:bosco-carbon)
  :components
  ((:file "package")
   (:file "utilities")
   (:file "geometry")
   (:file "drawing")
   (:file "draw-something")
   (:file "application")))

;;; ----------------------------------------------------------------------
;;; load the application
;;; ----------------------------------------------------------------------

(defun cl-user::load-draw-something ()
  (load "bosco.asd")
  (format t "~%loading draw-something...~%")
  (operate 'load-op :draw-something))

(defun cl-user::make (&key
		      (app-name "draw-something")
		      swank-port
		      (application-class "DRAW-SOMETHING-APPLICATION")
		      (application-name "draw-something")
		      (framework :carbon))
  (cl-user::load-draw-something)
  (cl-user::make-bosco :application-class application-class
		       :application-name application-name
		       :swank-port swank-port))

