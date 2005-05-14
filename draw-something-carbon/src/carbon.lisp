;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: bosco -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          carbon.lisp
;;;; Project:       Bosco - Application framework for OSX and OpenMCL
;;;; Purpose:       Main application for Carbon builds
;;;; Programmer:    mikel evins
;;;; Date Started:  05/12/2004
;;;;
;;;; $Id: carbon.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(in-package "BOSCO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :carbon)
  )

;;; ----------------------------------------------------------------------
;;; Carbon application
;;; ----------------------------------------------------------------------

(defclass bosco-carbon-application (bosco-application)
  ())

(defmethod initialize-instance :after ((app bosco-carbon-application) &key &allow-other-keys)
  (open-shared-library "/System/Library/Frameworks/Carbon.framework/Carbon"))

(defmethod run-application ((app bosco-carbon-application) &key
							&allow-other-keys)
  ;; Start the main event loop
  (when *bosco-swank-port*
	(swank:create-server :port *bosco-swank-port*))
  (#_RunApplicationEventLoop)
  (quit))

(defmethod toplevel-function ((app bosco-carbon-application) init-file)
  (declare (ignore init-file))
  (run-application app))

#| How to use bosco-carbon-application:

1. Use the build tools to make an app image whose application-class is
   bosco-carbon-application

2. launch the application, either from the command-line or by double-clicking

3. From an emacs session with slime loaded, do M-x slime-connect. As the port
   argument, give the slime port of the running Bosco app (11011 by default).

|#

