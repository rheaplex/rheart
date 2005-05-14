;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: bosco -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          interactive.lisp
;;;; Project:       Bosco - Application framework for OSX and OpenMCL
;;;; Purpose:       Bosco 0.6 toplevel for interactive development
;;;; Programmer:    mikel evins
;;;; Date Started:  05/12/2004
;;;;
;;;; $Id: interactive.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(in-package "BOSCO")
(require "FAKE-CFBUNDLE-PATH")

(defun enable-foreground ()
  (ccl::%stack-block ((psn 8))
    (ccl::external-call "_GetCurrentProcess" :address psn)
    (ccl::external-call "_CPSEnableForegroundOperation" :address psn)
    (eql 0 (ccl::external-call "_SetFrontProcess" :address psn :signed-halfword))))

;;; set up paths for interactive use
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((load-path (ccl::loading-file-source-file)))
	(defparameter +bosco-bundle-path+ 
	  (merge-pathnames (make-pathname :directory '(:relative :up "bin" "Bosco.app"
												   "Contents" "MacOS")
									  :name "Bosco") 
					   (make-pathname :directory (pathname-directory load-path)
									  :host (pathname-host load-path)))))
  (ccl::use-interface-dir :carbon)
  (ccl::open-shared-library "/System/Library/Frameworks/Carbon.framework/Carbon"))

(defun run-event-loop ()
  (ccl::%set-toplevel nil)
  (handler-case (#_RunApplicationEventLoop)
	(error (c) (ccl::nslog-condition c)))
  (return-from run-event-loop))

(defclass interactive-session (application)
  ())

(defmethod run-application ((app interactive-session) &key &allow-other-keys)
  (flet ((carbon-startup ()
		   (enable-foreground)           
           (run-event-loop)))
	;; run it
    (ccl::process-interrupt ccl::*initial-process*
							#'(lambda ()
								(ccl::%set-toplevel #'carbon-startup)
								(toplevel)))))

(defun bosco ()
  (setf *app* (make-instance 'interactive-session))
  (initialize-application *app*)
  (run-application *app*))

