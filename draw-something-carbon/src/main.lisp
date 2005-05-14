;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: bosco -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Project:       Bosco - Application framework for OSX and OpenMCL
;;;; Purpose:       Bosco 0.6 main program
;;;; Programmer:    mikel evins
;;;; Date Started:  05/12/2004
;;;;
;;;; $Id: main.lisp,v 1.1 2005-05-14 21:47:04 robmyers Exp $
;;;; ***********************************************************************

(in-package "BOSCO")

(defun cl-user::set-ccl-path (path)
  (setf (logical-pathname-translations "ccl")
        `(("lib;**;*.fasl" "ccl:bin;*.fasl")
          ("l1;**;*.fasl" "ccl:l1f;*.fasl")
          ("l1;**;*.pfsl" "ccl:l1pf;*.pfsl")
          ("l1;**;*.sfsl" "ccl:l1sf;*.sfsl")
          ("l1;**;*.dfsl" "ccl:l1df;*.dfsl")
          ("l1;**;*.*" "ccl:level-1;**;*.*")
          ("l1f;**;*.pfsl" "ccl:l1pf;**;*.pfsl")
          ("l1f;**;*.sfsl" "ccl:l1sf;**;*.sfsl")
          ("l1f;**;*.dfsl" "ccl:l1df;**;*.dfsl")
          ("bin;**;*.pfsl" "ccl:binppc;**;*.pfsl")
          ("bin;**;*.sfsl" "ccl:binsparc;**;*.sfsl")
          ("bin;**;*.dfsl" "ccl:bindarwin;**.*.dfsl")
          ("l1pf;**;*.*" "ccl:l1-pfsls;**;*.*")
          ("l1sf;**;*.*" "ccl:l1-sfsls;**;*.*")
          ("l1df;**;*.*" "ccl:l1-dfsls;**;*.*")
          ("l1f;**;*.*" "ccl:l1-fasls;**;*.*")
          ("ccl;*.*" ,(merge-pathnames "*.*" path))
          ("**;*.*" ,(merge-pathnames "**/*.*" path)))))

(defun cl-user::make-bosco (&key
							application-class
							application-name
							swank-port)
  (declare (special *bosco-swank-port*))
  (setf *bosco-swank-port* 
		(if (integerp swank-port)
			swank-port
			nil))
  (let* ((application-class (find-class (intern application-class :bosco)))
		 (image-name (concatenate 'string application-name ".image"))
		 (lisp-kernel-path (car ccl::*command-line-argument-list*))
		 (bosco-kernel-path (concatenate 'string (ccl::current-directory-name) "/" application-name)))
	(ccl::copy-file lisp-kernel-path bosco-kernel-path :if-exists :supersede)
	(cl-user::save-application image-name
							   :application-class application-class)))

