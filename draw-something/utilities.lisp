;;  utilities.lisp - Various utilities.
;;  Copyright (C) 2006  Rhea Myers rhea@myers.studio
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(in-package "DRAW-SOMETHING")

(defmethod debug-message (msg)
  "Write the message to the error stream, not to standard output."
  (format *debug-io* 
	  "~A~%" 
	  msg)
  (finish-output *debug-io*))

(defmethod advisory-message (msg)
  "Write the message to the error stream, not to standard output. No newline."
  (format *debug-io* 
	  msg)
  (finish-output *debug-io*))

(defmethod dequote (item)
  "Remove the quote from a symbol, returning the symbol."
  (cadr item))

(defmethod random-number (a)
  "The built in random doesn't like 0.0 ."
  (if (= a 0)
      a
      (random a)))

(defmethod random-range (a b)
  "Make a random number from a to below b."
  (let ((range (- b a)))
    (if (= range 0)
	a
	(+ (random range) a))))

(defmethod random-range-inclusive ((a integer) (b integer))
  "Make a random number from a to below b."
  (let ((range (+ (- b a) 1)))
    (if (= range 0)
	a
	(+ (random range) a))))

(defmethod choose-one-of ((possibilities list))
  "Choose one or none of the options."
  (nth (random (length possibilities)) possibilities))

(defmethod choose-one-of ((possibilities vector))
  "Choose one or none of the options."
  (aref possibilities (random (length possibilities))))

(defmethod maybe-choose-one-of (possibilities)
  "Choose one or none of the options."
  (when (< (random 1.0) 0.5)
    (choose-one-of possibilities)))

(defmethod maybe-choose-some-of (possibilities probability)
  "Choose none or more possibilities when random 1.0 < probability for it."
  (loop for item in possibilities
     when (< (random 1.0) probability)
     collect item))

(defun choose-n-of (n choice-list)
  "Choose n different entries from choice-list."
  (assert (<= n (length choice-list)))
  (let ((choices choice-list)
	(chosen '()))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
	(setf chosen (cons choice chosen))
	(setf choices (remove choice choices))))
    chosen))

(defun prefs-range (spec)
  "Get the total probability range of a prefs spec."
  (loop for prob in spec by #'cddr
	sum prob))

(defun prefs-cond (spec)
  "Make a cond to choose an option. eg (prefs 4 'a 4 'b 2 'c)"
  `(let ((i (random ,(prefs-range spec))))
    (cond
      ,@(loop for prob in spec by #'cddr
	      for val in (cdr spec) by #'cddr
	      sum prob into prob-so-far
	      collect `((< i ,prob-so-far) ,val)))))

(defmacro prefs (&rest spec)
  "Make a prefs cond to choose an option. eg (prefs 4 'a 4 'b 2 'c)"
  (prefs-cond spec))
		   
(defmacro prefs-list (spec)
  "Make a prefs cond to choose an option. eg (prefs-list '(4 'a 3 'b))"
  (prefs-cond spec))

(defun prefs-lambda (&rest spec)
  "Make a lambda to choose an option. eg (prefs-lambda 4 'a 4 'b 2 'c)"
  (eval `(lambda () ,(prefs-cond spec))))
		   
(defun prefs-list-lambda (spec)
  "Make a lambda to choose an option. eg (prefs-list-lambda '(4 'a 3 'b))"
   (eval `(lambda () ,(prefs-cond spec))))

(defmethod make-vector (initial-size)
  "Make a stretchy vector."
  (make-array initial-size
	      :adjustable t
	      :fill-pointer 0))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &rest body)
  `(do ()
       (,test)
     ,@body))

(defmacro dovector ((var vec) &rest body)
  `(loop for ,var across ,vec
      do (progn ,@body)))