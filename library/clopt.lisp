;; clopt.lisp - Simple methods to get options from the command line.
;; Copyright 2007 Rhea Myers <rhea@myers.studio>
;;
;; This file is part of rheart.
;; 
;; rheart is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; rheart is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Dummy until I sort command line options

(defmethod get-arg-value (long &key (short nil) (default nil) 
			       (args *cli-args*))
  default)

#|

(defparameter *cli-args* (ccl::command-line-arguments))

;; TODO: Consume parsed arguments

(defmethod get-arg (long &key (short nil) (args *cli-args*))
  "Check the command line arguments for a unary switch."
  (let ((result nil))
    (dolist (arg args)
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf result t)
	(return)))
    result))

(defmethod get-arg-value (long &key (short nil) (default nil) 
			       (args *cli-args*))
  "Check the command line arguments for a switch with an accompanying value."
  (let ((result nil)
	(found nil))
    (dolist (arg args)
      ;; If the last arg matched, return this one as the value
      ;;FIXME: Make sure it's not a - or --switch
      (when (eq found 
		t)
	(setf result arg)
	(return))
      ;; When the arg matches, set a flag to return the next value
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf found t )))
  ;; Return the result or the value  
  (or result
      default)))

(defmethod get-arg-values (long &key (short nil) (default '()) 
				(args *cli-args*))
  "Check the command line arguments for possibly many occurrences of a switch with an accompanying value."
  (let ((result nil)
	(found nil))
    (dolist (arg args)
      ;; If the last arg matched, return this one as the value
      ;;FIXME: Make sure it's not a - or --switch
      (when (eq found 
		t)
	(setf result arg))
      ;; When the arg matches, set a flag to return the next value
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf found t )))
  ;; Return the result (in the order found on the command line) or the value  
  (or (reverse result)
	default)))

|#