;;  draw-something.lisp -  The main lifecycle code for draw-something.
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

(defconstant save-directory "./drawings/")

(defmethod generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil 
	    "~a~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A~a" 
	    save-directory
	    "drawing" year month date hours minutes seconds ".eps")))

(defmethod run ()
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting draw-something.~%")
  (setf *random-state* (make-random-state t))
  ;;(format t "Random state: ~a.~%" (write-to-string *random-state*))
  (let ((filename (generate-filename)))
    (write-drawing filename
		   (draw-something))
    (advisory-message "Finished draw-something.~%")))
;;    #+openmcl (ccl::os-command (format nil "open ~a" filename))
;;    #+sbcl (sb-ext:run-program "/usr/bin/evince" (list filename))))

;; Move to a codelet-based system
;; So find possible spaces, generate figures bit by bit by codelets,
;;  add colour by codelets (or decide on colour by codelets).
;; A sea of possible rules and objects and developments.

(defconstant object-symbol-choices
  '(leaf vein blade branch flower tendril background))

(defconstant all-object-symbols
  (cons background object-symbol-choicess))

(defmethod object-symbol (obj)
  (choose-one-of object-symbol-choices))

(defmethod draw-something ()
  "Make a drawing."
  (let* ((the-drawing (make-drawing)))
    (colour-objects the-drawing all-object-symbols)
	 ;;(rack (make-instance 'coderack)))
;;    (initialise-coderack rack the-drawing)
;;    (coderack-draw-loop rack the-drawing)
    the-drawing))
