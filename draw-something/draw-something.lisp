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

;;(in-package "DRAW-SOMETHING")

(defconstant save-directory "./drawings/")

(defmethod generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil 
	    "~a~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A~a" 
	    save-directory
	    "drawing" year month date hours minutes seconds ".eps")))

(defmethod write-drawing ((name string) (the-drawing drawing))
  "Write the drawing"
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (ps name :direction :output
		      :if-exists :supersede)
    (write-eps-header (width (bounds the-drawing))
		      (height (bounds the-drawing))
		      :to ps)
    (write-ground the-drawing ps)
    ;;(write-frame the-drawing ps)
    (loop for plane across (planes the-drawing)
	  do (loop for fig across (figures plane)
		   do (write-figure fig ps)))
    (write-eps-footer :to ps)
    (namestring ps)))

(defconstant object-symbol-choices
  '(leaf vein blade branch flower tendril))

(defconstant all-object-symbols
  (cons 'background object-symbol-choices))

(defmethod object-symbol (obj)
  (choose-one-of object-symbol-choices))

(defmethod draw-something ()
  "Make the drawing data structures and create the image."
  (let ((the-drawing (make-drawing)))
    (make-composition-points the-drawing 30)
    (make-planes the-drawing (number-of-planes))
    (make-planes-skeletons the-drawing)
    (draw-planes-figures the-drawing)
    (colour-objects the-drawing all-object-symbols)
    the-drawing))

(defmethod run ()
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting draw-something.~%")
  (setf *random-state* (make-random-state t))
  ;;(format t "Random state: ~a.~%" (write-to-string *random-state*))
  (let* ((filename (generate-filename))
	 (filepath (write-drawing filename
				  (draw-something))))
    (advisory-message "Finished draw-something.~%")
    #+sbcl (sb-ext:run-program "/usr/bin/gv" (list filepath))))
;;    #+openmcl (ccl::os-command (format nil "open ~a" filepath))))
