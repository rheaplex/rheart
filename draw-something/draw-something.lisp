
;;  run.lisp -  A toy aesthetics description and evaluation system
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
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

(defconstant drawing-width 600)
(defconstant drawing-height 600)

(defmethod generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil 
	    "~a~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A~a" 
	    save-directory
	    "drawing" year month date hours minutes seconds ".eps")))

(defmethod write-figure-skeleton ((fig figure) ps)
  "Write the skeleton the drawing is made around."
  (write-rgb 0.4 0.4 1.0 :to ps)
  (write-new-path :to ps)
  (write-subpath (points (skeleton fig)) :to ps)
  (write-stroke :to ps))

(defmethod write-figure-fill ((fig figure) ps)
  "Write the drawing outline."
    (write-rgb 1.0 1.0 1.0 :to ps)
    (write-new-path :to ps)
    (write-subpath (points (outline fig)) :to ps)
    (write-fill :to ps))

(defmethod write-figure-stroke ((fig figure) ps)
  "Write the drawing outline."
    (write-rgb 0.0 0.0 0.0 :to ps)
    (write-new-path :to ps)
    (write-subpath (points (outline fig)) :to ps)
    (write-stroke :to ps))

(defmethod write-figure ((fig figure) ps)
  "Write the figure for early multi-figure versions of draw-something."
  (write-figure-fill fig ps)
  ;;(write-figure-skeleton fig ps)
  (write-figure-stroke fig ps))

(defmethod write-frame ((the-drawing drawing) ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (write-rectstroke (inset-rectangle (bounds the-drawing) -1)
		    :to ps))

(defmethod write-drawing ((name string) (the-drawing drawing))
  "Write the drawing"
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (ps name :direction :output
		      :if-exists :supersede)
    (write-eps-header (width (bounds the-drawing))
		      (height (bounds the-drawing))
		      :to ps)
    (write-frame the-drawing ps)
    (dolist (fig (figures the-drawing))
      (write-figure fig ps))
    (write-eps-footer :to ps)))

(defmethod draw-something ()
  "Make a drawing."
  (let ((the-drawing (make-drawing)))
    (draw-figures the-drawing)
    the-drawing))

(defmethod run ()
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting draw-something.~%")
  (setf *random-state* (make-random-state t))
  (let ((filename (generate-filename)))
    (write-drawing filename
		   (draw-something))
  (advisory-message "Finished draw-something.~%")
#+openmcl (ccl::os-command (format nil "open ~a" filename))))