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

(defconstant drawing-width 600)
(defconstant drawing-height 600)
(defconstant border (* pen-distance 2))

(defmethod write-skeleton ((d drawing) ps)
  "Write the skeleton the drawing is made around."
  (write-rgb 0.4 0.4 1.0 :to ps)
  (write-new-path :to ps)
  (write-subpath (points (skeleton d)) :to ps)
  (write-stroke :to ps))

(defmethod write-outline ((d drawing) ps)
  "Write the drawing outline."
    (write-rgb 0.0 0.0 0.0 :to ps)
    (write-new-path :to ps)
    (write-subpath (points (outline d)) :to ps)
    (write-stroke :to ps))

(defmethod write-drawing ((name string) (d drawing))
  "Write the drawing"
  (with-open-file (ps name 
		      :direction :output
		      :if-exists :supersede)
    (write-eps-header drawing-width drawing-height :to ps)
    ;;(write-skeleton d ps)
    (write-outline d ps)
    (write-eps-footer :to ps)))

(defmethod draw-something ()
  "Make a drawing."
  (let ((d (make-drawing border 
			 border 
			 (- drawing-width border) 
			 (- drawing-height border) 
			 12)))
    (do () 
	((should-finish d))
      (draw-step d))
    d))

(defmethod run (&key (name "./drawing.eps"))
  "The main method that generates the drawing and writes it to file."
  (setf *random-state* (make-random-state t))
  (let ((d (draw-something)))
    (write-drawing name d)))