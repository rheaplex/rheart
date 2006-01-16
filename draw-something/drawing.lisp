;;  drawing.lisp - Drawing around shapes.
;;  Copyright (C) 2004-5  Rhea Myers rhea@myers.studio
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

(defconstant min-figures 1)
(defconstant max-figures 12)
(defconstant min-figure-points 1)
(defconstant max-figure-points 12)

(defconstant min-drawing-size 200.0)
(defconstant max-drawing-size 600.0)

(defconstant figure-hints-choices 
  '((overlap separate)
    (big medium small)
    (complex simple)
    (many some few)))

(defclass drawing ()
  ((bounds :accessor bounds
	   :type rectangle
	   :initarg :bounds
	   :documentation "The dimensions of the drawing.")
   (figures :accessor figures
	    :type list
	    :initarg :figures
	    :initform '()
	    :documentation "The figures of the drawing.")
   (figure-hints :accessor hints
		 :type list
		 :initarg :hints
		 :initform '()
		 :documentation "The requirement tags for the drawing.")
   (ground :accessor ground
	   :type colour
	   :initarg :ground
	   :initform (random-colour)
	   :documentation "The flat body colour of the figure."))
   (:documentation "A drawing in progress."))

(defmethod make-figure-hints ()
  "Choose hints for the drawing."
  (let ((hints '()))
    (loop for options in figure-hints-choices
	 do (let ((hint (maybe-choose-one-of options)))
	      (when hint
		(setf hints (cons hint hints)))))
    hints))

(defmethod rectangle-overlaps-figures ((rect rectangle) (the-drawing drawing))
  "Check to see if the figure overlaps any of the others."
  (dolist (other-fig (figures the-drawing))
    (when (intersects rect (bounds other-fig))
      (return t))))

;; Make these account for pen width, distance and tolerance.
;; Hmm. Skeletons for some will be drawn before outlines for others,
;; this embodies planning but not feedback.

;; Handle other hints

(defmethod make-overlapping-figure-bounds (the-drawing max-bounds)
  "Make a bounding rectangle that overlaps at least one other."
    (let (this-figure-bounds)
      (loop 
	 do (setf this-figure-bounds
		  (random-rectangle-in-rectangle max-bounds))
	 until (rectangle-overlaps-figures this-figure-bounds
					   the-drawing)
	 finally (return this-figure-bounds))))

(defmethod make-non-overlapping-figure-bounds (the-drawing max-bounds)
  "Make a bounding rectangle that doesn't overlap another."
  (let (this-figure-bounds)
    (loop 
       do (setf this-figure-bounds
		(random-rectangle-in-rectangle max-bounds))
	    while (rectangle-overlaps-figures this-figure-bounds
					      the-drawing)
       finally (return this-figure-bounds))))

(defmethod make-drawing-figure-bounds (the-drawing max-bounds)
  "Make the bounds for one figure in the drawing."
    (cond
      ((member 'overlap (hints the-drawing))
       (make-overlapping-figure-bounds the-drawing max-bounds))
      ((member 'separate (hints the-drawing))
       (make-non-overlapping-figure-bounds the-drawing max-bounds))
      (t (random-rectangle-in-rectangle max-bounds))))
  
(defmethod make-figures ((the-drawing drawing))
  "Make the figures for the drawing."
  (let ((figure-bounds (inset-rectangle (bounds the-drawing) 
					(* pen-distance 2.0)))
	(figure-count (random-range min-figures max-figures)))
    (advisory-message (format nil "Generating ~a figure skeletons:~%" 
			      figure-count))
    (loop for i from 0 below figure-count
       collect (make-figure (make-drawing-figure-bounds the-drawing
							figure-bounds)
			    (random-range min-figure-points
					  max-figure-points)))))

(defmethod make-drawing-bounds ()
  "Make a bounds rectangle for a drawing."
  (make-instance 'rectangle :x 0 :y 0
		 :width (round (random-range min-drawing-size 
					     max-drawing-size))
		 :height (round (random-range min-drawing-size 
					      max-drawing-size))))

(defmethod make-drawing ()
  "Make a drawing, ready to be started."
  (let ((the-drawing (make-instance 'drawing 
				    :bounds (make-drawing-bounds)
				    :hints (make-figure-hints))))
    (format t "Drawing. Size: ~dx~d. Hints: ~a~%" 
	    (floor (width (bounds the-drawing)))
	    (floor (height (bounds the-drawing)))
	    (hints the-drawing))
    (setf (figures the-drawing) (make-figures the-drawing))
    the-drawing))

(defmethod draw-figures ((the-drawing drawing))
  "Draw each figure. We'll add a callback system to make this better."
  (advisory-message "Drawing figure outlines:")
  (let ((i 0)) ;; Just for the advisory message
    (dolist (fig (figures the-drawing))
      (advisory-message (format nil " ~a" (+ i 1)))
      (setf i (+ i 1))
      (draw-figure fig)))
  (advisory-message ".~%"))