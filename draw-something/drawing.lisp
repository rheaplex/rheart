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
   (ground :accessor ground
	   :type colour
	   :initarg :ground
	   :initform (random-colour)
	   :documentation "The flat body colour of the figure."))
   (:documentation "A drawing in progress."))

(defmethod make-figures ((the-drawing drawing))
  "Make the figures for the drawing."
  ;; Replace with a collecting loop
  (let* ((border (+ pen-distance pen-distance-tolerance pen-width))
	 (figure-bounds (inset-rectangle (bounds the-drawing) border))
	 (figure-count (random-range min-figures max-figures)))
    (advisory-message (format nil "Generating ~a figure skeletons:~%" 
			      figure-count))
    (dotimes (i figure-count)
      (advisory-message (format nil "  ~a. " (+ i 1)))
      (setf (figures the-drawing)
	    (cons 
	     (make-figure (random-rectangle-in-rectangle figure-bounds)
			  (random-range min-figure-points
					max-figure-points))
	     (figures the-drawing))))))

(defmethod make-drawing-bounds ()
  "Make a bounds rectangle for a drawing."
  (make-instance 'rectangle
		 :x 0.0
		 :y 0.0
		 :width (random-range min-drawing-size 
				      max-drawing-size)
		 :height (random-range min-drawing-size 
				       max-drawing-size)))

(defmethod make-drawing ()
  "Make a drawing, ready to be started."
  (let ((the-drawing (make-instance 'drawing :bounds (make-drawing-bounds))))
    (format t "Drawing size: ~dx~d.~%" 
	    (floor (width (bounds the-drawing)))
	    (floor (height (bounds the-drawing))))
    (make-figures the-drawing)
    the-drawing))

(defmethod set-next-figure ((the-drawing drawing))
  "Move on to the next figure and make the pen for it."
  (setf (current-figure-index the-drawing)
	(+ (curent-figure-index the-drawing) 1)))

(defmethod set-pen-for-next-figure ((the-drawing drawing))
  "Make a pen near the next figure and set it as current."
  (setf (pen the-drawing)
	(make-figure-pen (current-figure the-drawing))))

(defmethod draw-figures ((the-drawing drawing))
  "Draw each figure. We'll add a callback system to make this better."
  (advisory-message "Drawing figure outlines:")
  (let ((i 0)) ;; Just for the advisory message
    (dolist (fig (figures the-drawing))
      (advisory-message (format nil " ~a" (+ i 1)))
      (setf i (+ i 1))
      (draw-figure fig)))
  (advisory-message ".~%"))
