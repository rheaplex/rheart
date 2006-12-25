;;  drawing.lisp - A drawing.
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

(defconstant min-drawing-size 200.0)
(defconstant max-drawing-size 600.0)

(defconstant min-figures 3)
(defconstant max-figures 9)

(defclass drawing ()
  ((bounds :accessor bounds
	   :type rectangle
	   :initarg :bounds
	   :documentation "The dimensions of the drawing.")
   (figures :accessor figures
	    :type vector
	    :initarg :figures
	    :initform (make-vector 10)
	    :documentation "The figures of the drawing.")
   (cell-matrix :type 'array
		:accessor cell-matrix
		:initarg :cells
		:documentation "The cells of the image.")
   (notes :accessor notes
	  :type hashtable
	  :initform (make-hash-table)
	  :documentation "Any notes the codelets have recorded on the drawing.")
   (ground :accessor ground
	   :type colour
	   :initarg :ground
	   :initform (random-colour)
	   :documentation "The flat background colour of the drawing."))
   (:documentation "A drawing in progress."))

;; Change to width or height being max and other being random range

(defmethod make-drawing-bounds ()
  "Make a bounds rectangle for a drawing."
  (make-instance 'rectangle :x 0.0 :y 0.0
		 :width (random-range min-drawing-size 
				      max-drawing-size)
		 :height (random-range min-drawing-size 
				       max-drawing-size)))

(defmethod add-figure ((the-drawing drawing) fig)
  "Add the figure to the drawing."
    (vector-push-extend fig (figures the-drawing)))

;; Make these account for pen width, distance and tolerance.
;; Hmm. Skeletons for some will be drawn before outlines for others,
;; this embodies planning but not feedback.

(defmethod make-figure-for-layer ((figure-bounds rectangle) (layer integer))
  (let* ((form-width (/ (width figure-bounds) layer))
	 (form-height (/ (height figure-bounds) layer)))
    (make-figure figure-bounds form-width form-height)))
	 
(defmethod make-figures ((the-drawing drawing))
  "Make the figures for the drawing."
  (let ((figure-count (random-range-inclusive min-figures max-figures)))
    (advisory-message (format nil "Making ~a figures.~%" 
			      figure-count))
    (loop for i from 1 to figure-count
      do (advisory-message (format nil "Making figure ~a/~a.~%" i
				   figure-count))
      do (add-figure the-drawing
		     (make-figure-for-layer (bounds the-drawing) i)))))

(defmethod make-drawing ()
  "Make a drawing, ready to be started."
  (let ((the-drawing (make-instance 'drawing 
				    :bounds (make-drawing-bounds))))
    ;;(make-cell-matrix the-drawing)
    (format t "Drawing. Size: ~dx~d.~%" 
	    (floor (width (bounds the-drawing)))
	    (floor (height (bounds the-drawing))))
    (make-figures the-drawing)
    the-drawing))

#|

(defmethod make-drawing ()
  "Make a drawing, ready to be started."
  (let ((the-drawing (make-instance 'drawing 
				    :bounds (make-drawing-bounds))))
    (make-cell-matrix the-drawing)
    (format t "Drawing. Size: ~dx~d.~%" 
	    (floor (width (bounds the-drawing)))
	    (floor (height (bounds the-drawing))))
    the-drawing))

(defmethod draw-figures ((the-drawing drawing))
  "Draw each figure. We'll add a callback system to make this better."
  (advisory-message "Drawing figure outlines:")
  (let ((i 0)) ;; Just for the advisory message
    (dolist (fig (figures the-drawing))
      (advisory-message (format nil " ~a" (+ i 1)))
      (setf i (+ i 1))
      (draw-figure fig)
      ;;(mark-figure-cells fig the-drawing)
      ))
  (advisory-message ".~%"))
  
;; Configure pen for each figure (pos, reset heading)
;; Vary distance & tolerance & pen width for each figure?
;; AARON varies...
;; Skeleton will ultimately be a list of objects

|#

