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

(defconstant min-drawing-size 200.0)
(defconstant max-drawing-size 600.0)

(defconstant min-figures 1)
(defconstant max-figures 2)

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
	   :documentation "The flat body colour of the figure."))
   (:documentation "A drawing in progress."))

(defmethod make-drawing-bounds ()
  "Make a bounds rectangle for a drawing."
  (make-instance 'rectangle :x 0.0 :y 0.0
		 :width (random-range min-drawing-size 
				      max-drawing-size)
		 :height (random-range min-drawing-size 
				       max-drawing-size)))

(defmethod make-drawing ()
  "Make a drawing, ready to be started."
  (let ((the-drawing (make-instance 'drawing 
				    :bounds (make-drawing-bounds))))
    ;;(make-cell-matrix the-drawing)
    (format t "Drawing. Size: ~dx~d.~%" 
	    (floor (width (bounds the-drawing)))
	    (floor (height (bounds the-drawing))))
    the-drawing))
  
;; Configure pen for each figure (pos, reset heading)
;; Vary distance & tolerance & pen width for each figure?
;; Skeleton will ultimately be a list of objects

(defmethod draw-figure ((the-drawing drawing) (the-figure figure))
  (let ((figure-bounds (bounds the-figure))
	(the-forms (forms the-figure)))
    (dotimes (i (length the-forms))
      (draw-form (aref the-forms i))
      (include-rectangle figure-bounds (bounds (aref the-forms i)))))
      ;;(mark-figure-cells fig the-drawing)
      )