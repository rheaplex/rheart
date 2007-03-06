;;  figure.lisp - A drawn figure.
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

(defconstant min-forms 5)
(defconstant max-forms 10)

(defclass figure ()
  ((forms :accessor forms
	  :type vector
	  :initform (make-vector 5)
	  :initarg :forms
	  :documentation "The forms of the figure.")
   (bounds :accessor bounds
	   :type rectangle
	   :initarg :bounds
	   :documentation "The bounds of the figure."))
  (:documentation "A figure drawn in the drawing."))

(defmethod make-figure-from-points ((points vector))
  "Make a figure with a single polyline from the provided points."
  (let ((fig (make-instance 'figure)))
    (vector-push-extend (make-form-from-points points)
			(forms fig))
    fig))

(defmethod draw-figure ((fig figure))
  "Draw the forms of a figure."
  (loop for form across (forms fig)
	do (draw-form form)))

