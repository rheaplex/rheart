;;  figure.lisp - A drawn figure.
;;  Copyright (C) 2006  Rhea Myers rhea@myers.studio
;;
;; This file is part of draw-something.
;; 
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  (advisory-message "Making figure.~%")
  (let ((fig (make-instance 'figure)))
    (vector-push-extend (make-form-from-points points)
			(forms fig))
    fig))

(defmethod draw-figure ((fig figure))
  "Draw the forms of a figure."
  (loop for form across (forms fig)
     do (advisory-message "Drawing figure.~%")
     do (draw-form form)))

