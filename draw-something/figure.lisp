;;  figure.lisp - A drawn figure.
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

(defconstant min-forms 1)
(defconstant max-forms 2)

(defclass figure ()
  ((notes :accessor notes
	  :type hashtable
	  :initform (make-hash-table)
	  :documentation "Any notes recodred by codelets on the figure.")
   (forms :accessor forms
	  :type vector
	  :initform (make-vector 5)
	  :documentation "The forms of the figure.")
   (bounds :accessor bounds
	   :type rectangle
	   :initform (make-instance 'rectangle)
	   :initarg :bounds
	   :documentation "The bounds of the figure."))
  (:documentation "A figure drawn in the drawing."))

(defmethod make-figure (the-drawing)
  "Naive figure making method. Replace with many codelets."
  (let ((fig (make-instance 'figure)))
    (dotimes (i (random-range min-forms max-forms))
      (vector-push-extend (make-form (bounds the-drawing)
				     (random-range 1 max-form-points))
			  (forms fig)))
    (vector-push-extend fig (figures the-drawing))
    fig))



