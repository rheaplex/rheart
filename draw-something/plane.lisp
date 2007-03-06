;;  plane.lisp - A plane (layer, level, plane) in the drawing.
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

(defconstant min-planes 1)
(defconstant max-planes 5)

(defconstant min-figures 3)
(defconstant max-figures 9)

(defclass plane ()
  ((figure-policy :accessor figure-policy
		  :initarg :figure-policy
		  :documentation "The function for generating figures.")
   (figures :accessor figures
	    :initform (make-vector 10)
	    :documentation "The figures of the plane.")
   (figure-count :accessor figure-count
		 :type integer
		 :initarg :figure-count
		 :documentation "The number of figures to make for the plane.")
   (pen :accessor plane-pen
	;;:type pen
	:initarg :pen
	:documentation "The pen properties for the plane."))
  (:documentation "A plane of the drawing."))

(defconstant plane-pen-distance-minimum 0.1)
(defconstant plane-pen-distance-maximum 5.0)
(defconstant plane-pen-tolerance-minimum (/ plane-pen-distance-minimum 2.0))
(defconstant plane-pen-tolerance-maximum (/ plane-pen-distance-maximum 2.0))

(defmacro make-plane-pen (plane-index num-planes)
  "Make a pen for the plane."
 #| (let ((plane-factor (* (/ 1.0 (- num-planes 1))
			 plane-index)))
    (make-instance 'pen
		   :distance (+ plane-pen-distance-minimum
				(* plane-factor
				   (- plane-pen-distance-maximum
				      plane-pen-distance-minimum )))
		   :step 1.0
		   :tolerance (+ plane-pen-tolerance-minimum
				 (* plane-factor
				    (- plane-pen-tolerance-maximum
				       plane-pen-tolerance-minimum ))))))|#
  nil)

(defconstant minimum-number-of-planes 1)
(defconstant maximum-number-of-planes (length figure-generation-method-list))

(defmethod number-of-planes ()
  "Decide how many planes to have"
  (random-range minimum-number-of-planes
		maximum-number-of-planes))

(defmethod number-of-figures-for-plane (plane-index)
  "Randomly determine how many figures a plane should have."
  (random-range 2 10))

(defmethod make-planes ((d drawing) count)
  "Make the planes, ready to have skeletons for figures generated."
  (loop for point-method in (figure-generation-methods count)
	for i from 0 below count
	do (vector-push-extend 
	    (make-instance 'plane 
			   :figure-count (number-of-figures-for-plane i)
			   :figure-policy point-method
			   :pen (make-plane-pen i count))
	    (planes d))))

(defmethod make-plane-skeletons ((l plane) (d drawing))
  "Generate the skeletons for the figures of the plane."
  (setf (figures l)
	(funcall (figure-policy l)
		 (composition-points d))))

(defmethod make-planes-skeletons ((d drawing))
  "Generate the skeletons for the figures of each plane."
  (loop for l across (planes d)
	do (make-plane-skeletons l d)))

(defmethod draw-plane-figures ((l plane))
  "Draw around the skeletons of the figures of the plane."
  (loop for fig across (figures l)
	do (draw-figure fig))) ;; (pen l)

(defmethod draw-planes-figures ((d drawing))
  "Draw around the skeletons of the figures of each plane."
  (loop for l across (planes d)
	do (draw-plane-figures l)))

#|(defmethod make-figure-for-plane ((figure-bounds rectangle) (plane integer))
  (let* ((form-width (/ (width figure-bounds) plane))
	 (form-height (/ (height figure-bounds) plane)))
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
		     (make-figure-for-plane (bounds the-drawing) i)))))|#