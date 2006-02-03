;;  form.lisp - A form of a figure.
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


(defconstant min-form-points 1)
(defconstant max-form-points 12)

(defconstant pen-distance 5.0)
(defconstant pen-distance-tolerance 1.37)
(defconstant pen-forward-step 2.0)
(defconstant pen-turn-step 0.01)

(defconstant pen-width 1.0)

(defconstant form-step-limit 5000)

(defclass form ()
  ((notes :accessor notes
	  :type hashtable
	  :initform (make-hash-table)
	  :documentation "Any notes recorded on the form by codelets.")
   (skeleton :accessor skeleton 
	     :type vector
	     :initarg :skeleton
	     :initform (make-vector 5)
	     :documentation "The guide shape for the outline.")
   (outline :accessor outline
	    :type polyline
	    :initform (make-instance 'polyline)
	    :documentation "The outlines for the skeleton. Will be outline_s_.")
   (bounds :accessor bounds
	   :type rectangle
	   :initform (make-instance 'rectangle)
	   :initarg :bounds
	   :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
		:type colour
		:initarg :colour
		:initform (random-colour)
		:documentation "The flat body colour of the form."))
  (:documentation "A form drawn in the drawing."))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defmethod first-point ((the-form form))
  "Get the first point in the outline of the form."
  (aref (points (outline the-form))
	0))

(defmethod point-count ((the-form form))
  "The number of points in the outline of the form."
  (length (points (outline the-form))))

(defmethod most-recent-point ((the-form form))
  "The most recent point added to the outline of the form."
  (aref (points (outline the-form))
	(- (point-count the-form) 
	   1))) 

(defmethod make-form-start-point ((form-skeleton vector))
  "Get the point to start drawing at."
  (let ((start-point nil))
    (dovector (skel form-skeleton)
      (let* ((hp (highest-leftmost-point skel))
	     (candidate (make-instance 'point 
				       :x (x hp)
				       :y (+ (y hp) pen-distance))))
	(when (or (not start-point)
		  (> (y candidate) (y start-point))
		  (and (= (y candidate) (y start-point))
		       (< (x candidate) (x start-point))))
	  (setf start-point candidate))))
    start-point))

(defmethod make-form-pen ((form-skeleton vector))
  "Make the pen to draw around the form."
  (make-instance 'turtle 
		 :location (make-form-start-point form-skeleton)
		 :turn-step pen-turn-step
		 :move-step pen-forward-step))

(defmethod make-form ((bounds rectangle) (num-points integer))
  "Make a form, ready to be started."
  (advisory-message (format nil "Form: ~d points.~%" num-points))
  (let* ((skel (make-random-polyline-in-rectangle bounds num-points))
	 (the-form (make-instance 'form
				    :skeleton (vector skel)
				    :bounds (bounds skel))))
    the-form))

(defmethod path-ready-to-close ((the-form form) (the-pen turtle))
  (and (> (point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-form)
		    (first-point the-form))
	  (move-step the-pen))))

(defmethod path-timeout ((the-form form))
  "Make sure that a failure of the form algorithm hasn't resulted in a loop."
  (> (point-count the-form)
     form-step-limit))

(defmethod should-finish-form ((the-form form) (the-pen turtle))
  "Decide whether the form should finish."
  (or (path-ready-to-close the-form the-pen)
      (path-timeout the-form)))

(defmethod next-pen-distance ((the-form form) (the-pen turtle))
  "How far the pen will be from the guide shape when it next moves forwards."
  (let ((dist 999999.0)
	(p (next-point the-pen)))
    (dovector (skel (skeleton the-form))
      (let ((new-dist (distance p skel)))
	(when (< new-dist dist)
	  (setf dist new-dist))))
    dist))

(defmethod next-pen-too-close ((the-form form) (the-pen turtle))
  "Will the pen move to be too close from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- (next-pen-distance the-form the-pen)
	pen-distance)))

(defmethod next-pen-too-far ((the-form form) (the-pen turtle))
  "Will the pen move to be too far from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- pen-distance
	(next-pen-distance the-form the-pen))))
     
(defmethod ensure-next-pen-far-enough ((the-form form) (the-pen turtle))
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close the-form the-pen)
     do (left the-pen)))
     
(defmethod ensure-next-pen-close-enough ((the-form form) (the-pen turtle))
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far the-form the-pen)
     do (right the-pen)))
    
(defmethod adjust-next-pen ((the-form form) (the-pen turtle))
  "Set the pen back on the correct path around the shape."
  (ensure-next-pen-far-enough the-form the-pen)
  (ensure-next-pen-close-enough the-form the-pen))

(defmethod draw-form ((the-form form))
  "Find the next point forward along the drawn outline of the shape."
  (let* ((form-bounds (bounds the-form))
	 (the-outline (outline the-form))
	 (the-pen (make-form-pen (skeleton the-form))))
    (append-point (outline the-form) (location the-pen))
    (loop until (should-finish-form the-form the-pen)
       do (adjust-next-pen the-form the-pen)
	 (forward the-pen)
	 (let ((new-location (location the-pen)))
	   (append-point the-outline new-location)
	   (include-point form-bounds new-location)))))

(defmethod mark-form-cells (cells fm fig)
  "Mark the cells belonging to the form"
  (mark-polyline-outline-cells cells (outline fm) fig)
  (mark-polyline-fill-cells cells (outline fm) fig))
