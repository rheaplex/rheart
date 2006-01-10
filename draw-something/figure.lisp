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

(defconstant pen-distance 5.0)
(defconstant pen-distance-tolerance 1.4)
(defconstant pen-forward-step 2.0)
(defconstant pen-turn-step 0.01)

(defconstant pen-width 1.0)

(defconstant figure-step-limit 5000)

(defclass figure ()
  ((skeleton :accessor skeleton 
	     :type polyline
	     :initarg :skeleton
	     :documentation "The guide shape for the outline.")
   (outline :accessor outline
	    :type polyline
	    :initform (make-instance 'polyline)
	    :documentation "The outlines for the skeleton.")
   (pen :accessor pen
	:type turtle
	:initarg :pen
	:documentation "The pen drawing around the skeleton. Make sensible.")
   (figure-colour :accessor figure-colour
		  :type colour
		  :initarg :colour
		  :initform (random-colour)
		  :documentation "The flat body colour of the figure."))
  (:documentation "A figure drawn in the drawing."))

;; Configure pen for each figure (pos, reset heading)
;; Vary distance & tolerance & pen width for each figure?
;; Skeleton will ultimately be a list of objects
;; And may be complemented by an expanded version as a lineset
;; Outline likewise may consist of a list of polylines

(defmethod first-point ((the-figure figure))
  "Get the first point in the outline of the figure."
  (aref (points (outline the-figure))
	0))

(defmethod point-count ((the-figure figure))
  "The number of points in the outline of the figure."
  (length (points (outline the-figure))))

(defmethod most-recent-point ((the-figure figure))
  "The most recent point added to the outline of the figure."
  (aref (points (outline the-figure))
	(- (point-count the-figure) 
	   1))) 

(defmethod make-figure-start-point ((figure-skeleton polyline))
  "Get the point to start drawing at."
  (let ((start-point (highest-leftmost-point figure-skeleton)))
    (make-instance 'point 
		   :x (x start-point)
		   :y (+ (y start-point) 
			 pen-distance))))

(defmethod make-figure-pen ((figure-skeleton polyline))
  "Make the pen to draw around the figure."
  (make-instance 'turtle 
		 :location (make-figure-start-point figure-skeleton)
		 :turn-step pen-turn-step
		 :move-step pen-forward-step))

(defmethod make-figure ((bounds rectangle) (num-points real))
  "Make a figure, ready to be started."
  (advisory-message (format nil "Figure: ~d points.~%" num-points))
  (let* ((skel (make-random-polyline-in-rectangle bounds num-points))
	 (the-figure (make-instance 'figure
				    :pen (make-figure-pen skel)
				    :skeleton skel)))
    (append-point (outline the-figure) 
		  (location (pen the-figure)))
    the-figure))

(defmethod path-ready-to-close ((the-figure figure))
  "Would figure the next section bring us close enough to the start of the path that we should close the path?"
  (and (> (point-count the-figure) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-figure)
		    (first-point the-figure))
	  (move-step (pen the-figure)))))

(defmethod path-timeout ((the-figure figure))
  "Make sure that a failure of the figure algorithm hasn't resulted in a loop."
  (> (point-count the-figure)
     figure-step-limit))

(defmethod should-finish ((the-figure figure))
  "Decide if the figure should finish."
  (or (path-ready-to-close the-figure)
      (path-timeout the-figure)))

(defmethod next-pen-distance ((the-figure figure))
  "How far the pen will be from the guide shape when it next moves forwards."
  (distance (next-point (pen the-figure)) 
	    (skeleton the-figure)))

(defmethod next-pen-too-close ((the-figure figure))
  "Will the pen move to be too close from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- (next-pen-distance the-figure)
	pen-distance)))

(defmethod next-pen-too-far ((the-figure figure ))
  "Will the pen move to be too far from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- pen-distance
	(next-pen-distance the-figure))))
     
(defmethod ensure-next-pen-far-enough ((the-figure figure))
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close the-figure)
     do (left (pen the-figure))))
     
(defmethod ensure-next-pen-close-enough ((the-figure figure))
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far the-figure)
     do (right (pen the-figure))))
    
(defmethod adjust-next-pen ((the-figure figure))
  "Set the pen back on the correct path around the shape."
  (ensure-next-pen-far-enough the-figure)
  (ensure-next-pen-close-enough the-figure))

(defmethod draw-figure ((the-figure figure))
  "Find the next point forward along the drawn outline of the shape."
  (loop until (should-finish the-figure)
     do (adjust-next-pen the-figure)
       (forward (pen the-figure))
       (append-point (outline the-figure)
		     (location (pen the-figure)))))
