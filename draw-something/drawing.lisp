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

(defconstant pen-distance 5.0)
(defconstant pen-distance-tolerance 1.5)
(defconstant pen-forward-step 2.0)
(defconstant pen-turn-step 0.02)

(defconstant drawing-step-limit 5000)

(defclass drawing ()
  ((pen :accessor pen
	:type turtle
	:initarg :pen
	:documentation "The pen drawing around the outline.")
   (skeleton :accessor skeleton 
	     :type polyline
	     :initarg :skeleton
	     :documentation "The guide shape for the outline.")
   (outline :accessor outline
	    :type polyline
	    :initform (make-instance 'polyline)
	    :documentation "The cached drawing outline."))
   (:documentation "A drawing in progress."))

(defmethod first-point ((the-drawing drawing))
  "Get the first point in the outline of the drawing."
  (aref (points (outline the-drawing))
	0))

(defmethod point-count ((the-drawing drawing))
  "The number of points in the outline of the drawing."
  (length (points (outline the-drawing))))

(defmethod most-recent-point ((the-drawing drawing))
  "The most recent point added to the outline of the drawing."
  (aref (points (outline the-drawing))
	(- (point-count the-drawing) 
	   1))) 

(defmethod make-drawing-start-point ((skeleton polyline))
  "Get the point to start drawing at."
  (let ((start-point (highest-leftmost-point skeleton)))
    (make-instance 'point 
		   :x (x start-point)
		   :y (+ (y start-point) 
			 pen-distance))))

(defmethod make-drawing-pen ((skeleton polyline))
  "Make the pen for the drawing."
  (make-instance 'turtle 
		 :location (make-drawing-start-point skeleton)
		 :turn-step pen-turn-step
		 :move-step pen-forward-step))

(defmethod make-drawing (x y width height num-points)
  "Make a drawing, ready to be started."
  (let ((skel (make-random-polyline-in-bounds x y width height num-points)))
    (let ((the-drawing (make-instance 'drawing
				      :skeleton skel
				      :pen (make-drawing-pen skel))))
      (append-point (outline the-drawing) 
		    (location (pen the-drawing)))
      the-drawing)))

(defmethod path-ready-to-close ((the-drawing drawing))
  "Would drawing the next section bring us close enough to the start of the path that we should close the path?"
  (and (> (point-count the-drawing) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-drawing)
		    (first-point the-drawing))
	  (move-step (pen the-drawing)))))

(defmethod path-timeout ((the-drawing drawing))
  "Make sure that a failure of the drawing algorithm hasn't resulted in a loop."
  (> (point-count the-drawing)
     drawing-step-limit))

(defmethod should-finish ((the-drawing drawing))
  "Decide if the drawing should finish."
  (or (path-ready-to-close the-drawing)
      (path-timeout the-drawing)))

(defmethod next-pen-distance ((the-drawing drawing))
  "How far the pen will be from the guide shape when it next moves forwards."
  (distance (next-point (pen the-drawing)) 
	    (skeleton the-drawing)))

(defmethod next-pen-too-close ((the-drawing drawing))
  "Will the pen move to be too close from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- (next-pen-distance the-drawing)
	pen-distance)))

(defmethod next-pen-too-far ((the-drawing drawing ))
  "Will the pen move to be too far from the guide shape next time?"
  (< (random pen-distance-tolerance)
     (- pen-distance
	(next-pen-distance the-drawing))))
     
(defmethod ensure-next-pen-far-enough ((the-drawing drawing))
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close the-drawing)
     do (left (pen the-drawing))))
     
(defmethod ensure-next-pen-close-enough ((the-drawing drawing))
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far the-drawing)
     do (right (pen the-drawing))))
    
(defmethod adjust-next-pen ((the-drawing drawing))
  "Set the pen back on the correct path around the shape."
  (ensure-next-pen-far-enough the-drawing)
  (ensure-next-pen-close-enough the-drawing))

(defmethod draw-step ((the-drawing drawing))
  "Find the next point forward along the drawn outline of the shape."
  (adjust-next-pen the-drawing)
  (forward (pen the-drawing))
  (append-point (outline the-drawing)
		(location (pen the-drawing)))
  (most-recent-point the-drawing))
