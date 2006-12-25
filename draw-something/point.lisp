;;  point.lisp - A 2D point.
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

(defclass point ()
  ((x :accessor x 
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the point.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The y co-ordinate of the point."))
  (:documentation "A simple cartesian point on the picture plane (or page). 
                   y goes up"))

(defmethod distance ((left point) (right point))
  "The distance between two points."
  (sqrt (+ (expt (- (x right) (x left)) 2)
	   (expt (- (y right) (y left)) 2))))

(defmethod distance-point-co-ordinates ((left point) x2 y2)
  "The distance between two points."
  (sqrt (+ (expt (- x2 (x left)) 2)
	   (expt (- y2 (y left)) 2))))

(defmethod distance-to-closest-point ((p1 point) &rest points)
  "Get the distance to the point closest to p1"
  (let ((dist nil))
    (dolist (p points)
      (let ((new-dist (distance p1 p)))
	(if (or (not dist) (< new-dist dist))
	    (setq dist new-dist))))
    dist))

(defun random-co-ordinates (x-range y-range)
  "Make random co-ordinates avoiding bell-curve point distribution."
  ;; Yay bignum!
  (floor (random (+ x-range y-range))
	 x-range))

(defmethod random-point-in-bounds (x y width height)
  "Make a point placed randomly within the given bounds."
  (multiple-value-bind
	(x-dist y-dist) (random-co-ordinates width height)
    (make-instance 'point 
		   :x (+ x x-dist)
		   :y (+ y y-dist))))

(defmethod translate-point ((p point) by-x by-y)
  "Make a translated copy of the point."  
  (make-instance 'point 
		 :x (+ (x p) by-x)
		 :y (+ (y p) by-y)))

(defmethod co-ordinates-at-angle-around-point-co-ordinates (a b r theta)
  "Get the point on the circumference of the circle at theta."
  (values (+ a (* r (cos theta)))
	    (+ b (* r (sin theta)))))

(defmethod co-ordinates-at-angle (obj theta)
  "Get the point on the circumference of the arc/circle at theta. 
   Doesn't check limits of arc."
  (co-ordinates-at-angle-around-point-co-ordinates (x obj) (y obj) (radius obj)
						   theta))
	
(defmethod angle-between-two-points-co-ordinates (x1 y1 x2 y2)
  "Calculate the angle of the second point around the first."
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
    (cond
      ((= dx 0.0)
       (cond
	 ((= dx 0.0) 0.0)
	 ((> dy 0.0) (/ pi 2.0))
	 (t (* pi 2.0 3.0))))
      ((= dy 0.0)
       (cond
	 ((> dx 0.0) 0.0)
	 (t pi)))
      (t
       (cond
	 ((< dx 0.0) (+ (atan (/ dy dx)) pi))
	 ((< dy 0.0) (+ (atan (/ dy dx)) (* pi 2)))
	 (t (atan (/ dy dx))))))))
					
(defmethod angle-between-two-points ((p1 point) (p2 point))
  "Calculate the angle of the second point around the first."
  (angle-between-two-points-co-ordinates (x p1) (y p1) (x p2) (y p2)))

(defmethod highest-leftmost-of ((p1 point) (p2 point))
  "Compare and return the highest leftmost point."
  (if (or (> (y p1) (y p2))
	  (and (= (y p1) (y p2))
	       (< (x p1) (x p2)))) 
      p1  
    p2))