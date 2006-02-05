;;  arc.lisp - A 2D circle segment.
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

(defclass arc ()
  ((x :accessor x 
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the arc's circle's centre.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The y co-ordinate of the arc's circle's centre.")
   (radius :accessor radius
      :type float
      :initform  0.0
      :initarg :radius
      :documentation "The radius of the arc's circle.")
   (start-angle :accessor start-angle
		:initarg :from)
   (finish-angle :accessor finish-angle
		 :initarg :to)
   (start-point :accessor start-point)
   (finish-point :accessor finish-point))
  (:documentation "A simple circle."))

(defmethod initialize-instance :after ((instance arc) &rest args)
  "Make the from- and to- points from their angles."
  (declare (ignore args))
  (setf (start-point instance) 
	(arc-point-at-angle instance (start-angle instance)))
  (setf (finish-point instance) 
	(arc-point-at-angle instance (finish-angle instance)))
  instance)

(defmethod highest-leftmost-point ((a arc))
  "The highest point of the arc."
  ;;FIXME
  (if (or (> (start-angle a) (* pi 1.5))
	  (< (finish-angle a) (* pi 1.5)))
      (highest-leftmost-of (start-point a) (finish-point a))
      (make-instance 'point :x (x a) :y (+ (y a) (radius a)))))

(defmethod distance ((p point) (a arc))
  "Only calculates distance to outside of arc, more a slice distance."
  (let ((theta (angle-between-two-points-co-ordinates (x p) (y p) (x a) (y a))))
    (if (and (> theta (start-angle a)) (< theta (finish-angle a)))
	(abs (- (distance-point-co-ordinates p (x a) (y a)) (radius a)))
	(distance-to-closest-point p (start-point a) (finish-point a)))))

(defmethod bounds ((a arc))
  "The bounds of the arc."
  (make-instance 'rectangle :x 0.0 :y 0.0 :width 100.0 :height 100.0)) 

(defmethod arc-point-at-angle ((obj arc) theta)
  "Get the point on the circumference of the arc at theta. 
   Doesn't check limits of arc."
  (multiple-value-bind (ax ay) (co-ordinates-at-angle obj theta)
      (make-instance 'point :x ax :y ay)))

;; Do point-to-angle && fit-curve-to-3-points