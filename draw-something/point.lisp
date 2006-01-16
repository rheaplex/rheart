;;  point.lisp - A 2D point.
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

(defmethod random-point-in-bounds (x y width height)
  "Make a point placed randomly within the given bounds."
  (make-instance 'point 
		 :x (+ x (random width))
		 :y (+ y (random height))))

(defmethod translate-point ((p point) by-x by-y)
  "Make a translated copy of the point."  
  (make-instance 'point 
		 :x (+ (x p) by-x)
		 :y (+ (y p) by-y)))
