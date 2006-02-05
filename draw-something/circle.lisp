;;  circle.lisp - A 2D circle.
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

(defclass circle ()
  ((x :accessor x 
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the circle centre.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The y co-ordinate of the circle centre.")
   (radius :accessor radius
      :type float
      :initform  0.0
      :initarg :radius
      :documentation "The radius of the circle."))
  (:documentation "A simple circle."))

(defmethod distance ((p point) (c circle))
  "The distance from a point to a polyline. Negative is inside circle."
  (- (distance-point-co-ordinates p (x c) (y c))
	  (radius c)))

(defmethod highest-leftmost-point ((c circle))
  "The highest point of the circle."
  (make-instance 'point :x (x c) :y (+ (y c) (radius c))))