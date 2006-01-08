;;  rectangle.lisp - A 2D rectangle.
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

(defclass rectangle ()
  ((x :accessor x 
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The lower left x co-ordinate of the rectangle.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The lower left y co-ordinate of the rectangle.")
   (width :accessor width
      :type float
      :initform  0.0
      :initarg :width
      :documentation "The width of the rectangle.")
   (height :accessor height
      :type float
      :initform 0.0
      :initarg :height
      :documentation "The height of the rectangle."))
  (:documentation "A simple rectangle"))

(defmethod random-point-in-rectangle ((bounds-rect rectangle))
  "Make a point placed randomly within the given bounding rectangle."
  (make-instance 'point 
		 :x (+ (x bounds-rect) (random (width bounds-rect)))
		 :y (+ (y bounds-rect) (random (height bounds-rect)))))

(defmethod random-points-in-rectangle ((bounds-rect rectangle) count)
  "Create the specified number of points placed randomly within the given rectangle."
  (let ((points (make-array 0 
			    :adjustable t 
			    :fill-pointer 0)))
  (dotimes (i count)
    (vector-push-extend (random-point-in-rectangle bounds-rect) points))
  points))

(defmethod random-rectangle-in-rectangle ((bounds-rect rectangle))
  "Make a random rectangle of at least size 1x1 in another rectangle."
  (let* ((new-width (random (width bounds-rect)))
	 (new-height (random (height bounds-rect)))
	 (new-x (+ (x bounds-rect)
		   (random (- (width bounds-rect) new-width))))
	 (new-y (+ (y bounds-rect)
		   (random (- (height bounds-rect) new-height)))))
    (make-instance 'rectangle
		   :x new-x
		   :y new-y
		   :width new-width
		   :height new-height)))

(defmethod inset-rectangle ((source rectangle) (offset real))
  "Trim a rectangle by the given amount."
  (make-instance 'rectangle
		 :x (+ (x source) offset)
		 :y (+ (y source) offset)
		 :width (- (width source) (* offset 2.0))
		 :height (- (height source) (* offset 2.0))))