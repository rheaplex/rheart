;;  polyline.lisp - A classic computer graphics polyline.
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

(defclass polyline ()
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ;; For speed set initial dimension to a size unlikely to need increasing
  ((points :accessor points
	   :initform (make-array 1000 
				 :adjustable t
				 :fill-pointer 0)
	   :initarg :points
	   :documentation "The points of the polyline"))
  (:documentation "A polyline or polygon. A series of joined line segments."))

(defmethod append-point ((poly polyline) (pt point))
  "Append a point to the polyline."
  (vector-push-extend pt
		      (points poly)))

(defmethod make-random-polyline-in-rectangle (rect count)
  "Create a polyline with the given number of points in the given bounds."
  (let ((poly (make-instance 'polyline)))
    (dotimes (i count)
      (append-point poly
		    (random-point-in-rectangle rect)))
    poly))

(defmethod distance ((p point) (poly polyline))
  "The distance from a point to a polyline."
  (let ((distance-to-poly nil)
	(pts (points poly)))
    (do ((i 1 (+ i 1)))
	((= i (length pts)))
      (let ((d (distance-point-line p 
				    (aref pts (- i 1))
				    (aref pts i))))
	(if (or (not distance-to-poly) 
		(< d distance-to-poly)) 
	    (setf distance-to-poly d))))
    distance-to-poly))

(defmethod highest-leftmost-point ((poly polyline))
  "The highest point, or highest and leftmost point (if several are highest)."
  (let* ((the-points (points poly))
	 (highest (aref the-points 
			0)))
    (dotimes (i (length the-points))
      (let ((pt (aref the-points i)))
	(if (or (> (y pt) (y highest))
		(and (= (y highest) (y pt))
		     (< (x highest) (x pt)))) 
	    (setf highest pt))))
    highest))
