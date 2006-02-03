;;  line.lisp - A 2D line Segment, and utilities on points and lines.
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

(defclass line ()
  ((from :accessor from
	 :type point
	 :initform (make-instance 'point)
	 :initarg :from
	 :documentation "The start of the line.")
   (to :accessor to
       :type point
       :initform (make-instance 'point)
       :initarg :to
       :documentation "The end of the line."))
   (:documentation "A simple line (segment) between two points."))

;;        nearest_point_on_line
;;        From "Crashing Into the New Year", 
;;        Jeff Lander, GD Magazine, Jan 1999.
;;        From point t to line A = p1->p2, B is t -> p1, C is t ->p2, 
;;        n is nearest point on A to t
;;                 (p2 - p1) * (B o A)
;;        n = p1 + -------------------
;;                  (B o A) + (C o A)
;;        [o is the dot product sign]
;;        Note: Cull out-of-range points on the bounding circle of the 
;;        line for testing groups of lines to find closest.
;; This needs decomposing into smaller, more manageable and meaningful units

(defmethod nearest-point-on-line ((p point) (l line)) ;;la lb)
  (nearest-point-on-line-points p (from l) (to l)))

(defmethod nearest-point-on-line-points ((p point) (la point) (lb point))
  (nearest-point-on-line-coordinates (x p) (y p) (x la) (y la) (x lb) (y lb)))

(defmethod nearest-point-on-line-coordinates (xp yp xla yla xlb ylb)
  "Get the nearest point on a line"
  ;; Optimised to avoid point accessors
  (let ((dot-ta (+ (* (- xp xla) (- xlb xla))
		 (* (- yp yla) (- ylb yla)))))
    ;;(format t "~F%~%" dot-ta)
    (if (<= dot-ta 0.0)
	(make-instance 'point :x xla :y yla)
	;; else
	(let ((dot-tb (+ (* (- xp xlb) (- xla xlb)) 
			 (* (- yp ylb) (- yla ylb)))))
	  ;;(format t "~F%~%" dot-tb)
	  (if (<= dot-tb 0.0)
	      (make-instance 'point :x xlb :y ylb)
	      ;; else      
	      (make-instance 'point
			     :x (+ xla 
				   (/ (* (- xlb xla) dot-ta) 
				  (+ dot-ta dot-tb)))
			     :y (+ yla
				   (/ (* (- ylb yla) dot-ta) 
				      (+ dot-ta dot-tb)))))))))

(defmethod distance ((p point) (l line))
  "The distance between a point and a line."
  (distance p (nearest-point-on-line p l)))

(defmethod distance-point-line ((p point) (from point) (to point))
  "The distance between a point and a line."
  (distance p (nearest-point-on-line-points p from to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-line intersection
;; http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
;; Returns the time where the second line intersects the first line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod lines-intersect-co-ordinates (p1x p1y p2x p2y ;; First line 
					 p3x p3y p4x p4y);; Second line 
  "Find whether the two lines, expressed as 8 co-ordinates, intersect."
  (let ((denominator (- (* (- p4y p3y)
			   (- p2x p1x)) 
			(* (- p4x p3x)
			   (- p2y p1y)))))
    (if (= denominator 0.0)
	nil ;; Parallel lines
	(let ((ua (/ (- (* (- p4x p3x) 
			   (- p1y p3y))
			(* (- p4y p3y) 
			   (- p1x p3x)))
		     denominator))
	      (ub (/ (- (* (- p2x p1x) 
			   (- p1y p3y)) 
			(* (- p2y p1y) 
			   (- p1x p3x))) 
		     denominator)))
	  (if (and (>= ua 0.0)
		   (<= ua 1.0)
		   (>= ub 0.0)
		   (<= ub 1.0)) ;; Intersection (or not)
	      ua
	      nil)))))

(defmethod lines-intersect-points ((l1p1 point) (l1p2 point)
				   (l2p1 point) (l2p2 point))
  "Find whether the two lines, expressed as 4 points intersect."
  (lines-intersect-co-ordinates (x l1p1) (y l1p1) (x l1p2) (y l1p2)
				(x l2p1) (y l2p1) (x l2p2) (y l2p2)))


(defmethod line-intersect-line-points ((l1 line)
				       (l2p1 point) (l2p2 point))
  "Find whether the two lines, the second expressed as 2 points intersect."
  (lines-intersect-points (from l1) (to l1) l2p1 l2p2))

(defmethod intersect ((l1 line) (l2 line))
  "Find whether the two lines intersect."
  (lines-intersect-vertices (from l1) (to l1) (from l2) (to l2)))