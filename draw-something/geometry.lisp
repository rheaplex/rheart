;;  geometry.lisp - Basic geometry stuff.
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

(defconstant radian (* pi 2.0)
  "One radian.")

(defconstant %radians-to-degrees (/ radian 360.0)
  "The value to multiple radians by to get degrees.")

(defmethod radians-to-degrees (radians)
  "Convert a value in radians to a huamn-readable value in degrees. :-)"
  (/ radians %radians-to-degrees))

(defclass point ()
  ((x :accessor x 
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the point.")
   (y :accessor y
      :initform 0.0
      :initarg :y
      :documentation "The y co-ordinate of the point."))
  (:documentation "A simple cartesian point on the picture plane (or page). 
                   y goes up"))

(defmethod equals ((a point) (b point))
  "Check if two points are equal"
  (and (= (x a) (x b))
       (= (y a) (y b))))

(defclass line ()
  ((from :accessor from
	 :initform (make-instance 'point)
	 :initarg :from
	 :documentation "The start of the line.")
   (to :accessor to
	 :initform (make-instance 'point)
	 :initarg :to
	 :documentation "The end of the line."))
   (:documentation "A simple line (segment) between two points."))

(defclass rectangle ()
  ((x :accessor x 
      :initform  0.0
      :initarg :x
      :documentation "The lower left x co-ordinate of the rectangle.")
   (y :accessor y
      :initform 0.0
      :initarg :y
      :documentation "The lower left y co-ordinate of the rectangle.")
   (width :accessor width
      :initform  0.0
      :initarg :width
      :documentation "The width of the rectangle.")
   (height :accessor height
      :initform 0.0
      :initarg :height
      :documentation "The height of the rectangle."))
  (:documentation "A simple rectangle"))

(defclass polyline ()
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ((points :accessor points
	   :initform (make-array 1 
				 :adjustable t
				 :fill-pointer 0)
	   :initarg :points
	   :documentation "The points of the polyline"))
  (:documentation "A polyline or polygon. A series of joined line (segments)."))

(defmethod equals ((left polyline) (right polyline))
  "Slowly and laboriously check two polylines for sameness."
  ;; If the objects are the same, the polylines are equal
  (if (= left right)
      t
      ;; If the polylines are different lengths, they can't be the same
      (if (not (eq (length left)
		   (length right)))
	  nil
	  (let ((same t)
		(points1 (points left))
		(points2 (points left)))
	    (dotimes (i (length left))
		     (when (not (= (aref points1 i)
				   (aref points2 i)))
		       (setf same nil)
		       (return)))
	    same))))

(defclass circle ()
  ((x :accessor x 
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the centre of the circle.")
   (y :accessor y
      :initform 0.0
      :initarg :y
      :documentation "The lower y co-ordinate of the centre of the circle.")
   (radius :accessor radius
      :initform  0.0
      :initarg :radius
      :documentation "The radius (distance from the middle to the edge) 
                      of the circle."))
  (:documentation "A simple circle."))

(defmethod origin ((the-circle circle))
  "The origin (centre) of the circle."
  (make-instance 'point :x (x the-circle) :y (y the-circle)))

(defmethod distance ((from point) (to circle))
  "The distance from a point to a circle. If the point is inside the circle the distance will be negative."
  (- (distance from (origin to)) (radius to)))

(defmethod top-leftmost-point ((the-circle circle))
  "The highest point of the circle."
  (make-instance 'point 
		 :x (x (origin the-circle))
		 :y (+ (y (origin the-circle)) 
		       (radius the-circle))))

(defmethod distance ((left point) (right point))
  "The distance between two points."
  (sqrt (+ (expt (- (x right) (x left)) 2)
	   (expt (- (y right) (y left)) 2))))

;(deftest
;  (let ((a (make-instance 'point :x 0.0 :y 0.0))
;	(b (make-instance 'point :x 1.0 :y 1.0)))
;    (test 5.0 (distance a b))
;    (test 5.0 (distance b a))
;    (test 0.0 (distance a a))
;    (test 0.0 (distance b b))))
	

(defmethod highest-leftmost-point (the-points)
  "The highest point, or highest and leftmost point (if several are highest)."
  (let ((highest nil))
    (dotimes (i (length the-points))
	(let ((pt (aref the-points i)))
	  (if (not highest)
	      (setf highest pt)
	      (setf highest (highest-leftmost-of pt highest)))))
    highest))

(defmethod highest-leftmost-of ((p1 point) (p2 point))
  "Compare and return the highest leftmost point."
  (if (or (> (y p1) (y p2))
	  (and (= (y p1) (y p2))
	       (< (x p1) (x p2)))) 
      p1  
    p2))

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

(defmethod distance-points ((p point) (from point) (to point))
  "The distance between a point and a line."
  (distance p (nearest-point-on-line-points p from to)))

;; Replace with a closer-than, optimise so we break on closer?

(defmethod distance ((p point) (poly polyline))
  "The distance from a point to a polyline."
  (let* ((distance-to-poly nil)
	 (pts (points poly))
	 (num-pts (length pts)))
    (do ((i 1 (+ i 1)))
	((= i num-pts))
      (let ((d (distance-points p 
				(aref pts (- i 1))
				(aref pts i))))
	(if (or (not distance-to-poly) 
		(< d distance-to-poly)) 
	    (setf distance-to-poly d))))
    distance-to-poly))

;;(deftest
;;  (let ((poly (make-instance 
;;	       'polyline 
;;	       :points (list (make-instance 'point :x 0.0 :y 0.0)
;;			     (make-instance 'point :x 0.0 :y 10.0)
;;			     (make-instance 'point :x 10.0 :y 10.0)
;;			     (make-instance 'point :x 10.0 :y 0.0))))
;;	(e (make-instance 'point :x 5.0 :y 5.0))
;;	(f (make-instance 'point :x 5.0 :y 100.0)))
;;  (test 5.0 (distance e poly))
;;    (test 90.0 (distance f poly))))

(defmethod point-line-side (p0 p2 p1)
  "Find out which side of an infinite line through p1 and p2 that p0 lies on.
   < 0 = left, > 0 = right, == 0 = exactly on."
 (- (* (- (x p1) (x p0)) (- (y p2) (y p0)))
    (* (- (x p2) (x p0)) (- (y p1) (y p0)))))

(defmethod furthest-point (p points)
  "Return the point that is furthest from p"
  (let ((candidate nil)
	(candidate-distance -1.0))
    (dolist (pp points)
      (let ((ppd (distance p pp)))
	(when (> ppd candidate-distance)
	  (setf candidate pp)
	  (setf candidate-distance ppd))))
    candidate))

;;(deftest
;;  (let ((a (make-instance 'point :x 1 :y 5))
;;	(b (make-instance 'point :x -1 :y 55))
;;	(c (make-instance 'point :x 10 :y 555))
;;	(d (make-instance 'point :x -10 :y 5555))
;;	(e (make-instance 'point :x 100 :y 55555)))
;;	(test e (furthest-point a (list d a b e c)))
;;	(test e (furthest-point a (list c e d b)))
;;	(test a (furthest-point e (list b e d a c)))
;;	(test a (furthest-point e (list b a d c)))))

(defmethod all-points-leftp (p q the-points)
  "Are all points to the left of or colinear with pq?"
  (let ((is-left t))
  (dolist (pp the-points)
    (when (> (point-line-side pp p q) 0)
      (setf is-left nil)
      (return)))
  is-left))

;;(deftest
;;  (let* ((from (make-instance 'point :x 1000 :y 5))
;;	(to (make-instance 'point :x 1000 :y 55555))
;;	(the-points (list (make-instance 'point :x -1 :y 55)
;;			  (make-instance 'point :x 10 :y 555)
;;			  (make-instance 'point :x -10 :y 5555)))
;;	(the-points-with-colinear (cons (make-instance 'point :x 1000 :y 555)
;;					the-points)))
;;  (test t (all-points-leftp from to the-points))
;;    (test nil (all-points-leftp to from the-points) )    
;;    (test t (all-points-leftp from to the-points-with-colinear))
;;    (test nil (all-points-leftp to from the-points-with-colinear))))

(defmethod point-with-all-left (p points)
  "Return the point q that all other points lie to the left of the line pq.
   In the case of colinear points, returns the furthest point."
  (let ((candidates '()))
    (dolist (candidate points)
	(if (all-points-leftp p candidate points)
	    (setf candidates (cons candidate candidates))))
  (furthest-point p candidates)))

;;(deftest
;;  (let* ((from (make-instance 'point :x 1000 :y 5))
;;	(to (make-instance 'point :x 1000 :y 55555))
;;	(the-points (list (make-instance 'point :x -1 :y 55)
;;			  (make-instance 'point :x 10 :y 555)
;;			  to
;;			  (make-instance 'point :x -10 :y 5555)))
;;	(the-points-with-colinear (cons (make-instance 'point :x 1000 :y 555)
;;					the-points)))
;;  (test to (point-with-all-left from the-points))  
;;    (test to (point-with-all-left from the-points-with-colinear))))

(defmethod convex-hull (the-points)
  "Get the convex hull of an array of points."
  (let* ((first-point (highest-leftmost-point the-points))
	 (current-point first-point)
	 (next-point nil)
	 (hull (list first-point)))
    (loop until (and (not (eq next-point nil))
		     (equals next-point first-point))
       do (setf next-point 
		(point-with-all-left current-point the-points))
	 (push next-point hull)
	 (setf current-point next-point))
    (make-instance 'polyline :points hull)))

;;(deftest
;;  (let* ((a (make-instance 'point :x 0 :y 0))
;;	 (b (make-instance 'point :x 0 :y 10))
;;	 (c (make-instance 'point :x 10 :y 10))
;;	 (d (make-instance 'point :x 10 :y 0))
;;	 (e (make-instance 'point :x 5 :y 10))
;;	 (the-points (list c a d b))
;;	 (expected-result (make-instance 'polyline :points (list d c b a)))
;;	 (the-points-with-colinear (cons e the-points)))
;;  (test expected-result (convex-hull the-points) :test #'equals)
;;  (test expected-result (convex-hull the-points-with-colinear) :test #'equals)))


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