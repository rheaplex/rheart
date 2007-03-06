;;  rectangle.lisp - A 2D rectangle.
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

;;(in-package "DRAW-SOMETHING")

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

(defmethod copy-rectangle ((r rectangle))
  "Make a copy of the rectangle."
  (make-instance 'rectangle 
		 :x (x r) :y (y r) :width (width r) :height (height r)))

(defmethod random-point-in-rectangle ((bounds-rect rectangle))
  "Make a point placed randomly within the given bounding rectangle."
  (make-instance 'point 
		 :x (+ (x bounds-rect) (random (width bounds-rect)))
		 :y (+ (y bounds-rect) (random (height bounds-rect)))))

(defmethod random-points-in-rectangle ((bounds-rect rectangle) count)
  "Create the specified number of points placed randomly within the given rectangle."
  (map-into (make-vector count)
	    (lambda ()(random-point-in-rectangle bounds-rect))))

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

(defmethod random-rectangle-in-rectangle-size (in new-width new-height)
  "Make a random rectangle of the given size in the given bounds."
  (assert (<= new-width (width in)))
  (assert (<= new-height (height in)))
  (let ((new-x (+ (x in)
		  (random-number (- (width in) new-width))))
	(new-y (+ (y in)
		  (random-number (- (height in) new-height)))))
    (make-instance 'rectangle
		   :x new-x
		   :y new-y
		   :width new-width
		   :height new-height)))
  
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

(defmethod area ((rect rectangle))
  "Get the rectangle's area."
  (* (width rect) (height rect)))

(defmethod contains-point-co-ordinates ((rect rectangle) x y)
  "Find whether the rectangle contains the point."
  (if (and (> x (y rect))
	   (< x (+ (x rect) (width rect)))
	   (> y (y rect))
	   (< y (+ (y rect) (height rect))))
      t
      nil))

(defmethod contains ((rect rectangle) (p point))
  "Find whether the rectangle contains the point."
  (contains-point-co-ordinates rect (x p) (y p)))

(defmethod points-in-rectangle ((rect rectangle) (points vector))
  "Get the vector of points within the rectangle"
  (let ((contained (vector 1)))
    (loop for p across points
	  when (contains rect p)
	  do (vector-push-extend contained p))
    contained))

(defmethod intersects ((rect1 rectangle) (rect2 rectangle))
  "Find whether the rectangles intersect."
  (and (< (x rect1) (+ (x rect2) (width rect2)))
       (< (y rect1) (+ (y rect2) (height rect2)))
       (> (+ (x rect1) (width rect1)) (x rect2))
       (> (+ (y rect1) (height rect1)) (y rect2))))

(defmethod include-point ((rect rectangle) (p point))
  "Destructively expand the rectangle to include the point."
  (let ((right (+ (x rect) (width rect)))
	(top (+ (y rect) (height rect))))
    (cond
      ((< (x p) (x rect)) 
       (setf (width rect) 
	     (+ (width rect) (- (x rect) (x p))))
       (setf (x rect) (x p)))
      ((> (x p) right) 
       (setf (width rect) 
	     (+ (width rect) (- (x p) right))))
      ((< (y p) (y rect)) 
       (setf (height rect) 
	     (+ (height rect) (- (y rect) (y p))))
       (setf (y rect) (y p)))
      ((> (y p) top) 
       (setf (height rect) 
	     (+ (height rect) (- (y p) top)))))))

(defmethod include-rectangle ((rect rectangle) (include rectangle))
  "Expand the first rectangle to include the second."
  (include-point rect (make-instance 'point :x (x include) :y (y include)))
  (include-point rect (make-instance 'point :x (x include) 
				     :y (+ (y include) (height include))))
  (include-point rect (make-instance 'point :x (+ (x include) (width include)) 
				     :y (+ (y include) (height include))))
  (include-point rect (make-instance 'point :x (+ (x include) (width include))
				     :y (y include))))

(defmethod rectangle-from-point ((p point))
  "Make a zero-size rectangle for a point."
  (make-instance 'rectangle :x (x p) :y (y p) :width 0.0 :height 0.0))

(defmethod random-point-on-rectangle ((r rectangle))
  "Make a random point somewhere on the border of a rectangle."
  (case (random 4)
    (0 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					    :x (x r)
					    :y (y r))
		       :to (make-instance 'point
					  :x (x r)
					  :y (+ (y r)
						(height r))))))
    (1 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					  :x (x r)
					  :y (+ (y r)
						(height r)))
		       :to (make-instance 'point
					  :x (+ (x r)
						(width r))
					  :y (+ (y r)
						(height r))))))
    (2 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					  :x (+ (x r)
						(width r))
					  :y (+ (y r)
						(height r)))
		       :to (make-instance 'point
					  :x (+ (x r)
						(width r))
					  :y (y r)))))
    (3 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					    :x (+ (x r)
						(width r))
					    :y (y r))
		       :to (make-instance 'point
					  :x (x r)
					  :y (y r)))))))
		       
(defmethod random-points-on-rectangle ((r rectangle) count)
  "Generate count points on a rectangle's outline. These will not be ordered."
  (map-into (make-vector count) (lambda () (random-point-on-rectangle r))))

(defmethod random-points-at-rectangle-corners ((r rectangle) count) 
  "Return from 0 to 4 corner points of a rectangle, clamping out-of-range."
  ;; Inefficient but easy to code and easy to read ;-]
  (choose-n-of count
	       (vector (make-instance 'point 
				      :x (x r) 
				      :y (y r))
		       (make-instance 'point 
				      :x (x r) 
				      :y (+ (y r) (height r)))
		       (make-instance 'point 
				      :x (+ (x r) (width r)) 
				      :y (+ (y r) (height r)))
		       (make-instance 'point 
				      :x (+ (x r) (width r)) 
				      :y (y r)))))