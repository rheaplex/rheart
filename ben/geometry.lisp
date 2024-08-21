;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapl2 (list fun)
  "Apply fun to successive pairs of values from list. Only takes one list."
  (when (cadr list)
    (funcall fun (car list) (cadr list))
    (mapl2 (cddr list) fun)))

(defconstant 2pi (* pi 2))
(defconstant -pi (- pi))

(defconstant radian (* pi 2))
(defconstant radian/3/4 (/ (* pi 3) 2))
(defconstant radian/2 pi)
(defconstant radian/4 (/ pi 2))

(defun normalize-relative-angle (angle)
  "Normalise a relative angle in radians to the range [-pi,+pi[."
  (let ((trimmed-angle (mod angle 2pi)))
    (if (>= trimmed-angle 0)
        (if (< trimmed-angle pi)
            trimmed-angle
            (- trimmed-angle 2pi))
        (if (>= trimmed-angle -pi)
            trimmed-angle
            (+ trimmed-angle 2pi)))))

(defun bearing-to-position (x y heading target-x target-y)
  "Find the bearing from x,y,heading(radians, relative) to target-x,target-y."
  (normalize-relative-angle (- (atan (- target-y y) (- target-x x))
                               heading)))

(defun make-point (x y)
  (cons x y))

(defun point-x (item)
  (car item))

(defun point-y (item)
  (cdr item))

(defun point-distance (from to)
  "The distance between two points."
  (abs (sqrt (+ (expt (- (point-x to) (point-x from)) 2)
                (expt (- (point-y to) (point-y from)) 2)))))

(defun offset-point-along-direction (p direction amount)
  ;; Direction is radians anticlockwise from the positive x axis
  (make-point (+ (point-x p)
                 (* amount (cos direction)))
              (+ (point-y p)
                 (* amount (sin direction)))))

(defun angle-between-points (from to)
  "Calculate the angle of the second point around the first, 0..2pi from pos x."
  (atan (- (point-y to) (point-y from))
        (- (point-x to) (point-x from))))

(defun line-length (from to)
  "The length of the line between the two points."
  (abs (point-distance from to)))

(defstruct rectangle
  x
  y
  width
  height)

(defun rectangle-left (rect)
  (rectangle-x rect))

(defun rectangle-top (rect)
  (+ (rectangle-y rect) (rectangle-height rect)))

(defun rectangle-right (rect)
  (+ (rectangle-x rect) (rectangle-width rect)))

(defun rectangle-bottom (rect)
  (rectangle-y rect))

(defun co-ordinates-in-rectangle-p (x y r)
  (and (>= x (rectangle-x r))
       (>= y (rectangle-y r))
       (< x (+ (rectangle-x r) (rectangle-width r)))
       (< y (+ (rectangle-y r) (rectangle-height r)))))

(defun rectangle-include-point (rect p)
  "Destructively expand the rectangle to include the point."
  (let ((right (+ (rectangle-x rect) (rectangle-width rect)))
        (top (+ (rectangle-y rect) (rectangle-height rect))))
    (cond
      ((< (point-x p) (rectangle-x rect))
       (setf (rectangle-width rect)
             (+ (rectangle-width rect) (- (rectangle-x rect) (point-x p))))
       (setf (rectangle-x rect) (point-x p)))
      ((> (point-x p) right)
       (setf (rectangle-width rect)
             (+ (rectangle-width rect) (- (point-x p) right)))))
    (cond
      ((< (point-y p) (rectangle-y rect))
       (setf (rectangle-height rect)
             (+ (rectangle-height rect) (- (rectangle-y rect) (point-y p))))
       (setf (rectangle-y rect) (point-y p)))
      ((> (point-y p) top)
       (setf (rectangle-height rect)
             (+ (rectangle-height rect) (- (point-y p) top))))))
  rect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon Fill.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-apply-span (x1 x2 y fun)
  "Apply fun to the pixels between x1 and x2 on row y."
  (loop for x from x1 below x2
     do (funcall fun x y)))

(defun fill-between-node-pairs (nodes y fun)
  "Walk the list of node start/end pairs on row y, calling fun between them."
  (mapl2 nodes
         (lambda (x1 x2)
           (fill-apply-span x1 x2 y fun))))

(defun fill-line-intersects-row (p previous y)
  "Does the line between p and previous pass through row y?"
  (or (and (< (point-y p) y)
           (>= (point-y previous) y))
      (and (< (point-y previous) y)
           (>= (point-y p) y))))

(defun fill-line-row-intersection-column (p previous y)
  "The x position where line p->previous intersects row y (assuming it does)."
  ;; The flooring is needed to match the drawn outline, which is also floored.
  (let ((px (floor (point-x p)))
	(py (floor (point-y p)))
	(prevx (floor (point-x previous)))
	(prevy (floor (point-y previous))))
    (floor (+ px
	      (* (/ (- y py)
		    (- prevy py))
		 (- prevx px))))))

(defun fill-build-node-list (points y)
  "Build a list of positions where the path created by points intersects y."
  (let ((nodes '())
        (previous (car (last points))))
    (dolist (p points)
      (when (fill-line-intersects-row p previous y)
        (push (fill-line-row-intersection-column p previous y)
              nodes))
      (setf previous p))
    (sort nodes #'<)))

(defun fill-scanline (y points fun)
  "Call fun for each x, y coord on row y inside the path described by points."
  (let ((nodes (fill-build-node-list points y)))
    (fill-between-node-pairs nodes y fun)))

(defun fill-polygon (points bounds fun)
  "Call fun on each x, y coord inside path of points with cached bounds rect."
    (loop for y from (rectangle-bottom bounds) below (rectangle-top bounds)
       do (fill-scanline y points fun)))
