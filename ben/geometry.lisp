(defconstant 2pi (* pi 2))
(defconstant -pi (- pi))

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
