;;  Ben.lisp -  An toy reimplementation of Harold Cohen's AARON circa
;;              "What Is An Image".
;;  Copyright (C) 2008 Rhea Myers rhea@myers.studio
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; ben is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; IN PROGRESS
;; The line drawing algorithm from "on-purpose-line.lisp" needs importing


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-range (a b)
  "Make a random number from a to below b."
  (let ((range (- b a)))
    (if (= range 0)
        a
        (+ (random range) a))))

(defun choose-one-of (&rest possibilities)
  "Choose one of the given options."
  (nth (random (length possibilities)) possibilities))

(defun mapl2 (list fun)
  "Apply fun to successive pairs of values from list. Only takes one list."
  (when (cadr list)
    (funcall fun (car list) (cadr list))
    (mapl2 (cddr list) fun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point (x y)
  (cons x y))

(defun make-vector (x y)
  (cons x y))

(defun point-x (item)
  (car item))

(defun point-y (item)
  (cdr item))

(defun vector-length (to)
  (sqrt (+ (expt (point-x to) 2)
           (expt (point-y to) 2))))

(defun point-distance (from to)
  "The distance between two points."
  (sqrt (+ (expt (- (point-x to) (point-x from)) 2)
           (expt (- (point-y to) (point-y from)) 2))))

(defun scale (pt by)
  (make-point (* (point-x pt) by)
              (* (point-y pt) by)))

(defun divide (pt by)
  (make-point (/ (point-x pt) by)
              (/ (point-y pt) by)))

(defun vector-truncate (vec to)
  (let ((vec-length (vector-length vec)))
    (cond
      ((> vec-length to)
       (scale vec (/ to vec-length)))
      (t
       (make-vector (point-x vec) (point-y vec))))))

(defun add (a b)
  (make-point (+ (point-x a) (point-x b))
              (+ (point-y a) (point-y b))))

(defun subtract (a b)
  (make-point (- (point-x a) (point-x b))
              (- (point-y a) (point-y b))))

(defun offset-point-along-direction (p direction amount)
  (make-point (+ (point-x p)
                 (* amount (cos direction)))
              (+ (point-y p)
                 (* amount (sin direction)))))

(defconstant radian (* pi 2))
(defconstant radian/3/4 (/ (* pi 3) 2))
(defconstant radian/2 pi)
(defconstant radian/4 (/ pi 2))

(defun atan2 (x y)
  "An implementation of the atan2 function."
  (cond
    ((> y 0)
     (cond
       ((> x 0) (atan (/ y x)))
       ((< x 0) (- radian/2 (atan (- (/ y x)))))
       ((= x 0) radian/4))) ;; 90
    ((< y 0)
     (cond
       ((> x 0) (- (atan (- (/ y x)))))
       ((< x 0) (- (atan (/ y x)) radian/2)) ;; - 180
       ((= x 0) radian/3/4))) ;; 270
    ((= y 0)
     (cond
       ((> x 0) 0.0)
       ((< x 0) radian/2) ;; 180
       ((= x 0) 0.0))))) ;; Points are the same

(defun angle-between-points (p1 p2)
  "Calculate the angle of the second point around the first."
  (let* ((x1 (point-x p1))
         (y1 (point-y p1))
         (x2 (point-x p2))
         (y2 (point-y p2))
         (dx (- x2 x1))
         (dy (- y2 y1)))
    (atan2 dx dy)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The image cell matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *image-width* 1024
  "The width of the image cell matrix, in cells.")

(defparameter *image-height* 768
  "The height of the image cell matrix, in cells.")

(defstruct cell
  "An image matrix cell."
  (status nil)
  (figure nil))

(defparameter *statuses* '(nil rough figure-outline unused-inside-figure)
  "The statuses each cell can have. nil is ground.")

(defparameter *image-cells* (make-array (list *image-width* *image-height*))
  "The image cell matrix. Row major.")

(defun set-cell (x y value)
  "Set cell x,y of the image cells matrix to value."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (setf (aref *image-cells*
              x y)
        value))

(defun get-cell (x y)
  "Get the value of cell x,y in the image cells matrix. "
  (assert (>= x 0))
  (assert (<= x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (aref *image-cells*
        x y))

(defun set-cell-status (x y status)
  "Set the cell's status."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (setf (cell-status (get-cell x y)) status))

(defun get-cell-status (x y)
  "Set the cell's status."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (cell-status (get-cell x y)))

(defun apply-line-cells (x0 y0 x1 y1 fun)
  "Bresenham's algorithm. Always applies left to right."
  ;;FIXME: use modified version, see Cohen's writing.
  (assert (>= x0 0))
  (assert (< x0 *image-width*))
  (assert (>= y0 0))
  (assert (< y0 *image-height*))
  (assert (>= x1 0))
  (assert (< x1 *image-width*))
  (assert (>= y1 0))
  (assert (< y1 *image-height*))
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (when steep
      (rotatef x0 y0)
      (rotatef x1 y1))
    (when (> x0 x1)
      (rotatef x0 x1)
      (rotatef y0 y1))
    (let* ((deltax (- x1 x0))
           (deltay (abs (- y1 y0)))
           (err (- (/ (+ deltax 1) 2)))
           (ystep (if (< y0 y1) 1 -1))
           (y y0))
      (loop for x from x0 to x1 ;; Below?
         do (progn
              (if steep
                  (funcall fun y x)
                  (funcall fun x y))
              (setf err (+ err deltay))
              (when (>= err 0)
                (setf y (+ y ystep))
                (setf err (- err deltax))))))))

(defun apply-rect-cells (x y width height fun)
  "Applies the function to each cell of each row of the area top-to-bottom."
  (assert (>= x 0))
  (assert (<= x *image-width*))
  (assert (>= y 0))
  (assert (<= y *image-height*))
  (assert (>= (+ x width) 0))
  (assert (<= (+ x width) *image-width*))
  (assert (>= (+ y height) 0))
  (assert (<= (+ y height) *image-height*))
  (dotimes (j height)
    (dotimes (i width)
      (funcall fun
              (+ x i)
              (+ y j)))))

(defun clear-ground-p (x y width height)
  "Is the rectangular space clear of figure or rough cells?"
  (let ((clear t))
    (block clear-block
      (apply-rect-cells x y (1+ width) (1+ height)
                        (lambda (h v)
                          (when (not (eq (cell-status (get-cell h v)) nil))
                            (setf clear nil)
                            (return-from clear-block)))))
      clear))

(defun initialise-cell-matrix ()
  "Initialize the cell matrix."
  (apply-rect-cells 0 0 *image-width* *image-height*
                    (lambda (x y) (set-cell x y (make-cell)))))

(defun roughen (&optional (count 10))
  "Set some cells to be unusable for drawing."
  (dotimes (i count)
    (setf (cell-status (get-cell (random  *image-width*)
                                 (random  *image-height*)) )
          'rough)))

(defun write-cell-matrix-ppm-file (&optional (filename "./cells.ppm")
                                             (comment ""))
  "Write the cell matrix to a colour, binary ppm file."
  ;; Write the header in character stream mode
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (format stream "P6~%#~A~%~D ~D~%~d~%"
            comment *image-width* *image-height* 255))
  ;; Append the rasters in byte stream mode
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :append
                          :element-type '(unsigned-byte 8))
    (dotimes (j *image-height*)
      (dotimes (i *image-width*)
        (case (get-cell-status i (- *image-height* j 1))
          (figure-outline (write-byte 0 stream)
                          (write-byte 0 stream)
                          (write-byte 0 stream))
          (unused-inside-figure (write-byte 0 stream)
                                (write-byte 255 stream)
                                (write-byte 0 stream))
          (rough (write-byte 255 stream)
                 (write-byte 0 stream)
                 (write-byte 0 stream))
          (t (write-byte 255 stream)
             (write-byte 255 stream)
             (write-byte 255 stream)))))
    (namestring stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct figure
  allocated-space
  (actual-bounds nil)
  (outline '())
  (closed nil))

(defun set-cell-figure (x y fig)
  "Set the cell's figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (setf (cell-figure (get-cell x y)) fig)
  (if (null (figure-actual-bounds fig))
      (setf (figure-actual-bounds fig)
            (make-rectangle :x x :y y :width 1 :height 1))
      (rectangle-include-point (figure-actual-bounds fig) (make-point x y))))

(defun set-cell-figure-outline (x y fig)
  "Set the cell to be part of the outline of the figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (set-cell-figure x y fig)
  (set-cell-status x y 'figure-outline))

(defun set-cell-figure-unused-space (x y fig)
  "Set the cell to be part of the outline of the figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (set-cell-figure x y fig)
  (set-cell-status x y 'unused-inside-figure))

(defparameter *figures* '()
  "The figures of the image.")

(defun figure-outline-cell-p (x y)
  "Is the cell part of the outline of a figure?"
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (not (cell-figure (get-cell x y))))

(defun figure-closed-p (fig)
  "Has the figure been marked as closed?"
  (figure-closed fig))

(defun cell-figure-closed-p (x y)
  "Is the figure of the cell at x,y closed? No figure (or open) returns nil."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (let ((fig (cell-figure (get-cell x y))))
    (and fig
         (figure-closed-p fig))))

(defun cell-figure-p (x y)
  "Is the point in a figure or on its outline (not optically)?"
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (eq (cell-status (get-cell x y))
  ;;    'unused-inside-figure))
  (cell-figure (get-cell x y)))

(defun apply-figure-fill (fig)
  "Set the cells inside the figure outline to being figure space."
  (assert (figure-closed-p fig))
  (fill-polygon (figure-outline fig)
                (figure-actual-bounds fig)
                (lambda (x y)
                  (when (null (get-cell-status x y))
                    (set-cell-figure-unused-space x y fig)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *contexts* (make-hash-table))

(defparameter *context* nil
  "The currently operating level of the system, e.g. curves, line.")

(defparameter possible-contexts '(nil artwork mapping planning lines sectors curves movement-control))

(defstruct context
  name
  (productions '()))

(defun get-context (context-name)
  (or (gethash context-name *contexts*)
      (setf (gethash context-name *contexts*)
	    (make-context :name context-name :productions '()))))

(defun add-production-to-context (context-name production)
  (setf (context-productions (get-context context-name))
	(append (context-productions (get-context context-name))
		(list production))))

(defun set-context (context-name)
  "Set the current context."
  (assert (member context-name possible-contexts))
  (setf *context* (gethash context-name *contexts*)))

(defun current-context-productions ()
  (context-productions *context*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar previous-production nil)

(defstruct production
  description
  left-hand-side
  right-hand-side)

(defmacro defproduction (context-name description &body body)
  "Define a production rule. Note description doesn't have to be unique."
  (let* ((after-arrow (member '=> body))
         (left-hand-side (ldiff body after-arrow))
         (right-hand-side (rest after-arrow)))
    `(add-production-to-context
      ,context-name
      (make-production
       :description ,description
       :left-hand-side (lambda () (and ,@left-hand-side))
       :right-hand-side (lambda () ,@right-hand-side)))))

(defun apply-production ()
  "Try to find and apply a single production. Returns t if one ran."
  (dolist (the-production (current-context-productions))
    (when (funcall (production-left-hand-side the-production))
      (when (not (eq (production-description the-production)
                     previous-production))
        (format t "~a~%" (production-description the-production))
        (setf previous-production (production-description the-production)))
      (funcall (production-right-hand-side the-production))
      (return t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Current Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eventually need a stack of current developments
;; and a list of past developments

(defvar current-figure)

(defvar current-line-spec)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pen-position nil)
(defvar pen-direction 2)
(defvar pen-speed 1.0)
(defvar pen-max-turn (/ radian 10))
(defvar pen-turn-damping 20)
(defvar pen-concentration-distance 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Curves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that we don't try to match the line's finishing direction
;; This must be matched implicitly by reaching all the waypoints

(defvar next-waypoint)

(defvar waypoint-distance-tolerance 1)

(defun reaching-waypoint-impossible ()
  "Will we not be able to reach the waypoint given the current parameters?"
  ;; How to factor in pen-concentration-distance if used?
  ;; How to factor in maximum amount of random change if used?
  (let* ((steps-remaining (/ (point-distance pen-position next-waypoint)
                             pen-speed))
         (direction-to-waypoint (angle-between-points pen-position
                                                          next-waypoint))
         (direction-difference (- direction-to-waypoint pen-direction))
         (min-turn-per-remaining-step (/ direction-difference steps-remaining)))
    (< pen-max-turn min-turn-per-remaining-step)))

(defun veer-towards-waypoint ()
  (let* ((direction-to-waypoint (angle-between-points pen-position
                                                          next-waypoint))
         (direction-difference (- direction-to-waypoint pen-direction))
         (direction-change (/ (min direction-difference pen-max-turn)
                              pen-turn-damping)))

        (setf pen-direction (+ pen-direction direction-change))))

(defun close-enough-to-next-waypoint ()
  (< (point-distance pen-position next-waypoint) waypoint-distance-tolerance))

(defproduction 'curves
    "seeking to next waypoint"
  (not (close-enough-to-next-waypoint))
  =>
  (veer-towards-waypoint)
  (setf pen-position (offset-point-along-direction pen-position
                                                   pen-direction
                                                   pen-speed))
  (set-cell-figure-outline (floor (point-x pen-position))
                           (floor (point-y pen-position))
                           current-figure)
  (push pen-position (figure-outline current-figure)))

(defproduction 'curves
    "close enough to to next waypoint"
  (close-enough-to-next-waypoint)
  =>
  (set-context 'sectors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct line-spec
  from
  from-angle
  to
  to-angle
  curviness)

(defvar lines-for-current-development)
;; Used by Sectors, declared here to keep the compiler happy.
(defvar line-number-of-waypoints)
(defvar line-current-waypoint)

(defproduction 'lines
    "finished drawing lines of current development"
  (null lines-for-current-development)
  =>
  (set-context 'planning))

(defproduction 'lines
    "drawing next line of current development"
  (not (null lines-for-current-development))
  =>
  ;; Lines should instantiate the line templates and call sectors for each
  (setf current-line-spec (first lines-for-current-development))
  (pop lines-for-current-development)
  (set-context 'sectors)
  (setf line-number-of-waypoints 1)
  (setf line-current-waypoint 0)
  (setf pen-position (line-spec-from current-line-spec))
  (setf pen-direction (line-spec-from-angle current-line-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-finished ()
  ;; Replace with a pen distance check
  (= line-current-waypoint line-number-of-waypoints))

(defun set-waypoint ()
  (incf line-current-waypoint)
  (setf next-waypoint (line-spec-to current-line-spec)))

(defproduction 'sectors
    "setting next waypoint"
  (not (line-finished))
  =>
  (set-waypoint)
  (set-context 'curves))

(defproduction 'sectors
    "finished line"
  (line-finished)
  =>
  (set-context 'lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar planned-figure-space)

;; Just lines at the moment
;; Each development can consist of several lines...

(defun plan-line-figure ()
  (setf current-figure (make-figure :allocated-space planned-figure-space))
  (setf lines-for-current-development
        ;;FIXME: These should be line templates for lines to instantiate
        (list (make-line-spec
               :from (make-point (rectangle-left planned-figure-space)
                                 (+ (rectangle-y planned-figure-space)
                                    (/ (rectangle-height planned-figure-space)
                                       2)))
               :from-angle 1.6
               :to (make-point (rectangle-right planned-figure-space)
                               (+ (rectangle-y planned-figure-space)
                                  (/ (rectangle-height planned-figure-space)
                                     2)))
               :to-angle 1.6))))

(defun plan-loop-figure ()
  (setf current-figure (make-figure :allocated-space planned-figure-space
                                    :closed t))
  (print planned-figure-space)
  (setf lines-for-current-development
        ;;FIXME: These should be line templates for lines to instantiate
        (list (make-line-spec
               :from (make-point (rectangle-left planned-figure-space)
                                 (+ (rectangle-y planned-figure-space)
                                    (/ (rectangle-height planned-figure-space)
                                       2)))
               :from-angle 1.5
               :to (make-point (rectangle-right planned-figure-space)
                                 (+ (rectangle-y planned-figure-space)
                                    (/ (rectangle-height planned-figure-space)
                                       2)))
               :to-angle 5)
              (make-line-spec
               :from (make-point (rectangle-right planned-figure-space)
                                 (+ (rectangle-y planned-figure-space)
                                    (/ (rectangle-height planned-figure-space)
                                       2)))
               :from-angle 5
               :to (make-point (rectangle-left planned-figure-space)
                               (+ (rectangle-y planned-figure-space)
                                  (/ (rectangle-height planned-figure-space)
                                     2)))
               :to-angle 1.5))))

(defproduction 'planning
    "planning next development"
  (not (null planned-figure-space))
  =>
  (if (> (random 1.0) 0.5)
      (plan-loop-figure)
      (progn (print "line")
             (plan-line-figure)))
  (print lines-for-current-development)
  (setf planned-figure-space nil)
  (set-context 'lines))

(defproduction 'planning
    "finished planned development"
  (null planned-figure-space)
  =>
  (when (figure-closed current-figure)
    (apply-figure-fill current-figure))
  (push current-figure *figures*)
  (setf current-figure nil)
  (set-context 'artwork))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar required-figure-space-width nil)
(defvar required-figure-space-height nil)
(defvar figure-allocation-failures 0)

(defvar figure-tries-this-time 0)

(defun call-mapping ()
  (set-context 'mapping)
  (setf planned-figure-space nil)
  (setf figure-allocation-failures 0)
  (setf figure-tries-this-time 0))

(defun allocation-failed-this-time ()
  (> figure-tries-this-time 10))

(defun find-space-for-figure (width height &optional (tries 100))
  "Very simple and dumb space-finding."
  (let ((space nil))
    (dotimes (i tries)
      (let ((x (random (- *image-width* width)))
            (y (random (- *image-height* height))))
        (when (clear-ground-p x y width height)
          (setf space (cons x y))
          (return))))
    space))

(defun try-to-find-space-for-figure ()
    (let ((space (find-space-for-figure required-figure-space-width
                                        required-figure-space-height)))
      (cond
       ((not (null space))
        (setf planned-figure-space
              (make-rectangle :x (car space)
                              :y (cdr space)
                              :width required-figure-space-width
                              :height required-figure-space-height)))
       (t
        (incf figure-allocation-failures)
        (setf planned-figure-space nil)))))

(defproduction 'mapping
    "couldn't fulfill space requirement"
  (allocation-failed-this-time)
  =>
  (incf figure-allocation-failures)
  (setf figure-tries-this-time 0)
  (set-context 'drawing))

(defproduction 'mapping
    "fulfilled space requirement"
  (not (null planned-figure-space))
  =>
  (setf required-figure-space-width nil)
  (setf required-figure-space-height nil)
  ;; When planning can call mapping this will need changing
  (set-context 'planning))

(defproduction 'mapping
    "trying to fulfill space requirement"
  (null planned-figure-space)
  (not (allocation-failed-this-time))
  =>
  (try-to-find-space-for-figure))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artwork
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Decide on the number of small & large figures and general distribution

(defun enough-figures ()
  (>= (length *figures*) 10))

(defun no-more-room ()
  (> figure-allocation-failures 5))

(defun specify-required-figure-space ()
  (setf required-figure-space-width (* 100 (random-range 1 5)))
  (setf required-figure-space-height (* 100 (random-range 1 5))))

(defproduction 'artwork
    "drawn enough figures"
  (enough-figures)
  =>
  (set-context nil))

(defproduction 'artwork
    "filled up the picture plane"
  (no-more-room)
  =>
  (set-context nil))

(defproduction 'artwork
    "specifying figure space to find"
  (not (enough-figures))
  (not (no-more-room))
  =>
  (specify-required-figure-space)
  (call-mapping))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ben ()
  (initialise-cell-matrix)
  (roughen)
  (setf *figures* '())
  (set-context 'artwork)
  (loop until (null *context*)
     do (apply-production))
   (write-cell-matrix-ppm-file))
