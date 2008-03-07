
;;  on-purpose-line.lisp -  An implementation of the line drawing algorithm from
;;                          Harold Cohen's essay "On Purpose".
;;  Copyright (C) 2008 Rhea Myers rhea@myers.studio
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A simple implementation of the line drawing algorithm from Harold Cohen's
;; essay "On Perception".
;; The following are not implemented:
;; - Sub-phases taking their parameter range from the previous sub-phase.
;; The following are guesses:
;; - All the parameter ranges.
;; - The homing algorithm.
;; The following may be historically inauthentic:
;; - The use of the sigmoid function.
;; The following are historically inauthentic:
;; - The use of Lisp rather than C.
;; - The use of floating point maths rather than integer maths.
;; This is at best an approximation, but hopefully an interesting approximation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun choose-one-of (&rest possibilities)
  "Choose one of the given options."
  (nth (random (length possibilities)) possibilities))

(defun random-range (a b)
  "Make a random number from a to below b."
  (let ((range (- b a)))
    (if (= range 0)
        a
        (+ (random range) a))))

(defun sigmoid (time)
  "An S Curve / sigmoid function. Useful range roughly -/+8. See Wikipedia."
  ;; 0 at -8, 0.5 at 1.0 and 1 at +8 , with an S-shaped graph
  (/ 1 (+ 1 (exp (- time)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The image cell matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *image-width* 200
  "The width of the image cell matrix, in cells.")

(defparameter *image-height* 200
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

(defun initialise-cell-matrix ()
  "Initialize the cell matrix."
  (apply-rect-cells 0 0 *image-width* *image-height*
                    (lambda (x y) (set-cell x y (make-cell)))))

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
;; Line Drawing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pen-position)
(defvar pen-direction)
(defparameter pen-move-distance 1.0)

(defvar line-start)
(defvar line-end)

;; Cached calculations
(defvar line-distance)
(defvar line-direction)

(defun pen-direction-relative-to-line ()
  (- pen-direction
     line-direction))

;; Swings per phase. 0 or 2. 0 = no swings, > 0 = swings.
(defparameter swings 2)
(defvar swing-direction)

(defun set-swing-left-or-right ()
  (if (= swings 0)
      (setf swing-direction 'straight-ahead)
      (setf swing-direction (choose-one-of 'left 'right))))

(defun reverse-swing ()
  (case swing-direction
    (straight-ahead (setf swing-direction 'straight-ahead))
    (left (setf swing-direction 'right))
    (right (setf swing-direction 'left))))

(defvar sub-phase-length)

(defvar sub-phase-length-so-far)
(defvar sub-phase-count 0)

(defun end-of-phase ()
  (incf sub-phase-count)
  (let ((is-end nil))
    (when (>= sub-phase-count swings)
      (setf sub-phase-count 0)
      (setf is-end t))
    is-end))

(defun end-of-sub-phase ()
  (>= sub-phase-length-so-far sub-phase-length))

(defparameter min-sub-phase-length 5.0)
(defparameter max-sub-phase-length 100.0)

(defun set-length-of-sub-phase ()
  (setf sub-phase-length-so-far 0)
  (setf sub-phase-length (random-range (max min-sub-phase-length
                                            (/ line-distance 10.0))
                                       (min max-sub-phase-length
                                            (/ line-distance 5.0)))))

(defparameter min-angular-limit (/ pi 8))
(defparameter max-angular-limit (/ pi 4))

(defvar angular-limit)

(defun set-angular-limit ()
  (setf angular-limit (random-range min-angular-limit max-angular-limit)))

(defun within-angular-limits ()
   ;;  (format t "~a ~a ~a ~%" pen-direction line-direction (abs (- pen-direction line-direction)))
  (< (abs (pen-direction-relative-to-line))
     angular-limit))

(defparameter min-swing-rate 0.01)
(defparameter max-swing-rate 0.1)

(defparameter min-swing-rate-change least-positive-single-float)
(defparameter max-swing-rate-change 0.001)

(defvar swing-kind)

(defvar swing-rate)
(defvar swing-rate-change)

(defun set-swing-kind ()
  "Set whether the swing will be constant, accelerate, or deccelerate."
  (setf swing-kind
        (choose-one-of 'constant 'accelerating 'decelerating)))

(defun random-swing-rate-change ()
  "Set the amount the swing rate will increase or decrease by each step."
  (case swing-kind
    (constant 0)
    (t (random-range min-swing-rate-change
                     max-swing-rate-change))))

(defun corrective-swing-rate-change ()
  "Set the amount the swing rate will increase or decrease by each step."
  (case swing-kind
    (constant 0)
    (t (random-range min-swing-rate-change
                     max-swing-rate-change))))

(defun set-swing-straight-ahead ()
  "When the swing state is straight ahead, swing rate and change rate are 0."
  (setf swing-rate 0.0)
  (setf swing-rate-change 0.0))

(defun set-swing-amount ()
  "Set the swing and change rate for a left or right swing."
  (setf swing-rate (random-range min-swing-rate max-swing-rate))
  (setf swing-rate-change (random-swing-rate-change)))

(defun random-rate-of-swing ()
  "Choose random values for the swing based on its direction."
  (if (eq swing-direction 'straight-ahead)
      (set-swing-straight-ahead)
      (set-swing-amount)))

(defun set-swing-amount-corrective ()
  "Set the swing and change rate to correct from exceeding the angular limit."
  (let ((min-corrective-swing  (- (abs (pen-direction-relative-to-line))
                                  angular-limit)))
    (if (< min-corrective-swing max-swing-rate)
        (setf swing-rate (random-range min-corrective-swing
                                   max-swing-rate))
        ;; Something has gone horribly wrong, probably due to homing
        (progn
          (format t "Corrective turn exceeds maximum turn rate, probably due to homing.~%")
          (setf swing-rate min-corrective-swing)))
    (setf swing-rate-change (corrective-swing-rate-change))))

(defun corrective-rate-of-swing ()
  "Choose values for the swing to correct from exceeding the angular limit."
  (if (eq swing-direction 'straight-ahead)
      (break "corrective-weight-of-swing with swing-direction straight-ahead")
      (set-swing-amount-corrective)))

(defun set-rate-of-swing ()
  "Set values for the swing depending on whether we are within angular limit."
  (set-swing-kind)
  (if (within-angular-limits)
      (random-rate-of-swing)
      (corrective-rate-of-swing)))

(defun update-swing-rate ()
  "Update the swing rate, constraining the result to 0..max-swing-rate."
  (case swing-kind
    ;; Do nothing for constant
    (accelerating
     (setf swing-rate (+ swing-rate swing-rate-change))
     (when (> swing-rate max-swing-rate)
       (setf swing-rate max-swing-rate)))
    (decelerating
     (setf swing-rate (- swing-rate swing-rate-change))
     (when (< swing-rate 0.0)
       (setf swing-rate 0.0)))))

(defun calculate-new-direction-for-line ()
  "Apply the swing to the pen."
  (update-swing-rate)
  (case swing-direction
    (straight-ahead nil)
    (left
     (setf pen-direction (+ pen-direction swing-rate)))
    (right
     (setf pen-direction (- pen-direction swing-rate)))))

(defun position-difference ()
  (point-distance line-start
                  pen-position))

(defun direction-difference ()
  "The pen's relative direction to the line, -pi..+pi radians."
  (assert (< (bearing-to-position (point-x pen-position) (point-y pen-position)
                                pen-direction (point-x line-end) (point-y line-end))
           2.0))
  (bearing-to-position (point-x pen-position) (point-y pen-position)
                       pen-direction (point-x line-end) (point-y line-end)))

(defun weight-curve (time)
  "For time from 0..1 , increase more rapidly as time approaches 1.0 ."
  (assert (<= 0.0 time 1.0))
  (* (sigmoid (+ -8 (* time 8)))
     2.0))

(defun weight-from-distance ()
  "How close to the direction to the end point the pen should be now."
  (weight-curve (- 1.0
                   (/ (abs (- line-distance
                              (position-difference)))
                      line-distance))))

(defun weighted-correction ()
  (* (direction-difference)
     (weight-from-distance)))

;; It would be a lot easier to store this separately from the pen direction
;; and add it in when calculating the next pen position but I don't think that
;; fits the description.

(defun correction-for-homing ()
     ;;  (format t "~a ~a ~a ~%" pen-direction (weight-from-distance) (direction-difference))
  (assert (< (direction-difference) 2.0))
  (setf pen-direction (+ pen-direction (weighted-correction))))

(defun calculate-position-of-next-point ()
  (setf pen-position
        (offset-point-along-direction pen-position
                                      pen-direction
                                      pen-move-distance)))

(defun move-to-next-point ()
  (set-cell-status (floor (point-x pen-position))
                   (floor (point-y pen-position))
                   'figure-outline)
  (setf sub-phase-length-so-far
        (+ sub-phase-length-so-far
           pen-move-distance)))

(defparameter destination-distance-tolerance 1.0)

(defun reached-destination ()
  (< (point-distance pen-position line-end)
     destination-distance-tolerance))

(defun set-line-drawing-parameters (from to)
  (setf line-start from)
  (setf line-end to)
  (setf line-distance (line-length line-start line-end))
  (setf line-direction (angle-between-points line-start line-end))
  (setf pen-position line-start)
  (setf pen-direction line-direction))

(defvar panic-count)

(defun draw-line (from to)
  (apply-line-cells (point-x from) (point-y from)
                    (point-x to) (point-y to)
                    (lambda (x y)
                      (set-cell-status x y 'unused-inside-figure)))
  (set-line-drawing-parameters from to)
  (loop named stop
     do  (loop
            ;;      initially (format t "NEW PHASE~%")
            initially (set-swing-left-or-right)
            initially (setf panic-count 0)
            do (progn
                 (set-length-of-sub-phase)
                 (set-rate-of-swing)
                      ;;                 (format t "Subphase: length ~a direction ~a swing ~a change ~a~%" sub-phase-length swing-direction swing-rate swing-rate-change)
                 (loop
                    do (progn
                         (calculate-new-direction-for-line)
                         (when (not (within-angular-limits))
                                ;;                         (format t "panic: pen ~a limit ~a correction ~a~%" pen-direction angular-limit (weighted-correction))
                           (incf panic-count)
                           (when (> panic-count 20)
                             (break))
                           (setf sub-phase-count 0) ;; Start new phase
                           (loop-finish)) ;; This will still evaluate finally
                         (correction-for-homing)
                         (calculate-position-of-next-point)
                         (move-to-next-point)
                         (when (reached-destination)
                           (return-from stop)))
                    until (end-of-sub-phase)
                    finally (reverse-swing)))
            until (end-of-phase))))

(defun test-draw-line ()
  (initialise-cell-matrix)
  (set-angular-limit) ;; Where should this go and how often?

  ;; Horizontal
  (draw-line (make-point 10 100) (make-point 190 100))
  (draw-line (make-point 190 100) (make-point 10 100))

  ;; Up diagonal
  (draw-line (make-point 25 25) (make-point 175 175))
  (draw-line (make-point 175 175) (make-point 25 25))

  ;; Down diagonal
  (draw-line (make-point 25 175) (make-point 175 25))
  (draw-line (make-point 175 25) (make-point 25 175))

  ;; Vertical
  (draw-line (make-point 100 10) (make-point 100 190))
  (draw-line (make-point 100 190) (make-point 100 10))

  (write-cell-matrix-ppm-file))

(defun test-draw-lines ()
  (dotimes (i 1000)
    (test-draw-line)))

(defun test-draw-lines-lots ()
  (dotimes (i 1000)
    (format t "~a~%" i)
    (test-draw-lines)))