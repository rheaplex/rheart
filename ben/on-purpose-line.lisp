
;;  on-purpose-line.lisp -  An implementation of the line drawing algorithm from
;;                          Harold Cohen's essay "On Purpose".
;;  Copyright (C) 2008, 2024 Rhea Myers rhea@myers.studio
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
;; The following are guesses:
;; - All the parameter ranges.
;; - The homing algorithm.
;; The following are historically inauthentic:
;; - The use of Lisp rather than C.
;; This is at best an approximation, but hopefully an interesting approximation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Line Drawing.

(defvar pen-position)
(defvar pen-direction)
(defparameter pen-move-distance 2.0)

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

(defparameter min-sub-phase-length 50.0)
(defparameter max-sub-phase-length 256.0)

(defun set-length-of-sub-phase ()
  (setf sub-phase-length-so-far 0)
  (setf sub-phase-length (random-range (min min-sub-phase-length
                                            line-distance)
                                       (min max-sub-phase-length
                                            line-distance))))

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

(defun set-swing-straight-ahead ()
  "When the swing state is straight ahead, swing rate and change rate are 0."
  (setf swing-rate 0.0)
  (setf swing-rate-change 0.0))

(defun set-swing-amount ()
  "Set the swing and change rate for a left or right swing."
  (setf swing-rate (random-range min-swing-rate max-swing-rate))
  (setf swing-rate-change (random-swing-rate-change)))

(defun set-rate-of-swing ()
  "Set values for the swing depending on whether we are within angular limit."
  (set-swing-kind)
   (if (eq swing-direction 'straight-ahead)
      (set-swing-straight-ahead)
      (set-swing-amount)))

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
  "For time from 0..1 ."
  (assert (<= 0.0 time 1.0))
  time)

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
  ;;(format t "~a ~a ~a ~%" pen-direction (weight-from-distance) (direction-difference))
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

(defun new-sub-phase ()
  (set-length-of-sub-phase)
  (if (= sub-phase-count 0)
      (progn
        (set-swing-left-or-right)
        (set-rate-of-swing))
      (reverse-swing))
  ;;                 (format t "Subphase: length ~a direction ~a swing ~a change ~a~%" sub-phase-length swing-direction swing-rate swing-rate-change)
  (incf sub-phase-count))

(defun new-phase ()
  "Start a new phase ."
  (format t "NEW PHASE~%")
  (setf sub-phase-count 0))

(defun new-phase-outside-angular-limits ()
  (format t "Exceeded angular limit.~%")
  (let ((direction swing-direction))
    (new-phase)
    (new-sub-phase)
    (setf swing-direction direction)
    (reverse-swing)))

(defun draw-line-step ()
  (calculate-new-direction-for-line)
  (correction-for-homing)
  (calculate-position-of-next-point)
  (move-to-next-point))

(defun draw-line (from to)
  (apply-line-cells (point-x from) (point-y from) (point-x to) (point-y to)
                    (lambda (x y)
                      (set-cell-status x y 'unused-inside-figure)))
  (set-line-drawing-parameters from to)
  (loop
        do (loop
             initially (new-phase)
             do (loop
                  initially (new-sub-phase)
                  when (not (within-angular-limits))
                    do (new-phase-outside-angular-limits)
                  do (draw-line-step)
                  until (or (end-of-sub-phase) (reached-destination)))
             until (or (end-of-phase) (reached-destination)))
        until (reached-destination)))

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
