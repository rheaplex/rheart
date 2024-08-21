;;  Ben.lisp -  A toy reimplementation of Harold Cohen's AARON circa
;;              "What Is An Image".
;;  Copyright (C) 2008, 2024 Rhea Myers rhea@myers.studio
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
  (>= (length *figures*) 5))

(defun no-more-room ()
  (> figure-allocation-failures 5))

(defun specify-required-figure-space ()
  (setf required-figure-space-width (* 100 (random-range-int 1 5)))
  (setf required-figure-space-height (* 100 (random-range-int 1 5))))

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
