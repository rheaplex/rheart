;;  turtle.lisp - A classic, L-system style computer graphics turtle.
;;  Copyright (C) 2006  Rhea Myers rhea@myers.studio
;;
;; This file is part of draw-something.
;; 
;; draw-something is free software; you can redistribute it and/or modify
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

;;(in-package "DRAW-SOMETHING")

(defclass turtle ()
  ((location :accessor location 
	     :initform (make-instance 'point) 
	     :initarg :location
	     :documentation "The turtle's current location.")
   (direction :accessor direction
	     :initform 0.0
	     :initarg :direction
	     :documentation "The heading, in radians anticlockwise.")
   (turn-step :accessor turn-step 
	      :initform 0.01
	      :initarg :turn-step
	      :documentation "How far the turtle turns at a time.")
   (move-step :accessor move-step
	      :initform 1.0
	      :initarg :move-step
	      :documentation "How far the turtle moves each step."))
   (:documentation "A classic computer graphics turtle ."))

(defmethod left ((the-turtle turtle))
  "Turn the turtle left by the given amount in degrees,"
  (setf (direction the-turtle) 
	(- (direction the-turtle) 
	   (turn-step the-turtle))))

(defmethod right ((the-turtle turtle))
  "Turn the turtle right by the given amount in degrees,"
  (setf (direction the-turtle) 
	(+ (direction the-turtle) 
	   (turn-step the-turtle))))

(defmethod forward ((the-turtle turtle))
  "Move the turtle forward the given distance at the turtle's current angle."
  (setf (location the-turtle) 
	(next-point the-turtle)))

;; Hmmm. We need to make the step negative...
;;(defmethod backward ((the-turtle turtle))
;;  "Move the turtle backward the given distance at the turtle's current angle."
;;  (setf (location the-turtle) 
;;	(next-point the-turtle 
;;		    (- (move-step the-turtle)))))

(defmethod next-point-x ((the-turtle turtle))
  "The x co-ordinate of the next point the turtle would move forward to."
  (+ (x (location the-turtle)) 
     (* (move-step the-turtle) 
	(sin (direction the-turtle)))))

(defmethod next-point-y ((the-turtle turtle))
  "The y co-ordinate of the next point the turtle would move forward to."
  (+ (y (location the-turtle)) 
     (* (move-step the-turtle)
	(cos (direction the-turtle)))))

(defmethod next-point ((the-turtle turtle))
  "The next point the turtle would move forward to."
    (make-instance 'point 
		   :x (next-point-x the-turtle)
		   :y (next-point-y the-turtle)))