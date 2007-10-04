;;  pen.lisp - The pen to draw around skeletal forms with.
;;  Copyright (C) 2007 Rhea Myers rhea@myers.studio
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

(defclass pen-parameters (turtle-parameters)
  ((distance           :accessor pen-distance
                       :initarg :distance
                       :documentation "How far fromt the skeleton to draw.")
   (distance-tolerance :accessor distance-tolerance
                       :initarg :distance-tolerance
                       :documentation "How far from the distance is tolerable.")
   (drift-probability  :accessor drift-probability
                       :initarg :drift-probability
                       :documentation "How likely it is the pen will wobble.")
   (drift-range        :accessor drift-range
                       :initarg :drift-range
                       :documentation "The +/- pen wobble range."))
  (:documentation "A set of parameters for a pen to use."))

;; Start with fixed values for tuning
;; Move to random ranges for production

(defparameter plane-1-pen
  (make-instance 'pen-parameters
                 :move-step          4.0
                 :distance           20.0
                 :distance-tolerance 5.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-2-pen
  (make-instance 'pen-parameters
                 :move-step          2.0
                 :distance           10.0
                 :distance-tolerance 3.3
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-3-pen
  (make-instance 'pen-parameters
                 :move-step          2.0
                 :distance           7.0
                 :distance-tolerance 2.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-4-pen
  (make-instance 'pen-parameters
                 :move-step          2.0
                 :distance           5.0
                 :distance-tolerance 2.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-5-pen
  (make-instance 'pen-parameters
                 :move-step          2.0
                 :distance           3.0
                 :distance-tolerance 1.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-6-pen
  (make-instance 'pen-parameters
                 :move-step          2.0
                 :distance           2.0
                 :distance-tolerance 1.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-7-pen
  (make-instance 'pen-parameters
                 :move-step          1.0
                 :distance           1.0
                 :distance-tolerance 0.2
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter plane-pen-parameters (vector plane-1-pen plane-2-pen plane-3-pen
                                           plane-4-pen plane-5-pen plane-6-pen
                                           plane-7-pen))


(defmethod next-pen-distance ((skeleton-forms vector) (pen pen-parameters)
			      (the-turtle turtle))
  "How far the pen will be from the guide shape when it next moves forwards."
  (let ((dist most-positive-single-float)
        (p (next-point the-turtle (move-step pen) )))
    (dovector (skel skeleton-forms)
      (let ((new-dist (distance p skel)))
        (when (< new-dist dist)
          (setf dist new-dist))))
    dist))

(defmethod next-pen-too-close ((skeleton-forms vector) (pen pen-parameters)
			       (the-turtle turtle))
  "Will the pen move to be too close from the guide shape next time?"
  (< (random (distance-tolerance pen))
     (- (next-pen-distance skeleton-forms pen the-turtle)
       (pen-distance pen))))

(defmethod next-pen-too-far ((skeleton-forms vector) (pen pen-parameters)
			     (the-turtle turtle))
  "Will the pen move to be too far from the guide shape next time?"
  (< (random (distance-tolerance pen))
     (- (pen-distance pen)
        (next-pen-distance skeleton-forms pen the-turtle))))

(defmethod ensure-next-pen-far-enough ((skeleton-forms vector) 
				       (pen pen-parameters)
				       (the-turtle turtle))
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close skeleton-forms pen the-turtle)
     do (left the-turtle (random (turn-step pen)))))

(defmethod ensure-next-pen-close-enough ((skeleton-forms vector) 
					 (pen pen-parameters)
					 (the-turtle turtle))
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far skeleton-forms pen the-turtle)
     do (right the-turtle (random (turn-step pen)))))

(defmethod drift-pen-direction ((pen pen-parameters)
				(the-turtle turtle))
  "Adjust the pen's direction to simulate human neurophysiological noise."
  (if (< (random 1.0) (drift-probability pen))
      (turn the-turtle
            (random-range (- (drift-range pen)) 
			  (drift-range pen)))))

(defmethod adjust-next-pen ((skeleton-forms vector) (pen pen-parameters)
			    (the-turtle turtle))
  "Drift or correct the pen's heading around the shape."
  (drift-pen-direction pen the-turtle)
  (ensure-next-pen-far-enough skeleton-forms pen the-turtle)
  (ensure-next-pen-close-enough skeleton-forms pen the-turtle))
