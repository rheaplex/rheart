;;  form.lisp - A form of a figure.
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


(defconstant min-form-points 1)
(defconstant max-form-points 12)

(defconstant form-step-limit 5000)

(defconstant pen-width 1.0)

(defclass form ()
  ((skeleton :accessor skeleton
             :type vector
             :initarg :skeleton
             :initform (make-vector 5)
             :documentation "The guide shape for the outline.")
   (outline :accessor outline
            :type polyline
            :initform (make-instance 'polyline)
            :documentation "The outlines for the skeleton. Will be outline_s_.")
   (bounds :accessor bounds
           :type rectangle
           :initform (make-instance 'rectangle)
           :initarg :bounds
           :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
                :type colour
                :initarg :colour
                :initform (random-colour)
                :documentation "The flat body colour of the form."))
  (:documentation "A form drawn in the drawing."))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defmethod first-point ((the-form form))
  "Get the first point in the outline of the form."
  (first-point (outline the-form)))

(defmethod point-count ((the-form form))
  "The number of points in the outline of the form."
  (point-count (outline the-form)))

(defmethod most-recent-point ((the-form form))
  "The most recent point added to the outline of the form."
  (last-point (outline the-form)))

(defmethod make-form-start-point ((form-skeleton vector)
                                  (params pen-parameters))
  "Get the point to start drawing at."
  (let ((start-point nil))
    (dovector (skel form-skeleton)
      (let* ((hp (highest-leftmost-point skel))
             (candidate (make-instance 'point
                                       :x (x hp)
                                       :y (+ (y hp) (pen-distance params)))))
        (when (or (not start-point)
                  (> (y candidate) (y start-point))
                  (and (= (y candidate) (y start-point))
                       (< (x candidate) (x start-point))))
          (setf start-point candidate))))
    start-point))

(defmethod make-form-turtle ((the-form form) (params pen-parameters))
  "Make the turtle to draw around the form."
  (make-instance 'turtle
                 :location (make-form-start-point (skeleton the-form) params)
                 :direction (- (/ pi 2.0))
                 ))

(defmethod make-form-from-points ((points vector))
  "Make a form, ready to be started."
  (advisory-message (format nil "Making form.~%"))
  (let* ((skel (make-polyline-from-points points))
         (the-form (make-instance 'form
                                    :skeleton (vector skel)
                                    :bounds (bounds skel))))
    ;;(draw-form the-form) ;; Remove for codelets
    the-form))

(defmethod path-ready-to-close ((the-form form) (the-pen pen-parameters))
  (and (> (point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-form)
                    (first-point the-form))
          (move-step the-pen))))

(defmethod path-timeout ((the-form form))
  "Make sure that a failure of the form algorithm hasn't resulted in a loop."
  (> (point-count the-form)
     form-step-limit))

(defmethod should-finish-form ((the-form form) (the-pen pen-parameters))
  "Decide whether the form should finish."
  (or (path-ready-to-close the-form the-pen)
      (path-timeout the-form)))

(defmethod draw-form ((the-form form))
  "Find the next point forward along the drawn outline of the shape."
  (let* ((form-bounds (bounds the-form))
         (the-outline (outline the-form))
         (the-pen (choose-one-of plane-pen-parameters))
	 (the-turtle (make-form-turtle the-form the-pen)))
    (advisory-message "Drawing form.~%")
    (append-point the-outline (location the-turtle))
    (loop until (should-finish-form the-form the-pen)
       do (progn 
	    (adjust-next-pen (skeleton the-form) the-pen the-turtle)
	    (forward the-turtle (move-step the-pen))
	    (let ((new-location (location the-turtle)))
	      (append-point the-outline new-location)
	      (include-point form-bounds new-location))))
    (append-point the-outline (first-point the-form))))
