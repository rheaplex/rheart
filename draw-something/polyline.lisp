;;  polyline.lisp - A classic computer graphics polyline.
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

(defclass polyline ()
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ;; For speed set initial dimension to a size unlikely to need increasing
  ((points :accessor points
           :initform (make-vector 1000)
           :initarg :points
           :documentation "The points of the polyline")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the polyline."))
  (:documentation "A polyline or polygon. A series of joined line segments."))

(defmethod append-point ((poly polyline) (pt point))
  "Append a point to the polyline."
  (vector-push-extend pt (points poly))
  (if (slot-boundp poly 'bounds)
      (include-point (bounds poly) pt)
      (setf (bounds poly) (rectangle-from-point pt))))

(defmethod point-count ((poly polyline))
  "Get the number of points in the polyline"
  (length (points poly)))

(defmethod first-point ((poly polyline))
  "Get the first point of the polyline."
  (aref (points poly) 0))

(defmethod last-point ((poly polyline))
  "Get the last point of the polyline."
  (aref (points poly)
        (- (point-count poly) 1)))

(defmethod make-random-polyline-in-rectangle (rect count)
  "Create a polyline with the given number of points in the given bounds."
  (let ((poly (make-instance 'polyline)))
    (dotimes (i count)
      (append-point poly (random-point-in-rectangle rect)))
    poly))

(defmethod make-polyline-from-points ((points vector))
  "Create a polyline with the given points."
  (let ((poly (make-instance 'polyline)))
    (loop for p across points
      do (append-point poly p))
    poly))

(defmethod distance ((p point) (poly polyline))
  "The distance from a point to a polyline."
  (cond ((= (length (points poly)) 0)
         nil) ;; Infinite distance? Zero?
        ((= (length (points poly)) 1)
         (distance p (aref (points poly) 0)))
        (t ;; More than 1 point
         (let ((distance-to-poly nil)
               (pts (points poly)))
           (do ((i 1 (+ i 1)))
               ((= i (length pts)))
             (let ((d (distance-point-line p
                                           (aref pts (- i 1))
                                           (aref pts i))))
               (if (or (not distance-to-poly)
                       (< d distance-to-poly))
                   (setf distance-to-poly d))))
           distance-to-poly))))

(defmethod highest-leftmost-point ((poly polyline))
  "The highest point, or highest and leftmost point (if several are highest)."
  (let* ((the-points (points poly))
         (highest (aref the-points 0)))
    (dotimes (i (length the-points))
      (let ((pt (aref the-points i)))
        (if (or (> (y pt) (y highest))
                (and (= (y highest) (y pt))
                     (< (x highest) (x pt))))
            (setf highest pt))))
    highest))

(defmethod area ((poly polyline))
  "Get the area of the POLYGON"
  ;; Cleanme!
  (if (< (length (points poly)) 3)
      0.0
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (area 0.0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (setf area (+ area
                          (* (x (aref pts i))
                             (y (aref pts j)))))
            (setf area (- area
                          (* (y (aref pts i))
                             (x (aref pts j)))))))
        (setf area (/ area 2.0))
        (abs area))))

(defmethod contains ((poly polyline) (p point))
  "Find whether the POLYGON contains the point."
  ;; Count ray-poly-line intersections. Odd = inside, 0 or even = outside.
  (if (> (length (points poly)) 2)
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (ray-line (make-instance 'line
                                     :from p
                                     :to (translate-point 10000.0 0.0)))
            (crossings 0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (when (line-intersect-line-points ray-line
                                              (aref pts i)
                                              (aref pts j))
              (setf crossings(+ crossings 1)))))
        (oddp crossings))))

(defmacro do-poly-lines ((sym poly) &body body)
  "Apply fun to each line in the polyline, or not if there is only 1 point."
  (with-gensyms (poly-points previous-point current-point i)
   `(let ((,poly-points (points ,poly)))
      (when (> (point-count ,poly-points) 1)
        (let ((,previous-point (first-point ,poly-points))
              (,current-point nil)
              (,sym nil))
          (dotimes (,i (point-count ,poly-points))
            (setf ,current-point (aref ,poly-points (+ ,i 1)))
            (setf ,sym (make-instance 'line
                                      :from ,previous-point
                                      :to ,current-point))
            ,@body
            (setf ,previous-point ,current-point)))))))

(defmethod intersects ((l line) (poly polyline))
  "Find whether the line intersects or is contained by the polyline."
  ;; Currently only intersects
  (let ((result nil))
    (do-poly-lines (l2 poly)
      (when (intersects l l2)
        (setf result t)
        (return)))
    result))

(defmethod intersects ((poly1 polyline) (poly2 polyline))
  "Find whether the two POLYGONS intersect or contain each other."
  ;; Currently only intersects
  (dolist (p (points poly2))
          (when (contains poly1 p)
            (return t))))

(defmethod intersects ((rect rectangle) (poly polyline))
  "Find whether the polygon intersects or contains the rectangle."
  ;; Currently only intersects
  (dolist (p (points poly))
          (when (contains rect p)
            (return t))))

(defmethod convex-hull (the-points)
  "Get the convex hull of an array of points."
  (let* ((first-point (highest-leftmost-point the-points))
         (current-point first-point)
         (next-point nil)
         (hull (make-vector 10)))
    (vector-push-extend first-point hull)
    (loop until (and (not (eq next-point nil))
                     (eq next-point first-point))
          do (setf next-point
                   (point-with-all-left current-point the-points))
          (vector-push-extend next-point hull)
          (setf current-point next-point))
    (make-instance 'polyline :points hull)))

(defmethod adding-point-would-cause-self-intersection ((poly polyline)
                                                       (p point))
  "Check if adding the point to the polyline would make it self-intersect."
  (let ((l (make-instance 'line
                          :from (last-point poly)
                          :to p))
        (result nil))
    (do-poly-lines (ll poly)
             (when (intersects l ll)
               (setf result t)
               (break)))
    result))
