;;  composition.lisp - Generating an image with some kind of intent.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating the point population for the composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-composition-points ((d drawing) count)
  "Generate the points on the image plane that the composition will use."
  (let* ((b (bounds d))
	 (corner-count (random 4))
	 (interior-count (random (- count 
				    corner-count)))
	 (edge-count (- count 
			interior-count 
			corner-count)))
    (setf (composition-points d)
	  (concatenate 'vector
		       (random-points-at-rectangle-corners b corner-count)
		       (random-points-on-rectangle b edge-count)
		       (random-points-in-rectangle b interior-count)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The various point combining methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	
(defmethod make-hull-figures (the-points count low-count high-count)
  "Make count hull figures. They may overlap or touch. Todo: prevent this."
  (let* ((count (random-range low-count
			      high-count))
	 (hulls (make-vector count)))
    (map-into hulls
	      (lambda () 
		(make-figure-from-points
		 (points (convex-hull
			  (choose-n-of count
				       the-points))))))))

(defconstant min-hulls-per-plane 4)
(defconstant max-hulls-per-plane 8)
(defconstant min-hull-points 3)
(defconstant max-hull-points 12)

(defmethod make-hull-figures-for-plane (points)
  "The plane population policy using hulls. "
  (make-hull-figures points 
			(random-range min-hulls-per-plane
				      max-hulls-per-plane)
			min-hull-points
			max-hull-points))
	
(defmethod make-polygon-figures (points count low-count high-count)
  "Make count polygon figures. They may overlap or touch. Todo: prevent this."
  (let ((polygons (make-vector count)))
    (map-into
     polygons
     (lambda () 
       (make-figure-from-points (choose-n-of (random-range low-count
							   high-count)
					     points))))))

(defconstant min-polygons-per-plane 4)
(defconstant max-polygons-per-plane 8)
(defconstant min-polygon-points 3)
(defconstant max-polygon-points 12)

(defmethod make-polygon-figures-for-plane (points)
  "The plane population policy using polygons. "
  (make-polygon-figures points 
			(random-range min-polygons-per-plane
				      max-polygons-per-plane)
			min-polygon-points
			max-polygon-points))

(defmethod make-line-figure (points)
  "Make a line figure using two of the points."
  (let ((p1p2 (choose-n-of 2 points)))
    (make-instance 'figure
		   :forms 
		   (make-instance 'form
				  :contents 
				  (make-instance 'line
						 :from (first p1p2)
						 :to (second p1p2))))))
	
(defmethod make-line-figures (points count)
  "Make count line figures. They may overlap or touch. Todo: ensure they don't."
  (let ((lines (make-vector count)))
    (map-into lines
	      (lambda () (make-figure-from-points (choose-n-of 2 points))))))

(defconstant min-lines-per-plane 1)
(defconstant max-lines-per-plane 12)

(defmethod make-line-figures-for-plane (points)
  "The plane population policy using liness. "
  (make-line-figures points 
		     (random-range min-lines-per-plane
				   max-lines-per-plane)))

(defmethod make-point-figure (point)
  "Make a point figure."
  (make-instance 'figure
		 :forms (make-instance 'form
				       :contents point)))

(defmethod make-point-figures (points count)
  "Make count point figures."
  (let ((source-points (choose-n-of count points))
	(point-figures (make-vector count)))
    (loop for p across source-points
	  do (vector-push-extend (make-figure-from-points (vector p)) 
				 point-figures))
    point-figures))

(defconstant min-points-per-plane 1)
(defconstant max-points-per-plane 20)

(defmethod make-point-figures-for-plane (points)
  "The plane population policy using points. "
  (make-point-figures points 
		      (random-range min-points-per-plane
				    max-points-per-plane)))

(defconstant figure-generation-method-list
  '(make-hull-figures-for-plane
    make-polygon-figures-for-plane
    make-line-figures-for-plane
    make-point-figures-for-plane))

(defmethod figure-generation-methods (count)
  (choose-n-of-ordered count figure-generation-method-list))