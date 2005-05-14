;;  run.lisp -  A toy aesthetics description and evaluation system
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(in-package "DRAW-SOMETHING")

(defmethod run ()
  "Get the number of descriptions to generate and generate them."
    (draw-something nil))

(defmethod write-debug-image (skeleton)
  "Write the debugging image."
  (with-open-file (ps "./debug.eps" 
		      :direction :output
		      :if-exists :supersede)
    (write-eps-header 420 420 :to ps)
    (write-rgb 0.4 0.4 1.0 :to ps)
    (write-new-path :to ps)
    (write-subpath (points skeleton) :to ps)
    ;; (write-close-path)
    (write-stroke :to ps)
    ;; Write the first segment
    (write-rgb 1.0 0.0 0.0 :to ps)
    (write-new-path :to ps)
    (write-subpath (vector (aref (points skeleton) 0) 
			   (aref (points skeleton) 1)) :to ps)
    (write-stroke :to ps)
    (write-eps-footer :to ps)))

(defmethod write-drawing (name drawing)
  "Write the drawing"
  (debug-message "Writing drawing to file.")
  (with-open-file (ps name 
		      :direction :output
		      :if-exists :supersede)
    (write-eps-header 420 420 :to ps)
    (write-rgb (random 1.0) (random 1.0) (random 1.0) :to ps)
    (write-rectfill 0 0 420 420 :to ps)
    (write-rgb (random 1.0) (random 1.0) (random 1.0) :to ps)
    (write-new-path :to ps)
    (write-subpath drawing :to ps)
    (write-fill :to ps)
    (write-eps-footer :to ps))
  (debug-message "Finished writing drawing to file."))

(defmethod draw-around-skeleton (draw-hull skeleton)
  "Actually draw arounf the guide shape"
  (let ((the-pen (make-instance 'pen :distance 5.0 :speed 2.0)))
    (if draw-hull
	(debug-message "Drawing around convex hull.")
	(debug-message "Drawing around skeleton."))
    (let ((drawing (draw-around skeleton the-pen)))
      (if draw-hull
	  (debug-message "Finished drawing around convex hull.")
	  (debug-message "Finished drawing around skeleton."))
      drawing)))

(defmethod generate-polyline ()
  "Generate the skeleton"
  (debug-message "Generating skeleton.")
  (let* ((rect (make-instance 'rectangle
			      :x 10.0 
			      :y 10.0 
			      :width 400 
			      :height 400))
	 (skeleton (random-points-in-rectangle rect 
					       12)))
    (debug-message "Finished generating skeleton.")
    (make-instance 'polyline 
		   :points skeleton)))

(defmethod generate-hull (skeleton)
  "Make the convex hull for the skeleton."
  (debug-message "Generating convex hull.")
  (let ((hull (convex-hull skeleton)))
    (debug-message "Finished generating convex hull.")
    hull))

(defmethod generate-skeleton (draw-hull)
  (let ((skeleton (generate-polyline)))
    (if draw-hull
	(generate-hull (points skeleton))
	skeleton)))

(defmethod draw-something (draw-hull &key (name "./drawing.eps"))
  "The main method that generates the drawing and writes it to file."
  (debug-message "Drawing something.")
  ;; Set the random number generator seed to a random value
  (setf *random-state* (make-random-state t))
  (let ((skeleton (generate-skeleton draw-hull)))
    (write-debug-image skeleton)
    (let ((drawing (draw-around-skeleton draw-hull skeleton)))
      (write-drawing name drawing)))
  (debug-message "Finished drawing something.")
  ;(quit)
  )

;; Run the tests
;;(run-tests)

;; Call the main drawing routine
;;(run)
