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

(load "../library/ptester.lisp")
(use-package 'ptester)
(load "../library/utilities.lisp")
(load "../library/clopt.lisp")

(load "./geometry.lisp")
(load "./postscript.lisp")
(load "./drawing.lisp")


(defmethod maybe-help ()
  "Check the command line for the --help flag and act accordingly."
  (when (get-arg "help")
    (format t "Draw Something~%Rhea Myers (C) 2004~%usage: <lisp> run.lisp <options> where option is:~%    -d, --draw : the shape to draw around, skeleton or hull (default: hull)~%~%")
    (quit)))

(defmethod run ()
  "Get the number of descriptions to generate and generate them."
  (maybe-help)
  (let ((what (get-arg-value "draw" 
			     :short "d" 
			     :default "hull")))
    (draw-something (equal what "hull"))))

(defmethod draw-something (draw-hull &key (name "./drawing.eps"))
  "The main method that generates the drawing and writes it to file."
  (debug-message "Drawing something.")
  ;; Set the random number generator seed to a random value
  (setf *random-state* (make-random-state t))
  (debug-message "Generating skeleton.")
  (let* ((rect (make-instance 'rectangle
			      :x 10.0 :y 10.0 :width 400 :height 400))
	 (skeleton (random-points-in-rectangle rect 10)))
    (debug-message "Finished generating skeleton.")
    (when draw-hull
      (debug-message "Generating convex hull.")
      (setf skeleton (convex-hull skeleton))
      (debug-message "Finished generating convex hull."))
  ;; Write the debugging image
  (with-open-file (ps "./debug.eps" 
		      :direction :output
		      :if-exists :supersede)
		  (write-eps-header 420 420 :to ps)
		  (write-rgb 0.4 0.4 1.0 :to ps)
		  (write-new-path :to ps)
		  (write-subpath (points skeleton) :to ps)
		  ;; (write-close-path)
		  (write-stroke-path :to ps)
		  ;; Write the first segment
		  (write-rgb 1.0 0.0 0.0 :to ps)
		  (write-new-path :to ps)
		  (write-subpath (list (car (points skeleton)) 
				       (cadr (points skeleton))) :to ps)
		  (write-stroke-path :to ps)
		  (write-eps-footer :to ps))
      (let ((the-hand (make-instance 'hand :distance 5.0 :speed 5.0)))
	(if draw-hull
	    (debug-message "Drawing around convex hull.")
	  (debug-message "Drawing around skeleton."))
	(let ((drawing (draw-around skeleton the-hand)))
	  (if draw-hull
	      (debug-message "Finished drawing around convex hull.")
	    (debug-message "Finished drawing around skeleton."))
	  ;; Write the drawing
	  (debug-message "Writing drawing to file.")
	  (with-open-file (ps name 
			      :direction :output
			      :if-exists :supersede)
			  (write-eps-header 420 420 :to ps)
			  (write-rgb 0.1 0.1 0.1 :to ps)
			  (write-new-path :to ps)
			  (write-subpath drawing :to ps)
			  (write-stroke-path :to ps)
			  (write-eps-footer :to ps))
	  (debug-message "Finished writing drawing to file.")
	  (debug-message "Finished drawing something.")
	  (quit)))))

;; Run the tests
(run-tests)

;; Call the main drawing routine
(run)