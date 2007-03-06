;;  cell-matrix.lisp - A picture cell matrix.
;;  Copyright (C) 2006  Rhea Myers rhea@myers.studio
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

;; This version is much simpler than AARON: just a grid of figure references

(defclass cell ()
  ((figure :accessor figure
	   :initform nil
	   :documentation "The cell's figure.")
   (status :accessor status
	   :initform 'unmarked
	   :documentation "The cell's status."))
  (:documentation "A cell in the matrix."))

(defconstant cell-size 1)

(defmethod drawing-to-cell-x (x)
  "Convert a drawing x co-ordinate to a cell x co-ordinate."
  (floor (/ x cell-size)))

(defmethod drawing-to-cell-y (y)
  "Convert a drawing y co-ordinate to a cell y co-ordinate."
  (floor (/ y cell-size)))

(defmethod make-cell-matrix (the-drawing)
  "Make the picture cell matrix for the drawing"
  (let ((matrix-width (ceiling (/ (width (bounds the-drawing)) cell-size)))
	(matrix-height (ceiling (/ (height (bounds the-drawing)) cell-size))))
    (setf (cell-matrix the-drawing) 
	  (make-array (list matrix-height matrix-width)
		      :element-type 'cell))
    (dotimes (i matrix-height)
      (dotimes (j matrix-width)
	(setf (aref (cell-matrix the-drawing) i j)
	      (make-instance 'cell))))))

(defmethod apply-line (fun x0 y0 x1 y1)
  "Call fun along the bresenham line co-ordinates between the two points."
  ;;(declare (optimize speed 3))
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
     (when steep
         (rotatef x0 y0)
         (rotatef x1 y1))
     (let* ((deltax (abs (- x1 - x0)))
	    (deltay (abs (- y1 y0)))
	    (error-value 0)
	    (deltaerr deltay)
	    (x x0)
	    (y y0)
	    (xstep (if (< x0 x1) 1 -1))
	    (ystep (if (< y0 y1) 1 -1))
	    (apply-fun (if steep
			   (lambda (x y) (funcall fun y x))
			   (lambda (x y) (funcall fun x y)))))
       (declare (type integer deltax deltay error-value deltaerr x y xstep 
		      ystep))
       (funcall apply-fun x y)
       (loop while (/= x x1)
	  do (progn (setf x (+ x xstep))
		    (setf error-value (+ error-value deltaerr))
		    (when (> error-value deltax)
		      (setf y (+ y ystep))
		      (setf error-value (- error-value deltax)))
		    (funcall apply-fun x y))))))

(defmethod cell-figure ((cells array) x y)
  "Get the cell's figure."
  (figure (aref cells x y)))

(defmethod cell-figure ((d drawing) x y)
  "Get the cell's figure."
  (cell-figure (cell-matrix d)
	       (drawing-to-cell-x x)
	       (drawing-to-cell-y y)))

(defmethod set-cell-figure ((cells array) x y fig role)
  "Set the cell to the figure. x and y are in cell co-ordinates."
  (let ((the-cell (aref cells x y))
	(setf (figure the-cell fig))
	(setf (role the-cell role)))))

(defmethod set-cell-figure ((d drawing) x y fig role)
  "Set the cell to the figure. x and y are in cell co-ordinates."
  (setf (cell-matrix d)
	(drawing-to-cell-x x)
	(drawing-to-cell-y y)
	fig
	role))

(defmethod mark-polyline-outline-cells (cells polyline fig role)
  "Mark the cells that the polyline passes through."
  (let* ((pts (points polyline))
	 (numpts (length pts)))
    (cond
      ((= numpts 0) nil)
      ((= numpts 1) (set-cell-figure cells 
				      (drawing-to-cell-x (x (first pts)))
				      (drawing-to-cell-y (y (first pts)))
				      fig
				      role))
      (t (let ((fun (lambda (x y) 
			 (set-cell-figure cells x y fig role))))
		 (dotimes (i (- numpts 1))
		   (let ((p1 (aref pts i))
			 (p2 (aref pts (mod (1+ i) numpts))))
		     (apply-line fun 
				 (drawing-to-cell-x (x p1))
				 (drawing-to-cell-y (y p1))
				 (drawing-to-cell-x (x p2))
				 (drawing-to-cell-y (y p2))))))))))

(defmethod scanline-intersections (cells fig x width y)
  "Find where the scanline intersects the outline."
  (let ((intersections (make-vector 4)))
    (do ((i x (1+ i)))
	((> i (+ x width)))
      (when (equal (aref cells (+ x i) y) fig)
	(vector-push-extend i intersections)))
    intersections))

(defmethod fill-scanline-intersections (cells fig role intersections y)
  "If there is more than one intersection, fill between the pairs.
    This is OK as we are filling closed outlines, not open self-intersecters."
  (when (> (length intersections) 1)
    (loop for i from 0 by 2 below (length intersections)
	  do (apply-line (lambda (x y) (set-cell-figure cells x y fig role))
			 (nth i intersections)
			 y
			 (nth (1+ i) intersections)
			 y))))
  
(defmethod mark-polyline-fill-cells (cells poly fig role)
  "Flood fill inside the figure."
  (let ((poly-x (drawing-to-cell-x (x (bounds poly))))
	(poly-y (drawing-to-cell-y (y (bounds poly)))) 
	(poly-width (drawing-to-cell-x (width (bounds poly))))
	(poly-height (drawing-to-cell-x (height (bounds poly)))))
  (do ((i poly-y (1+ i)))
      ((> i (+ poly-y poly-height)))
    (fill-scanline-intersections cells fig role
				 (scanline-intersections cells fig
							 poly-x
							 poly-width
							 i)))))

(defmethod colour-for-cell (cells x y)
  "Get the colour for a cell."
  (fill-colour (cell-figure (cell cells x y))))

(defun write-cells-ppm (cells filename comment)
  "Write the drawing to a ppm file."
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (format stream "P6~%#~A~%~D ~D~%~d~%" comment width height 255)
    (dotimes (i (array-dimension cells 1))
      (dotimes (j (array-dimension cells 0))
	(let ((col (colour-for-cell cells i j)))
	  (write-byte (normal-to-255 (red col)) stream)
	  (write-byte (normal-to-255 (green col)) stream)
	  (write-byte (normal-to-255 (blue col)) stream))))
    (namestring stream)))