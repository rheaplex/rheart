;;  postscript.lisp - Writing PostScript to streams.
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

(defvar *ps-stream* t)

(defmethod write-eps-header (width height &key (to *ps-stream*))
  "Write the standard raw PostScript header."
  (format to "%!PS-Adobe-3.0 EPSF-3.0~%")
  (format to "%%BoundingBox: 0 0 ~a ~a~%" width height)
  (format to "/L {lineto} bind def~%/M {moveto} bind def~%"))

(defmethod write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional PostScript footer"
  (format to "%%EOF~%"))

(defmethod write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defmethod write-colour ((col colour) &key (to *ps-stream*))
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (write-rgb r g b :to to)))

(defmethod write-fill (&key (to *ps-stream*))
  "Write the fill operator."
  (format to "fill~%"))

(defmethod write-close-path (&key (to *ps-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to "closepath~%"))

(defmethod write-stroke (&key (to *ps-stream*))
  "Stroke the current PostScript path."
  (format to "stroke~%"))

(defmethod write-fill (&key (to *ps-stream*))
  "Fill the current PostScript path."
  (format to "fill~%"))

(defmethod write-new-path (&key (to *ps-stream*))
  "Start a new PostScript path."
  (format to "newpath~%"))

(defmethod write-moveto (x y &key (to *ps-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F M " x y))

(defmethod write-lineto (x y &key (to *ps-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F L " x y))

(defmethod write-subpath (points &key (to *ps-stream*))
  "Write a subpath of a PostScript path."
  (write-moveto (x (aref points 0)) 
		(y (aref points 0))
		:to to)
  (do ((i 1 (+ i 1))) 
      ((= i (length points)))
    (write-lineto (x (aref points i)) 
		  (y (aref points i)) 
		  :to to)))

(defmethod write-rectfill ((rect rectangle) &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" (x rect) (y rect) (width rect) 
	  (height rect)))


(defmethod write-rectstroke ((rect rectangle) &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectstroke~%" (x rect) (y rect) (width rect) 
	  (height rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-form-skeleton ((f form) ps)
  "Write the skeleton the drawing is made around."
  (write-rgb 0.4 0.4 1.0 :to ps)
  (write-new-path :to ps)
  (write-subpath (points (skeleton f)) :to ps)
  (write-stroke :to ps))

(defmethod write-form-fill ((f form) ps)
  "Write the drawing outline."
  (write-colour (fill-colour f) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline f)) :to ps)
  (write-fill :to ps))

(defmethod write-form-stroke ((f form) ps)
  "Write the drawing outline."
  (write-rgb 0.0 0.0 0.0 :to ps)
  ;;(write-rectstroke (bounds f) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline f)) :to ps)
  (write-stroke :to ps))

(defmethod write-form ((f form) ps)
  "Write the form."
  (write-form-fill f ps)
  ;;(write-figure-skeleton fig ps)
  ;;(write-form-stroke f ps)
  )

(defmethod write-figure ((fig figure) ps)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(write-rgb 0.0 0.0 0.0 :to ps)
  ;;(write-rectstroke (bounds fig) :to ps)
  ;;(write-stroke :to ps)
  (loop for fm across (forms fig)
       do (write-form fm ps)))

(defmethod write-ground ((the-drawing drawing) ps)
  "Colour the drawing ground."
  (write-colour (ground the-drawing) :to ps)
  (write-rectfill (bounds the-drawing) :to ps))

(defmethod write-frame ((the-drawing drawing) ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (write-rectstroke (inset-rectangle (bounds the-drawing) -1)
		    :to ps))
