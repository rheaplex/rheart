;;  postscript.lisp - Writing PostScript to streams.
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

(defmethod write-rectfill (x y w h &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" x y w h))



