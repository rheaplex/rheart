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

(defvar *ps-stream* t)

(defmethod write-eps-header (width height &key (to *ps-stream*))
  "Write the standard raw PostScript header."
  (format to "%!PS-Adobe-3.0 EPSF-3.0~%")
  (format to "%%BoundingBox: 0 0 ~a ~a~%" width height))

(defmethod write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional PostScript footer"
  (format to "%%EOF~%"))

(defmethod write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defmethod write-close-path (&key (to *ps-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to "closepath~%"))

(defmethod write-stroke-path (&key (to *ps-stream*))
  "Stroke the current PostScript path."
  (format to "stroke~%"))

(defmethod write-new-path (&key (to *ps-stream*))
  "Start a new PostScript path."
  (format to "newpath~%"))

(defmethod write-moveto (x y &key (to *ps-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to "~F ~F moveto~%" x y))

(defmethod write-lineto (x y &key (to *ps-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to "~F ~F lineto~%" x y))

(defmethod write-subpath (points &key (to *ps-stream*))
  "Write a subpath of a PostScript path."
  (write-moveto (x (car points)) 
		(y (car points))
		:to to)
  (dolist (p (cdr points))
    (write-lineto (x p) 
		  (y p) 
		  :to to)))



