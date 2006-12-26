;;  postscript.lisp - Writing PostScript to streams.
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

(in-package "COLOUR-CELLS")

(defvar *ps-stream* t)

(defmethod write-eps-header (width height &key (to *ps-stream*))
  "Write the standard raw PostScript header."
  (format to "%!PS-Adobe-3.0 EPSF-3.0~%")
  (format to "%%BoundingBox: 0 0 ~D ~D~%" width height))

(defmethod write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional PostScript footer"
  (format to "%%EOF~%"))

(defmethod write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defmethod write-colour ((col colour) &key (to *ps-stream*))
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (write-rgb r g b :to to)))

(defmethod write-rectfill ((rect rectangle) &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" (x rect) (y rect) (width rect) 
	  (height rect)))