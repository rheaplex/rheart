;;  svg.lisp - Writing SVG to streams.
;;  Copyright (C) 2007  Rhea Myers rhea@myers.studio
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

(defvar *svg-stream* t)

(defmethod svg-header (width height &key (to *svg-stream*))
  "Write the start of the SVG file."
  (format to "<?xml version=\"1.0\" standalone=\"no\"?>~%")
  (format to "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format to "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%")
  (format to "<svg width=\"~dpx\" height=\"~dpx\" viewBox=\"0 0 ~d ~d\"~%"
          width height width height)
  (format to "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">~%"))

(defmethod svg-footer (&key (to *svg-stream*))
  "Write the end of the svg file."
  (format to "</svg>~%"))

(defmethod svg-rgb ((col colour) )
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (format nil "#~X2~X2~X2"  (round (* r 15)) 
	    (round (*  b 15)) (round (* b 15)))))

(defmethod svg-path-tag-start (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to "<path"))

(defmethod svg-path-tag-end (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to " />~%"))

(defmethod svg-path-d-start (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to " d=\""))

(defmethod svg-path-d-end (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to "\""))

(defmethod svg-fill ((col colour) &key (to *svg-stream*))
  "Write the fill property."
  (format to " fill=\"~a\" " (svg-rgb col)))

(defmethod svg-stroke ((col colour) &key (to *svg-stream*))
  "Write the stroke property."
  (format to " stroke=\"~a\" " (svg-rgb col)))

(defmethod svg-close-path (&key (to *svg-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to " z"))

(defmethod svg-moveto (x y &key (to *svg-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to " M ~,3F ~,3F" x y))

(defmethod svg-lineto (x y &key (to *svg-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to " L ~,3F ~,3F" x y))

(defmethod svg-subpath (points &key (to *svg-stream*))
  "Write a subpath of a PostScript path."
  (write-moveto (x (aref points 0))
                (y (aref points 0))
                :to to)
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (svg-lineto (x (aref points i))
                  (y (aref points i))
                  :to to)))

(defmethod svg-rectfill ((rect rectangle) (col colour) &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
          "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" fill=\"~a\" />~%"
          (x rect) (y rect) (width rect) (height rect) (svg-rgb col)))

(defmethod svg-rectstroke ((rect rectangle) (col colour)
                             &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
   "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" stroke=\"~a\" />~%"
   (x rect) (y rect) (width rect) (height rect) (svg-rgb col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod svg-form-skeleton ((f form) ps)
  "Write the skeleton the drawing is made around."
  (svg-path-tag-start :to ps)
  (svg-stroke (make-instance 'colour
                                   :hue 0.3
                                   :saturation 0.6
                                   :brightness 0.6)
                    :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (skeleton f)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defmethod svg-form-fill ((f form) ps)
  "Write the drawing outline."
  (svg-path-tag-start :to ps)
  (svg-fill (fill-colour f)
                  :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (outline f)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defmethod svg-form-stroke ((f form) ps)
  "Write the drawing outline."
 (svg-path-tag-start :to ps)
  (svg-stroke (make-instance 'colour
                                   :hue 0.0
                                   :saturation 0.0
                                   :brightness 0.0)
                    :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (outline f)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defmethod svg-form ((f form) ps)
  "Write the form."
  (svg-form-fill f ps)
  ;;(svg-figure-skeleton fig ps)
  ;;(svg-form-stroke f ps)
  )

(defmethod svg-figure ((fig figure) ps)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(svg-rgb 0.0 0.0 0.0 :to ps)
  ;;(svg-rectstroke (bounds fig) :to ps)
  ;;(svg-stroke :to ps)
  (loop for fm across (forms fig)
       do (svg-form fm ps)))

(defmethod svg-ground ((the-drawing drawing) ps)
  "Colour the drawing ground."
  (svg-rectfill (bounds the-drawing) (ground the-drawing)
                  :to ps))

(defmethod svg-frame ((the-drawing drawing) ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (svg-rectstroke (inset-rectangle (bounds the-drawing) -1)
                    (make-instance 'colour :brightness 0.0)
                    :to ps))

(defmethod svg-write-drawing ((name string) (the-drawing drawing))
  "Write the drawing"
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (ps name :direction :output
                      :if-exists :supersede)
    (svg-header (width (bounds the-drawing))
                (height (bounds the-drawing))
                      :to ps)
    (svg-ground the-drawing ps)
    ;;(svg-frame the-drawing ps)
    (loop for plane across (planes the-drawing)
       do (loop for fig across (figures plane)
                   do (svg-figure fig ps)))
    (svg-footer :to ps)
    (namestring ps)))

(defmethod svg-display-drawing (filepath)
  "Show the drawing to the user in the GUI."
  (let ((command
         #+(or macos macosx darwin) "/usr/bin/open"
         #-(or macos macosx darwin) "/usr/bin/iceweasel"))
    #+sbcl (sb-ext:run-program command (list filepath) :wait nil)
    #+openmcl (ccl::os-command (format nil "~a ~a" command filepath))))

(defmethod write-and-show-svg ((the-drawing drawing))
  "Write and display the drawing as an svg file."
  (advisory-message "Saving and viewing drawing as svg.~%")
  (svg-display-drawing (svg-write-drawing (generate-filename ".svg")
                                          the-drawing)))
