();;  load.lisp - Load the asdf system.
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
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

;;(load "draw-something.asd")
;;(asdf:operate 'asdf:load-op :draw-something)

(load "utilities.lisp")
(load "geometry.lisp")
(load "point.lisp")
(load "line.lisp")
(load "circle.lisp")
(load "arc.lisp")
(load "rectangle.lisp")
(load "polyline.lisp")
(load "colour.lisp")
(load "colouring-new.lisp")
(load "turtle.lisp")
(load "pen.lisp")
(load "form.lisp")
(load "figure.lisp")
(load "drawing.lisp")
;;(load "cell-matrix")
;;(load "find-space")
(load "composition.lisp")
(load "plane.lisp")
(load "postscript.lisp")
(load "svg.lisp")
(load "draw-something.lisp")
