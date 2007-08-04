;;  run.lisp - Load the asdf system and make a drawing
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
;;
;; This file is part of colour-cells.
;; 
;; colour-cells is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; colour-cells is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(load "draw-something.asd")
(asdf:operate 'asdf:load-op :draw-something)
(draw-something:run)  

(quit)