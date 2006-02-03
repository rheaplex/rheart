;;  geometry.lisp - Basic geometry stuff.
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

(defconstant radian (* pi 2.0)
  "One radian.")

(defconstant %radians-to-degrees (/ radian 360.0)
  "The value to multiple radians by to get degrees.")

(defmethod radians-to-degrees (radians)
  "Convert a value in radians to a huamn-readable value in degrees. :-)"
  (/ radians %radians-to-degrees))

(defconstant radians-to-t-ratio (/ 1.0 (* pi 2.0)))

(defmethod radians-to-t (r)
	"Convert the value in radians to a value from 0.0 to 1.0"
	(* r radians-to-t-ratio))