;;  package.lisp - The main package for colour-cell
;;  Copyright (C) 2006 Rhea Myers rhea@myers.studio
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

(in-package "CL-USER")

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage COLOUR-CELLS
	(:documentation
	 "The colour-cells package.")
	(:use common-lisp)
	(:export run)))