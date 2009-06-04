;;  draw-something.lisp -  The main lifecycle code for draw-something.
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

(defparameter object-symbol-choices
  '(leaf vein blade branch flower tendril))

(defparameter all-object-symbols
  (cons 'background object-symbol-choices))

(defmethod object-symbol (obj)
  (choose-one-of object-symbol-choices))

(defmethod draw-something ()
  "Make the drawing data structures and create the image."
  (let ((the-drawing (make-drawing)))
    (make-composition-points the-drawing (random-range 8 42))
    (make-planes the-drawing (number-of-planes))
    (make-planes-skeletons the-drawing)
    (draw-planes-figures the-drawing)
    (colour-objects the-drawing all-object-symbols)
    the-drawing))

(defmethod run ()
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting draw-something.~%")
  (setf *random-state* (make-random-state t))
  ;;(format t "Random state: ~a.~%" (write-to-string *random-state*))
  (let ((the-drawing (draw-something)))
    (advisory-message "Finished drawing.~%")
    (write-svg the-drawing)
    (advisory-message "Finished draw-something.~%")))

(defun run-draw-something ()
  "An sbcl-specific wrapper to make draw-something useful as a script."
  (run)
  #+sbcl (sb-ext:run-program "gzip"
			     (list "--suffix" ".svgz ~a/*.svg" save-directory)
			     :wait nil)
  #+sbcl (quit))