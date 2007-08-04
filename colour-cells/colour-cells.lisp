;;  draw-something.lisp -  The main lifecycle code for draw-something.
;;  Copyright (C) 2006  Rhea Myers rhea@myers.studio
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
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(in-package "COLOUR-CELLS")

(defconstant save-directory "./drawings/")

(defmethod generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil 
	    "~a~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A~a" 
	    save-directory
	    "drawing" year month date hours minutes seconds ".eps")))

(defmethod run ()
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting colour-cells.~%")
  (setf *random-state* (make-random-state t))
  ;;(format t "Random state: ~a.~%" (write-to-string *random-state*))
  (let ((filename (generate-filename))
	(image-width 7)
	(image-height 10)
	(cell-size 10)
	(file-path t))
    (ensure-directories-exist save-directory)
    (with-open-file (ps filename :direction :output
			:if-exists :supersede)
      (setf file-path (namestring ps))
      (write-eps-header (* image-width cell-size)
			(* image-height cell-size)
			:to ps)
      (generate-colours ps image-width image-height cell-size)
      (write-eps-footer :to ps))
    #+sbcl (sb-ext:run-program "/usr/bin/gv" (list file-path))))
    
(defconstant object-symbol-choices
  '(leaf vein blade branch flower tendril background))

(defmethod object-symbol ()
  (choose-one-of object-symbol-choices))

(defmethod generate-colours (ps along up cell-size)
  (let* ((scheme (make-colour-scheme object-symbol-choices 7 7 .3 .6))
	 (sv-spec-list (chooser-spec))
	 (applier (make-colour-scheme-applier scheme sv-spec-list))
	 (cell (make-instance 'rectangle :x 0 :y 0
			      :width cell-size :height cell-size)))
    (format t "sv-spec: ~a~%" sv-spec-list)
    (print-colour-scheme scheme)
    (format t "Colouring forms.~%")
    (dotimes (cell-y up)
      (setf (x cell) 0)
      (setf (y cell) (* cell-y cell-size))
      (dotimes (cell-x along)
	(setf (x cell) (* cell-x cell-size))
	(write-colour (choose-colour-for applier
					 (object-symbol))
		      :to ps)
	(write-rectfill cell :to ps)))))