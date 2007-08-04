;; cybernetic.lisp : random image description generator
;;
;; Copyright (c) 2004 Rhea Myers, rhea@myers.studio
;;
;; This file is part of The Cybernetic Artwork Nobody Wrote ("Cybernetic").
;; 
;; Cybernetic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Cybernetic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant the-random-state (make-random-state t))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod generate-description ()
  "Describe a single (set of) figure(s) on a single ground."
  (let ((plural (amount)))
    (concatenate-string plural (shape plural)
			"on a" (ground) "ground.")))

(defmethod amount ()
  "Generate a quantity description."
  (choose-randomly '("A" "A pair of" "Some" "Many")))

(defmethod pluralise (object plurality)
  "Make a word plural if necessary."
  (if (equal plurality "A")
      object
    (concatenate 'string object "s")))

;; Appearance

(defmethod appearance ()
  "Generate the appearance of a figure."
  (concatenate-string (maybe #'texture :default "")
		      (colour)))

(defmethod texture ()
  "Choose a texture."
  (choose-randomly '("halftoned" "crosshatched" "scumbled" "glazed" "sketchy" 
		     "smooth")))

;; The texture definitions

(defparameter monochromes '("black" "grey" "white"))
(defparameter hues '("red" "orange" "yellow" "green" "blue" "purple"))
(defparameter colours '("magenta" "cyan" "brown" "pink" "turquoise" "mauve"))
(defparameter metals '("gold" "silver" "bronze" "platinum" "copper" 
		       "rust-coloured"))
(defparameter fabrics '("khaki" "cotton-coloured" "denim blue" 
			"suede-coloured"))
(defparameter naturals '("sky blue" "leaf green" "sea green" "sunset red"))
(defparameter artificials '("neon blue" "sunset yellow" "shocking pink" 
			    "non-repro blue" "blue-screen blue"))
(defparameter palettes (list monochromes hues colours metals fabrics naturals 
			     artificials))
(defparameter tone '("pale" "" "rich" "bright" "" "dark"))

(defmethod colour ()
  "Generate a colour description."
  (concatenate-string (choose-randomly tone)
		      (choose-randomly-deep palettes)))

;; Shape

(defmethod shape (plural)
  "Generate a shape description."
  (concatenate-string (shape-size)
		      (appearance)
		      (shape-form plural)))

(defmethod shape-size ()
  "Generate a size for the shape."
  (choose-randomly '("" "" "tiny" "small" "large" "massive")))

(defparameter geometric '("circle" "triangle" "square" "pentagon" "hexagon" 
			  "octagon"))
(defparameter form '("organic shape" "spiky shape" "irregular shape"))
(defparameter abstract-shapes (list geometric form))
(defparameter abstract-shape-treatment '("" "" "outlined"))
(defparameter building '("house" "skyscraper"))
(defparameter transport '("car" "aeroplane" "ship"))
(defparameter animal '("bird" "cat" "dog" "horse"))
(defparameter generic-shapes (list building transport animal))
(defparameter generic-shape-treatments '("" "" "" "silhouetted" "outlined" 
					 "abstracted"))

(defmethod shape-form (plural)
  "Generate a shape form description."
  (cond 
   ((> (random 1.0 the-random-state) 0.5)
    (concatenate-string (choose-randomly abstract-shape-treatment)
		 (pluralise (choose-randomly-deep abstract-shapes) plural)))
   (t
    (concatenate-string (choose-randomly generic-shape-treatments)
		 (pluralise (choose-randomly-deep generic-shapes) plural)))))

;; Ground

(defmethod ground ()
  "Generate a simple ground description."
  (appearance))

;; Utilities

(defmethod maybe (fun &key (probability 0.5) (default nil))
  "Call fun with aqrgs if random(0..1) is less than probability."
  (if (< (random 1.0 the-random-state) probability)
      (funcall fun)
    default))

(defmethod choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices) the-random-state) 
       choices))

(defmethod choose-randomly-deep (choices)
  "Choose one item from a list of lists."
  (choose-randomly (choose-randomly choices)))

(defmethod concatenate-string (&rest strings)
  "Concatenate a list of strings with an optional given prefix, separator and suffix."
  (let ((all (car strings)))
    (dolist (s (cdr strings))
      (when (not (equal s ""))
	(setf all (concatenate 'string all
			       (if (equal all "")
				   ""
				 " ") 
			       s))))
    all))
