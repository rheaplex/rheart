;;  colour.lisp -  Colour handling.
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

(defclass colour ()
  ((hue :accessor hue
	:initform 1.0
	:initarg :hue
	:documentation "The colour-wheel hue of the colour.")
   (saturation :accessor saturation
	:initform 1.0
	:initarg :saturation
	:documentation "The saturation of the colour.")
   (brightness :accessor brightness
	:initform 1.0
	:initarg :brightness
	:documentation "The brightness of the colour."))
  (:documentation "A colour"))

(defmethod random-colour ()
  "Make a random colour."
  (make-instance 'colour :hue (random 1.0) :saturation (random 1.0)
		 :brightness (random 1.0)))

(defmethod hsb-to-rgb ((col colour))
  "Convert the hue/saturation/brightness colour to RGB."
  (if (= (saturation col) 0)
      (values-list (list (brightness col) (brightness col) (brightness col)))
      (let* ((p (* (brightness col) 
		   (- 1.0 
		      (saturation col))))
	     (q (* (brightness col) 
		   (- 1.0
		      (* (saturation col) (hue col)))))
	     (tt (* (brightness col)
		   (- 1.0
		      (* (saturation col)
			 (- 1.0 (hue col)))))))
	(case (floor (* (hue col) 5.0))
	  ((0) (values-list (list (brightness col) tt p))) ;; Red
	  ((1) (values-list (list q (brightness col) p))) ;; Yellow
	  ((2) (values-list (list p (brightness col) tt))) ;; Green
	  ((3) (values-list (list p q (brightness col)))) ;; Cyan
	  ((4) (values-list (list tt p (brightness col)))) ;; Blue
	  ((5) (values-list (list (brightness col) p q))))))) ;; Magenta

(defmethod difference ((a colour) (b colour))
  "Decide how different two colours are. 0 .. approx 2.1"
  (+ (* (abs (- (hue a) 
		(hue b)))
	0.00028)
     (abs (- (saturation a) 
	     (saturation b)))
     (abs (- (brightness a) 
	     (brightness b)))))

#|(defmethod different-value ((target float) (separation float) (wrap float))
  "Make a value 0 .. wrap where value is < or > target +/- separation."
  (cond 
    ;; Range is from 0 .. target - separation
    ((> (+ target separation)
	wrap)
     (random (- target separation)))
    ;; Range is from target + separation .. wrap
    ((< (- target separation)
	0.0)
     (+ target 
	separation 
	(random (- wrap 
		   target 
		   separation))))
    ;; Range is either of the above
    (t
     ((mod (+ (+ target 
		 separation) 
	      (random (- wrap 
			 (* 2.0 
			    separation))))
      wrap)))))|#

