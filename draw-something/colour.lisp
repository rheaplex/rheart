;;  colour.lisp -  Colour handling.
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
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

(defmethod hsb-to-rgb ((col colour))
  "Convert the hue/saturation/brightness colour to RGB."
  (if (= (saturation col) 0)
      (list (value col) (value col) (value col))
      (let* ((h (/ (mod (hue col) 
			360.0) 
		   60))
	     (i (floor h))
	     (f (- h i))
	     (p (* v 
		   (- 1.0 
		      (saturation col))))
	     (q (* v 
		   (- 1.0
		      (* s f))))
	     (t (* v
		   (- 1.0
		      (* s
			 (- 1.0 f))))))
	(case i
	  ((0) (list v t p)) ;; Red
	  ((1) (list q v p)) ;; Yellow
	  ((2) (list p v t)) ;; Green
	  ((3) (list p q v)) ;; Cyan
	  ((4) (list t p v)) ;; Blue
	  ((5) (list v p q)))))) ;; Magenta

(defmethod difference ((a colour) (b colour))
  "Decide how different two colours are. 0 .. approx 2.1"
  (+ (* (abs (- (hue a) 
		(hue b)))
	0.00028)
     (abs (- (saturation a) 
	     (saturation b)))
     (abs (- (brightness a) 
	     (brightness b))))

(defmethod different-value ((target float) (separation float) (wrap float))
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
      wrap)))))

(defmethod make-different ((col colour))
  ;; Which property(ies?) 1..3
  ;; make & return
    
)  

same, opposite, similar/adjacent, different


