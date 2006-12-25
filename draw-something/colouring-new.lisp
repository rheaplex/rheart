;;  colour-scheme.lisp -  Colour scheme generation and application.
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

(defconstant minimum-spec-probability 3)
(defconstant maximum-spec-probability 9)
(defconstant minimum-spec-count 2)
(defconstant maximum-spec-count 3)
(defconstant sv-spec-options '('ll 'lm 'lh 'ml 'mm 'mh 'hl 'hm 'hh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LMH - Low medium and high value ranges from 0.0..1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lmh-values ()
  ((medium-start :type real
		 :initarg :medium 
		 :initform 0.33
		 :accessor medium-value-start)
   (high-start :type real
	       :initarg :high 
	       :initform 0.33
	       :accessor high-value-start)
   (lows :type vector 
	 :initform (make-vector 7)
	 :accessor low-values)
   (mediums :type vector 
	    :initform (make-vector 7)
	    :accessor medium-values)
   (highs :type vector 
	  :initform (make-vector 7)
	  :accessor high-values))
  (:documentation 
    "A range of values divided into low, medium and high values."))

(defmethod insert-lmh-low-value (lmh val)
  "Append a value to the list of low values in the lmh, unsorted."
  (vector-push-extend val (low-values lmh)))

(defmethod insert-lmh-medium-value (lmh val)
  "Append a value to the list of medium values in the lmh, unsorted."
  (vector-push-extend val (medium-values lmh)))

(defmethod insert-lmh-high-value (lmh val)
  "Append a value to the list of medium values in the lmh, unsorted."
  (vector-push-extend val (high-values lmh)))

(defmethod insert-lmh-value (lmh val)
  "Insert the value into the correct range list for the lmh."
  (cond 
   ((< val (medium-value-start lmh))
    (insert-lmh-low-value lmh val))
   ((< val (high-value-start lmh))
    (insert-lmh-medium-value lmh val))
   (t
    (insert-lmh-high-value lmh val))))
  
(defmethod insert-lmh-values (lmh values)
  "Insert a list of values into the correct range lists of the lmh."
  (dolist (value (sort values #'<))
   (insert-lmh-value lmh value)))

;; Ensure minimum separation for each value (read notes)?

(defmethod populate-lmh (lmh count)
  "Generate count values and insert them into the lmh."
  (assert (>= count 3))
  (insert-lmh-values lmh
		     (loop for i from 0 below count collect (random 1.0)))
  (if (or (eq (length (low-values lmh)) 0)
	  (eq (length (medium-values lmh)) 0)
	  (eq (length (high-values lmh)) 0))
      (populate-lmh lmh count)))

(defmethod make-lmh (count medium high)
  "Make the lmh and populate it with count values separated by medium & high."
  (let ((lmh (make-instance 'lmh-values
			       :medium medium
			       :high high)))
    (populate-lmh lmh 
		  count)
    lmh))

(defmethod random-low-value ((lmh lmh-values))
  "Randomly choose a value from the list of low values of the lmh."
  (choose-one-of (low-values lmh)))

(defmethod random-medium-value ((lmh lmh-values))
  "Randomly choose a value from the list of medium values of the lmh."
  (choose-one-of (medium-values lmh)))

(defmethod random-high-value ((lmh lmh-values))
  "Randomly choose a value from the list of medium values of the lmh."
  (choose-one-of (high-values lmh)))

(defmethod random-lmh-value (lmh which)
  "Return a random low ('l), medium ('m) or high ('h) value based on which."
  (case which
	('l (random-low-value lmh))
	('m (random-medium-value lmh))
	('h (random-high-value lmh))))

(defmethod print-lmh (lmh)
  (format t "low: ")
  (loop for l across (low-values lmh)
	do (format t "~a " l))
  (format t "~%medium: ")
  (loop for m across (medium-values lmh)
	do (format t "~a " m))
  (format t "~%high: ")
  (loop for h across (high-values lmh)
	do (format t "~a " h))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-scheme ()
  ((hues :type hashtable
	 :initarg :hues
	 :initform (make-hash-table)
	 :accessor colour-scheme-hues)
   (saturations :type lmh-values
		:initarg :saturations
		:initform (make-instance 'lmh-values)
		:accessor colour-scheme-saturations)
   (values :type lmh-values
	   :initarg :values
	   :initform (make-instance 'lmh-values)
	   :accessor colour-scheme-values))
  (:documentation "The values describing a colour scheme."))

(defmethod print-colour-scheme (scheme)
  (format t "Colour Scheme:~%")
  (format t "hues:~%")
  (maphash (lambda (key value)
	     (format t "~a ~a " key value))
	   (colour-scheme-hues scheme))
  (format t "~%saturations:~%")
  (print-lmh (colour-scheme-saturations scheme))
  (format t "values:~%")
  (print-lmh (colour-scheme-values scheme)))

(defmethod colour-scheme-hue-for (scheme hue-id)
  "Get the hue value from the scheme for a given id, eg :stem, :stalk."
  (gethash hue-id (colour-scheme-hues scheme)))

(defmethod colour-scheme-saturation-lmh (scheme spec)
  "Choose a saturation from the range specified by spec."
  (random-lmh-value (colour-scheme-saturations scheme)
		    spec))

(defmethod colour-scheme-value-lmh (scheme spec)
  "Choose a value from the range specified by spec."
  (random-lmh-value (colour-scheme-values scheme)
		    spec))

(defmethod sv-spec-components (spec)
  "Return each half of the symbol specifying how to choose saturation & value."
  (case spec
	('ll (values 'l 'l))
	('lm (values 'l 'm))
	('lh (values 'l 'h))
	('ml (values 'm 'l))
	('mm (values 'm 'm))
	('mh (values 'm 'h))
	('hl (values 'h 'l))
	('hm (values 'h 'm))
	('hh (values 'h 'h))))

(defmethod make-colour-by-sv-spec (scheme hue-id sv-spec)
  "Choose a colour for the hue id using the sv-spec eg 'lm, 'hh, 'ml."
  (multiple-value-bind 
	(saturation-spec value-spec) (sv-spec-components sv-spec)
    (make-instance 'colour 
		   :hue (colour-scheme-hue-for scheme hue-id)
		   :saturation (colour-scheme-saturation-lmh scheme
							     saturation-spec)
		   :brightness (colour-scheme-value-lmh scheme
							value-spec))))

;; Generate additive colour range

(defmethod make-hue-additive-series (hue-list)
  (let ((series (make-hash-table))
	(hue-value (random 1.0)))
    ;; Use loop? Can that gather into a hashtable?
    (dolist (hue-symbol hue-list)
      (setf hue-value (mod (+ hue-value (random 0.3)) 
			   1.0))
      (setf (gethash hue-symbol series)
	    hue-value))
    series))

(defmethod make-colour-scheme (hue-list saturations values medium high)
  "Make a colour scheme for the saturation symbol list."
  (make-instance 'colour-scheme
		 :hues (make-hue-additive-series hue-list)
		 :saturations (make-lmh saturations medium high)
		 :values (make-lmh values medium high)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying colour schemes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-scheme-applier ()
  ((scheme :type colour-scheme
	   :accessor applier-scheme
	   :initarg :scheme)
   (count :type integer
	  :initform 1
	  :accessor applier-count)
   (check :type integer
	  :initform 5
	  :initarg :check
	  :accessor applier-when-to-check)
   ;; This one is more part of the scheme but is more convenient here
   (sv-chooser :initarg :sv-chooser
	       :initform (lambda () 'll)
	       :accessor applier-sv-chooser)
   (sv-probabilites :type hashtable
		    :initform (make-hash-table)
		    :initarg :sv-probabilities
		    :accessor applier-probabilities)
   (sv-occurrunces :type hashtable
		   :initform (make-hash-table)
		   :accessor applier-occurences))
  (:documentation
    "The data used in applying a colour scheme to an image."))

(defmethod set-applier-probabilities (applier spec-list)
  "Set the probabilites from a list of num/specs, and set occurences to zero"
  (let ((total-prob (float (prefs-range spec-list))))
    (loop for prob in spec-list by #'cddr 
	  for spec in (cdr spec-list) by #'cddr
	  do (setf (gethash (dequote spec)
			    (applier-probabilities applier))
		   (/ prob total-prob))
	  do (setf (gethash (dequote spec)
			    (applier-occurences applier)) 0))))

(defmethod set-applier-chooser (applier spec-list)
  "Make and set the chooser function for sv specs from the list."
  (setf (applier-sv-chooser applier)
	(prefs-list-lambda spec-list)))
  
(defmethod set-applier-sv-spec (applier spec-list)
  "Configure the applier from the sv spec eg '(1 'hh 4 'ml 3 'lm)"
  (set-applier-chooser applier spec-list)
  (set-applier-probabilities applier spec-list))

;; Change to being an :after initialize-instance method
(defmethod make-colour-scheme-applier (scheme spec-list)
  "Make a colour scheme applier."
  (let ((applier (make-instance 'colour-scheme-applier
				:scheme scheme)))
    (set-applier-chooser applier spec-list)
    (set-applier-probabilities applier spec-list)
    applier))
       
(defmethod spec-probability-difference (applier spec)
  "Get the difference between the intended and actual occurence of a spec."
  (let* ((generation-count (float (applier-count applier)))
	 (spec-count (gethash spec (applier-occurences applier)))
	 (target (float (gethash spec (applier-probabilities applier))))
	 (current (/ spec-count generation-count))
	 (difference (- target current)))
    (format t "  ~a ~a ~,3F ~,3F ~,3F~%"
	    spec spec-count target current difference) 
    difference))

(defmethod most-deviant-spec (applier)
  "Find the spec that is being called the least compared to its probability."
  (let ((highest 0.0)
	(result nil))
    ;; Change to use loop
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (let ((difference (spec-probability-difference applier key)))
		   (when (> difference highest)
		     (setf highest difference)
		     (setf result key))))
	     (applier-probabilities applier))
    (format t "~a~%" result)
    result))

(defmethod increase-spec-count (applier spec)
  "Update the number of times a spec has been used."
  (incf (gethash spec (applier-occurences applier))))

(defmethod increase-applier-count (applier)
  "Increase the applier's count of objects it has been called for by 1."
  (incf (applier-count applier)))

(defmethod update-applier-state (applier spec)
  "Call when you've applied colour to an object & are ready for the next one."
  (increase-spec-count applier spec)
  (increase-applier-count applier))

(defmethod applier-should-correct (applier)
  "Decide whether the applier should correct the spec probability error."
  (eq (mod (applier-count applier) 
	   (applier-when-to-check applier))
      0))

(defmethod applier-spec-choice (applier hue-id)
  "Choose the spec to be used."
  (if (applier-should-correct applier)
      (most-deviant-spec applier)
    (funcall (applier-sv-chooser applier))))

(defmethod choose-colour-for (applier hue-id)
  "Choose a colour from the scheme for the hue-id."
  (let* ((spec (applier-spec-choice applier hue-id))
	 (choice (make-colour-by-sv-spec (applier-scheme applier)
					 hue-id spec)))
    (update-applier-state applier spec)
    (format t "colour: ~,3F ~,3F ~,3F~%"
	    (hue choice) (saturation choice) (brightness choice))
    choice))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to make and apply a colour scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chooser-spec ()
  "Construct a list describing a random spec pref, eg (6 'll 3 'hm 2 'lh)."
  (loop for spec in (choose-n-of (random-range-inclusive minimum-spec-count 
							 maximum-spec-count) 
				 sv-spec-options)
	collect (random-range-inclusive minimum-spec-probability 
					maximum-spec-probability)
	collect spec))

(defmethod colour-objects (drawing symbols)
  (let* ((scheme (make-colour-scheme symbols 7 7 .3 .6))
	 (sv-spec-list (chooser-spec))
	 (applier (make-colour-scheme-applier scheme sv-spec-list)))
    (print-colour-scheme scheme)
    (format t "sv-spec: ~a~%" sv-spec-list)
    (format t "Colouring forms.~%")
    (setf (ground drawing) 
	  (choose-colour-for applier
			     'background))
    (loop for figure across (figures drawing)
	  do (loop for form across (forms figure)
		   do (setf (fill-colour form) 
			    (choose-colour-for applier
					       (object-symbol form)))))))
