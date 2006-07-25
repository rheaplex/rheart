;; chordinator.lisp - Colour chord generation

;; Types and default values for keywords???
;; Min & max bounds for saturation & brightness?value

;; Gamut for inkjet?
;; Tween colours, esp. complements?

;; For colour: plane (fore/mid/back/etc), 
;; object (ground/table/tree), 
;; group (edges/top/legs/trunk/branches), 
;; element (branch1..20/leg1..4/leaf1..200)

;; So a red pot in the blue distance will need to compose the plane, 
;; object and element colour generators

(defvar acceptible-value-deviation 0.1)

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
	:documentation "The brightness (HSV) of the colour."))
  (:documentation "A colour"))
  
(defmethod combined-value ((col colour))
  "Get the value 0.0..1.0 by combining brightness and saturation."
  (/ (+ (brightness col)
	(- 1.0 (saturation col)))
     2.0))

;; Change to (write) methods

(defmethod dump ((col colour))
  (format t
	  "Colour hue: ~a saturation: ~a brightness ~a" 
	  (hue col)
	  (saturation col)
	  (brightness col)))

(defmethod dump (cols)
  (dolist (col cols)
    (dump col)
    (format t "~%")))
		
(defmethod fixed-generator (val)
  "Make a function to always return the same value."
  (lambda ()
    val))

(defmethod random-generator ((from float) (to float))
  "make a function to generate random numbers between from and to."
  (let ((base (min from to))
	(range (abs (- to from))))
    (lambda ()
      (+ base
	 (random range)))))

(defmethod step-generator ((from float) (to float) (steps integer))
  "Make a function to return a value that steps from from to to in steps steps."
  (let ((step (/ (abs (- to from))
		 steps))
	(i 0))
    (lambda ()
      (let ((current-value (+ (* i step)
			      from)))
	(incf i)
	current-value))))

(defmethod colour-generator (hue-fun saturation-fun brightness-fun)
  "Make a function to make a new instance of colour."
  (lambda ()
    (make-instance 'colour 
		   :hue (funcall hue-fun)
		   :saturation (funcall saturation-fun)
		   :brightness (funcall brightness-fun))))

(defmethod random-colour-generator (&key (min-hue 0.0) (max-hue 1.0) 
				    (min-saturation 0.0) (max-saturation 1.0)
				    (min-brightness 0.0) (max-brightness 1.0))
  "Make a function to make a random colour."
  (colour-generator (random-generator min-hue max-hue)
		    (random-generator min-saturation max-saturation)
		    (random-generator min-brightness max-brightness)))

(defmethod n-random-colours ((n integer) &key (min-hue 0.0) (max-hue 1.0) 
			     (min-saturation 0.0) (max-saturation 1.0)
			     (min-brightness 0.0) (max-brightness 1.0))
  "Make a list of n random colours."
  (let ((generate (random-colour-generator :min-hue min-hue 
					   :max-hue max-hue
					   :min-saturation min-saturation
					   :max-saturation max-saturation
					   :min-brightness min-brightness
					   :max-brightness max-brightness)))
    (loop repeat n
       collect (funcall generate))))

(defmethod hue-step-colour-generator ((n integer) 
				      &key (min-hue 0.0) (max-hue 1.0) 
				      (min-saturation 0.0) (max-saturation 1.0)
				      (min-brightness 0.0) (max-brightness 1.0))
  "Make a function to make colours with stepped hue, random sat / brightness."
  (colour-generator (step-generator min-hue max-hue n)
		    (random-generator min-saturation max-saturation)
		    (random-generator min-brightness max-brightness)))

(defmethod n-hue-step-colours ((n integer) &key (min-hue 0.0) (max-hue 1.0) 
			       (min-saturation 0.0) (max-saturation 1.0)
			       (min-brightness 0.0) (max-brightness 1.0))
  "Make a list of n colours equally space around hue, random sat / brightness."
  (let ((generate 
	 (hue-step-colour-generator n
				    :min-hue min-hue 
				    :max-hue max-hue
				    :min-saturation min-saturation
				    :max-saturation max-saturation
				    :min-brightness min-brightness
				    :max-brightness max-brightness))
	(i 0))
    (loop repeat n
       collect (funcall generate)
       do (incf i))))
       
(defmethod n-hue-step-colours-start-offset 
    ((n integer) &key (min-hue 0.0) (max-hue 1.0) 
			       (min-saturation 0.0) (max-saturation 1.0)
			       (min-brightness 0.0) (max-brightness 1.0))
  "Make a list of n colours equally space around hue, random sat / brightness."
  (let ((offset (random (- 1.0 
			   (min (- 1.0 ;; Smaller distance from range to limit
				   max-hue) 
				min-hue)))))
    (n-hue-step-colours n :min-hue (+ min-hue offset) 
			:max-hue (+ max-hue offset)
			:min-saturation min-saturation
			:max-saturation max-saturation
			:min-brightness min-brightness
			:max-brightness max-brightness)))
				    
;; Random in bounds 
;; Like step, but with each colour offset randomly from step with min gap

;; Additive series, use complements etc. but avoid already used colours

(defmethod list-property-range (items key)
  (let ((lowest (funcall key (car items)))
	(highest (funcall key (car items)))
	(count 0))
    (dolist (item items)
      (let ((val (funcall key item)))
	(if (> val highest)
	    (setf highest val))
	(if (< val lowest)
	    (setf lowest val)))
      (incf count))
    (values lowest highest (- highest lowest) count)))
    
(defmethod colours-brightness-range (colours)
  "Get the range of brightness for the colours in the list."
  (list-property-range colours #'brightness))
	
(defmethod colours-value-range (colours)
  "Get the range of value for the colours in the list."
  (list-property-range colours #'combined-value))

(defmethod converge-value ((col colour) (value-target real))
  "Get the colour as close as possible to the target value."
  nil)
#|  (let ((delta (- value-target
		  (combined-value col))))
    (when (> delta acceptible-value-deviation)
      ;; Try to change the brightness, but not too much
      ;; Avoid a change of > .2 if possible
      ;; If we'd have to change it too much, 
      ;;change the saturation as well
      ;; If both > 0.2, change brightness most
      ;; Or something
      (when (> (delta (brightness col))
	       (setf (brightness col 
				 )
		     ) |#
    
(defmethod dolist-index (items fun)
  "Iterate through items calling (fun item index-of-item) ."
  (let ((count 0))
    (dolist (item items)
      (funcall fun item count)
      (incf count))))
    
(defmethod make-linearise-property (colours key)
  "Make a function which when called with a colour and index will set the colour's brightness to its linear position in the range from darkest to lightest."
  (multiple-value-bind 
	(from to range steps) (list-property-range colours key)
    (declare (ignore to))
    (let ((step (/ range steps)))
      (lambda (col i)
	(setf (slot-value col key) 
	      (+ from 
		 (* i step)))))))

(defmethod linearise-brightness (colours)
  "Set each colour's brightness to its linear position in the range from darkest to lightest."
  (dolist-index colours (make-linearise-property colours 'brightness)))

(defmethod linearise-saturation (colours)
  "Set each colour's saturation to its linear position in the range."
  (dolist-index colours (make-linearise-property colours 'saturation)))
  
(defmethod make-linearise-value (colours)
  "Make a function which when called with a colour and index will set the colour's brightness to its linear position in the range from least to most saturated-and-bright."
  (multiple-value-bind (from to range count) (colours-value-range colours)
    (declare (ignore range))
    (let ((step (/ (- to from) ;; Get the range in value-range? Yes.
		   count)))
      (lambda (col i)
	(converge-value col (* i step))))))
      
(defmethod linearise-value (colours)
  "Make a function which when called with a colour and index will set the colour's brightness to its linear position in the range from least to most saturated-and-bright."
  (dolist-index colours (make-linearise-value colours)))
			    
(defmethod sort-by-brightness (colours)
  "Sort the colours from darkest to lightest."
  (stable-sort colours #'< :key #'brightness))
  		
(defmethod sort-by-value (colours)
  "Sort the colours into ascending value (brightness + saturation)."
  (stable-sort colours #'< :key #'combined-value))

(defmethod hsb-to-rgb ((col colour))
  "Convert the hue/saturation/brightness colour to RGB."
  (cond 
    ((= (saturation col) 0.0)
      (values (brightness col)  (brightness col) (brightness col)))
    ((= (brightness col) 0.0)
      (values (saturation col) (saturation col) (saturation col)))
    (t (let* ((s (saturation col))
	      (v (brightness col))
	      (j (* (hue col) 6.0))
	      (i (floor j))
	      (f (- j i))
	      (p (* v (- 1 s)))
	      (q (* v (- 1 (* s f ))))
	      (u (* v (- 1 (* s (- 1 f))))))
	 (case i
	   ((0) (values v u p))
	   ((1) (values q v p))
	   ((2) (values p v u))
	   ((3) (values p q v))
	   ((4) (values u p v))
	   ((5) (values v p q)))))))
  
(defmethod hue-weight ((col colour))
  "Get the perceptual brightness weight for the hue."
  (case (floor (* (hue col) 5.0))
    ((0) 0) ;; Red
    ((1) 0.2) ;; Yellow
    ((2) 0) ;; Green
    ((3) 0.1) ;; Cyan
    ((4) -0.1) ;; Blue
    ((5) -0.1))) ;; Magenta

(defmethod difference ((a colour) (b colour))
  "Decide how different two colours are. 0 .. approx 2.1"
  (+ (* (abs (- (hue a) 
		(hue b)))
	0.00028)
     (abs (- (saturation a) 
	     (saturation b)))
     (abs (- (brightness a) 
	     (brightness b)))))

(defmethod value-difference ((a colour) (b colour))
  "The difference in value between the two colours."
  (/ (+ (saturation a)
	(saturation b)
	(brightness a)
	(brightness b))
     4.0))

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
     (mod (+ (+ target 
		 separation) 
	      (random (- wrap 
			 (* 2.0 
			    separation))))
      wrap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Postscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ps-stream* t)

(defmethod write-eps-header (width height &key (to *ps-stream*))
  "Write the standard raw PostScript header."
  (format to "%!PS-Adobe-3.0 EPSF-3.0~%")
  (format to "%%BoundingBox: 0 0 ~a ~a~%" width height)
  (format to "/L {lineto} bind def~%/M {moveto} bind def~%"))

(defmethod write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional PostScript footer"
  (format to "%%EOF~%"))

(defmethod write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defmethod write-rectfill (x y width height &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" x y width height))

(defmethod write-rectstroke (x y width height &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectstroke~%" x y width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour chord (palette) generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-colours ((name string) colours width height)
  "Write the drawing"
  (with-open-file (ps name :direction :output
		      :if-exists :supersede)
    (write-eps-header width height :to ps)
    (let* ((count (length colours))
	   (step (/ width count))
	   (i 0))
      (dolist (col colours)
	(multiple-value-bind (r g b) (hsb-to-rgb col)
	  (write-rgb r g b :to ps))
	(write-rectfill (* i step) 0 step height :to ps)
	(incf i)))
    (write-eps-footer :to ps)))

(defmethod create-colours ()
  "Sort by value, but adjust only brightness."
  (let* ((n (+ (random 8) 4))
	 (colours (sort-by-value (n-random-colours n))))
    (linearise-brightness colours)
    (dump colours)
    (write-colours "./test.ps" colours 1000 200)
  #+openmcl (ccl::os-command (format nil "open ~a" "./test.ps"))))

;; random (num colours * min separation -> 1.0) = range, make & step



;; Light on dark ground, or dark on light ground?

;; Ensure separation of HSV axes independently

;; Choose the components of the chord equally spaced around the colour wheel
;;  (change the spacing/start to have components in just part of the spectrum)
;; Or by additive sequences (so 4 colours random, separated, with 3 split comps)
;;  (n cols random, w/ 1 or 2 or 4 adjacents)
;; Sort the chord by brightness
;; Divide the range from lightest to darkest into equal steps
;; Adjust each component to the brightness required by its sequence position
;; Or increase the saturation to get to the darkness/value



;; Get number of colours required.

;; Get the list of colours, chosen from equally around the colour wheel
;;  with random brightness and saturation.
;; (Or: n times
;;      generate starting point
;;      choose colours around it additively)

;; Order by brightness.
;; Divide the range from lightest to darkness into equal steps.
;; Adjust each to the brightness required by its position in the sequence.
;; If too bright, darken and desaturate.

;; AARON used inks with set maximum darknesses and brightnesses (not sats)
;; So if 