;;  codelet.lisp - Codelets and the coderack.
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

;; Fluid Concepts & Creative Analogies notes:
;; - Platonet, slipnet, workspace, coderack.
;; p223 - How codekets are added.
;; p415 - Variations in perception.


(in-package "DRAW-SOMETHING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant maximum-urgency 100)
(defconstant minimum-urgency 1)

(defconstant maximum-number-of-codelets 100)
(defconstant number-of-ticks-per-pruning 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass codelet ()
  ((action :accessor action
	   :type symbol
	   :initarg :action
	   :documentation "The method to call.")
   (arguments :accessor arguments
	      :type list
	      :initform '()
	      :initarg :arguments
	      :documentation "The arguments to pass to the method.")
   (category :accessor category
	     :type symbol
	     :initform 'general
	     :initarg :category
	     :documentation "The category of the codelet.")
   (urgency :accessor urgency
	    :type integer
	    :initform minimum-urgency
	    :initarg :urgency
	    :documentation "The urgency of the codelet.")
   (created :accessor created
	    :type integer
	    :initarg :created
	    :documentation "The creation time of the codelet."))
  (:documentation "A codelet."))

(defclass coderack ()
  ((codelets :accessor codelets
	     :type vector
	     :initform (make-vector maximum-number-of-codelets)
	     :documentation "The codelets.")
   (time :accessor codelet-ticks
	 :type integer
	 :initform 0
	 :documentation "The number of ticks (run loop cycles) that have run.")
   (should-continue :accessor should-continue
		    :initform t
		    :documentation "Whether the codelrt loop should continue."))
  (:documentation "The coderack."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod should-finish-running ((rack coderack))
  (setf (should-continue rack) nil))

(defmethod advance-codelet-ticks ((rack coderack))
  (incf (codelet-ticks rack)))

(defmethod should-prune-codelets ((rack coderack))
  "Check whether it's time to prune again."
  (mod (codelet-ticks rack) number-of-ticks-per-pruning))

(defmethod add-codelet-to-coderack ((c codelet) (rack coderack))
  "Add the codelet to the list."
  (format t "Adding: ~a~%" (string-downcase (string (action c))))
  (vector-push-extend c (codelets rack)))

(defmethod add-codelet ((rack coderack) action urgency category &rest arguments)
  "Make and add the codelet to the list."
  (add-codelet-to-coderack (make-instance 'codelet
					  :action action
					  :arguments arguments
					  :category category
					  :urgency urgency
					  :created (codelet-ticks rack))
			   rack))

(defmethod remove-codelet ((rack coderack) i)
  "Remove the codelet from the vector, filling the resulting hole."
  (if (< i (1- (fill-pointer (codelets rack))))
      (setf (aref (codelets rack) i) 
	    (aref (codelets rack) (1- (fill-pointer (codelets rack))))))
  (decf (fill-pointer (codelets rack))))

(defmethod remove-codelets-matching ((rack coderack) predicate)
  "Remove all codelets that predicate returns t for."
  (loop for i from 0 to (length rack)
       when (apply predicate (aref rack i))
       do (remove-codelet rack i)))

(defmethod codelet-should-run ((rack coderack) (c codelet))
  "Probabilistically decide whether the codelet should run."
  t) ;; todo

(defmethod run-codelet ((c codelet))
  "Run the codelet."
  (format t "Running: ~a~%" (string-downcase (string (action c))))
  (apply #'funcall (action c) (arguments c)))

(defmethod run-one-codelet ((rack coderack))
  "Run one codelet."
  ;; Make sure we run exactly one?
  (dotimes (i (length (codelets rack)))
    (let ((candidate (aref (codelets rack) i)))
      (when (codelet-should-run rack candidate)
	(run-codelet candidate) 
	(remove-codelet rack i)
	(return)))))

(defmethod random-urgency ()
  "A random urgency."
  (random-range minimum-urgency maximum-urgency))

(defmethod codelet-age ((c codelet) (rack coderack))
  "The age of the codelet."
  (- (codelet-ticks rack) (created c)))

(defmethod should-remove-codelet ((c codelet))
  "Should the codelet be removed? Weighted random choice."
  (> (random-urgency)
     (/ (urgency c)
	(codelet-age c))))

(defmethod prune-codelets ((rack coderack))
  "Randomly remove codelets that are too old and low priority."
  (let ((to-remove (make-vector 10)))
    (dotimes (i (length (codelets rack)))
      (let ((c (aref (codelets rack) i)))
	(when (should-remove-codelet c)
	  (format t "Removing: ~a~%" (string-downcase (string (action c))))
	  (vector-push-extend i to-remove))))
    (dolist (remove to-remove)
      (remove-codelet rack remove))))
  
(defmethod initialise-coderack ((rack coderack) the-drawing)
  "Populate the coderack with the initial codelets."
  ;; Randomly add n. codelets
  ;; Ones that locate various sizes of wide/tall/equalish space
  ;; The codelets spawn figure-making codelets
  ;; Which spawn skeleton-making codelets
  ;; Which spawn drawing codelets
  ;; Which then call finish
  ;; this is just deterministic, go probabilistic for 0.5
  (dotimes (i (random-range min-figures max-figures))
    (add-figure-making-codelet rack the-drawing)))
  
(defmethod draw-loop-step ((rack coderack) the-drawing)
  "One step of the loop"
  (declare (ignore the-drawing))
  ;;(when (should-prune-codelets)
  ;;  (prune-coderack coderack))
  ;;add new codelets from figures & from ongoing list to add
  (run-one-codelet rack))

(defmethod coderack-draw-loop ((rack coderack) the-drawing)
  "Run until complete."
  (loop while (should-continue rack)
     do (draw-loop-step rack the-drawing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific codelets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod figure-making-codelet ((rack coderack) the-drawing)
  "Replace with space finder, form-adder, etc."
  (add-figure-drawing-codelet rack the-drawing (make-figure the-drawing)))

(defmethod add-figure-making-codelet ((rack coderack) the-drawing)
  "Add a codelet to make the figure."
  (add-codelet rack 'figure-making-codelet 100 'drawing rack the-drawing))

(defmethod figure-drawing-codelet ((rack coderack) the-drawing fig)
  "Draw the figure. Replace with various."
  (draw-figure the-drawing fig)
  (when (= (length (codelets rack)) 1)
    (should-finish-running rack)))

(defmethod add-figure-drawing-codelet ((rack coderack) the-drawing fig)
  "Add a codelet to draw the figure."
  (add-codelet rack 'figure-drawing-codelet 100 'drawing rack 
	       the-drawing fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loop until drawing-finished
;;    Coderack empty? post initial set of codelets
;;    Steps modulo prune-steps? prune
;;    Do one step, running a codelet



;; Temperature?
;; Dealing with snags?
;; Allow deletion of *drawn* "structures"?


;; Initial codelets:
;;  Space finders, drawers, adorners

;; Find space
;; Find interesting figures to relate to
;; Add "system" to figure
;; Maybe start non-overlapping figure
;; Maybe start overlapping figure
;; Maybe start adornments on figure
;; Maybe start figure relating to figure
;; a grid or mirror or radial or other compositional arrangement
;; Maybe stop new figure generation, removing all possible figure generators
;; maybe start generating skeleton
;; Maybe add skeleton line according to notes on skeleton, or maybe change
;; Maybe mark skeleton as finished
;; Maybe start drawing
;; Maybe set ground colour
;; Maybe set figure colour
;; Maybe identify position for new figure based on existing figures
;; Maybe check to see if drawing is finished
;; &c

;; Codelets can post another codelet(s), remove codelets, add notes
;; create skeletons, draw outlines or fills, add other structures

;; The drawing can have skeletons, fills, outlines, negative space, 
;; compositional guides, potential places for figures, and so on as objects
;; It is the workspace


;; -->
;; Grid of coarse cells, or grid of points to start expanding a hull at?