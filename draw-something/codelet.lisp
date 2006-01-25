;;  codelet.lisp - Codelets and coderack
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

(defconstant drawing-is-finished nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *draw-something-continue* t)

(defmethod draw-something-should-finish ()
  "Tell draw-soemthing that it should finish drawing."
  (setf *draw-something-continue* nil))

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
	    :initform *codelet-ticks*
	    :documentation "The creation time of the codelet."))
  (:documentation "A codelet."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *codelet-ticks* 0)

(defmethod advance-codelet-ticks ()
  "Move on the codelet time."
  (incf *codelet-ticks*))

(defmethod should-prune-codelets ()
  "Check whether it's time to prune again."
  (mod *codelet-ticks* number-of-ticks-per-pruning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-coderack ()
  (make-vector 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-codelet-to-coderack (coderack (c codelet))
  "Add the codelet to the list."
  (vector-push-extend c coderack))

(defmethod add-codelet (coderack action urgency category &rest arguments)
  "Make and add the codelet to the list."
  (add-codelet-to-coderack coderack
			   (make-instance 'codelet
					  :action action
					  :arguments arguments
					  :category category
					  :urgency urgency)))

(defmethod remove-codelet (coderack i)
  "Remove the codelet from the vector, filling the resulting hole."
  (if (< i (1- (fill-pointer coderack)))
      (setf (aref coderack i) 
	    (aref coderack (1- (fill-pointer coderack)))))
  (decf (fill-pointer coderack)))

(defmethod remove-codelets-matching (coderack predicate)
  "Remove all codelets that predicate returns t for."
  (loop for i from 0 to (length coderack)
       when (apply predicate (aref coderack i))
       do (remove-codelet i)))

(defmethod codelet-should-run ((c codelet))
  "Probabilistically decide whether the codelet should run."
  t) ;; todo


(defmethod run-codelet ((c codelet))
  "Run the codelet."
  (format t "Running: ~a~%" (action c))
  (apply #'funcall (action c) (arguments c)))

(defmethod run-one-codelet (coderack)
  "Run one codelet."
  ;; Make sure we run one?
  (dotimes (i (length coderack))
    (let ((candidate (aref coderack i)))
      (when (codelet-should-run candidate)
	(run-codelet candidate) 
	(remove-codelet coderack i)
	(return)))))

(defmethod random-urgency ()
  "A random urgency."
  (random-range minimum-urgency maximum-urgency))

(defmethod codelet-age ((c codelet))
  "The age of the codelet."
  (- *codelet-ticks* (created c)))

(defmethod should-remove-codelet ((c codelet))
  "Should the codelet be removed? Weighted random choice."
  (> (random-urgency)
     (/ (urgency c)
	(codelet-age c))))

(defmethod prune-codelets (coderack)
  "Randomly remove codelets that are too old and low priority."
  (let ((to-remove (make-vector 10)))
    (dotimes (i (length coderack))
	(when (should-remove-codelet (aref coderack i))
	  (vector-push-extend i to-remove)))
    (dolist (remove to-remove)
      (remove-codelet coderack remove))))
  
(defmethod initialise-coderack (coderack the-drawing)
  "Populate the coderack with the initial codelets."
  ;; Randomly add n. codelets
  ;; Ones that locate various sizes of wide/tall/equalish space
  ;; The codelets spawn figure-making codelets
  ;; Which spawn skeleton-making codelets
  ;; Which spawn drawing codelets
  ;; Which then call finish
  ;; this is just deterministic, go probabilistic for 0.5
  (dotimes (i (random-range 5 20))
    (add-figure-making-codelet coderack the-drawing)))

(defmethod draw-loop-step (coderack the-drawing)
  "One step of the loop"
  ;;(when (should-prune-codelets)
  ;;  (prune-coderack coderack))
  ;;add new codelets from figures & from ongoing list to add
  (run-one-codelet coderack))

(defmethod coderack-draw-loop (coderack the-drawing)
  "Run until complete."
  (setf *draw-something-continue* t) ;; Make sure value is reset in REPL
  (setf *codelet-ticks* 0) ;; Make sure value is reset in REPL
  (format t "~a~%" *draw-something-continue*)
  (loop while *draw-something-continue* 
     do (draw-loop-step coderack the-drawing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific codelets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod figure-making-codelet (coderack the-drawing)
  "Replace with space finder, form-adder, etc."
  (add-figure-drawing-codelet coderack the-drawing (make-figure the-drawing)))

(defmethod add-figure-making-codelet (coderack the-drawing)
  "Add a codelet to make the figure."
  (add-codelet coderack 'figure-making-codelet 100 'drawing coderack 
	       the-drawing))

(defmethod figure-drawing-codelet (coderack the-drawing fig)
  "Draw the figure. Replace with various."
  (draw-figure the-drawing fig)
  (when (= (length coderack) 1)
    (draw-something-should-finish)))

(defmethod add-figure-drawing-codelet (coderack the-drawing fig)
  "Add a codelet to draw the figure."
  (add-codelet coderack 'figure-drawing-codelet 100 'drawing coderack 
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