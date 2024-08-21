;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *contexts* (make-hash-table))

(defparameter *context* nil
  "The currently operating level of the system, e.g. curves, line.")

(defparameter possible-contexts '(nil artwork mapping planning lines sectors curves movement-control))

(defstruct context
  name
  (productions '()))

(defun get-context (context-name)
  (or (gethash context-name *contexts*)
      (setf (gethash context-name *contexts*)
	    (make-context :name context-name :productions '()))))

(defun add-production-to-context (context-name production)
  (setf (context-productions (get-context context-name))
	(append (context-productions (get-context context-name))
		(list production))))

(defun set-context (context-name)
  "Set the current context."
  (assert (member context-name possible-contexts))
  (setf *context* (gethash context-name *contexts*)))

(defun current-context-productions ()
  (context-productions *context*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar previous-production nil)

(defstruct production
  description
  left-hand-side
  right-hand-side)

(defmacro defproduction (context-name description &body body)
  "Define a production rule. Note description doesn't have to be unique."
  (let* ((after-arrow (member '=> body))
         (left-hand-side (ldiff body after-arrow))
         (right-hand-side (rest after-arrow)))
    `(add-production-to-context
      ,context-name
      (make-production
       :description ,description
       :left-hand-side (lambda () (and ,@left-hand-side))
       :right-hand-side (lambda () ,@right-hand-side)))))

(defun apply-production ()
  "Try to find and apply a single production. Returns t if one ran."
  (dolist (the-production (current-context-productions))
    (when (funcall (production-left-hand-side the-production))
      (when (not (eq (production-description the-production)
                     previous-production))
        (format t "~a~%" (production-description the-production))
        (setf previous-production (production-description the-production)))
      (funcall (production-right-hand-side the-production))
      (return t))))
