;; clopt.lisp - Simple methods to get options from the command line.

;; Dummy until I sort command line options

(defmethod get-arg-value (long &key (short nil) (default nil) 
			       (args *cli-args*))
  default)

#|

(defparameter *cli-args* (ccl::command-line-arguments))

;; TODO: Consume parsed arguments

(defmethod get-arg (long &key (short nil) (args *cli-args*))
  "Check the command line arguments for a unary switch."
  (let ((result nil))
    (dolist (arg args)
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf result t)
	(return)))
    result))

(defmethod get-arg-value (long &key (short nil) (default nil) 
			       (args *cli-args*))
  "Check the command line arguments for a switch with an accompanying value."
  (let ((result nil)
	(found nil))
    (dolist (arg args)
      ;; If the last arg matched, return this one as the value
      ;;FIXME: Make sure it's not a - or --switch
      (when (eq found 
		t)
	(setf result arg)
	(return))
      ;; When the arg matches, set a flag to return the next value
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf found t )))
  ;; Return the result or the value  
  (or result
      default)))

(defmethod get-arg-values (long &key (short nil) (default '()) 
				(args *cli-args*))
  "Check the command line arguments for possibly many occurrences of a switch with an accompanying value."
  (let ((result nil)
	(found nil))
    (dolist (arg args)
      ;; If the last arg matched, return this one as the value
      ;;FIXME: Make sure it's not a - or --switch
      (when (eq found 
		t)
	(setf result arg))
      ;; When the arg matches, set a flag to return the next value
      (when (or (equal long 
		       arg)
		(and short
		     (equal short 
			    arg)))
	(setf found t )))
  ;; Return the result (in the order found on the command line) or the value  
  (or (reverse result)
	default)))

|#