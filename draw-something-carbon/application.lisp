;; TODO
;; Optimise the list length finding

(in-package "BOSCO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :carbon)
  (require :pascal-strings))

;; The window object and dimensions
(defvar *window* nil)
(defvar *window-width* nil)
(defvar *window-height* nil)

;; The drawing data
(defvar *bg-colour* nil)
(defvar *fg-colour* nil)
(defvar *figure-points* nil)

;; The drawing-in-progress 
(defvar *erase-drawing* nil)
(defvar *draw-timer* nil)
(defvar *path* nil)

(defclass draw-something-application (bosco-carbon-application)
  ())

(defmethod run-application ((app draw-something-application) &key
			    &allow-other-keys)
  ;; Start the main event loop
  (when *bosco-swank-port*
    (swank:create-server :port *bosco-swank-port*))

  (get-screen-size)
  (new-drawing)

  (application-to-front)
  (begin-full-screen)

  (setf *window* (make-window))
  (register-draw-callback *window*)
  (show-window *window*)
  
  (register-draw-timer)

  (#_RunApplicationEventLoop)

  (end-full-screen)

  (quit))

(defmethod application-to-front ()
  (let ((psn (make-record <P>rocess<S>erial<N>umber)))
    (#_GetCurrentProcess psn)
    (#_SetFrontProcess psn)))

(defmethod begin-full-screen ()
  (#_HideCursor)
  (#_SetSystemUIMode #$kUIModeAllHidden 0))

(defmethod end-full-screen ()
  (#_SetSystemUIMode #$kUIModeNormal 0)
  (#_ShowCursor))

(defmethod toplevel-function ((app draw-something-application) init-file)
  (declare (ignore init-file))
  (run-application app))

(defmethod get-screen-size ()
  ;; Make and return the WindowRef the size of the screen
  (let* ((gd (#_GetMainDevice))
	 (bounds (pref (pref (pref gd <GDP>tr) <GD>evice) <GD>evice.gd<R>ect)))
    (setf *window-width* (- (pref bounds :<R>ect.right)
			    (pref bounds :<R>ect.left)))
    (setf *window-height* (- (pref bounds :<R>ect.bottom)
			    (pref bounds :<R>ect.top)))))

(defmethod make-window ()
  ;; Make and return the WindowRef the size of the screen
  (let* ((gd (#_GetMainDevice))
	 (bounds (pref (pref (pref gd <GDP>tr) <GD>evice) <GD>evice.gd<R>ect))
	 (window (make-record <W>indow<R>ef)))
    (#_CreateNewWindow #$kPlainWindowClass #$kWindowStandardHandlerAttribute
		       bounds window)
    window))

(defmethod show-window (window)
  (#_TransitionWindow (pref window <W>indow<R>ef)
		      #$kWindowZoomTransitionEffect 
		      #$kWindowShowTransitionAction 
		      (%null-ptr)))

(defmethod draw-figure ()
  (#_SetPortWindowPort (ccl::%get-ptr *window*))
  (rlet ((context <CGC>ontext<R>ef)
	 (bounds <CGR>ect))
	(setf (pref bounds :<CGR>ect.origin.x) 0.0)
	(setf (pref bounds :<CGR>ect.origin.y) 0.0)
	(setf (pref bounds :<CGR>ect.size.width) 
	      (coerce *window-width* 'float))
	(setf (pref bounds :<CGR>ect.size.height) 
	      (coerce *window-height* 'float))
	(#_QDBeginCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			    context)
	(#_CGContextSetRGBFillColor (ccl::%get-ptr context) 
				       1.0 1.0 1.0 1.0)
	(#_CGContextFillRect (ccl::%get-ptr context) 
			     bounds)
	(#_CGContextSetShouldAntialias (ccl::%get-ptr context) 1)
	(#_CGContextSetRGBStrokeColor (ccl::%get-ptr context) 
				       0.0 0.0 0.0 1.0)
	(#_CGContextAddPath (ccl::%get-ptr context) 
			    (ccl::%get-ptr *path*))
	(#_CGContextStrokePath (ccl::%get-ptr context))
	(#_CGContextSynchronize (ccl::%get-ptr context))
	(#_QDEndCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			  context)))


(defmethod draw-next-segment ()
  (when (cdr *figure-points*)
    (#_SetPortWindowPort (ccl::%get-ptr *window*))
    (rlet ((context <CGC>ontext<R>ef))
	(#_QDBeginCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			    context)
	(#_CGContextSetLineWidth (ccl::%get-ptr context) 2.0)
	(#_CGContextMoveToPoint (ccl::%get-ptr context) 
			   (draw-something:x (car *figure-points*))
			   (draw-something:y (car *figure-points*)))
	(#_CGContextAddLineToPoint (ccl::%get-ptr context) 
			      (draw-something:x (cadr *figure-points*))
			      (draw-something:y (cadr *figure-points*)))
	(#_CGContextStrokePath (ccl::%get-ptr context))
	(#_CGContextSynchronize (ccl::%get-ptr context))
	(#_QDEndCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			  context))))

(defcallback draw-callback (:address ref :<E>vent<H>andler<C>all<R>ef event 
			    :address data :<OSE>rr)
  (draw-figure)
  #$noErr)

(defmethod draw-event-spec ()
  (let ((spec (make-record <E>vent<T>ype<S>pec)))
    (setf (pref spec :<E>vent<T>ype<S>pec.event<C>lass)
	  #$kEventClassWindow)
    (setf (pref spec :<E>vent<T>ype<S>pec.event<K>ind)
	  #$kEventWindowDrawContent)
    spec))

(defmethod register-draw-callback (window)
  (#_InstallEventHandler (#_GetWindowEventTarget (ccl::%get-ptr window))
			 (#_NewEventHandlerUPP draw-callback)
			 1
			 (draw-event-spec)
			 (%null-ptr)
			 (%null-ptr)))


;;    (ccl::with-pstr (message-str (format nil "~A~%" err))
;;      (#_StandardAlert #$kAlertNoteAlert message-str (%null-ptr) (%null-ptr) (%null-ptr)))

(defmethod erase-drawing ()
  (#_SetPortWindowPort (ccl::%get-ptr *window*))
  (rlet ((context <CGC>ontext<R>ef)
	 (bounds <CGR>ect))
	(setf (pref bounds :<CGR>ect.origin.x) 0.0)
	(setf (pref bounds :<CGR>ect.origin.y) 0.0)
	(setf (pref bounds :<CGR>ect.size.width) 
	      (coerce *window-width* 'float))
	(setf (pref bounds :<CGR>ect.size.height) 
	      (coerce *window-height* 'float))
	(#_QDBeginCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			    context)
	(#_CGContextSetRGBFillColor (ccl::%get-ptr context) 
				       1.0 1.0 1.0 1.0)
	(#_CGContextFillRect (ccl::%get-ptr context) 
			     bounds)
	(#_CGContextSynchronize (ccl::%get-ptr context))
	(#_QDEndCGContext (#_GetWindowPort (ccl::%get-ptr *window*))
			  context))
  (setf *erase-drawing* nil))


(defmethod new-drawing ()
  (setf *figure-points* 
	(draw-something:draw-something nil *window-width* *window-height*))
  (first-point))

(defmethod next-drawing ()
  (new-drawing)
  (setf *erase-drawing* t)
  (#_SetEventLoopTimerNextFireTime (ccl::%get-ptr *draw-timer*) 
				   (coerce 10.0 'double-float)))

(defmethod first-point ()
  (if *path*
      (#_CGPathRelease (ccl::%get-ptr *path*)))
  (setf *path* (#_CGPathCreateMutable))
  ;;(#_CGPathRetain (ccl::%get-ptr *path*))
  (let ((p (car *figure-points*)))
    (#_CGPathMoveToPoint (ccl::%get-ptr *path*)
			 (%null-ptr)
			 (draw-something:x p) 
			 (draw-something:y p)))
  (setf *figure-points* (cdr *figure-points*)))

(defmethod next-point ()
  (let ((p (car *figure-points*)))
    (#_CGPathAddLineToPoint (ccl::%get-ptr *path*)
			    (%null-ptr)
			    (draw-something:x p) 
			    (draw-something:y p))
  (setf *figure-points* (cdr *figure-points*))))

(defmethod next-frame ()
  (next-point)
  (draw-next-segment))

(defcallback draw-timer (:<E>vent<L>oop<T>imer<R>ef event 
						    :address data :void) 
  (if *erase-drawing*
      (erase-drawing))
  (if *figure-points*
      (next-frame)
      (next-drawing)))

(defmethod register-draw-timer ()
  (setf *draw-timer* (make-record <E>vent<L>oop<T>imer<R>ef))
  (#_InstallEventLoopTimer (#_GetMainEventLoop)
			   (coerce 0.0 'double-float) 
			   (coerce 0.01 'double-float) 
			   (#_NewEventLoopTimerUPP draw-timer) 
			   (%null-ptr) 
			   *draw-timer*))