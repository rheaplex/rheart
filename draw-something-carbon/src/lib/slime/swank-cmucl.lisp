;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;+" -*-
;;;
;;;; Introduction
;;;
;;; This is the CMUCL implementation of the `swank-backend' package.

(in-package :swank-backend)

;;;; "Hot fixes"
;;;
;;; Here are necessary bugfixes to the latest released version of
;;; CMUCL (currently 18e). Any fixes placed here should also be
;;; submitted to the `cmucl-imp' mailing list and confirmed as
;;; good. When a new release is made that includes the fixes we should
;;; promptly delete them from here. It is enough to be compatible with
;;; the latest release.

(in-package :lisp)

;;; `READ-SEQUENCE' with large sequences has problems in 18e. This new
;;; definition works better.

#+cmu18e
(progn
  (let ((s (find-symbol (string :*enable-package-locked-errors*) :lisp)))
    (when s
      (setf (symbol-value s) nil)))

  (defun read-into-simple-string (s stream start end)
    (declare (type simple-string s))
    (declare (type stream stream))
    (declare (type index start end))
    (unless (subtypep (stream-element-type stream) 'character)
      (error 'type-error
             :datum (read-char stream nil #\Null)
             :expected-type (stream-element-type stream)
             :format-control "Trying to read characters from a binary stream."))
    ;; Let's go as low level as it seems reasonable.
    (let* ((numbytes (- end start))
           (total-bytes 0))
      ;; read-n-bytes may return fewer bytes than requested, so we need
      ;; to keep trying.
      (loop while (plusp numbytes) do
            (let ((bytes-read (system:read-n-bytes stream s start numbytes nil)))
              (when (zerop bytes-read)
                (return-from read-into-simple-string total-bytes))
              (incf total-bytes bytes-read)
              (incf start bytes-read)
              (decf numbytes bytes-read)))
      total-bytes))

  (let ((s (find-symbol (string :*enable-package-locked-errors*) :lisp)))
    (when s
      (setf (symbol-value s) t)))

  )

(in-package :swank-backend)


;;;; TCP server
;;;
;;; In CMUCL we support all communication styles. By default we use
;;; `:SIGIO' because it is the most responsive, but it's somewhat
;;; dangerous: CMUCL is not in general "signal safe", and you don't
;;; know for sure what you'll be interrupting. Both `:FD-HANDLER' and
;;; `:SPAWN' are reasonable alternatives.

(defimplementation preferred-communication-style ()
  :sigio)

(defimplementation create-socket (host port)
  (ext:create-inet-listener port :stream
                            :reuse-address t
                            :host (resolve-hostname host)))

(defimplementation local-port (socket)
  (nth-value 1 (ext::get-socket-host-and-port (socket-fd socket))))

(defimplementation close-socket (socket)
  (sys:invalidate-descriptor socket)
  (ext:close-socket (socket-fd socket)))

(defimplementation accept-connection (socket)
  #+mp (mp:process-wait-until-fd-usable socket :input)
  (make-socket-io-stream (ext:accept-tcp-connection socket)))

;;;;; Sockets

(defun socket-fd (socket)
  "Return the filedescriptor for the socket represented by SOCKET."
  (etypecase socket
    (fixnum socket)
    (sys:fd-stream (sys:fd-stream-fd socket))))

(defun resolve-hostname (hostname)
  "Return the IP address of HOSTNAME as an integer."
  (let* ((hostent (ext:lookup-host-entry hostname))
         (address (car (ext:host-entry-addr-list hostent))))
    (ext:htonl address)))

(defun make-socket-io-stream (fd)
  "Create a new input/output fd-stream for FD."
  (sys:make-fd-stream fd :input t :output t :element-type 'base-char))

;;;;; Signal-driven I/O

(defvar *sigio-handlers* '()
  "List of (key . function) pairs.
All functions are called on SIGIO, and the key is used for removing
specific functions.")

(defun set-sigio-handler ()
  (sys:enable-interrupt :sigio (lambda (signal code scp)
                                 (sigio-handler signal code scp))))

(defun sigio-handler (signal code scp)
  (declare (ignore signal code scp))
  (mapc #'funcall (mapcar #'cdr *sigio-handlers*)))

(defun fcntl (fd command arg)
  "fcntl(2) - manipulate a file descriptor."
  (multiple-value-bind (ok error) (unix:unix-fcntl fd command arg)
    (unless ok (error "fcntl: ~A" (unix:get-unix-error-msg error)))))

(defimplementation add-sigio-handler (socket fn)
  (set-sigio-handler)
  (let ((fd (socket-fd socket)))
    (format *debug-io* "; Adding input handler: ~S ~%" fd)
    (fcntl fd unix:f-setown (unix:unix-getpid))
    (fcntl fd unix:f-setfl unix:fasync)
    (push (cons fd fn) *sigio-handlers*)))

(defimplementation remove-sigio-handlers (socket)
  (let ((fd (socket-fd socket)))
    (setf *sigio-handlers* (remove fd *sigio-handlers* :key #'car))
    (sys:invalidate-descriptor fd))
  (close socket))

;;;;; SERVE-EVENT

(defimplementation add-fd-handler (socket fn)
  (let ((fd (socket-fd socket)))
    (format *debug-io* "; Adding fd handler: ~S ~%" fd)
    (sys:add-fd-handler fd :input (lambda (_) 
                                    _
                                    (funcall fn)))))

(defimplementation remove-fd-handlers (socket)
  (sys:invalidate-descriptor (socket-fd socket)))


;;;; Stream handling
;;; XXX: How come we don't use Gray streams in CMUCL too? -luke (15/May/2004)

(defimplementation make-fn-streams (input-fn output-fn)
  (let* ((output (make-slime-output-stream output-fn))
         (input  (make-slime-input-stream input-fn output)))
    (values input output)))

(defstruct (slime-output-stream
             (:include lisp::lisp-stream
                       (lisp::misc #'sos/misc)
                       (lisp::out #'sos/out)
                       (lisp::sout #'sos/sout))
             (:conc-name sos.)
             (:print-function %print-slime-output-stream)
             (:constructor make-slime-output-stream (output-fn)))
  (output-fn nil :type function)
  (buffer (make-string 512) :type string)
  (index 0 :type kernel:index)
  (column 0 :type kernel:index))

(defun %print-slime-output-stream (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :type t :identity t)))

(defun sos/out (stream char)
  (let ((buffer (sos.buffer stream))
	(index (sos.index stream)))
    (setf (schar buffer index) char)
    (setf (sos.index stream) (1+ index))
    (incf (sos.column stream))
    (when (char= #\newline char)
      (setf (sos.column stream) 0))
    (when (= index (1- (length buffer)))
      (force-output stream)))
  char)

(defun sos/sout (stream string start end)
  (loop for i from start below end 
	do (sos/out stream (aref string i))))

(defun sos/misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    ((:force-output :finish-output)
     (let ((end (sos.index stream)))
       (unless (zerop end)
         (funcall (sos.output-fn stream) (subseq (sos.buffer stream) 0 end))
         (setf (sos.index stream) 0))))
    (:charpos (sos.column stream))
    (:line-length 75)
    (:file-position nil)
    (:element-type 'base-char)
    (:get-command nil)
    (:close nil)
    (t (format *terminal-io* "~&~Astream: ~S~%" stream operation))))

(defstruct (slime-input-stream
             (:include string-stream
                       (lisp::in #'sis/in)
                       (lisp::misc #'sis/misc))
             (:conc-name sis.)
             (:print-function %print-slime-output-stream)
             (:constructor make-slime-input-stream (input-fn sos)))
  (input-fn nil :type function)
  ;; We know our sibling output stream, so that we can force it before
  ;; requesting input.
  (sos      nil :type slime-output-stream)
  (buffer   ""  :type string)
  (index    0   :type kernel:index))

(defun sis/in (stream eof-errorp eof-value)
  (declare (ignore eof-errorp eof-value))
  (let ((index (sis.index stream))
	(buffer (sis.buffer stream)))
    (when (= index (length buffer))
      (force-output (sis.sos stream))
      (setf buffer (funcall (sis.input-fn stream)))
      (setf (sis.buffer stream) buffer)
      (setf index 0))
    (prog1 (aref buffer index)
      (setf (sis.index stream) (1+ index)))))

(defun sis/misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (ecase operation
    (:file-position nil)
    (:file-length nil)
    (:unread (setf (aref (sis.buffer stream) 
			 (decf (sis.index stream)))
		   arg1))
    (:clear-input 
     (setf (sis.index stream) 0
			(sis.buffer stream) ""))
    (:listen (< (sis.index stream) (length (sis.buffer stream))))
    (:charpos nil)
    (:line-length nil)
    (:get-command nil)
    (:element-type 'base-char)
    (:close nil)))


;;;; Compilation Commands

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defvar *previous-context* nil
  "Previous compiler error context.")

(defvar *buffer-name* nil
  "The name of the Emacs buffer we are compiling from.
NIL if we aren't compiling from a buffer.")

(defvar *buffer-start-position* nil)
(defvar *buffer-substring* nil)

(defimplementation call-with-compilation-hooks (function)
  (let ((*previous-compiler-condition* nil)
        (*previous-context* nil)
        (*print-readably* nil))
    (handler-bind ((c::compiler-error #'handle-notification-condition)
                   (c::style-warning  #'handle-notification-condition)
                   (c::warning        #'handle-notification-condition))
      (funcall function))))

(defimplementation swank-compile-file (filename load-p)
  (clear-xref-info filename)
  (with-compilation-hooks ()
    (let ((*buffer-name* nil))
      (multiple-value-bind (output-file warnings-p failure-p)
          (compile-file filename :load load-p)
        (unless failure-p
          ;; Cache the latest source file for definition-finding.
          (source-cache-get filename (file-write-date filename)))
        (values output-file warnings-p failure-p)))))

(defimplementation swank-compile-string (string &key buffer position)
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-substring* string))
      (with-input-from-string (stream string)
        (ext:compile-from-stream 
         stream 
         :source-info `(:emacs-buffer ,buffer 
                        :emacs-buffer-offset ,position
                        :emacs-buffer-string ,string))))))


;;;;; Trapping notes
;;;
;;; We intercept conditions from the compiler and resignal them as
;;; `SWANK:COMPILER-CONDITION's.

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (unless (eq condition *previous-compiler-condition*)
    (let ((context (c::find-error-context nil)))
      (setq *previous-compiler-condition* condition)
      (setq *previous-context* context)
      (signal-compiler-condition condition context))))

(defun signal-compiler-condition (condition context)
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :severity (severity-for-emacs condition)
           :short-message (brief-compiler-message-for-emacs condition)
           :message (long-compiler-message-for-emacs condition context)
           :location (compiler-note-location context))))

(defun severity-for-emacs (condition)
  "Return the severity of CONDITION."
  (etypecase condition
    (c::compiler-error :error)
    (c::style-warning :note)
    (c::warning :warning)))

(defun brief-compiler-message-for-emacs (condition)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (princ-to-string condition))

(defun long-compiler-message-for-emacs (condition error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (c::compiler-error-context-enclosing-source error-context)
                  (c::compiler-error-context-source error-context)))
    (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~^~%~}~]~A"
            enclosing source condition)))

(defun compiler-note-location (context)
  "Derive the location of a complier message from its context.
Return a `location' record, or (:error REASON) on failure."
  (if (null context)
      (note-error-location)
      (let ((file (c::compiler-error-context-file-name context))
            (source (c::compiler-error-context-original-source context))
            (path
             (reverse (c::compiler-error-context-original-source-path context))))
        (or (locate-compiler-note file source path)
            (note-error-location)))))

(defun note-error-location ()
  "Pseudo-location for notes that can't be located."
  (list :error "No error location available."))

(defun locate-compiler-note (file source source-path)
  (cond ((and (eq file :stream) *buffer-name*)
         ;; Compiling from a buffer
         (let ((position (+ *buffer-start-position*
                            (source-path-string-position
                             source-path *buffer-substring*))))
           (make-location (list :buffer *buffer-name*)
                          (list :position position))))
        ((and (pathnamep file) (null *buffer-name*))
         ;; Compiling from a file
         (make-location (list :file (unix-truename file))
                        (list :position
                              (1+ (source-path-file-position
                                   source-path file)))))
        ((and (eq file :lisp) (stringp source))
         ;; No location known, but we have the source form.
         ;; XXX How is this case triggered? -luke (16/May/2004)
         (make-location (list :source-form source)
                        (list :position 1)))))

(defun unix-truename (pathname)
  (ext:unix-namestring (truename pathname)))


;;;; XREF
;;;
;;; Cross-reference support is based on the standard CMUCL `XREF'
;;; package. This package has some caveats: XREF information is
;;; recorded during compilation and not preserved in fasl files, and
;;; XREF recording is disabled by default. Redefining functions can
;;; also cause duplicate references to accumulate, but
;;; `swank-compile-file' will automatically clear out any old records
;;; from the same filename.
;;;
;;; To enable XREF recording, set `c:*record-xref-info*' to true. To
;;; clear out the XREF database call `xref:init-xref-database'.

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      xref:who-calls)
(defxref who-references xref:who-references)
(defxref who-binds      xref:who-binds)
(defxref who-sets       xref:who-sets)

;;; More types of XREF information were added since 18e:
;;;
#+cmu19
(progn
  (defxref who-macroexpands xref:who-macroexpands)
  ;; XXX
  (defimplementation who-specializes (symbol)
    (let* ((methods (xref::who-specializes (find-class symbol)))
           (locations (mapcar #'method-location methods)))
      (mapcar #'list methods locations))))

(defun xref-results (contexts)
  (mapcar (lambda (xref)
            (list (xref:xref-context-name xref)
                  (resolve-xref-location xref)))
          contexts))

(defun resolve-xref-location (xref)
  (let ((name (xref:xref-context-name xref))
        (file (xref:xref-context-file xref))
        (source-path (xref:xref-context-source-path xref)))
    (cond ((and file source-path)
           (let ((position (source-path-file-position source-path file)))
             (make-location (list :file (unix-truename file))
                            (list :position (1+ position)))))
          (file
           (make-location (list :file (unix-truename file))
                          (list :function-name (string name))))
          (t
           `(:error ,(format nil "Unknown source location: ~S ~S ~S " 
                             name file source-path))))))

(defun clear-xref-info (namestring)
  "Clear XREF notes pertaining to NAMESTRING.
This is a workaround for a CMUCL bug: XREF records are cumulative."
  (when c:*record-xref-info*
    (let ((filename (truename namestring)))
      (dolist (db (list xref::*who-calls*
                        #+cmu19 xref::*who-is-called*
                        #+cmu19 xref::*who-macroexpands*
                        xref::*who-references*
                        xref::*who-binds*
                        xref::*who-sets*))
        (maphash (lambda (target contexts)
                   ;; XXX update during traversal?  
                   (setf (gethash target db)
                         (delete filename contexts 
                                 :key #'xref:xref-context-file
                                 :test #'equalp)))
                 db)))))


;;;; Find callers and callees
;;;
;;; Find callers and callees by looking at the constant pool of
;;; compiled code objects.  We assume every fdefn object in the
;;; constant pool corresponds to a call to that function.  A better
;;; strategy would be to use the disassembler to find actual
;;; call-sites.

(declaim (inline map-code-constants))
(defun map-code-constants (code fn)
  "Call FN for each constant in CODE's constant pool."
  (check-type code kernel:code-component)
  (loop for i from vm:code-constants-offset below (kernel:get-header-data code)
	do (funcall fn (kernel:code-header-ref code i))))

(defun function-callees (function)
  "Return FUNCTION's callees as a list of functions."
  (let ((callees '()))
    (map-code-constants 
     (vm::find-code-object function)
     (lambda (obj)
       (when (kernel:fdefn-p obj)
	 (push (kernel:fdefn-function obj) callees))))
    callees))

(declaim (ext:maybe-inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of SPACES.  FN
receives the object as argument.  SPACES should be a list of the
symbols :dynamic, :static, or :read-only."
  (dolist (space spaces)
    (declare (inline vm::map-allocated-objects))
    (vm::map-allocated-objects
     (lambda (obj header size)
       (declare (type fixnum size) (ignore size))
       (when (= vm:code-header-type header)
	 (funcall fn obj)))
     space)))

(declaim (ext:maybe-inline map-caller-code-components))
(defun map-caller-code-components (function spaces fn)
  "Call FN for each code component with a fdefn for FUNCTION in its
constant pool."
  (let ((function (coerce function 'function)))
    (declare (inline map-allocated-code-components))
    (map-allocated-code-components
     spaces 
     (lambda (obj)
       (map-code-constants 
	obj 
	(lambda (constant)
	  (when (and (kernel:fdefn-p constant)
		     (eq (kernel:fdefn-function constant)
			 function))
	    (funcall fn obj))))))))

(defun function-callers (function &optional (spaces '(:read-only :static 
						      :dynamic)))
  "Return FUNCTION's callers.  The result is a list of code-objects."
  (let ((referrers '()))
    (declare (inline map-caller-code-components))
    (ext:gc :full t)
    (map-caller-code-components function spaces 
                                (lambda (code) (push code referrers)))
    referrers))

(defun debug-info-definitions (debug-info)
  "Return the defintions for a debug-info.  This should only be used
for code-object without entry points, i.e., byte compiled
code (are theree others?)"
  ;; This mess has only been tested with #'ext::skip-whitespace, a
  ;; byte-compiled caller of #'read-char .
  (check-type debug-info (and (not c::compiled-debug-info) c::debug-info))
  (let ((name (c::debug-info-name debug-info))
        (source (c::debug-info-source debug-info)))
    (destructuring-bind (first) source 
      (ecase (c::debug-source-from first)
        (:file 
         (list (list name
                     (make-location 
                      (list :file (unix-truename (c::debug-source-name first)))
                      (list :function-name name)))))))))

(defun code-component-entry-points (code)
  "Return a list ((NAME LOCATION) ...) of function definitons for
the code omponent CODE."
  (delete-duplicates
   (loop for e = (kernel:%code-entry-points code)
         then (kernel::%function-next e)
         while e
         collect (list (kernel:%function-name e)
                       (function-location e)))
   :test #'equal))

(defimplementation list-callers (symbol)
  "Return a list ((NAME LOCATION) ...) of callers."
  (let ((components (function-callers symbol))
        (xrefs '()))
    (dolist (code components)
      (let* ((entry (kernel:%code-entry-points code))
             (defs (if entry
                       (code-component-entry-points code)
                       ;; byte compiled stuff
                       (debug-info-definitions 
                        (kernel:%code-debug-info code)))))
        (setq xrefs (nconc defs xrefs))))
    xrefs))

(defimplementation list-callees (symbol)
  (let ((fns (function-callees symbol)))
    (mapcar (lambda (fn)
              (list (kernel:%function-name fn)
                    (function-location fn)))
            fns)))


;;;; Resolving source locations
;;;
;;; Our mission here is to "resolve" references to code locations into
;;; actual file/buffer names and character positions. The references
;;; we work from come out of the compiler's statically-generated debug
;;; information, such as `code-location''s and `debug-source''s. For
;;; more details, see the "Debugger Programmer's Interface" section of
;;; the CMUCL manual.
;;;
;;; The first step is usually to find the corresponding "source-path"
;;; for the location. Once we have the source-path we can pull up the
;;; source file and `READ' our way through to the right position. The
;;; main source-code groveling work is done in
;;; `swank-source-path-parser.lisp'.

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
This is useful when debugging the definition-finding code.")

(defvar *source-snippet-size* 256
  "Maximum number of characters in a snippet of source code.
Snippets at the beginning of definitions are used to tell Emacs what
the definitions looks like, so that it can accurately find them by
text search.")

(defmacro safe-definition-finding (&body body)
  "Execute BODY and return the source-location it returns.
If an error occurs and `*debug-definition-finding*' is false, then
return an error pseudo-location.

The second return value is NIL if no error occurs, otherwise it is the
condition object."
  `(flet ((body () ,@body))
    (if *debug-definition-finding*
        (body)
        (handler-case (values (progn ,@body) nil)
          (error (c) (values (list :error (princ-to-string c)) c))))))

(defun code-location-source-location (code-location)
  "Safe wrapper around `code-location-from-source-location'."
  (safe-definition-finding
   (source-location-from-code-location code-location)))

(defun source-location-from-code-location (code-location)
  "Return the source location for CODE-LOCATION."
  (let ((debug-fun (di:code-location-debug-function code-location)))
    (when (di::bogus-debug-function-p debug-fun)
      ;; Those lousy cheapskates! They've put in a bogus debug source
      ;; because the code was compiled at a low debug setting.
      (error "Bogus debug function: ~A" debug-fun)))
  (let* ((debug-source (di:code-location-debug-source code-location))
         (from (di:debug-source-from debug-source))
         (name (di:debug-source-name debug-source)))
    (ecase from
      (:file 
       (location-in-file name code-location debug-source))
      (:stream
       (location-in-stream code-location debug-source))
      (:lisp
       ;; The location comes from a form passed to `compile'.
       ;; The best we can do is return the form itself for printing.
       (make-location
        (list :source-form (with-output-to-string (*standard-output*)
                             (debug::print-code-location-source-form 
                              code-location 100 t)))
        (list :position 1))))))

(defun location-in-file (filename code-location debug-source)
  "Resolve the source location for CODE-LOCATION in FILENAME."
  (let* ((code-date (di:debug-source-created debug-source))
         (source-code (or (source-cache-get filename code-date)
                          (read-file filename))))
    (make-location (list :file (unix-truename filename)) nil)
    (with-input-from-string (s source-code)
      (make-location (list :file (unix-truename filename))
                     (list :position
                           (1+ (code-location-stream-position
                                code-location s)))
                     `(:snippet ,(read-snippet s))))))

(defun location-in-stream (code-location debug-source)
  "Resolve the source location for a CODE-LOCATION from a stream.
This only succeeds if the code was compiled from an Emacs buffer."
  (unless (debug-source-info-from-emacs-buffer-p debug-source)
    (error "The code is compiled from a non-SLIME stream."))
  (let* ((info (c::debug-source-info debug-source))
         (string (getf info :emacs-buffer-string))
         (position (code-location-string-offset 
                    code-location
                    string)))
    (make-location
     (list :buffer (getf info :emacs-buffer))
     (list :position (+ (getf info :emacs-buffer-offset) position))
     (list :snippet (with-input-from-string (s string)
                      (file-position s position)
                      (read-snippet s))))))

(defun read-file (filename)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input)
    (let ((string (make-string (file-length s))))
      (read-sequence string s)
      string)))

(defun read-snippet (stream)
  "Read a string of upto *SOURCE-SNIPPET-SIZE* characters from STREAM."
  (read-upto-n-chars stream *source-snippet-size*))

(defun read-upto-n-chars (stream n)
  "Return a string of upto N chars from STREAM."
  (let* ((string (make-string n))
         (chars  (read-sequence string stream)))
    (subseq string 0 chars)))

;;;;; Function-name locations
;;;
(defun debug-info-function-name-location (debug-info)
  "Return a function-name source-location for DEBUG-INFO.
Function-name source-locations are a fallback for when precise
positions aren't available."
  (with-struct (c::debug-info- (fname name) source) debug-info
    (with-struct (c::debug-source- info from name) (car source)
      (ecase from
        (:file 
         (make-location (list :file (namestring (truename name)))
                        (list :function-name fname)))
        (:stream
         (assert (debug-source-info-from-emacs-buffer-p (car source)))
         (make-location (list :buffer (getf info :emacs-buffer))
                        (list :function-name fname)))
        (:lisp
         (make-location (list :source-form (princ-to-string (aref name 0)))
                        (list :position 1)))))))

(defun debug-source-info-from-emacs-buffer-p (debug-source)
  "Does the `info' slot of DEBUG-SOURCE contain an Emacs buffer location?
This is true for functions that were compiled directly from buffers."
  (info-from-emacs-buffer-p (c::debug-source-info debug-source)))

(defun info-from-emacs-buffer-p (info)
  (and info 
       (consp info)
       (eq :emacs-buffer (car info))))


;;;;; Groveling source-code for positions

(defun code-location-stream-position (code-location stream)
  "Return the byte offset of CODE-LOCATION in STREAM.  Extract the
toplevel-form-number and form-number from CODE-LOCATION and use that
to find the position of the corresponding form.

Finish with STREAM positioned at the start of the code location."
  (let* ((location (debug::maybe-block-start-location code-location))
	 (tlf-offset (di:code-location-top-level-form-offset location))
	 (form-number (di:code-location-form-number location)))
    (let ((pos (form-number-stream-position tlf-offset form-number stream)))
      (file-position stream pos)
      pos)))

(defun form-number-stream-position (tlf-number form-number stream)
  "Return the starting character position of a form in STREAM.
TLF-NUMBER is the top-level-form number.
FORM-NUMBER is an index into a source-path table for the TLF."
  (let ((*read-suppress* t))
    (dotimes (i tlf-number) (read stream))
    (multiple-value-bind (tlf position-map) (read-and-record-source-map stream)
      (let* ((path-table (di:form-number-translations tlf 0))
             (source-path
              (if (<= (length path-table) form-number) ; source out of sync?
                  (list 0)              ; should probably signal a condition
                  (reverse (cdr (aref path-table form-number))))))
        (source-path-source-position source-path tlf position-map)))))
  
(defun code-location-string-offset (code-location string)
  "Return the byte offset of CODE-LOCATION in STRING.
See CODE-LOCATION-STREAM-POSITION."
  (with-input-from-string (s string)
    (code-location-stream-position code-location s)))

;;;;; Source-file cache
;;;
;;; To robustly find source locations it's useful to have the exact
;;; source code that the loaded code was compiled from. In this source
;;; we can accurately find the right location, and from that location
;;; we can extract a "snippet" of code to show what the definition
;;; looks like. Emacs can use this snippet in a best-match search to
;;; locate the right definition, which works well even if the buffer
;;; has been modified.
;;;
;;; The idea is that if a definition previously started with
;;; `(define-foo bar' then it probably still does.
;;;
;;; Whenever we see that the file on disk has the same
;;; `file-write-date' as a location we're looking for, we cache the
;;; whole file inside Lisp. That way we will still have the matching
;;; version even if the file is later modified on disk. If the file is
;;; later recompiled and reloaded then we replace our cache entry.

(defvar *cache-sourcecode* t
  "When true complete source files are cached.
The cache is used to keep known good copies of the source text which
correspond to the loaded code. Finding definitions is much more
reliable when the exact source is available, so we cache it in case it
gets edited on disk later.")

(defvar *source-file-cache* (make-hash-table :test 'equal)
  "Cache of source file contents.
Maps from truename to source-cache-entry structure.")

(defstruct (source-cache-entry
             (:conc-name source-cache-entry.)
             (:constructor make-source-cache-entry (text date)))
  text date)

(defun source-cache-get (filename date)
  "Return the source code for FILENAME as written on DATE in a string.
Return NIL if the right version cannot be found."
  (let ((entry (gethash filename *source-file-cache*)))
    (cond ((and entry (equal date (source-cache-entry.date entry)))
           ;; Cache hit.
           (source-cache-entry.text entry))
          ((or (null entry)
               (not (equal date (source-cache-entry.date entry))))
           ;; Cache miss.
           (if (equal (file-write-date filename) date)
               ;; File on disk has the correct version.
               (let ((source (read-file filename)))
                 (setf (gethash filename *source-file-cache*)
                       (make-source-cache-entry source date))
                 source)
               nil)))))


;;;; Finding definitions

;;; There are a great many different types of definition for us to
;;; find. We search for definitions of every kind and return them in a
;;; list.

(defimplementation find-definitions (name)
  (append (function-definitions name)
          (setf-definitions name)
          (variable-definitions name)
          (class-definitions name)
          (type-definitions name)
          (compiler-macro-definitions name)
          (source-transform-definitions name)
          (function-info-definitions name)
          (ir1-translator-definitions name)))

;;;;; Functions, macros, generic functions, methods
;;;
;;; We make extensive use of the compile-time debug information that
;;; CMUCL records, in particular "debug functions" and "code
;;; locations." Refer to the "Debugger Programmer's Interface" section
;;; of the CMUCL manual for more details.

(defun function-definitions (name)
  "Return definitions for NAME in the \"function namespace\", i.e.,
regular functions, generic functions, methods and macros.
NAME can any valid function name (e.g, (setf car))."
  (let ((macro?    (and (symbolp name) (macro-function name)))
        (special?  (and (symbolp name) (special-operator-p name)))
        (function? (and (ext:valid-function-name-p name)
                        (ext:info :function :definition name))))
    (cond (macro? 
           (list `((defmacro ,name)
                   ,(function-location (macro-function name)))))
          (special?
           (list `((:special-operator ,name) 
                   (:error ,(format nil "Special operator: ~S" name)))))
          (function?
           (let ((function (fdefinition name)))
             (if (genericp function)
                 (generic-function-definitions name function)
                 (list (list `(function ,name)
                             (function-location function)))))))))

;;;;;; Ordinary (non-generic/macro/special) functions
;;;
;;; First we test if FUNCTION is a closure created by defstruct, and
;;; if so extract the defstruct-description (`dd') from the closure
;;; and find the constructor for the struct.  Defstruct creates a
;;; defun for the default constructor and we use that as an
;;; approximation to the source location of the defstruct.
;;;
;;; For an ordinary function we return the source location of the
;;; first code-location we find.
;;;
(defun function-location (function)
  "Return the source location for FUNCTION."
  (cond ((struct-closure-p function)
         (struct-closure-location function))
        ((c::byte-function-or-closure-p function)
         (byte-function-location function))
        (t
         (compiled-function-location function))))

(defun compiled-function-location (function)
  "Return the location of a regular compiled function."
  (multiple-value-bind (code-location error)
      (safe-definition-finding (function-first-code-location function))
    (cond (error (list :error (princ-to-string error)))
          (t (code-location-source-location code-location)))))

(defun function-first-code-location (function)
  "Return the first code-location we can find for FUNCTION."
  (and (function-has-debug-function-p function)
       (di:debug-function-start-location
        (di:function-debug-function function))))

(defun function-has-debug-function-p (function)
  (di:function-debug-function function))

(defun function-code-object= (closure function)
  (and (eq (vm::find-code-object closure)
	   (vm::find-code-object function))
       (not (eq closure function))))


(defun byte-function-location (fn)
  "Return the location of the byte-compiled function FN."
  (etypecase fn
    ((or c::hairy-byte-function c::simple-byte-function)
     (let* ((component (c::byte-function-component fn))
            (debug-info (kernel:%code-debug-info component)))
       (debug-info-function-name-location debug-info)))
    (c::byte-closure
     (byte-function-location (c::byte-closure-function fn)))))

;;; Here we deal with structure accessors. Note that `dd' is a
;;; "defstruct descriptor" structure in CMUCL. A `dd' describes a
;;; `defstruct''d structure.

(defun struct-closure-p (function)
  "Is FUNCTION a closure created by defstruct?"
  (or (function-code-object= function #'kernel::structure-slot-accessor)
      (function-code-object= function #'kernel::structure-slot-setter)
      (function-code-object= function #'kernel::%defstruct)))

(defun struct-closure-location (function)
  "Return the location of the structure that FUNCTION belongs to."
  (assert (struct-closure-p function))
  (safe-definition-finding
    (dd-location (struct-closure-dd function))))

(defun struct-closure-dd (function)
  "Return the defstruct-definition (dd) of FUNCTION."
  (assert (= (kernel:get-type function) vm:closure-header-type))
  (flet ((find-layout (function)
	   (sys:find-if-in-closure 
	    (lambda (x) 
	      (let ((value (if (di::indirect-value-cell-p x)
			       (c:value-cell-ref x) 
			       x)))
		(when (kernel::layout-p value)
		  (return-from find-layout value))))
	    function)))
    (kernel:layout-info (find-layout function))))

(defun dd-location (dd)
  "Return the location of a `defstruct'."
  ;; Find the location in a constructor.
  (function-location (struct-constructor dd)))

(defun struct-constructor (dd)
  "Return a constructor function from a defstruct definition.
Signal an error if no constructor can be found."
  (let ((constructor (or (kernel:dd-default-constructor dd)
                         (car (kernel::dd-constructors dd)))))
    (when (or (null constructor)
              (and (consp constructor) (null (car constructor))))
      (error "Cannot find structure's constructor: ~S"
             (kernel::dd-name dd)))
    (coerce (if (consp constructor) (first constructor) constructor)
            'function)))

;;;;;; Generic functions and methods

(defun generic-function-definitions (name function)
  "Return the definitions of a generic function and its methods."
  (cons (list `(defgeneric ,name) (gf-location function))
        (gf-method-definitions function)))

(defun gf-location (gf)
  "Return the location of the generic function GF."
  (definition-source-location gf (pcl::generic-function-name gf)))

(defun gf-method-definitions (gf)
  "Return the locations of all methods of the generic function GF."
  (mapcar #'method-definition (pcl::generic-function-methods gf)))

(defun method-definition (method)
  (list (method-dspec method)
        (method-location method)))

(defun method-dspec (method)
  "Return a human-readable \"definition specifier\" for METHOD."
  (let* ((gf (pcl:method-generic-function method))
         (name (pcl:generic-function-name gf))
         (specializers (pcl:method-specializers method))
         (qualifiers (pcl:method-qualifiers method)))
    `(method ,name ,@qualifiers ,(pcl::unparse-specializers specializers))))

;; XXX maybe special case setters/getters
(defun method-location (method)
  (function-location (or (pcl::method-fast-function method)
                         (pcl:method-function method))))

(defun genericp (fn)
  (typep fn 'generic-function))

;;;;;; Types and classes

(defun type-definitions (name)
  "Return `deftype' locations for type NAME."
  (maybe-make-definition (ext:info :type :expander name) 'deftype name))

(defun maybe-make-definition (function kind name)
  "If FUNCTION is non-nil then return its definition location."
  (if function
      (list (list `(,kind ,name) (function-location function)))))

(defun class-definitions (name)
  "Return the definition locations for the class called NAME."
  (if (symbolp name)
      (let ((class (kernel::find-class name nil)))
        (etypecase class
          (null '())
          (kernel::structure-class 
           (list (list `(defstruct ,name) (dd-location (find-dd name)))))
          #+(or)
          (conditions::condition-class
           (list (list `(define-condition ,name) 
                       (condition-class-location class))))
          (kernel::standard-class
           (list (list `(defclass ,name) 
                       (class-location (find-class name)))))
          ((or kernel::built-in-class 
               conditions::condition-class
               kernel:funcallable-structure-class)
           (list (list `(kernel::define-type-class ,name)
                       `(:error 
                         ,(format nil "No source info for ~A" name)))))))))

(defun class-location (class)
  "Return the `defclass' location for CLASS."
  (definition-source-location class (pcl:class-name class)))

(defun find-dd (name)
  "Find the defstruct-definition by the name of its structure-class."
  (let ((layout (ext:info :type :compiler-layout name)))
    (if layout 
        (kernel:layout-info layout))))

(defun condition-class-location (class)
  (let ((slots (conditions::condition-class-slots class))
        (name (conditions::condition-class-name class)))
    (cond ((null slots)
           `(:error ,(format nil "No location info for condition: ~A" name)))
          (t
           ;; Find the class via one of its slot-reader methods.
           (let* ((slot (first slots))
                  (gf (fdefinition 
                       (first (conditions::condition-slot-readers slot)))))
             (method-location 
              (first 
               (pcl:compute-applicable-methods-using-classes 
                gf (list (find-class name))))))))))

(defun make-name-in-file-location (file string)
  (multiple-value-bind (filename c)
      (ignore-errors 
        (unix-truename (merge-pathnames (make-pathname :type "lisp")
                                        file)))
    (cond (filename (make-location `(:file ,filename)
                                   `(:function-name ,string)))
          (t (list :error (princ-to-string c))))))

(defun source-location-form-numbers (location)
  (c::decode-form-numbers (c::form-numbers-form-numbers location)))

(defun source-location-tlf-number (location)
  (nth-value 0 (source-location-form-numbers location)))

(defun source-location-form-number (location)
  (nth-value 1 (source-location-form-numbers location)))

(defun resolve-file-source-location (location)
  (let ((filename (c::file-source-location-pathname location))
        (tlf-number (source-location-tlf-number location))
        (form-number (source-location-form-number location)))
    (with-open-file (s filename)
      (let ((pos (form-number-stream-position tlf-number form-number s)))
        (make-location `(:file ,(unix-truename filename))
                       `(:position ,(1+ pos)))))))

(defun resolve-stream-source-location (location)
  (let ((info (c::stream-source-location-user-info location))
        (tlf-number (source-location-tlf-number location))
        (form-number (source-location-form-number location)))
    ;; XXX duplication in frame-source-location
    (assert (info-from-emacs-buffer-p info))
    (destructuring-bind (&key emacs-buffer emacs-buffer-string 
                              emacs-buffer-offset) info
      (with-input-from-string (s emacs-buffer-string)
        (let ((pos (form-number-stream-position tlf-number form-number s)))
          (make-location `(:buffer ,emacs-buffer)
                         `(:position ,(+ emacs-buffer-offset pos))))))))

;; XXX predicates for 18e backward compatibilty.  Remove them when
;; we're 19a only.
(defun file-source-location-p (object) 
  (when (fboundp 'c::file-source-location-p)
    (c::file-source-location-p object)))

(defun stream-source-location-p (object)
  (when (fboundp 'c::stream-source-location-p)
    (c::stream-source-location-p object)))

(defun source-location-p (object)
  (or (file-source-location-p object)
      (stream-source-location-p object)))

(defun resolve-source-location (location)
  (etypecase location
    ((satisfies file-source-location-p)
     (resolve-file-source-location location))
    ((satisfies stream-source-location-p)
     (resolve-stream-source-location location))))

(defun definition-source-location (object name)
  (let ((source (pcl::definition-source object)))
    (etypecase source
      (null 
       `(:error ,(format nil "No source info for: ~A" object)))
      ((satisfies source-location-p)
       (resolve-source-location source))
      (pathname 
       (make-name-in-file-location source name))
      (cons
       (destructuring-bind ((dg name) pathname) source
         (declare (ignore dg))
         (etypecase pathname
           (pathname (make-name-in-file-location pathname (string name)))
           (null `(:error ,(format nil "Cannot resolve: ~S" source)))))))))

(defun setf-definitions (name)
  (let ((function (or (ext:info :setf :inverse name)
                      (ext:info :setf :expander name))))
    (if function
        (list (list `(setf ,name) 
                    (function-location (coerce function 'function)))))))


(defun variable-location (symbol)
  (multiple-value-bind (location foundp)
      ;; XXX for 18e compatibilty. rewrite this when we drop 18e
      ;; support.
      (ignore-errors (eval `(ext:info :source-location :defvar ',symbol)))
    (if (and foundp location)
        (resolve-source-location location)
        `(:error ,(format nil "No source info for variable ~S" symbol)))))

(defun variable-definitions (name)
  (if (symbolp name)
      (multiple-value-bind (kind recorded-p) (ext:info :variable :kind name)
        (if recorded-p
            (list (list `(variable ,kind ,name)
                        (variable-location name)))))))

(defun compiler-macro-definitions (symbol)
  (maybe-make-definition (compiler-macro-function symbol)
                         'define-compiler-macro
                         symbol))

(defun source-transform-definitions (name)
  (maybe-make-definition (ext:info :function :source-transform name)
                         'c:def-source-transform
                         name))

(defun function-info-definitions (name)
  (let ((info (ext:info :function :info name)))
    (if info
        (append (loop for transform in (c::function-info-transforms info)
                      collect (list `(c:deftransform ,name 
                                      ,(c::type-specifier 
                                        (c::transform-type transform)))
                                    (function-location (c::transform-function 
                                                        transform))))
                (maybe-make-definition (c::function-info-derive-type info)
                                       'c::derive-type name)
                (maybe-make-definition (c::function-info-optimizer info)
                                       'c::optimizer name)
                (maybe-make-definition (c::function-info-ltn-annotate info)
                                       'c::ltn-annotate name)
                (maybe-make-definition (c::function-info-ir2-convert info)
                                       'c::ir2-convert name)
                (loop for template in (c::function-info-templates info)
                      collect (list `(c::vop ,(c::template-name template))
                                    (function-location 
                                     (c::vop-info-generator-function 
                                      template))))))))

(defun ir1-translator-definitions (name)
  (maybe-make-definition (ext:info :function :ir1-convert name)
                         'c:def-ir1-translator name))


;;;; Documentation.

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (ext:info variable kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (maybe-push
       :generic-function 
       (if (and (fboundp symbol)
                (typep (fdefinition symbol) 'generic-function))
           (doc 'function)))
      (maybe-push
       :function (if (and (fboundp symbol)
                          (not (typep (fdefinition symbol) 'generic-function)))
                     (doc 'function)))
      (maybe-push
       :setf (if (or (ext:info setf inverse symbol)
		     (ext:info setf expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (ext:info type kind symbol)
		 (doc 'type)))
      (maybe-push
       :class (if (find-class symbol nil) 
		  (doc 'class)))
      (maybe-push
       :alien-type (if (not (eq (ext:info alien-type kind symbol) :unknown))
		       (doc 'alien-type)))
      (maybe-push
       :alien-struct (if (ext:info alien-type struct symbol)
			 (doc nil)))
      (maybe-push
       :alien-union (if (ext:info alien-type union symbol)
			 (doc nil)))
      (maybe-push
       :alien-enum (if (ext:info alien-type enum symbol)
		       (doc nil)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (describe (ecase namespace
              (:variable
               symbol)
              ((:function :generic-function)
               (symbol-function symbol))
              (:setf
               (or (ext:info setf inverse symbol)
                   (ext:info setf expander symbol)))
              (:type
               (kernel:values-specifier-type symbol))
              (:class
               (find-class symbol))
              (:alien-struct
               (ext:info :alien-type :struct symbol))
              (:alien-union
               (ext:info :alien-type :union symbol))
              (:alien-enum
               (ext:info :alien-type :enum symbol))
              (:alien-type
               (ecase (ext:info :alien-type :kind symbol)
                 (:primitive
                  (let ((alien::*values-type-okay* t))
                    (funcall (ext:info :alien-type :translator symbol) 
                             (list symbol))))
                 ((:defined)
                  (ext:info :alien-type :definition symbol))
                 (:unknown
                  (return-from describe-definition
                    (format nil "Unknown alien type: ~S" symbol))))))))

;;;;; Argument lists

(defimplementation arglist (symbol)
  (let* ((fun (or (macro-function symbol)
                  (symbol-function symbol)))
         (arglist
          (cond ((eval:interpreted-function-p fun)
                 (eval:interpreted-function-arglist fun))
                ((pcl::generic-function-p fun)
                 (pcl:generic-function-lambda-list fun))
                ((c::byte-function-or-closure-p fun)
                 (byte-code-function-arglist fun))
                ((kernel:%function-arglist (kernel:%function-self fun))
                 (handler-case (read-arglist fun)
                     (error () :not-available)))
                ;; this should work both for compiled-debug-function
                ;; and for interpreted-debug-function
                (t 
                 (handler-case (debug-function-arglist 
                                (di::function-debug-function fun))
                   (di:unhandled-condition () :not-available))))))
    (check-type arglist (or list (member :not-available)))
    arglist))

;;; A simple case: the arglist is available as a string that we can
;;; `read'.

(defun read-arglist (fn)
  "Parse the arglist-string of the function object FN."
  (let ((string (kernel:%function-arglist 
                 (kernel:%function-self fn)))
        (package (find-package
                  (c::compiled-debug-info-package
                   (kernel:%code-debug-info
                    (vm::find-code-object fn))))))
    (with-standard-io-syntax
      (let ((*package* (or package *package*)))
        (read-from-string string)))))

;;; A harder case: an approximate arglist is derived from available
;;; debugging information.

(defun debug-function-arglist (debug-function)
  "Derive the argument list of DEBUG-FUNCTION from debug info."
  (let ((args (di::debug-function-lambda-list debug-function))
        (required '())
        (optional '())
        (rest '())
        (key '()))
    ;; collect the names of debug-vars
    (dolist (arg args)
      (etypecase arg
        (di::debug-variable 
         (push (di::debug-variable-symbol arg) required))
        ((member :deleted)
         (push ':deleted required))
        (cons
         (ecase (car arg)
           (:keyword 
            (push (second arg) key))
           (:optional
            (push (debug-variable-symbol-or-deleted (second arg)) optional))
           (:rest 
            (push (debug-variable-symbol-or-deleted (second arg)) rest))))))
    ;; intersperse lambda keywords as needed
    (append (nreverse required)
            (if optional (cons '&optional (nreverse optional)))
            (if rest (cons '&rest (nreverse rest)))
            (if key (cons '&key (nreverse key))))))

(defun debug-variable-symbol-or-deleted (var)
  (etypecase var
    (di:debug-variable
     (di::debug-variable-symbol var))
    ((member :deleted)
     '#:deleted)))

(defun symbol-debug-function-arglist (fname)
  "Return FNAME's debug-function-arglist and %function-arglist.
A utility for debugging DEBUG-FUNCTION-ARGLIST."
  (let ((fn (fdefinition fname)))
    (values (debug-function-arglist (di::function-debug-function fn))
            (kernel:%function-arglist (kernel:%function-self fn)))))

;;; Deriving arglists for byte-compiled functions:
;;;
(defun byte-code-function-arglist (fn)
  ;; There doesn't seem to be much arglist information around for
  ;; byte-code functions.  Use the arg-count and return something like
  ;; (arg0 arg1 ...)
  (etypecase fn
    (c::simple-byte-function 
     (loop for i from 0 below (c::simple-byte-function-num-args fn)
           collect (make-arg-symbol i)))
    (c::hairy-byte-function 
     (hairy-byte-function-arglist fn))
    (c::byte-closure
     (byte-code-function-arglist (c::byte-closure-function fn)))))

(defun make-arg-symbol (i)
  (make-symbol (format nil "~A~D" (string 'arg) i)))

;;; A "hairy" byte-function is one that takes a variable number of
;;; arguments. `hairy-byte-function' is a type from the bytecode
;;; interpreter.
;;;
(defun hairy-byte-function-arglist (fn)
  (let ((counter -1))
    (flet ((next-arg () (make-arg-symbol (incf counter))))
      (with-struct (c::hairy-byte-function- min-args max-args rest-arg-p
                                            keywords-p keywords) fn
        (let ((arglist '())
              (optional (- max-args min-args)))
          ;; XXX isn't there a better way to write this?
          ;; (Looks fine to me. -luke)
          (dotimes (i min-args)
            (push (next-arg) arglist))
          (when (plusp optional)
            (push '&optional arglist)
            (dotimes (i optional)
              (push (next-arg) arglist)))
          (when rest-arg-p
            (push '&rest arglist)
            (push (next-arg) arglist))
          (when keywords-p
            (push '&key arglist)
            (loop for (key _ __) in keywords
                  do (push key arglist))
            (when (eq keywords-p :allow-others)
              (push '&allow-other-keys arglist)))
          (nreverse arglist))))))


;;;; Miscellaneous.

(defimplementation macroexpand-all (form)
  (walker:macroexpand-all form))

(defimplementation set-default-directory (directory)
  (setf (ext:default-directory) (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (pathname (ext:default-directory)))
  (default-directory))

(defimplementation default-directory ()
  (namestring (ext:default-directory)))

(defimplementation call-without-interrupts (fn)
  (sys:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (unix:unix-getpid))

(defimplementation lisp-implementation-type-name ()
  "cmucl")

(defimplementation quit-lisp ()
  (ext::quit))

;;; source-path-{stream,file,string,etc}-position moved into 
;;; swank-source-path-parser


;;;; Debugging

(defvar *sldb-stack-top*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (unix:unix-sigsetmask 0)
  (let* ((*sldb-stack-top* (or debug:*stack-top-hint* (di:top-frame)))
	 (debug:*stack-top-hint* nil))
    (handler-bind ((di:debug-condition 
		    (lambda (condition)
                      (signal (make-condition
                               'sldb-condition
                               :original-condition condition)))))
      (funcall debugger-loop-fn))))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (di:frame-down f)
	  for i from start below end
	  while f
	  collect f)))

(defimplementation print-frame (frame stream)
  (let ((*standard-output* stream))
    (debug::print-frame-call frame :verbosity 1 :number nil)))

(defimplementation frame-source-location-for-emacs (index)
  (code-location-source-location (di:frame-code-location (nth-frame index))))

(defimplementation eval-in-frame (form index)
  (di:eval-in-frame (nth-frame index) form))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (location (di:frame-code-location frame))
	 (debug-function (di:frame-debug-function frame))
	 (debug-variables (di::debug-function-debug-variables debug-function)))
    (loop for v across debug-variables collect 
          (list :name (di:debug-variable-symbol v)
                :id (di:debug-variable-id v)
                :value (ecase (di:debug-variable-validity v location)
                         (:valid 
                          (di:debug-variable-value v frame))
                         ((:invalid :unknown) 
                          ':not-available))))))

(defimplementation frame-catch-tags (index)
  (mapcar #'car (di:frame-catches (nth-frame index))))

(defun set-step-breakpoints (frame)
  (when (di:debug-block-elsewhere-p (di:code-location-debug-block
                                     (di:frame-code-location frame)))
    (error "Cannot step, in elsewhere code~%"))
  (let* ((code-location (di:frame-code-location frame))
         (debug::*bad-code-location-types* 
          (remove :call-site debug::*bad-code-location-types*))
         (next (debug::next-code-locations code-location)))
    (cond (next
           (let ((steppoints '()))
             (flet ((hook (frame breakpoint)
                      (let ((debug:*stack-top-hint* frame))
                        (mapc #'di:delete-breakpoint steppoints)
                        (let ((cl (di::breakpoint-what breakpoint)))
                          (break "Breakpoint: ~S ~S" 
                                 (di:code-location-kind cl)
                                 (di::compiled-code-location-pc cl))))))
               (dolist (code-location next)
                 (let ((bp (di:make-breakpoint #'hook code-location
                                               :kind :code-location)))
                   (di:activate-breakpoint bp)
                   (push bp steppoints))))))
         (t
          (flet ((hook (frame breakpoint values cookie)
                   (declare (ignore cookie))
                   (di:delete-breakpoint breakpoint)
                   (let ((debug:*stack-top-hint* frame))
                     (break "Function-end: ~A ~A" breakpoint values))))
            (let* ((debug-function (di:frame-debug-function frame))
                   (bp (di:make-breakpoint #'hook debug-function
                                           :kind :function-end)))
              (di:activate-breakpoint bp)))))))

(defimplementation sldb-step (frame)
  (cond ((find-restart 'continue)
         (set-step-breakpoints (nth-frame frame))
         (continue))
        (t
         (error "No continue restart."))))

(defun frame-cfp (frame)
  "Return the Control-Stack-Frame-Pointer for FRAME."
  (etypecase frame
    (di::compiled-frame (di::frame-pointer frame))
    ((or di::interpreted-frame null) -1)))

(defun frame-ip (frame)
  "Return the (absolute) instruction pointer and the relative pc of FRAME."
  (if (not frame)
      -1
      (let ((debug-fun (di::frame-debug-function frame)))
        (etypecase debug-fun
          (di::compiled-debug-function 
           (let* ((code-loc (di:frame-code-location frame))
                  (component (di::compiled-debug-function-component debug-fun))
                  (pc (di::compiled-code-location-pc code-loc))
                  (ip (sys:without-gcing
                       (sys:sap-int
                        (sys:sap+ (kernel:code-instructions component) pc)))))
             (values ip pc)))
          ((or di::bogus-debug-function di::interpreted-debug-function)
           -1)))))

(defun frame-registers (frame)
  "Return the lisp registers CSP, CFP, IP, OCFP, LRA for FRAME-NUMBER."
  (let* ((cfp (frame-cfp frame))
         (csp (frame-cfp (di::frame-up frame)))
         (ip (frame-ip frame))
         (ocfp (frame-cfp (di::frame-down frame)))
         (lra (frame-ip (di::frame-down frame))))
    (values csp cfp ip ocfp lra)))

(defun print-frame-registers (frame-number)
  (let ((frame (di::frame-real-frame (nth-frame frame-number))))
    (flet ((fixnum (p) (etypecase p
                         (integer p)
                         (sys:system-area-pointer (sys:sap-int p)))))
      (apply #'format t "~
CSP  =  ~X
CFP  =  ~X
IP   =  ~X
OCFP =  ~X
LRA  =  ~X~%" (mapcar #'fixnum 
                      (multiple-value-list (frame-registers frame)))))))


(defimplementation disassemble-frame (frame-number)
  "Return a string with the disassembly of frames code."
  (print-frame-registers frame-number)
  (terpri)
  (let* ((frame (di::frame-real-frame (nth-frame frame-number)))
         (debug-fun (di::frame-debug-function frame)))
    (etypecase debug-fun
      (di::compiled-debug-function
       (let* ((component (di::compiled-debug-function-component debug-fun))
              (fun (di:debug-function-function debug-fun)))
         (if fun
             (disassemble fun)
             (disassem:disassemble-code-component component))))
      (di::bogus-debug-function
       (format t "~%[Disassembling bogus frames not implemented]")))))


;;;; Inspecting

(defconstant +lowtag-symbols+ 
  '(vm:even-fixnum-type
    vm:function-pointer-type
    vm:other-immediate-0-type
    vm:list-pointer-type
    vm:odd-fixnum-type
    vm:instance-pointer-type
    vm:other-immediate-1-type
    vm:other-pointer-type)
  "Names of the constants that specify type tags.
The `symbol-value' of each element is a type tag.")

(defconstant +header-type-symbols+
  (flet ((suffixp (suffix string)
           (and (>= (length string) (length suffix))
                (string= string suffix :start1 (- (length string) 
                                                  (length suffix))))))
    ;; Is there a convinient place for all those constants?
    (remove-if-not
     (lambda (x) (and (suffixp "-TYPE" (symbol-name x))
                      (not (member x +lowtag-symbols+))
                      (boundp x)
                      (typep (symbol-value x) 'fixnum)))
     (append (apropos-list "-TYPE" "VM" t)
             (apropos-list "-TYPE" "BIGNUM" t))))
  "A list of names of the type codes in boxed objects.")

(defimplementation describe-primitive-type (object)
  (with-output-to-string (*standard-output*)
    (let* ((lowtag (kernel:get-lowtag object))
	   (lowtag-symbol (find lowtag +lowtag-symbols+ :key #'symbol-value)))
      (format t "lowtag: ~A" lowtag-symbol)
      (when (member lowtag (list vm:other-pointer-type
                                 vm:function-pointer-type
                                 vm:other-immediate-0-type
                                 vm:other-immediate-1-type
                                 ))
        (let* ((type (kernel:get-type object))
               (type-symbol (find type +header-type-symbols+
                                  :key #'symbol-value)))
          (format t ", type: ~A" type-symbol))))))

(defimplementation inspected-parts (o)
  (cond ((di::indirect-value-cell-p o)
	 (inspected-parts-of-value-cell o))
	(t
	 (destructuring-bind (text labeledp . parts)
	     (inspect::describe-parts o)
	   (let ((parts (if labeledp 
			    (loop for (label . value) in parts
				  collect (cons (string label) value))
			    (loop for value in parts
				  for i from 0
				  collect (cons (format nil "~D" i) value)))))
	     (values text parts))))))

(defun inspected-parts-of-value-cell (o)
  (values (format nil "~A~% is a value cell." o)
	  (list (cons "Value" (c:value-cell-ref o)))))

(defmethod inspected-parts ((o function))
  (let ((header (kernel:get-type o)))
    (cond ((= header vm:function-header-type)
	   (values 
	    (format nil "~A~% is a function." o)
	    (list (cons "Self" (kernel:%function-self o))
		  (cons "Next" (kernel:%function-next o))
		  (cons "Name" (kernel:%function-name o))
		  (cons "Arglist" (kernel:%function-arglist o))
		  (cons "Type" (kernel:%function-type o))
		  (cons "Code Object" (kernel:function-code-header o)))))
	  ((= header vm:closure-header-type)
	   (values (format nil "~A~% is a closure." o)
		   (list* 
		    (cons "Function" (kernel:%closure-function o))
		    (loop for i from 0 below (- (kernel:get-closure-length o) 
						(1- vm:closure-info-offset))
			  collect (cons (format nil "~D" i)
					(kernel:%closure-index-ref o i))))))
	  (t (call-next-method o)))))

(defmethod inspected-parts ((o kernel:code-component))
  (values (format nil "~A~% is a code data-block." o)
	  `(("First entry point" . ,(kernel:%code-entry-points o))
	    ,@(loop for i from vm:code-constants-offset 
		    below (kernel:get-header-data o)
		    collect (cons (format nil "Constant#~D" i)
				  (kernel:code-header-ref o i)))
	    ("Debug info" . ,(kernel:%code-debug-info o))
	    ("Instructions"  . ,(kernel:code-instructions o)))))

(defmethod inspected-parts ((o kernel:fdefn))
  (values (format nil "~A~% is a fdefn object." o)
	  `(("Name" . ,(kernel:fdefn-name o))
	    ("Function" . ,(kernel:fdefn-function o)))))


;;;; Profiling
(defimplementation profile (fname)
  (eval `(profile:profile ,fname)))

(defimplementation unprofile (fname)
  (eval `(profile:unprofile ,fname)))

(defimplementation unprofile-all ()
  (profile:unprofile)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (profile:report-time))

(defimplementation profile-reset ()
  (profile:reset-time)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  profile:*timed-functions*)

(defimplementation profile-package (package callers methods)
  (profile:profile-all :package package
                       :callers-p callers
                       #-cmu18e :methods #-cmu18e methods))


;;;; Multiprocessing

#+mp
(progn
  (defimplementation startup-multiprocessing ()
    ;; Threads magic: this never returns! But top-level becomes
    ;; available again.
    (mp::startup-idle-and-top-level-loops))

  (defimplementation spawn (fn &key (name "Anonymous"))
    (mp:make-process fn :name name))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (mp:process-whostate thread))

  (defimplementation current-thread ()
    mp:*current-process*)

  (defimplementation all-threads ()
    (copy-list mp:*all-processes*))

  (defimplementation interrupt-thread (thread fn)
    (mp:process-interrupt thread fn))

  (defimplementation kill-thread (thread)
    (mp:destroy-process thread))

  (defvar *mailbox-lock* (mp:make-lock "mailbox lock"))
  
  (defstruct (mailbox (:conc-name mailbox.)) 
    (mutex (mp:make-lock "process mailbox"))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock-held (*mailbox-lock*)
      (or (getf (mp:process-property-list thread) 'mailbox)
          (setf (getf (mp:process-property-list thread) 'mailbox)
                (make-mailbox)))))
  
  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (mp:with-lock-held (mutex)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message))))))
  
  (defimplementation receive ()
    (let* ((mbox (mailbox mp:*current-process*))
           (mutex (mailbox.mutex mbox)))
      (mp:process-wait "receive" #'mailbox.queue mbox)
      (mp:with-lock-held (mutex)
        (pop (mailbox.queue mbox)))))

  ) ;; #+mp

;; Local Variables:
;; pbook-heading-regexp:    "^;;;\\(;+\\)"
;; pbook-commentary-regexp: "^;;;\\($\\|[^;]\\)"
;; End: