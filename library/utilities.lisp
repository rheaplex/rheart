;;  utilities.lisp - Various utilities.
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
;;
;; This file is part of rheart.
;; 
;; rheart is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; rheart is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defmacro while (test &rest body)
  "Execute the body until test evaluates to false."
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &rest body)
  "Execute the body until test evaluates to true."
  `(do ()
       ((eq t 
	    ,test))
     ,@body))

(defmethod debug-message (msg)
  "Write the message to the error stream, not to standard output."
  (format *debug-io* 
	  "~A~%" 
	  msg)
  (finish-output *debug-io*))