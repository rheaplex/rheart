;; cybernetic.lisp : random image description generator
;;
;; Copyright (c) 2004 Rhea Myers, rhea@myers.studio
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(load "../library/clopt.lisp")
(load "./cybernetic.lisp")

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod maybe-help ()
  "Check the command line for the --help flag and act accordingly."
  (when (get-arg "help")
    (format t "The Cybernetic Artwork Nobody Wrote~%Rhea Myers (C) 2004~%usage: <lisp> run.lisp <options> where option is:~%    -c, --count : the number of images to describe (default: 10)~%~%")
    (quit)))

(defmethod run ()
  "Get the number of descriptions to generate and generate them."
  (maybe-help)
  (let ((number-of-descriptions (parse-integer (get-arg-value "count" 
							      :short "c" 
							      :default "10"))))
    (dotimes (i number-of-descriptions)
      (format t "~a~%" (generate-description))))
  (finish-output)
  (quit))

;; Just call the main method when loaded

(run)