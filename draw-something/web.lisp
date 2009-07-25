;; web.lisp - Web generator version of draw-something.
;; Copyright (C) 2009 Rhea Myers rhea@myers.studio
;;
;; This file is part of draw-something.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;(in-package "DRAW-SOMETHING")

;;TODO - Write then swap all files?
;;TODO - Can this be AGPL and the rest of the package GPL unless this is used?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load code and libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "load")

(require 'cl-ppcre)
(require 'gzip-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO Use same as draw-something so they can be configured simultaneously
(defparameter +folder-path+ ".")
(defparameter +index-file-path+ (format nil "~a/index.html" +folder-path+))
(defparameter +rss-file-path+ (format nil "~a/rss.xml" +folder-path+))
(defparameter +web-page-count+ 2)
(defparameter +rss-entry-count+ 10)
(defparameter +svg-extention+ ".svgz")
(defparameter +svg-extention-length+ (length +svg-extention+))
(defparameter +rss-header+ "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
  <channel>
    <title>draw-something</title>
    <link>https://rhea.art/draw-something/</link>
    <atom:link href=\"https://rhea.art/draw-something/rss.xml\" rel=\"self\" type=\"application/rss+xml\" />
    <description>Images by draw-something.</description>
    <language>en</language>
")
(defparameter +rss-footer+ "  </channel>
</rss>
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ends-with (source target)
  "Check whether the source string ends with the target string"
  (= (search target source :from-end t)
     (length source)))

(defun slurp-gzip-file (file-path)
  "Slurp the contents of the file into a string"
  (gzip-stream:with-open-gzip-file (stream file-path)
    (with-output-to-string (str)
      (loop for char = (read-char stream nil nil)
	   while char
	   do (write-char char str))
      str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-svg-filename (svg-filename)
  "Change an .svg extention to an .html extention"
  (format nil "~a.html" (pathname-name svg-filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVG Tag writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-svg-dimensions (svg-filename)
  "Get the width and height of the svg file"
  (let ((file-text (slurp-gzip-file svg-filename)))
    (cl-ppcre:register-groups-bind 
     (width height) 
     ("width=\"([0-9]+)[^\"]*\" height=\"([0-9]+)[^\"]*\"" file-text)
     (values width height))))

(defun write-svg-tag (html-file svg-filename)
  (with-open-file (svg-file svg-filename)
    (multiple-value-bind (width height) (get-svg-dimensions svg-filename)
      (format html-file "<p align=\"center\"><embed src=\"~a~a\" width=\"~a\" height=\"~a\" /></p>~%"
	      (pathname-name svg-filename) +svg-extention+ width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-link (html-file text link)
  "Write the html link, or just the label if the link is null"
  (if link
      (format html-file "<a href=\"~a\">~a</a>" link text)
      (format html-file "~a" text)))
  
(defun write-links (html-file previous-html-file next-html-file)
  "Write the next and previous links (or placeholders for first/last files)"
  (format html-file "<p align=\"center\">")
  (write-link html-file "next" next-html-file)
  (format html-file "&nbsp;&nbsp;")
  (write-link html-file "index" "index.html")
  (format html-file "&nbsp;&nbsp;")
  (write-link html-file "previous" previous-html-file)
  (format html-file "</p>~%"))

(defun write-web-page (svg-filename previous-html-file next-html-file)
  "Write a web page to wrap an svg file, complete with links to next/prev"
  (with-open-file (html-file (format nil "~a/~a" +folder-path+
				     (html-svg-filename svg-filename))
			     :direction :output
			     :if-exists :supersede)
    (format html-file "<html><head><title>~a</title>
<link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS\" href=\"https://rhea.art/draw-something/drawings/rss.xml\" />
<link rel=\"StyleSheet\" href=\"style.css\" type=\"text/css\" media=\"all\">
</head><body>~%"
	    (pathname-name svg-filename))
    (write-svg-tag html-file svg-filename)
    ;; YOU MUST CHANGE THE SOURCE LINK IF YOU MODIFY THIS CODE
    (format html-file "<p align=\"center\"><a href=\"http://creativecommons.org/licenses/by-sa/3.0/\">Image licence - CC-BY-SA</a>&nbsp;&nbsp;<a href=\"https://rhea.art/git/?p=rheart.git;a=tree;f=draw-something\">Code licence - GPL 3 or higher.</a></p>~%")
    (write-links html-file previous-html-file next-html-file)
    (format html-file "</body></html>~%")))

(defun write-web-pages (svg-list next-html limit)
  "Recursively generate (at most) the given number of web pages for svg files"
  (when (and svg-list 
	     (> limit 0))
    (let ((html-filename (or next-html 
			     (html-svg-filename (car svg-list))))
	  (prev-html (if (cadr svg-list) (html-svg-filename (cadr svg-list)))))
      (write-web-page (car svg-list) prev-html next-html)
      (write-web-pages (cdr svg-list) html-filename (1- limit)))))

(defun link-index-page (html-file-path)
  "Make a link to the most recent html file as the index for the directory"
  (sb-posix:unlink +index-file-path+)
  (sb-posix:symlink html-file-path +index-file-path+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rfc-822-time (universal-time)
  "Return a string representing the universal-time in rfc 822 time format"
  (multiple-value-bind (seconds minutes hours day month year day-of-the-week 
				daylight-savings-time-flag time-zone)
      (decode-universal-time universal-time 0) 
    (declare (ignore day-of-the-week daylight-savings-time-flag time-zone))
    (format nil "~a ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT" 
	    day
	    (nth (1- month) 
		 '("Jan" "Feb" "Mar" "Apr"
		   "May" "Jun" "Jul" "Aug"
		   "Sep" "Oct" "Nov" "Dec"))
	    year hours minutes seconds)))

(defun file-rss-timestamp (file-path)
  "Get a file's creation time in a format usable in an rss entry"
  (rfc-822-time (file-write-date file-path)))

(defun write-rss-entry (rss-file svg-file)
  "Write an rss entry for the svg file"
  (let ((html-filename (html-svg-filename svg-file))
	(basename (pathname-name svg-file)))
    (format rss-file "  <title>draw-something image ~a</title>
      <link>https://rhea.art/draw-something/drawings/~a</link>
      <guid>https://rhea.art/draw-something/drawings/~a</guid>
      <pubDate>~a</pubDate>
      <description><![CDATA[<p><a href=\"https://rhea.art/draw-something/drawings/~a\">draw-something image ~a</a></p>]]></description>
    </item>
"
	    ;; We use the svg file time as the html file may have been remade
	    basename html-filename html-filename (file-rss-timestamp svg-file) html-filename basename)))

(defun write-rss-file (svg-filenames)
  "Write the rss file listing the most recent html files"
  (with-open-file (rss-file +rss-file-path+
			    :direction :output
			    :if-exists :supersede)
    (format rss-file +rss-header+)
    (loop for i below +rss-entry-count+
       for svg-filename in svg-filenames
       do (write-rss-entry rss-file svg-filename))
    (format rss-file +rss-footer+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-draw-something ()
  "An sbcl-specific wrapper to make draw-something useful as a script"
  (let ((filepath (run)))
      #+sbcl (sb-ext:run-program "/bin/gzip" 
				 (list "--suffix" "z" 
				       (namestring filepath)) 
				 :wait t))
  ;; Yes it would be better if we cached the svg info somehow
  (let ((svg-files (reverse (directory (format nil "~a/*~a" +folder-path+ 
					       +svg-extention+)))))
    (when svg-files
      (write-web-pages svg-files nil +web-page-count+)
      (link-index-page (html-svg-filename (car svg-files)))
      (write-rss-file svg-files)))
  #+sbcl (quit))
