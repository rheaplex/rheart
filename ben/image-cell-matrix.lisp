(defparameter *image-width* 1024
  "The width of the image cell matrix, in cells.")

(defparameter *image-height* 768
  "The height of the image cell matrix, in cells.")

(defstruct cell
  "An image matrix cell."
  (status nil)
  (figure nil))

(defparameter *statuses* '(nil rough figure-outline unused-inside-figure)
  "The statuses each cell can have. nil is ground.")

(defparameter *image-cells* (make-array (list *image-width* *image-height*))
  "The image cell matrix. Row major.")

(defun set-cell (x y value)
  "Set cell x,y of the image cells matrix to value."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (setf (aref *image-cells*
              x y)
        value))

(defun get-cell (x y)
  "Get the value of cell x,y in the image cells matrix. "
  (assert (>= x 0))
  (assert (<= x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (aref *image-cells*
        x y))

(defun set-cell-status (x y status)
  "Set the cell's status."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (setf (cell-status (get-cell x y)) status))

(defun get-cell-status (x y)
  "Set the cell's status."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (cell-status (get-cell x y)))

(defun apply-line-cells (x0 y0 x1 y1 fun)
  "Bresenham's algorithm. Always applies left to right."
  ;;FIXME: only connect cells horizontally or vertically.
  (assert (>= x0 0))
  (assert (< x0 *image-width*))
  (assert (>= y0 0))
  (assert (< y0 *image-height*))
  (assert (>= x1 0))
  (assert (< x1 *image-width*))
  (assert (>= y1 0))
  (assert (< y1 *image-height*))
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (when steep
      (rotatef x0 y0)
      (rotatef x1 y1))
    (when (> x0 x1)
      (rotatef x0 x1)
      (rotatef y0 y1))
    (let* ((deltax (- x1 x0))
           (deltay (abs (- y1 y0)))
           (err (- (/ (+ deltax 1) 2)))
           (ystep (if (< y0 y1) 1 -1))
           (y y0))
      (loop for x from x0 to x1 ;; Below?
         do (progn
              (if steep
                  (funcall fun y x)
                  (funcall fun x y))
              (setf err (+ err deltay))
              (when (>= err 0)
                (setf y (+ y ystep))
                (setf err (- err deltax))))))))

(defun apply-rect-cells (x y width height fun)
  "Applies the function to each cell of each row of the area top-to-bottom."
  (assert (>= x 0))
  (assert (<= x *image-width*))
  (assert (>= y 0))
  (assert (<= y *image-height*))
  (assert (>= (+ x width) 0))
  (assert (<= (+ x width) *image-width*))
  (assert (>= (+ y height) 0))
  (assert (<= (+ y height) *image-height*))
  (dotimes (j height)
    (dotimes (i width)
      (funcall fun
              (+ x i)
              (+ y j)))))

(defun clear-ground-p (x y width height)
  "Is the rectangular space clear of figure or rough cells?"
  (let ((clear t))
    (block clear-block
      (apply-rect-cells x y (1+ width) (1+ height)
                        (lambda (h v)
                          (when (not (eq (cell-status (get-cell h v)) nil))
                            (setf clear nil)
                            (return-from clear-block)))))
      clear))

(defun initialise-cell-matrix ()
  "Initialize the cell matrix."
  (apply-rect-cells 0 0 *image-width* *image-height*
                    (lambda (x y) (set-cell x y (make-cell)))))

(defun roughen (&optional (count 10))
  "Set some cells to be unusable for drawing."
  (dotimes (i count)
    (setf (cell-status (get-cell (random  *image-width*)
                                 (random  *image-height*)) )
          'rough)))

(defun write-cell-matrix-ppm-file (&optional (filename "./cells.ppm")
                                             (comment ""))
  "Write the cell matrix to a colour, binary ppm file."
  ;; Write the header in character stream mode
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (format stream "P6~%#~A~%~D ~D~%~d~%"
            comment *image-width* *image-height* 255))
  ;; Append the rasters in byte stream mode
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :append
                          :element-type '(unsigned-byte 8))
    (dotimes (j *image-height*)
      (dotimes (i *image-width*)
        (case (get-cell-status i (- *image-height* j 1))
          (figure-outline (write-byte 0 stream)
                          (write-byte 0 stream)
                          (write-byte 0 stream))
          (unused-inside-figure (write-byte 0 stream)
                                (write-byte 255 stream)
                                (write-byte 0 stream))
          (rough (write-byte 255 stream)
                 (write-byte 0 stream)
                 (write-byte 0 stream))
          (t (write-byte 255 stream)
             (write-byte 255 stream)
             (write-byte 255 stream)))))
    (namestring stream)))
