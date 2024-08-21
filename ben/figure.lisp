;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct figure
  allocated-space
  (actual-bounds nil)
  (outline '())
  (closed nil))

(defun set-cell-figure (x y fig)
  "Set the cell's figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (setf (cell-figure (get-cell x y)) fig)
  (if (null (figure-actual-bounds fig))
      (setf (figure-actual-bounds fig)
            (make-rectangle :x x :y y :width 1 :height 1))
      (rectangle-include-point (figure-actual-bounds fig) (make-point x y))))

(defun set-cell-figure-outline (x y fig)
  "Set the cell to be part of the outline of the figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (set-cell-figure x y fig)
  (set-cell-status x y 'figure-outline))

(defun set-cell-figure-unused-space (x y fig)
  "Set the cell to be part of the outline of the figure."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (assert (co-ordinates-in-rectangle-p x y (figure-allocated-space fig)))
  (set-cell-figure x y fig)
  (set-cell-status x y 'unused-inside-figure))

(defparameter *figures* '()
  "The figures of the image.")

(defun figure-outline-cell-p (x y)
  "Is the cell part of the outline of a figure?"
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (not (cell-figure (get-cell x y))))

(defun figure-closed-p (fig)
  "Has the figure been marked as closed?"
  (figure-closed fig))

(defun cell-figure-closed-p (x y)
  "Is the figure of the cell at x,y closed? No figure (or open) returns nil."
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  (let ((fig (cell-figure (get-cell x y))))
    (and fig
         (figure-closed-p fig))))

(defun cell-figure-p (x y)
  "Is the point in a figure or on its outline (not optically)?"
  (assert (>= x 0))
  (assert (< x *image-width*))
  (assert (>= y 0))
  (assert (< y *image-height*))
  ;;  (eq (cell-status (get-cell x y))
  ;;    'unused-inside-figure))
  (cell-figure (get-cell x y)))



(defun apply-figure-fill (fig)
  "Set the cells inside the figure outline to being figure space."
  (assert (figure-closed-p fig))
  (fill-polygon (figure-outline fig)
                (figure-actual-bounds fig)
                (lambda (x y)
                  (when (null (get-cell-status x y))
                    (set-cell-figure-unused-space x y fig)))))
