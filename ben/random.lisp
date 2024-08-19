;; 1970s-compliant pseudo-random number generator.
;; See: https://en.wikipedia.org/wiki/Linear_congruential_generator

(defparameter +rand-a+ 1664525)
(defparameter +rand-c+ 1013904223)
(defparameter +rand-m+ (expt 2 32))
(defvar *rand-seed* 0)

(defun set-randseed (seed)
  (setf *rand-seed* seed))

(defun rand ()
  (setf *rand-seed*
        (mod (* +rand-a+
                (+ *rand-seed* +rand-c+))
             +rand-m+)))

(defun randint (max)
  "Return an integer from 0 to max, exclusive."
  (ash (* (rand) max)
       -32))

(defun random-range-int (min max)
  "Return an integer from min to max, exclusive."
  (let ((range (- max min)))
    (if (= range 0)
        min
        (+ (randint range) min))))

(defun random-range (min max)
  "Return an integer from min to max, exclusive."
  ;;FIXME:!!!!
  (if (= min max)
      min
      (let ((scale (/ (- max min) 1000)))
        (+ (* (randint 1000) scale) min))))

(defun choose-one-of (&rest possibilities)
  "Choose one of the given options."
  (nth (randint (length possibilities)) possibilities))
