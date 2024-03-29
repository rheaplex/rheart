;;  ae.lisp -  A toy aesthetics description and evaluation system
;;  Copyright (C) 2004  Rhea Myers rhea@myers.studio
;;
;; This file is part of ae.
;;
;; ae is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; ae is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;;	When comparing properties to criteria, allow conceptual slippage.
;;      How should this affect weights?
;;	Representational imagery, representational colour, referential colour,
;;      associative colour.
;;	An emotional slipnet.
;;	Slipnet-style "conceptual levels" (levels of abstractness)?
;;	Scanners for relative/relational properties/links (bigger, inside,
;;      adjacent, row of)
;;	- Should these have prerequisites, or a predicate function? For example,
;;        making sure that a bigger
;;    object isn't ultimately inside an object it's smaller than requires
;;        walking the Image. Making sure an
;;    object is bigger requires evaluating one of a range of sizes (or
;;        nothing) against another.
;;      Possibly allow either, simple prereq/blocks *or* a fun depending on the
;;      input. Wrap to always a fun or check in the calling routine (no, wrap a
;;      lambda???)
;;	Consequential relationships. So if something's bigger, the other must
;;      be smaller.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package 'ptester);; "./third-party/ptester.lisp")

;; Global

(defconstant the-random-state (make-random-state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass link ()
  ((to :accessor link-to
       :initarg :to
       :initform ""
       :documentation "The concept the link leads to.")
   (by :accessor link-by
       :initarg :by
       :initform ""
       :documentation "The concept the concepts are linked by.")
   (strength :accessor link-strength
       :initarg :strength
       :initform 0.0
       :documentation "The strength (weight) of the link."))
  (:documentation "A conceptual link between two concepts, a weighted
 relationship to another concept. Do we need bidirectional and to-self
relationships as well? "))

(defmethod finish-create ((the-link link))
  "Once every Link in every Concept is declared, we can iterate through and replace names with objects. This method does that for a single Link."
  (setf (link-to the-link) (aref (concept-concepts 'concept)
         (link-to the-link)))
  (setf (link-by the-link) (aref (concept-concepts 'concept)
         (link-by the-link))))

(defmethod print-object ((the-link link) the-stream)
  "Describe the link."
  (print-object (link-by the-link) the-stream))

(defmethod equals ((left link) (right link))
  (and (equal (link-to left) (link-to right))
       (equal (link-by left) (link-by right))
       (= (link-strength left) (link-strength right))))

;; (deftest
;;   (let ((a (make-instance 'link))
;;    (b (make-instance 'link)))
;;     (with-tests (:name "ontology - link")
;;      (test t (equals a b) :fail-info "Instance equality"))))

(defclass concept ()
  ((name :accessor concept-name
   :initarg :name
   :initform ""
   :documentation "The name of the concept. Must be unique.")
   (general :accessor concept-general
      :initarg :general
      :initform '()
      :documentation
      "The more general concepts that the concept can generalise to. Finished by finish-create.")
   (specific :accessor concept-specific
       :initarg :specific
       :initform '()
       :documentation
       "The more specific concepts that a concept can be specified to.")
   (related :accessor concept-related
      :initarg :related
      :initform '()
      :documentation
      "Concepts related to this concept by link instances."))
  (:documentation "A Concept in the slipnet/semantic net. The class keeps track of all Concepts in a program and all relationships between them. A Concept has a name, a more general concept, and relationships to other concepts. It also keeps track of concepts that are more specific than it is.
  TODO:
  We should have weights for generalisations
  And we should have pre-requisites for applying or NOT applying a Concept to an object with other concepts already applied
  And atoms/sub-conceipts for the conceipt"))

(defconstant *concept-concepts* (make-hash-table :test 'equal)
  "A lookup table of all the concepts, keyed on their names.")

(defmethod concept-concepts ()
  *concept-concepts*)

(defmethod initialize-instance :after ((instance concept) &rest initargs)
  "Store the concept under its name in the class hash of concepts."
  initargs ; Silence the compiler warning
  (setf (gethash (concept-name instance) (concept-concepts)) instance))

(defmethod print-object ((the-concept concept) the-stream)
  "Describe the concept by name."
  (format the-stream "~A" (concept-name the-concept)))

(defmethod equals ((left concept) (right concept))
  "Compare ourself to another object by name. So names must be unique..."
  (eq (concept-name left)
      (concept-name right)))

(defmethod find-concept (name)
  "Find a concept object by name"
  (gethash name (concept-concepts)))

(defmethod link-concepts ()
  "Call exactly once after all Concepts have been defined to finish setting up the Concept graph."
  ;; Substitute the more general concept for its name
  (maphash #'(lambda (key value)
         key ; Silence unused variable warning
         ;; Note that concept-general is a string now, we replace this
         (setf (concept-general value)
         (find-concept (concept-general value))))
         (concept-concepts))
  ;; Add each object to its general concept's list of specific concepts
  (maphash #'(lambda (key value)
         key ; Silence unused variable warning
         (setf (concept-specific (concept-general value))
         (cons value (concept-specific (concept-general value)))))
         (concept-concepts))
  ;; Finish the relationships
  (maphash #'(lambda (key value)
         key ; Silence unused variable warning
         (dolist (r (concept-related value))
     (finish-create r)))
     (concept-concepts)))

(defmethod print-all-concepts ()
  "Print a list of all the concept names."
  (format t "Concepts:~%")
  (maphash #'(lambda (key value)
         value ; Silence unused variable warning
         (format t "    ~A" key))
     (concept-concepts)))

(defmethod random-leaf-concept (general)
  "Randomly choose a leaf Concept below the named general concept. A leaf Concept is a concept with no more specific concepts, i.e. an empty specific list. This version is incredibly inefficient and should be improved to findall then choose..."
  (let ((the-concept (find-concept general)))
    (loop while (not (eq (concept-specific the-concept) '()))
    do (let ((specific (concept-specific the-concept)))
         (setf the-concept
         (nth (random (length specific)
          the-random-state)
        specific))))
    the-concept))

;; (deftest
;;   (let ((a (make-instance 'concept))
;;    (b (make-instance 'concept)))
;;     (with-tests (:name "ontology - concept")
;;      (test t (equals a b) :fail-info "Instance equality"))))

(defclass weight ()
  ((value :accessor weight-value
     :initarg :value
     :initform 0.5
     :documentation "The conceptual weight or value of the object.")
   (min :accessor weight-min
  :allocation :class
  :initform 0.0
  :documentation "The minimum value for weights.")
   (max :accessor weight-max
  :allocation :class
  :initform 1.0
  :documentation "The maximum value for weights."))
  (:documentation "Abstract base class for a property of some object or system with a weight/value."))

(defmethod initialize-instance :after ((instance weight) &rest initargs)
  "Randomly generate a weight within the constraints set by our min and max.
   The weight will be >= min, < max ."
  initargs ; Silence the coimpiler warning
  (let ((weight-range (- (weight-max instance)
       (weight-min instance))))
    (setf (weight-value instance) (+ (weight-min instance)
             (random weight-range
               the-random-state)))))

(defmethod equals ((left weight) (right weight))
  "Compare our weight to the other Weight's weight for equality."
  (format t "~A ~A ~%" (weight-value left) (weight-value right))
  (eql (weight-value left) (weight-value right)))

(defmethod print-object ((object weight) the-stream)
  "Describe the weight value."
  (format the-stream "weight: ~A" (weight-value object)))

;;;; Random element, can't test equality?
;;(deftest
;;  (let ((a (make-instance 'weight))
;;	(b (make-instance 'weight)))
;;    (with-tests (:name "ontology - weight")
;;      (test t (equals a b) :fail-info "Instance equality")
;;		))
;;)

(defclass formal-property (weight)
  ((concept :accessor formal-property-concept
      :initarg :concept
      :documentation "The concept the property embodies.")
   (min :accessor weight-min
  :allocation :class
  :initform 0.001
  :documentation "The minimum value for weights.")
   (max :accessor weight-max
  :allocation :class
  :initform 1.0
  :documentation "The maximum value for weights."))
  (:documentation
  "A formal property of a figure of an image. The weight of this property will be positive, to represent its strength of presence."))

(defmethod initialize-instance :after ((instance formal-property)
               &rest initargs)
  "Store the concept under its name in the class hash of concepts."
  initargs ; Silence the compiler warning
  (setf (formal-property-concept instance)
  (random-leaf-concept "formal quality"))
  (setf (gethash (concept-name (formal-property-concept instance))
     (concept-concepts)) instance))

(defmethod equals ((left formal-property) (right formal-property))
  (and (equals (formal-property-concept left)
         (formal-property-concept right))
       ;; Call equals on weight
       (call-next-method left right)))

(defmethod print-object ((instance formal-property) the-stream)
  "Describe the concept by name."
  (format the-stream
    "~A ~A"
    (concept-name (formal-property-concept instance))
    (weight-value instance)))

(defclass aesthetic-criterion (formal-property)
  ((domain :accessor aesthetic-criterion-domain
      :initarg :domain
      :documentation "The concept the criterion applies to.")
   (min :accessor min-value
  :initform -0.999
  :initarg :min
  :documentation "The minimum possible value for criteria")
   (max :accessor max-value
  :initform 0.999
  :initarg :max
  :documentation "The maximum possible value for criteria"))
  (:documentation "A criterion for aesthetic evaluation of an image. The weight of this criterion can be positive or negative, to reflect varying strengths of like and dislike. Inheriting from FormalProperty may be slighly broken, but it's convenient."))

(defmethod initialize-instance :after ((instance aesthetic-criterion)
               &rest initargs)
  "Randomly choose the criterions' domain."
  initargs ; Silence the unused variable warning
  (setf (aesthetic-criterion-domain instance)
  (random-leaf-concept "extra-aesthetic domain")))

(defmethod print-object ((instance aesthetic-criterion) the-stream)
  "Print the criterion."
  (format the-stream "Criterion: ~a " (aesthetic-criterion-domain instance))
  ;; Describe the formal property elements
  (call-next-method))


(defclass figure ()
  ((properties :accessor figure-properties
         :initform '()
         :initarg :properties
         :documentation "The properties of the figure."))
  (:documentation "A figure described by its formal properties."))

(defmethod initialize-instance :after ((instance figure)
               &rest initargs
               &key (property-count 10))
  "Randomly choose the given number of formal properties to describe the figure."
  initargs ; Silence the unused variable warning
  (dotimes (i property-count)
    (push (make-instance 'formal-property)
    (figure-properties instance))))

(defmethod print-object ((instance figure) the-stream)
  "Describe the formal qualities of the figures of the image."
  (format the-stream "Figure: ~%")
  (dolist (prop (figure-properties instance))
    (format the-stream "        ~a~%" prop)))

(defclass image ()
  ((figures :accessor image-figures
       :initform '()
       :initarg :criteria
       :documentation "The figures of the image."))
  (:documentation "An image consisting of a number of figures."))

(defmethod initialize-instance :after ((instance image)
               &rest initargs
               &key (figure-count 10)
               (property-count-min 4)
               (property-count-max 10))
  "Generate a composition with the given number of figures, described by a random number (between minProperties/maxProperties) of formal properties each."
  initargs ; Silence the unused variable warning
  (dotimes (i figure-count)
    (let ((property-count (+ property-count-min
           (random (- property-count-max
          property-count-min)
             the-random-state))))
      (push (make-instance 'figure :property-count property-count)
      (image-figures instance)))))

(defmethod print-object ((img image) the-stream)
  "Describe the formal qualities of each of the figures in the image."
  (format the-stream "Image: ~%")
  (dolist (fig (image-figures img))
    (format the-stream "    ~A" fig)))

(defclass aesthetic ()
  ((criteria :accessor aesthetic-criteria
       :initform '()
       :initarg :criteria
       :documentation "The criteria that the aesthetic looks for in a work"))
  (:documentation "An aesthetic described as a series of formal likes (positively weighted criteria) and dislikes (negatively weighted criteria) determined by extra-aesthetic concerns. This is essentially an aesthetic of taste, and cannot handle combination, absence, irony, etc. Give me time... :-)"))

(defmethod initialize-instance :after ((instance aesthetic)
               &rest initargs
               &key (criteria-count 10))
  "Generate the given number of aesthetic criteria with random properties."
  initargs ; Silence the unused variable warning
  (dotimes (i criteria-count)
    (push (make-instance 'aesthetic-criterion)
    (aesthetic-criteria instance))))

(defmethod print-object ((ae aesthetic) the-stream)
  "Describe an aesthetic's criteria."
  (format the-stream "Aesthetic: ~%")
  (dolist (crit (aesthetic-criteria ae))
    (format the-stream "    ~a~%" crit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concept Registration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-instance 'concept :name "concept" :general "concept")
(make-instance 'concept :name "formal quality" :general "concept")

(make-instance 'concept :name "shape" :general "formal quality")
(make-instance 'concept :name "line" :general "shape")
(make-instance 'concept :name "circle" :general "shape")
(make-instance 'concept :name "triangle" :general "shape")
(make-instance 'concept :name "square" :general "shape")
(make-instance 'concept :name "oval" :general "shape")
(make-instance 'concept :name "rectangle" :general "shape")
(make-instance 'concept :name "diamond" :general "shape")
(make-instance 'concept :name "lozenge" :general "shape")
(make-instance 'concept :name "star" :general "shape")
(make-instance 'concept :name "spiral" :general "shape")

(make-instance 'concept :name "colour" :general "formal quality")

(make-instance 'concept :name "hue" :general "colour")
(make-instance 'concept :name "red" :general "hue")
(make-instance 'concept :name "orange" :general "hue")
(make-instance 'concept :name "yellow" :general "hue")
(make-instance 'concept :name "green" :general "hue")
(make-instance 'concept :name "blue" :general "hue")
(make-instance 'concept :name "purple" :general "hue")

(make-instance 'concept :name "chroma" :general "colour")
(make-instance 'concept :name "bright" :general "chroma")
(make-instance 'concept :name "dark" :general "colour")
(make-instance 'concept :name "pale" :general "colour")
(make-instance 'concept :name "rich" :general "colour")
(make-instance 'concept :name "medium chroma" :general "colour")

(make-instance 'concept :name "size" :general "formal quality")
(make-instance 'concept :name "very small" :general "size")
(make-instance 'concept :name "small" :general "size")
(make-instance 'concept :name "medium sized" :general "size")
(make-instance 'concept :name "large" :general "size")
(make-instance 'concept :name "very large" :general "size")

(make-instance 'concept :name "texture" :general "formal quality")
(make-instance 'concept :name "rough" :general "texture")
(make-instance 'concept :name "smooth" :general "texture")
(make-instance 'concept :name "scribbled" :general "texture")
(make-instance 'concept :name "scumbled" :general "texture")
(make-instance 'concept :name "grainy" :general "texture")

(make-instance 'concept :name "extra-aesthetic domain" :general "concept")
(make-instance 'concept :name "environmental" :general "extra-aesthetic domain")
(make-instance 'concept :name "historical" :general "extra-aesthetic domain")
(make-instance 'concept :name "political" :general "extra-aesthetic domain")
(make-instance 'concept :name "cultural" :general "extra-aesthetic domain")
(make-instance 'concept :name "spiritual" :general "extra-aesthetic domain")
(make-instance 'concept :name "emotional" :general "extra-aesthetic domain")
(make-instance 'concept :name "perceptual" :general "extra-aesthetic domain")
(make-instance 'concept :name "cognitive" :general "extra-aesthetic domain")

(link-concepts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mutate-property-weight (property weight-drift)
  "Mutate the property's weight by +/-<=weightAmount"
  ;; random -weightAmount -> +weightAmount
  (let ((drift (- (* 2.0 (random 1.0) weight-drift) weight-drift)))
    (setf (weight-value property)
    (+ (weight-value property)
       drift))))

(defmethod mutate-property-weights (properties probability weight-amount)
  "For each property, mutate its weight by +/-<=weightAmount if random< probability"
  (dolist (prop properties)
    (when (< (random 1.0
         the-random-state)
       probability)
      (mutate-property-weight prop weight-amount))))

(defmethod mutate-property-concept (property generalise)
  "Mutate the property's concept.
   TODO: allowing generalisation if generalise is true."
  generalise ;; Silence unused variable warning
  (let ((links (concept-related (formal-property-concept property))))
    (when (not (eq links
     '()))
      (setf (formal-property-concept property)
      (nth (random (length links)
       the-random-state)
     links)))))

(defmethod mutate-property-concepts (properties probability generalise)
  "For each property, mutate its concept if random< probability"
  (dolist (prop properties)
    (when (< (random 1.0) probability)
      (mutate-property-concept prop generalise))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation, Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod evaluate (ae img)
  "Match the image's figure's properties to the aesthetic's properties and get the resulting score."
  (let (;(criteria '())
  (score 0.0))
    (dolist (fig (image-figures img))
      (dolist (prop (figure-properties fig))
  (dolist (crit (aesthetic-criteria ae))
    (when (equal (formal-property-concept crit)
           (formal-property-concept prop))
      (let ((weight (* (weight-value crit)
           (weight-value prop))))
        (setf score weight)
        (format t "Evaluation: ~a - ~a -> ~a~%"
          (concept-name (aesthetic-criterion-domain crit))
          (concept-name (formal-property-concept crit))
          weight)
        ;; If criteria doesn't contain the concept, insert,
        ;;otherwise add the weight
        ;;criteria.append()
        (setf score (+ score weight)))))))
    score))

(defmethod critique ()
  "Describe the results of evaluating a work against an aesthetic."
  (let* ((ae (make-instance 'aesthetic :criteria-count (+ 3 (random 3))))
   (pic (make-instance 'image :figure-count (+ 3 (random 3)))))
    (format t "~a" ae)
    (format t "~a" pic)
    (format t "Score: ~a ~%" (evaluate ae pic))))

(defmethod group-show ()
  "Generate a series of works and evaluate them against an aesthetic."
  (let* ((ae (make-instance 'aesthetic :criteria-count (+ 4 (random 10))))
   (highest-score -10000.0)
   ;; What about ties?
   (highest-index 0))
    (format t "~a~%" ae)
    (dotimes (i (+ 2 (random 10)))
      (format t "Image ~a ~%" (+ i 1))
      (let ((pic (make-instance 'image
         :figure-count (+ 1 (random 14))
         :property-count-min 1
         :property-count-max 9)))
  (format t "~a" pic)
  (let ((score (evaluate ae pic)))
    (format t "Score: ~a~%~%" score)
    (when (> score highest-score)
      (setf highest-score score)
      (setf highest-index i)))))
    (format t "Highest score was Image ~a~%" (+ highest-index 1))))

(defmethod critical-convention ()
  "Generate a series of aesthetics and evaluate them against a work (multiple reviews of a work)."
  (let ((pic (make-instance 'image
          :figure-count (+ 1 (random 15))
          :property-count-min 3
          :property-count-max 12))
  (highest-score -10000.0)
  ;; What about ties?
  (highest-index 0))
    (format t "~a~%" pic)
    (dotimes (i (+ 2 (random 10)))
      (format t "Aesthetic ~a~%" (+ i 1))
      (let ((ae (make-instance 'aesthetic
        :criteria-count (+ 4 (random 10)))))
  (format t "~a" ae)
  (let ((score (evaluate ae pic)))
    (format t "Score: ~a~%~%" score)
    (when (> score highest-score)
      (setf highest-score score)
      (setf highest-index i)))))
    (format t "Highest score was Aesthetic ~a~%" (+ highest-index 1))))

;; Generate a series of mutated works and evaluate them against an aesthetic (retrospective)

;; Etc.

;; Generate an aesthetic for an arists, generate a work or series of works for the aesthetic, evaluate them against
;; an aesthetic for a critic (aesthetics for critics), describe the results, allow the artists to point out what the
;; critic missed
