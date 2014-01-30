;;;; bukvonesis.lisp

(in-package :bukvonesis)

(defparameter *max-value* 90000000)
(defparameter *worker-count* 20)

(defclass font-app ()
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva :writer (setf bukva))
   (glyph :accessor glyph)
   (result :accessor result :initform '())
   (progress-queue :accessor progress-queue)
   (on-progress :accessor on-progress :initform nil)
   (on-finish :accessor on-finish :initform nil)))

(defun (setf bukva) (bukva app)
  (setf (slot-value app 'bukva) bukva)
  (setf (slot-value app 'glyph)
	(zpb-ttf:find-glyph (bukva app) (font-loader app))))

(defun (setf result) (result app)
  (format t "~a" result)
  (setf (slot-value app 'result) result)
  (when (functionp (on-finish app))
    (funcall (on-finish app) (result app)))

  (zpb-ttf:close-font-loader (font-loader app))
  (kill-tasks :default))

(Defun make-font-app (letter font-path)
  (let ((app (make-instance 'font-app)))
    (setf (font-loader app) (zpb-ttf:open-font-loader font-path))
    (setf (bukva app) letter)
    (setf (progress-queue app) (make-queue))
    app))

(defun font-app-start (app)
  (main-loop app *worker-count*))

(defun main-loop (app worker-count)
  (unless *kernel*
    (setf *kernel* (make-kernel worker-count :name "bukvonesis-main")))
  (kill-tasks :default)

  (let ((channel (make-channel))
	(control-queue (make-queue))
	(segments-count 0))

    (zpb-ttf:do-contours (contour (glyph app))
      (zpb-ttf:do-contour-segments (start ctrl end) contour
	(incf segments-count)
	(submit-task channel 'evolve-curve start ctrl end (progress-queue app) control-queue)))

    (future (report-progress app segments-count))
    (future (collect-results app control-queue channel segments-count))))

(defun evolve-curve (start control end message-queue control-queue)
  (let ((population '())
	(scores '())
	(population-size 350)
	(max-generations 400)
	(mutation-probability 0.1)
	(max-coordinate-value (get-max-coords-value start end))
	(min-coordinate-value (get-min-coords-value start end))
	(straight-line-probability (if control 0.0 1.0))
	(best-candidate)
	(best-score))

    ;; initialization
    (setf population (initialize-population population-size min-coordinate-value max-coordinate-value straight-line-probability))

    (loop repeat max-generations
       ;; evaluation
       do (setf scores
 		(loop for chromosome in population
 		   collect (evaluate chromosome start control end)))

       do (multiple-value-bind (index score)
 	      (get-best-chromosome-index scores)

	    (setf best-candidate (nth index  population))
	    (setf best-score score)

	    (when (= score 0) ;;Yay we found the perfect match ;;TODO relax this
 	      (return))

	    (push-queue best-candidate message-queue))

       ;; selection
       do (let ((total-score (float (reduce '+ scores))))
	    (setf population
		  (cons best-candidate
			(loop repeat population-size
			   collect (let ((parent1 (select population scores total-score))
					 (parent2 (select population scores total-score)))
				     ;; crossover and mutation
				     (mutate (crossover parent1 parent2) mutation-probability)))))))
    (push-queue 1 control-queue)
    best-candidate))

(defun report-progress (app segments-count)
  (let ((finished-tasks-count 0))
    (loop
       with result = nil
       do (setf result (try-pop-queue (progress-queue app)))
       do (incf finished-tasks-count)
       while result
       collect result into result-list
       do (when (and (= (mod finished-tasks-count segments-count) 0)
		     (functionp (on-progress app)))
	    (funcall (on-progress app) result-list))
       do (setf result-list nil))))

(defun collect-results (app control-queue channel segments-count)
  (let ((finished-tasks-count 0))
    (loop
       do (pop-queue control-queue)
       do (incf finished-tasks-count)
       while (< finished-tasks-count segments-count))
    (setf (result app)
	  (loop repeat finished-tasks-count
	     collect (receive-result channel)))))

;;;; genetic operations
(defun initialize-population (population-size min-coordinate-value max-coordinate-value straight-line-probability)
  (loop repeat population-size
     collect (make-chromosome min-coordinate-value max-coordinate-value straight-line-probability)))

(defun evaluate (candidate start control end)
  ;;TODO do not hard code max-value
  (- *max-value* (coords-distance candidate start control end)))

(defun select (population scores total-score)
  "Population is a list of chromosomes. Scores is a list of relative scores for each chromosome."
  (loop for score in scores
     for chromosome in population
     with current = 0
     with pick = (random total-score)
     do (incf current score)
       (when (> current pick)
	 (return chromosome))))

(defun crossover (parent1 parent2)
  "Parents are lists of coordinates."
  (let ((cross-point (random 6))) ;; 3 points * 2 coordinates
    (append (subseq parent1 0 cross-point)
	    (subseq parent2 cross-point))))


(defun mutate (chromosome mutation-probability)
;;TODO fix mutation
;  "Chromosome is integer, mutation-probability is < 1.0"
;  (let ((chromosome-place (list chromosome)))
;    (loop for i below (integer-length chromosome)
;       do (when (> mutation-probability (random 1.0))
;	    (setf (ldb (byte 1 i) (car chromosome-place))
;		  (lognot (ldb (byte 1 i) chromosome)))))
;    (car chromosome-place))
  chromosome
)

;;;; helpers
(defun make-chromosome (min-value max-value straight-prob)
  (loop for k below (* 3 2)
     collect (cond ((and (or (= k 2) (= k 3))
			 (<= (random 1.0) straight-prob))
		    0)
		   (T
		    (random-range min-value max-value)))))

(defun coords-distance (candidate start ctrl end)
  (let ((score 0))
    (unless ctrl
      (setf ctrl (zpb-ttf::make-control-point 0 0 nil)))

    (incf score (distance-squared (zpb-ttf:x start) (zpb-ttf:y start)
				  (first candidate) (second candidate)))
    (incf score (distance-squared (zpb-ttf:x ctrl) (zpb-ttf:y ctrl)
				  (third candidate) (fourth candidate)))
    (incf score (distance-squared (zpb-ttf:x end) (zpb-ttf:y end)
				  (fifth candidate) (sixth candidate)))

    score))

(defun distance-squared (p1-x p1-y p2-x p2-y)
  (+ (expt (- p1-x p2-x) 2)
     (expt (- p1-y p2-y) 2)))

(defun get-best-chromosome-index (scores)
  (loop for score in scores
     for i from 0
     with current = 0
     with index = 0
     do (when (> score current)
	  (setf current score
		index i))
     finally (if (< current 0)
		 (error "Negative score!!! ~a ~a" index current)
		 (return (values index current)))))

(defun get-max-coords-value (start end)
  (float (max (zpb-ttf:x start) (zpb-ttf:y start)
	      (zpb-ttf:x end) (zpb-ttf:y end))))

(defun get-min-coords-value (start end)
  (float (min (zpb-ttf:x start) (zpb-ttf:y start)
	      (zpb-ttf:x end) (zpb-ttf:y end))))

(defun random-range (x y)
  (when (> x y)
    (rotatef x y))
  (let ((range (- y x)))
    (when (rationalp range)
      (setf range (coerce range 'float)))
    (+ x (random range ))))
