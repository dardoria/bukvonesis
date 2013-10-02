;;;; bukvonesis.lisp

(in-package :bukvonesis)

(defparameter *allele-size* 8)
(defparameter *coords-range* 3000)
(defparameter *max-value* 100000)
(defparameter *worker-count* 10)

(defclass font-app (base-app)
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva :writer (setf bukva))
   (glyph :accessor glyph)
   (candidate :accessor candidate :initform '())))

(defmethod setup ((app font-app))
  (set-background 0.25 0.63 0.54))

(defmethod draw ((app font-app))
  (clear)
  (set-color 0.0 0.0 0.0)
  (draw-string (font-loader app) (bukva app) 2000 -3500 :filled nil :size 250)

  (when (candidate app)    
    (loop for curve in (candidate app)
       do (if (and (= (row-major-aref curve 3) 0)
		   (= (row-major-aref curve 4) 0))
	      (draw-line (row-major-aref curve 0) (row-major-aref curve 1)
			 (row-major-aref curve 6) (row-major-aref curve 7))
	      (draw-curve curve)))))

(defmethod exit ((app font-app))
  (zpb-ttf:close-font-loader (font-loader app))
  (kill-tasks :default))

(defmethod key-pressed ((app font-app) key)
  (case key
    (#\r
     (main-loop app *worker-count*))))

(defun (setf bukva) (bukva app)
  (setf (slot-value app 'bukva) bukva)
  (setf (slot-value app 'glyph) 
	(zpb-ttf:find-glyph (aref (bukva app) 0) (font-loader app))))

(defun start ()
  ;;setup drawing window
  (let ((app (make-app 'font-app  :title "bukvonesis" :pos-x 100 :pos-y 100 :width 700 :height 700)))
    #+darwin
    (setf (font-loader app) (zpb-ttf:open-font-loader #P"/Library/Fonts/Arial.ttf"))
    #+unix
    (setf (font-loader app) (zpb-ttf:open-font-loader #P"/usr/share/fonts/truetype/ttf-droid/DroidSansMono.ttf"))
    (setf (bukva app) "B")
;    (setf (bukva app) "Б")
    
    (main-loop app *worker-count*)
    (run app)))

(defun main-loop (app worker-count)
  (unless *kernel*
    (setf *kernel* (make-kernel worker-count :name "bukvonesis-main")))
  (kill-tasks :default)

  (let ((queue (make-queue)))
    (zpb-ttf:do-contours (contour (glyph app))
      (zpb-ttf:do-contour-segments (start ctrl end) contour
	(future (evolve-curve start ctrl end queue))))
    (format t "~a" (try-pop-queue queue))))

(defun evolve-curve (start control end queue)
  (let ((population '())
	(scores '())
	(population-size 200)
	(chromosome-length 1)
	(max-generations 2)
	(mutation-probability 0.1))

    ;; initialization
    (setf population (initialize-population population-size chromosome-length))
    
    (loop repeat max-generations
       ;; evaluation
       do (setf scores
 		(loop for chromosome in population
 		   collect (evaluate (chromosome->coordinates chromosome) start control end)))
	 
       do (multiple-value-bind (index score)
 	      (get-best-chromosome-index scores)
 	    (when (= score 0) ;;Yay we found the perfect match ;;TODO relax this
 	      (return))
	    
	    (push-queue 
	     (loop for coords across (chromosome->coordinates (nth index  population))
		collect (coords->curve coords))
	     queue))
	 
       ;; selection
       do (let ((total-score (float (reduce '+ scores))))
	    (setf population
		  (loop repeat population-size
		     collect (let ((parent1 (select population scores total-score))
				   (parent2 (select population scores total-score)))
			       ;; crossover and mutation
			       (mutate (crossover parent1 parent2) mutation-probability))))))))

;;;; genetic operations
(defun initialize-population (population-size chromosome-length)
  (loop repeat population-size
     collect (make-chromosome chromosome-length)))

(defun evaluate (candidate start control end)
  "Candidate is array of lists, start, control and end are zpb-ttf:control-point."
  ;;TODO do not hard code max-value
  (- *max-value* (coords-distance start control end (aref candidate 0))))

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
  "Parents are integers."
  ;;TODO crossover at coordinates only
  (let* ((min-length (min (integer-length parent1) (integer-length parent2)))
	 (max-length (max (integer-length parent1) (integer-length parent2)))
	 (cross-point (* (random (round (/ min-length *allele-size*))) *allele-size*))
	 (p2-chunk-size (- max-length cross-point)))
    ;(format t "~a~%" cross-point)
    (dpb (ldb (byte p2-chunk-size cross-point) parent2)
	 (byte p2-chunk-size cross-point)
	 (dpb (ldb (byte cross-point 0) parent1) (byte cross-point 0) 0))))

(defun mutate (chromosome mutation-probability)
  "Chromosome is integer, mutation-probability is < 1.0"
  (let ((chromosome-place (list chromosome)))
    (loop for i below (integer-length chromosome)
       do (when (> mutation-probability (random 1.0))
	    (setf (ldb (byte 1 i) (car chromosome-place))
		  (lognot (ldb (byte 1 i) chromosome)))))
    (car chromosome-place)))

;;;; helpers
(defun make-chromosome (chromosome-length &optional (range *coords-range*) (straight-prob 1.0))
  "Chromosome length is the number of contours for a character. Each contour consists of three pairs of control point coordinates. Every contour is a straight line with probability straight-prob. A straight line is denoted by a zero second control point."
  ;;todo no magick numbers
  (let ((chromosome 0))
    (loop for i below (* chromosome-length 3 2) by (* 3 2)
       do (loop for k below (* 3 2)
	     do (let ((coord 
		       (cond ((and (or (= k 2) (= k 3))
				   (<= (random 1.0) straight-prob))
			      0)
			     (T
			      (random range)))))
		  (setf chromosome (dpb coord (byte *allele-size* (* *allele-size* (+ k i))) chromosome)))))
    chromosome))

(defun chromosome->coordinates (chromosome)
  "Chromosome is a number representing a list of contours. Returns an array of contours. Each contour is a list of 3 x,y coordinates."
  (let* ((contour-count (ceiling (integer-length chromosome) (* *allele-size* 3 2)))
	 (contours (make-array contour-count)))
    (loop for i below contour-count
       for j below (* contour-count 3 2) by (* 3 2)
       do (setf (aref contours i)
		(loop for k below (* 3 2)
		   collect (ldb (byte *allele-size* (* *allele-size* (+ j k))) chromosome))))
    contours))

(defun coords->curve (coords)
  "Coords is a list consiting of 3 control points, represented by x,y pairs. Returns an array of coordinates. Each coordinate is an array of x,y,z coordinates. Z is always zero."
  (make-array '(3 3) :initial-contents
	      (loop for i below (- (length coords) 1) by 2
		 collect (list (elt coords i) (elt coords (+ 1 i)) 0))))

(defun coords-distance (start ctrl end candidate)
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
     finally (return (values index current))))
