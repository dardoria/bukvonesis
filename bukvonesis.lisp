;;;; bukvonesis.lisp

(in-package :bukvonesis)

(defparameter *max-value* 90000000)
(defparameter *worker-count* 20)
(defparameter *message-queue* nil)

(defclass font-app (base-app)
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva :writer (setf bukva))
   (glyph :accessor glyph)
   (candidate :accessor candidate :initform '())))

(defmethod setup ((app font-app))
  (set-background 0.0 0.0 0.0))

;(defmethod update ((app font-app))
  ;;TODO collect curves to show realtime prgress
;  (let ((candidate (try-pop-queue *message-queue*)))
;    (when candidate 
;      (setf (candidate app) (push candidate (candidate app))))))
;)

(defmethod draw ((app font-app))
  (clear)
  (set-color 0.2 0.7 0.1)
  (draw-string (font-loader app) (bukva app) 2000 -3500 :filled nil :size 250)

  (with-state
    ;;TODO no magick numbers
    (scale 125/1024 (* 125/1024 -1) 125/1024)
    (translate 2000 -3500 0)

    (when (candidate app)
      (progn
	(set-color 0.7 0.2 0.1)
	(loop for curve in (candidate app)
	   do (if (and (= (row-major-aref curve 3) 0)
		       (= (row-major-aref curve 4) 0))
		  (draw-line (row-major-aref curve 0) (row-major-aref curve 1)
			     (row-major-aref curve 6) (row-major-aref curve 7))
		  (draw-curve curve)))))))

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
    
;    (setf (bukva app) "S")
    (setf (bukva app) "Б")
    
    (setf *message-queue* (make-queue))

    (main-loop app *worker-count*)
    (run app)))

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
	(submit-task channel 'evolve-curve start ctrl end *message-queue* control-queue)))

    (future (collect-results app control-queue channel segments-count))))

(defun evolve-curve (start control end message-queue control-queue)
  (let ((population '())
	(scores '())
	(population-size 350)
	(max-generations 400)
	(mutation-probability 0.1)
	(max-coordinate-value (get-max-coords-value start end))
	(straight-line-probability (if control 0.0 1.0))
	(best-candidate)
	(best-score))

    ;; initialization
    (setf population (initialize-population population-size max-coordinate-value straight-line-probability))

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

	    (push-queue (coords->curve best-candidate) message-queue))
	 
       ;; selection
       do (let ((total-score (float (reduce '+ scores))))
	    (setf population
		  (cons best-candidate
			(loop repeat population-size
			   collect (let ((parent1 (select population scores total-score))
					 (parent2 (select population scores total-score)))
				     ;; crossover and mutation
				     (mutate (crossover parent1 parent2) mutation-probability)))))))
;    (format t "~a, ~a, ~a, ~a, ~a ~%" (- *max-value* best-score) start control end best-candidate)
    (push-queue 1 control-queue)
    best-candidate))

(defun collect-results (app control-queue channel segments-count)
  (let ((finished-tasks-count 0))
    (loop
       do (pop-queue control-queue)
       do (incf finished-tasks-count)
       while (< finished-tasks-count segments-count))
    (setf (candidate app)
	  (loop repeat finished-tasks-count
	     collect (coords->curve (receive-result channel))))))

;;;; genetic operations
(defun initialize-population (population-size max-coordinate-value straight-line-probability)
  (loop repeat population-size
     collect (make-chromosome max-coordinate-value straight-line-probability)))

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
(defun make-chromosome (range straight-prob)
  (loop for k below (* 3 2)
     collect (cond ((and (or (= k 2) (= k 3))
			 (<= (random 1.0) straight-prob))
		    0)
		   (T
		    (random range)))))

(defun coords->curve (coords)
  "Coords is a list consiting of 3 control points, represented by x,y pairs. Returns an array of coordinates. Each coordinate is an array of x,y,z coordinates. Z is always zero."
  (make-array '(3 3) :initial-contents
	      (loop for i below (- (length coords) 1) by 2
		 collect (list (elt coords i) (elt coords (+ 1 i)) 0))))

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
