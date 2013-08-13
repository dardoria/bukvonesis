;;;; bukvonesis.lisp

(in-package :bukvonesis)

(defparameter *population* '())
(defparameter *allele-size* 10)

(defclass font-app (base-app)
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva)
   (candidate :accessor candidate :initform '())))

(defmethod setup ((app font-app))
  (set-background 0.25 0.63 0.54))

(defmethod draw ((app font-app))
  (clear)
  (set-color 0.0 0.0 0.0)
  (draw-string (font-loader app) (bukva app) 2000 -3500 :filled nil :size 250)
  (when (candidate app)
    (loop for curve in (candidate app)
	 do (draw-curve curve))))

(defmethod exit ((app font-app))
  (zpb-ttf:close-font-loader (font-loader app)))

(defun start ()
  ;;setup drawing window
  (let ((app (make-app 'font-app  :title "bukvonesis" :pos-x 100 :pos-y 100 :width 700 :height 700))
	(population-size 50)
	(chromosome-length 10))
    (setf (font-loader app) (zpb-ttf:open-font-loader #P"/Library/Fonts/Arial.ttf"))
    (setf (bukva app) "Б")    

    ;; initialization
    (setf *population* (initialize-population population-size chromosome-length))
    ;; evaluation
    ;; selection
    ;; crossover
    ;; mutation
    (setf (candidate app)
	  (loop for coords across (chromosome->coordinates (first *population*))
	     collect (coords->curve coords)))
    (run app)
))

(defun initialize-population (population-size chromosome-length)
  (loop repeat population-size
     collect (make-chromosome chromosome-length)))

(defun make-chromosome (chromosome-length)
  "Chromosome length is the number of contours for a character. Each contour consists of three pairs of control point coordinates"
  ;;todo no magick numbers
  ;;todo make second point 0 for straight lines
  (let ((chromosome 0))
    (loop for i below (* chromosome-length 3 2) by (* 3 2)
       do (loop for k below (* 3 2)
	     do (setf chromosome (dpb (random 10000) (byte *allele-size* (* *allele-size* (+ k i))) chromosome))))
    chromosome))

(defun chromosome->coordinates (chromosome)
  (let* ((contour-count (/ (ceiling (/ (integer-length chromosome) *allele-size*)) (* 3 2)))
	 (contours (make-array contour-count)))
    (loop for i below contour-count
       do (setf (aref contours i)
		(loop for k below (* 3 2)
		   collect (ldb (byte *allele-size* (* *allele-size* (+ i k))) chromosome))))
    contours))

(defun coords->curve (coords)
  (make-array '(3 3) :initial-contents
	      (loop for i below (- (length coords) 1) by 2
		 collect (list (elt coords i) (elt coords (+ 1 i)) 0))))
