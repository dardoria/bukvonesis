;;;; bukvonesis.lisp

(in-package :bukvonesis)

(defparameter *population* '())

(defclass font-app (base-app)
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva)))

(defmethod setup ((app font-app))
  (set-background 0.25 0.63 0.54))

(defmethod draw ((app font-app))
  (clear)
  (set-color 0.0 0.0 0.0)
  (draw-string (font-loader app) (bukva app) 2000 -3500 :filled nil :size 250))

(defmethod exit ((app font-app))
  (zpb-ttf:close-font-loader (font-loader app)))

(defun start ()
  ;;setup drawing window
  (let ((app (make-app 'font-app  :title "bukvonesis" :pos-x 100 :pos-y 100 :width 700 :height 700))
	(population-size 50)
	(chromosome-length 10))
    (setf (font-loader app) (zpb-ttf:open-font-loader #P"/Library/Fonts/Arial.ttf"))
    (setf (bukva app) "Б")    
  (run app)
  ;; initialization
  (setf *population* (initialize-population population-size chromosome-length))
  ;; evaluation
  ;; selection
  ;; crossover
  ;; mutation
))

(defun initialize-population (population-size chromosome-length)
  (loop repeat population-size
     collect (make-chromosome chromosome-length)))

(defun make-chromosome (chromosome-length)
  "Chromosome length is the number of contours for a character, mutiply by three for each control point"
  ;;todo no magick numbers
  (let ((chromosome 0))
    (loop for i below (* chromosome-length 3) by 3
       do (setf chromosome (dpb (random 10000) (byte 10 (* 10 i)) chromosome)) ;;todo use bounds
	 (setf chromosome (dpb (random 10000) (byte 10 (* 10 (+ i 1))) chromosome)) ;;todo add a zero with some probability
	 (setf chromosome (dpb (random 10000) (byte 10 (* 10 (+ i 2))) chromosome)))
    chromosome))
