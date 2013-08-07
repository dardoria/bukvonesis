;;;; bukvonesis.lisp

(in-package :bukvonesis)

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
  (let ((app (make-app 'font-app  :title "bukvonesis" :pos-x 100 :pos-y 100 :width 700 :height 700)))
    (setf (font-loader app) (zpb-ttf:open-font-loader #P"/Library/Fonts/Arial.ttf"))
    (setf (bukva app) "Б")    
  (run app)))

