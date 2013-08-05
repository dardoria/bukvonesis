;;;; bukvonesis.lisp

(in-package #:bukvonesis)

(defclass font-app (base-app)
  ((font-loader :accessor font-loader)
   (bukva :accessor bukva)))

(defmethod setup ((app font-app))
  (set-background 1 1 1))

(defmethod draw ((app font-app))
  (clear)
  (set-color 0.23 0.67 1)

  (draw-string (font-loader app) (bukva app) 30 30 :filled nil :size 100))

(defun start ()
  (let ((window (make-app 'font-app  :title "drawing" :pos-x 100 :pos-y 100 :width 700 :height 700)))
    (setf (font-loader window) (zpb-ttf:open-font-loader #P"/Library/Fonts/Arial.ttf"))
    (setf (bukva window) "Б")    
    (run window)))

