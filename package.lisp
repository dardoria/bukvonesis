;;;; package.lisp
(in-package :cl-user)

;;;; bukvonesis config
(defpackage :bukvonesis-config (:export :*base-directory* :*temp-directory*))

(defparameter bukvonesis-config:*base-directory*
  (make-pathname :name nil :type nil :defaults #.(or *compile-file-truename* *load-truename*)))

(defparameter bukvonesis-config:*temp-directory*
  (merge-pathnames (make-pathname :directory '(:relative "tmp")) bukvonesis-config:*base-directory*))

;;;; bukvonesis
(defpackage :bukvonesis
  (:use :cl :lparallel :lparallel.queue :hunchentoot :parenscript :clws)
  (:shadowing-import-from :parenscript :chain))


;;todo this doesn't belong here
(in-package :bukvonesis)
(defpsmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(defpsmacro console.log (thing)
  `(chain console (log ,thing)))

(defpsmacro make-paper-object (thing &body body)
  `(new (chain paper (,thing ,@body))))

(defpsmacro proc-setf (name value)
  `(setf (@ processing-instance ,name) ,value))

(defpsmacro p5 (&body body)
  `(chain processing ,@body))
