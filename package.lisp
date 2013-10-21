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
  (:use :cl :lparallel :lparallel.queue :ponon :hunchentoot :parenscript :clws)
  (:shadowing-import-from :parenscript :chain))

(in-package :bukvonesis)

(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(defmacro console.log (thing)
  `(chain console (log ,thing)))

