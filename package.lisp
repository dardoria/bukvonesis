;;;; package.lisp
(in-package :cl-user)

;;;; bukvonesis config
(defpackage :bukvonesis-config (:export :*base-directory* :*temp-directory*))

(defparameter bukvonesis-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults #.(or *compile-file-truename* *load-truename*)))

(defparameter bukvonesis-config:*temp-directory*
  (merge-pathnames bukvonesis-config:*base-directory* "tmp"))

;;;; bukvonesis
(defpackage :bukvonesis
  (:use :cl :lparallel :lparallel.queue :ponon :hunchentoot :cl-who :parenscript)
  (:shadowing-import-from :parenscript :chain))

