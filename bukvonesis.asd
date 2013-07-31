;;;; bukvonesis.asd

(asdf:defsystem #:bukvonesis
  :serial t
  :description "Describe bukvonesis here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:ponon)
  :components ((:file "package")
               (:file "bukvonesis")))

