;;;; bukvonesis.asd

(asdf:defsystem #:bukvonesis
  :serial t
  :description "Growing cyrillic characters for latin-only fonts."
  :author "Boian Tzonev <boiantz@gmail.com>"
  :license "Apache License, Version 2.0"
  :depends-on ("hunchentoot" "cl-who" "parenscript" "ponon" "lparallel" "closure-template") ;;todo remove ponon
  :components ((:file "package")
               (:file "bukvonesis")
	       (:file "server")))
