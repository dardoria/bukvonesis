(in-package :bukvonesis)

(defparameter *httpd* nil)

(defun index.html ()
  (bukvonesis.html:index))

(defun bukvonesis.js ()
  (with-open-file (out (merge-pathnames (make-pathname :name "bukvonesis" :type "js" :directory '(:relative "js")) bukvonesis-config:*base-directory*) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (princ (ps:ps-compile-file (merge-pathnames bukvonesis-config:*base-directory* "bukvonesis.paren")) out)))
  
(defun font-upload ()
  (destructuring-bind (path name content-type) (post-parameter "fontfile")
    (declare (ignore content-type))
    (cl-fad:copy-file path (make-pathname :name name :defaults bukvonesis-config:*temp-directory*) :overwrite t)
    (format nil "/tmp/~a" name)))

(defun start-hunchentoot ()
  (setf *show-lisp-errors-p* t
	*show-lisp-backtraces-p* t
	*break-on-signals* nil)

  ;;compile js and html templates
  (bukvonesis.js)
  (closure-template:compile-template :common-lisp-backend (merge-pathnames "bukvonesis.soy" bukvonesis-config:*base-directory*))

  ;;routes  
  (setf *dispatch-table* 
	(list 'dispatch-easy-handlers 
	      (create-folder-dispatcher-and-handler "/js/" (cl-fad:merge-pathnames-as-directory bukvonesis-config:*base-directory* "js/"))
	      (create-folder-dispatcher-and-handler "/css/" (cl-fad:merge-pathnames-as-directory bukvonesis-config:*base-directory* "css/"))
	      (create-folder-dispatcher-and-handler "/fonts/" (cl-fad:merge-pathnames-as-directory bukvonesis-config:*base-directory* "font/"))
	      (create-folder-dispatcher-and-handler "/tmp/" (cl-fad:merge-pathnames-as-directory bukvonesis-config:*base-directory* "tmp/") "font/truetype")
	      (create-prefix-dispatcher "/font-upload" 'font-upload)
	      (create-prefix-dispatcher "/" 'index.html)))

  (when *httpd*
    (stop *httpd*)
    (setf *httpd* nil))

  (setf *httpd* (make-instance 'hunchentoot:easy-acceptor :port 9081))
  (start *httpd*))
  

