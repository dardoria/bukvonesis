(in-package :bukvonesis)

(defparameter *httpd* nil)
 
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html 
       (:head 
	(:meta :http-equiv "Content-Type" 
	       :content    "text/html;charset=utf-8")
	(:title ,title)
	(:script :type "text/javascript" :src "/bukvonesis.js"))
       (:body 
	,@body))))
	
(defun index.html ()
  (standard-page
    (:title "bukvonesis")
    (:h1 "Select Font:")
    (:input :type "file" :id "font-selector" :onchange (ps-inline (upload-file
							    (@ (chain document (get-element-by-id "font-selector")) files 0))))
    (:canvas :id "bukvonesis" :width "1000" :height "1000")))

(defun bukvonesis.js ()
  (setf (content-type*) "text/javascript")
  (ps (defun upload-file (file)
	(let ((xhr (new (*x-m-l-http-request)))
	      (form-data (new (*form-data))))
	  
	  (chain form-data (append "fontfile" file))
	  (chain console (log file))
	  (chain xhr (open "POST" "font-upload"))
	  (chain xhr (send form-data))))))
  
(defun font-upload ()
  (break "~A" (post-parameter "fontfile"))
  )

(defun start-server ()
  (setf *show-lisp-errors-p* t
	*show-lisp-backtraces-p* t
	*break-on-signals* nil)
  
  (setf *dispatch-table* 
	(list 'dispatch-easy-handlers 
	      (create-prefix-dispatcher "/bukvonesis.js" 'bukvonesis.js)
	      (create-prefix-dispatcher "/font-upload" 'font-upload)
	      (create-prefix-dispatcher "/" 'index.html)))

  (when *httpd*
    (stop *httpd*)
    (setf *httpd* nil))

  (setf *httpd* (make-instance 'hunchentoot:easy-acceptor :port 9081))
  (start *httpd*))

