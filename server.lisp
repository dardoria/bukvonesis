(in-package :bukvonesis)

(defparameter *httpd* nil)
 
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html 
       (:head 
	(:meta :http-equiv "Content-Type" 
	       :content    "text/html;charset=utf-8")
	(:title ,title)
	(:script :type "text/javascript" 
		 :href "/utils.js")
	)
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
  (html (:princ
	 (ps (defun upload-file (file)
	       (chain console (log file))
	       (let ((reader (new (*file-reader)))
		     (xhr (new (*x-m-l-http-request)))
		     (self this))
		 (setf (@ this xhr) xhr)
		 
		 (chain this xhr (open "POST" "font-upload"))
		 (chain this xhr (override-mime-type "text/plain; charset=x-user-defined-binary"))
		 (setf (@ reader onload) (lambda (evt)
					   (chain xhr (send-as-binary (@ evt target result)))))
		 (chain reader (read-as-binary-string file))))))))

(defun font-upload ()
  )

(defun start-server ()
  (setf *show-lisp-errors-p* t
	*show-lisp-backtraces-p* t
	*break-on-signals* nil)
  
  (setf *dispatch-table* (list (create-static-file-dispatcher-and-handler "/utils.js" 
									  #p"/~/proekti/bukvonesis/utils.js" ;;todo this doesn't work
									  "text/javascript")
			       (create-prefix-dispatcher "/tags" 'tips-for-tag)
			       (create-prefix-dispatcher "/bukvonesis.js" 'bukvonesis.js)
			       (create-prefix-dispatcher "/font-upload" 'font-upload)
			       (create-prefix-dispatcher "/" 'index.html)))

  (unless *httpd*
    (setf *httpd* (make-instance 'hunchentoot:easy-acceptor :port 9081))
    (start *httpd*)))

