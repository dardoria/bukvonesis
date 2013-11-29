(in-package :bukvonesis)

(defvar *server-port* 12345)

(defclass bukvonesis-resource (ws-resource)
  ())

(defmethod resource-client-connected ((res bukvonesis-resource) client)
  (format t "got connection on bukvonesis server from ~s : ~s~%" (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource bukvonesis-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))

(defmethod resource-received-text ((res bukvonesis-resource) client message)
  (let* ((json-message (yason:parse message)) ;;todo sanitize?
	 (letter-code (gethash "letter-code" json-message))
	 (font-path (multiple-value-bind (name present-p)
			(gethash "font-name" json-message)
		      (make-font-path name present-p))))

    (when (and letter-code (cl-fad:file-exists-p font-path))
      (let ((app (make-font-app  (code-char letter-code) font-path)))
	(setf (on-finish app) (lambda (result)
				(send-result client result)))
;	(setf (on-progress app) (lambda (result)
;				  (send-result client result)))
	(font-app-start app)))))

(defun send-result (client result)
  (write-to-client-text client (with-output-to-string (response)
				 (yason:encode result response))))

(defmethod resource-received-binary((res bukvonesis-resource) client message)
  (format t "got binary frame ~s from client ~s" (length message) client)
  (write-to-client-binary client message))

(defun start-clws ()
  (bordeaux-threads:make-thread (lambda ()
				  (run-server *server-port*))
				:name "bukvonesis server")

  (register-global-resource "/bukvonesis"
			    (make-instance 'bukvonesis-resource)
			    (origin-prefix "http://127.0.0.1" "http://localhost"))

  (bordeaux-threads:make-thread (lambda ()
				  (run-resource-listener
				   (find-global-resource "/bukvonesis")))
				:name "resource listener for /bukvonesis"))

(defun make-font-path (font-path present-p)
  (when present-p
    (let ((font-name-path (pathname font-path)))
      (cl-fad:merge-pathnames-as-file bukvonesis-config:*temp-directory*
				      (make-pathname :name (pathname-name font-name-path) :type (pathname-type font-name-path))))))
