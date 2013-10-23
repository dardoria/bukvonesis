(in-package :bukvonesis)

(defvar *server-port* 12345)

(defclass bukvonesis-resource (ws-resource)
  ())

(defmethod resource-client-connected ((res bukvonesis-resource) client)
  (format t "got connection on echo server from ~s : ~s~%" (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource bukvonesis-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))

(defmethod resource-received-text ((res bukvonesis-resource) client message)
  (write-to-client-text client message))

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
