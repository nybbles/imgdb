(in-package :imgdb-web)

(defparameter *imgdb-dbconn* nil)
(defparameter *imgdb-web-server* nil)

(defun start-imgdb-web-server (&key dbconn-spec dbconn-type (port 4242))
  (setf *imgdb-dbconn* (create-imgdb-dbconn dbconn-spec dbconn-type))
  (setf *imgdb-web-server* (start-server :port 4242)))
