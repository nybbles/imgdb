(in-package :imgdb-web)

(defun start-imgdb-web-server (&key dbconn-spec dbconn-type (port 4242))
  (setf *imgdb-dbconn* (create-imgdb-dbconn dbconn-spec dbconn-type))
  (setf *imgdb-web-server* (start-server :port port)))
