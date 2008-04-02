(in-package :imgdb-web)

(defparameter *imgdb-web-root* nil)

(defparameter *imgdb-dbconn* nil)
(defparameter *imgdb-web-server* nil)

(setf *dispatch-table* '())
