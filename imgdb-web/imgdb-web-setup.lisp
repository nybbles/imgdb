(in-package :imgdb-web)

(defparameter *imgdb-web-root* nil)

(defparameter *imgdb-store-db-conn-spec* nil)
(defparameter *imgdb-store-db-type* nil)
(defparameter *imgdb-web-server* nil)

(setf *dispatch-table* '())
