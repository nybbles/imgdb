(in-package :imgdb-web)

(defun start-imgdb-web-server
    (&key db-conn-spec db-type
     (imgdb-web-root nil imgdb-web-root-provided-p) (port 4242))
  (unless (null *imgdb-web-server*)
    (error "imgdb-web-server already running"))
  (if imgdb-web-root-provided-p
      (setf *imgdb-web-root* imgdb-web-root)
      (error "imgdb-web root not provided."))
  (setf *imgdb-store-db-conn-spec* db-conn-spec)
  (setf *imgdb-store-db-type* db-type)
  (setup-dispatch-table)
  (setf *imgdb-web-server* (start-server :port port)))

(defun stop-imgdb-web-server ()
  (when (null *imgdb-web-server*)
    (error "No imgdb-web server running"))
  (stop-server *imgdb-web-server*)
  (setf *imgdb-web-server* nil))
