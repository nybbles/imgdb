(in-package :imgdb-web)

(defun start-imgdb-web-server
    (&key dbconn-spec dbconn-type
     (imgdb-web-root nil imgdb-web-root-provided-p) (port 4242))
  (if imgdb-web-root-provided-p
      (setf *imgdb-web-root* imgdb-web-root)
      (error "imgdb-web root not provided."))
  (setf *imgdb-dbconn* (create-imgdb-dbconn dbconn-spec dbconn-type))
  (setf *imgdb-web-server* (start-server :port port)))
