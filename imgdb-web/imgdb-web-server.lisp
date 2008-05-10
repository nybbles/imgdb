(in-package :imgdb-web)

(defun start-imgdb-web-server
    (&key db-conn-spec db-type
     (imgdb-web-root nil imgdb-web-root-provided-p) (port 4242))
  (if imgdb-web-root-provided-p
      (setf *imgdb-web-root* imgdb-web-root)
      (error "imgdb-web root not provided."))
  (setf *imgdb-store-db-conn-spec* db-conn-spec)
  (setf *imgdb-store-db-type* db-type)
  (setf *imgdb-web-server* (start-server :port port)))
