(defparameter *img-db-type* :postgresql)
(defparameter *img-db*
  '("127.0.0.1"
    "imgdb-test"
    "imgdb" "bobobo"))

(defparameter *img-store*
  (merge-pathnames "Desktop/imgdb-test/store/" *default-pathname-defaults*))
(defparameter *img-drop*
  (merge-pathnames "Desktop/imgdb-test/drop/" *default-pathname-defaults*))

(defparameter *postgresql-so-load-path*
  "/opt/local/lib/postgresql83/")

(defparameter *imgdb-web-root*
  (merge-pathnames "code/imgdb/imgdb-web/" *default-pathname-defaults*))

(defparameter *img-resize-cache-store* 
  (merge-pathnames "Desktop/imgdb-test/cache/" *default-pathname-defaults*))

(defparameter *startup-script-dir*
  (merge-pathnames "code/imgdb/imgdbd/" *default-pathname-defaults*))
