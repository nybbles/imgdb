(require :asdf)

; Load swank
(asdf:oos 'asdf:load-op :swank)
(push '(merge-pathnames "systems/" *default-pathname-defaults*)
      asdf:*central-registry*)

; Load configuration file
(load (merge-pathnames "imgdbd-cfg.lisp" *startup-script-dir*))

; Configure imgdbd
(defvar *img-db* nil)
(defvar *img-db-type* nil)
(defvar *img-store* nil)
(defvar *img-drop* nil)
(defvar *imgdb-web-root* nil)
(defvar *img-resize-cache-store* nil)

(unless *img-drop* (error "No image drop directory specified."))
(unless *img-store* (error "No image store directory specified."))
(unless (and *img-db* *img-db-type*)
  (error "No database information specified."))

; Add libpq binary path if using postgresql
(when (eq *img-db-type* :postgresql)
  (push *postgresql-so-load-path* clsql:*foreign-library-search-paths*))

; Establish connection to database and create tables, if necessary
(asdf:oos 'asdf:load-op :imgdb-store)
(imgdb-store:create-all-tables *img-db* *img-db-type*)

; Set up database connection information for the resize cache
(setf imgdb-store:*img-resize-cache-conn-spec* *img-db*)
(setf imgdb-store:*img-resize-cache-conn-type* *img-db-type*)

; Start up imgdb-web server
(asdf:oos 'asdf:load-op :imgdb-web)
(defparameter *imgdb-web-server*
  (imgdb-web:start-imgdb-web-server
   :db-type *img-db-type* :db-conn-spec *img-db*
   :imgdb-web-root *imgdb-web-root*))

; Index images in imgdrop
(format t "Indexing image drop...~%")
(format t "Indexed ~D images.~%"
        (imgdb-store:index-img-drop *img-drop* *img-store*
                                    *img-db* *img-db-type*))
