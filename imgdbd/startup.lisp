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

(defconstant *default-img-db*
  (list (namestring
         (merge-pathnames
          "imgdb.db"
          (if *img-store* *img-store* *default-pathname-defaults*)))))
(defconstant *default-img-db-type* :sqlite3)

(unless *img-drop* (error "No image drop directory specified."))
(unless *img-store* (error "No image store directory specified."))
(unless (and *img-db* *img-db-type*)
  (setf *img-db* *default-img-db*)
  (setf *img-db-type* *default-img-db-type*))

(asdf:oos 'asdf:load-op :imgdb-store)
(use-package :imgdb-store)

; Establish connection to database and create tables, if necessary
(defparameter *img-db-conn* (connect-to-dbserver *img-db* *img-db-type*))
(unless (img-table-exists *img-db-conn*)
  (create-img-table *img-db-conn*))

; Index images in imgdrop
(format t "Indexing image drop...~%")
(format t "Indexed ~D images.~%"
        (index-img-drop *img-drop* *img-store* *img-db-conn*))
