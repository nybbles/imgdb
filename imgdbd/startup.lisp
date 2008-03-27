(require :asdf)

; Load swank
(asdf:oos 'asdf:load-op :swank)
(push '(merge-pathnames "systems/" *default-pathname-defaults*)
      asdf:*central-registry*)

; Load configuration file
(load "imgdbd-cfg.lisp")

; Configure imgdbd
(defvar *img-db* nil)
(defvar *img-db-type* nil)
(defvar *img-store* nil)
(defvar *img-drop* nil)

(defconstant *default-imgstore-db*
  (list (merge-pathnames
         "imgdb.db"
         (if *img-store* *img-store* *default-pathname-defaults*))))
(defconstant *default-imgstore-db-type* :sqlite3)

(unless *img-drop* (error "No image drop directory specified."))
(unless *img-store* (error "No image store directory specified."))
(unless (and *img-db* *img-db-type)
  (setf *img-db* *default-img-db*)
  (setf *img-db-type* *default-img-db-type*))

; Establish connection to database
(connect-to-dbserver *img-db* *img-db-type*)

; Index images in imgdrop
(index-img-drop *img-drop* *img-store* *imgdb-db*)
