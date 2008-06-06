(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *img-resize-cache-table* '[imgresizecache])
(defparameter *img-resize-cache-holds-table* '[imgresizecacheholds])

(defparameter *img-resize-cache-max-size* (* 2 (expt 1024 3))) ; 2GB
(defvar *img-resize-cache-store* nil)

(defvar *img-resize-cache-conn-spec* '())
(defvar *img-resize-cache-conn-type* '())

(defun select-from-table (table-name select-columns &rest args)
  (when (position :from args)
    (error "Invalid keyword argument"))
  (unless (position :database args)
    (error "Missing database connection argument"))
  (apply #'select `(,@select-columns :from ,table-name ,@args)))

(defun get-thread-id (&optional (thread *current-process*))
  (register-groups-bind (tid)
      ("{([0-9A-F]+)}>$" (write-to-string thread))
    (concatenate 'string "localhost:" tid)))

(restore-sql-reader-syntax-state)
