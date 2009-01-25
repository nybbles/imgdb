(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *img-table* '[imgs])
(defparameter *img-resize-cache-table* '[imgresizecache])
(defparameter *img-resize-cache-holds-table* '[imgresizecacheholds])
(defparameter *img-tags-table* '[imgtags])

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

;;; Utility macros for opening pooled database connections
(defmacro with-dbconn-info ((dbconn-var dbconn-info-var) &body body)
  (let ((dbconn-spec-var (gensym "DBCONN-SPEC-"))
        (dbconn-type-var (gensym "DBCONN-TYPE-")))
  `(let ((,dbconn-spec-var (dbconn-spec ,dbconn-info-var))
         (,dbconn-type-var (dbconn-type ,dbconn-info-var)))
     (with-pooled-dbconn (,dbconn-var ,dbconn-spec-var ,dbconn-type-var)
       ,@body))))

(defmacro with-pooled-dbconn
    ((dbconn-var dbconn-spec-var dbconn-type-var) &body body)
  `(with-database
       (,dbconn-var ,dbconn-spec-var
                    :database-type ,dbconn-type-var
                    :pool t
                    :if-exists :old)
     ,@body))

(restore-sql-reader-syntax-state)
