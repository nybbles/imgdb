(in-package :imgdb-web)

(defclass imgdb-web-server ()
  ((image-store :initarg :image-store
                :initform (error "Image store not provided")
                :reader image-store
                :type imgdb-store:imgdb-store)
   (web-root :initarg :web-root
             :initform (error "Web root not provided")
             :reader web-root
             :type pathname)
   (port :initarg :port
         :initform 4242
         :reader port
         :type (integer 0 65335))
   (is-running? :initform nil
                :accessor is-running?
                :type boolean)
   (server-handle :initform nil
                  :accessor server-handle)))

(defmethod dispatch-table ((web imgdb-web-server))
  (server-dispatch-table (server-handle web)))

(defmethod set-dispatch-table ((web imgdb-web-server) new-val)
  (setf (server-dispatch-table (server-handle web)) new-val))

(defsetf dispatch-table set-dispatch-table)

(defmethod start-imgdb-web-server ((web imgdb-web-server))
  (when (is-running? web)
    (error "imgdb web server is already running"))
  (setf (server-handle web)
        (start-server :dispatch-table '() :port (port web)))
  (setup-dispatch-table web)
  (setf (is-running? web) t))

(defmethod stop-imgdb-web-server ((web imgdb-web-server))
  (unless (is-running? web)
    (error "imgdb web server is not running"))
  (clear-dispatch-table web)
  (stop-server (server-handle web))
  (setf (server-handle web) nil)
  (setf (is-running? web) nil))
