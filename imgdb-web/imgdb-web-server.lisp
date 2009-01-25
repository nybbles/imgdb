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
   (dispatch-table :initarg :dispatch-table
                   :initform (error "Dispatch table not provided")
                   :accessor dispatch-table
                   :type list)
   (port :initarg :port
         :initform (error "Port not provided")
         :reader port
         :type (integer 0 65335))
   (is-running? :initform nil
                :accessor is-running?
                :type boolean)
   (server-handle :initform nil
                  :accessor server-handle)))

(defmethod start-imgdb-web-server ((web imgdb-web-server))
  (when (is-running? web)
    (error "imgdb web server is already running"))
  (setup-dispatch-table web)
  (setf (server-handle web) (start-server :port (port web)))
  (setf (is-running? web) t))

(defmethod stop-imgdb-web-server ((web imgdb-web-server))
  (unless (is-running? web)
    (error "imgdb web server is not running"))
  (stop-server (server-handle web))
  (setf (server-handle web) nil)
  (setf (is-running? web) nil))
