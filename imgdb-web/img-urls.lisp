(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(define-condition invalid-img-url-request ()
  ((img-url :initform (script-name)
            :reader img-url
            :type '(simple-array character))))

(defun img-url-handler ()
  (handler-case
      (with-database (dbconn *imgdb-store-db-conn-spec*
                             :database-type *imgdb-store-db-type*
                             :pool t :if-exists :old)
        (let ((resize-parameters
               (calculate-resize-parameters-from-request
                "/img-urls" dbconn)))
          (ecase (length resize-parameters)
            (4 (let ((img-id (first resize-parameters))
                     (width (third resize-parameters))
                     (height (fourth resize-parameters)))
                 (transfer-resized-image img-id width height)))
            (2 (let ((img-store-url (second resize-parameters)))
                 (transfer-unresized-image img-store-url))))))
    (invalid-img-url-request () (not-found-page))))

(defun img-thumbnail-handler ()
  (handler-case
      (with-database (dbconn *imgdb-store-db-conn-spec*
                             :database-type *imgdb-store-db-type*
                             :pool t :if-exists :old)
        (let ((resize-parameters
               (calculate-resize-parameters-from-request
                "/img-urls/thumbnails" dbconn)))
          (ecase (length resize-parameters)
            (4 (let ((img-id (first resize-parameters))
                     (width (third resize-parameters))
                     (height (fourth resize-parameters)))
                 (transfer-image-thumbnail img-id width height)))
            (2 (let ((img-id (first resize-parameters)))
                 (transfer-image-thumbnail img-id))))))
    (invalid-img-url-request () (not-found-page))))

(defun calculate-resize-parameters-from-request (prefix dbconn)
  (let* ((img-id (get-img-id-from-url (script-name) prefix))
         (req-params (get-parameters))
         (req-type (when req-params (caar req-params)))
         (req-arg (when req-params
                    (parse-integer (cdar req-params) :junk-allowed t))))
    (cond
      ((and (= (length req-params) 1)
            (not (null req-arg))
            (subsetp (list req-type) '("resize" "width" "height")
                     :test #'equal))
       (let ((img-store-url-and-size
              (get-img-store-url-and-size-from-img-id img-id dbconn)))
         (if (null img-store-url-and-size)
             (signal 'invalid-img-url-request)
             (let* ((img-store-url (first img-store-url-and-size))
                    (img-width (second img-store-url-and-size))
                    (img-height (third img-store-url-and-size)))
               (if (resize-valid? img-width img-height req-type req-arg)
                   (let ((resized-dimensions
                          (resize-required? img-width img-height
                                            req-type req-arg)))
                     (if (not resized-dimensions)
                         (list img-id img-store-url)
                         (list img-id img-store-url
                               (first resized-dimensions)
                               (second resized-dimensions))))
                   (signal 'invalid-img-url-request))))))
      ((= (length req-params) 0)
       (let ((img-store-url
              (first (get-img-store-url-and-size-from-img-id img-id dbconn))))
         (if (null img-store-url)
             (signal 'invalid-img-url-request)
             (list img-id img-store-url))))
      (t (signal 'invalid-img-url-request)))))

(defun transfer-unresized-image (img-store-url)
  ;; Just serve up the unaltered image
  (with-open-file (img img-store-url
                   :direction :input :element-type '(unsigned-byte 8)
                   :if-does-not-exist :error)
    (setf (content-type) "image/jpeg" ;; only valid for jpegs
          (content-length) (file-length img)
          (header-out "Last-Modified")
          (rfc-1123-date (or (file-write-date img-store-url)
                             (get-universal-time))))
    (let ((out (flexi-stream-stream (send-headers))))
      (loop with buf = (make-array 65536 :element-type '(unsigned-byte 8))
            for pos = (read-sequence buf img)
            until (zerop pos)
            do (write-sequence buf out :end pos)
               (finish-output out)))))

(defun transfer-resized-image (img-id new-width new-height)
  (setf (content-type) "image/jpeg" ;; only valid for jpegs
        (header-out "Last-Modified")
        (rfc-1123-date (get-universal-time)))
  (with-resized-image (in img-id (list new-width new-height))
    (setf (content-length) (file-length in))
    (let ((out (flexi-stream-stream (send-headers))))
      (loop with buf = (make-array 65536 :element-type '(unsigned-byte 8))
           for buf-pos = (read-sequence buf in)
           until (zerop buf-pos)
           do (write-sequence buf out :end buf-pos)
              (finish-output out)))))

(defvar *default-thumbnail-size* 100)

(defun transfer-image-thumbnail (img-id &optional new-width new-height)
  (assert
   (or (and (null new-width) (null new-height))
       (and (not (null new-width)) (not (null new-height)))))
  (setf (content-type) "image/jpeg" ;; only valid for jpegs
        (header-out "Last-Modified")
        (rfc-1123-date (get-universal-time)))
  (let* ((new-size (if (and (null new-width) (null new-height))
                       *default-thumbnail-size*
                       (min new-width new-height))))
    (with-thumbnail (in img-id new-size)
      (setf (content-length) (file-length in))
      (let ((out (flexi-stream-stream (send-headers))))
        (loop with buf = (make-array 8192 :element-type '(unsigned-byte 8))
           for buf-pos = (read-sequence buf in)
           until (zerop buf-pos)
           do (write-sequence buf out :end buf-pos)
           (finish-output out))))))

(defun resize-required? (width height req-type req-arg)
  (cond
    ((equal req-type "resize")
     (and (not (= req-arg 100))
          (resize-dimensions-by-percentage width height req-arg)))
    ((equal req-type "width")
     (and (not (= req-arg width))
          (resize-dimensions-to-width width height req-arg)))
    ((equal req-type "height")
     (and (not (= req-arg height))
          (resize-dimensions-to-height width height req-arg)))))

(defun resize-valid? (width height req-type req-arg)
  (<= req-arg
     (cond
       ((equal req-type "resize") 100)
       ((equal req-type "width") width)
       ((equal req-type "height") height)
       (t (error "Invalid resize parameter")))))

(defun resize-dimensions-by-percentage (width height percent)
  (mapcar #'(lambda (x)
              (floor (* x (/ percent 100))))
          (list width height)))

(defun resize-dimensions-to-width (width height new-width)
  (list new-width (floor (* height (/ new-width width)))))

(defun resize-dimensions-to-height (width height new-height)
  (list (floor (* width (/ new-height height))) new-height))

(defun get-img-store-url-and-size-from-img-id (img-id dbconn)
  (car (select-img-records ([url] [width] [height])
                           :where [= [digest] img-id]
                           :database dbconn)))

(defun get-img-id-from-url (url prefix)
  (register-groups-bind (img-id)
      ((concatenate 'string prefix "/([0-9a-z]*)(\\?|$)") url)
    img-id))

(restore-sql-reader-syntax-state)
