(in-package :imgdb-web)

(locally-enable-sql-reader-syntax)

(define-condition invalid-img-url-request ()
  ((img-url :initform (script-name)
            :reader img-url
            :type '(simple-array character))))

(defun img-url-handler ()
  (handler-case
      (with-database (dbconn *imgdb-store-db-conn-spec*
                             :database-type *imgdb-store-db-conn-spec*
                             :pool t)
        (let ((resize-parameters
               (calculate-resize-parameters-from-request
                "/img-urls" dbconn)))
          (ecase (length resize-parameters)
            (3 (let ((img-store-url (first resize-parameters))
                     (width (second resize-parameters))
                     (height (third resize-parameters)))
                 (transfer-resized-image img-store-url width height)))
            (1 (let ((img-store-url (first resize-parameters)))
                 (transfer-unresized-image img-store-url))))))
    (invalid-img-url-request () (not-found-page))))

(defun img-thumbnail-handler ()
  (handler-case
      (with-database (dbconn *imgdb-store-db-conn-spec*
                             :database-type *imgdb-store-db-type*
                             :pool t)
        (let ((resize-parameters
               (calculate-resize-parameters-from-request
                "/img-urls/thumbnails" dbconn)))
          (apply #'transfer-image-thumbnail resize-parameters)))))

(defun calculate-resize-parameters-from-request (prefix dbconn)
  (let* ((img-id (get-img-id-from-url (script-name) prefix))
         (req-params (get-parameters))
         (req-type (when req-params (read-from-string (caar req-params))))
         (req-arg (when req-params
                    (parse-integer (cdar req-params) :junk-allowed t))))
    (cond
      ((and (= (length req-params) 1)
            (not (null req-arg))
            (subsetp (list req-type)
                     '(resize width height)))
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
                         (list img-store-url)
                         (list img-store-url
                               (first resized-dimensions)
                               (second resized-dimensions))))
                   (signal 'invalid-img-url-request))))))
      ((= (length req-params) 0)
       (let ((img-store-url
              (first (get-img-store-url-and-size-from-img-id img-id dbconn))))
         (if (null img-store-url)
             (signal 'invalid-img-url-request)
             (list img-store-url))))
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

(defun transfer-resized-image (img-store-url new-width new-height)
  (setf (content-type) "image/jpeg" ;; only valid for jpegs
        (header-out "Last-Modified")
        (rfc-1123-date (get-universal-time)))
  (with-magick-wand (wand)
    (magick-read-image wand img-store-url)
    (magick-adaptive-resize-image wand new-width new-height)
    (with-image-blob (wand img-blob img-blob-size)
      (setf (content-length) img-blob-size)
      (let ((out (flexi-stream-stream (send-headers))))
        (iterate-over-foreign-buffer
            (vec 65536 vec-pos) (img-blob img-blob-size img-blob-pos)
          (write-sequence vec out :end vec-pos)
          (finish-output out))))))

(defvar *default-thumbnail-size* 100)

(defun transfer-image-thumbnail (img-store-url &optional new-width new-height)
  (assert
   (or (and (null new-width) (null new-height))
       (and (not (null new-width)) (not (null new-height)))))
  (setf (content-type) "image/jpeg" ;; only valid for jpegs
        (header-out "Last-Modified")
        (rfc-1123-date (get-universal-time)))
  (with-magick-wand (wand)
    (magick-read-image wand img-store-url)
    (let* ((width (image-width wand))
           (height (image-height wand))
           (size (min width height))
           (x (floor (/ (- width size) 2)))
           (y (floor (/ (- height size) 2)))
           (new-size (if (and (null new-width) (null new-height))
                         *default-thumbnail-size*
                         (min new-width new-height))))
      (magick-crop-image wand size size  x y)
      (magick-adaptive-resize-image wand new-size new-size)
      (with-image-blob (wand img-blob img-blob-size)
        (setf (content-length) img-blob-size)
        (let ((out (flexi-stream-stream (send-headers))))
          (iterate-over-foreign-buffer
              (vec 65536 vec-pos) (img-blob img-blob-size img-blob-pos)
            (write-sequence vec out :end vec-pos)
            (finish-output out)))))))

(defun resize-required? (width height req-type req-arg)
  (ccase req-type
    (resize
     (and (not (= req-arg 100))
          (resize-dimensions-by-percentage width height req-arg)))
    (width
     (and (not (= req-arg width))
          (resize-dimensions-to-width width height req-arg)))
    (height
     (and (not (= req-arg height))
          (resize-dimensions-to-height width height req-arg)))))

(defun resize-valid? (width height req-type req-arg)
  (<= req-arg
     (ccase req-type
       (resize 100)
       (width width)
       (height height))))

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
