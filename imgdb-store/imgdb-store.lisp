(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *img-types* '("jpeg" "jpg"))
(defparameter *default-dbconn-spec* '())
(defparameter *default-thumbnail-size* 100)

(defclass dbconn-info ()
  ((dbconn-spec :initarg :dbconn-spec
                :initform
                (error "Database connection specification not provided")
                :reader dbconn-spec)
   (dbconn-type :initarg :dbconn-type
                :initform
                (error "Database connection type not provided")
                :reader dbconn-type)))

(defclass imgdb-store ()
  ((img-drop :initarg :img-drop
             :initform (error "No image drop location provided")
             :type pathname
             :reader img-drop
             :documentation
             "The location that images are indexed from.")
   (img-store :initarg :img-store
              :initform (error "No image store location provided")
              :type pathname
              :reader img-store
              :documentation
              "The location where indexed images are stored.")
   (resize-cache :initarg :resize-cache
                 :initform (error "No resize cache location provided")
                 :type pathname
                 :reader resize-cache
                 :documentation
                 "The location where cached resized images are stored")
   (dbconn-info :initarg :dbconn-info
                :initform
                (error "No database connection information provided")
                :type dbconn-info
                :reader dbconn-info
                :documentation
                "Database connection information for the database
backend containing metadata")))

(defmethod initialize-instance :after ((store imgdb-store) &key)
  ;; Create any missing tables
  (with-dbconn-info (dbconn (dbconn-info store))
    (create-all-tables store dbconn)))

;;;; Functions for indexing and removing images

(defmethod index-img-drop ((store imgdb-store) dbconn)
  "Indexes all images in img-drop by moving them to the img-store and creating an entry in the img-store database."
  (let ((num-imgs 0)
        (img-drop (img-drop store))
        (img-store (img-store store)))
    (walk-directory
     img-drop
     (lambda (x)
       (handler-bind
           ((sql-database-data-error #'skip-img-record-creation))
         (when (index-img x img-store dbconn)
           (incf num-imgs))))
     :test
     (lambda (x)
       (and (file-exists-p x)
            (subsetp (list (pathname-type x)) *img-types*
                     :test #'string-equal)))))
  num-imgs)

(defun skip-img-record-creation (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-img-record-creation)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defmethod index-img ((store imgdb-store) img-url dbconn)
  "Creates a record of the image in the database"
  ;; How can the insertion of duplicate file entries be handled?
  (let ((img-store (img-store store))
        (img-digest (byte-array-to-hex-string (digest-file :sha1 img-url)))
        (img-inserted nil))
    (unless (img-record-exists img-digest dbconn)
      (with-magick-wand (wand)
        (magick-ping-image wand (namestring img-url))
        (let* ((img-date (or (image-original-date wand) (image-date wand)))
               (img-year (when img-date (first img-date)))
               (img-month (when img-date (second img-date)))
               (img-day (when img-date (third img-date)))
               (img-store-url
                (move-img-from-img-drop-to-img-store
                 img-digest img-url img-year img-month img-day img-store))
               (img-store-url-digest
                (byte-array-to-hex-string
                 (digest-sequence
                  :sha1
                  (ascii-string-to-byte-array (namestring img-store-url)))))
               (img-width (image-width wand))
               (img-height (image-height wand)))
          (restart-case
              (progn
                (insert-records :into *img-table*
                                :attributes
                                '([digest] [urldigest] [url]
                                  [width] [height]
                                  [year] [month] [day])
                                :values 
                                (list img-digest img-store-url-digest
                                      (namestring img-store-url)
                                      img-width img-height
                                      img-year img-month img-day)
                                :database dbconn)
                (setf img-inserted t))
            (skip-img-record-creation ()
              (unless (equal img-url img-store-url)
                (delete-file img-store-url)))))))
    (when img-inserted
      (assert (not (null *img-resize-cache-conn-spec*)))
      (assert (not (null *img-resize-cache-conn-type*)))
      ;; Create thumbnail.
      (with-thumbnail (in img-digest *default-thumbnail-size*)))
    img-inserted))

(defmethod remove-img-record ((store imgdb-store) img-url dbconn)
  "Removes the record indexed by id from the database"
  (let ((img-urldigest (byte-array-to-hex-string
                        (digest-sequence
                         :sha1 (ascii-string-to-byte-array img-url)))))
    (delete-records :from *img-table*
                    :where [= [urldigest] img-urldigest]
                    :database dbconn)
    (delete-file img-url)))

;;;; Functions for querying and updating images

(defmethod img-record-exists ((store imgdb-store) img-digest dbconn)
  (> (caar (select [count [digest]]
                   :from *img-table*
                   :where [= [digest] img-digest]
                   :database dbconn))
     0))

(defmethod count-img-records ((store imgdb-store) dbconn)
  (caar (select [count [*]]
                :from *img-table*
                :database dbconn)))

(defmethod select-img-title ((store imgdb-store) img-id dbconn)
  (let ((result
         (car (select-img-records store dbconn
                                  (list [title])
                                  :where [= [digest] img-id]
                                  :flatp t))))
    (if (null result)
        "Untitled"
        result)))

(defmethod select-img-records ((store imgdb-store) dbconn
                               select-columns &rest args)
  (apply #'select-from-table dbconn *img-table* select-columns args))

(defmethod update-img-title ((store imgdb-store) img-id title dbconn)
  (with-transaction (:database dbconn)
    (if (img-record-exists img-id dbconn)
        (progn
          (update-img-records
           store dbconn
           :av-pairs (list (list 'title (if (scan "^\s*$" title) nil title)))
           :where [= [digest] img-id])
          (select-img-title store img-id dbconn))
        "Untitled")))

(defmethod update-img-records ((store imgdb-store) dbconn &rest args)
  (apply #'update-records *img-table* args :database dbconn))

;;;; Utility macros and functions

;;; Utility functions for pathnames in image store directory
(defun get-img-store-url (img-url year month day img-store)
  (merge-pathnames
   (make-pathname :name (pathname-name img-url)
                  :type (pathname-type img-url))
   (get-img-store-directory year month day img-store)))

(defun get-img-store-directory (year month day img-store)
  (merge-pathnames
   (make-pathname :directory
                  (if (and year month day)
                      (list :relative year month day)
                      (list :relative "undated")))
   img-store))

(let ((url-version-scanner (create-scanner "(.+)-(\\d+)$")))
  (defun get-next-available-img-store-url (img-digest img-store-url)
    (if (is-img-store-url-occupied img-digest img-store-url)
        (get-next-available-img-store-url
         img-digest
         (let* ((name (pathname-name img-store-url))
                (type (pathname-type img-store-url))
                (next-name
                 (register-groups-bind (name version)
                     (url-version-scanner name)
                   (unless (or (null name) (null version))
                     (let ((next-version (+ 1 (parse-integer version))))
                       (concatenate 'string
                                    name "-"
                                    (write-to-string next-version)))))))
           (merge-pathnames
            (make-pathname :name
                           (if next-name
                               next-name
                               (concatenate 'string name "-" "1"))
                           :type type)
            img-store-url)))
        img-store-url)))

(defun is-img-store-url-occupied (img-digest img-store-url)
  (and
   (probe-file img-store-url)
   (not
    (equal img-digest
           (byte-array-to-hex-string (digest-file :sha1 img-store-url))))))

(defun move-img-from-img-drop-to-img-store
    (img-digest img-url year month day img-store)
  (let ((img-store-url
         (get-next-available-img-store-url
          img-digest
          (get-img-store-url img-url year month day img-store))))
    (if (equal img-url img-store-url)
        img-store-url
        (progn
          (ensure-directories-exist
           (directory-namestring img-store-url))
          (copy-file img-url img-store-url)
          img-store-url))))

(restore-sql-reader-syntax-state)
