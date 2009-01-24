(in-package :imgdb-store)

(locally-enable-sql-reader-syntax)

(defparameter *img-types* '("jpeg" "jpg"))
(defparameter *default-dbconn-spec* '())
(defparameter *default-thumbnail-size* 100)

(defclass dbconn-info ()
  ((dbconn-spec :initarg :conn-spec
              :initform
              (error "Database connection specification not provided"))
   (dbconn-type :initarg :conn-type
              :initform
              (error "Database connection type not provided"))))

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
  (create-all-tables store))

;;;; Functions for indexing and removing images

(defun index-img-drop (img-drop img-store dbconn-spec db-type)
  "Indexes all images in img-drop by moving them to the img-store and creating an entry in the img-store database."
  (let ((num-imgs 0))
    (with-database (dbconn dbconn-spec
                           :database-type db-type :pool t :if-exists :old)
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
    num-imgs))

(defun skip-img-record-creation (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-img-record-creation)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defun index-img (img-url img-store dbconn)
  "Creates a record of the image in the database"
  ;; How can the insertion of duplicate file entries be handled?
  (let ((img-digest (byte-array-to-hex-string (digest-file :sha1 img-url)))
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

(defun remove-img-record (img-url dbconn)
  "Removes the record indexed by id from the database"
  (let ((img-urldigest (byte-array-to-hex-string
                        (digest-sequence
                         :sha1 (ascii-string-to-byte-array img-url)))))
    (delete-records :from *img-table*
                    :where [= [urldigest] img-urldigest]
                    :database dbconn)
    (delete-file img-url)))

;;;; Functions for querying and updating images

(defmethod img-record-exists ((store imgdb-store) img-digest)
  (with-dbconn-info (dbconn (dbconn-info store))
    (> (caar (select [count [digest]]
                     :from *img-table*
                     :where [= [digest] img-digest]
                     :database dbconn))
       0)))

(defmethod count-img-records ((store imgdb-store))
  (with-dbconn-info (dbconn (dbconn-info store))
    (caar (select [count [*]]
                  :from *img-table*
                  :database dbconn))))

(defun select-img-records (select-columns &rest args)
  (apply #'select-from-table *img-table* select-columns args))

(defun update-img-records (&rest args)
  (apply #'update-records *img-table* args))

;;;; Database table creation/deletion functions
(defmethod create-img-table ((store imgdb-store))
  (with-dbconn-info (dbconn (dbconn-info store))
      (create-table *img-table*
                    '(([digest] (vector char 40) :not-null :unique :primary-key)
                      ([urldigest] (vector char 40) :not-null)
                      ([url] string :not-null :unique)
                      ([width] integer :not-null)
                      ([height] integer :not-null)
                      ([year] integer)
                      ([month] integer)
                      ([day] integer)
                      ([title] (vector char 40))
                      ([description] blob))
                    :database dbconn)))

(defmethod drop-img-table ((store imgdb-store))
  (with-dbconn-info (dbconn (dbconn-info store))
      (drop-table *img-table* :database dbconn)))
(defmethod img-table-exists ((store imgdb-store))
  (with-dbconn-info (dbconn (dbconn-info store))
      (table-exists-p *img-table* :database dbconn)))

(defmethod create-all-tables ((store imgdb-store))
  (unless (img-table-exists store)
    (create-img-table store))
  (unless (img-tags-table-exists store)
    (create-img-tags-table store))
  (unless (resize-cache-tables-exist? store)
    (create-resize-cache-tables store)))

(defmethod drop-all-tables ((store imgdb-store))
  (let* ((dbconn-info (dbconn-info store))
         (dbconn-spec (dbconn-spec dbconn-info))
         (dbconn-type (dbconn-type dbconn-info)))
    (drop-img-table dbconn-spec dbconn-type)
    (drop-img-tags-table dbconn-spec dbconn-type)
    (drop-resize-cache-tables dbconn-spec dbconn-type)))

;;;; Utility macros and functions

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
