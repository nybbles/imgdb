(in-package :imgdb-store)

(enable-sql-reader-syntax)

(defparameter *img-types* '("jpeg" "jpg"))
(defparameter *default-dbconn-spec* '())
(defparameter *img-table-name* '[imgs])

(defun index-img-drop (img-drop img-store dbconn)
  "Indexes all images in img-drop by moving them to the img-store and creating an entry in the img-store database."
  (let ((num-imgs 0))
    (walk-directory
     img-drop
     (lambda (x)
       (incf num-imgs)
       (handler-bind
           ((sql-database-data-error #'skip-img-record-creation))
         (create-img-record x img-store dbconn)))
     :test
     (lambda (x)
       (and (file-exists-p x)
            (subsetp (list (pathname-type x)) *img-types*
                     :test #'string-equal))))
    num-imgs))

(defun get-img-url-in-img-store (img-url img-store year month day)
  (merge-pathnames
   (make-pathname :directory 
                  (if (and year month day)
                      (list :relative year month day)
                      (list :relative "undated"))
                  :name (pathname-name img-url)
                  :type (pathname-type img-url))
   img-store))

(defun move-img-from-img-drop-to-img-store (img-url img-store year month day)
  (let ((img-store-url
         (get-img-url-in-img-store img-url img-store year month day)))
    (unless (equal img-url img-store-url)
      (ensure-directories-exist
       (directory-namestring img-store-url))
      (copy-file img-url img-store-url))
    img-store-url))

(defun skip-img-record-creation (c)
  (declare (ignore c))
  (let ((restart (find-restart 'skip-img-record-creation)))
    (if restart
        (invoke-restart restart)
        (error 'control-error))))

(defun create-img-record (img-url img-store dbconn)
  "Creates a record of the image in the database"
  ;; How can the insertion of duplicate file entries be handled?
  (let*
      ((img (read-image img-url))
       (img-date (image-date img))
       (img-year (when img-date (first img-date)))
       (img-month (when img-date (second img-date)))
       (img-day (when img-date (third img-date)))
       (img-store-url
        (move-img-from-img-drop-to-img-store img-url img-store
                                             img-year img-month img-day))
       (img-digest (byte-array-to-hex-string (digest-file :md5 img-store-url)))
       (img-store-url
        (concatenate 'string "file://" (namestring img-store-url)))
       (img-store-url-digest
        (byte-array-to-hex-string
         (digest-sequence :md5 (ascii-string-to-byte-array img-store-url)))))
    (restart-case
        (insert-records :into *img-table-name*
                        :attributes
                        '([digest] [urldigest] [url] [year] [month] [day])
                        :values 
                        (list img-digest img-store-url-digest img-store-url
                              img-year img-month img-day)
                        :database dbconn)
      (skip-img-record-creation ()
        (delete-file (regex-replace "^file://" img-store-url ""))
        nil))))

(defun remove-img-record (img-url dbconn)
  "Removes the record indexed by id from the database"
  (let ((img-urldigest (byte-array-to-hex-string
                        (digest-sequence
                         :md5 (ascii-string-to-byte-array img-url)))))
    (delete-records :from *img-table-name*
                    :where [= [urldigest] img-urldigest]
                    :database dbconn)
    (delete-file (regex-replace "^file://" img-url ""))))

(defmacro select-img-records (fields &rest constraints)
  `(select ,@fields :from ,*img-table-name* ,@constraints))

(defmacro do-img-record-select (vars fields constraints dbconn &body body)
  `(do-query ,vars [select ,@fields :from [imgs] ,@constraints]
             :database ,dbconn
             ,@body))

(defun img-record-exists (img-digest)
  (select-img-records '([digest]) :where [= [digest] img-digest]))

(defun connect-to-dbserver (dbconn-spec db-type)
  "Connect to a database"
  (connect dbconn-spec :database-type db-type))
(defun disconnect-from-dbserver (dbconn)
  (disconnect :database dbconn))

(defun create-img-database (dbconn db-type)
  (create-database dbconn :database-type db-type))
(defun destroy-img-database (dbconn db-type)
  (destroy-database dbconn :database-type db-type))
(defun img-database-exists (dbconn db-type)
  (probe-database dbconn :database-type db-type))

(defun create-img-table (dbconn)
  (create-table *img-table-name*
                '(([digest] (vector char 32) :not-null)
                  ([urldigest] (vector char 32) :not-null :unique :primary-key)
                  ([url] string :not-null :unique)
                  ([year] integer)
                  ([month] integer)
                  ([day] integer))
                :database dbconn))
(defun drop-img-table (dbconn)
  (drop-table *img-table-name* :database dbconn))
(defun img-table-exists (dbconn)
  (table-exists-p *img-table-name* :database dbconn))

;;; Stubs
'(defun repair-img-store (img-store dbconn))
